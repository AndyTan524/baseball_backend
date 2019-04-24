var utils = require('../helpers/utils');
var request = require('request');
var cheerio = require('cheerio');
var moment = require('moment-timezone');
var mongoose = require('mongoose');
var League = mongoose.model('League');
var Team = mongoose.model('Team');
var Roster = mongoose.model('Roster');
var Player = mongoose.model('Player');
var Transaction = mongoose.model('Transaction');
var Log = mongoose.model('Log');
var AccumulatedStats = mongoose.model('AccumulatedStats');

var sendGridHelper = require('../helpers/sendGridHelper');
var leagueHelper = require('../helpers/leagueHelper');
var messageHelper = require('../helpers/messageHelper');
var simulationHelper = require('../helpers/simulationHelper');
var teamInfo = require('../helpers/teamInfo');
var rosterLineupHelper = require('../helpers/rosterLineupHelper');

var playerStatus = require('../helpers/playerStatus');

module.exports = {

    removePlayer: function (leagueId, teamId, player, callback) {

        if (teamId == "Free Agents") {
            // get the league document to do this
            League.update(
                { _id: leagueId },
                { $pull: { FreeAgents: { PlayerId: player.PlayerId } } },
                function (err, FreeAgent) {
                    callback(err);
                }
            )
        } else {
            // remove from a team roster
            Roster.findOne({ LeagueId: leagueId, TeamId: teamId }, function (err, roster) {
                if (err) {
                    callback(err);
                } else {
                    // look through the fortyman first
                    var found = false;
                    var pid = player.PlayerId;
                    for (i = 0; i < roster.FortyManNL.length; i++) {
                        if (roster.FortyManNL[i].PlayerId == pid) {
                            found = true;
                            roster.FortyManNL.splice(i, 1);
                            roster.FortyManAL.splice(i, 1);
                            roster.markModified("FortyManNL");
                            roster.markModified("FortyManAL");
                            break;
                        }
                    }

                    if (found == false) {
                        for (i = 0; i < roster.NonRoster.length; i++) {
                            if (roster.NonRoster[i].PlayerId == pid) {
                                found = true;
                                roster.NonRoster.splice(i, 1);
                                roster.markModified("NonRoster");
                                break;
                            }
                        }
                    }

                    // now remove from the rosterlineup too...
                    rosterLineupHelper.removePLayerFromRosterLineup(roster, player, false, function(err, playerWithEStats){
                        if (found) {
                            roster.save(function (err, response, playerWithEStats) {
                                callback(err);
                            });
                        } else {
                            callback("no player found", null, playerWithEStats);
                        }
                    })

                }
            });
        }
    },

    addPlayerToTeam: function (leagueId, teamId, player, fromTeamId, playerEStats, callback) {

        var context = this;
        if (teamId == "Free Agents") {
            // add to the free agents list: get the league document to do this for the TO team
            League.findOne({ _id: leagueId }, function (err, league) {
                if (err) {
                    callback(err);
                } else {
                    // "push" to the front of the array
                    player.Status = playerStatus.FreeAgent;
                    player.onFortyMan = false;
                    player["formerTeamId"] = fromTeamId;
                    player["formerTeam"] = context.findTeamNameInLeague(league, fromTeamId);
                    league.FreeAgents.unshift(player);
                    league.markModified("FreeAgents");
                    league.save(function (err, response) {
                        callback(err, response, "Free Agents");
                    });
                }
            }
            )
        } else {
            // add to a team roster
            Roster.findOne({ LeagueId: leagueId, TeamId: teamId }, function (err, roster) {
                // "push" onto the front of the array
                player.Status = playerStatus.Unknown;
                player.onFortyMan = false;
                var addedTo = "ACTIVE ROSTER";
                if (player.Level == "ML") {
                    roster.FortyManAL.unshift(player);
                    roster.FortyManNL.unshift(player);
                    roster.markModified("FortyManAL");
                    roster.markModified("FortyManNL");
                } else {
                    // put on minors
                    roster.NonRoster.unshift(player);
                    roster.markModified("NonRoster");
                    addedTo = "MINOR LEAGUE SYSTEM";
                }
                var rostersize = roster.FortyManNL.length + roster.NonRoster.length;


                // now need to add to rosterLineup
                if( playerEStats ) {
                    // then put the player's estats into the rosterLineup.Players array and clean up the roster lineup
                    rosterLineupHelper.updateDepthChartsFromRoster(roster, playerEStats, false, function(err, response){
                        roster.save(function (err, response) {
                            if (err) {
                                callback(err, "Player not added to team.");
                            } else {
                                callback(err, "Player added to " + addedTo + ". New roster size: " + rostersize);
                            }
                        });
                    })
                } else {
                    // need to calculate the player's eligilibity, then put into the players array and clean up the roster lineup
                    rosterLineupHelper.addPlayerToRosterLineup(player, roster, false, function(err, response){
                        roster.save(function (err, response) {
                            if (err) {
                                callback(err, "Player not added to team.");
                            } else {
                                callback(err, "Player added to " + addedTo + ". New roster size: " + rostersize);
                            }
                        });
                    })
                }
            });

        }
    },

    // ****************************************
    //
    // movePlayerToTeam moves a player from one team to another
    // also moves their roster lineup data between teams
    // cannot move between free agents.
    // ***************************************
    movePlayerToTeam: function (playerId, fromTeamRoster, toTeamRoster, fromRosterLineup, toRosterLineup) {
        var player = null;
        var playerMajorsIndex = -1;
        var playerMinorsIndex = -1;
        var rlPlayerIndex = -1;
        var rlInactivePlayerIndex = -1;
        var rlNonRosterPlayerIndex = -1;

        // first check the majors...
        for (var i = 0; i < fromTeamRoster.FortyManNL.length; i++) {
            if (fromTeamRoster.FortyManNL[i].PlayerId == playerId) {
                player = fromTeamRoster.FortyManNL[i];
                playerMajorsIndex = i;
                break;
            }
        }

        // not in the majors, check the minors...
        if (playerMajorsIndex == -1) {

            for (var i = 0; i < fromTeamRoster.NonRoster.length; i++) {
                if (fromTeamRoster.NonRoster[i].PlayerId == playerId) {
                    player = fromTeamRoster.NonRoster[i];
                    playerMinorsIndex = i;
                    break;
                }
            }
        }

        // before we switch the player, be sure they are not still eligible for trades
        player["TradeStatus"] = playerStatus.Unknown;
        
        if (playerMajorsIndex >= 0) {
            toTeamRoster.FortyManAL.unshift(player);
            toTeamRoster.FortyManNL.unshift(player);
            fromTeamRoster.FortyManAL.splice(playerMajorsIndex, 1);
            fromTeamRoster.FortyManNL.splice(playerMajorsIndex, 1);

        } else if (playerMinorsIndex >= 0) {
            toTeamRoster.NonRoster.unshift(player);
            fromTeamRoster.NonRoster.splice(playerMinorsIndex, 1);
        } else {
            // player not found
        }

        // do a similar thing for the rosterLineups
        var rlPlayer = null
        for( var i=0; i<fromRosterLineup.Players.length; i++ ) {
            if (fromRosterLineup.Players[i].PlayerId == playerId) {
                rlPlayer = fromRosterLineup.Players[i];
                rlPlayerIndex = i;
                break;
            }           
        }

        // if not in the players, see if in the inactive
        if (rlPlayerIndex == -1) {
            for (var i = 0; i < fromRosterLineup.InactivePlayers.length; i++) {
                if (fromRosterLineup.InactivePlayers[i].PlayerId == playerId) {
                    rlPlayer = fromRosterLineup.InactivePlayers[i];
                    rlInactivePlayerIndex = i;
                    break;
                }
            }
        }

        // if not in the players or in the inactive players, see if in the non-roster
        if( rlPlayerIndex == -1  && rlInactivePlayerIndex == -1) {
            for( var i=0; i<fromRosterLineup.NonRosterPlayers.length; i++ ) {
                if (fromRosterLineup.NonRosterPlayers[i].PlayerId == playerId) {
                    rlPlayer = fromRosterLineup.NonRosterPlayers[i];
                    rlNonRosterPlayerIndex = i;
                    break;
                }           
            }          
        }


        // now switch 'em in the roster lineups arrays
        if (rlPlayerIndex >= 0) {
            toRosterLineup.Players.unshift(rlPlayer);
            fromRosterLineup.Players.splice(rlPlayerIndex, 1);

        } else if (rlInactivePlayerIndex >= 0) {
            toRosterLineup.InactivePlayers.unshift(rlPlayer);
            fromRosterLineup.InactivePlayers.splice(rlInactivePlayerIndex, 1);
            
        } if (rlNonRosterPlayerIndex >= 0) {
            toRosterLineup.NonRosterPlayers.unshift(rlPlayer);
            fromRosterLineup.NonRosterPlayers.splice(rlNonRosterPlayerIndex, 1);
        } else {
            // player not found in roster lineups
        }

        return (player);
    },

    findTeamIndexInLeague: function (league, teamId) {
        for (i = 0; i < league.Teams.length; i++) {
            if (league.Teams[i]._id == teamId) {
                return (i);
            }
        }

        return (false);
    },

    findTeamIndexInLeagueByRName: function (league, rname) {
        for (i = 0; i < league.Teams.length; i++) {
            if (league.Teams[i].r_name == rname) {
                return (i);
            }
        }

        return (false);
    },

    findTeamNameInLeague: function (league, teamId) {
        for (i = 0; i < league.Teams.length; i++) {
            if (league.Teams[i]._id == teamId) {
                return (league.Teams[i].r_name);
            }
        }

        return ("");
    },



    movePlayerToTeamOrFA: function (
        leagueId,
        user,
        fromTeamId,
        toTeamId,
        player,
        carriedSalary,
        transMsg,
        callback
    ) {
        // first grab and remove player from original list
        // fromTeamId or toTeamId is "Free Agents" if that's the "team" to move them to/from
        var context = this;
        var moment = require('moment-timezone');
        moment.tz.setDefault("America/Los_Angeles");

        context.removePlayer(leagueId, fromTeamId, player, function (err, msg, playerEStats) {

            if (err) {
                callback({ status: 500, "msg": msg });
            } else {
                var removemsg = msg;
                context.addPlayerToTeam(leagueId, toTeamId, player, fromTeamId, playerEStats, function (err, msg) {
                    if (err) {
                        callback({ status: 500, "msg": " Error:  " + msg });
                    } else {
                        callback({ status: 200, "msg": msg });

                        // now create a new transaction.. grab the league...
                        League.findOne({ _id: leagueId }, function (err, league) {

                            if (fromTeamId == "Free Agents") {
                                fromTeamName = "Free Agents";
                            } else {
                                fromTeamName = context.findTeamNameInLeague(league, fromTeamId);
                            }
                            if (toTeamId == "Free Agents") {
                                toTeamName = "Free Agents";
                            } else {
                                toTeamName = context.findTeamNameInLeague(league, toTeamId);
                            }


                            transaction = new Transaction();
                            transaction.Type = "Transfer";
                            transaction.Status = "Complete";
                            transaction.Archived = false;
                            transaction.LeagueId = leagueId;
                            transaction.DateUTC = new Date().toISOString();
                            transaction.DealId = 0;
                            transaction.Teams = [
                                { name: fromTeamName, id: fromTeamId },
                                { name: toTeamName, id: toTeamId }
                            ];

                            transferDate = moment(transaction.DateUTC).format("llll");
                            if (transMsg == "") {
                                var userName = "System";
                                if( user && user._doc ) {
                                    userName = user._doc.FirstName + " " + user._doc.LastName;
                                }
                                transaction.Headline = "TRANSFER: " + player.FullName + " transfered from " + fromTeamName + " to " + toTeamName + " on " + transferDate + ". Transfer made by " + userName  + " (in the League Office)";
                            } else {
                                transaction.Headline = transMsg.replace("{{player}}", player.FullName);
                                transaction.Headline = transaction.Headline.replace("{{toTeam}}", toTeamName);
                                transaction.Headline = transaction.Headline.replace("{{fromTeam}}", fromTeamName);

                            }
                            Transaction.create(transaction, function (err, t) {
                                if (err) {
                                } else {

                                    // *************************************************************************************
                                    //
                                    // see if carried salary.. if so, push the player into the team's retained player area
                                    // note, in this case, only carrying the salary.
                                    // must add the date of the transaction to the carried player
                                    //
                                    // *************************************************************************************
                                    var teamlist = [fromTeamId, toTeamId];

                                    // only carry the salary if this player had a guaranteed salary
                                    if (player.Level != "ML" && player.onFortyMan == false) {
                                        if (!("Guaranteed" in player.rSalary) || player.rSalary.Guaranteed == false) {
                                            carriedSalary = false;
                                        }
                                    }

                                    if (carriedSalary) {

                                        Object.keys(player.rSalary).forEach(function (year, index) {
                                            // year: the name of the object key
                                            // index: the ordinal position of the key within the object 
                                            if (player.rSalary[year].Salary != "") {
                                                salaryyear = "Salary" + year;
                                                player[salaryyear] = player.rSalary[year].Salary;
                                            }
                                            if (player.rSalary[year].Buyout != "") {
                                                salaryyear = "Buyout" + year;
                                                player[salaryyear] = player.rSalary[year].Buyout;
                                            }
                                            if (player.rSalary[year].Contract != "") {
                                                salaryyear = "Contract" + year;
                                                player[salaryyear] = player.rSalary[year].Contract;
                                            }
                                        });

                                        // move his AAV into the retained player aav slot
                                        player["AAV"] = "0.00";
                                        if (player.rSalary.AAV)
                                            player["AAV"] = player.rSalary.AAV;

                                        // add today's date to the player's data
                                        player["CarriedDate"] = new Date().toISOString();
                                        league.Teams[context.findTeamIndexInLeague(league, fromTeamId)].retainedPlayers.push(player);
                                        league.markModified("Teams");
                                        league.save(function (err, result) {
                                            if (err) { }
                                            else { }
                                            // update the financials here
                                            leagueHelper.updateFinancials(leagueId, "list", teamlist, true, function () {
                                                skipAdmins = false;
                                                messageHelper.alertInterestedParties(user, leagueId, t.Headline, fromTeamId, fromTeamName, toTeamId, player, "msg", transaction.Headline, skipAdmins, function (response) {

                                                });

                                            });
                                        });
                                    } else {
                                        // update the financials here
                                        leagueHelper.updateFinancials(leagueId, "list", teamlist, true, function () {
                                            skipAdmins = false;
                                            messageHelper.alertInterestedParties(user, leagueId, t.Headline, fromTeamId, fromTeamName, toTeamId, player, "msg", transaction.Headline, skipAdmins, function (response) {

                                            });

                                        });
                                    }
                                }
                            });

                        });
                    }
                });
            }
        });
    },



    // **********************************************
    //
    // executeDealPoints
    //  --> cycle through deal points for one team
    //      transfer players
    //      if needed, put player onto fromTeam's carried contract list (in the league document)
    //      if there is a cash consideration, adjust the salary caps of both teams
    //      if there is a draft pick consideration, put it in the financial notes for both teams
    //      if there is a international slot pool $ put in the financial notes for both teams
    //      if there is a player to be nambed later, put it in the financial notes for both teams
    // 
    executeDealPoints: function (league, dealPoints, fromTeam, toTeam, fromRoster, toRoster, fromRosterLineup, toRosterLineup) {

        var fromTeamIndex = this.findTeamIndexInLeague(league, fromTeam.id);
        var toTeamIndex = this.findTeamIndexInLeague(league, toTeam.id);

        var context = this;

        dealPoints.forEach(function (dp, index) {
            var noteToAdd = "";

            if (dp.type == "Player") {

                player = context.movePlayerToTeam(dp.player.PlayerId, fromRoster, toRoster, fromRosterLineup, toRosterLineup);

                // *************************************************************************************
                //
                // see if carried salary.. if so, push the player into the team's retained player area
                // note, in this case, only carrying the salary.
                // must add the date of the transaction to the carried player
                //
                // *************************************************************************************
                if (dp.detail != "" && dp.detail != "0.000") {
                    if (player) {
                        var carriedAmount = "$" + dp.detail;
                        Object.keys(player.rSalary).forEach(function (year, index) {
                            // year: the name of the object key
                            // index: the ordinal position of the key within the object 
                            if (player.rSalary[year].Salary != "") {
                                salaryyear = "Salary" + year;
                                player[salaryyear] = carriedAmount;
                            }
                        });

                        // move his AAV into the retained player aav slot
                        player["AAV"] = "0.00";
                        if (player.rSalary.AAV)
                            player["AAV"] = player.rSalary.AAV;

                        // add today's date to the player's data
                        player["CarriedDate"] = new Date().toISOString();
                        league.Teams[fromTeamIndex].retainedPlayers.push(player);
                    }
                    noteToAdd = fromTeam.name + " will carry $" + dp.detail + "M of " + dp.player.FullName + "'s salary";
                }

            } else if (dp.type == "Cash") {
                var fromTeamCap = Number(league.Teams[fromTeamIndex].financials['Spending Cap'].replace(/[^0-9.]/g, ""));
                var toTeamCap = Number(league.Teams[toTeamIndex].financials['Spending Cap'].replace(/[^0-9.]/g, ""));
                var cashConsideration = Number(dp.detail) * 1000000;

                fromTeamCap = fromTeamCap - cashConsideration;
                toTeamCap = toTeamCap + cashConsideration

                league.Teams[fromTeamIndex].financials['SpendingCap'] = ("$" + (fromTeamCap).toFixed(0)).replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");
                league.Teams[toTeamIndex].financials['SpendingCap'] = ("$" + (toTeamCap).toFixed(0)).replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");

                noteToAdd = fromTeam.name + " gave $" + dp.detail + "M to " + toTeam.name;

            } else if (dp.type == "DraftPick") {
                noteToAdd = fromTeam.name + " gave its draft pick(s) " + dp.detail + " to " + toTeam.name;

            } else if (dp.type == "PTBNL") {
                // player to be named later
                noteToAdd = fromTeam.name + " gave a PLAYER TO BE NAMED LATER to " + toTeam.name;

            } else if (dp.type == "ISPD") {
                // international spending pool dollars
                noteToAdd = fromTeam.name + " gave $" + dp.detail + "M from the International Spending Pool Dollars to " + toTeam.name;

            } else {
                // have no idea what the deal point is, skip it.
            }

            if (noteToAdd != "") {
                if (!league.Teams[fromTeamIndex].financials.Notes) {
                    league.Teams[fromTeamIndex].financials['Notes'] = [];
                }
                league.Teams[fromTeamIndex].financials.Notes.push(noteToAdd);

                if (!league.Teams[toTeamIndex].financials.Notes) {
                    league.Teams[toTeamIndex].financials['Notes'] = [];
                }
                league.Teams[toTeamIndex].financials.Notes.push(noteToAdd);
            }
        });

    },

    putTeamsInDivisions: function (league, leagueId) {
        setDivisions = function (teams) {
            var at = league.Conferences;

            leagueIndex = new Array(2);
            divisionIndex = new Array(2);
            leagueStyle = new Array(2)
            for (l = 0; l < 2; l++) {
                leagueIndex[l] = at[l].name;
                leagueStyle[l].backgroundColor = at[l].color;
                for (d = 0; d < 4; d++) {
                    // empty the teams from the division.
                    if (d == 0) {
                        divisionIndex[l] = new Array(4);
                    }
                    divisionIndex[l][d] = at[l].divisions[d].name;
                    at[l].divisions[d].teams = [];
                }
            }


            for (t = 0; t < teams.length; t++) {
                nextTeam = leagu.Teams[t];

                nextTeam.useDH = false;

                if (nextTeam.conference) {
                    li = leagueIndex.indexOf(nextTeam.conference);
                } else {
                    li = Math.round(t / 15);
                }
                if (li == 0) {
                    nextTeam.useDH = true;
                }
                if (nextTeam.division) {
                    div = divisionIndex[li].indexOf(nextTeam.division);
                } else {
                    div = Math.round(t / 6);
                }
                at[li].divisions[div].teams.push(nextTeam);

            }
        }
    },

    /// ******************* GETTING / UPDATING ACCUMULATED STATS AND USED GAMES ****************
   
    // lowest level code to actually add the stats from today's game to the previously accumulated stats
    //              also puts in the full and partial games played

    calculateWAR : function( finalStats, season ) {

        // first do the batters
        var battingStats = null;
        var pitchingStats
        var rOW = 0;
        var rDW = 0;
        var rBW = 0;
        if (finalStats.Stats) {

            for (i = 0; i < finalStats.Stats.length; i++) {
                if (finalStats.Stats[i].Season == season) {
                    battingStats = finalStats.Stats[i];
                    break;
                }
            }
            // everyone gets batting stats.. be sure he's had a PA.
            if (battingStats && battingStats.PA) {

                        /*
            WAR is calculated as the following...

            For hitters, it's ((rOW) + (rDW) + (rBW)) / (10) 
            where rOW equals batting LW, rDW equals total fielding LW (including catcher stats where applicable) 
            and rBW equals bases/baserunning stats.
            */
                rOW = battingStats.LW;
                rDW = battingStats.FieldLW;
                rBW = battingStats.Bases
                var WAR = (rOW + rDW + rBW) / 10;
                battingStats["WAR"] = WAR;
            }
        }


        if( finalStats.PitchingStats) {
            for (i = 0; i < finalStats.PitchingStats.length; i++) {
                if (finalStats.PitchingStats[i].Season == season) {
                    pitchingStats = finalStats.PitchingStats[i];
                    break;
                }
            }
            // everyone has pitching stats, be sure they have some innings pitched in this season...
            if (pitchingStats && pitchingStats.IP) {
                /*
                WAR is calculated as the following...
           
                For pitchers, it's ((rOW) + (rDW) + (rBW) + (rPW *1.5)) / (10) 
                where rOW equals batting LW, rDW equals total fielding LW
                and rBW equals bases/baserunning stats.
                */
                var rPW = pitchingStats.LW ? pitchingStats.LW : 0;
               var WAR =  (rOW + rDW + rBW + (rPW*1.5)) / 10;
               pitchingStats["WAR"] = WAR;
            }
        }


    },

    calculateYTDStats: function (gameStats, isPitcher) {
        if( !gameStats ) {
            return (null);
        }
        if (isPitcher) {
            // for Pitchers: ERA, IP, Bavg, wlpct, WAR?
            //        
            var outs = gameStats.OUT ? gameStats.OUT : 0;
            var IP = outs / 3;
            var ER = gameStats.ER ? gameStats.ER : 0;
            var era = (IP > 0) ? (ER * 9) / IP : "--";

            var wins = gameStats.W ? gameStats.W : 0;
            var loss = gameStats.L ? gameStats.L : 0;
            var wpct = (wins + loss > 0) ? (wins / (wins + loss)) : 0;

            var hits = gameStats.H ? gameStats.H : 0;
            var bf = gameStats.BFP ? gameStats.BFP : 0;
            var obp = (bf > 0) ? (hits / bf) : null;

            gameStats["ERA"] = era;
            gameStats["WLPct"] = wpct;
            gameStats["BOBP"] = obp;

            if (IP > 0) {
                var base = Math.floor(IP);
                var remainder = IP - base;
                if (remainder != 0) {
                    IP = (remainder < .5) ? (IP + 0.1) : (IP + 0.2);
                }
                gameStats["IP"] = IP;
            }

        } else {

            // for batters: BAvg,  SLG, OBP, OBS, ISO, WAR?
            //               possibly: Field, Zone, Block, Frame, cERA

            var hits = gameStats.H ? gameStats.H : 0;
            var bb = gameStats.BB ? gameStats.BB : 0;
            var hbp = gameStats.HBP ? gameStats.HBP : 0;
            var sf = gameStats.SF ? gameStats.SF : 0;
            var ab = gameStats.AB ? gameStats.AB : 0;
            var pa = gameStats.PA ? gameStats.PA : 0;
            var b1 = gameStats["1B"] ? gameStats["1B"] : 0;
            var b2 = gameStats["2B"] ? gameStats["2B"] : 0;
            var b3 = gameStats["3B"] ? gameStats["3B"] : 0;
            var hr = gameStats.HR ? gameStats.HR : 0;
            var bases = b1 + (2 * b2) + (3 * b3) + (4 * hr);
            var bavg = ab > 0 ? (hits / ab) : 0;

            var obp = ab + bb + sf + hbp;
            obp = obp > 0 ? (hits + bb + hbp) / obp : 0;

            var slg = ab > 0 ? (bases / ab) : 0;
            var ops = slg + obp;

            // ISO = Slugging % - Batting Avg.
            var iso = slg - bavg;
            gameStats['AVG'] = bavg;
            gameStats['SLG'] = slg;
            gameStats['OBP'] = obp;
            gameStats['OPS'] = ops;
            gameStats['ISO'] = iso;

        }

        return gameStats;
    },

    positionsByNumber : ["", "P", "CA", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "DH"],

    calculateYTDSpeed: function (battingStats, fieldingStats, primary, secondary, tertiary) {
       
        // https://en.wikipedia.org/wiki/Speed_Score

        var speed = 0;
        var SB = 0;
        var CS = 0;
        var H = 0;
        var B1 = 0;
        var BB = 0;
        var HBP = 0;
        var AB = 0;
        var B3 = 0;
        var HR = 0;
        var K  = 0;
        var R  = 0;
        var GDP = 0;

        if (battingStats) {
            SB = "SB" in battingStats ? Number(battingStats.SB) : 0;
            CS = "CS" in battingStats ?Number(battingStats.CS) : 0;
            BB = "BB" in battingStats ?Number(battingStats.BB) : 0;
            H = "H" in battingStats ? Number(battingStats.H) : 0;
            B1 = "1B" in battingStats ?Number(battingStats["1B"]) : 0;
            B3 = "3B" in battingStats ?Number(battingStats["3B"]) : 0;
            HBP = "HBP" in battingStats ?Number(battingStats.HBP) : 0;
            AB = "AB" in battingStats ?Number(battingStats.AB) : 0;
            HR = "HR" in battingStats ?Number(battingStats.HR) : 0;
            K = "K" in battingStats ?Number(battingStats.K) : 0;
            R = "R" in battingStats ?Number(battingStats.R) : 0;
            GDP = "GIDP" in battingStats ?Number(battingStats.GIDP) : 0;

        } 

        
        // F1 = 20 * ( ((sb+3)/(sb*cs+7)-0.4);
        var F1 = 20 *  ( ( (SB+3)/(SB*CS+7) )-0.4);
        if( F1 > 10)   F1 = 10;

        // F2 = 14.286 * Math.sqrt( (sb+cs)/(1b+bb+hbp) )   ; first factor was 1/0.07
        var F2  =  0;
        if( (B1+BB+HBP) != 0 )
            F2 =14.286 * Math.sqrt( (SB+CS)/(B1+BB+HBP) )   ; //first factor was 1/0.07
            if( F2 > 10)   F2 = 10;

        // F3 = 626 * (3b/(ab-hr-k))
        var F3 = 0;
        if( (AB-HR-K) != 0)
            F3 = 626 * (B3/(AB-HR-K));
        if( F3 > 10)   F3 = 10;

        // F4 = 25 * ( ((r-hr)/(h+bb+hbp-hr)) - 0.1   );
        var F4 = 25 * (0 - 0.1);        // have to assume something here!
        if( (H+BB+HBP-HR) != 0)
            F4 = 25 * ( ((R-HR)/(H+BB+HBP-HR)) - 0.1   );
        if( F4 > 10)   F4 = 10;

        // F5 = 142.86 * (0.063 - ( gdp/(ab-hr-k)))
        var F5 = 142.86 * (0.063 - ( 0)); // have to assume something here!
        if ( (AB-HR-K) != 0 ) {
           F5 =  142.86 * (0.063 - ( GDP/(AB-HR-K)))
        }
        if( F5  > 10)   F5 = 10;

        var OffensiveSpeed = (F1+F2+F3+F4+F5) / 6;

        // 
        // now get defensive speed...
        //

        // F6 = 0 (for pitchers.. more below)
        // speed = (F1+F2+F3+F4+F5+F6) / 6
        //   OR  = (F1 + F2+ F3+F4+F5)/6   +  (F6/6) = Offensive Speed + (F6/6)
        var F6 = 0;
             // get position that will give them the most speed...OF, 2b, ss, 1b, 3b, c, p

             var speedChart = ["LF", "CF", "RF", "OF", "2B", "SS", "1B", "3B", "DH", "C", "CA",  "P"];
             var speedIndex = 99;
 
             if (primary && primary.length > 0) {
 
                 // get speed based on primary position
                 for (var i = 0; i < primary.length; i++) {
                     var si = speedChart.indexOf(primary[i]);
                     if (si >= 0 && si < speedIndex) {
                         speedIndex = si;
                         if (speedIndex < 4)
                             break;
                     }
                 }
 
             } else if (secondary && secondary.length > 0) {
 
                 // get speed based on secondary position
                 for (var i = 0; i < primary.length; i++) {
                     var si = speedChart.indexOf(secondary[i]);
                     if (si >= 0 && si < speedIndex) {
                         speedIndex = si;
                         if (speedIndex < 4)
                             break;
                     }
                 }
             } else if (tertiary && tertiary.length > 0) {
 
                 // get speed based on tertiary position
                 for (var i = 0; i < primary.length; i++) {
                     var si = speedChart.indexOf(tertiary[i]);
                     if (si >= 0 && si < speedIndex) {
                         speedIndex = si;
                         if (speedIndex < 4)
                             break;
                     }
                 }
             } else if (fieldingStats && fieldingStats.Pos) {
                 speedIndex = speedChart.indexOf(this.positionsByNumber[fieldingStats.Pos]);
             }
 
             if (speedIndex == -1 || speedIndex == 99) {
                 speedIndex = 9;
             }
        if( speedChart[speedIndex] == "P") {
            F6 = 0;
        } else if (speedChart[speedIndex] == "C" || speedChart[speedIndex] == "CA") {
            F6 = 1;
        } else if (speedChart[speedIndex] == "1B" || speedChart[speedIndex] == "DH") {
            F6 = 2;
        } else if (fieldingStats && fieldingStats.G && fieldingStats.G > 0) {


            var PO = "PO" in fieldingStats ? Number(fieldingStats.PO) : 0;
            var A = "A" in fieldingStats ? Number(fieldingStats.A) : 0;
            var G = Number(fieldingStats.G);

            var defensiveFactor = (PO*A)/G;

            // pitcher f6=0, catcher f6=1, 1b f6=2
            // 2b F6 = (5/4) * ((PO+A) / G)
            // 3B F6 = (4/2.65) * ((PO+A)/G)
            // SS F6 = (7/4.6) * ((PO+A)/G)
            // OF F6 = 3 * ((PO+A)/G)

            if( speedChart[speedIndex] == "2B") {
                F6 = (5/4) * defensiveFactor;
            } else if (speedChart[speedIndex] == "3B" ) {
                F6 = (4/2.65) * defensiveFactor;
            } else if (speedChart[speedIndex] == "SS" ) {
                F6 = (7/4.6) * defensiveFactor;
            } else {
                // outfield...
                F6 = 3 * defensiveFactor;
            }

            if( F6 > 10)
                F6 = 10;
        } else {
            // have no idea what position this person plays
            F6 = 1;
        }

        speed = OffensiveSpeed + (F6 / 6);

        if( battingStats)
            battingStats["Speed"] = speed;
        if( fieldingStats) 
            fieldingStats["Speed"] = speed;

    },

    // ******************************************************
    //
    // do the actual work of adding previous stats to new game stats
    // handles decisions for pitchers, full games used partial games used
    // if pitchStats != null, then lineupStats is the pitching lineup stats..... so have the SP, RP, CL positions!
    //
    // ************************************************************************** 
    addGameStatsToAccStats: function (player, gameStats, pitchStats, previousStats, prevNLPStats, fullGames, partialGames, lineupStats, playerRawStats, isPitcher) {

        var skip = ["Pos", "Season", "FullName", "CalendarDay", "Slot", "SubSlot", "YTDISO", "PlayerId", "GameDate", "cERA", "MlbId", "GameString", "GameId", "IPG", "CalendarDay", "IPPG"];
        
        // internal helper functions...
        function getOutsFromIP(ip) {
            var ip = Number( ip).toFixed(1);
            var innings = ip.charAt(0);
            var fraction = ip.charAt(2);
            var outs = innings * 3;
            if( fraction == "2") {
                outs += 2;
            } else if(  fraction == "1") {
                outs += 1;
            }
            return (outs);
        }

        function getIPFromOuts(outs) {
            var fractOuts = outs / 3;
            var whole = Math.floor(fractOuts);
            var ip = whole + (fractOuts % 1 == 0 ? 0 : (fractOuts % 1 > 0.5 ? 0.2 : 0.1));
            return (ip);
        }
        
        
        var pitcherPositions = ["P", "SP", "RP", "CL"];
        
        var positionPlayed = gameStats.Pos;
        if( pitchStats) {
            positionPlayed = pitchStats.Pos;
            isPitcher = true;
        }
        var CalendarDay = gameStats.CalendarDay;

        
        // insert game's calendar day..
        if( player.DaysList.indexOf( CalendarDay) == -1 )
            player.DaysList.unshift( CalendarDay);
        player.LastDayPlayed = CalendarDay;

        // ****************** CALCULATE DAYS IN A ROW
        var inarow = 1; // he played today, so one day in a row...
        var previousDay  = CalendarDay;
        var previousPosition = positionPlayed;
        var positionDaysInARow = 1;

        for( let d=1; d<10; d++ ) {
            if( player.DaysList[d]) {
                var nextDay  = player.DaysList[d];

                // remember, double header are like 93.2 compared to 93 for the day game or a single game
                if( previousDay != nextDay && previousDay - nextDay <= 1) {
                    inarow++;
                    if( player.PositionsList[d] && player.PositionsList[d]==previousPosition) {
                        positionDaysInARow++;
                    } else {
                        previousPosition = ""; // stops the counting
                    }
                } else if( (nextDay > previousDay) && (nextDay - previousDay <= 1)) {
                    // in case in backwards
                    inarow++;
                    if( player.PositionsList[d] && player.PositionsList[d]==previousPosition) {
                        positionDaysInARow++;
                    } else {
                        previousPosition = ""; // stops the counting
                    }
                } else {
                    break;// no more days in a row
                }
            } else {
                // no more days
                break;
            }
        }
        player.GamesPlayedInARow = inarow;
        player.NextAvilableGame = null;     // to indicate it's not set yet...

        // **************** DO PITCHER CALCULATIONS INCLUDING DAYS REST
        if( pitcherPositions.indexOf( positionPlayed) >= 0) {
            // calculate days rest...
            var outs = 1;
            if( gameStats.OUT ) {
                outs = gameStats.OUT;
            } else if (pitchStats.OUT) {
                outs = pitchStats.OUT;
            }
            var daysRest = 0;

            /*
                Rules for days out:
                outs >= (5*3) --> 3 days
                outs >= (3*3) --> 2 days
                outs >= (2*3) --> 1 day

                if( pitched 3 days in a row) --> 1 day
                if( 3*3 Outs combined over two days) --> at least 1 (if not rules above)
       
            */
           if( positionPlayed=="SP" ) {
               daysRest = 4;
           } else if( outs >= (5*3)) {
               daysRest = 3;
           } else if( outs >= (3*3)) {
               daysRest = 2;
           } else if( (outs >= (2*3)) || (inarow > 2) ){
               daysRest = 1;
           } else if( inarow == 2 ) {
               // then no other rules have applied and he's pitched 2 days in a row!
               if( outs + player.OutsPitchedList[0] >= (3*3)) {
                   daysRest = 1;
               }
           }

           // ok, store everything
           player.OutsPitchedList.unshift(outs);    // store how many outs he pitched today
           player.NextAvilableGame = Math.floor(CalendarDay) + (daysRest+1);
        }

        // get the most likely game date used by this player for this game...
        var newDate = null;
        var pitchersRawDate = null;
        if (gameStats.GameDate || pitchStats && pitchStats.GameDate) {
       
            if (pitchStats && pitchStats.GameDate) {
                var fdate = pitchStats.GameDate.split("/");
            } else {
                var fdate = gameStats.GameDate.split("/");
            } 
            if (fdate[0].length == 1)
                fdate[0] = "0" + fdate[0];
            if (fdate[1].length == 1)
                fdate[1] = "0" + fdate[1];
            var newDate = fdate[2] + fdate[0] + fdate[1];
            var pitchersRawDate = fdate[2] + fdate[0] + fdate[1];
        }

        // **** NOTE: should be using newDate = lineupStats.gameDates[0] so we get the "minors"
        var realNewDate = lineupStats.gameDates ? lineupStats.gameDates[0] : null;
        if( isPitcher && lineupStats.RosterIndex && lineupStats.RosterIndex[1]) {
            if( gameStats.OUT) {
                realNewDate = lineupStats.RosterIndex[1];
                var justDate = realNewDate.replace('Minors', '');
                if( justDate != pitchersRawDate) {
                    realNewDate = pitchersRawDate;
                }
            } else {

                if( playerRawStats.eStats[pitchersRawDate]) {
                    realNewDate = pitchersRawDate;
                } else  {
                    // move forward a date...
                    var moment = require('moment-timezone');
                    var minorDate = moment(pitchersRawDate);
                    minorDate = minorDate.add(1, 'days').format('YYYYMMDD');
    
                    if (playerRawStats.eStats[minorDate + "Minors"]) {
                        realNewDate = minorDate + "Minors";
                    } 
                }
            }
        } else if( isPitcher && playerRawStats && playerRawStats.eStats ) {
            // need to look for minor league date
            if( playerRawStats.eStats[pitchersRawDate]) {
                realNewDate = pitchersRawDate;
            } else {
                // move forward a date...
                var moment = require('moment-timezone');
                var minorDate = moment(pitchersRawDate);
                minorDate = minorDate.add(1, 'days').format('YYYYMMDD');

                if (playerRawStats.eStats[minorDate + "Minors"]) {
                    realNewDate = minorDate + "Minors";
                } 
            }
        }
        if( realNewDate)
            newDate = realNewDate;
        
        // ************** DONE WITH PRELIMINARIES AND PITCHER SPECIFICS.. WORK THROUGH THEIR DATA AND ADD IT UP...
        var hasMulti = false;
        var sourceStats = gameStats;
        if( pitchStats ) {
            sourceStats = pitchStats;
        } else {
            // check to see if there are game stats available...
            if( !isPitcher && !gameStats.GameDate && !lineupStats.useStats ) {
                // reach into the old style game array
                if (lineupStats && lineupStats.gameDates && lineupStats.gameDates.length > 0 && lineupStats.gameDates[0]) {
                    gameStats.GameDate = lineupStats.gameDates[0];
                    newDate = gameStats.GameDate;
                } else {
                    // not sure what to do here.

                }

            }
        }


        if( lineupStats.useStats || (pitchStats && pitchStats.multiPitchGame) || (isPitcher && lineupStats.pStats && lineupStats.pStats.Pitching.multiPitchGame)) {
            hasMulti = true;
        }

        for (var stat in sourceStats ) {
            if (stat == "GameDate" && (sourceStats.GameDate || hasMulti )) {
                
                 // ******************************* GET PARTIAL: FIGURE OUT IF IT'S A PARTIAL GAME HERE *********************************
                var isPartial = false;
                if( isPitcher || pitchStats) {
                    // see if IP doesn't match his game's IP and/or if he has a multiPitchingGames stat handy
                    // might need to loop through them

                    if( lineupStats.pStats && lineupStats.pStats.Pitching  && lineupStats.pStats.Pitching.multiPitchGame ) {
                        // then get the multi pitch information
                        var multiGames = lineupStats.pStats.Pitching.multiPitchGame;
                        for (let m = 0; m < multiGames.length; m++) {
                            if (multiGames[m]) {
                                var gd = multiGames[m].GameDate;
                                ratio = multiGames[m].Ratio;
                                if (multiGames[m].Ratio < 1.0) {
                                    // then a partial
                                    // see there's already a parital here...
                                    if (partialGames && partialGames[gd]) {
                                        partialGames[gd].outsUsed += multiGames[m].OutsUsed;
                                        // see if completely used this game up.
                                        if (partialGames[gd].outsUsed >= multiGames[m].OutsAvailable) {
                                            fullGames.unshift(gd);
                                        }
                                    } else {
                                        // it's a new partialGame    
                                        var uo = multiGames[m].OutsUsed + 0;
                                        var pi = String(gd);
                                        partialGames[String(pi)] = { outsUsed: uo };
                                    }
                                } else {
                                    // it's a full game.
                                    fullGames.unshift(gd);
                                }
                            }
                        }

                    } else {
                        // look to see if the outs pitched match the outs in the raw stats...
                        // check to see if the "newDate" is the same as the raw stats date but not for minor leaguers
                        if( pitchersRawDate && !newDate.includes("Minors") ) {
                            newDate = pitchersRawDate;
                        } else {
                            var isminors = true;
                        }

                        var outs = getOutsFromIP( sourceStats.IP );
                        if( playerRawStats.eStats &&  playerRawStats.eStats[newDate] && playerRawStats.eStats[newDate].Pitching) {
                        var raw = playerRawStats.eStats[newDate].Pitching;
                        var rawOuts = getOutsFromIP(raw.StIP ) + getOutsFromIP(raw.RelIP);
                            if (rawOuts > outs) {
                                // need to register a partial game
                                // then a partial
                                // see there's already a parital here...
                                if (partialGames && partialGames[newDate]) {
                                    partialGames[newDate].outsUsed += outs;
                                    // see if completely used this game up.
                                    if (partialGames[newDate].outsUsed >= rawOuts) {
                                        fullGames.unshift(newDate);
                                    }
                                } else {
                                    // it's a new partialGame    
                                    var pi = String(newDate);
                                    partialGames[String(pi)] = { outsUsed: outs };
                                }

                            } else if (rawOuts < outs) {
                                // need to combine games (including possibly a partial or two)
                            } else {
                                // need to register a fully used game.
                                fullGames.unshift(newDate);
                            } 
                    } else {
                        // something's wrong
                    }

                    }
                } else {
                    // a batter, see if PA doesn't match is game's PA and/or if he has a useStats array/object handy
                    // might need to loop through them.

                    // first look for the useStats object/array
                    if (lineupStats.useStats) {
                        // then look here for what stats got used
                        var multiGames = lineupStats.useStats;

                        for (let m = 0; m < multiGames.length; m++) {
                            if (multiGames[m]) {
                                var gd = multiGames[m].GameDay;
                                ratio = multiGames[m].Ratio;
                                if (multiGames[m].Ratio < 1.0) {
                                    // then a partial
                                    // see there's already a parital here...
                                    if (partialGames && partialGames[gd]) {
                                        partialGames[gd].paUsed += multiGames[m].PA;
                                        // see if completely used this game up.
                                        if (partialGames[gd].paUsed >= multiGames[m].TotalPA) {
                                            fullGames.unshift(gd);
                                        }
                                    } else {
                                        // it's a new partialGame    
                                        var pa = multiGames[m].PA + 0;
                                        var pi = String(gd);
                                        partialGames[String(pi)] = { paUsed: pa };
                                    }
                                } else {
                                    // it's a full game.
                                    fullGames.unshift(gd);
                                }
                            }
                        }

                    } else {
                        //compare to the raw stats
                        var PA = sourceStats.PA;
                        if (playerRawStats.eStats && playerRawStats.eStats[newDate]) {
                            var raw = playerRawStats.eStats[newDate].Batting;
                            var rawPA = raw.PA;

                            if (rawPA != PA) {
                                // then figure it out
                                var ratio = rawPA / PA;
                                // assume ratio < 1
                                // see if there's a partial already
                                    // see there's already a parital here...
                                if (partialGames && partialGames[newDate]) {
                                    partialGames[newDate].paUsed += PA;

                                    // see if completely used this game up.
                                    if (partialGames[newDate].paUsed >= rawPA) {
                                        fullGames.unshift(newDate);
                                    }
                                } else {
                                    // it's a new partialGame    
         
                                    partialGames[newDate] = { paUsed: PA };
                                }
                            } else {
                                // need to register a fully used game.
                                fullGames.unshift(newDate);
                            }
                        } else {
                            // there's an error in the game data?
                        }
                    }


                }

                if (isPartial) {
                    if (isPitcher || pitchStats)
                        partialGames[newDate] = { usedIP: 0 };
                    else
                        partialGames[newDate] = { paUsed: 0 };
                }

                /* reference for above: Format of array and object we're filling in...
                           FullyUsedGames: [ {Season: 2017, fullGames:[]}   ],
                            PartialGames: [  {Season: 2017, 
                                                           partialGames{"20170401" : {paUsed: 3, usedIP: 0}}} ]
                */

            }

            // ********************** count games *********************************
            if (stat == "Pos") {
                if (isPitcher || pitchStats) {
                    if (sourceStats[stat] == "SP") {
                        // starter!
                        if (previousStats["GS"]) {
                            previousStats["GS"]++;
                        } else {
                            previousStats["GS"] = 1;
                        }
                        if (pitchStats) {
                            // starter!
                            if (prevNLPStats["GS"]) {
                                prevNLPStats["GS"]++;
                            } else {
                                prevNLPStats["GS"] = 1;
                            }

                            // and while we're at it, count the game appearance.
                            if (prevNLPStats["G"]) {
                                prevNLPStats["G"]++;
                            } else {
                                prevNLPStats["G"] = 1;
                            }
                        }
                    }   
                    player.PositionsList.push( positionPlayed )
                } else {
                    // count games in the non-pitching position

                    // CALCULATE CATCHER'S DAYS IN A ROW HERE!
                        if (!player.NextAvilableGame) {
                            if (sourceStats[stat] == "CA" && positionDaysInARow >= 10) {
                                player.NextAvilableGame = Math.floor(CalendarDay)+2;
                            } else {
                                // since it's not set, or the catcher's not played catcher in more than 1 game in a row, just let them play tomorrow
                                player.NextAvilableGame = Math.floor(CalendarDay)+1;
                            }
                        }
                    player.PositionsList.push( positionPlayed )

                }
                // and while we're at it, count the game appearance.
                if (previousStats["G"]) {
                    previousStats["G"]++;
                } else {
                    previousStats["G"] = 1;
                }
            }

            // *********************************** deal with decisions ***********************
            if( (isPitcher || pitchStats) && stat=="Decision") {
                var dec = sourceStats[stat];
                if( dec == "W") {
                    stat = "W";
                } else if( dec == "SV") {
                    stat = "SV";
                } else if( dec == "HD") {
                    stat = "HD";
                } else if( dec == "L") {
                    stat = "L";
                } else if ( dec == "BS" ) {
                    stat = "BS";
                } else if ( dec == "BS, L") {
                    stat = "L";

                    // make an exception and add the BS part here...
                    if( !previousStats["BS"]) {
                        previousStats["BS"] = 0;
                    }
                    previousStats["BS"] += 1;
                    if( pitchStats) {       
                        if( !prevNLPStats["BS"]) {
                            prevNLPStats["BS"] = 0;
                        }
                        prevNLPStats["BS"] += 1;
                    }
                }
                sourceStats[stat] = 1;
            }

            // 
            // ok, special values taken care of.
            //
            if (skip.indexOf(stat) == -1) {
                // don't skip
                value = sourceStats[stat];

                if (!pitchStats) {
                    if (!previousStats[stat]) {
                        previousStats[stat] = 0;
                    }
                    if (value == null || value === false || isNaN(value))
                        value = 0;
                    previousStats[stat] = Number(previousStats[stat]) + Number(value);
                } else {
                    if (!prevNLPStats[stat]) {
                        prevNLPStats[stat] = 0;
                    }
                    if (value == null || value === false || isNaN(value))
                        value = 0;
                        prevNLPStats[stat] = Number(prevNLPStats[stat]) + Number(value);
                }
            }
        }

        // ok.. 2nd time through, just for the NL pitcher as batter batting stats...
        if (pitchStats) {
            for (var stat in gameStats) {
                if (skip.indexOf(stat) == -1) {
                    // don't skip
                    value = gameStats[stat];
                    if (!previousStats[stat]) {
                        previousStats[stat] = 0;
                    }
                    if (value == null || value === false || isNaN(value))
                        value = 0;
                    previousStats[stat] = Number(previousStats[stat]) + Number(value);
                }
            }

            // have the batting stats, now update the pitching stats...
            prevNLPStats = this.calculateYTDStats(prevNLPStats, true);
        }

  

        previousStats = this.calculateYTDStats(previousStats, isPitcher);
        return (previousStats);
    },

    // *********************************************************
    //
    // prepare to evalute today's game, and add to the previous game's stats
    // then call addGameStatsToAccStats() to do the work
    // NOTE accPlayers is from the DB and is in NO PARTICULAR ORDER
    // playerIdList helps find the correct index
    //
    // **********************************************************
    updatePlayerAccStats: function( index, finalScore, accPlayers, playerIdList, typeList, lineupStats, playerRawStats, teamNameList, teamConferenceList, callback ) {
        // note accPlayer can be null
        var context = this;
        if( index == accPlayers.length ) {
            // DONE!
            callback();
        } else {
            var player = accPlayers[index];
            var newStats = null;
            var playerIndex = playerIdList.indexOf(player.PlayerId)
            var type = typeList[  playerIndex ];

            var updated = false;
            if( type == 'VBatter' || type == 'HBatter') {
                var batters = finalScore.Boxscore.Home.Batters;
                var pitchers = finalScore.Boxscore.Home.Pitchers;
                if( type == 'VBatter') {
                    batters = finalScore.Boxscore.Visit.Batters;
                    pitchers = finalScore.Boxscore.Visit.Pitchers;
                }

                for( let i=0; i<batters.length; i++) {
                    if( batters[i] && batters[i].PlayerId == player.PlayerId) {

                        // get/create the stats array element that matches the year....
                        var statsYearIndex = -1;
                        for( s=0; s<player.Stats.length; s++) {
                            if(player.Stats[s].Season == finalScore.Season ) {
                                statsYearIndex = s;
                                break;
                            }
                        }
                        if( statsYearIndex == -1 ) {
                            player.Stats.push( {Season: finalScore.Season, Team:teamNamesList[index]} );
                        }

                        if (batters[i].Pos == "CA") {
                            var team = teamNameList[index];
                            var score = finalScore.Boxscore.summary.Visit;
                            if (score.name != team) {
                                score = finalScore.Boxscore.summary.Home;
                            }
                            batters[i]["cRuns"] = score.R;
                            batters[i]["cInnings"] = 9;
                            if (batters[i].PA < 3) {
                                // didn't play the full game, assume he played 2 innings for each PA
                                // so 1 PA==> 2innings, 2PA = 4 innings
                                batters[i]["cInnings"] = batters[i].PA + 1;
                            }
                        }

                        // get/create the full games array element that matches the year
                        var fullStatsIndex = -1;
                        for (s = 0; s < player.FullyUsedGames.length; s++) {
                            if (player.FullyUsedGames[s].Season == finalScore.Season) {
                                fullStatsIndex = s;
                                break;
                            }
                        }
                        if (fullStatsIndex == -1) {
                            // creat the empty list for the season.
                            player.FullyUsedGames.push({ Season: finalScore.Season, fullGames: [] });
                        }

                        // get/create the partial games array element that matches the year
                        var partialStatsIndex = -1;
                        for (s = 0; s < player.PartialGames.length; s++) {
                            if (player.PartialGames[s].Season == finalScore.Season) {
                                partialStatsIndex = s;
                                if(  !player.PartialGames[s].partialGames ) {
                                    player.PartialGames[s]['partialGames'] = {};
                                }
                                break;
                            }
                        }
                        if (partialStatsIndex == -1) {
                            player.PartialGames.push({ Season: finalScore.Season, partialGames: {} });
                            
                        }


                        // ************ unfortunately, have to repeat pitching code here for NL pitchers who bat
                        var pitchBoxStats = null;
                        var pitchingStats = null;
                        for (let pit = 0; pit < pitchers.length; pit++) {
                            if (pitchers[pit] && pitchers[pit].PlayerId == player.PlayerId) {

                                pitchBoxStats = pitchers[pit];
                                // get/create the stats array element that matches the year....
                                var statsYearIndex = -1;
                                for (s = 0; s < player.PitchingStats.length; s++) {
                                    if (player.PitchingStats[s].Season == finalScore.Season) {
                                        statsYearIndex = s;
                                        break;
                                    }
                                }
                                if (statsYearIndex == -1) {
                                    player.PitchingStats.push({ Season: finalScore.Season, Team: teamNamesList[index] });
                                }

                                // get/create the full games array element that matches the year
                                var fullStatsIndex = -1;
                                for (s = 0; s < player.FullyUsedGames.length; s++) {
                                    if (player.FullyUsedGames[s].Season == finalScore.Season) {
                                        fullStatsIndex = s;
                                        break;
                                    }
                                }
                                if (fullStatsIndex == -1) {
                                    player.FullyUsedGames.push({ Season: finalScore.Season, fullGames: [] });
                                }

                                // get/create the partial games array element that matches the year
                                var partialStatsIndex = -1;
                                for (s = 0; s < player.PartialGames.length; s++) {
                                    if (player.PartialGames[s].Season == finalScore.Season) {
                                        partialStatsIndex = s;
                                        if( !player.PartialGames[s].partialGames ) {
                                            player.PartialGames[s]['partialGames'] = {};
                                        }
                                        break;
                                    }
                                }
                                if (partialStatsIndex == -1) {
                                    player.PartialGames.push({ Season: finalScore.Season, partialGames: {} });
                                }

                                player.PitchingStats[statsYearIndex]["Team"] = teamNameList[index];
                                pitchingStats = player.PitchingStats[statsYearIndex];
                                break;
                            }
                        }

                        player.Stats[statsYearIndex]["Team"] = teamNameList[index];
                        context.addGameStatsToAccStats(
                            player,
                            batters[i],
                            pitchBoxStats,         // only use this parameter for players that go both ways...
                            player.Stats[statsYearIndex],
                            pitchingStats,
                            player.FullyUsedGames[fullStatsIndex].fullGames,
                            player.PartialGames[0].partialGames,
                            lineupStats[playerIndex], playerRawStats[playerIndex], null); // null = not pitcher

                        updated = true;
                        break;
                    }
                }
            } else {
                // AL pitchers...
                var pitchers = finalScore.Boxscore.Home.Pitchers;
                if (type == 'VPitcher') {
                    pitchers = finalScore.Boxscore.Visit.Pitchers;
                }
                for (i = 0; i < pitchers.length; i++) {
                    if (pitchers[i] && pitchers[i].PlayerId == player.PlayerId) {

                        // get/create the stats array element that matches the year....
                        var statsYearIndex = -1;
                        for( s=0; s<player.PitchingStats.length; s++) {
                            if(player.PitchingStats[s].Season == finalScore.Season ) {
                                statsYearIndex = s;
                                break;
                            }
                        }
                        if( statsYearIndex == -1 ) {
                            player.PitchingStats.push( {Season: finalScore.Season, Team:teamNamesList[index]} );
                        }

                        // get/create the full games array element that matches the year
                        var fullStatsIndex = -1;
                        for( s=0; s<player.FullyUsedGames.length; s++) {
                            if(player.FullyUsedGames[s].Season == finalScore.Season ) {
                                fullStatsIndex = s;
                                break;
                            }
                        }
                        if( fullStatsIndex == -1 ) {
                            player.FullyUsedGames.push( {Season: finalScore.Season, fullGames:[]} );
                        }

                        // get/create the partial games array element that matches the year
                        var partialStatsIndex = -1;
                        for( s=0; s<player.PartialGames.length; s++) {
                            if(player.PartialGames[s].Season == finalScore.Season ) {
                                partialStatsIndex = s;
                                if( !player.PartialGames[s].partialGames ) {
                                    player.PartialGames[s]['partialGames'] = {};
                                }
                                break;
                            }
                        }
                        if( partialStatsIndex == -1 ) {
                            player.PartialGames.push( {Season: finalScore.Season, partialGames:{}} );
                        }

                        player.PitchingStats[statsYearIndex]["Team"] = teamNameList[index];
                        context.addGameStatsToAccStats( 
                           player,
                            pitchers[i],
                            null,
                            player.PitchingStats[statsYearIndex],  
                            null,
                            player.FullyUsedGames[fullStatsIndex].fullGames, 
                            player.PartialGames[0].partialGames, 
                            lineupStats[playerIndex], playerRawStats[playerIndex], true); // true == pitcher
                        updated = true;
                        break;
                    }
                }
            }
            context.calculateWAR( player, finalScore.Season);

            if (updated) {
                // remember accPlayers[index] is the same as player
                accPlayers[index].markModified("Stats");
                accPlayers[index].markModified("PitchingStats");
                accPlayers[index].markModified("FullyUsedGames");
                accPlayers[index].markModified("PartialGames");
                accPlayers[index].save(function (err, response) {
                    if (err) {
                    }
                    context.updatePlayerAccStats(++index, finalScore, accPlayers, playerIdList, typeList, lineupStats, playerRawStats,teamNameList, teamConferenceList, callback);
 

                })
       
            } else {
                context.updatePlayerAccStats(++index, finalScore, accPlayers, playerIdList, typeList, lineupStats, playerRawStats,teamNameList, teamConferenceList, callback);
            }
        }

    },

    

    // ************************* CREATE PLAYER/TEAM ACCUMULATED STATS DOCUMENT **********************
    // *********************************************************
    //
    // create a new accumulated stats document for this player
    // then call addGameStatsToAccStats() to do the work
    //
    // **********************************************************
    createPlayerAccStats: function( index, finalScore, playerIds, typeList,lineupStats, playerRawStats,  teamNameList, teamConferenceList, callback ) {
        // note accPlayer can be null
       var context = this;
        if( index == playerIds.length ) {
            // DONE!
            callback();
        } else {

              // create an accumulate stats document
                    /*
        var accumulatedStatsSchema = new mongoose.Schema({
            LeagueId: String,
            PlayerId: String,
            MlbId: String,
            FullName: String,
            LastDayPlayed: String,
            GamesPlayedInARow: 0,
            Stats: [
                {Season: 2017, AB: 100, PA: 123, H:89 ... } // includes fielding and baserunning
            ],
            PitchingStats: [
                   {Season: 2017, AB: 100, PA: 123, H:89 ... }
            ],
            FullyUsedGames: [
                {Season: 2017, fullGames:[]}
            ],
            PartialGames: [
                {Season: 2017, partialGames{
                    "20170401" : {paUsed: 3, usedIP: 0}
                }}
            ],
            TeamName: String (this is when the team is being treated like a player),
            TeamId: String,
            TeamConference: String

            });

        */
            var accStats = new AccumulatedStats();
            accStats.LeagueId = finalScore.LeagueId;
            accStats.PlayerId = playerIds[index];
            accStats.Stats[0] = {Season: finalScore.Season, Team: teamNameList[index]};
            accStats.PitchingStats[0] = {Season: finalScore.Season, Team: teamNameList[index]};
            accStats.FullyUsedGames[0] = {Season: finalScore.Season, fullGames:[]};
            accStats.PartialGames[0] = {Season: finalScore.Season, partialGames:{}};
            accStats.MlbId = "";
            accStats.FullName = "";
            accStats.LastDayPlayed = "";
            accStats.GamesPlayedInARow = 0;

            // now update the stats....
            var playerId = playerIds[index];
            var newStats = null;
            var type = typeList[ playerIds.indexOf(playerId)];

            var updated = false;
            if( type == 'VBatter' || type == 'HBatter') {
                var batters = finalScore.Boxscore.Home.Batters;
                var pitchers = finalScore.Boxscore.Home.Pitchers;
                if( type == 'VBatter') {
                    batters = finalScore.Boxscore.Visit.Batters;
                    pitchers = finalScore.Boxscore.Visit.Pitchers;
                }

                for( i=0; i<batters.length; i++) {
                    if(batters[i] &&  batters[i].PlayerId == playerId) {
                        
                        var pitchBoxStats = null;
                        for (let pit = 0; pit < pitchers.length; pit++) {
                            if (pitchers[pit] && pitchers[pit].PlayerId == playerId) {
                                pitchBoxStats = pitchers[pit];
                                break;
                            }
                        }

                        // see if a pitcher too
                        accStats.FullName = batters[i].FullName;
                        accStats.MlbId = batters[i].MlbId;
                        accStats.PartialGames[0].partialGames = {};
                        context.addGameStatsToAccStats( 
                            accStats, 
                            batters[i], 
                            pitchBoxStats,
                            accStats.Stats[0], 
                            accStats.PitchingStats[0], 
                            accStats.FullyUsedGames[0].fullGames, 
                            accStats.PartialGames[0].partialGames, 
                            lineupStats[index], playerRawStats[index], false); // false = non-pitcher
                        updated = true;
                        break;
                    }
                }
            } else {
                // pitchers...
                var pitchers = finalScore.Boxscore.Home.Pitchers;
                if (type == 'VPitcher') {
                    pitchers = finalScore.Boxscore.Visit.Pitchers;
                }
                for (i = 0; i < pitchers.length; i++) {
                    if (pitchers[i] && pitchers[i].PlayerId == playerId) {
                        accStats.FullName = pitchers[i].FullName;
                        accStats.MlbId = pitchers[i].MlbId;
                        context.addGameStatsToAccStats(
                            accStats, 
                            pitchers[i], 
                            null,
                            accStats.PitchingStats[0],  
                            null,
                            accStats.FullyUsedGames[0].fullGames, 
                            accStats.PartialGames[0].partialGames, 
                            lineupStats[index], playerRawStats[index], true); // true = pitchers
                        updated = true;
                        break;
                    }
                }
            }
            context.calculateWAR( accStats, finalScore.Season );

            AccumulatedStats.create( accStats, function(err, newAccStats ){
                context.createPlayerAccStats(++index, finalScore, playerIds, typeList, lineupStats, playerRawStats, teamNameList, teamConferenceList, callback);
            });

        }

    },

    // *********************************************************
    //
    // entry point to accumulate a team's total stats based on the players in a game
    //
    // *******************************************************************
    updateTeamAccStats: function( index, finalScore, accTeams, callback ) {
        // note accPlayer can be null
        var context = this;
        if( index == accTeams.length ) {
            // DONE!
            callback();
        } else {
            var team = accTeams[index];
            var newStats = null;
  
            var side = "Home";
            var batters = finalScore.Boxscore.Home.BattingTotals;
            var pitchers = finalScore.Boxscore.Home.PitchingTotals;
            if( finalScore.Boxscore.summary.Visit.name == accTeams[index].TeamName) {
                side = "Visit";
                batters = finalScore.Boxscore.Visit.BattingTotals;
                pitchers = finalScore.Boxscore.Visit.PitchingTotals;
            }

            // get/create the stats array element that matches the year....
            var statsYearIndex = -1;
            for (s = 0; s < accTeams[index].Stats.length; s++) {
                if (accTeams[index].Stats[s].Season == finalScore.Season) {
                    statsYearIndex = s;
                    break;
                }
            }
            if (statsYearIndex == -1) {
                accTeams[index].Stats.push({ Season: finalScore.Season });
            }

            var pStatsYearIndex = -1;
            for (s = 0; s < accTeams[index].PitchingStats.length; s++) {
                if (accTeams[index].PitchingStats[s].Season == finalScore.Season) {
                    pStatsYearIndex = s;
                    break;
                }
            }
            if (pStatsYearIndex == -1) {
                accTeams[index].PitchingStats.push({ Season: finalScore.Season });
            }

            context.addGameStatsToAccStats( batters, accTeam[index].Stats[statsYearIndex],  
                            null,  null, false);


             context.addGameStatsToAccStats( pitchers, accTeams[index].PitchingStats[pStatsYearIndex],  
                            null, null, true);
                        updated = true;
           
            if (updated) {
                accTeams[index].markModified("Stats");
                accTeams[index].markModified("PitchingStats");
                accTeams[index].save(function (err, response) {
                    if (err) {
                    }
                    context.updateTeamAccStats(++index, finalScore, accTeams, callback);
 

                })
            } else {
                context.updateTeamAccStats(++index, finalScore, accTeams, allback);
            }
        }

    },

    createTeamAccStats: function( index, finalScore, teamNameList, teamIdList, teamConferenceList, callback ) {
        // note accPlayer can be null
       var context = this;
        if( index == teamNameList.length ) {
            // DONE!
            callback();
        } else {

              // create an accumulate stats document FOR A TEAM!
                    /*
        var accumulatedStatsSchema = new mongoose.Schema({
            LeagueId: String,
            PlayerId: String,
            MlbId: String,
            FullName: String,
            LastDayPlayed: String,
            GamesPlayedInARow: 0,
            Stats: [
                {Season: 2017, AB: 100, PA: 123, H:89 ... } // includes fielding and baserunning
            ],
            PitchingStats: [
                   {Season: 2017, AB: 100, PA: 123, H:89 ... }
            ],
            FullyUsedGames: [
                {Season: 2017, fullGames:[]}
            ],
            PartialGames: [
                {Season: 2017, partialGames{
                    "20170401" : {paUsed: 3, usedIP: 0}
                }}
            ],
            TeamName: String (this is when the team is being treated like a player),
            TeamId: String,
            TeamConference: String

            });

        */
            var accStats = new AccumulatedStats();
            accStats.LeagueId = finalScore.LeagueId;
            accStats.PlayerId = null;
            accStats.Stats[0] = {Season: finalScore.Season, Team: teamNameList[index]};
            accStats.PitchingStats[0] = {Season: finalScore.Season, Team: teamNameList[index]};
            accStats.FullyUsedGames = [];
            accStats.PartialGames = [];
            accStats.TeamName = teamNameList[index];
            accStats.TeamId   = teamIdList[index];
            accStats.Confference = teamConferenceList[index];

            // now update the stats....
            var playerId = playerIds[index];
            var newStats = null;
            var type = typeList[ playerIds.indexOf(playerId)];

            var updated = false;

            var side = "Home";
            var batters = finalScore.Boxscore.Home.BattingTotals;
            var pitchers = finalScore.Boxscore.Home.PitchingTotals;
            if( finalScore.Boxscore.summary.Visit.name == teamNameList[index]) {
                side = "Visit";
                batters = finalScore.Boxscore.Visit.BattingTotals;
                pitchers = finalScore.Boxscore.Visit.PitchingTotals;
            }

            context.addGameStatsToAccStats( batters[i], accStats.Stats[0], accStats.FullyUsedGames[0].fullGames, accStats.PartialGames[0].partialGames, false);
            context.addGameStatsToAccStats(pitchers[i], accStats.PitchingStats[0], accStats.FullyUsedGames[0].fullGames, accStats.PartialGames[0].partialGames, true);

             
            AccumulatedStats.create( accStats, function(err, newAccStats ){
                context.createTeamAccStats(++index, finalScore, teamNameList, teamIdsList, teamConferenceList, callback);
            });

        }

    },

    // ***********************************************************************************
    //
    // entry point for saving the player stats from a game
    //
    // ***********************************************************************************

    saveGameStats: function (finalScore, callback) {

          var context = this;

        // example of gettings sorted stats based on season and sorted field.
        // db.getCollection('aatest').find({"stats.season":2018},{name:1, PlayerId: 1,"stats.$":1}).sort({"stats.games":-1})
        /*
        var accumulatedStatsSchema = new mongoose.Schema({
            LeagueId: String,
            PlayerId: String,
            MlbId: String,
            FullName: String,
            LastDayPlayed: String,
            GamesPlayedInARow: 0,
            Stats: [
                {Season: 2017, team:"rDodgers", AB: 100, PA: 123, H:89 ... }
            ],
            PitchingStats: [],
            FullyUsedGames: [
                {Season: 2017, fullGames:[]}
            ],
            PartialGames: [
                {Season: 2017, partialGames{
                    "20170401" : {paUsed: 3, usedIP: 0}
                }}
            ],
            TeamName: String (this is when the team is being treated like a player),
            TeamId: String,
            TeamConference: String

            });

        */

 
        // 1) create a list of players....
        var playerIdList = [];
        var lineupStats = [];
        var playerRawStats = [];
        var typeList = [];
        var teamIdList = [];
        var teamNameList = [];
        var teamConferenceList = [];
        var homeName = finalScore.Boxscore.summary.Home.name;
        var visitName = finalScore.Boxscore.summary.Visit.name;
        var homeConference = "National";
        if( teamInfo.ALTeams.indexOf( homeName) >= 0) {
            homeConference = "American";
        }
        var visitConference = "National";
        if( teamInfo.ALTeams.indexOf( visitName) >= 0) {
            visitConference = "American";
        }

        function getBattingOrderStats( playerId, battingOrder ) {
            for( let b=0; b<battingOrder.length; b++ ) {
                if( battingOrder[b] && battingOrder[b].PlayerId == playerId)
                return( battingOrder[b]);
            }
            
            return({DidNotBat:true});
        }

        function getUsedPitcherStats( playerId, usedPitchers ) {
            for( let b=0; b<usedPitchers.length; b++ ) {
                if( usedPitchers[b] && usedPitchers[b].PlayerId == playerId)
                return( usedPitchers[b]);
            }
            
            return({DidNotPitch:true});
        }

        function getDepthStats( playerId, depthChart ) {
            for( let p=0; p<depthChart.length; p++ ) {
                if( depthChart[p] && depthChart[p].Players) {
                    for( let d=0; d<depthChart[p].Players.length; d++) {
                        if(depthChart[p].Players[d] && depthChart[p].Players[d].PlayerId == playerId) {
                            return (depthChart[p].Players[d]);
                        }
                    }
                }
            }
            return( {NoRawData:true});
        }

        // ********* GET BATTER STATS...
        for (p = 0; p < finalScore.Boxscore.Visit.Batters.length; p++) {
            if (finalScore.Boxscore.Visit.Batters[p]) {
                playerIdList.push(finalScore.Boxscore.Visit.Batters[p].PlayerId);
                typeList.push('VBatter');
                teamNameList.push(visitName);
                if (finalScore.Boxscore.Visit.Batters[p].Pose != "P") {
                    lineupStats.push(
                        getBattingOrderStats(
                            finalScore.Boxscore.Visit.Batters[p].PlayerId,
                            finalScore.TeamData.visitTeam.batters.battingOrder
                        )
                    );
                } else {
                    lineupStats.push(
                        getUsedPitcherStats(
                            finalScore.Boxscore.Visit.Batters[p].PlayerId,
                            finalScore.TeamData.visitTeam.pitchingStaff.pitchersUsed
                        )
                    );
                }
                playerRawStats.push(
                    getDepthStats(finalScore.Boxscore.Visit.Batters[p].PlayerId,
                        finalScore.TeamData.visitRoster
                    )
                );
                teamConferenceList.push(visitConference);
            }
        }
        for (p = 0; p < finalScore.Boxscore.Home.Batters.length; p++) {
            if (finalScore.Boxscore.Home.Batters[p]) {
                playerIdList.push(finalScore.Boxscore.Home.Batters[p].PlayerId);
                typeList.push('HBatter');
                teamNameList.push(homeName);
                if (finalScore.Boxscore.Home.Batters[p].Pos != "P") {
                    lineupStats.push(
                        getBattingOrderStats(
                            finalScore.Boxscore.Home.Batters[p].PlayerId,
                            finalScore.TeamData.homeTeam.batters.battingOrder
                        )
                    );
                } else {
                    lineupStats.push( 
                        getUsedPitcherStats(
                        finalScore.Boxscore.Home.Batters[p].PlayerId,
                        finalScore.TeamData.homeTeam.pitchingStaff.pitchersUsed
                        )
                    );
                }
                playerRawStats.push(
                    getDepthStats(
                        finalScore.Boxscore.Home.Batters[p].PlayerId,
                        finalScore.TeamData.homeRoster
                    )
                );
                teamConferenceList.push(homeConference);
            }
        }

        // *********** GET PITCHER STATS
        // note, don't put them in if they batted!
        for (p = 0; p < finalScore.Boxscore.Visit.Pitchers.length; p++) {
            if (finalScore.Boxscore.Visit.Pitchers[p] && (finalScore.Game.dhGame || playerIdList.indexOf(finalScore.Boxscore.Visit.Pitchers[p].PlayerId)==-1)) {
                playerIdList.push(finalScore.Boxscore.Visit.Pitchers[p].PlayerId);
                typeList.push('VPitcher');
                lineupStats.push( 
                    getUsedPitcherStats(
                    finalScore.Boxscore.Visit.Pitchers[p].PlayerId,
                    finalScore.TeamData.visitTeam.pitchingStaff.pitchersUsed
                    )
                );
                playerRawStats.push(
                    getDepthStats(
                    finalScore.Boxscore.Visit.Pitchers[p].PlayerId,
                    finalScore.TeamData.visitRoster
                    )
                 );
                teamNameList.push(visitName);
                teamConferenceList.push(visitConference);
            }
        }
        for (p = 0; p < finalScore.Boxscore.Home.Pitchers.length; p++) {
            if (finalScore.Boxscore.Home.Pitchers[p]  && (finalScore.Game.dhGame || playerIdList.indexOf(finalScore.Boxscore.Home.Pitchers[p].PlayerId)==-1)) {

                playerIdList.push(finalScore.Boxscore.Home.Pitchers[p].PlayerId);
                typeList.push('HPitcher');
                lineupStats.push( 
                    getUsedPitcherStats(
                    finalScore.Boxscore.Home.Pitchers[p].PlayerId,
                    finalScore.TeamData.homeTeam.pitchingStaff.pitchersUsed
                    )
                );
                playerRawStats.push(
                    getDepthStats(
                    finalScore.Boxscore.Home.Pitchers[p].PlayerId,
                    finalScore.TeamData.homeRoster
                    )
                 );
                teamNameList.push(homeName);
                teamConferenceList.push(homeConference);
            }
        }

        // 2) grab the players from the accumulatedStats collection
        AccumulatedStats.find({LeagueId: finalScore.LeagueId, PlayerId: {$in:playerIdList}},
            function(err, accPlayers){
                if( err) {
                    callback( err );
                } else {

                    // first update all the players that already have a record
                    context.updatePlayerAccStats(0, finalScore, accPlayers, playerIdList, typeList, lineupStats, playerRawStats,  teamNameList, teamConferenceList, function(){

                        // second, remove the players which just updated...
                        // go backwards as pitchers might be in the list twice!
                        for( i=(accPlayers.length-1); i>=0; i-- ) {

                            // get index and remove the player.
                            var pindex = playerIdList.lastIndexOf(accPlayers[i].PlayerId);
                            playerIdList.splice(pindex, 1);
                            typeList.splice(pindex, 1);
                            teamNameList.splice(pindex, 1);
                            teamConferenceList.splice(pindex,1);
                            lineupStats.splice(pindex, 1);
                            playerRawStats.splice(pindex, 1);

                            // in case the pitcher batted...
                            pindex = playerIdList.lastIndexOf(accPlayers[i].PlayerId);
                            if (pindex >= 0) {
                                playerIdList.splice(pindex, 1);
                                typeList.splice(pindex, 1);
                                lineupStats.splice(pindex, 1);
                                playerRawStats.splice(pindex, 1);
                                teamNameList.splice(pindex, 1);
                                teamConferenceList.splice(pindex,1);
                            }

                        }

                        // third, create the new players
                        context.createPlayerAccStats(0, finalScore, playerIdList, typeList, lineupStats, playerRawStats, teamNameList, teamConferenceList, function(){
                            callback("success");
                        })
                    });
                }
        });
    },

    // iterator to work through all games in a game day
    saveGameStatsForDay: function( index, boxscores, callback ) {
        var context = this;
        if( index >= boxscores.length ) {
            callback( null, "success");
        } else {
            this.saveGameStats( boxscores[index], function(msg){
                context.saveGameStatsForDay(++index, boxscores, callback);
            })
        }
    }
}