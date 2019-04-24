var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var playerHelper = require('../helpers/playerHelper');
var playerStatus = require('../helpers/playerStatus');
var simulationHelper = require('../helpers/simulationHelper');
var setPitcherHelper = require('../simulator/set-pitchers-helper');
var setLineupHelper = require('../simulator/set-lineup-helper');
var leagueHelper = require('../helpers/leagueHelper');
var teamHelper = require('../helpers/teamHelper');
var rosterHelper = require('../helpers/rosterHelper');
var gameHelper = require('../helpers/gameHelper');
var Import = require("../helpers/importHelper");

var sim = require('../helpers/simulation');

var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var AccumulatedStats = mongoose.model('AccumulatedStats');
var UsedGame = mongoose.model("UsedGame");
var Roster = mongoose.model("Roster");
var RosterLineup = mongoose.model("RosterLineup");
var Boxscore = mongoose.model("Boxscore");
var Standings = mongoose.model("Standings");
var Progress = mongoose.model("Progress");
var GameDay = mongoose.model("GameDay");
var Schedule = mongoose.model("Schedule");

var moment = require('moment-timezone');
moment.tz.setDefault("America/Los_Angeles");

///
/// ******************* getting/updating used game stats ****************
///
router.get('/api/usedgames/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {


        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// *********************************** GET ONE PLAYER'S ACCUMULATED STATS  ************************************ //
router.get('/api/accumulated/getplayer', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            // playertype (batter/pitcher), position, minIP, minPA, stat, page, pagesize
            var leagueId = req.query.lid;
            var playerId = req.query.playerId;

            AccumulatedStats.findOne({ LeagueId: leagueId, PlayerId: playerId }, function (err, stats) {
                if (err) {
                    res.status(201).json({ status: 201, msg: "search failed" });
                } else {
                    if (stats.length == 0) {
                        res.status(200).json({ status: 200, msg: "no results", stats: null });
                    } else {
                        res.status(200).json({ status: 200, msg: "success", stats: stats });
                    }
                }
            });

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// *********************************** GET INDIVIDUAL LEADER LISTS  ************************************ //
router.get('/api/accumulated/getleaders', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            // playertype (batter/pitcher), position, minIP, minPA, stat, page, pagesize
            var leagueId = req.query.lid;
            var playertype = req.query.playertype;
            var season = req.query.season;
            var position = req.query.position;
            var pitchertype = req.query.pitchertype;
            var minIP = Number(req.query.minIP);
            var minPA = Number(req.query.minPA);
            var stat = req.query.stat;
            var page = Number(req.query.page);
            var pagesize = Number(req.query.pagesize);

            var direction = -1;  // -1 is from highest... set to 1 for things like ERA
            if (stat == "ERA")
                direction = 1;

            var query = { LeagueId: leagueId, };

            var sortby = {};



            if (playertype == 'Pitchers') {
                query["PitchingStats." + stat] = { $exists: true };
                query["PitchingStats.IP"] = { $exists: true, $gte: minIP };
                query["PitchingStats.Season"] = season;
                if (req.query.teamName != "" && req.query.teamName != "All") {
                    query["PitchingStats.Team"] = req.query.teamName;
                }
                sortby["PitchingStats." + stat] = direction;
                if (pitchertype != 'All') {
                    var qp = "PitchingStats.Position-" + pitchertype;
                    query[qp] = { $exists: true, $gte: 1 };
                }

                AccumulatedStats.count(query, function (err, count) {
                    AccumulatedStats.find(query, { FullName: 1, "PitchingStats.$": 1 }, function (err, stats) {
                        if (err) {
                            res.status(299).json({ status: 299, msg: "search failed" });
                        } else {
                            if (stats.length == 0) {
                                res.status(200).json({ status: 200, msg: "no results", stats: stats, count: count });
                            } else {
                                res.status(200).json({ status: 200, msg: stats.length + " results", stats: stats, count: count });
                            }
                        }

                    }).skip(page * pagesize).sort(sortby).limit(pagesize);
                });

            } else {

                // non-pitchers
                query["Stats." + stat] = { $exists: true };
                query["Stats.PA"] = { $exists: true, $gte: minPA };
                query["Stats.Season"] = season;
                if (req.query.teamName != "" && req.query.teamName != "All") {
                    query["Stats.Team"] = req.query.teamName;
                }
                sortby["Stats." + stat] = direction;
                if (position != "All") {
                    var qp = "Stats.Position-" + position;
                    query[qp] = { $exists: true, $gte: 1 };
                }
                AccumulatedStats.count(query, function (err, count) {
                    AccumulatedStats.find(query, { FullName: 1, "Stats.$": 1 }, function (err, stats) {
                        if (err) {
                            res.status(299).json({ status: 299, msg: "search failed" });
                        } else {
                            if (stats.length == 0) {
                                res.status(200).json({ status: 200, msg: "no results", stats: stats, count: count });
                            } else {
                                res.status(200).json({ status: 200, msg: stats.length + " results", stats: stats, count: count });
                            }
                        }

                    }).skip(page * pagesize).sort(sortby).limit(pagesize);
                });


            }

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// *****************************************************************************
//
// get eligilibity for inactive players only
//
// *****************************************************************************
router.get('/api/usedgames/getAvailableGamesForInactives', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var leagueId = req.query.lid;
            var teamId = req.query.tid;
            RosterLineup.findOne({ LeagueId: leagueId, TeamId: teamId }, { InactivePlayers: 1, NonRosterPlayers: 1 }, function (err, rosterLineup) {
                if (!err && rosterLineup && rosterLineup.InactivePlayers) {
                    var showall = true;
                    var inactivePitchers = [];
                    var inactivePlayers = [];
                    var nonRosterPitchers = [];
                    var nonRosterPlayers = [];
                    for (let i = 0; i < rosterLineup.InactivePlayers.length; i++) {
                        nextPlayer = rosterLineup.InactivePlayers[i];
                        if (nextPlayer.eStats || showall) {
                            if (nextPlayer.Primary && nextPlayer.Primary[0] == "P"
                                || nextPlayer.Secondary && nextPlayer.Secondary[0] == "P"
                                || nextPlayer.Tertiary && nextPlayer.Tertiary[0] == "P"
                                || nextPlayer.hightestIPMinors) {
                                inactivePitchers.push(nextPlayer);
                            } else {
                                inactivePlayers.push(nextPlayer);
                            }
                        }
                    }
                    rosterHelper.putInDisplayStats(inactivePitchers);
                    rosterHelper.putInDisplayStats(inactivePlayers);

                    for (let i = 0; i < rosterLineup.NonRosterPlayers.length; i++) {
                        nextPlayer = rosterLineup.NonRosterPlayers[i];
                        if (nextPlayer.eStats || showall) {
                            if (nextPlayer.Primary && nextPlayer.Primary[0] == "P"
                                || nextPlayer.Secondary && nextPlayer.Secondary[0] == "P"
                                || nextPlayer.Tertiary && nextPlayer.Tertiary[0] == "P"
                                || nextPlayer.highestIPMinors) {
                                nonRosterPitchers.push(nextPlayer);
                            } else {
                                nonRosterPlayers.push(nextPlayer);
                            }
                        }
                    }
                    rosterHelper.putInDisplayStats(nonRosterPitchers);
                    rosterHelper.putInDisplayStats(nonRosterPlayers);
                    res.status(200).json({ status: 200, msg: "success", inactivePitchers: inactivePitchers, inactivePlayers: inactivePlayers, nonRosterPitchers: nonRosterPitchers, nonRosterPlayers: nonRosterPlayers });
                } else {
                    res.status(401).json({ status: 401, msg: "no data found" });
                }


            });
        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// *****************************************************************************
//
// set eligilibity for inactive players only
//
// *****************************************************************************
router.get('/api/usedgames/getAvailableGamesForInactivesProgress', function (req, res) {
    /*
    Progress.findOne({ _id: req.query.progressId }, function (err, progress) {
        if (err) {
            res.status(200).json({ status: 200, msg: "fail", progress: null });
        } else {
            res.status(200).json({ status: 200, msg: "success", progress: progress });
        }
    });
    */
    utils.getProgress("elibility", function (err, progress) {
        var response = { PercentComplete: Number(progress) };
        if (err) {
            res.status(200).json({ status: 200, msg: "fail", progress: null });
        } else {
            res.status(200).json({ status: 200, msg: "success", progress: response });
        }
    })

});

router.get('/api/usedgames/setAvailableGamesForInactives', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var season = req.query.season;
            var leagueId = req.query.lid;
            var teamId = req.query.tid;
            var activeOnly = req.query.activeOnly;
            var game = null;
            if (req.query.game)
                game = JSON.parse(req.query.game);
            var teamName = req.query.teamName;

            var gameRange = { From: 20170301, To: 20170510 };
            if (req.query.From) {
                gameRange.From = req.query.From;
            }
            if (req.query.To) {
                gameRange.To = req.query.To;
            } else {
                var moment = require('moment-timezone');
                moment.tz.setDefault("America/Los_Angeles");

                var endDate = moment();
                endDate = endDate.subtract(3, "days");
                endDate = endDate.format("YYYYMMDD");
                // could be endDate = = moment().subtract(3, "days").format("YYYYMMDD");
            }

            utils.startProgress(leagueId, teamId, teamName, function (err, id) {
                res.status(200).json({ status: 200, msg: "success", progressId: id });


                Roster.findOne({ LeagueId: leagueId, TeamId: teamId }, { FortyManNL: 1, NonRoster: 1 }, function (err, roster) {
                    if (err) {
                        res.status(500).json({ status: 500, msg: "error", err: err });
                    } else {

                        var nonRosterData = [];
                        for (i = 0; i < roster.NonRoster.length; i++) {
                            nonRosterData.push(rosterHelper.createRosterLineupPlayer(leagueId, roster.NonRoster[i], season));
                            if (i >= 300) {
                                break;
                            }
                        }

                        var inactiveData = [];
                        var inactive = roster.FortyManNL.filter(function (value) {
                            return value.Status != playerStatus.ActiveRoster;
                        });
                        for (i = 0; i < inactive.length; i++) {
                            inactiveData.push(rosterHelper.createRosterLineupPlayer(leagueId, inactive[i], season));
                        }

                        // forces iterators to update progress
                        gameRange["leagueId"] = leagueId;
                        gameRange['teamId'] = teamId;
                        gameRange['teamName'] = teamName;
                        gameRange['increment'] = 100 / ((inactiveData.length + nonRosterData.length) * 5);

                        utils.updateProgress(null, leagueId, teamId, teamName, 1.5, null);
                        gameHelper.setInactiveEligibilty(inactiveData, nonRosterData, gameRange, leagueId, season, teamName, teamId, function (err, inactiveUpdate) {
                            console.log("completed inactive eligibility...");
                            gameHelper.setNonRosterEligibility(inactiveUpdate, nonRosterData, gameRange, leagueId, season, teamName, teamId, function (err, result) {
                                console.log("completed non-roster eligibility...");
                                // res.status(200).json({ status: 200, msg: "success"}); 
                                utils.updateProgress(null, leagueId, teamId, teamName, 100, null);
                            });
                        });

                    }
                });
            });
        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});
// *********************************** SET AVAILABLE GAMES FOR ROSTER (ACTIVE PLAYERS ONLY) ************************************ //

//  called from Admin's PREVIEW GAME button for ACTIVE PLAYERS ONLY
// and from Game Day Wizard

router.get('/api/usedgames/setAvailableGamesForRoster', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            console.log("start set available games..." + req.query.teamName);
            var season = req.query.season;
            var leagueId = req.query.lid;
            var teamId = req.query.tid;
            var activeOnly = req.query.activeOnly;
            var game = null;
            if (req.query.game)
                game = JSON.parse(req.query.game);
            var teamName = req.query.teamName;

            var gameRange = { From: 20170301, To: 20170510 };
            if (req.query.From) {
                gameRange.From = req.query.From;
            }
            if (req.query.To) {
                gameRange.To = req.query.To;
            } else {
                var moment = require('moment-timezone');
                moment.tz.setDefault("America/Los_Angeles");

                var endDate = moment();
                endDate = endDate.subtract(3, "days");
                endDate = endDate.format("YYYYMMDD");
                // could be endDate = = moment().subtract(3, "days").format("YYYYMMDD");
            }

            Roster.findOne({ LeagueId: leagueId, TeamId: teamId }, { TeamAbbr: 1, TeamId: 1, FortyManNL: 1, DepthChartNL: 1, BattingOrderAL: 1, BattingOrderNL: 1 }, function (err, roster) {
                if (err) {

                } else {
                    var activePlayers = [];
                    var activePlayerGamesUsed = [];

                    for (i = 0; i < roster.FortyManNL.length; i++) {

                        var newPlayer = rosterHelper.createRosterLineupPlayer(leagueId, roster.FortyManNL[i], season);
                        if (roster.FortyManNL[i].Status == playerStatus.ActiveRoster) {

                            activePlayerGamesUsed.push(newPlayer);
                            activePlayers.push(newPlayer.PlayerId);

                        }
                    }

                    // finally, get active players...
                    playerHelper.getEligiblePositions(0, activePlayerGamesUsed, gameRange, function (playersWithEligibility) {
                        console.log("start batting...");
                        playerHelper.getAvailableBatting(0, activePlayerGamesUsed, "ML", gameRange, function (players) {
                            console.log("start pitching...");
                            playerHelper.getAvailablePitching(0, activePlayerGamesUsed, "ML", gameRange, function (players) {
                                console.log("start fielding...");
                                playerHelper.getAvailableFielding(0, activePlayerGamesUsed, "ML", gameRange, function (players) {
                                    console.log("start baserunning...");
                                    playerHelper.getAvailableBaserunning(0, activePlayerGamesUsed, "ML", gameRange, function (players) {

                                        console.log("start minors...");
                                        playerHelper.getAvailableBatting(0, activePlayerGamesUsed, "Minors", gameRange, function (players) {
                                            playerHelper.getAvailablePitching(0, activePlayerGamesUsed, "Minors", gameRange, function (players) {
                                                playerHelper.getAvailableFielding(0, activePlayerGamesUsed, "Minors", gameRange, function (players) {
                                                    playerHelper.getAvailableBaserunning(0, activePlayerGamesUsed, "Minors", gameRange, function (players) {

                                                        RosterLineup.update(
                                                            { LeagueId: leagueId, TeamId: teamId },
                                                            {
                                                                LeagueId: leagueId,
                                                                TeamdId: teamId,
                                                                TeamName: teamName,
                                                                Players: players,
                                                                // InactivePlayers: inactivePlayers,
                                                                // NonRosterPlayers: nonRosterPlayers,
                                                                CreatedUTC: new Date().toISOString(),
                                                                //  Bench: roster.Bench ? roster.Bench : null,
                                                                //  DepthChartNL: roster.DepthChartNL ? roster.DepthChartNL : null,
                                                                //  BattingOrderAL: roster.BattingOrderAL ? roster.BattingOrderAL : null,
                                                                //  BattingOrderNL: roster.BattingOrderNL ? roster.BattingOrderNL: null

                                                            },
                                                            { upsert: true }, function (err, response) {
                                                                if (err) {

                                                                } else {
                                                                    var positionReference = ["C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "SP", "P", "CL", "DH", "PH"];

                                                                    // stuff in the lineups and default depth chart
                                                                    // get available positions... 
                                                                    var qry = { LeagueId: leagueId, TeamId: teamId };
                                                                    RosterLineup.findOne({ LeagueId: leagueId, TeamId: teamId }, function (err, dbRoster) {
                                                                        if (err) {
                                                                            res.status(500).json({ status: 500, msg: err.errmsg, isValid: false });
                                                                        } else {
                                                                            if (!dbRoster) {
                                                                                dbRoster = {
                                                                                    Players: []
                                                                                }
                                                                            }
                                                                            else {
                                                                                // if batting orders, update the stats
                                                                                if (roster.BattingOrderNL) {
                                                                                    for (var b = 0; b < roster.BattingOrderNL.length; b++) {
                                                                                        if (roster.BattingOrderNL[b]) {
                                                                                            for (var p = 0; p < dbRoster.Players.length; p++) {
                                                                                                if (roster.BattingOrderNL[b].PlayerId == dbRoster.Players[p].PlayerId) {
                                                                                                    var sp = roster.BattingOrderNL[b].Position;
                                                                                                    roster.BattingOrderNL[b] == dbRoster.Players[p]
                                                                                                    roster.BattingOrderNL[b].Position = sp;
                                                                                                    break;
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                    dbRoster.BattingOrderNL = roster.BattingOrderNL;
                                                                                    roster.markModified("BattingOrderNL");
                                                                                    dbRoster.markModified("BattingOrderNL");
                                                                                }

                                                                                if (roster.BattingOrderAL) {
                                                                                    for (var b = 0; b < roster.BattingOrderAL.length; b++) {
                                                                                        if (roster.BattingOrderAL[b]) {
                                                                                            for (var p = 0; p < dbRoster.Players.length; p++) {
                                                                                                if (roster.BattingOrderAL[b].PlayerId == dbRoster.Players[p].PlayerId) {
                                                                                                    var sp = roster.BattingOrderAL[b].Position;
                                                                                                    roster.BattingOrderAL[b] == dbRoster.Players[p]
                                                                                                    roster.BattingOrderAL[b].Position = sp;
                                                                                                    break;
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                    dbRoster.BattingOrderAL = roster.BattingOrderAL;
                                                                                    dbRoster.markModified("BattingOrderAL");
                                                                                    roster.markModified("BattingOrderAL");
                                                                                }

                                                                                // stil have the proper roster and see if there's already a depth chart...
                                                                                if (roster.DepthChartNL == null) {

                                                                                    // create the default depth chart... .. and lineups...
                                                                                    rosterHelper.getMasterPlayerPosition(0, dbRoster, function (MProster) {
                                                                                        var depth = rosterHelper.getDepthChartTemplate();
                                                                                        depth[0].Players = rosterHelper.getPlayersByPosition("CA", MProster.Players);
                                                                                        depth[1].Players = rosterHelper.getPlayersByPosition("1B", MProster.Players);
                                                                                        depth[2].Players = rosterHelper.getPlayersByPosition("2B", MProster.Players);
                                                                                        depth[3].Players = rosterHelper.getPlayersByPosition("3B", MProster.Players);
                                                                                        depth[4].Players = rosterHelper.getPlayersByPosition("SS", MProster.Players);
                                                                                        depth[5].Players = rosterHelper.getPlayersByPosition("LF", MProster.Players);
                                                                                        depth[6].Players = rosterHelper.getPlayersByPosition("CF", MProster.Players);
                                                                                        depth[7].Players = rosterHelper.getPlayersByPosition("RF", MProster.Players);
                                                                                        depth[8].Players = rosterHelper.getPlayersByPosition("P", MProster.Players);
                                                                                        depth[9].Players = depth[8].Players.length > 5 ? depth[8].Players.splice(4) : [];
                                                                                        depth[10].Players = depth[8].Players.length > 0 ? depth[8].Players.splice(0, 1) : [];
                                                                                        depth[8].Players.length > 5 ? depth[8].Players.splice(0, 4) : depth[8].Players;

                                                                                        depth[11].Players = rosterHelper.getPlayersByPosition("DH", MProster.Players);

                                                                                        roster.DepthChartNL = depth;
                                                                                        roster.markModified("DepthChartNL");

                                                                                        dbRoster.DepthChartNL = depth;
                                                                                        dbRoster.markModified("DepthChartNL");
                                                                                        roster.save(function (err, result) {
                                                                                            if (err) {
                                                                                                res.status(500).json({ status: 500, msg: err.errmsg, isValid: false });
                                                                                            } else {
                                                                                                res.status(200).json({ status: 200, msg: "success", players: players, activeRoster: roster.FortyManNL });
                                                                                            }
                                                                                            dbRoster.save(function (err, result) {
                                                                                                if (err)
                                                                                                    console.log("error updating rosterLineup in accumulated stats");
                                                                                            })
                                                                                        });
                                                                                    });
                                                                                } else {

                                                                                    // update the depth chart with the eligibilty 
                                                                                    var foundplayers = [];
                                                                                    var placedPlayers = [];
                                                                                    var positionReference = ["C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "SP", "P", "CL", "DH", "PH"];

                                                                                    for (i = 0; i < roster.DepthChartNL.length; i++) {

                                                                                        var playersAtDepth = [];    // track their id's and don't put them in twice!

                                                                                        // for each "slot" which represents a position..CA, 1B, through P, CL and DH
                                                                                        // find the player in that slot, then find his stats in the dbRoster and insert them

                                                                                        if (!roster.DepthChartNL[i]) {
                                                                                            roster.DepthChartNL[i] = {
                                                                                                Position: positionReference[i],
                                                                                                Players: [],
                                                                                                Pos: positionReference[i]
                                                                                            }
                                                                                        }

                                                                                        for (p = roster.DepthChartNL[i].Players.length - 1; p >= 0; p--) {
                                                                                            var foundplayer = false;
                                                                                            if (roster.DepthChartNL[i].Players[p]) {

                                                                                                var nextPID = roster.DepthChartNL[i].Players[p].PlayerId;
                                                                                                var isActive = false;
                                                                                                if (activePlayers.indexOf(nextPID) >= 0) {
                                                                                                    isActive = true;
                                                                                                }
                                                                                                var isAlreadyIn = false; {
                                                                                                    if (playersAtDepth.indexOf(nextPID) >= 0) {
                                                                                                        isAlreadyIn = true;
                                                                                                    }
                                                                                                }

                                                                                                // now find the player in the dbRosters with that ID
                                                                                                // if we don't, remove him from the depth chart!

                                                                                                for (dp = 0; dp < dbRoster.Players.length; dp++) {
                                                                                                    if (dbRoster.Players[dp].PlayerId == nextPID) {

                                                                                                        if (isActive && !isAlreadyIn) {

                                                                                                            // substitute the stats...lock, stock, and barrel
                                                                                                            roster.DepthChartNL[i].Players[p] = dbRoster.Players[dp];
                                                                                                            foundplayers.push(dp);
                                                                                                            placedPlayers.push(dbRoster.Players[dp].PlayerId)
                                                                                                            playersAtDepth.push(nextPID);
                                                                                                            foundplayer = true;

                                                                                                        } else {
                                                                                                            // this player isn't active pop him off.
                                                                                                            foundplayer = false;
                                                                                                        }
                                                                                                        break;
                                                                                                    }
                                                                                                }
                                                                                            }
                                                                                            if (!foundplayer) {
                                                                                                // remove this player from the depth chart as
                                                                                                // he's no longer in the roster or he's already in it once
                                                                                                roster.DepthChartNL[i].Players.splice(p, 1);
                                                                                            }
                                                                                        }
                                                                                    }

                                                                                    if (foundplayers.length != dbRoster.Players.length) {
                                                                                        // then there are players that haven't made it into the depth chart!
                                                                                        for (i = 0; i < dbRoster.Players.length; i++) {
                                                                                            if (foundplayers.indexOf(i) == -1) {
                                                                                                // then this player wasn't found
                                                                                                // find a good position for him and stick  him in the depth chart!
                                                                                                var newPlayer = dbRoster.Players[i];
                                                                                                positionIndex = 12; // default.. Pinch hitter
                                                                                                var pos = "PH";
                                                                                                var positionReference = ["C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "SP", "P", "CL", "DH", "PH"];
                                                                                                if (newPlayer.Primary && newPlayer.Primary.length > 0) {

                                                                                                    pos = newPlayer.Primary[0];


                                                                                                } else if (newPlayer.Secondary && newPlayer.Secondary.length > 0) {

                                                                                                    pos = newPlayer.Secondary[0];

                                                                                                } else if (newPlayer.Tertiary && newPlayer.Tertiary.length > 0) {
                                                                                                    pos = newPlayer.Tertiary[0];
                                                                                                }
                                                                                                if (pos == "CA")
                                                                                                    pos = "C";
                                                                                                positionIndex = positionReference.indexOf(pos);
                                                                                                if (positionIndex == -1)
                                                                                                    positionIndex = 12;


                                                                                                if (positionIndex == 12 && !roster.DepthChartNL[12]) {
                                                                                                    roster.DepthChartNL[12] = { Pos: "PH", Position: "Pinch Hitter", Players: [] };
                                                                                                }
                                                                                                // ok.. put him in the real depth chart!
                                                                                                roster.DepthChartNL[positionIndex].Players.push(newPlayer);
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                    // there's already a depth chart.. have to put these players in the chart.
                                                                                    roster.markModified("DepthChartNL");
                                                                                    dbRoster.DepthChartNL = roster.DepthChartNL;
                                                                                    dbRoster.markModified("DepthChartNL");

                                                                                    roster.save(function (err, result) {
                                                                                        if (err) {
                                                                                            res.status(201).json({ status: 201, msg: "error", players: null, activeRoster: null });

                                                                                        } else {
                                                                                            res.status(200).json({ status: 200, msg: "success", players: players, activeRoster: roster.FortyManNL });
                                                                                        }
                                                                                        // save updated rosterLineup DB
                                                                                        dbRoster.save(function (err, result) {
                                                                                            if (err)
                                                                                                console.log("error saving rosterLineups in 2nd part of accumulated stats")
                                                                                        })
                                                                                    });
                                                                                }


                                                                            }
                                                                        }

                                                                    });

                                                                }
                                                            });
                                                    });
                                                });

                                            });
                                        }); // end of getting all the active players - minors
                                    });

                                });
                            });
                        }); // end of getting all the inactive players - minors

                    });
                }
            });
        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.post('/api/usedgames/update', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {


        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

///
/// ******************* GETTING / UPDATING ACCUMULATED STATS AND USED GAMES ****************
///

router.get('/api/accstats/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            // example of gettings sorted stats based on season and sorted field.
            // db.getCollection('aatest').find({"stats.season":2018},{name:1, PlayerId: 1,"stats.$":1}).sort({"stats.games":-1})


        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

/*
router.post('/api/accstats/save', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

    
            var leagueId = req.body.lid;
            var boxId = req.body.boxId;

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});
*/

/*
router.get('/api/accstats/getForRoster', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var season = req.query.season;
            var leagueId = req.query.lid;
            var teamId = req.query.tid;

            Roster.findOne({LeagueId: leagueId, TeamId: teamId}, function(err, roster){
                if( err ) {

                } else {

                }
            });

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

*/

// ****************************** ACCSTATS INSERT STATUS AND SCORES OF GAMES INTO SCHEDULE FOR SPEED LATER //
// update the scheduled games state (official or not) and score if official
router.post('/api/accstats/insert-scores-into-schedule', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var gameDate = req.body.gameDate;

            Schedule.findOne({ LeagueId: req.body.lid }, function (err, schedule) {

                Boxscore.find({ LeagueId: req.body.lid, GameDate: req.body.gameDate }, { Game: 1, Status: 1, Boxscore: 1 }, function (err, boxscores) {
                    if (err || boxscores == null || boxscores.length == 0) {
                        res.status(201).json({ status: 201, msg: "Sorry, couldn't find scores or schedule" });
                    } else {
                        for (var g = 0; g < schedule.Games.length; g++) {
                            if (schedule.Games[g].simpleDate == req.body.gameDate) {
                                // then find a boxscore that might match.
                                var gameId = schedule.Games[g].gameId;
                                for (var b = 0; b < boxscores.length; b++) {
                                    if (boxscores[b].Game.gameId == gameId) {
                                        // then if this game is official, then store it as official....
                                        if (boxscores[b].Status == "Official") {
                                            schedule.Games[g].extra.played = true;
                                            schedule.Games[g].extra.homeScore = boxscores[b].Boxscore.summary.Home.R;
                                            schedule.Games[g].extra.visitScore = boxscores[b].Boxscore.summary.Visit.R;
                                        } else {
                                            // no longer official.. clear things out.
                                            schedule.Games[g].extra.played = false;
                                            schedule.Games[g].extra.homeScore = "";
                                            schedule.Games[g].extra.visitScore = "";
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                        schedule.markModified("Games");
                        schedule.save(function (err, result) {
                            if (err) {
                                res.status(201).json({ status: 201, msg: "Sorry, couldn't updater schedule" });
                            } else {
                                res.status(200).json({ status: 200, msg: "Schedule updated for " + req.body.gameDate });
                            }
                        })
                    }
                })
            })

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ****************************** ACCSTATS ACCUMULATE STATS FOR A SINGLE GAME DAY //
// for all players in EACH box score FOR ONE DAY WORTH OF GAMES...
router.post('/api/accstats/update/accumulateForGameDay', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var gameDate = req.body.gameDate;

            if (req.body.delete === true) {
                AccumulatedStats.deleteMany({ LeagueId: req.body.lid }, function (err, result) {

                    Boxscore.find({ LeagueId: req.body.lid, GameDate: gameDate }, function (err, boxscores) {
                        if (err || boxscores == null || boxscores.length == 0) {
                            res.status(201).json({ status: 201, msg: "Sorry, couldn't find boxscores for:" + gameDate });
                        } else {

                            teamHelper.saveGameStatsForDay(0, boxscores, function (err, message) {
                                res.status(200).json({ status: 200, msg: "Player stats recorded for " + gameDate });
                            })

                        }
                    })
                })
            } else {
                Boxscore.find({ LeagueId: req.body.lid, GameDate: gameDate }, function (err, boxscores) {
                    if (err || boxscores == null || boxscores.length == 0) {
                        res.status(201).json({ status: 201, msg: "Sorry, couldn't find boxscores for:" + gameDate });
                    } else {

                        teamHelper.saveGameStatsForDay(0, boxscores, function (err, message) {
                            res.status(200).json({ status: 200, msg: "Player stats recorded for " + gameDate });
                        })

                    }
                })
            }

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ****************************** ACCSTATS UPDATE: PLAYER STATS AND PLAYER USEAGE ///
// for all players in one box score.
router.post('/api/accstats/update/statsonly', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var boxId = req.body.boxId;
            Boxscore.findOne({ _id: boxId }, function (err, finalscore) {
                if (err || finalscore == null) {
                    res.status(201).json({ status: 201, msg: "Server error: couldn't find game" });
                } else {



                    // ************** update all the stats based on this game:
                    // 1) individual player stats for both rosters
                    // 2) used games/partial games for each player that played.

                    var season = finalscore.Season;
                    teamHelper.saveGameStats(finalscore, function (message) {
                        if (message != "success") {
                            res.status(299).json({ status: 299, msg: "Error" });

                        } else {
                            // finally, make the game official
                            // finalscore.Status = "Official";
                            /*
                             finalscore.save(function (err, response) {
                                 if (err) {
               
                                 }
                                 res.status(200).json({ status: 200, msg: "Player stats recorded." });
                             })
                             */
                            res.status(200).json({ status: 200, msg: "Player stats recorded." });

                        }
                    })
                }
            })
        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ****************************** ACCSTATS UPDATE: THIS UPDATES ONLY W/L RECORDS ///
router.post('/api/accstats/update', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var boxId = req.body.boxId;
            Boxscore.findOne({ _id: boxId }, function (err, finalscore) {
                if (err || finalscore == null) {
                    res.status(201).json({ status: 201, msg: "Server error: couldn't find game" });
                } else {

                    // ************** update all the stats based on this game:
                    // 1) w/l records for the team in the standings for both teams
                    // 2) individual player stats for both rosters
                    // 3) used games for each player that played.

                    var season = finalscore.Season;
                    if (finalscore.Status == "Official") {
                        res.status(200).json({ status: 200, msg: "NO UPDATE. This Game is already Official." });
                    } else {

                        leagueHelper.updateWonLossRecords(finalscore.LeagueId, 2017, finalscore, function (err) {
                            //  teamHelper.saveGameStats(finalscore, function (message) {
                            //      if (message != "success") {
                            //          res.status(299).json({ status: 299, msg: "Error" });
                            ///
                            //    } 
                            if (err) {
                                res.status(299).json({ status: 299, msg: "Error" });
                            } else {
                                // finally, make the game official
                                finalscore.Status = "Official";
                                finalscore.save(function (err, response) {
                                    if (err) {

                                    }
                                    res.status(200).json({ status: 200, msg: "W/L Records recorded and game marked Official." });
                                })

                            }
                        })

                        //  })
                    }


                }
            })


        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// *********************************** BOXSCORES ************************************ //
/*
router.get('/api/accumulated/boxscores/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var gameDate = req.query.gameDate;
            var gameNumber = req.query.gameNumber;

            var query = { LeagueId: req.query.lid };
            if( req.query.showall != "true" ) {
                query.Status = "Official";  // only show official boxscores.
            }
            if (gameDate) {
                if (gameDate == "Today") {
                    gameDate = moment().format("YYYYMMDD");
                }
                query.GameDate = gameDate;
            }
            if (gameNumber) {
                query.GameNumber = gameNumber;
            }

            var limit = 50;
            if (req.query.limit)
                limit = Number(req.query.limit);
            Boxscore.find(query, function (err, boxscores) {
                if (err || boxscores.length == 0) {
                    res.status(500).json({ status: 500, msg: "No games found" });
                } else {
                    var table = {};
                    if (req.query.getSS) {
                        table = sim.getBoxscoreAsTable(boxscores[0]);
                    }
                    res.status(200).json({ status: 200, msg: "success", boxscores: boxscores, boxtable: table });
                }
            }).limit(limit);

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
})
*/
// *********************************** BOXSCORES ************************************ //

router.get('/api/accumulated/boxscores/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var gameDate = req.query.gameDate;
            var gameNumber = req.query.gameNumber;

            var query = { LeagueId: req.query.lid };
            if (req.query.showall != "true") {
                query.Status = "Official";  // only show official boxscores.
            }
            if (gameDate) {
                if (gameDate == "Today") {
                    gameDate = moment().format("YYYYMMDD");
                }
                query.GameDate = gameDate;
            }
            if (gameNumber) {
                query.GameNumber = gameNumber;
            }
            var projection = { "Game": 1, "Boxscore.summary": 1, "Status": 1, "URL": 1 };
            if (req.query.getSS)
                projection = {};

            var limit = 50;
            if (req.query.limit)
                limit = Number(req.query.limit);
            Boxscore.find(query, projection, function (err, boxscores) {
                if (err) {
                    res.status(500).json({ status: 500, msg: "error", error: err });
                }
                else if (boxscores.length == 0) {
                    res.status(200).json({ status: 200, msg: "No games found" });
                } else {
                    var table = {};
                    if (req.query.getSS) {
                        table = sim.getBoxscoreAsTable(boxscores[0]);
                    }
                    res.status(200).json({ status: 200, msg: "success", boxscores: boxscores, boxtable: table });
                }
            }).limit(limit);

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
})

// *********************************** STANDINGS ************************************ //

router.get('/api/accumulated/standings/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var season = 2017;
            if (req.query.season) {
                season = req.query.season;
            }

            Standings.findOne({ LeagueId: req.query.lid, Season: season }, function (err, standings) {
                if (err) {
                    res.status(500).json({ status: 500, msg: "No games found" });
                } else {
                    if (standings) {
                        // get standings in order
                        var newStandings = leagueHelper.calcuatedStandingsOrder(standings);
                        res.status(200).json({ status: 200, msg: "success", Standings: newStandings });
                    } else {
                        // create standings (which by definition puts them in order as all 0's)
                        leagueHelper.createLeagueStandings(req.query.lid, season, function (err, standings) {
                            if (err) {
                                res.status(201).json({ status: 201, msg: "couldn't find/create standings" });
                            } else {
                                res.status(200).json({ status: 200, msg: "success", Standings: standings });

                            }
                        })

                    }
                }
            })

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
})

// ************************ get a gameday record ************************************
router.get('/api/accumulated/gameday/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            GameDay.findOne({ LeagueId: req.query.lid, GameDate: req.query.gameDate }, function (err, gameday) {
                if (err || gameday == null) {
                    res.status(500).json({ status: 500, msg: "No gameday found" });
                } else {
                    res.status(200).json({ status: 200, msg: "success", gameDay: gameday });
                }
            })

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
})


module.exports = router;