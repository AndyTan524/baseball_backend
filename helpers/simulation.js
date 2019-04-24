require('../data/mongo');
require("../simulator/schedule");            // Functions for managing the schedule

require("../simulator/rosters");       // Step 1: get the full team for the two teams playing
require("../simulator/players");       // Step 2: get the raw mlb stats for all players (batting, fielding, running) and pitchers

require("../simulator/set-pitchers");  // Step 3: set up the pitching staff
require("../simulator/set-lineup");    // Step 4: set up the batting order and fielders

require("../simulator/sim-game");        // step 5: calculate the bonus Runs (Arw) and RBI (Abrw) values for each batter

require("../simulator/play-game");
require("../simulator/record-game");

require("../simulator/render-CSV");

var moment = require("moment");

require("../simulator/utils");
var jsonfile = require('jsonfile')

algorithms = require("../simulator/algorithms.json");
boxscoreTemplate = require("../simulator/boxscore-template.json");

var playerStatus = require("../helpers/playerStatus");
var simulationHelper = require("../helpers/simulationHelper");
var leagueHelper = require("../helpers/leagueHelper");
var playerHelper = require('../helpers/playerHelper');
var rosterHelper = require('../helpers/rosterHelper');
var setPitcherHelper = require('../simulator/set-pitchers-helper');
var setLineupHelper = require('../simulator/set-lineup-helper');
var mongoose = require('mongoose');
var RosterLineup = mongoose.model("RosterLineup");
var Roster = mongoose.model("Roster");
var Boxscore = mongoose.model("Boxscore");

setGameRequest = function (request) {
    gameRequest.request = request;
}

getGameRequest = function () {
    return (gameRequest.request);
}



var gameRequest = {}

module.exports = {

    algorithms: require("../simulator/algorithms.json"),

    boxscoreTemplate: require("../simulator/boxscore-template.json"),



    gamePlayRequest: {
        gameDay: "today",   // either "today" or date in "2017-08-09T02:29:40.399Z" format
        games: ["all"],     // array of games to play or games[0] == "all" for all games
        useFakeData: true,
        requireAvailableStats: false, // default: TRUE for real games (only use "unused" PA and BF games if true)
        requireMLE: false,   // default: TRUE for real games (Major Leauge Equivalent Calculations used if true)
        rerun: true,         // true means replay any played games and play unplayed games
        // false means ONLY play unplayed games.
        saveCSVTestFile: true
    },

    schedule: [],

    //*************** set lineups */
    setPitchingStaff: function (game, team, players) {
        var game = {};
        var team = {};
        setPitchers(game, team, players);
        return pStaff;
    },


    runSimulation: function (homeTeam, visitTeam, game, callback) {
        setGameRequest(this.gamePlayRequest);

        var isRetry = false;
        simulationHelper.getPlayerStats(homeTeam, 0, function (homeRoster) {


            simulationHelper.getPlayerStats(visitTeam, 0, function (visitRoster) {
                if (visitRoster == null) {
                    isRetry = true;
                    league.Teams[0].TeamName = game.visit.name;
                    leagueHelper.getTeam(league, function (err, dbLeague) {
                        playerHelper.generateDefaultPlayersByTeam(dbLeague, function () {

                        });
                    });
                }

                var date = game.gameDate;
                for (var i = 0; i < homeRoster.length; i++) {
                    var player = homeRoster[i];
                    if (player.Baserunning && player.Baserunning.daily) {
                        player.Baserunning.daily = playerHelper.getCurrentData(date, player.Baserunning.daily);
                        if (!player.Baserunning.daily) {
                            player._doc.Baserunning.daily = [];
                        }
                    }
                    if (player.Batting && player.Batting.daily) {
                        player.Batting.daily = playerHelper.getCurrentData(date, player.Batting.daily);
                        if (!player.Batting.daily) {
                            player._doc.Batting.daily = [];
                        }
                    }
                    if (player.Fielding && player.Fielding.daily) {
                        player.Fielding.daily = playerHelper.getCurrentData(date, player.Fielding.daily);
                        if (!player.Fielding.daily) {
                            player._doc.Fielding.daily = [];
                        }
                    }
                    if (player.Pitching && player.Pitching.daily) {
                        player.Pitching.daily = playerHelper.getCurrentData(date, player.Pitching.daily);
                        if (!player.Pitching.daily) {
                            player._doc.Pitching.daily = [];
                        }
                    }
                }

                for (var i = 0; i < visitRoster.length; i++) {
                    var player = visitRoster[i];
                    if (player.Baserunning && player.Baserunning.daily) {
                        player.Baserunning.daily = playerHelper.getCurrentData(date, player.Baserunning.daily);
                        if (!player.Baserunning.daily) {
                            player._doc.Baserunning.daily = [];
                        }
                    }
                    if (player.Batting && player.Batting.daily) {
                        player.Batting.daily = playerHelper.getCurrentData(date, player.Batting.daily);
                        if (!player.Batting.daily) {
                            player._doc.Batting.daily = [];
                        }
                    }
                    if (player.Fielding && player.Fielding.daily) {
                        player.Fielding.daily = playerHelper.getCurrentData(date, player.Fielding.daily);
                        if (!player.Fielding.daily) {
                            player._doc.Fielding.daily = [];
                        }
                    }
                    if (player.Pitching && player.Pitching.daily) {
                        player.Pitching.daily = playerHelper.getCurrentData(date, player.Pitching.daily);
                        if (!player.Pitching.daily) {
                            player._doc.Pitching.daily = [];
                        }
                    }
                }



                //jsonfile.writeFile("./simulator/sampleDataFromMongo/teamStats-ARI-new.json", visitRoster, function (err) {
                //    console.error(err)
                //})
                //jsonfile.writeFile("./simulator/sampleDataFromMongo/teamStats-COL-new.json", homeRoster, function (err) {
                //    console.error(err)
                //})

                if (!isRetry) {

                    // ((((((((((((((((((( TODO: INSURE REAL USER LINEUPS)))))))))))))))))))
                    // (((((((((((((((((((       FOR DH and 9Man         )))))))))))))))))))

                    var visitFakeOrder = [
                        ["2B", -1],
                        ["CF", -1],
                        ["LF", -1],
                        ["SS", -1],
                        ["CA", -1],
                        ["RF", -1],
                        ["1B", -1],
                        ["P", -1],
                        ["3B", -1]];
                    var homeFakeOrder = [
                        ["CF", -1],
                        ["2B", -1],
                        ["LF", -1],
                        ["3B", -1],
                        ["RF", -1],
                        ["1B", -1],
                        ["SS", -1],
                        ["CA", -1],
                        ["P", -1]];;

                    visitRoster.battingOrder = visitFakeOrder;


                    setPitcherHelper.setPitchers(game, visitTeam, visitRoster);

                    homeRoster.battingOrder = homeFakeOrder;
                    setPitchers(game, homeTeam, homeRoster);

                    setLineup(game, visitTeam, visitRoster);
                    setLineup(game, homeTeam, homeRoster);

                    simulateOffense(visitTeam.batters.battingOrder, visitRoster);
                    simulateOffense(homeTeam.batters.battingOrder, homeRoster);

                    var boxScore = playGame(game, visitTeam, visitRoster, homeTeam, homeRoster);

                    callback(boxScore);
                }
                else {
                    callback({ Msg: "Missing roster, please retry." });
                }
            });
        });


    },

    // *************************************************** 
    //
    // called on way into games when there's an override. (also called from www's pitching view)
    //  assumes that the first time the game was played that all the lineups were corrected
    //  then calls setPitcher and setLineup
    //
    // ****************************************************
    getEligibleLineupsForOverride: function (game, isHome, action, gameRange, Override, callback) {

        var leagueId = game.leagueId;
        var teamname = game.visit;
        var teamId = game.visitId;
        if (isHome) {
            teamname = game.home;
            teamId = game.homeId;
        }

        Boxscore.find({ LeagueId: leagueId, GameNumber: game.gameId }, function (err, boxscore) {
            if (err) {
                callback(err);
            } else {

                // ****** internal function to find the estats for the lineup players
                function findEStatsForPlayer(player, roster) {

                    for (var p = 0; p < 13; p++) {
                        // cycle through the positions
                        if (roster[p] && roster[p].Players && roster[p].Players.length > 0) {
                            for (var d = 0; d < roster[p].Players.length; d++) {
                                if (player.PlayerId == roster[p].Players[d].PlayerId) {
                                    // found him
                                    return (roster[p].Players[d]);
                                }
                            }
                        }
                    }
                    return ({});
                }
                // grab the visiting team from the box score
                var isHome = false;
                var vteam = { extra: boxscore[0].TeamData.visitTeam.extra };
                if (!vteam.extra) {
                    vteam.extra = {
                        LastAllowed3DaysRest: 90,
                        Allow3DaysRest: true,
                        Coordinators: []
                    }
                } else if (!vteam.extra.Coordinators) {
                    vteam.extra.Coordinators = [];
                }
                var vplayers = boxscore[0].TeamData.visitRoster;    // has info on coordinators and 3 day override.
                var battingOrder = [];
               // var boxOrder = boxscore[0].TeamData.visitTeam.batters.battingOrder;
               var boxOrder = boxscore[0].Boxscore.Visit.Batters;
                // reconstruct original batting order!
                for (var i = 0; i < boxOrder.length; i++) {
                    // first see if this player is a starter.. i.e. position NOT PH
                    if (boxOrder[i]  && boxOrder[i].Pos != "PH") {
                        // ok.. a starter.. find his real stats...
                        battingOrder.push(findEStatsForPlayer(boxOrder[i], vplayers));
                    }
                }
                var boxPitchersIP = [];
                for( var i=0; i<boxscore[0].Boxscore.Visit.Pitchers.length; i++ ) {
                    var outs = 3;
                    if( boxscore[0].Boxscore.Visit.Pitchers[i].OUT ) {
                        outs = boxscore[0].Boxscore.Visit.Pitchers[i].OUT;
                    }
                    boxPitchersIP[i] = outs;
                }
                Override['vBoxIP'] = boxPitchersIP;

                vplayers.battingOrder = battingOrder;
                setPitcherHelper.setPitchers(game, vteam, vplayers, true, isHome, Override);
                setLineupHelper.setLineup(game, vteam, vplayers, true, []);

                // grab the home team from the box score
                isHome = true;
                var hteam = { extra: boxscore[0].TeamData.homeTeam.extra };
                if (!hteam.extra) {
                    hteam.extra = {
                        LastAllowed3DaysRest: 90,
                        Allow3DaysRest: true,
                        Coordinators: []
                    }
                } else if (!hteam.extra.Coordinators) {
                    hteam.extra.Coordinators = [];
                }
                var hplayers = boxscore[0].TeamData.homeRoster;    // has info on coordinators and 3 day override.
                var battingOrder = [];
             //   var boxOrder = boxscore[0].TeamData.homeTeam.batters.battingOrder;
                var boxOrder = boxscore[0].Boxscore.Home.Batters;
                // reconstruct original batting order!
                for (var i = 0; i < boxOrder.length; i++) {
                    // first see if this player is a starter.. i.e. position NOT PH
                    if (boxOrder[i] && boxOrder[i].Pos != "PH") {
                        // ok.. a starter.. find his real stats...
                        battingOrder.push(findEStatsForPlayer(boxOrder[i], hplayers));
                    }
                }
                hplayers.battingOrder = battingOrder;

                boxPitchersIP = [];
                for( var i=0; i<boxscore[0].Boxscore.Home.Pitchers.length; i++ ) {
                    var outs = 3;
                    if( boxscore[0].Boxscore.Home.Pitchers[i].OUT ) {
                        outs = boxscore[0].Boxscore.Home.Pitchers[i].OUT;
                    }
                    boxPitchersIP[i] = outs;
                }
                Override['hBoxIP'] = boxPitchersIP;
                setPitcherHelper.setPitchers(game, hteam, hplayers, true, isHome, Override);
                setLineupHelper.setLineup(game, hteam, hplayers, true, []);
                callback(null, vplayers, vteam, hplayers, hteam);
            }
        })

    },

    // *************************************************** 
    //
    // called on way into games (also called from www's pitching view)
    //  if there isn't an eligible roster, create one
    //  then load up the "game roster"
    // be sure to use the data from the roster or roster lineups and NOT from the batting order!!!
    //
    // ****************************************************
    getEligibleLineup: function (game, isHome, action, gameRange, Override, callback) {

        var leagueId = game.leagueId;
        var teamname = game.visit;
        var teamId = game.visitId;
        if (isHome) {
            teamname = game.home;
            teamId = game.homeId;
        }


        var team = {};
        var visitFakeOrder = [
            ["2B", -1],
            ["CF", -1],
            ["LF", -1],
            ["SS", -1],
            ["CA", -1],
            ["RF", -1],
            ["1B", -1],
            ["P", -1],
            ["3B", -1]];
        var homeFakeOrder = [
            ["CF", -1],
            ["2B", -1],
            ["LF", -1],
            ["3B", -1],
            ["RF", -1],
            ["1B", -1],
            ["SS", -1],
            ["CA", -1],
            ["P", -1]];
        var homeFakeDHOrder = [
            ["CF", -1],
            ["2B", -1],
            ["DH", -1],
            ["LF", -1],
            ["3B", -1],
            ["RF", -1],
            ["1B", -1],
            ["SS", -1],
            ["CA", -1]];


        // ************** step 1: grab the rosterLineup if it exists
        Roster.findOne({ LeagueId: leagueId, TeamId: teamId }, { LeagueId: 1, TeamId: 1, DepthChartNL: 1, FortyManNL: 1, TeamAbbr: 1, BattingOrderNL: 1, DepthChartAL: 1, BattingOrderAL: 1, Bench: 1, Extra: 1 }, function (err, rosterLineup) {
            if (!err && rosterLineup != null && rosterLineup.DepthChartNL) {

                var players;
                var battingOrder;
                players = rosterLineup.DepthChartNL;    // same for both league types.. just use the NL

                // add in a fake batting order in case the user didn't create one.
                // grab the order we care about!
                players.battingOrder = rosterLineup.BattingOrderNL ? rosterLineup.BattingOrderNL : null;
                if (game.dhGame) {
                    players.battingOrder = rosterLineup.BattingOrderAL ? rosterLineup.BattingOrderAL : rosterLineup.BattingOrderAL;
                }

                var activePlayerIDs = [];
                for (var ap = 0; ap < rosterLineup.FortyManNL.length; ap++) {
                    var nextActivePlayer = rosterLineup.FortyManNL[ap];
                    if (nextActivePlayer.Status == playerStatus.ActiveRoster) {
                        activePlayerIDs.push(nextActivePlayer.PlayerId);
                    } else {
                        var inactive = nextActivePlayer;
                    }
                }

                // remove any players not on the active roster from the depth chart.

                if (!players.battingOrder) {

                    // grab a fake.
                    players.battingOrder = homeFakeOrder;
                    if (game.dhGame)
                        players.battingOrder = homeFakeDHOrder;
                } else {

                    // ok, swap out the data in the batting order!
                    for (let bo = 0; bo < players.battingOrder.length; bo++) {
                        if (players.battingOrder[bo]) {
                            var nextBatter = players.battingOrder[bo];
                            var found = false;
                            for (d = 0; d < rosterLineup.DepthChartNL.length; d++) {
                                for (p = 0; p < rosterLineup.DepthChartNL[d].Players.length; p++) {
                                    if (rosterLineup.DepthChartNL[d].Players[p] && rosterLineup.DepthChartNL[d].Players[p].PlayerId == nextBatter.PlayerId) {
                                        var position = nextBatter.Position;
                                        if (activePlayerIDs.indexOf(nextBatter.PlayerId) >= 0) {
                                            players.battingOrder[bo] = rosterLineup.DepthChartNL[d].Players[p];
                                            players.battingOrder[bo].Position = position;
                                        } else {
                                            players.battingOrder[bo] = [position, -1];
                                        }
                                        found = true;
                                        break;

                                    }
                                    if (found)
                                        break;
                                }
                                if (found)
                                    break;
                            }
                        }
                    }
                }

                // clean up pinch hitters (de-dupe)
                if (rosterLineup.DepthChartNL[12]) {
                    var dcId = [];
                    var splicers = [];
                    for (var d = 0; d < rosterLineup.DepthChartNL[12].Players.length; d++) {
                        if (rosterLineup.DepthChartNL[12].Players[d] && dcId.indexOf(rosterLineup.DepthChartNL[12].Players[d].PlayerId) == -1) {
                            dcId.push(rosterLineup.DepthChartNL[12].Players[d].PlayerId);
                        } else {
                            // get the index for the dupe
                            splicers.push(d);
                        }

                    }
                    if (splicers.length > 0) {
                        for (var s = splicers.length - 1; s >= 0; s--) {
                            rosterLineup.DepthChartNL[12].Players.splice(splicers[s], 1);
                        }
                    }
                }

                // be sure there's a bench to draw from...
                var bench = [];
                if (rosterLineup.Bench && rosterLineup.Bench.length > 0) {
                    if (rosterLineup.DepthChartNL[12] && rosterLineup.DepthChartNL[12].Players.length > 0) {
                        bench = rosterLineup.DepthChartNL[12].Players.concat(rosterLineup.Bench);
                    } else {
                        bench = rosterLineup.Bench;
                    }
                } else {
                    // make a bench here

                    for (var p = rosterLineup.DepthChartNL.length - 1; p >= 0; p--) {
                        var length = rosterLineup.DepthChartNL[p].Players.length;
                        for (var d = 0; d < length; d++) {
                            nextBencher = rosterLineup.DepthChartNL[p].Players[d];
                            if (nextBencher) {
                                bench.push(nextBencher);
                            }
                        }
                    }
                }

                if (!rosterLineup.Extra) {
                    rosterLineup.Extra = {
                        Coordinators: {},
                        "Allow3DaysRest": true,
                        "LastAllowed3DaysRest": 91
                    }
                }
                team.extra = rosterLineup.Extra;    // has info on coordinators and 3 day override.
                setPitcherHelper.setPitchers(game, team, players, true, isHome, Override);
                setLineupHelper.setLineup(game, team, players, true, bench);
                callback(players, team);

            } else {

                // ************** no Depth chart: create the rosters and depth chart by games used...
                leagueHelper.getRosterByRName(leagueId, teamname, function (err, roster) {

                    if (err) {
                        callback(null, null);
                    } else {
                        var activePlayers = [];
                        var playerGamesUsed = [];
                        for (i = 0; i < roster.FortyManNL.length; i++) {
                            if (roster.FortyManNL[i].Status == playerStatus.ActiveRoster) {
                                activePlayers.push(roster.FortyManNL[i].PlayerId);

                                playerGamesUsed.push(
                                    {
                                        LeagueId: leagueId,
                                        PlayerId: roster.FortyManNL[i].PlayerId,
                                        MlbId: roster.FortyManNL[i].MlbId,
                                        FullName: roster.FortyManNL[i].FullName,
                                        LineupDH: {
                                            Position: roster.FortyManNL[i].StartingPositionDH,
                                            BattingOrder: roster.FortyManNL[i].BattingOrderDH,
                                            BenchDH: roster.FortyManNL[i].BenchOrderDH
                                        },
                                        Lineup9: {
                                            Position: roster.FortyManNL[i].StartingPosition9,
                                            BattingOrder: roster.FortyManNL[i].BattingOrder9,
                                            BenchDH: roster.FortyManNL[i].BenchOrder9
                                        },
                                        Season: "2017",
                                        /* examples...
                                        FullGames: ["20170404", "20170406"],
                                        PartialGames: {"20170405": {id:"1234", paTotal: 6, paUsed:3, ipTotal: 4, ipUsed:0} , "20170409": {id:"1234", paTotal: 9, paUsed:3, ipUsed:0} },
                                        */
                                        FullGames: [],
                                        PartialGames: {},

                                        CanCatch: false, // note this only means the player isn't prohibited by the 10 day rule
                                        CanStart: false,  // only means pitcher isn't prohibitied by the 4 days rest rule
                                        CanRelieve: false,   // only means pitcher isn't prohibited from relieving by rules
                                        FirstFullOffensiveGame: "",
                                        FirstFullPitchingGame: ""
                                    }
                                )

                            }
                        }

                        playerHelper.getEligiblePositions(0, playerGamesUsed, gameRange, function (playersWithEligibility) {
                            playerHelper.getAvailableBatting(0, playerGamesUsed, "ML", gameRange, function (players) {
                                //   res.status(200).json({ status: 200, msg: "success", players:players });
                                playerHelper.getAvailablePitching(0, playerGamesUsed, "ML", gameRange, function (players) {
                                    //   res.status(200).json({ status: 200, msg: "success", players:players });
                                    playerHelper.getAvailableFielding(0, playerGamesUsed, "ML", gameRange, function (players) {
                                        //   res.status(200).json({ status: 200, msg: "success", players:players });
                                        playerHelper.getAvailableBaserunning(0, playerGamesUsed, "ML", gameRange, function (players) {

                                            rosterHelper.getMasterPlayerPosition(0, players, function (roster) {
                                                var depth = rosterHelper.getDepthChartTemplate();
                                                depth[0].Players = rosterHelper.getPlayersByPosition("CA", roster);
                                                depth[1].Players = rosterHelper.getPlayersByPosition("1B", roster);
                                                depth[2].Players = rosterHelper.getPlayersByPosition("2B", roster);
                                                depth[3].Players = rosterHelper.getPlayersByPosition("3B", roster);
                                                depth[4].Players = rosterHelper.getPlayersByPosition("SS", roster);
                                                depth[5].Players = rosterHelper.getPlayersByPosition("LF", roster);
                                                depth[6].Players = rosterHelper.getPlayersByPosition("CF", roster);
                                                depth[7].Players = rosterHelper.getPlayersByPosition("RF", roster);
                                                depth[8].Players = rosterHelper.getPlayersByPosition("P", roster);
                                                depth[9].Players = depth[8].Players.length > 5 ? depth[8].Players.splice(4) : [];
                                                depth[10].Players = depth[8].Players.length > 0 ? depth[8].Players.splice(0, 1) : [];
                                                depth[8].Players.length > 5 ? depth[8].Players.splice(0, 4) : depth[8].Players;

                                                depth[11].Players = rosterHelper.getPlayersByPosition("DH", roster);

                                                var bench = roster;


                                                // add in a fake batting order in case the user didn't create one.
                                                depth.battingOrder = homeFakeOrder;
                                                if (game.dhGame)
                                                    depth.battingOrder = homeFakeDHOrder;

                                                if (action == "play game") {
                                                    setPitcherHelper.setPitchers(game, team, depth, true, isHome, Override);
                                                    setLineupHelper.setLineup(game, team, depth, true, bench);
                                                }
                                                callback(depth, team);

                                            });
                                        });
                                    });
                                });
                            });
                        });



                        /*
                                        // have ids of the active players.
                                        UsedGame.find({LeagueId:leagueId, Season:season, PlayerId: {$in: activePlayers} }, function (err, usedGames){
                                            if( err ) {
                        
                                            } else {
                                                // have used games for each player in the active roster
                        
                                            }
                                        });
                                        */
                    }
                });
            }
        });
    },

    storeEligibleLineup: function (game, isHome, callback) {

        this.getEligibleLineup(game, isHome, action, gameRange, function (players, team) {

        })
    },

    storeBoxscore: function (game, boxscore, teamData, override, callback) {

        /*
            Season: String,
                LeagueId: String,
    GameDate: String,
    GameNumber: String,
    CreatedUTC: String,
    Game: Object,
    Boxscore: Object
        */

        var newURL = "/#!/fullscore?gameID=" + game.gameId;

        var overrideGame = false;
        if (override && override.UpdateOfficialScore && override.UpdateOfficialScore === true) {
            game.Status = "Override";
            overrideGame = true;
        }

        if (game.Status && game.Status == "Official") {
            // don't do it!
            callback(null, "Game already official. Not re-saved.");
        } else {
            Boxscore.findOne({ LeagueId: game.leagueId, GameNumber: game.gameId }, function (err, oldBoxScore) {
                if (overrideGame === false && !err && oldBoxScore && oldBoxScore.Status && oldBoxScore.Status == "Official") {
                    callback({ msg: "Not updated as game was already Official", type: "Was Official" }, oldBoxScore);
                    return;
                }

                var overrideResults = { change: false, winner: 'home' };
                if (overrideGame) {
                    // figure out who won the first time through before saving it.
                    // original winner
                    var oldWinner = 'home';
                    if (oldBoxScore.Boxscore.summary.Visit.R > oldBoxScore.Boxscore.summary.Home.R)
                        oldWinner = 'visit';

                    if (boxscore.summary.Visit.R > boxscore.summary.Home.R)
                        overrideResults.winner = 'visit';

                    if (oldWinner != overrideResults.winner) {
                        overrideResults.change = true;
                    }
                }

                if (!err && oldBoxScore && oldBoxScore._doc) {
                    if (oldBoxScore.Boxscore.Home.OPitchers) {
                        boxscore.Home["OPitchers"] = oldBoxScore.Boxscore.Home.OPitchers;
                    }
                    if (oldBoxScore.Boxscore.Visit.OPitchers) {
                        boxscore.Visit["OPitchers"] = oldBoxScore.Boxscore.Visit.OPitchers;
                    }

                    oldBoxScore.Boxscore = boxscore;
                    oldBoxScore.markModified("Boxscore");


                    oldBoxScore.save(function (err, response) {
                        callback(err, response, overrideResults);
                    })
                } else {

                    /*
                    Boxscore.findOneAndUpdate(
                      { LeagueId: game.leagueId, GameNumber: game.gameId },
                      {
                        Season: 2018,
                        LeagueId: game.leagueId,
                        GameNumber: game.gameId,
                        GameDate: game.simpleDate,
                        CreatedUTC: new Date().toISOString(),
                        Game: game,
                        Boxscore: boxscore,
                        TeamData: teamData,
                        URL: newURL
                      },
                      { upsert: true, new: true }, function (err, response) {
                        callback(err, response);
                      }
                    )
                    */
                    Boxscore.create(
                        {
                            Season: 2018,
                            LeagueId: game.leagueId,
                            GameNumber: game.gameId,
                            GameDate: game.simpleDate,
                            CreatedUTC: new Date().toISOString(),
                            Game: game,
                            Boxscore: boxscore,
                            TeamData: teamData,
                            URL: newURL
                        }, function (err, response) {
                            callback(err, response, null);

                        }
                    )
                }
            });

        }
    },

    getLineupsAndPlayGame: function (game, gameRange, Override, callback) {

        var context = this;
        var isRetry = false;
        if (Override && Override.FromBoxScore && Override.FromBoxScore === true) {

            // ***************** get rosters from boxscore **********
            try {

                this.getEligibleLineupsForOverride(game, false, "play game", gameRange, Override, function (err, visitRoster, visitTeam, homeRoster, homeTeam) {

                    if (!isRetry) {
                        simulateOffense(visitTeam.batters.battingOrder, visitRoster);
                        simulateOffense(homeTeam.batters.battingOrder, homeRoster);

                        var boxScore = playGame(game, visitTeam, visitRoster, homeTeam, homeRoster);
                        callback(null, boxScore, visitTeam, visitRoster, homeTeam, homeRoster);
                    }
                    else {
                        callback("error", "Missing boxscore, please retry.");
                    }

                });
            }
            catch (err) {
                if (err) {
                    console.log("game error from try.." + err);
                    callback("error", "Game service error..." + err);
                }
            }
        } else {

            // ******************* get lineups from the rosters *****************
            try {
                this.getEligibleLineup(game, false, "play game", gameRange, Override, function (visitRoster, visitTeam) {
                    context.getEligibleLineup(game, true, "play game", gameRange, Override, function (homeRoster, homeTeam) {

                        if (!isRetry) {
                            simulateOffense(visitTeam.batters.battingOrder, visitRoster);
                            simulateOffense(homeTeam.batters.battingOrder, homeRoster);

                            var boxScore = playGame(game, visitTeam, visitRoster, homeTeam, homeRoster);

                            callback(null, boxScore, visitTeam, visitRoster, homeTeam, homeRoster);
                        }
                        else {
                            callback("error", "Missing roster, please retry.");
                        }
                    });
                });
            }
            catch (err) {
                if (err) {
                    console.log("game error from try.." + err);
                    callback("error", "Game service error.");
                }
            }
        }

    },

    getBoxscoreAsTable: function (boxscore) {


        // *************** create data for handsontable to display box score ****************

        // **** from api: render-CSV
        // ************************************************************
        //
        // number formatting functions
        //
        // ************************************************************
        isNumber = function (n) {
            return !isNaN(parseFloat(n)) && isFinite(n);
        }

        formatValue = function (column, n) {
            if (typeof (n) == "undefined")
                return ("--");

            if (!isNumber(n))
                return (n);


            formatDecimals = false;
            decimalplaces = 0;
            if (decimalColumns.indexOf(column) > -1) {
                formatDecimals = true;
                decimalplaces = 2;
            }
            if (column == "IP") {
                formatDecimals = true;
                decimalplaces = 1;
            }
            if (column == "YTDISO" || column == "Zone" || column == "Block" || column == "Frame") {
                formatDecimals = true;
                decimalplaces = 3;
            }
            rounding = false;
            if (column == "R" || column == "Runs") {
                rounding = true;
            }

            // is a number...
            var value = 0;

            if (decimalplaces == 0) {
                if (rounding)
                    n = Math.floor(n) + 0.5;
                value = Math.floor(n);
            } else {
                value = Number(n).toFixed(decimalplaces);
            }

            return value;
        }

        // *****************************************************
        //
        // columns that are 2 decimal places
        //
        //
        var decimalColumns = ["LW", "ArW", "ArbW", "Bases", "FieldLW", "LI", "Zone", "Block", "Frame", "cERA", "IPPG", "YTDSpeed", "YTDISO"];

        var boxscoreTemplate = {
            "Comment": "arrays for pitchers and batters are objects of the source (e.g. L=lineup data) and the json tag for the stat",
            "Batters": ["L-Pos",

                "S-FullName",
                "B-LW",
                "B-ArW",
                "B-ArbW",
                "B-RSRuns",
                "B-RSRBI",
                "R-Bases",
                "S-YTDSpeed",
                "S-YTDISO",
                "F-FieldLW",
                "F-E",
                "F-Zone",
                "F-Block",
                "F-Frame",
                "F-cERA",

                "B-PA",
                "B-AB",
                "B-R",
                "B-H",
                "B-1B",
                "B-2B",
                "B-3B",
                "B-HR",
                "B-RBI",
                "B-SAC",
                "B-SF",
                "B-BB",
                "B-HBP",
                "B-K",
                "B-SB",
                "B-CS",
                "B-GIDP",
                "B-HFC",
                "B-GB",
                "B-FB",
                "B-LD",
                "B-POPU",
                "B-BH",
                "B-IFH",
                "B-OUTS",
                "B-MLBId",
                "B-GameDate",
                "B-GameString",
                "B-GameId"
            ],
            "Pitchers":
                ["L-Pos",

                    "S-FullName",
                    "P-LW",
                    "L-Decision",
                    "P-R",
                    "P-IPG",

                    "P-LI",
                    "P-IP",
                    "P-BFP",
                    "P-H",
                    "P-1B",
                    "P-2B",
                    "P-3B",
                    "P-HR",
                    "P-ER",
                    "P-SH",
                    "P-SF",
                    "P-HBP",
                    "P-BB",
                    "P-K",
                    "P-WP",
                    "P-BLK",
                    "P-IR",
                    "P-IRS",
                    "P-GB",
                    "P-FB",
                    "P-LD",
                    "P-LI",
                    "P-POPU",
                    "P-SB",
                    "P-CS",
                    "P-PKO",
                    "P-OUT",
                    "P-MLBId",
                    "P-GameDate",
                    "P-GameString",
                    "P-GameId",

                    "F-FieldLW",
                    "F-E",
                    "F-Zone",
                    "F-Block",
                    "F-Frame",
                    "F-cERA"
                ],
            "Summary": {
                "Comment": "columns and row for csv box score",
                "Rows": [
                    ["", "FINAL SCORE", "R", "H", "E", "Off", "BR", "Def", "Pit"],
                    ["empty", "name", 0, 0, 0, 0, 0, 0, 0],
                    ["empty", "hometeam", 0, 0, 0, 0, 0, 0, 0]
                ]
            },

            "Details": {
                "Comment": "column headers for details of box score",
                "Rows": {
                    "Batters": ["Pos", "Name", "LW", "ArW", "ArbW", "Bases", "FieldLW", "E", "Zone", "Block", "Frame", "cERA", "empty", "PA", "AB", "R", "H", "1B", "2B", "3B", "HR", "RBI", "SAC", "SF", "HBP", "BB", "K", "SB", "CS", "GIDP", "HFC", "GB", "FB", "LD", "POPU", "BH", "IFH", "OUTS", "GameDate"],
                    "BattersTotal": ["empty", "empty", "empty", "Total"],
                    "OverallOffense": ["empty", "Overall Off."],
                    "OverallDefense": ["empty", "Overall Def."],
                    "Pitchers": ["Pos", "Name", "LW", "DEC", "Runs", "LI", "FieldLW", "E", "empty", "empty", "empty", "empty", "empty", "IP", "BFP", "H", "1B", "2B", "3B", "HR", "ER", "SH", "SF", "HBP", "BB", "K", "WP", "BLK", "IR", "IRS", "GB", "FB", "LD", "POPU", "SB", "CS", "PKO", "OUT", "GameDate"],
                    "PitchersTotal": ["empty", "empty", "empty", "Total"]
                }

            }

        }
        // *****************************************************
        //
        //  create box spreadsheet main code
        //
        // *****************************************************

        // renderCSV = function (game, score) {

        var game = boxscore.Boxscore.summary;
        var score = boxscore.Boxscore;


        // grab team names to save time/space
        var teamNames = [game.Visit.name, game.Home.name];

        var bt = boxscoreTemplate;

        // create both the csv file and the array of arrays for the spreadsheet handsontable display
        var boxCSV = "";
        var boxSS = [];
        var blankSSline = [];
        var headvb = 0;
        var headvp = 0;
        var headhb = 0;


        // ***********  create summary ***************
        var summary = bt.Summary.Rows[0];
        for (var line = 0; line < 3; line++) {

            var ssline = [];

            var results = score.summary.Visit;
            if (line == 2)
                results = score.summary.Home;

            for (var c = 0; c < summary.length; c++) {
                var column = summary[c];

                // remove the "empty" as it is just for readablity of the code
                if (column == "empty")
                    column = "";
                if (c == 1) {
                    if (line > 0) {
                        column = results.name;
                        if (column == "")
                            column = "team" + line;
                    }
                } else {
                    // grab the value
                    if (line > 0 && column != "") {

                        column = results[column];

                    }
                }

                // *** add column data to csv and to array 
                boxCSV += column;
                ssline.push(column);

                //***  below ONLY for CSV
                if ((c + 1) == summary.length) {
                    // end of line
                    boxCSV += '\r\n';
                } else {

                    // not end, add comma
                    boxCSV += ", ";
                }
            }
            boxSS.push(ssline);
        }

        // ***********  create details ***************

        // start by adding two blank rows
        boxCSV += '\r\n\r\n';

        boxSS.push(blankSSline);
        boxSS.push(blankSSline);

        // twice through.. one for each team
        // each team does:
        // title
        // column headers for batters
        // data for batters
        // totals for batters
        // same for pitchers

        var details = bt.Details.Rows;
        for (team = 0; team < 2; team++) {


            // ******* section header
            boxCSV += ">>> " + teamNames[team] + '\r\n';
            boxSS.push([">>> " + teamNames[team], "BATTING"]);

            boxCSV += ">>> BATTING " + '\r\n';


            if (team == 0) {
                headvb = boxSS.length;
            } else {
                headhb = boxSS.length;
            }

            // ******* batters
            var batters = details['Batters'];

            var linestats = score.Visit.Batters;
            if (team == 1)
                linestats = score.Home.Batters;

            var numberlines = linestats.length;

            for (var lines = -1; lines < numberlines; lines++) {

                ssline = [];

                for (var c = 0; c < batters.length; c++) {
                    var column = batters[c];

                    // remove the "empty" as it is just for readablity of the code
                    if (column == "empty") {
                        column = "";
                    } else {
                        if (lines >= 0) {
                            if (column == "Name")
                                column = "FullName";
                            if (column == "DEC")
                                column = "Decision";


                            // column = linestats[lines][column];
                            if (linestats[lines])
                                column = formatValue(column, linestats[lines][column]);
                            else {
                                column = "";
                            }
                        }
                    }
                    boxCSV += column;
                    ssline.push(column);

                    if ((c + 1) == batters.length) {
                        boxCSV += '\r\n';
                    } else {
                        boxCSV += ", ";
                    }
                }

                boxSS.push(ssline);
            }

            // ******* batters total
            batters = details['Batters'];
            linestats = score.Visit.BattingTotals;
            if (team == 1)
                linestats = score.Home.BattingTotals;

            ssline = [];
            for (var c = 0; c < batters.length; c++) {
                var column = batters[c];


                // remove the "empty" as it is just for readablity of the code
                if (column == "empty") {
                    column = "";
                } else {
                    // column = linestats[column];

                    column = formatValue(column, linestats[column]);
                }
                boxCSV += column;
                ssline.push(column);

                if ((c + 1) == batters.length) {
                    boxCSV += '\r\n';
                } else {
                    boxCSV += ", ";
                }
            }
            boxSS.push(ssline);


            results = score.summary.Visit;
            if (team == 1)
                results = score.summary.Home;

            // ******* offense total
            batters = details['OverallOffense'];
            ssline = [];

            for (var c = 0; c < batters.length; c++) {
                var column = batters[c];

                // remove the "empty" as it is just for readablity of the code
                if (column == "empty")
                    column = "";

                boxCSV += column;
                ssline.push(column);

                if ((c + 1) == batters.length) {
                    // add in the actual value
                    boxCSV += ", " + results.Off;
                    boxCSV += '\r\n';

                    ssline.push(results.Off);
                } else {
                    boxCSV += ", ";
                }

            }

            boxSS.push(ssline);

            // ******* defense total
            batters = details['OverallDefense'];
            ssline = [];
            for (var c = 0; c < batters.length; c++) {
                var column = batters[c];


                // remove the "empty" as it is just for readablity of the code
                if (column == "empty")
                    column = "";
                boxCSV += column;
                ssline.push(column);

                if ((c + 1) == batters.length) {
                    boxCSV += ", " + results.Def;
                    boxCSV += '\r\n';

                    ssline.push(results.Def);
                } else {
                    boxCSV += ", ";
                }
            }
            boxSS.push(ssline);

            // blank line
            boxCSV += '\r\n';
            boxSS.push(blankSSline);

            // ******* pitchers
            boxCSV += ">>> PITCHING " + '\r\n';

            boxSS.push([">>> " + teamNames[team], "PITCHING"]);

            if (team == 0) {
                headvp = boxSS.length;
            } else {
                headhp = boxSS.length;
            }

            var pitchers = details['Pitchers'];


            var linestats = score.Visit.Pitchers;
            if (team == 1)
                linestats = score.Home.Pitchers;

            var numberlines = linestats.length;

            for (var lines = -1; lines < numberlines; lines++) {
                ssline = [];
                for (var c = 0; c < pitchers.length; c++) {
                    var column = pitchers[c];

                    // remove the "empty" as it is just for readablity of the code
                    if (column == "empty") {
                        column = "";
                    } else {
                        if (lines >= 0) {
                            if (column == "Name")
                                column = "FullName";
                            if (column == "DEC")
                                column = "Decision";

                            //  column = linestats[lines][column];
                            column = formatValue(column, linestats[lines][column]);
                        }
                    }
                    boxCSV += column;
                    ssline.push(column);

                    if ((c + 1) == pitchers.length) {
                        boxCSV += '\r\n';
                    } else {
                        boxCSV += ", ";
                    }

                }
                boxSS.push(ssline);
            }


            // ******* pitchers total
            pitchers = details['Pitchers'];
            linestats = score.Visit.PitchingTotals;
            if (team == 1)
                linestats = score.Home.PitchingTotals;

            ssline = [];
            for (var c = 0; c < pitchers.length; c++) {
                var column = pitchers[c];

                // remove the "empty" as it is just for readablity of the code
                if (column == "empty") {
                    column = "";
                } else {
                    // column = linestats[column];
                    column = formatValue(column, linestats[column]);
                }
                boxCSV += column;
                ssline.push(column);

                if ((c + 1) == pitchers.length) {
                    boxCSV += '\r\n';
                } else {
                    boxCSV += ", ";
                }
            }
            boxSS.push(ssline);

            // blank line
            boxCSV += '\r\n';
            boxSS.push(blankSSline);

            // end of one team
        }

        //  if (row == 0 || row == headvb || row == headhb || row == headvp || row == headhp) {
        var headerInfo = {
            headvb: headvb,
            headhb: headhb,
            headvp: headvp,
            headhp: headhp
        };
        return ({ boxSS: boxSS, headerInfo: headerInfo });
    }
}