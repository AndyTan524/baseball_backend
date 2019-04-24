var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var playerHelper = require('../helpers/playerHelper');
var sim = require('../helpers/simulation');
 
var utils = require('../helpers/utils');
var simulationHelper = require('../helpers/simulationHelper');

var mongoose = require('mongoose');
var Player = mongoose.model('Player');
var League = mongoose.model('League');
var Roster = mongoose.model('Roster');
var SimPlayer = mongoose.model('SimPlayer');

var Pitching = mongoose.model('Pitching');
var Batting = mongoose.model('Batting');
var Baserunning = mongoose.model('Baserunning');
var Fielding = mongoose.model('Fielding');
var Log = mongoose.model('Log');

var R = require("r-script");

var leagueHelper = require("../helpers/leagueHelper");

router.get('/api/simulation/run', function (req, res) {

    res.status(200).json({ status: 200, msg: "not working yet" });
}); 


router.get('/api/simulation/game/team', function (req, res) {
    var date = req.query.date;
    var team = req.query.team;
    var lid = req.query.lid;

    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            League.findOne({ _id: req.query.lid }, function (err, league) {
                if (err) {
                    res.status(500).json(err);
                }
                else if (league) {
                    league.Messages = [];
                    league.Trades = [];
                    league.Owners = [];
                    league.Support = [];

                    var selectedTeam = league.Teams.filter(function (value) {
                        return value.TeamName == team.toUpperCase();
                    });

                    if (selectedTeam.length > 0) {
                        Roster.findOne({ TeamId: selectedTeam[0]._id, LeagueId: lid }, function (err, roster) {

                            if (err) {
                                res.status(500).json({ status: 500, msg: err.errmsg });
                            }
                            else if (roster) {
                                var players = roster.FortyMan;
                                playerHelper.getDailyBatting(date, 0, players, function (data) {
                                    playerHelper.getDailyPitching(date, 0, data, function (data) {
                                        playerHelper.getDailyBaseRunning(date, 0, data, function (data) {
                                            playerHelper.getDailyFielding(date, 0, data, function (data) {
                                                //var output = data.filter(function (value) {
                                                //    return ((value.Batting && value.Batting.GameString != null) || (value.Pitching && value.Pitching.GameString != null) ||
                                                //        ((value.Baserunning && value.Baserunning.GameString != null) || (value.Fielding && value.Fielding.GameString != null)));
                                                //});
                                                res.status(200).json({ status: 200, msg: "success", players: data });
                                            });
                                        });
                                    });
                                });
                            }
                            else {
                                res.status(500).json({ status: 500, msg: "Roster not found" });
                            }
                        });
                    }
                    else {
                        res.status(500).json({ status: 500, msg: "Team not found in league" });
                    }
                }
                else {
                    res.status(500).json({ status: 500, msg: "League not found" });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    }); 
}); 



router.get('/api/simulation/game/date', function (req, res) {
    var date = req.query.date;
    var team = req.query.team;
    var lid = req.query.lid;

    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            League.findOne({ _id: req.query.lid }, function (err, league) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    

                    var selectedTeam = league.Teams.filter(function (value) {
                        return value.TeamName == team.toUpperCase();
                    });

                    if (selectedTeam.length > 0) {
                        
                    }
                    else {
                        res.status(500).json({ status: 500, msg: "Team not found in league" });
                    }
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    }); 
}); 


router.get('/api/simulation/calendar', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var date = req.query.date;
            League.findOne({ _id: req.query._id }, function (err, league) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    simulationHelper.getGamesByDate(date, function (err, games) {
                        res.status(200).json(games);
                    }); 
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ********************************************************
//
// DEPRICATED!!!!
// run a single game simulation - old version 
//
// ********************************************************
router.post('/api/simulation/game/run', function (req, res) {
    
    var game = req.body.game;

    if (game) {
        var homeTeam = game.home;
        var visitTeam = game.visit;
        game._id = req.body.lid;

        var league = {
            Teams: [{ TeamName: game.home.name, _id: "" }],
            _id: game._id
        }



        simulationHelper.getFortManByLeague(req.body.lid, homeTeam, function (err, homeRoster) {
            if (!homeRoster || homeRoster.length == 0) {
                isRetry = true;
                leagueHelper.getTeam(league, function (err, dbLeague) {
                    playerHelper.generateDefaultPlayersByTeam(dbLeague, function (homeRoster) {
                        simulationHelper.getFortManByLeague(req.body.lid, visitTeam, function (err, visitRoster) {
                            if (!visitRoster || visitRoster.length == 0) {
                                league.Teams[0].TeamName = visitTeam;
                                leagueHelper.getTeam(league, function (err, dbLeague) {
                                    playerHelper.generateDefaultPlayersByTeam(dbLeague, function (visitRoster) {
                                        sim.runSimulation(homeRoster, visitRoster, game, function (boxscore) {
                                            res.status(200).json({ status: 200, msg: "success", boxscore: boxscore });
                                        });
                                    });
                                });
                            }
                            else {
                                sim.runSimulation(homeRoster, visitRoster, game, function (boxscore) {
                                    res.status(200).json({ status: 200, msg: "success", boxscore: boxscore });
                                });
                            }
                        });
                    });
                });
            }
            else {
                simulationHelper.getFortManByLeague(req.body.lid, visitTeam, function (err, visitRoster) {
                    if (!visitRoster || visitRoster.length == 0) {
                        league.Teams[0].TeamName = visitTeam;
                        leagueHelper.getTeam(league, function (err, dbLeague) {
                            playerHelper.generateDefaultPlayersByTeam(dbLeague, function (visitRoster) {
                                sim.runSimulation(homeRoster, visitRoster, game, function (boxscore) {
                                    res.status(200).json({ status: 200, msg: "success", boxscore: boxscore });
                                });
                            });
                        });
                    }
                    else {
                        sim.runSimulation(homeRoster, visitRoster, game, function (boxscore) {
                            res.status(200).json({ status: 200, msg: "success", boxscore: boxscore });
                        });
                    }
                });
            }
        });
    }
    else {
        res.status(500).json({ status: 500, msg: "no game to play", boxscore: {} });
    }
    /*
    var visitRoster = roster.getTeamWithRoster(visitTeam, { useFakeData: false });

    pitchers.setPitchers(game, visitTeam, visitRoster);
    pitchers.setPitchers(game, homeTeam, homeRoster); 

    lineup.setLineup(game, visitTeam, visitRoster);
    lineup.setLineup(game, homeTeam, homeRoster); 

    simGame.simulateOffense(visitTeam.batters.battingOrder, visitRoster);
    simGame.simulateOffense(homeTeam.batters.battingOrder, homeRoster);

    var boxScore = playGame.playGame(game, visitTeam, visitRoster, homeTeam, homeRoster);
    */
    //recordGame(boxScore, game, g);

});

 // ********************************************************
//
// play a single game simulation
//
// ********************************************************
router.post('/api/simulation/game/play', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var game = req.body.game;
            game.leagueId = req.body.lid;
            var gameRange = {From: 20170301, To: 20170510};
            if( req.body.From) {
                gameRange.From = req.body.From;
            }
            if( req.body.To ) {
                gameRange.To = req.body.To;
            } else {
                var moment = require('moment-timezone');
                moment.tz.setDefault("America/Los_Angeles");
    
                var endDate = moment();
                endDate = endDate.subtract(3, "days");
                endDate = endDate.format("YYYYMMDD");
                // could be endDate = = moment().subtract(3, "days").format("YYYYMMDD");
            }

            // ******************************* override here ************************************
            var Override = null;
            if( req.body.Override) {
                Override = req.body.Override;
            }
            sim.getLineupsAndPlayGame(game, gameRange, Override, function (err, boxscore, visitTeam, visitRoster, homeTeam, homeRoster) {
                if( err ) {
                    res.status(500).json({ status: 500, msg: "gameplay error" });
                    
                } else {
 
                    // store the boxscore now.
                    var teamData = {};
                    teamData["visitTeam"] = visitTeam;
                    teamData["homeTeam"] = homeTeam;
                    teamData["visitRoster"] = visitRoster;
                    teamData["homeRoster"] = homeRoster;

                    if (Override) {
                        function putInDecision(boxscore, decision, letter) {

                            if (Override.Decision[decision] && Override.Decision[decision] != "") {
                                var found = false;
                                for (var p = 0; p < boxscore.Home.Pitchers.length; p++) {
                                    if (boxscore.Home.Pitchers[p].FullName == Override.Decision[decision]) {
                                        found = true;
                                        boxscore.summary.Decision[decision] = Override.Decision[decision];
                                        boxscore.summary.Decision[decision + "Id"] = boxscore.Home.Pitchers[p].PlayerId;
                                        if( boxscore.Home.Pitchers[p].Decision == "BS" && letter=="L" ) {
                                            letter = "BS, L"
                                        }
                                        boxscore.Home.Pitchers[p].Decision = letter;
                                        break;
                                    }
                                }
                                if (!found) {
                                    // look on visitor's team;
                                    for (var p = 0; p < boxscore.Visit.Pitchers.length; p++) {
                                        if (boxscore.Visit.Pitchers[p].FullName == Override.Decision[decision]) {
                                            found = true;
                                            boxscore.summary.Decision[decision] = Override.Decision[decision];
                                            boxscore.summary.Decision[decision + "Id"] = boxscore.Visit.Pitchers[p].PlayerId;
                                            if( boxscore.Visit.Pitchers[p].Decision == "BS" && letter=="L" ) {
                                                letter = "BS, L"
                                            }
                                            boxscore.Visit.Pitchers[p].Decision = letter;
                                            break;
                                        }
                                    }
                                }
                            }
                        }

                        function putInHold(boxscore, holds) {

                            if (Override.Decision.Hold && Override.Decision.Hold.length > 0) {

                                var holdIndex = 0;
                                for (var h = 0; h < Override.Decision.Hold.length; h++) {
                                    var nextHold = Override.Decision.Hold[h];

                                    if (nextHold && nextHold != "") {
                                        var found = false;
                                        for (var p = 0; p < boxscore.Home.Pitchers.length; p++) {
                                            if (boxscore.Home.Pitchers[p].FullName == nextHold) {
                                                found = true;
                                                boxscore.summary.Decision.Hold[holdIndex] = nextHold;
                                                boxscore.summary.Decision.HoldId[holdIndex] = boxscore.Home.Pitchers[p].PlayerId;
                                                boxscore.Home.Pitchers[p].Decision = "HD";
                                                holdIndex++
                                                break;
                                            }
                                        }
                                        if (!found) {
                                            // look on visitor's team;
                                            for (var p = 0; p < boxscore.Visit.Pitchers.length; p++) {
                                                if (boxscore.Visit.Pitchers[p].FullName == nextHold) {
                                                    found = true;
                                                    boxscore.summary.Decision.Hold[holdIndex] = nextHold;
                                                    boxscore.summary.Decision.HoldId[holdIndex] = boxscore.Visit.Pitchers[p].PlayerId;
                                                    boxscore.Visit.Pitchers[p].Decision = "HD";
                                                    holdIndex++
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // stuff the data into the boxscore...
                      if (Override.Decision) {
                        if ((Override.Decision.Win && Override.Decision.Win != "")
                          || (Override.Decision.Loss && Override.Decision.Loss != "")
                          || (Override.Decision.Blown && Override.Decision.Blown != "")
                          || (Override.Decision.Save && Override.Decision.Save != "")
                          || (Override.Decision.Hold && Override.Decision.Hold.length > 0)) {
                                // first clear out any previous winners...
                                for( var p=0; p<boxscore.Home.Pitchers.length; p++) {
                                    boxscore.Home.Pitchers[p].Decision = ""; 
                                }
                                for( var p=0; p<boxscore.Visit.Pitchers.length; p++) {
                                    boxscore.Visit.Pitchers[p].Decision = ""; 
                                }
                                putInDecision( boxscore, "Win", "W");
                                putInDecision( boxscore, "Blown", "BS"); // do this first in case he also lost.
                                putInDecision( boxscore, "Loss", "L");
                                putInDecision( boxscore, "Save", "SV");
                                putInHold(boxscore, Override.Decision.Hold);
                            }

                        }

                        // log the override here
                        var oMessage = "Override (" + req.body.game.title + "): ";
                        if( Override.Decision && Override.Decision.Win && Override.Decision.Win != "") {
                            oMessage += " ** DECISION **. "
                        }
                        var oPCount = 0;
                        var oIPCount = 0;
                        if( Override.vPitchers ) {
                            for( var p=0; p<Override.vPitchers.length; p++) {
                                if( Override.vPitchersIP[p]) {
                                    oIPCount++;
                                }
                            }
                        }
                        if( Override.hPitchers) {
                            for( var p=0; p<Override.hPitchers.length; p++) {
                                if( Override.hPitchersIP[p]) {
                                    oIPCount++;
                                }
                            }
                        }
                        oMessage += "Pitchers changed: " + oIPCount;
                        var now = moment().tz("America/Los_Angeles");
                        var nowF = now.format("llll") + " PST";
                        
                        console.log("Override " + oMessage);
                        
                        
                        Log.create({
                            Message: "Pitchers Override: " + oMessage + " at " + nowF,
                            Level: "Simulations",
                            Route: "Simulation",
                            Method: "Simulation/game/play",
                            CreatedUTC: new Date().toISOString()
                        });
                       
                    } else {
                        // record original pitching staff in case of override
                        boxscore.Home["OPitchers"] = boxscore.Home.Pitchers;
                        boxscore.Visit["OPitchers"] = boxscore.Visit.Pitchers;
                    }
                    sim.storeBoxscore( req.body.game, boxscore, teamData, Override, function( err, response, overrideResults ){
                        if( !err ) {
                            boxId = response._doc._id.toString();

                            if( Override && Override.UpdateOfficialScore  && Override.UpdateOfficialScore === true ) {
                                leagueHelper.overwriteWonLossRecords(game.leagueId, 2017, boxscore, game.gameId, overrideResults, function (err) {
                                    res.status(200).json({ status: 200, msg: "success", boxscore: boxscore, boxId: boxId, teamData: teamData });
                                });
                            } else {
                            res.status(200).json({ status: 200, msg: "success", boxscore: boxscore, boxId: boxId, teamData: teamData });
                            }

                        } else {
                            var message = "Gameplay Error";
                            if( err.msg) {
                                message = err.msg;
                                if( err.type && err.type=="Was Official") {
                                    boxId = null;
                                    if( response._doc._id)
                                        boxId = response._doc._id.toString();
                                    res.status(201).json({ status: 201, msg: message, boxscore: boxscore, boxId: boxId, teamData: teamData });
                                    return;
                                }
                            } 
                            res.status(201).json({ status: 201, msg: message, boxscore: boxscore, boxId: null });

                        }
  
                    });
                }

            });
        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});


router.get('/api/simulation/game/save', function (req, res) {

    var game = req.query;
    var homeTeam = game.home;
    var visitTeam = game.visit;

    
    createStats(0, 9736, res);


    //simulationHelper.getFortManByLeague(req.query.lid, homeTeam, function (err, homeRoster) {
    //    //simulationHelper.getFortManByLeague(req.body.lid, visitTeam, function (err, visitRoster) {
    //    simulationHelper.getPlayerStats(homeRoster, game.date, function (roster) {

    //        res.status(200).json({ status: 200, msg: "success", roster: roster });

            
    //    });
    //    //});
    //}); 
});

function createStats(start, end, res) {
    console.log(start);
    if (start == end) {
        res.status(200).json({ status: 200, msg: "success" });
    }
    else {
        Player.find({}, function (err, players) {
            if (err) {
                res.status(500).json({ status: 500, msg: err.errmsg });
            }
            else if (players.length > 0) {
                //simulationHelper.getPlayerStats(players, "20170412", function (roster) {
                var player = players[0]._doc;
                player.Fielding = {
                    daily: []
                }
                player.Batting = {
                    daily: []
                }
                player.Baserunning = {
                    daily: []
                }
                player.Pitching = {
                    daily: []
                }
                SimPlayer.findOne({ MlbId: player.MlbId }, function (err, dbPlayer) {
                    if (dbPlayer) { 
                        createStats(++start, end, res); 
                    }
                    else {
                        SimPlayer.create(player, function (err, result) {
                            createStats(++start, end, res);
                        });
                    }
                });
                //});
            }
            else {
                createStats(++start, end, res);
            }
        }).skip(start).limit(1);
    }
}

router.get('/api/simulation/game/stats/save', function (req, res) {

    var game = req.query;
    var homeTeam = game.home;
    var visitTeam = game.visit;
    var date = req.query.date;

    var qry = { GameDate: date };
    var proj = { "GameDate": 1, "Stats.MLBId": 1 };

    Batting.findOne(qry, proj, function (err, results) {
        if (results && results.Stats && results.Stats.length > 500) {
            updateStats(results.Stats.slice(0, 500), 0, date, res);
        }
        else {
            res.status(500).json({ status: 500, msg: "no data" });
        }
    });

    
});

function updateStats(players, index, date, res) {
    console.log(index);
    if (players.length == index + 1) { 
        res.status(200).json({ status: 200, msg: "success" });
    }
    else {
       
        SimPlayer.findOne({ MlbId: players[index].MLBId }, function (err, dbPlayer) {
            if (dbPlayer) {
                var baseRunningStats = dbPlayer.Baserunning.daily;
                var battingStats = dbPlayer.Batting.daily;
                var fieldingStats = dbPlayer.Fielding.daily;
                var pitchingStats = dbPlayer.Pitching.daily ? dbPlayer.Pitching.daily : [];

                var isUpdate = false;

                playerHelper.getDailyBaseRunning(date, 0, [dbPlayer], function (data) {
                    if (data) {
                        if (data[0]._doc.Baserunning && data[0]._doc.Baserunning.GameDate) {
                            baseRunningStats.push(data[0]._doc.Baserunning)
                            isUpdate = true;
                        }
                        data[0]._doc.Baserunning = { daily: filterDuplicates(baseRunningStats) };

                        playerHelper.getDailyBatting(date, 0, [dbPlayer], function (data) {
                            if (data[0]._doc.Batting && data[0]._doc.Batting.RBI) {
                                battingStats.push(data[0]._doc.Batting)
                                isUpdate = true;
                            }
                            data[0]._doc.Batting = { daily: battingStats };

                            playerHelper.getDailyFielding(date, 0, [dbPlayer], function (data) {
                                if (data[0]._doc.Fielding && data[0]._doc.Fielding.Ethrow) {
                                    fieldingStats.push(data[0]._doc.Fielding)
                                    isUpdate = true;
                                }
                                data[0]._doc.Fielding = { daily: fieldingStats };

                                playerHelper.getDailyPitching(date, 0, [dbPlayer], function (data) {
                                    if (data[0]._doc.Pitching && data[0]._doc.Pitching.ERA) {
                                        pitchingStats.push(data[0]._doc.Pitching)
                                        isUpdate = true;
                                    }
                                    data[0]._doc.Pitching = { daily: pitchingStats };

                                    if (isUpdate) {
                                        savePlayer(data[0], function () {
                                            updateStats(players, ++index, date, res);
                                        });
                                    } else {
                                        updateStats(players, ++index, date, res);
                                    }
                                });
                            });
                        });
                    }
                    else {
                        updateStats(players, ++index, date, res)
                    }
                });
            }
            else {
                updateStats(players, ++index, date, res)
            }
        });
    }
}

function filterDuplicates(input) {
    var unique = [];
    for (var i = 0; i < input.length; i++){
        var object = input[i];
        var filter = unique.filter(function (value) {
            return value.GameString == object.GameString;
        });
        if (filter.length == 0) {
            unique.push(object);
        }
    }
    return unique;
}

function savePlayer(dbPlayer, callback) {
    if (dbPlayer) {
        dbPlayer.markModified("Baserunning");
        dbPlayer.markModified("Batting");
        dbPlayer.markModified("Pitching");
        dbPlayer.markModified("Fielding");
        dbPlayer.save(function (err, response) {
            console.log(response.MlbId);
            callback();
        });
    }
    else {
        callback();
    }
}

module.exports = router;