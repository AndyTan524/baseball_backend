var express = require('express');
var router = express.Router();

var accessHelper = require('../helpers/accessHelper');
var playerHelper = require('../helpers/playerHelper');
var Simulation = require('../helpers/simulation');
var TeamInfo = require('../helpers/teamInfo');
var rosterHelper = require('../helpers/rosterHelper');
var messageHelper = require('../helpers/messageHelper');
var rosterLineupHelper = require('../helpers/rosterLineupHelper');

var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var Roster = mongoose.model('Roster');
var FreeAgents = mongoose.model('FreeAgent');
var League = mongoose.model('League');
var RosterLineup = mongoose.model('RosterLineup');
var Content = mongoose.model('Content');



var playerStatus = require('../helpers/playerStatus');

router.get('/api/rosters/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (roster) {
                    var output = roster
                    if (req.query.type) {
                        switch (req.query.type) {
                            case "al":
                                output = roster.FortyManAL;
                                break;
                            case "nl":
                                output = roster.FortyManNL;
                                break;
                            default:
                                break;
                        }
                    }
                    res.status(200).json({ status: 200, msg: "Found roster", roster: output });
                }
                else {
                    res.status(200).json({ status: 500, msg: "No data", roster: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});


// ******************************************************************
//
// get/set extras: 
//     "Extra" : {
//    "Allow3DaysRest" : true,
//    "LastAllowed3DaysRest" : 94,
//    "Coordinators" : {Pitching: false, Batting: false, Baserunning: false, Infield: false, Outfield: false, Catching: false}
// },
// ***********************************************************************************************************************************
router.get('/api/teams/roster/extras/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            Roster.findOne(query, { LeagueId: 1, TeamId: 1, Extra: 1, TeamAbbr: 1 }, function (err, roster) {
                if (err) {
                    res.status(201).json({ status: 201, msg: err.errmsg });
                }
                else if (roster) {
                    res.status(200).json({ status: 200, msg: "Found roster", roster: roster });
                }
                else {
                    res.status(201).json({ status: 201, msg: "No data", roster: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});

router.post('/api/teams/roster/extras/set', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {

            Roster.findOne({ LeagueId: req.body.lid, TeamId: req.body.tid }, { LeagueId: 1, TeamId: 1, Extra: 1, TeamAbbr: 1 }, function (err, roster) {
                if (err) {
                    res.status(201).json({ status: 201, msg: err.errmsg });
                } else if (roster) {
                    if (roster.Extra) {
                        roster.Extra.Coordinators = req.body.coaches;
                    } else {
                        roster.Extra = {
                            Coordinators: req.body.coaches,
                            Allow3DaysRest: true,
                            LastAllowed3DaysRest: 91
                        }
                    }
                    roster.markModified("Extra");
                    roster.save(function (err, results) {
                        if (err) {
                            res.status(201).json({ status: 201, msg: "Error, coaches not saved" });
                        } else {
                            res.status(200).json({ status: 200, msg: "Coaches saved" });
                        }
                    });

                }
                else {
                    res.status(201).json({ status: 201, msg: "No data", roster: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});

router.get('/api/rosters/lineup/validate', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var query = {
                $and: [
                    { _id: req.query.lid },
                    { "Teams._id": req.query.tid }
                ]
            };
            League.findOne(query, { "Teams.$": 1, _id: 1 }, function (err, team) {
                var qry = { LeagueId: req.query.lid, TeamId: req.query.tid };
                // need to see if the depthchart is in the normal ROSTER not the roster lineup
                /*
                RosterLineup.findOne({ LeagueId: req.query.lid, TeamId: req.query.tid }, function (err, roster) {
                    if (err) {
                        res.status(500).json({ status: 500, msg: err.errmsg, isValid: false });
                    } 
                    else {
                        res.status(200).json({ status: 200, msg: "Success", isValid: roster ? true : false });
                    }
                });
                
                */
                Roster.findOne({ LeagueId: req.query.lid, TeamId: req.query.tid }, function (err, roster) {
                    if (err) {
                        res.status(500).json({ status: 500, msg: err.errmsg, isValid: false });
                    }
                    else {
                        if (roster && roster.DepthChartNL)
                            res.status(200).json({ status: 200, msg: "Success", isValid: roster ? true : false });
                        else
                            res.status(500).json({ status: 500, msg: "Valid roster, but no DepthChart yet", isValid: false });
                    }
                });
            });

        } else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});

// *****************************************************
//
// /api/rosters/lineup/eligible/generateDepthChart
// 
// used by the front end to set rosterLineup for a team
//
// *****************************************************

router.get('/api/rosters/lineup/eligible/generateDepthChart', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;

        if (isValid) {
            var query = {
                $and: [
                    { _id: req.query.lid },
                    { "Teams._id": req.query.tid }
                ]
            };
            League.findOne(query, { "Teams.$": 1, _id: 1 }, function (err, team) {
                Roster.findOne({ LeagueId: req.query.lid, TeamId: req.query.tid }, function (err, roster) {
                    if (err) {
                        res.status(500).json({ status: 500, msg: err.errmsg, eligibleLineup: [], activeRoster: [] });
                    }
                    else if (!roster) {
                        res.status(500).json({ status: 500, msg: "No roster", eligibleLineup: [], activeRoster: [] });
                    }
                    else {
                        if (!req.query.game) {
                            var selectedTeam = team.Teams[0].r_name;
                            game = { leagueId: req.query.lid, home: selectedTeam, visit: selectedTeam, dhGame: false };
                            // figure out dhGame
                            if (TeamInfo.ALTeams.indexOf(selectedTeam) >= 0) {
                                game.dhGame = true;
                            }
                        }

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
                        Simulation.getEligibleLineup(game, true, "no game", gameRange, function (players) {
                            var qry = { LeagueId: req.query.lid, TeamId: req.query.tid };
                            RosterLineup.findOne(qry, function (err, rosterLineup) {
                                if (err) {
                                    res.status(500).json({ status: 500, msg: err.errmsg, eligibleLineup: [], activeRoster: [] });
                                }
                                else if (rosterLineup) {
                                    // should I update? probably need to add something to see if it is required to run it again. 

                                    rosterLineup.DepthChartNL = players;
                                    rosterLineup.markModified("DepthChartNL");
                                    rosterLineup.save(rosterLineup, function (err, result) {

                                        // saved there, let's see if we need to save it in the roster

                                        if (!roster.DepthChartNL) {
                                            // need to save the players
                                            // then save this new depth chart to the roster
                                            roster.DepthChartNL = players;
                                            roster.markModified("DepthChartNL");
                                            roster.save(function (err, result) {
                                                res.status(200).json({ status: 200, msg: "Success" });
                                            })
                                        } else {
                                            res.status(200).json({ status: 200, msg: "Success" });
                                        }
                                    });
                                    //res.status(200).json({ status: 200, msg: "Success" });
                                }
                                else {
                                    // need to save the players
                                    // first save this new depth chart to the roster
                                    roster.DepthChartNL = players;
                                    roster.markModified("DepthChartNL");
                                    roster.save(function (err, result) {

                                        // then be sure it's saved in the rosterlineup as it doesn't exist for this team.
                                        var newLineup = {
                                            LeagueId: team._id.toString(),
                                            TeamId: team.Teams[0]._id,
                                            CreatedUTC: new Date().toISOString(),
                                            DepthChartNL: players,
                                            TeamName: team.Teams[0].r_name
                                        };
                                        RosterLineup.create(newLineup, function (err, result) {
                                            res.status(200).json({ status: 200, msg: "Success" });
                                        });
                                    })

                                }
                            });
                        })
                    }
                });
            });

        } else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});

router.get('/api/rosters/lineup/eligible', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var query = {
                $and: [
                    { _id: req.query.lid },
                    { "Teams._id": req.query.tid }
                ]
            };
            League.findOne(query, { "Teams.$": 1, _id: 1 }, function (err, team) {
                Roster.findOne({ LeagueId: req.query.lid, TeamId: req.query.tid }, function (err, roster) {
                    if (err) {
                        res.status(500).json({ status: 500, msg: err.errmsg, eligibleLineup: [], activeRoster: [] });
                    }
                    else if (!roster) {
                        res.status(500).json({ status: 500, msg: "No roster", eligibleLineup: [], activeRoster: [] });
                    }
                    else {
                        if (!req.query.game) {
                            var selectedTeam = team.Teams[0].r_name;
                            game = { leagueId: req.query.lid, home: selectedTeam, visit: selectedTeam, dhGame: false };
                            // figure out dhGame
                            if (TeamInfo.ALTeams.indexOf(selectedTeam) >= 0) {
                                game.dhGame = true;
                            }
                        }

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

                        Simulation.getEligibleLineup(game, true, "no game", gameRange, function (players) {
                            var qry = { LeagueId: req.query.lid, TeamId: req.query.tid };
                            RosterLineup.findOne(qry, function (err, rosterLineup) {
                                if (err) {
                                    res.status(500).json({ status: 500, msg: err.errmsg, eligibleLineup: [], activeRoster: [] });
                                }
                                else if (rosterLineup) {
                                    // should I update? probably
                                    rosterLineup.Players = players;
                                    rosterLineup.markModified("Players");
                                    rosterLineup.save(rosterLineup, function (err, result) {
                                        res.status(200).json({ status: 200, msg: "Success", eligibleLineup: players });
                                    });
                                }
                                else {
                                    // need to save the players
                                    var newLineup = {
                                        LeagueId: team._id.toString(),
                                        TeamId: team.Teams[0]._id,
                                        CreatedUTC: new Date().toISOString(),
                                        Players: players,
                                        TeamName: team.Teams[0].r_name
                                    };
                                    RosterLineup.create(newLineup, function (err, result) {
                                        res.status(200).json({ status: 200, msg: "Success", eligibleLineup: players });
                                    });
                                }
                            });
                        })
                    }
                });
            });

        } else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});
// ************************
// 
// getEligibleForLineup
//    lid: league id
//    tid: team id
//    teamName: r_name of team
//    game: null, or today's game
//
// *******************************
router.get('/api/rosters/getEligibleForLineup', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var query = {
                $and: [
                    { _id: req.query.lid },
                    { "Teams._id": req.query.tid }
                ]
            };
            League.findOne(query, { "Teams.$": 1 }, function (err, team) {
                Roster.findOne({ LeagueId: req.query.lid, TeamId: req.query.tid }, function (err, roster) {
                    if (err) {
                        res.status(500).json({ status: 500, msg: err.errmsg, eligibleLineup: [], activeRoster: [] });
                    }
                    else if (!roster) {
                        res.status(500).json({ status: 500, msg: "No roster", eligibleLineup: [], activeRoster: [] });
                    }
                    else {
                        if (!req.query.game) {
                            var selectedTeam = team.Teams[0].r_name;
                            game = { leagueId: req.query.lid, home: selectedTeam, visit: selectedTeam, dhGame: false };
                            // figure out dhGame
                            if (TeamInfo.ALTeams.indexOf(selectedTeam) >= 0) {
                                game.dhGame = true;
                            }
                        }

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

                        Simulation.getEligibleLineup(game, true, "no game", gameRange, function (players) {

                            res.status(200).json({ status: 200, msg: "Success", eligibleLineup: players, activeRoster: roster.FortyManNL });

                        })
                    }
                });
            });

        } else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});

router.get('/api/rosters/get-waiver-wire', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid }
                ]
            };
            Roster.find(query, { TeamAbbr: 1, TeamId: 1, FortyManNL: 1, NonRoster: 1 }, function (err, teams) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (teams) {

                    var lists = {
                        "A": 10,
                        "TradeWaivers": [],
                        "Waivers": [],
                        Forty: -1,
                        NoForty: []
                    }

                    var tlist = [];
                    var wlist = [];

                    for (t = 0; t < teams.length; t++) {
                        var team = teams[t];
                        if (!team.FortyManNL) {
                            lists.Forty = t;
                            lists.NoForty.push[t];
                        } else {

                            for (i = 0; i < team.FortyManNL.length; i++) {

                                var player = team.FortyManNL[i];

                                if (player.Status) {

                                    player["Team"] = team.TeamAbbr;
                                    player["TeamId"] = team.TeamId;
                                    player["StatusMessage"] = playerHelper.getPlayerStatusMsg(player);

                                    if (player.Status >= playerStatus.Waivers && player.Status < playerStatus.WaiversEnd) {
                                        wlist.push(player);
                                    }

                                    if (player.TradeStatus && player.TradeStatus >= playerStatus.TradeWaivers && player.TradeStatus < playerStatus.TradeWaiversEnd) {
                                        tlist.push(player);
                                    }

                                }

                            }

                            if (team.NonRoster) {
                                for (i = 0; i < team.NonRoster.length; i++) {
                                    var player = team.NonRoster[i];

                                    if (player.Status) {
                                        player["Team"] = team.TeamAbbr;
                                        player["TeamId"] = team.TeamId;
                                        player["StatusMessage"] = playerHelper.getPlayerStatusMsg(player);

                                        if (player.Status >= playerStatus.Waivers && player.Status < playerStatus.WaiversEnd) {
                                            wlist.push(player);
                                        }

                                        if (player.TradeStatus && player.TradeStatus >= playerStatus.TradeWaivers && player.TradeStatus < playerStatus.TradeWaiversEnd) {
                                            tlist.push(player);
                                        }
                                    }
                                }
                            }
                        }
                    }

                    lists.TradeWaivers = tlist;
                    lists.Waivers = wlist;

                    res.status(200).json({ status: 200, msg: "Created Waiver Wire", lists });
                }
                else {
                    res.status(200).json({ status: 500, msg: "No data", teams: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});

router.get('/api/rosters/get-trading-block', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid }
                ]
            };
            Roster.find(query, function (err, teams) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (teams) {

                    var lists = {
                        "A": 10,
                        "TradeWaivers": [],
                        "TradingBlock": [],
                        "Teams": [],
                        "TeamId": [],
                        Forty: -1
                    }

                    var tlist = [];
                    var wlist = [];

                    for (t = 0; t < teams.length; t++) {
                        var team = teams[t];
                        var teamHasTrades = false;
                        if (!team.FortyManNL) {
                            lists.Forty = t;
                        } else {

                            for (i = 0; i < team.FortyManNL.length; i++) {

                                var player = team.FortyManNL[i];

                                if (player.TradeStatus) {

                                    player["Team"] = team.TeamAbbr;
                                    player["TeamId"] = team.TeamId;
                                    player["StatusMessage"] = playerHelper.getPlayerStatusMsg(player);

                                    if (player.TradeStatus >= playerStatus.TradeWaivers && player.TradeStatus < playerStatus.TradeWaiversEnd) {
                                        wlist.push(player);
                                        teamHasTrades = true;
                                    }

                                    if (player.TradeStatus >= playerStatus.TradingBlock && player.TradeStatus < playerStatus.TradingBlockEnd) {
                                        tlist.push(player);
                                        teamHasTrades = true;
                                    }

                                }

                            }

                            if (team.NonRoster) {
                                for (i = 0; i < team.NonRoster.length; i++) {
                                    var player = team.NonRoster[i];

                                    if (player.TradeStatus) {
                                        player["Team"] = team.TeamAbbr;
                                        player["TeamId"] = team.TeamId;
                                        player["StatusMessage"] = playerHelper.getPlayerStatusMsg(player);

                                        if (player.Status >= playerStatus.TradeWaiers && player.Status < playerStatus.TradeWaiversEnd) {
                                            wlist.push(player);
                                            teamHasTrades = true;
                                        }

                                        if (player.TradeStatus >= playerStatus.TradingBlock && player.TradeStatus < playerStatus.TradingBlockEnd) {
                                            tlist.push(player);
                                            teamHasTrades = true;
                                        }
                                    }
                                }
                            }
                        }
                        if (teamHasTrades) {
                            lists.Teams.push(team.TeamAbbr);
                            lists.TeamId.push(team.TeamId);
                        }
                    }

                    lists.TradeWaivers = wlist;
                    lists.TradingBlock = tlist;

                    res.status(200).json({ status: 200, msg: "Created Trading Block", lists });
                }
                else {
                    res.status(200).json({ status: 500, msg: "No data", teams: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});

router.get('/api/rosters/get-freeagents', function (req, res) {
    utils.validate(req, function (isValid, user) {

        var leagueQuery = {};

        if (req.query.lid) {
            leagueQuery = { "_id": req.query.lid };
        } else {
            res.status(401).json({ status: 401, msg: "No league specified." });
        }

        var query = JSON.parse(req.query.query);
        var filter = JSON.parse(req.query.filter);
        var show = req.query.show;

        /*
        var nameQuery =  { "FullName": { $regex: ".*" + req.query.q + "*", $options: "ix" } };
        if (req.query.q == "") {
            nameQuery = { "LastName" :  {"$ne": ""}};
        }
        */

        var nameQueryRegex = "";
        if (req.query.q) {
            var nameQueryRegex = new RegExp(req.query.q, "i");
        }

        var positionQuery = [];
        if (filter.pitcher) {
            positionQuery.push("P");
        }
        if (filter.fielder) {
            positionQuery.push("CF");
            positionQuery.push("RF");
            positionQuery.push("LF");
            positionQuery.push("OF");
        }
        if (filter.catcher) {
            positionQuery.push("CA");
        }
        if (filter.infield) {
            positionQuery.push("SS");
            positionQuery.push("3B");
            positionQuery.push("2B");
            positionQuery.push("1B");
        }
        if (filter.outfield) {
            positionQuery.push("OF");
        }

        nonRosterLevels = {
            "ML": "ML",
            "Triple-A": "AAA",
            "Double-A": "AA",
            "High-A": "A+",
            "Low-A": "A-",
            "ADV-RK": "RA",
            "RK": "R",
            "EST": "EST",
            "DSL": "DSL"
        };
        var levelQuery = [];
        if (filter.levels) {
            Object.keys(filter.levels).forEach(function (key, index) {
                // key: the name of the object key
                // index: the ordinal position of the key within the object 
                var level = filter.levels[key];
                if (level == true) {
                    levelQuery.push(nonRosterLevels[key]);
                }
            });
        }

        var typeQuery = [];
        if (filter.type) {
            Object.keys(filter.type).forEach(function (type, index) {
                // type: the name of the object key in this case the type
                // index: the ordinal position of the key within the object 
                if (filter.type[type] == true) {
                    if (type == "none")
                        type = "";
                    typeQuery.push(type);
                }
            });
        }

        var limit = query.limit;
        var page = query.page;
        var sort = {};
        sort[query.sort] = query.dir;

        /*
        { "instock": { warehouse: "A", qty: 5 } } 
         { $and: [ { price: { $ne: 1.99 } }, { price: { $exists: true } } ] }
        */
        League.findOne({ _id: req.query.lid }, function (err, league) {
            if (err) {
                res.status(401).json({ status: 401, msg: err });
            } else {

                var moment = require('moment-timezone');
                for (i = league.FreeAgents.length - 1; i >= 0; i--) {
                    // start with name...
                    var freeagent = league.FreeAgents[i];
                    var freeagentname = league.FreeAgents[i].FullName;


                    if (freeagent.FullName == "") {
                        league.FreeAgents.splice(i, 1);
                    } else if (req.query.q != "" && !nameQueryRegex.test(freeagentname)) {
                        league.FreeAgents.splice(i, 1);
                    } else if (positionQuery.length != 0 && positionQuery.indexOf(freeagent.Position) == -1) {
                        league.FreeAgents.splice(i, 1);
                    } else
                        if (levelQuery.length != 0 && levelQuery.indexOf(freeagent.Level) == -1) {
                            league.FreeAgents.splice(i, 1);
                        } else if (typeQuery.length != 0 && typeQuery.indexOf(freeagent.Type) == -1) {
                            league.FreeAgents.splice(i, 1);
                        }
                        else {
                            // found a player, clean up his data.

                            if (freeagent.Status) {
                                league.FreeAgents[i]["StatusMessage"] = playerHelper.getPlayerStatusMsg(league.FreeAgents[i]);
                            }
                            var dob = freeagent.DOB
                            if (dob != "") {
                                try {
                                    var isoDate = new Date(dob).toISOString();
                                    var d = moment(isoDate);
                                    var age = moment().diff(d, 'years', true);
                                    league.FreeAgents[i]['Age'] = Math.floor(age);                                   
                                } catch (error) {
                                    league.FreeAgents[i]['Age'] = "";                                    
                                }
                            }

                        }
                }
                res.status(200).json({ status: 200, players: league.FreeAgents });
            }

        });
        /*
        FreeAgents.count(qry, function (err, count) {
             
            FreeAgents.find(qry, function (err, players) {
                if (err) {
                    res.status(200).json({ status: 200, msg: err });
                }
                else if (players) {
                
                    res.status(200).json({ status: 200, players: players, pages: count / limit, count: count });
                }
                else {
                    res.status(200).json({ status: 200, msg: "not found" });
                }
            }).sort( sort ).skip(parseInt(page) * parseInt(limit)).limit(limit);
    
        }).sort( sort ).skip(parseInt(page) * parseInt(limit)).limit(limit);
        */
    });
});

router.get('/api/rosters/generate-free-agents', function (req, res) {
    utils.validate(req, function (isValid, user) {


        FreeAgents.count({ LeagueId: req.query.lid }, function (err, count) {
            if (count > 0) {

            } else {
                FreeAgents.find({}, function (err, players) {
                    if (err) {
                        res.status(200).json({ status: 200, msg: err });
                    }
                    else if (players) {
                        var newPlayers = [];

                        for (i = 0; i < players.length; i++) {
                            if (!players[i].LeagueId || (players[i].LeagueId && players[i].LeagueId == "mlb")) {

                                var fa = players[i]._doc;
                                if (fa.LastName != "") {
                                    fa._id = null;
                                    fa["LeagueId"] = req.query.lid;
                                    newPlayers.push(fa);
                                }
                            }
                        }

                        res.status(200).json({ status: 200, count: "Generated " + newPlayers.length + " free agents in list" });

                        // try to insert them into the db
                        if (newPlayers.length > 0) {
                            FreeAgents.insertMany(newPlayers, function (err) {
                                var msg = err;
                            });
                        }
                    }
                    else {
                        res.status(401).json({ status: 401, msg: "not found" });
                    }
                }).sort({ LastName: "asc" });
            }
        });
    });
});

router.post('/api/rosters/freeagent/delete', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            League.findOne({ _id: req.body.lid }, { FreeAgents: 1 }, function (err, league) {
                if (err) {

                } else {
                    var faid = req.body.player.PlayerId;
                    var pname = req.body.player.FullName;
                    var playerToDelete = -1;
                    for (i = 0; i < league.FreeAgents.length; i++) {
                        if (league.FreeAgents[i].PlayerId == faid) {
                            playerToDelete = i;
                            break;
                        }
                    }

                    if (playerToDelete >= 0) {
                        // found him, delete him!
                        league.FreeAgents.splice(playerToDelete, 1);
                        league.markModified("FreeAgents");
                        league.save(function (err, response) {
                            if (!err) {
                                res.status(200).json({ status: 200, msg: pname + " deleted from league." });
                            } else {
                                res.status(401).json({ status: 401, msg: pname + " found, but was not deleted." });
                            }
                        });
                    } else {
                        res.status(401).json({ status: 401, msg: pname + " not found, not deleted." });
                    }
                }
            });


        }
    });

});

router.post('/api/rosters/update', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.body.lid },
                    { TeamId: req.body.tid }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (roster) {
                    if (req.body.type) {
                        switch (req.body.type) {
                            case "al":
                                roster.FortyManAL = req.body.FortyManAL;
                                roster.markModified("FortyManAL");
                                break;
                            case "nl":
                                roster.FortyManNL = req.body.FortyManNL;
                                roster.markModified("FortyManNL");
                                break;
                            default:
                                break;
                        }
                    }
                    else {
                        roster.FortyManNL = req.body.FortyManNL;
                        roster.markModified("FortyManNL");
                    }

                    if (req.body.TeamAbbr) {
                        roster.TeamAbbr = req.body.TeamAbbr;
                    }

                    roster.save(function (err, response) {
                        if (err) {
                            console.log(err.message);
                        }
                        res.status(200).json({ status: 200, msg: "Update complete" });
                    });
                }
                else {
                    res.status(500).json({ status: 500, msg: "No data", roster: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});

router.post('/api/rosters/create', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.body.lid },
                    { TeamId: req.body.tid }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (roster) {
                    res.status(500).json({ status: 500, msg: "Roster already exists" });
                }
                else {
                    var newRoster = {
                        LeagueId: req.body.lid,
                        TeamId: req.body.tid,
                        CreatedUTC: new Date().toISOString(),
                        TeamAbbr: req.body.TeamAbbr,
                        FortyManNL: req.body.FortyManNL,
                        FortyManAL: req.body.FortyManAL,
                        NonRoster: []
                    };
                    Roster.create(newRoster, function (err, response) {
                        if (err) {
                            console.log(err.message);
                        }
                        res.status(200).json({ status: 500, msg: "Create complete" });
                    });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});

router.post('/api/rosters/reset', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.body.lid },
                    { TeamId: req.body.tid }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (roster) {
                    roster.FortyManAL = [];
                    roster.FortyManNL = [];
                    roster.NonRoster = [];

                    roster.markModified("FortyManAL");
                    roster.markModified("FortyManNL");
                    roster.markModified("NonRoster");
                    /*
                    roster.DL = [];
                    roster.Waivers =[];
                    roster.OtherLists = [];
                    roster.markModified("DL");
                    roster.markModified("Waivers");
                    roster.markModified("OtherLists");
                    */

                    roster.save(function (err, response) {
                        if (err) {
                            console.log(err.message);
                        }
                        res.status(200).json({ status: 500, msg: "Update complete" });
                    });
                }
                else {
                    res.status(500).json({ status: 500, msg: "No data", roster: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});

router.get('/api/rosters/check-sizes', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            League.findOne({ _id: req.query.lid }, function (err, league) {
                if (err) {
                    res.status(500).json({ status: 500, msg: "League not found" });
                } else {
                    Content.findOne({ name: "settings" }, function (err, content) {
                        var settings = [];
                        if (content && content.content) {
                            settings = content.content;
                        }
                        var messageArray = [];
                        rosterHelper.checkRosterSizes(0, league, settings, messageArray, function (err, messages) {
                            if (err) {
                                res.status(500).json({ status: 500, msg: "Error checking rosters" });
                            } else {
                                if (!req.query.action || req.query.action != "sendmessages") {
                                    res.status(200).json({ status: 200, msg: "Rosters checked. There are " + messages.length + " messages.", messages: messages });

                                } else {
                                    // sending messages to interested parties now...
                                    res.status(200).json({ status: 200, msg: "Sending Messages to " + messages.length + " teams.", messages: messages });

                                    var skipAdmins = true;
                                    messageHelper.alertSetOfInterestedParties(0, user, messages, "Roster Overages", skipAdmins, function (response) {
                                        // done.. service will stop now..
                                        return;
                                    })
                                }
                            }
                        });
                    });
                }
            })
        } else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    })
});

router.post('/api/rosters/batting-order/update', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            /*    var query = {
                    $and: [
                        { LeagueId: req.body.lid },
                        { TeamId: req.body.tid }
                    ]
                };
                */
            var query = { LeagueId: req.body.lid, TeamId: req.body.tid };
            Roster.findOne(query, function (err, league) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (league && req.body.lineup) {
                    // need to do weird cleanup on lineup...
                    var battingOrder = req.body.lineup;
                    for (var b = 0; b < battingOrder.length; b++) {
                        if (battingOrder[b] && battingOrder[b].$isMongooseArray) {
                            battingOrder[b] = null;
                        }
                    }
                    if (!req.body.battingType) {
                        req.body.battingType = "9";
                    }

                    if (req.body.battingType == "9") {
                        league.BattingOrderNL = battingOrder;
                        league.markModified("BattingOrderNL");
                    }
                    else {
                        // "DH"
                        league.BattingOrderAL = battingOrder;
                        league.markModified("BattingOrderAL");
                    }
                    if (req.body.bench) {
                        league.Bench = req.body.bench;
                        league.markModified("Bench");
                    }

                    league.save(function (err, result) {
                        res.status(200).json({ status: 200, msg: "success" });

                        // save the lineup to the roster lineup too!
                        RosterLineup.findOne(query, function (err, rosterLineup) {
                            if (req.body.battingType == "9") {
                                rosterLineup.BattingOrderNL = battingOrder;
                                rosterLineup.markModified("BattingOrderNL");
                            }
                            else {
                                // "DH"
                                rosterLineup.BattingOrderAL = battingOrder;
                                rosterLineup.markModified("BattingOrderAL");
                            }
                            if (req.body.bench) {
                                rosterLineup.Bench = req.body.bench;
                                rosterLineup.markModified("Bench");
                            }
                            rosterLineup.save(function (err, result) {
                                if (err) {
                                    console.log("error writing lineup to rosterLineup collection");
                                }
                            })
                        })
                    });
                }
                else {
                    res.status(500).json({ status: 500, msg: "League not found" });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});

// actually gets the lineup AND the bench.
router.get('/api/rosters/batting-order/bench/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            RosterLineup.findOne(query, function (err, rosterLineup) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (rosterLineup) {
                    var output = [];
                    // players is the entire list
                    // DepthChartNL is the depth chart that this is working through...
                    if (rosterLineup._doc.DepthChartNL) {
                        var dc = rosterLineup._doc.DepthChartNL;
                        for (var i = 0; i < dc.length; i++) {
                            output = output.concat(dc[i].Players);
                        }
                    }
                    if (rosterLineup.BattingOrderNL) {
                        output = rosterLineup.BattingOrderNL;
                    }
                    startingPitchers = [];
                    if (rosterLineup.DepthChartNL && rosterLineup.DepthChartNL[8])
                        startingPitchers = rosterLineup.DepthChartNL[8].Players;

                    var bench = rosterLineup.Players;
                    for (var b = bench.length - 1; b >= 0; b--) {
                        if (!bench[b] || bench[b].Status != 1) {
                            bench.splice(b, 1);
                        }
                    }


                    res.status(200).json({ status: 200, msg: "success", lineup: output, roster: bench, startingPitchers: startingPitchers });
                }
                else {
                    res.status(500).json({ status: 500, msg: "League not found" });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});

// called from www to get the batting order for the user.
router.get('/api/rosters/batting-order/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            Roster.findOne(query, function (err, league) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (league) {

                    // read in the roster lineup to get the active players, etc.
                    RosterLineup.findOne(query, function (err, rosterLineup) {
                        if (!rosterLineup) {
                            res.status(500).json({ status: 500, msg: "Roster Lineup not found" });
                        } else {
                            // put the right data into the batting order... save it in both places after the return to use

                            // this updates depthchart to only include roster players
                         //   rosterLineupHelper.makeLegalDepthChart(league.FortyManNL, league.NonRoster, league.DepthChartNL, rosterLineup);
                            rosterHelper.putInDisplayStats(rosterLineup.Players);

                            if (league.BattingOrderNL) {
                                for (var batter in league.BattingOrderNL) {
                                    if (league.BattingOrderNL[batter]) {
                                        var foundActive = false;
                                        for (var player = 0; player < rosterLineup.Players.length; player++) {
                                            if (rosterLineup.Players[player] && rosterLineup.Players[player].PlayerId == league.BattingOrderNL[batter].PlayerId) {
                                                var pos = league.BattingOrderNL[batter].Position;
                                                league.BattingOrderNL[batter] = rosterLineup.Players[player];
                                                league.BattingOrderNL[batter].Position = pos;
                                                foundActive = true;
                                                break;
                                            }
                                        }
                                        if( !foundActive ) {
                                            league.BattingOrderNL[batter] = null;
                                        }
                                    }
                                }
                                if (league.BattingOrderNL.length < 9) {
                                    league.BattingOrderNL.push(["P"]);
                                }
                            }

                            if (league.BattingOrderAL) {
                                for (var batter in league.BattingOrderAL) {
                                    if (league.BattingOrderAL[batter] && league.BattingOrderAL[batter].PlayerId) {
                                        var foundActive = false;
                                        for (var player = 0; player < rosterLineup.Players.length; player++) {
                                            if (rosterLineup.Players[player].PlayerId == league.BattingOrderAL[batter].PlayerId) {


                                                // need to make a deep clone here...
                                                // but only if it's a valid player...
                                                if (rosterLineup.Players[player].PlayerId) {
                                                    var pos = league.BattingOrderAL[batter].Position;
                                                    var alPlayer = JSON.parse(JSON.stringify(rosterLineup.Players[player]));
                                                    league.BattingOrderAL[batter] = alPlayer;
                                                    league.BattingOrderAL[batter].Position = pos;
                                                    foundActive = true;
                                                } else {
                                                    league.BattingOrderAL[batter] = null;
                                                }
                                                break;
                                            }
                                        }
                                        if( !foundActive ) {
                                            league.BattingOrderAL[batter] = null;
                                        }
                                    } else {
                                        league.BattingOrderAL[batter] = null;
                                    }
                                }
                                if (league.BattingOrderAL.length < 9) {
                                    league.BattingOrderAL.push(["DH"]);
                                }
                            } else {
                                league.BattingOrderAL = league.BattingOrderNL;
                                // but remove the pitcher
                                for (var batter in league.BattingOrderAL) {
                                    if (league.BattingOrderAL[batter] && league.BattingOrderAL[batter].Position == "P") {
                                        league.BattingOrderAL[batter] = ["DH"];
                                    }
                                }
                            }
                            if (!req.query.battingType) {
                                req.query.battingType = "9";
                            }

                            var bench = [];
                            var missingPlayers = [];
                            if (rosterLineup.Bench) {

                                if (rosterLineup.Bench.length == 0) {
                                    rosterLineup.Bench = rosterLineup.Players;
                                }
                                for (var player = 0; player < rosterLineup.Players.length; player++) {
                                    if (rosterLineup.Players[player]) {
                                        var found = false;
                                        for (var s = 0; s < rosterLineup.Bench.length; s++) {
                                            if (rosterLineup.Players[player].PlayerId == rosterLineup.Bench[s].PlayerId
                                                && rosterLineup.Players[player].Status == 1) {
                                                bench.push(rosterLineup.Players[player]);
                                                found = true;
                                                break;
                                            }
                                        }
                                        if (!found) {
                                            // push all this guy on to the bench.
                                            if (rosterLineup.Players[player].PlayerId)
                                                bench.push(rosterLineup.Players[player]);
                                        }
                                    }
                                }
                                /*
                                for (var s = 0; s < rosterLineup.Bench.length; s++) {
                                    for (var player in rosterLineup.Players) {
                                        if (rosterLineup.Players[player].PlayerId == rosterLineup.Bench[s].PlayerId
                                           && rosterLineup.Players[player].Status == 1) {
                                            bench.push(rosterLineup.Players[player]);
                                            break;
                                        }
                                    }
                                }
                                */
                            }
                            // finally grab the starting pitchers.
                            var startingPitchers = [];
                            if (rosterLineup.DepthChartNL && rosterLineup.DepthChartNL[8]) {
                                startingPitchers = rosterLineup.DepthChartNL[8].Players;
                                for (var s = 0; s < startingPitchers.length; s++) {
                                    for (var player = 0; player < rosterLineup.Players.length; player++) {
                                        if (rosterLineup.Players[player].PlayerId == startingPitchers[s].PlayerId) {
                                            startingPitchers[s] = rosterLineup.Players[player];
                                            break;
                                        }
                                    }
                                }
                            }
                            res.status(200).json({ status: 200, msg: "success", lineup: req.query.battingType == "9" ? (league.BattingOrderNL ? league.BattingOrderNL : []) : (league.BattingOrderAL ? league.BattingOrderAL : []), startingPitchers: startingPitchers, bench: bench });

                            // now save everything back to the db
                            rosterLineup.BattingOrderAL = league.BattingOrderAL;
                            rosterLineup.BattingOrderNL = league.BattingOrderNL;
                            rosterLineup.markModified("BattingOrderAL");
                            rosterLineup.markModified("BattingOrderNL");
                            rosterLineup.markModified("Players");
                            rosterLineup.markModified("InactivePlayers");
                            rosterLineup.markModified("NonRosterPlayers");
                            rosterLineup.save(function (err, resonse) {

                                league.markModified("BattingOrderAL");
                                league.markModified("BattingOrderNL");
                                league.markModified("DepthChartNL");                                
                                league.save(function (err, resonse) {

                                })

                            })

                        }
                    })
                }
                else {
                    res.status(500).json({ status: 500, msg: "League not found" });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "Unauthorized" });
        }
    });
});



module.exports = router;