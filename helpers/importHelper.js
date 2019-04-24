
var request = require('request');
var cheerio = require('cheerio');
var moment = require('moment');
var mongoose = require('mongoose');
var MasterPlayer = mongoose.model('MasterPlayer');
var TempMasterPlayers = mongoose.model('TempMasterPlayers');
var Content = mongoose.model("Content");
var League = mongoose.model("League");
var rosterHelper = require('../helpers/rosterHelper');
var playerHelper = require('../helpers/playerHelper');
var ftp = require("../helpers/ftp");

var Pitching = mongoose.model('Pitching');
var Baserunning = mongoose.model('Baserunning');
var Fielding = mongoose.model('Fielding');
var Batting = mongoose.model('Batting');
var YtdFielding = mongoose.model('YtdFielding');
var YtdStats = mongoose.model('YtdStats');

var PitchingMinors = mongoose.model('PitchingMinors');
var BaserunningMinors = mongoose.model('BaserunningMinors');
var FieldingMinors = mongoose.model('FieldingMinors');
var BattingMinors = mongoose.model('BattingMinors');
var YtdFieldingMinors = mongoose.model('YtdFieldingMinors');

var PlayerBatting = mongoose.model('PlayerBatting');
var PlayerBattingMinors = mongoose.model('PlayerBattingMinors');
var PlayerBaserunning = mongoose.model('PlayerBaserunning');
var PlayerBaserunningMinors = mongoose.model('PlayerBaserunningMinors');
var PlayerFielding = mongoose.model('PlayerFielding');
var PlayerFieldingMinors = mongoose.model('PlayerFieldingMinors');
var PlayerPitching = mongoose.model('PlayerPitching');
var PlayerPitchingMinors = mongoose.model('PlayerPitchingMinors');

var Roster = mongoose.model('Roster');
var RosterLineup = mongoose.model('RosterLineup');
var playerStatus = require('../helpers/playerStatus');

var utils = require('../helpers/utils');
var moment = require('moment-timezone');

module.exports = {
    addMasterPlayers: function (index, data, newPlayers, callback) {
        /*
        var context = this;

        if (index < data.length) {
            
            var player = data[index];


            player.Image = player.MlbId ? "http://mlb.mlb.com/mlb/images/players/head_shot/" + player.MlbId + ".jpg" : "//rsportsbaseball.com/assets/img/avatars/avatar1.jpg";
            if( player.NEW && player.NEW != '') {
                newPlayers.push( player );
            }

            MasterPlayer.create(player, function (err, result) {
                // add in new _id for this player
                data[index]._id = result._doc._id.toString();
                context.addMasterPlayers(++index, data, newPlayers, callback);
            });
        }
        else {
            callback(data);
        }
        */
        var nextFA = newPlayers.length;
        for (i = 0; i < data.length; i++) {
            data[i].Image = data[i].MlbId ? "http://mlb.mlb.com/mlb/images/players/head_shot/" + data[i].MlbId + ".jpg" : "//rsportsbaseball.com/assets/img/avatars/avatar1.jpg";
            if (data[i].NEW && data[i].NEW != '') {
                newPlayers.push(data[i]);
            }
        }
        MasterPlayer.insertMany(data, function (err, mplayers) {
            // add in new _id for this player
            if (!err) {
                for (i = nextFA; i < newPlayers.length; i++) {
                    var freeAgentName = newPlayers[i].FullName;
                    var freeAgentTeam = newPlayers[i].MlbTeam;

                    // now see if this guy matches...
                    for (p = 0; p < mplayers.length; p++) {
                        if (mplayers[p].FullName == freeAgentName && mplayers[p].MlbTeam == freeAgentTeam) {
                            newPlayers[i]["_id"] = mplayers[p]._doc._id;
                            break;
                        }
                    }
                }

            }
            callback(data);
        });
    },

    newPlayers: [],

    insertNewFreeAgentsIntoLeague(index, leagues, freeagents, callback) {
        var context = this;
        if (index >= leagues.length) {
            callback(null, "success");
        } else {

            for (n = 0; n < freeagents.length; n++) {
                leagues[index]._doc.FreeAgents.unshift(freeagents[n]);
                leagues[index].markModified("FreeAgents");
            }

            leagues[index].save(function (err, response) {
                if (err) {
                    callback(err, "Error creating new free agents")
                } else {
                    context.insertNewFreeAgentsIntoLeague(++index, leagues, freeagents, callback);
                }

            })
        }
    },

    addNewPlayersToFreeAgents: function (players, callback) {
        // now have array of players who need to be added to free agent pool
        var context = this;

        var newPlayerMsg = "No new players found.";
        if (players.length == 0) {
            callback(null, newPlayerMsg);
        }

        // if there are new players... add in their contractExtras, put their salaries in, 
        // and add them to the current leagues' free agent pools
        newPlayerMsg = players.length + " new player(s) found";

        // for the new players, add in their contractInfo by MLBId;
        Content.findOne({ name: "contractInfo" }, function (err, contractInfo) {
            var contractExtras = {};
            if (!err) {
                var dataset = contractInfo.content.dataset;
                for (i = 0; i < dataset.length; i++) {
                    mid = dataset[i].MlbId;
                    if (contractExtras[mid]) {

                    } else {
                        contractExtras[mid] = [];
                    }
                    contractExtras[mid].push(dataset[i]);
                }


                // for each new player, build their player data including salaries and contractExtras
                newFreeAgents = [];
                for (i = 0; i < players.length; i++) {
                    newFreeAgents[i] = rosterHelper.buildOneFreeAgent(players[i], contractExtras);
                }

                // ok, add the newFreeAgents to each league
                League.find({}, { FreeAgents: 1 }, function (err, leagues) {
                    if (!err) {
                        context.insertNewFreeAgentsIntoLeague(0, leagues, newFreeAgents, function (err, message) {
                            if (err) {
                                callback(err, "couldn't insert free agents");
                            } else {
                                callback(err, "inserted new free agents");
                            }
                        })


                    } else {
                        callback(err, "couldn't open leagues")
                    }
                });
            }
        });
    },

    addMasterPlayersFromTempPlayers: function (start, end, callback) {
        var context = this;
        if (start == 0) {
            context.newPlayers = [];    // clear it out.
        }
        if (start >= end) {
            context.addNewPlayersToFreeAgents(context.newPlayers, function (err, message) {
                callback(err, "success. " + message);
            });
        } else {
            var sliceSize = 250;
            TempMasterPlayers.find({}, { MasterPlayers: { $slice: [start, sliceSize] } }, function (err, mplayers) {
                if (err) {
                    callback("err", "couldn't read from: " + start)
                } else {
                    // will process
                    if (mplayers[0] && mplayers[0].MasterPlayers) {
                        context.addMasterPlayers(0, mplayers[0].MasterPlayers, context.newPlayers, function (updatedPlayers) {
                            console.log("Players " + start + " to " + (start + sliceSize));
                            context.addMasterPlayersFromTempPlayers(start + sliceSize, end, callback);
                        });
                    } else {
                        callback("err", "error with reading temp master players");
                    }

                }

            });
        }
    },

    addYtdBlockFrameCEra: function (index, data, callback) {
        var context = this;
        if (index < data.length) {
            console.log(index + " - " + data[index].PlayerName);
            var query = {
                $and: [
                    { Name: data[index].PlayerName },
                    { Team: data[index].Team }
                ]
            };
            YtdStats.find(query, function (err, stats) {
                if (err) {
                    context.addYtdBlockFrameCEra(++index, data, callback);
                }
                else if (stats && stats.length > 0) {
                    // already exists, check for baserunning
                    if (stats.length > 1) {
                        console.log(stats);
                        context.addYtdBlockFrameCEra(++index, data, callback);
                    }
                    else {
                        stats[0].PlayerId = data[index].PlayerId;
                        stats[0].MlbId = data[index].MLBId;
                        stats[0].BlockFrameCEra = data[index];
                        stats[0].markModified("BlockFrameCEra");
                        stats[0].save(function (err, result) {
                            context.addYtdBlockFrameCEra(++index, data, callback);
                        });
                    }
                }
                else {
                    // insert new record
                    var player = {
                        Name: data[index].PlayerName,
                        Team: data[index].Team,
                        PlayerId: data[index].PlayerId,
                        MlbId: data[index].MLBId,
                        BlockFrameCEra: data[index]
                    }
                    YtdStats.create(player, function (err, result) {
                        context.addYtdBlockFrameCEra(++index, data, callback);
                    });
                }
            });
        }
        else {
            callback(data);
        }
    },
    addYtdFieldingIndex: function (index, data, callback) {
        var context = this;
        if (index < data.length) {
            console.log(index + " - " + data[index].Name);
            var query = {
                $and: [
                    { Name: data[index].Name },
                    { Team: data[index].Team }
                ]
            };
            YtdStats.find(query, function (err, stats) {
                if (err) {
                    context.addYtdFieldingIndex(++index, data, callback);
                }
                else if (stats && stats.length > 0) {
                    // already exists, check for baserunning
                    if (stats.length > 1) {
                        console.log(stats);
                        context.addYtdFieldingIndex(++index, data, callback);
                    }
                    else {
                        //stats[0].PlayerId = data[index].playerid;
                        //stats[0].POS = data[index].POS;
                        if (!stats[0].FieldingIndex || !Array.isArray(stats[0].FieldingIndex)) {
                            stats[0].FieldingIndex = [];
                        }
                        stats[0].FieldingIndex.push(data[index]);
                        stats[0].markModified("FieldingIndex");
                        stats[0].save(function (err, result) {
                            context.addYtdFieldingIndex(++index, data, callback);
                        });
                    }
                }
                else {
                    // insert new record
                    var player = {
                        Name: data[index].Name,
                        Team: data[index].Team,
                        //PlayerId: data[index].playerId, 
                        //POS: data[index].POS,
                        FieldingIndex: [data[index]]
                    }
                    YtdStats.create(player, function (err, result) {
                        context.addYtdFieldingIndex(++index, data, callback);
                    });
                }
            });
        }
        else {
            callback(data);
        }
    },
    addYtdZone: function (index, data, callback) {
        var context = this;
        if (index < data.length) {
            console.log(index + " - " + data[index].PlayerName);
            var query = {
                $and: [
                    { Name: data[index].PlayerName },
                    { Team: data[index].Team }
                ]
            };
            YtdStats.find(query, function (err, stats) {
                if (err) {
                    context.addYtdZone(++index, data, callback);
                }
                else if (stats && stats.length > 0) {
                    // already exists, check for baserunning
                    if (stats.length > 1) {
                        console.log(stats);
                        context.addYtdZone(++index, data, callback);
                    }
                    else {
                        stats[0].Name = data[index].PlayerName;
                        stats[0].Zone = data[index];
                        stats[0].markModified("Zone");
                        stats[0].save(function (err, result) {
                            context.addYtdZone(++index, data, callback);
                        });
                    }
                }
                else {
                    // insert new record
                    var player = {
                        Name: data[index].PlayerName,
                        Team: data[index].Team,
                        //PlayerId: data[index].playerId,
                        MlbId: data[index].MLBId,
                        Zone: data[index]
                    }
                    YtdStats.create(player, function (err, result) {
                        context.addYtdZone(++index, data, callback);
                    });
                }
            });
        }
        else {
            callback(data);
        }
    },
    addYtdRValue: function (index, data, callback) {
        var context = this;
        if (index < data.length) {
            console.log(index + " - " + data[index].Name);
            var query = {
                $and: [
                    { Name: data[index].Name },
                    { PlayerId: data[index].playerid }
                ]
            };
            YtdStats.find(query, function (err, stats) {
                if (err) {
                    context.addYtdRValue(++index, data, callback);
                }
                else if (stats && stats.length > 0) {
                    // already exists, check for baserunning
                    if (stats.length > 1) {
                        console.log(stats);
                        context.addYtdRValue(++index, data, callback);
                    }
                    else {
                        stats[0].PlayerId = data[index].playerid;
                        stats[0].RValue = data[index];
                        stats[0].markModified("RValue");
                        stats[0].save(function (err, result) {
                            context.addYtdRValue(++index, data, callback);
                        });
                    }
                }
                else {
                    // insert new record
                    var player = {
                        Name: data[index].Name,
                        Team: data[index].Team,
                        PlayerId: data[index].playerid,
                        RValue: data[index]
                    }
                    YtdStats.create(player, function (err, result) {
                        context.addYtdRValue(++index, data, callback);
                    });
                }
            });
        }
        else {
            callback(data);
        }
    },
    addYtdCatchers: function (index, data, callback) {
        var context = this;
        if (index < data.length) {
            console.log(index + " - " + data[index].Name);
            var query = {
                $and: [
                    { Name: data[index].Name },
                    { Team: data[index].Team }
                ]
            };
            YtdStats.find(query, function (err, stats) {
                if (err) {
                    context.addYtdCatchers(++index, data, callback);
                }
                else if (stats && stats.length > 0) {
                    // already exists, check for baserunning
                    if (stats.length > 1) {
                        console.log(stats);
                        context.addYtdCatchers(++index, data, callback);
                    }
                    else {
                        stats[0].PlayerId = data[index].playerid;
                        stats[0].POS = "C";
                        stats[0].Catcher = data[index];
                        stats[0].markModified("Catcher");
                        stats[0].save(function (err, result) {
                            context.addYtdCatchers(++index, data, callback);
                        });
                    }
                }
                else {
                    // insert new record
                    var player = {
                        Name: data[index].Name,
                        POS: "C",
                        Team: data[index].Team,
                        PlayerId: data[index].playerId,
                        Catcher: data[index]
                    }
                    YtdStats.create(player, function (err, result) {
                        context.addYtdCatchers(++index, data, callback);
                    });
                }
            });
        }
        else {
            callback(data);
        }
    },
    addYtdDefense: function (index, data, callback) {
        var context = this;
        if (index < data.length) {
            console.log(index + " - " + data[index].Name);
            var query = {
                $and: [
                    { Name: data[index].Name },
                    { PlayerId: data[index].playerid }
                ]
            };
            YtdStats.find(query, function (err, stats) {
                if (err) {
                    context.addYtdDefense(++index, data, callback);
                }
                else if (stats && stats.length > 0) {
                    // already exists, check for baserunning
                    if (stats.length > 1) {
                        console.log(stats);
                        context.addYtdDefense(++index, data, callback);
                    }
                    else {
                        stats[0].PlayerId = data[index].playerid;
                        stats[0].Defense = data[index];
                        stats[0].markModified("Defense");
                        stats[0].save(function (err, result) {
                            context.addYtdDefense(++index, data, callback);
                        });
                    }
                }
                else {
                    // insert new record
                    var player = {
                        Name: data[index].Name,
                        Team: data[index].Team,
                        PlayerId: data[index].playerid,
                        POS: data[index].POS,
                        Defense: data[index]
                    }
                    YtdStats.create(player, function (err, result) {
                        context.addYtdDefense(++index, data, callback);
                    });
                }
            });
        }
        else {
            callback(data);
        }
    },
    addYtdPsOverall: function (index, data, callback) {
        var context = this;
        if (index < data.length) {
            console.log(index + " - " + data[index].Name);
            var query = {
                $and: [
                    { Name: data[index].Name },
                    { Team: data[index].Team }
                ]
            };
            YtdStats.find(query, function (err, stats) {
                if (err) {
                    context.addYtdPsOverall(++index, data, callback);
                }
                else if (stats && stats.length > 0) {
                    // already exists, check for baserunning
                    if (stats.length > 1) {
                        console.log(stats);
                        context.addYtdPsOverall(++index, data, callback);
                    }
                    else {
                        stats[0].PlayerId = data[index].playerid;
                        stats[0].POS = "P";
                        stats[0].POverall = data[index];
                        stats[0].markModified("POverall");
                        stats[0].save(function (err, result) {
                            context.addYtdPsOverall(++index, data, callback);
                        });
                    }
                }
                else {
                    // insert new record
                    var player = {
                        Name: data[index].Name,
                        Team: data[index].Team,
                        PlayerId: data[index].playerId,
                        //MlbId: data[index].MlbId,
                        POS: "P",
                        POverall: data[index]
                    }
                    YtdStats.create(player, function (err, result) {
                        context.addYtdPsOverall(++index, data, callback);
                    });
                }
            });
        }
        else {
            callback(data);
        }
    },
    addYtdRPs: function (index, data, callback) {
        var context = this;
        if (index < data.length) {
            console.log(index + " - " + data[index].Name);
            var query = {
                $and: [
                    { Name: data[index].Name },
                    { Team: data[index].Team }
                ]
            };
            YtdStats.find(query, function (err, stats) {
                if (err) {
                    context.addYtdRPs(++index, data, callback);
                }
                else if (stats && stats.length > 0) {
                    // already exists, check for baserunning
                    if (stats.length > 1) {
                        console.log(stats);
                        context.addYtdRPs(++index, data, callback);
                    }
                    else {
                        stats[0].PlayerId = data[index].playerid;
                        stats[0].POS = "P";
                        stats[0].RP = data[index];
                        stats[0].markModified("RP");
                        stats[0].save(function (err, result) {
                            context.addYtdRPs(++index, data, callback);
                        });
                    }
                }
                else {
                    // insert new record
                    var player = {
                        Name: data[index].Name,
                        Team: data[index].Team,
                        PlayerId: data[index].playerId,
                        //MlbId: data[index].MlbId,
                        POS: "P",
                        RP: data[index]
                    }
                    YtdStats.create(player, function (err, result) {
                        context.addYtdRPs(++index, data, callback);
                    });
                }
            });
        }
        else {
            callback(data);
        }
    },

    addYtdSPs: function (index, data, callback) {
        var context = this;
        if (index < data.length) {
            console.log(index + " - " + data[index].Name);
            var query = {
                $and: [
                    { Name: data[index].Name },
                    { Team: data[index].Team }
                ]
            };
            YtdStats.find(query, function (err, stats) {
                if (err) {
                    context.addYtdSPs(++index, data, callback);
                }
                else if (stats && stats.length > 0) {
                    // already exists, check for baserunning
                    if (stats.length > 1) {
                        console.log(stats);
                        context.addYtdSPs(++index, data, callback);
                    }
                    else {
                        stats[0].PlayerId = data[index].playerid;
                        stats[0].SP = data[index];
                        stats[0].POS = "P";
                        stats[0].markModified("SP");
                        stats[0].save(function (err, result) {
                            context.addYtdSPs(++index, data, callback);
                        });
                    }
                }
                else {
                    // insert new record
                    var player = {
                        Name: data[index].Name,
                        Team: data[index].Team,
                        PlayerId: data[index].playerId,
                        POS: "P",
                        //MlbId: data[index].MlbId,
                        SP: data[index]
                    }
                    YtdStats.create(player, function (err, result) {
                        context.addYtdSPs(++index, data, callback);
                    });
                }
            });
        }
        else {
            callback(data);
        }
    },

    addYtdBatters: function (index, data, callback) {
        var context = this;
        if (index < data.length) {
            console.log(index + " - " + data[index].Name);
            var query = {
                $and: [
                    { Name: data[index].Name },
                    { Team: data[index].Team }
                ]
            };
            YtdStats.find(query, function (err, stats) {
                if (err) {
                    context.addYtdBatters(++index, data, callback);
                }
                else if (stats && stats.length > 0) {
                    // already exists, check for baserunning
                    if (stats.length > 1) {
                        console.log(stats);
                        context.addYtdBatters(++index, data, callback);
                    }
                    else {
                        stats[0].PlayerId = data[index].playerid;
                        stats[0].Batter = data[index];
                        stats[0].markModified("Batter");
                        stats[0].save(function (err, result) {
                            context.addYtdBatters(++index, data, callback);
                        });
                    }
                }
                else {
                    // insert new record
                    var player = {
                        Name: data[index].Name,
                        Team: data[index].Team,
                        PlayerId: data[index].playerId,
                        //MlbId: data[index].MlbId,
                        Batter: data[index]
                    }
                    YtdStats.create(player, function (err, result) {
                        context.addYtdBatters(++index, data, callback);
                    });
                }
            });
        }
        else {
            callback(data);
        }
    },

    addYtdBaserunning: function (index, data, callback) {
        var context = this;
        if (index < data.length) {
            console.log(index + " - " + data[index].Name);
            var query = {
                $and: [
                    { Name: data[index].Name },
                    { Team: data[index].Team }
                ]
            };
            YtdStats.find(query, function (err, stats) {
                if (err) {
                    context.addYtdBaserunning(++index, data, callback);
                }
                else if (stats && stats.length > 0) {
                    // already exists, check for baserunning
                    if (stats.length > 1) {
                        console.log(stats);
                        context.addYtdBaserunning(++index, data, callback);
                    }
                    else {
                        stats[0].Baserunning = data[index];
                        stats[0].markModified("Baserunning");
                        stats[0].save(function (err, result) {
                            context.addYtdBaserunning(++index, data, callback);
                        });
                    }
                }
                else {
                    // insert new record
                    var player = {
                        Name: data[index].Name,
                        Team: data[index].Team,
                        //PlayerId: data[index].PlayerId,
                        //MlbId: data[index].MlbId,
                        Baserunning: data[index]
                    }
                    YtdStats.create(player, function (err, result) {
                        context.addYtdBaserunning(++index, data, callback);
                    });
                }
            });
        }
        else {
            callback(data);
        }
    },

    // **************** set availability and eligibility for active players ************************
    setAvailabilityForActivePlayers: function (index, league, keyDates, messages, callback) {

        if (index >= league.Teams.length) {
            callback(messages);
        }
        else {
            var context = this;
            var gameRange = keyDates;
            var season = keyDates.Season;
            var leagueId = league._id;
            var teamId = league.Teams[index]._id;
            var teamName = league.Teams[index].r_name;
            messages[teamName] = "running";
            Roster.findOne({ LeagueId: leagueId, TeamId: teamId }, function (err, roster) {
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

                    //  get active players...
                    playerHelper.getEligiblePositions(0, activePlayerGamesUsed, gameRange, function (playersWithEligibility) {
                        playerHelper.getAvailableBatting(0, activePlayerGamesUsed, "ML", gameRange, function (players) {
                            playerHelper.getAvailablePitching(0, activePlayerGamesUsed, "ML", gameRange, function (players) {
                                playerHelper.getAvailableFielding(0, activePlayerGamesUsed, "ML", gameRange, function (players) {
                                    playerHelper.getAvailableBaserunning(0, activePlayerGamesUsed, "ML", gameRange, function (players) {

                                        // now do minor leagues...
                                        // get active players minor league stats...
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
                                                                CreatedUTC: new Date().toISOString(),
                                                                Bench: roster.Bench ? roster.Bench : null,
                                                                DepthChartNL: roster.DepthChartNL ? roster.DepthChartNL : null,
                                                                BattingOrderAL: roster.BattingOrderAL ? roster.BattingOrderAL : null,
                                                                BattingOrderNL: roster.BattingOrderNL ? roster.BattingOrderNL : null

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
                                                                            messages[teamName] = false;
                                                                            console.log("Eligilibity failed for " + teamName);
                                                                            context.setAvailabilityForActivePlayers(++index, keyDates, messages, callback);

                                                                        } else {
                                                                            if (!dbRoster) {
                                                                                dbRoster = {
                                                                                    Players: []
                                                                                }
                                                                            } else {
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
                                                                                                messages[teamName] = false;
                                                                                            } else {
                                                                                                messages[teamName] = "complete";
                                                                                            }
                                                                                            dbRoster.save(function (err, result) {
                                                                                                if (err)
                                                                                                    console.log("error updating rosterLineup in accumulated stats");
                                                                                                console.log("Eligilibity succedded for " + teamName);
                                                                                                context.setAvailabilityForActivePlayers(++index, league, keyDates, messages, callback);
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
                                                                                    dbRoster._doc.DepthChartNL = roster.DepthChartNL;
                                                                                    dbRoster.markModified("DepthChartNL");

                                                                                    roster.save(function (err, result) {
                                                                                        if (err) {
                                                                                            messages[teamName] = false;
                                                                                        } else {
                                                                                            messages[teamName] = "complete";
                                                                                        }
                                                                                        dbRoster.save(function (err, result) {
                                                                                            if (err)
                                                                                                console.log("error saving rosterLineups in 2nd part of accumulated stats")
                                                                                            console.log("Active Eligilibity succedded for " + teamName);
                                                                                            context.setAvailabilityForActivePlayers(++index, league, keyDates, messages, callback);
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
            })
        }
    },


    // *****************************************
    // set Availability and Eligiblity for Non Roster and Inactive players.. this is SLOW
    setAvailabilityForInactivePlayers: function (index, league, keyDates, messages, callback) {

        if (index >= league.Teams.length) {
            callback(messages);
        }
        else {
            var context = this;
            var gameRange = keyDates;
            var season = keyDates.Season;
            var leagueId = league._id;
            var teamId = league.Teams[index]._id;
            var teamName = league.Teams[index].r_name;
            messages[teamName] = "running";

            Roster.findOne({ LeagueId: leagueId, TeamId: teamId }, function (err, roster) {
                if (err) {
                    callback("error", "couldn't find roster");
                } else {
                    var inactivePlayers = [];
                    var nonRosterPlayers = [];
                    var inactivePlayerGamesUsed = [];
                    var nonRosterPlayerGamesUsed = [];
                    for (i = 0; i < roster.FortyManNL.length; i++) {

                        var newPlayer = rosterHelper.createRosterLineupPlayer(leagueId, roster.FortyManNL[i], season);
                        if (roster.FortyManNL[i].Status == playerStatus.ActiveRoster) {
                        } else {
                            // inactive player
                            inactivePlayerGamesUsed.push(newPlayer);

                        }
                    }
                    for (i = 0; i < roster.NonRoster.length; i++) {
                        var newPlayer = rosterHelper.createRosterLineupPlayer(leagueId, roster.NonRoster[i], season);
                        nonRosterPlayerGamesUsed.push(newPlayer);
                    }

                    // get non-roster players first...

                    playerHelper.getEligiblePositions(0, nonRosterPlayerGamesUsed, gameRange, function (nonRosterPlayers) {
                        playerHelper.getAvailableBatting(0, nonRosterPlayerGamesUsed, "ML", gameRange, function (nonRosterPlayers) {
                            playerHelper.getAvailablePitching(0, nonRosterPlayerGamesUsed, "ML", gameRange, function (nonRosterPlayers) {
                                playerHelper.getAvailableFielding(0, nonRosterPlayerGamesUsed, "ML", gameRange, function (nonRosterPlayers) {
                                    playerHelper.getAvailableBaserunning(0, nonRosterPlayerGamesUsed, "ML", gameRange, function (nonRosterPlayers) {


                                        // now do minor leagues...
                                        // get non-roster players first...
                                        playerHelper.getAvailableBatting(0, nonRosterPlayerGamesUsed, "Minors", gameRange, function (nonRosterPlayers) {
                                            playerHelper.getAvailablePitching(0, nonRosterPlayerGamesUsed, "Minors", gameRange, function (nonRosterPlayers) {
                                                playerHelper.getAvailableFielding(0, nonRosterPlayerGamesUsed, "Minors", gameRange, function (nonRosterPlayers) {
                                                    playerHelper.getAvailableBaserunning(0, nonRosterPlayerGamesUsed, "Minors", gameRange, function (nonRosterPlayers) {

                                                        // now get inactive players...
                                                        playerHelper.getEligiblePositions(0, inactivePlayerGamesUsed, gameRange, function (inactivePlayers) {
                                                            playerHelper.getAvailableBatting(0, inactivePlayerGamesUsed, "ML", gameRange, function (inactivePlayers) {
                                                                playerHelper.getAvailablePitching(0, inactivePlayerGamesUsed, "ML", gameRange, function (inactivePlayers) {
                                                                    playerHelper.getAvailableFielding(0, inactivePlayerGamesUsed, "ML", gameRange, function (inactivePlayers) {
                                                                        playerHelper.getAvailableBaserunning(0, inactivePlayerGamesUsed, "ML", gameRange, function (inactivePlayers) {


                                                                            // now get minors for inactive players...
                                                                            playerHelper.getAvailableBatting(0, inactivePlayerGamesUsed, "Minors", gameRange, function (inactivePlayers) {
                                                                                playerHelper.getAvailablePitching(0, inactivePlayerGamesUsed, "Minors", gameRange, function (inactivePlayers) {
                                                                                    playerHelper.getAvailableFielding(0, inactivePlayerGamesUsed, "Minors", gameRange, function (inactivePlayers) {
                                                                                        playerHelper.getAvailableBaserunning(0, inactivePlayerGamesUsed, "Minors", gameRange, function (inactivePlayers) {

                                                                                            RosterLineup.update(
                                                                                                { LeagueId: leagueId, TeamId: teamId },
                                                                                                {
                                                                                                    LeagueId: leagueId,
                                                                                                    TeamdId: teamId,
                                                                                                    TeamName: teamName,
                                                                                                    InactivePlayers: inactivePlayers,
                                                                                                    NonRosterPlayers: nonRosterPlayers,

                                                                                                },
                                                                                                { upsert: true }, function (err, response) {
                                                                                                    if (err) {
                                                                                                        messages[teamName] = false;
                                                                                                        console.log("error saving roster lineup for inactives in set eligibility");
                                                                                                    } else {
                                                                                                        messages[teamName] = "complete";
                                                                                                    }
                                                                                                    console.log("Non-Roster/Inactive Eligilibity succedded for " + teamName);

                                                                                                    context.setAvailabilityForInactivePlayers(++index, league, keyDates, messages, callback);

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
                                                    });

                                                });
                                            }); // end of getting all the nonroster players - minors
                                        });
                                    });
                                });
                            });
                        });
                    });

                }
            });
        }
    },

    calculatedKeyDates: function () {
        var keyDates = {
            From: 20180329, // starting date of 2018 season
            To: 20180401,   // date BEFORE today's game
            GameDate: 20180401, // date OF today's game
            GameCalendarDay: 91, // calendar day of today's game
            Time: "00:00"      // time right now on 24 hour clock.. just hours and minutes
        }

        var now = moment().tz("America/Los_Angeles");
        keyDates.GameDate = now.format("YYYYMMDD");
        keyDates.GameCalendarDay = now.dayOfYear;
        keyDates.Time = now.format("HH:mm");
        keyDates.To = now.subtract(1, "days").format("YYYYMMDD");
        return (keyDates);
    },

    cronSetEligilibityForActives: function (keyDates, callback) {
        // for 2018 season
        var gameRange = keyDates;
        var context = this;
        var messages = { status: "success" };

        League.find({}, { _id: 1, Name: 1, Settings: 1, "Teams._id": 1, "Teams.r_name": 1, "Teams.r_abbreviation": 1 }, function (err, leagues) {
            if (err || leagues == null || leagues.length == 0) {
                messages.status = "no leagues found";
                callback(false, messages);
            } else {
                var defaultLeague = false;
                for (var l = 0; l < leagues.length; l++) {
                    if (leagues[l].Settings && leagues[l].Settings.Default && leagues[l].Settings.Default === true) {
                        defaultLeague = leagues[l];
                        break;
                    }
                }
                if (!defaultLeague) {
                    messages.status = "no default league found";
                    callback(false, messages);
                } else {
                    // ready to roll with this league!

                    // call the iterator...
                    context.setAvailabilityForActivePlayers(0, defaultLeague, keyDates, messages, function (messageObj) {

                        // check to see if we have a full success
                        var error = false;
                        for (let t = 0; t < defaultLeague.Teams.length; t++) {
                            var nextTeam = defaultLeague.Teams[t].r_name;
                            if (!messageObj[nextTeam] || messageObj[nextTeam] === false) {
                                error = true;
                            }
                        }
                        callback(error, messageObj);
                    })


                }
            }
        })

    },

    cronSetEligilibityForInactives: function (keyDates, callback) {
        // for 2018 season
        var gameRange = keyDates;
        var context = this;
        var messages = { status: "success" };

        League.find({}, { _id: 1, Name: 1, Settings: 1, "Teams._id": 1, "Teams.r_name": 1, "Teams.r_abbreviation": 1 }, function (err, leagues) {
            if (err || leagues == null || leagues.length == 0) {
                messages.status = "no leagues found";
                callback(false, messages);
            } else {
                var defaultLeague = false;
                for (var l = 0; l < leagues.length; l++) {
                    if (leagues[l].Settings && leagues[l].Settings.Default && leagues[l].Settings.Default === true) {
                        defaultLeague = leagues[l];
                        break;
                    }
                }
                if (!defaultLeague) {
                    messages.status = "no default league found";
                    callback(false, messages);
                } else {
                    // ready to roll with this league!

                    // call the iterator...
                    context.setAvailabilityForInactivePlayers(0, defaultLeague, keyDates, messages, function (messageObj) {
                        // check to see if we have a full success
                        var error = false;
                        for (let t = 0; t < defaultLeague.Teams.length; t++) {
                            var nextTeam = defaultLeague.Teams[t].r_name;
                            if (!messageObj[nextTeam] || messageObj[nextTeam] === false) {
                                error = true;
                            }
                        }
                        callback(error, messageObj);
                    })


                }
            }
        })

    },

    // **********************************************************************************     */        
    // **********************************************************************************     */   
    //
    // helpers to import BIS stats and process them
    //
    // ********************************************************************************** */
    // **********************************************************************************     */
    checkDailyGames: function (date, callback) {
        var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate())).slice(-2);

        var message = "For date: " + dateString;
        var isError = false;
        Pitching.findOne({ GameDate: dateString }, function (err, stat) {
            if (err) {
                message += " pitching data NOT found.";
                isError = true;
                console.log(err)
            }
            else if (!stat || stat.Stats.length == 0) {
                message += " pitching data EMPTY. Please report to dev team";
                isError = true;
            } else {
                message += " pitching games found.";
            }
            Batting.findOne({ GameDate: dateString }, function (err, stat) {
                if (err) {
                    message += ". Batting data NOT found.";
                    isError = true;
                    console.log(err)
                }
                else if (!stat || stat.Stats.length == 0) {
                    message += ". Batting data EMPTY. Please report to dev team";
                    isError = true;
                } else {
                    message += ". Batting games found.";
                }
                Baserunning.findOne({ GameDate: dateString }, function (err, stat) {
                    if (err) {
                        message += ". Baserunning data NOT found.";
                        isError = true;
                        console.log(err)
                    }
                    else if (!stat || stat.Stats.length == 0) {
                        message += ". Baserunning data EMPTY. Please report to dev team";
                        isError = true;
                    } else {
                        message += ". Baserunning games found.";
                    }
                    Fielding.findOne({ GameDate: dateString }, function (err, stat) {
                        if (err) {
                            message += " . Fielding data NOT found.";
                            isError = true;
                            console.log(err)
                        }
                        else if (!stat || stat.Stats.length == 0) {
                            message += ". Fielding data EMPTY. Please report to dev team";
                            isError = true;
                        } else {
                            message += ". Fielding games found.";
                        }

                        // and now for the minors...
                        PitchingMinors.findOne({ GameDate: dateString }, function (err, stat) {
                            if (err) {
                                message += " Minor League pitching data NOT found.";
                                isError = true;
                                console.log(err)
                            }
                            else if (!stat || stat.Stats.length == 0) {
                                message += " Minor League pitching data EMPTY. Please report to dev team";
                                isError = true;
                            } else {
                                message += " Minor League pitching games found.";
                            }
                            BattingMinors.findOne({ GameDate: dateString }, function (err, stat) {
                                if (err) {
                                    message += ". Minor League Batting data NOT found.";
                                    isError = true;
                                    console.log(err)
                                }
                                else if (!stat || stat.Stats.length == 0) {
                                    message += ". Minor League Batting data EMPTY. Please report to dev team";
                                    isError = true;
                                } else {
                                    message += ". Minor League Batting games found.";
                                }
                                BaserunningMinors.findOne({ GameDate: dateString }, function (err, stat) {
                                    if (err) {
                                        message += ". Minor League Baserunning data NOT found.";
                                        isError = true;
                                        console.log(err)
                                    }
                                    else if (!stat || stat.Stats.length == 0) {
                                        message += ". Minor League Baserunning data EMPTY. Please report to dev team";
                                        isError = true;
                                    } else {
                                        message += ". Minor League Baserunning games found.";
                                    }
                                    FieldingMinors.findOne({ GameDate: dateString }, function (err, stat) {
                                        if (err) {
                                            message += " . Minor League Fielding data NOT found.";
                                            isError = true;
                                            console.log(err)
                                        }
                                        else if (!stat || stat.Stats.length == 0) {
                                            message += ". Minor League Fielding data EMPTY. Please report to dev team";
                                            isError = true;
                                        } else {
                                            message += ". Minor League Fielding games found.";
                                        }
                                        if (isError) {
                                            message += ". Please try again. "
                                        } else {
                                            message += ". SUCCESS. ALL ACCOUNTED FOR. "
                                        }
                                        callback({ status: 200, msg: message });
                                    });
                                });
                            });
                        });
                    });
                });
            });
        });

    },

    // ****************************************** GET BIS DATA FOR MAJOR LEAGUE GAMES *************************
    getYTDFielding: function (dateString, season, callback) {

        YtdFielding.findOne({ GameDate: dateString }, function (err, stat) {
            if (err) {
                console.log(err)
            }
            else if (!stat) {
                var playerStats = {
                    Season: season,
                    GameDate: dateString,
                    GameId: "",
                    CreatedUTC: new Date().toISOString(),
                    LastModifiedUTC: new Date().toISOString(),
                    Stats: []
                };

                var fixFilename = "YTDFielding_20180329_Fix.csv";
                var fileName = "YTDFielding_" + dateString + ".csv";
                if (dateString == "20180329")
                    fileName = fixFilename;

                ftp.syncFile(fileName, function (data) {
                    if (!data || data.length == 0) {
                        if (callback) {
                            callback(false);
                        }
                        else {
                            callback(false, { status: 201, msg: "YTD Fielding FTP error" });
                        }

                    } else {
                        playerStats.Stats = data;
                        YtdFielding.create(playerStats, function (err, result) {
                            if (err) {
                                console.log(err)
                            }
                            else {
                                console.log(dateString);
                            }
                            // date = new Date(date.setDate(date.getDate() + 1));

                            if (callback) {
                                callback(dateString);
                            }
                            else {
                                callback(dateString, { status: 201, msg: "ytd fielding complete" });
                            }
                        });
                    }
                });

            }
            else {
                // date = new Date(date.setDate(date.getDate() + 1));
                // already in the db
                if (callback) {
                    callback(dateString);
                }
                else {
                    callback(dateString, { status: 201, msg: "ytd fielding already loaded" });
                }
            }
        }).limit(2);
    },

    getPitching: function (dateString, season, callback) {

        Pitching.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
            if (err) {
                console.log(err)
            }
            else if (!stat) {
                var stat = {
                    Season: season,
                    GameDate: dateString,
                    GameId: "",
                    CreatedUTC: new Date().toISOString(),
                    LastModifiedUTC: new Date().toISOString(),
                    Stats: []
                };
                ftp.syncFile("Pitching_" + dateString + ".csv", function (data) {
                    if (!data || data.length == 0) {
                        if (callback) {
                            callback(false)
                        }
                        else {
                            callback(false, { status: 201, msg: "Pitching FTP Error" });
                        }
                    } else {
                        stat.Stats = data;
                        Pitching.create(stat, function (err, result) {
                            if (err) {
                                console.log(err)
                            }
                            else {
                                console.log(dateString);
                            }
                            //  date = new Date(date.setDate(date.getDate() + 1));

                            if (callback) {
                                callback(dateString);
                            }
                            else {
                                callback(dateString, { status: 201, msg: "complete" });
                            }
                        });
                    }
                });

            }
            else {
                // date = new Date(date.setDate(date.getDate() + 1));

                // already in the DB
                if (callback) {
                    callback(dateString);
                }
                else {
                    callback(dateString, { status: 200, msg: "pitching already loaded" });
                }
            }
        });
    },

    getBaserunning: function (dateString, season, callback) {

        Baserunning.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
            if (err) {
                console.log(err)
            }
            else if (!stat) {
                var stat = {
                    Season: season,
                    GameDate: dateString,
                    GameId: "",
                    CreatedUTC: new Date().toISOString(),
                    LastModifiedUTC: new Date().toISOString(),
                    Stats: []
                };
                ftp.syncFile("Baserunning_" + dateString + ".csv", function (data) {
                    if (!data || data.length == 0) {
                        if (callback) {
                            callback(false)
                        }
                        else {
                            callback(false, { status: 201, msg: "Baserunning FTP Error" });
                        }
                    } else {
                        stat.Stats = data;
                        Baserunning.create(stat, function (err, result) {
                            if (err) {
                                console.log(err)
                            }
                            else {
                                console.log(dateString);
                            }
                            // date = new Date(date.setDate(date.getDate() + 1));

                            if (callback) {
                                callback(dateString);
                            }
                            else {
                                callback(dateString, { status: 200, msg: "completed last step" });
                            }
                        });
                    }
                });

            }
            else {
                // already in the db
                if (callback) {
                    callback(dateString);
                }
                else {
                    callback(dateString, { status: 200, msg: "complete last step" });
                }
            }
        });
    },

    getBatting: function (dateString, season, callback) {

        Batting.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
            if (err) {
                console.log(err)
            }
            else if (!stat) {
                var stat = {
                    Season: season,
                    GameDate: dateString,
                    GameId: "",
                    CreatedUTC: new Date().toISOString(),
                    LastModifiedUTC: new Date().toISOString(),
                    Stats: []
                };
                ftp.syncFile("Batting_" + dateString + ".csv", function (data) {
                    if (!data || data.length == 0) {
                        if (callback) {
                            callback(false);
                        }
                        else {
                            callback(false, { status: 201, msg: "Batting FTP Error" });
                        }
                    } else {
                        stat.Stats = data;
                        Batting.create(stat, function (err, result) {
                            if (err) {
                                console.log(err)
                                callback(false);
                            }
                            else {
                                console.log(dateString);
                            }
                            // date = new Date(date.setDate(date.getDate() + 1));

                            if (callback) {
                                callback(dateString)
                            }
                            else {
                                callback({ status: 200, msg: "complete" });
                            }
                        });
                    }

                });

            } else {
                // date = new Date(date.setDate(date.getDate() + 1));
                // already has data
                if (callback) {
                    callback(dateString);
                }
                else {
                    callback({ status: 200, msg: "batting complete" });
                }
            }
        });
    },

    putZoneDataInFielding: function (dateString, playerStats, callback) {
        // get two most recent games for this player and create 
        // Outs Out of Zone and Missed Balls in Zone for the fielding data.
        // get the two most recent  documents...

        YtdFielding.find({GameDate: {$lte: dateString}}, function (err, fStats) {
            if (err || !fStats || fStats.length == 0 || !playerStats.Stats) {
                callback();
            } else {
                for (let p = 0; p < playerStats.Stats.length; p++) {
                    var nextPlayer = playerStats.Stats[p];
                    var oldStats = null;
                    var newStats = null;

                    // 0th is most recent
                    for (f0 = 0; f0 < fStats[0].Stats.length; f0++) {
                        // match the player!
                        if (fStats[0].Stats[f0].MLBId == nextPlayer.MLBId) {
                            var newStats = fStats[0].Stats[f0];
                            break;
                        }
                    }
                    if (fStats.length == 2) {
                        for (f1 = 0; f1 < fStats[1].Stats.length; f1++) {
                            // match the player!
                            if (fStats[1].Stats[f1].MLBId == nextPlayer.MLBId) {
                                var oldStats = fStats[1].Stats[f1];
                                break;
                            }
                        }
                    }
                    var ballsInZone = 0;
                    var outsOutOfZone = 0;
                    var missedBallsInZone = 0;
                    var cFraming = 0;
                    var cBlocking = 0;
                    if (nextPlayer.LastName == "Molina") {
                        var molina = true;
                    }
                    if (oldStats) {
                        ballsInZone = oldStats.BallsInZone;
                        outsOutOfZone = oldStats.OutsOutOfZone;
                        missedBallsInZone = oldStats.MissedBallsInZone;
                        cFraming = oldStats.CFramingRuns != null ? oldStats.CFramingRuns : 0;
                        cBlocking = oldStats.CBlockingRuns != null ? oldStats.CBlockingRuns : 0;


                        if (newStats) {
                            // have both, subtract them.
                            /*
                            ballsInZone = newStats.BallsInZone - oldStats.BallsInZone;
                            outsOutOfZone = newStats.OutsOutOfZone - oldStats.OutsOutOfZone;
                            missedBallsInZone = newStats.MissedBallsInZone - oldStats.MissedBallsInZone;
                            //  cFraming = newStats.cFramingRuns != null ? newStats.cFramingRuns - cFraming : cFraming;  
                            //  cBlocking = newStats.cBlockingRuns != null ? newStats.CBlockingRuns - cBlocking : cBlocking;
                            cFraming = newStats.CFramingRuns != null ? newStats.CFramingRuns : cFraming;
                            //  cBlocking = newStats.cBlockingRuns != null ? newStats.CBlockingRuns - cBlocking : cBlocking;;  
                            cBlocking = newStats.CBlockingRuns != null ? newStats.CBlockingRuns : cBlocking;
                            */
                            // have both, subtract them.
                            ballsInZone = newStats.BallsInZone - oldStats.BallsInZone;
                            if (ballsInZone < 0) {
                                ballsInZone = newStats.BallsInZone;
                            }

                            outsOutOfZone = newStats.OutsOutOfZone - oldStats.OutsOutOfZone;
                            if (outsOutOfZone < 0) {
                                if( newStats.Pos != oldStats.Pos) {
                                    outsOutOfZone = newStats.OutsOutOfZone;
                                    if( outsOutOfZone > 5)
                                        outsOutOfZone = 5;
                                } else {
                                    // indicates scorer changed their mind.
                                    outsOutOfZone = 0;
                                }
                            }

                            missedBallsInZone = newStats.MissedBallsInZone - oldStats.MissedBallsInZone
                            if (missedBallsInZone < 0)
                                missedBallsInZone = newStats.MissedBallsInZone;
                            //  cFraming = newStats.cFramingRuns != null ? newStats.cFramingRuns - cFraming : cFraming;  
                            //  cBlocking = newStats.cBlockingRuns != null ? newStats.CBlockingRuns - cBlocking : cBlocking;
                            cFraming = newStats.CFramingRuns != null ? newStats.CFramingRuns : cFraming;
                            //  cBlocking = newStats.cBlockingRuns != null ? newStats.CBlockingRuns - cBlocking : cBlocking;;  
                            cBlocking = newStats.CBlockingRuns != null ? newStats.CBlockingRuns : cBlocking;

                        }
                    } else {
                        if (newStats) {
                            // if only new stats, use them.
                            ballsInZone = newStats.BallsInZone;
                            outsOutOfZone = newStats.OutsOutOfZone;
                            missedBallsInZone = newStats.MissedBallsInZone;
                            cFraming = newStats.CFramingRuns != null ? newStats.CFramingRuns : cFraming;
                            cBlocking = newStats.CBlockingRuns != null ? newStats.CBlockingRuns : cBlocking;
                        }
                    }
                    playerStats.Stats[p].BallsInZone = ballsInZone;
                    playerStats.Stats[p].OutsOutOfZone = outsOutOfZone;
                    playerStats.Stats[p].MissedBallsInZone = missedBallsInZone;
                    playerStats.Stats[p].cBlockingRuns = cBlocking;
                    playerStats.Stats[p].cFramingRuns = cFraming;
                }
                callback();
            }
        }).sort({ "GameDate": -1 }).limit(2);

    },

    getFielding: function (dateString, season, callback) {
        var context = this;

        Fielding.findOne({ GameDate: dateString }, function (err, playerStats) {
            if (!playerStats) {
                var playerStats = {
                    Season: season,
                    GameDate: dateString,
                    GameId: "",
                    CreatedUTC: new Date().toISOString(),
                    LastModifiedUTC: new Date().toISOString(),
                    Stats: []
                };
                ftp.syncFile("Fielding_" + dateString + ".csv", function (data) {
                    if (!data || data.length == 0) {
                        if (callback) {
                            callback(false)
                        }
                        else {
                            callback(false, { status: 200, msg: "Fielding FTP error" });
                        }
                    } else {
                        playerStats.Stats = data;


                        Fielding.create(playerStats, function (err, newPlayerStats) {
                            if (err) {
                                console.log(err)
                            }
                            else {
                                console.log(dateString);
                            }
                            //  date = new Date(date.setDate(date.getDate() + 1));

                            context.putZoneDataInFielding(dateString, newPlayerStats, function () {

                                newPlayerStats.markModified("Stats");
                                newPlayerStats.save(function (err, result) {
                                    if (callback) {
                                        callback(dateString);
                                    }
                                    else {
                                        callback(dateString, { status: 200, msg: "complete" });
                                    }
                                })
                            });
                        });
                    }
                });

            }
            else {

                context.putZoneDataInFielding(dateString, playerStats, function () {
                    playerStats.markModified("Stats");
                    playerStats.save(function (err, result) {
                        if (callback) {
                            callback(dateString)
                        }
                        else {
                            callback(dateString, { status: 200, msg: "fielding complete" });
                        }
                    });
                });
            }
        });
    },

    //
    // ****************************************** GET BIS DATA FOR MINOR LEAGUE GAMES *************************
    //
    getYTDFieldingMinors: function (dateString, season, callback) {

        YtdFieldingMinors.findOne({GameDate:  dateString}, function (err, stat) {
            if (err) {
                console.log(err)
            }
            else if (!stat) {
                var playerStats = {
                    Season: season,
                    GameDate: dateString,
                    GameId: "",
                    CreatedUTC: new Date().toISOString(),
                    LastModifiedUTC: new Date().toISOString(),
                    Stats: []
                };
                ftp.syncFile("MI_YTDFielding_" + dateString + ".csv", function (data) {
                    if (!data || data.length == 0) {
                        if (callback) {
                            callback(false)
                        }
                        else {
                            callback(false, { status: 200, msg: "YTD Fielding Minors FTP error" });
                        }

                    } else {
                        playerStats.Stats = data;
                        YtdFieldingMinors.create(playerStats, function (err, result) {
                            if (err) {
                                console.log(err)
                            }
                            else {
                                console.log(dateString);
                            }
                            // date = new Date(date.setDate(date.getDate() + 1));

                            if (callback) {
                                callback(dateString);
                            }
                            else {
                                callback(dateString, { status: 200, msg: "ytd fielding minors complete" });
                            }
                        });
                    }
                });

            }
            else {
                // already in db
                if (callback) {
                    callback(dateString);
                }
                else {
                    callback(dateString, { status: 200, msg: "ytd fielding already loaded" });
                }
            }
        }).limit(2);
    },

    getPitchingMinors: function (dateString, season, callback) {

        PitchingMinors.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
            if (err) {
                console.log(err)
            }
            else if (!stat) {
                var stat = {
                    Season: season,
                    GameDate: dateString,
                    GameId: "",
                    CreatedUTC: new Date().toISOString(),
                    LastModifiedUTC: new Date().toISOString(),
                    Stats: []
                };
                ftp.syncFile("MI_Pitching_" + dateString + ".csv", function (data) {
                    if (!data || data.length == 0) {
                        if (callback) {
                            callback(false)
                        }
                        else {
                            callback(false, { status: 200, msg: "Pitching Minors FTP Error" });
                        }
                    } else {
                        stat.Stats = data;
                        PitchingMinors.create(stat, function (err, result) {
                            if (err) {
                                console.log(err)
                            }
                            else {
                                console.log(dateString);
                            }
                            //   date = new Date(date.setDate(date.getDate() + 1));

                            if (callback) {
                                callback(dateString);
                            }
                            else {
                                callback(dateString, { status: 200, msg: "complete" });
                            }
                        });
                    }
                });

            }
            else {
                // already in db

                if (callback) {
                    callback(dateString);
                }
                else {
                    callback(dateString, { status: 200, msg: "pitching already loaded" });
                }
            }
        });
    },

    getBaserunningMinors: function (dateString, season, callback) {

        BaserunningMinors.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
            if (err) {
                console.log(err)
            }
            else if (!stat) {
                var stat = {
                    Season: season,
                    GameDate: dateString,
                    GameId: "",
                    CreatedUTC: new Date().toISOString(),
                    LastModifiedUTC: new Date().toISOString(),
                    Stats: []
                };
                ftp.syncFile("MI_Baserunning_" + dateString + ".csv", function (data) {
                    if (!data || data.length == 0) {
                        if (callback) {
                            callback(false);
                        }
                        else {
                            callback(false, { status: 200, msg: "Baserunning Minors FTP Error" });
                        }
                    } else {
                        stat.Stats = data;
                        BaserunningMinors.create(stat, function (err, result) {
                            if (err) {
                                console.log(err)
                            }
                            else {
                                console.log(dateString);
                            }
                            //   date = new Date(date.setDate(date.getDate() + 1));

                            if (callback) {
                                callback(dateString);
                            }
                            else {
                                callback(dateString, { status: 200, msg: "complete" });
                            }
                        });
                    }
                });

            }
            else {
                // already in db

                if (callback) {
                    callback(dateString);
                }
                else {
                    callback(dateString, { status: 200, msg: "all complete" });
                }
            }
        });
    },

    getBattingMinors: function (dateString, season, callback) {

        BattingMinors.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
            if (err) {
                console.log(err)
            }
            else if (!stat) {
                var stat = {
                    Season: season,
                    GameDate: dateString,
                    GameId: "",
                    CreatedUTC: new Date().toISOString(),
                    LastModifiedUTC: new Date().toISOString(),
                    Stats: []
                };
                ftp.syncFile("MI_Batting_" + dateString + ".csv", function (data) {
                    if (!data || data.length == 0) {
                        if (callback) {
                            callback(false);
                        }
                        else {
                            callback(false, { status: 200, msg: "Batting Minors FTP Error" });
                        }
                    } else {
                        stat.Stats = data;
                        BattingMinors.create(stat, function (err, result) {
                            if (err) {
                                console.log(err)
                            }
                            else {
                                console.log(dateString);
                            }

                            if (callback) {
                                callback(dateString);
                            }
                            else {
                                res.status(200).json({ status: 200, msg: "complete" });
                            }
                        });
                    }

                });

            }
            else {
                //     date = new Date(date.setDate(date.getDate() + 1));

                if (callback) {
                    callback(dateString)
                }
                else {
                    callback(dateString, { status: 200, msg: "batting complete" });
                }
            }
        });
    },


    getFieldingMinors: function (dateString, season, callback) {

        var context = this;

        FieldingMinors.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, playerStats) {
            if (!playerStats) {
                var stat = {
                    Season: season,
                    GameDate: dateString,
                    GameId: "",
                    CreatedUTC: new Date().toISOString(),
                    LastModifiedUTC: new Date().toISOString(),
                    Stats: []
                };
                ftp.syncFile("MI_Fielding_" + dateString + ".csv", function (data) {
                    if (!data || data.length == 0) {
                        if (callback) {
                            callback(false);
                        }
                        else {
                            callback(false, { status: 200, msg: "Fielding Minors FTP Error" });
                        }
                    } else {
                        stat.Stats = data;
                        FieldingMinors.create(stat, function (err, newPlayerStats) {
                            if (err) {
                                console.log(err)
                            }
                            else {
                                console.log(dateString);
                            }
                            //     date = new Date(date.setDate(date.getDate() + 1));
                            context.putZoneDataInFielding(dateString, newPlayerStats, function () {

                                newPlayerStats.markModified("Stats");
                                newPlayerStats.save(function (err, result) {
                                    if (callback) {
                                        callback(dateString)
                                    }
                                    else {
                                        callback(dateString, { status: 200, msg: "complete" });
                                    }
                                })
                            });

                        });
                    }
                });

            }
            else {

                context.putZoneDataInFielding(dateString, playerStats, function () {
                    playerStats.markModified("Stats");
                    playerStats.save(function (err, result) {
                        if (callback) {
                            callback(dateString)
                        }
                        else {
                            callback(dateString, { status: 200, msg: "fielding complete" });
                        }
                    });
                });
            }
        });
    },

    saveOneGroupOfStats: function (count, StatsDB, PlayerStatsDB, groupSize, totalSize, type, callback) {
        var context = this;

        if (count * groupSize >= totalSize) {
            callback("success");
        } else {

            StatsDB.find({}, function (err, nextStats) {
                var n = nextStats;
                if (!err && nextStats && nextStats.length > 0) {
                    var players = [];
                    var playerIds = [];

                    // for each gamedate...
                    for (var g = 0; g < nextStats.length; g++) {
                        var nextGame = nextStats[g];
                        if (nextGame.Stats) {

                            // for each game's stats in the gamedate
                            for (var s = 0; s < nextGame.Stats.length; s++) {
                                var playersIndex = playerIds.indexOf(nextGame.Stats[s].MLBId);
                                if( type='Fielding') {
                                    // *********** deal with errors in fielding 
                                    // from BIS data and/or error in requirements ********* //
                                    let stats = nextGame.Stats[s];
                                    if( stats.BallsInZone < 0 ) {
                                        stats.BallsInZone = 0;
                                    }
                                    if( stats.OutsOutOfZone < 0 ) {
                                        stats.OutsOutOfZone = 0;
                                    }
                                    if( stats.MissedBallsInZone < 0) {
                                        stats.MissedBallsInZone = 0;
                                    }
                                    if( stats.BallsInZone > 12 ) {
                                        stats.BallsInZone = 0;
                                        if( stats.OutsOutOfZone > 3 ) {
                                            stats.OutsOutOfZone = 0;
                                        }
                                        if( stats.MissedBallsInZone > 2) {
                                            stats.MissedBallsInZone = 0;
                                        }
                                    }
                                    if( stats.Pos == "" || stats.OutsOutOfZone > 7 ) {
                                        stats.OutsOutOfZone = 0;
                                    }
                                    if( stats.MissedBallsInZone > 5) {
                                        stats.MissedBallsInZone = 0;
                                    }
                                }
                                if (playersIndex == -1) {
                                    // create the player in the list and push his stats into the array;
                                    playerIds.push(nextGame.Stats[s].MLBId);
                                    players.push({ MLBId: nextGame.Stats[s].MLBId, PlayerName: nextGame.Stats[s].PlayerName, Games: [{ GameDate: nextGame.GameDate, GameId: nextGame.Stats[s].GameId, Stats: nextGame.Stats[s] }] });

                                } else {
                                    // add another row to this player's stats
                                    players[playersIndex].Games.push({ GameDate: nextGame.GameDate, GameId: nextGame.Stats[s].GameId, Stats: nextGame.Stats[s] });
                                }


                            }
                        }

                    }

                }
                // now insert all of these records (aka documents) into the right collection
                PlayerStatsDB.insertMany(players, function (err, results) {
                    players = null;
                    results = null;
                    context.saveOneGroupOfStats(++count, StatsDB, PlayerStatsDB, groupSize, totalSize, type, callback);
                })

            }).skip(count * groupSize).limit(groupSize);

        }
    },

    saveStatsByType: function (type, level, callback) {
        var StatsDB;
        var PlayerStatsDB;
        switch (type) {
            case "Batting":
                StatsDB = Batting;
                PlayerStatsDB = PlayerBatting;
                if (level != "ML") {
                    StatsDB = BattingMinors;
                    PlayerStatsDB = PlayerBattingMinors;
                }
                break;
            case "Baserunning":
                StatsDB = Baserunning;
                PlayerStatsDB = PlayerBaserunning;
                if (level != "ML") {
                    StatsDB = BaserunningMinors;
                    PlayerStatsDB = PlayerBaserunningMinors;
                }
                break;
            case "Fielding":
                StatsDB = Fielding;
                PlayerStatsDB = PlayerFielding;
                if (level != "ML") {
                    StatsDB = FieldingMinors;
                    PlayerStatsDB = PlayerFieldingMinors;
                }
                break;

            default:
                StatsDB = Pitching;
                PlayerStatsDB = PlayerPitching;
                if (level != "ML") {
                    StatsDB = PitchingMinors;
                    PlayerStatsDB = PlayerPitchingMinors;
                }
                break;
        }

        var context = this;
        StatsDB.count({}, function (err, count) {
            if (!err && count && count > 0) {

                // remove all the players from the PlayerStats<type> collection
                PlayerStatsDB.remove({}, function (err, results) {

                    // using iterator, read games in in groups of a set size:
                    var groupSize = 30;

                    context.saveOneGroupOfStats(0, StatsDB, PlayerStatsDB, groupSize, count, type, function (err) {
                        callback(err, 0);
                    })
                })

            } else {
                callback(err, 0);
            }
        });

    },

    savePlayerStats(callback) {
        var context = this;

        context.saveStatsByType("Batting", "ML", function () {
            console.log("completed batting");
            context.saveStatsByType("Baserunning", "ML", function () {
                console.log("completed baserunning");
                context.saveStatsByType("Fielding", "ML", function () {
                    console.log("completed fielding");
                    context.saveStatsByType("Pitching", "ML", function () {
                        console.log("completed pitching");

                        context.saveStatsByType("Batting", "Minors", function () {
                            console.log("completed batting minors");
                            context.saveStatsByType("Baserunning", "Minors", function () {
                                console.log("completed baserunning minors");
                                context.saveStatsByType("Fielding", "Minors", function () {
                                    console.log("completed fielding minors");
                                    context.saveStatsByType("Pitching", "Minors", function () {
                                        console.log("completed pitching minors");
                                        callback("success");
                                    })
                                })
                            })
                        })
                    })
                })
            })
        })
    }
}