var utils = require('../helpers/utils');
var request = require('request');
var cheerio = require('cheerio');
var moment = require('moment');
var mongoose = require('mongoose');
var Team = mongoose.model('Team');
var GameDay = mongoose.model('GameDay');
var League = mongoose.model('League');
var Schedule = mongoose.model('Schedule');
var Roster = mongoose.model("Roster");
var Deal = mongoose.model("Deal");
var Boxscore = mongoose.model("Boxscore");

var Import = require("../helpers/importHelper");
var leagueHelper = require("../helpers/leagueHelper");
var teamHelper = require("../helpers/teamHelper");
var playerHelper = require("../helpers/playerHelper");
var messageHelper = require('../helpers/messageHelper');

var sim = require('../helpers/simulation');

var playerStatus = require('../helpers/playerStatus');


module.exports = {

    /// gameDayHelper manages the game day sequence:
    /*
        all times PST
        3am - import all the daily stats from the previous day MLB and Minors
         - run active player eligilbity
         - run inactive player eligilibty
        ? 9am  - trigger all 9am roster moves
            - update all trade requests 
            - update all auto-transfers if possible
        if set in leagues, use league time, otherwise - play all games, 
        if set to autosave in league settings: register wins, update standings, accumulate stats, send notifications
    */

    // **********************************************************************
    //
    // MAIN ENTRY POINT FOR CRON JOB TO AUTOMATE A GAME DAY
    //
    // ***********************************************************************
    nextGameDayActivity: function (callback) {
        var keyDates = this.getKeyDatesAndTime();
        var context = this;

        var DO_COMPLETE_GAMEDAY = true;

        // test date
        const testDayAndTime = false;
        if( testDayAndTime ) {
            testDate = "20180828";
            testTime = 5;
            keyDates.testDayAndTime = true;
            keyDates.testTime = 5;
        }

        // only do this after the appointed hour to start the day...
        if (keyDates.Hour >= 3) {
            // see if there's a gameday for each league.
            console.log("After 3am.. doing next game day activity");
            League.find({}, { _id: 1, Name: 1, Settings: 1, "Teams._id": 1, "Teams.r_name": 1, "Teams.r_abbreviation": 1 }, function (err, leagues) {
                if (err || leagues == null || leagues.length == 0) {
                    messages.status = "no leagues found";
                    callback(err, messages);
                } else {

                    // sort leagues so default league is first
                    leagues.sort(function (a, b) {
                        if (a.Settings && a.Settings.Default && a.Settings.Default === true) {
                            if (b.Settings && b.Settings.Default && b.Settings.Default === true) {
                                // a and b are default leagues
                                return 0;
                            } else {
                                // a is default and b is not
                                return -1;
                            }

                        } else {
                            if (b.Settings && b.Settings.Default && b.Settings.Default === true) {
                                // only b is default league
                                return 1;
                            } else {
                                // neither is default
                                return 0;
                            }
                        }
                    }
                    );

                    // remove leagues that aren't default and aren't active for game day games
                    for (var l = leagues.length - 1; l >= 0; l--) {
                        var remove = false;
                        if (leagues[l].Settings) {
                            if (leagues[l].Settings.Default && leagues[l].Settings.Default === true) {
                                remove = false;
                            } else if (leagues[l].Settings.AutoPlayGameDays && leagues[l].Settings.AutoPlayGameDays === true) {
                                remove = false;
                            } else {
                                remove = true;
                            }
                        } else {
                            remove = true;
                        }
                        if (remove) {
                            leagues.splice(l, 1);
                        }
                    }

                    var defaultLeague = false;
                    for (var l = 0; l < leagues.length; l++) {
                        if (leagues[l].Settings && leagues[l].Settings.Default && leagues[l].Settings.Default === true) {
                            defaultLeague = leagues[l];
                            break;
                        }
                    }
                    if (!defaultLeague && leagues.length > 0) {
                        // nothing specified as default, just grab the first active league
                        defaultLeague = leagues[0];
                    }
                    if (!defaultLeague) {
                        messages.status = "no default or active league found";
                        callback("error", messages);
                    } else {

                        // get any league settings that impact timing or execution
                        var gameHour = 12;
                        var gameHourNC = 18;
                        var autoSaveBoxScores = false;
                        var autoExecuteExpiredRequests = false;
                        if (defaultLeague.Settings) {
                            if (defaultLeague.Settings.GameHour) {
                                var hour = Number(defaultLeague.Settings.GameHour);
                                if (!isNaN(hour) && Number.isInteger(hour) && hour > 4 && hour < 27) {
                                    // it's a number, is it an integer
                                    gameHour = defaultLeague.Settings.GameHour;
                                }
                            }
                            if (defaultLeague.Settings.GameHourNC) {
                                var hour = Number(defaultLeague.Settings.GameHourNC);
                                if (!isNaN(hour) && Number.isInteger(hour) && hour > 4 && hour < 27) {
                                    // it's a number, an integer and between 4am and 11pm
                                    gameHourNC = defaultLeague.Settings.GameHourNC;
                                }
                            }
                            if( testDayAndTime ) {
                                gameHour = testTime;
                                gameHourNC = testTime;
                                keyDates.GameDate = testDate;
                            
                            }

                            if (defaultLeague.Settings.AutoSaveScores && defaultLeague.Settings.AutoSaveScores == true) {
                                autoSaveBoxScores = true;
                            }
                            if (defaultLeague.Settings.autoExecuteExpiredRequests && defaultLeague.Settings.autoExecuteExpiredRequests == true) {
                                autoExecuteExpiredRequests = true;
                            }
                        }

                        // *************************  ready to roll with this league!
                        // *************************  based on the time, do the next activity
                        // first see if there's a gameDay document for this day for this league, if not just create it.

                        context.getGameDay(defaultLeague._id, keyDates, null, function (baseGameDay) {

                            // iterate through the active leagues
                            context.doGameDayForLeague(0, leagues, keyDates, baseGameDay, function (result){
                                callback( result );
                            })
                        })
                    }
                }
            })

        } else {
            callback("too early"); // too early
        }

    },

    doGameDayForLeague: function(index, leagues, keyDates, baseGameDay, callback ) {
        if( index >= leagues.length ) {
            callback("success");
        } else {
            // do all the stuff for this league.. note default league is done first.
            var context = this;
            var DO_COMPLETE_GAMEDAY = true;

            var defaultLeague = leagues[index];
                     // get any league settings that impact timing or execution
                     var gameHour = 12;
                     var gameHourNC = 18;
                     var autoSaveBoxScores = false;
                     var autoExecuteExpiredRequests = false;
                     if (defaultLeague.Settings) {
                         if (defaultLeague.Settings.GameHour) {
                             var hour = Number(defaultLeague.Settings.GameHour);
                             if (!isNaN(hour) && Number.isInteger(hour) && hour > 4 && hour < 27) {
                                 // it's a number, is it an integer
                                 gameHour = defaultLeague.Settings.GameHour;
                             }
                         }
                         if (defaultLeague.Settings.GameHourNC) {
                             var hour = Number(defaultLeague.Settings.GameHourNC);
                             if (!isNaN(hour) && Number.isInteger(hour) && hour > 4 && hour < 27) {
                                 // it's a number, an integer and between 4am and 11pm
                                 gameHourNC = defaultLeague.Settings.GameHourNC;
                             }
                         }
                         if( keyDates.testDayAndTime ) {
                             gameHour = keyDates.testTime;
                             gameHourNC = keyDates.testTime;
                         }
                         if (defaultLeague.Settings.AutoSaveScores && defaultLeague.Settings.AutoSaveScores == true) {
                             autoSaveBoxScores = true;
                         }
                         if (defaultLeague.Settings.autoExecuteExpiredRequests && defaultLeague.Settings.autoExecuteExpiredRequests == true) {
                             autoExecuteExpiredRequests = true;
                         }
                     }
                     console.log("START LEAGUE DAY: " + defaultLeague.Name);
            context.getGameDay(defaultLeague._id, keyDates, baseGameDay, function (gameday) {

                if (gameday.Completed == true) {
                    context.doGameDayForLeague(++index, leagues, keyDates, baseGameDay, callback );
                } else {

                    // do this every time through... it triggers at 9am PT if it runs at all.
                    context.makeRosterMoves(defaultLeague._id, gameday, keyDates, autoExecuteExpiredRequests, function (err, message, list) {
                        console.log("trade eligilibity done and intra-team moves.");


                        // ************************ IMPORT MLB STATS AND MINOR LEAGUE STATS

                        // try 3 times only
                        context.tryToImportMLBStats(gameday, keyDates, function (error) {

                            if (error == "error") {
                                // we're stuck in the water.
                                context.doGameDayForLeague(++index, leagues, keyDates, baseGameDay, callback );
                            } else {

                                // if first league, then grab the imported stats
                                if( index == 0 ) {
                                    baseGameDay = gameday;
                                }
                                // ************************ SET STATUS FOR EACH ACIVE PLAYER
                                if (DO_COMPLETE_GAMEDAY) {
                                    console.log("start setActives()");
                                    context.setActives(gameday, keyDates, function (err, messages) {
                                        if (err) {
                                            gameday.Status = "set actives failed";
                                        } else {
                                            gameday.Status = "actives complete";
                                        }
                                        if (messages) {
                                            var now = moment().tz("America/Los_Angeles");
                                            var nowFormatted = now.format("HH:mm");

                                            gameday.EligibilityActive = messages;
                                            gameday.EligibilityActive["Completed"] = nowFormatted;

                                            gameday.LastUpdate = nowFormatted;
                                            gameday.markModified('EligibilityActive');
                                            gameday.save(function (err, results) {
                                                var e = err;
                                                console.log("completed attempt to set actives eligibility");
                                            })
                                        }
                                        if (err) {
                                            context.doGameDayForLeague(++index, leagues, keyDates, baseGameDay, callback );
                                        } else {

                                            // ************************ SET STATUS FOR EACH INACTIVE/NON-ROSTER PLAYER

                                            // have actives.. get the inactives
                                            if (keyDates.Hour < gameHour || (gameday.GamesPlayed && gameday.GamesPlayed.status && gameday.GamesPlayed.status == "completed")) {
                                                // then have time to try to set the inactives


                                                context.setInactives(gameday, keyDates, function (err, messageObj) {
                                                    console.log("completed attempt to set inactives eligibility");
                                                    if (err) {
                                                        gameday.Status = "inactives failed";
                                                    } else {
                                                        gameday.Status = "inactives complete";
                                                    }
                                                    if (messageObj) {
                                                        var now = moment().tz("America/Los_Angeles");
                                                        var nowFormatted = now.format("HH:mm");

                                                        gameday.EligibilityInactive = messageObj;
                                                        gameday.EligibilityInactive["Completed"] = nowFormatted;
                                                        gameday.markModified('EligibilityInactive');
                                                        gameday.LastUpdate = nowFormatted;
                                                        gameday.save(function (err, results) {
                                                            var e = err;
                                                            console.log("completed attempt to set inactives eligibility");
                                                            // *********************************************** Play all games
                                                            context.playAllGames(defaultLeague._id, gameday, keyDates, gameHour, "game1", autoSaveBoxScores, function (err, messageObj) {
                                                                context.playAllGames(defaultLeague._id, gameday, keyDates, gameHourNC, "nightcap", autoSaveBoxScores, function (err, messageObj) {
                                                                    context.doGameDayForLeague(++index, leagues, keyDates, baseGameDay, callback );
                                                                })
                                                            })
                                                        })
                                                    } else {
                                                        // messageObj does not exist
                                                        if (err) {
                                                            context.doGameDayForLeague(++index, leagues, keyDates, baseGameDay, callback );
                                                        } else {

                                                            // *********************************************** Play all games
                                                            context.playAllGames(defaultLeague._id, gameday, keyDates, gameHour, "game1", autoSaveBoxScores, function (err, messageObj) {
                                                                context.playAllGames(defaultLeague._id, gameday, keyDates, gameHourNC, "nightcap", autoSaveBoxScores, function (err, messageObj) {
                                                                    context.doGameDayForLeague(++index, leagues, keyDates, baseGameDay, callback );
                                                                })
                                                            })
                                                        }
                                                    }

                                                })
                                            } else {
                                                // too late in the day to set the inactives.. just play the games now
                                                // *********************************************** Play all games
                                                context.playAllGames(defaultLeague._id, gameday, keyDates, gameHour, "game1", autoSaveBoxScores, function (err, messageObj) {
                                                    context.playAllGames(defaultLeague._id, gameday, keyDates, gameHourNC, "nightcap", autoSaveBoxScores, function (err, messageObj) {
                                                        context.doGameDayForLeague(++index, leagues, keyDates, baseGameDay, callback );
                                                    })
                                                })
                                            }
                                        }
                                    })

                                } else {
                                    // don't do complete gameday
                                    context.doGameDayForLeague(++index, leagues, keyDates, baseGameDay, callback );
                                }

                            }
                        })
                    })
                }
            })
        }
    },

    getKeyDatesAndTime: function () {
        var keyDates = {
            From: 20180329, // starting date of 2018 season
            To: 20180401,   // date BEFORE today's game
            GameDate: 20180401, // date OF today's game
            GameCalendarDay: 91, // calendar day of today's game
            Season: 2018,
            Time: "00:00",     // time right now on 24 hour clock.. just hours and minutes
            Hour: "00"
        }

        var now = moment().tz("America/Los_Angeles");
        keyDates.GameDate = now.format("YYYYMMDD");
        keyDates.GameCalendarDay = now.dayOfYear();
        keyDates.Season = now.format("YYYY");
        keyDates.Time = now.format("HH:mm");
        keyDates.Hour = now.format("HH");
        keyDates.To = now.subtract(1, "days").format("YYYYMMDD");
        keyDates.Raw = now;
        return (keyDates);
    },

    getGameDay: function (leagueId, keyDates, baseGameDay, callback) {
        GameDay.findOne({ LeagueId: leagueId, GameDate: keyDates.GameDate }, function (err, gameday) {
            if (err || !gameday) {
                // create a gameday
                    var gd = new GameDay;
                    /*
                    var gameDaySchema = new mongoose.Schema({
                        LeagueId: String,
                        GameDate: String,
                        CalendarGameDay: Number,
                        Completed: Boolean,
                        Status: String,
                        DoubleHeader: Boolean,
                        ImportStats: {},
                        EligibilityActive: {},
                        EligibilityInactive: {},
                        Transactions: {},
                        Games: {},
                        AccumulatedStats: {}
                    });
                    */
                gd.LeagueId = leagueId;
                gd.GameDate = keyDates.GameDate;
                gd.CalendarGameDay = keyDates.GameCalendarDay;
                gd.Completed = false;
                gd.Status = "not started";
                gd.ImportStatus = "";
                if (baseGameDay) {
                    // copy key data/status from existing game day
                    gd.Status = baseGameDay.Status;
                    if( baseGameDay.ImportStats) {
                        gd["ImportStats"] = baseGameDay.ImportStats;
                        gd["ImportStatus"] = baseGameDay.ImportStatus;
                    }
                } 
                GameDay.create(gd, function (err, gameday) {
                    callback(gameday);
                })

            } else {
                // move on with the gameday.
                callback(gameday);
            }
        })
    },

    importAllMLBStats: function (gameDay, keyDates, callback) {
        var msg = {};
        var error = false;
        var errorMinors = false;

        Import.getBatting(keyDates.To, keyDates.Season, function (message) {
            msg["Batting"] = message;
            if (!message)
                error = true;
            Import.getPitching(keyDates.To, keyDates.Season, function (message) {
                msg["Pitching"] = message;
                if (!message)
                    error = true;
                Import.getBaserunning(keyDates.To, keyDates.Season, function (message) {
                    msg["Baserunning"] = message;
                    if (!message)
                        error = true;
                    Import.getYTDFielding(keyDates.To, keyDates.Season, function (message) {
                        msg["YTD Fielding"] = message;
                        if (!message)
                            error = true;
                        Import.getFielding(keyDates.To, keyDates.Season, function (message) {
                            msg["Fielding"] = message;
                            if (!message)
                                error = true;
                            if (!error) {
                                msg['MajorsComplete'] = true;
                            }

                            // minors now...
                            Import.getBattingMinors(keyDates.To, keyDates.Season, function (message) {
                                msg["Minors Batting"] = message;
                                if (!message)
                                    errorMinors = true;
                                Import.getPitchingMinors(keyDates.To, keyDates.Season, function (message) {
                                    msg["Minors Pitching"] = message;
                                    if (!message)
                                        errorMinors = true;

                                    Import.getBaserunningMinors(keyDates.To, keyDates.Season, function (message) {
                                        msg["Minors Baserunning"] = message;
                                        if (!message)
                                            errorMinors = true;

                                        Import.getYTDFieldingMinors(keyDates.To, keyDates.Season, function (message) {
                                            msg["Minors YTD Fielding"] = message;
                                            if (!message)
                                                errorMinors = true;
                                            Import.getFieldingMinors(keyDates.To, keyDates.Season, function (message) {
                                                msg["Minors Fielding"] = message;
                                                if (!message)
                                                    errorMinors = true;
                                                if (!errorMinors) {
                                                    msg["MinorsComplete"] = true;
                                                }
                                                callback(error || errorMinors, msg);
                                            })
                                        })
                                    })
                                })
                            })
                        })
                    })
                })
            })
        })
    },

    tryToImportMLBStats: function (gameDay, keyDates, callback) {

        // first see if it's already done!
        if (gameDay.ImportStatus && (gameDay.ImportStatus === true || gameDay.ImportStatus == "true")) {
            // double check on the minors
            callback(false, "success");
            return;
        }

        // not done, work through the FTP loads/imports
        var context = this;

        // try 3 times only
        context.importAllMLBStats(gameDay, keyDates, function (error, messageObj) {

            if (error) {
                // try a 2nd time
                context.importAllMLBStats(gameDay, keyDates, function (error, messageObj) {

                    if (error) {
                        // try 1 more time only
                        context.importAllMLBStats(gameDay, keyDates, function (error, messageObj) {
                            // try 3 times only
                            if (error) {
                                // is error, save the results so far
                                var majorsSuccess = false;
                                if (messageObj.MajorsComplete && messageObj.MajorsComplete == true) {
                                    majorsSuccess = true;
                                }
                                gameDay.ImportStats = messageObj;
                                gameDay.markModified("ImportStats");
                                gameDay.Status = "import failed";
                                gameDay.ImportStatus = false;
                                if (majorsSuccess) {
                                    gameDay.Status = "majors import success. minors import failed";
                                    gameDay.ImportStatus = "majors only success";
                                }

                                var now = moment().tz("America/Los_Angeles");
                                gameDay.LastUpdate = now.format("HH:mm");
                                Import.savePlayerStats(function (message) {

                                    var now = moment().tz("America/Los_Angeles");
                                    var nowFormatted = now.format("HH:mm");
                                    gameDay.ImportStats["StatsByPlayerCompleted"] = nowFormatted;

                                    gameDay.save(function (err, results) {
                                        var e = err;
                                        if (majorsSuccess) {
                                            callback("success");
                                        } else {
                                            callback("error");
                                        }
                                    });
                                });


                            } else {
                                // no errors, save the results
                                var now = moment().tz("America/Los_Angeles");
                                var nowFormatted = now.format("HH:mm");
                                gameDay.ImportStats = messageObj;
                                gameDay.ImportStats["Completed"] = nowFormatted;
                                gameDay.markModified("ImportStats");

                                gameDay.Status = "import complete";
                                gameDay.LastUpdate = nowFormatted;
                                gameDay.ImportStatus = nowFormatted;
                                Import.savePlayerStats(function (message) {

                                    var now = moment().tz("America/Los_Angeles");
                                    var nowFormatted = now.format("HH:mm");
                                    gameDay.ImportStats["StatsByPlayerCompleted"] = nowFormatted;


                                    gameDay.save(function (err, results) {
                                        var e = err;
                                        callback("success");
                                    });
                                });

                            }
                        })

                    } else {
                        // no errors, save the results
                        var now = moment().tz("America/Los_Angeles");
                        var nowFormatted = now.format("HH:mm");

                        gameDay.ImportStats = messageObj;
                        gameDay.ImportStats["Completed"] = nowFormatted;
                        gameDay.markModified("ImportStats");
                        gameDay.Status = "import complete";
                        var now = moment().tz("America/Los_Angeles");
                        gameDay.LastUpdate = now.format("HH:mm");
                        gameDay.ImportStatus = now.format("HH:mm");
                        Import.savePlayerStats(function (message) {

                            var now = moment().tz("America/Los_Angeles");
                            var nowFormatted = now.format("HH:mm");
                            gameDay.ImportStats["StatsByPlayerCompleted"] = nowFormatted;

                            gameDay.save(function (err, results) {
                                var e = err;
                                console.log("completed import of MLB stats");
                                callback("success");
                            });

                        })

                    }

                })
            } else {
                // no errors, save the results
                var now = moment().tz("America/Los_Angeles");
                var nowFormatted = now.format("HH:mm");
                var savePlayerStatsDone = false;
                if (gameDay.ImportStats && gameDay.ImportStats.StatsByPlayerCompleted) {
                    savePlayerStatsDone = gameDay.ImportStats.StatsByPlayerCompleted;
                }
                gameDay.ImportStats = messageObj;
                gameDay.ImportStats["Completed"] = nowFormatted;

                gameDay.markModified("ImportStats");
                gameDay.Status = "import complete";
                var now = moment().tz("America/Los_Angeles");
                gameDay.LastUpdate = now.format("HH:mm");
                gameDay.ImportStatus = now.format("HH:mm");

                // don't do this more than once if possible
                if (savePlayerStatsDone) {

                    gameDay.ImportStats["StatsByPlayerCompleted"] = savePlayerStatsDone;
                    gameDay.save(function (err, results) {
                        var e = err;
                        console.log("completed import of MLB stats");
                        callback("success");
                    });
                } else {
                    Import.savePlayerStats(function (message) {

                        var now = moment().tz("America/Los_Angeles");
                        var nowFormatted = now.format("HH:mm");
                        gameDay.ImportStats["StatsByPlayerCompleted"] = nowFormatted;

                        gameDay.save(function (err, results) {
                            var e = err;
                            console.log("completed import of MLB stats");
                            callback("success");
                        });

                    })
                }


            }
        })
    },

    setActives: function (gameDay, keyDates, callback) {
        // first see if it's already done!
        if (gameDay.EligibilityActive && gameDay.EligibilityActive.status && gameDay.EligibilityActive.status == "success") {
            callback(false, null);
            return;
        }

        // not done yet
        Import.cronSetEligilibityForActives(keyDates, function (err, messages) {
            callback(err, messages);
        })
    },

    // we call this after games are complete...... once after normal games, once after the nightcaps
    setActivesPostGame: function (gameDay, keyDates, callback) {
        // always do this when called...
        Import.cronSetEligilibityForActives(keyDates, function (err, messages) {
            callback(err, messages);
        })
    },

    // for the 2nd time we call this... right before we play the games....
    setActives2: function (gameDay, keyDates, callback) {
        // first see if it's already done!
        if (gameDay.EligibilityActive2 && gameDay.EligibilityActive2.status && gameDay.EligibilityActive2.status == "success") {
            callback(false, null);
            return;
        }

        // not done yet
        Import.cronSetEligilibityForActives(keyDates, function (err, messages) {
            callback(err, messages);
        })
    },

    setInactives: function (gameDay, keyDates, callback) {
        // first see if it's already done!

        /*
        var test = false;
        if (test) {
            callback(false, gameDay.EligibilityInactive);
        return;
    }
    */
        if (gameDay.EligibilityInactive && gameDay.EligibilityInactive.status && gameDay.EligibilityInactive.status == "success") {
            callback(false, null);
            return;
        }

        // not done yet
        Import.cronSetEligilibityForInactives(keyDates, function (err, messages) {
            callback(err, messages);
        })
    },

    makeDealPlayersEligible: function (index, deals, tradeIds, callback) {
        if (index >= deals.length) {
            callback();
        } else {
            var context = this;

            var nextDeal = deals[index];

            // go through each team's deal points and make any players in the tradeIds eligible
            for (let t = 0; t < nextDeal.Teams.length; t++) {
                for (let d = 0; d < nextDeal.Teams[t].dealPoints.length; d++) {
                    if (nextDeal.Teams[t].dealPoints[d].player) {
                        var dealPlayer = nextDeal.Teams[t].dealPoints[d].player;
                        if (tradeIds.indexOf(dealPlayer.PlayerId) >= 0) {
                            dealPlayer["iseligible"] = "Yes";
                        }
                    }
                }
            }
            // write out deal...
            deals[index].markModified("Teams");
            deals[index].save(function (err, result) {
                context.makeDealPlayersEligible(++index, deals, tradeIds, callback);
            })

        }
    },

    doOneTeamsTradeRequests: function (index, league, nowGMT, tradeIds, tradeList, callback) {
        var context = this;
        if (index >= league.Teams.length) {
            // cycled through all the teams...
            // now iterate through all of the trades
            Deal.find({ LeagueId: league._id, Type: "Trade", ChildDealId: "", Status: { $ne: "Approved" }, "Players.PlayerId": { $in: tradeIds } }, function (err, deals) {
                if (err) {
                    callback(true, "success", tradeList);
                } else {
                    context.makeDealPlayersEligible(0, deals, tradeIds, function (err, result) {
                        callback(true, "success", tradeList);
                    })
                }
            })
        } else {

            Roster.findOne({ LeagueId: league._id, TeamId: league.Teams[index]._id }, { FortyManNL: 1, NonRoster: 1 }, function (err, roster) {
                if (err) {
                    // do the next team
                    context.doOneTeamsTradeRequests(++index, league, now, tradeIds, tradeList, callback);
                } else {
                    for (var p = 0; p < roster.FortyManNL.length; p++) {
                        if (roster.FortyManNL[p].TradeStatus == playerStatus.RequestTradingBlock) {

                            var dueTime = moment(roster.FortyManNL[p].TradeDueTime);
                            var timeDiff = nowGMT.diff(dueTime, 'minutes');

                            if (timeDiff >= 0) {
                                roster.FortyManNL[p].TradeStatus = playerStatus.OnTradingBlock;
                                tradeList.push({ Name: roster.FortyManNL[p].FullName, PlayerId: roster.FortyManNL[p].PlayerId });
                                tradeIds.push(roster.FortyManNL[p].PlayerId);
                            } else {
                                timeDiff = -1;
                            }
                        }
                    }
                    for (var p = 0; p < roster.NonRoster.length; p++) {
                        if (roster.NonRoster[p].TradeStatus == playerStatus.RequestTradingBlock) {
                            var dueTime = moment(roster.NonRoster[p].TradeDueTime);
                            var timeDiff = nowGMT.diff(dueTime, 'minutes');

                            if (timeDiff >= 0) {
                                roster.NonRoster[p].TradeStatus = playerStatus.OnTradingBlock;
                                tradeList.push({ Name: roster.NonRoster[p].FullName, PlayerId: roster.NonRoster[p].PlayerId });
                                tradeIds.push(roster.NonRoster[p].PlayerId);
                            } else {
                                timeDiff = -1
                            }
                        }
                    }

                    roster.markModified("FortyManNL");
                    roster.markModified("NonRoster");
                    roster.save(function (err, result) {
                        // do the next team
                        context.doOneTeamsTradeRequests(++index, league, nowGMT, tradeIds, tradeList, callback);
                    })
                }
            })


        }
    },

    doEachPlayersIntraTeamMove: function (index, leagueId, teamId, players, callback) {
        var context = this;
        if (index >= players.length) {
            callback();
        } else {
            playerHelper.executeNextAction(null, leagueId, teamId, players[index], true, function (response) {
                context.doEachPlayersIntraTeamMove(++index, leagueId, teamId, players, callback)
            })
        }
    },

    doOneTeamsIntraTeamMoves: function (index, league, nowGMT, callback) {
        var context = this;
        if (index >= league.Teams.length) {
            // cycled through all the teams...

            callback(true, "success");


        } else {

            Roster.findOne({ LeagueId: league._id, TeamId: league.Teams[index]._id }, { FortyManNL: 1, NonRoster: 1, TeamId: 1 }, function (err, roster) {
                if (err) {
                    // do the next team
                    context.doOneTeamsIntraTeamMoves(++index, league, now, callback);
                } else {

                    var players = [];
                    for (var p = 0; p < roster.FortyManNL.length; p++) {
                        if (roster.FortyManNL[p].Status != playerStatus.ActiveRoster) {

                            var dueTime = moment(roster.FortyManNL[p].DueTime);
                            var timeDiff = nowGMT.diff(dueTime, 'minutes');

                            if (timeDiff >= 0) {
                                players.push(roster.FortyManNL[p]);

                            } else {
                                timeDiff = -1;
                            }
                        }
                    }
                    for (var p = 0; p < roster.NonRoster.length; p++) {
                        if (roster.NonRoster[p].TradeStatus >= playerStatus.DL) {
                            var dueTime = moment(roster.NonRoster[p].DueTime);
                            var timeDiff = nowGMT.diff(dueTime, 'minutes');

                            if (timeDiff >= 0) {
                                players.push(roster.NonRoster[p]);

                            } else {
                                timeDiff = -1
                            }
                        }
                    }

                    context.doEachPlayersIntraTeamMove(0, league._id, roster.TeamId, players, function () {
                        // do the next team
                        context.doOneTeamsIntraTeamMoves(++index, league, nowGMT, callback);
                    })
                }
            })


        }
    },


    executeExpiredTradeRequests: function (leagueId, gameDay, keyDates, callback) {
        var context = this;

        var moment = require('moment-timezone');
        var nowGMT = moment().tz("America/Los_Angeles");
        League.findOne({ _id: leagueId }, { Teams: 1 }, function (err, league) {
            if (err) {
                callback(err, "failed");
            } else {

                // first do trade status update, then update all the Deals (end of iterator's function)
                var tradeIds = []
                var tradeList = [];

                // run the iterator now
                context.doOneTeamsTradeRequests(0, league, nowGMT, tradeIds, tradeList, function (err, message, tradeList) {
                    var now = moment().tz("America/Los_Angeles");
                    var nowFormatted = now.format("HH:mm");
                    if (!gameDay.Transactions) {
                        gameDay["Transactions"] = {};
                    }
                    gameDay.Transactions["TradesUpdated"] = nowFormatted;
                    gameDay.Transactions["TradesStatus"] = "success";

                    gameDay.markModified('Transactions');
                    gameDay.save(function (err, results) {
                        callback(err, "success");
                    })
                });
            }
        })

    },

    executeExpiredIntraTeamMoves: function (leagueId, gameDay, keyDates, callback) {
        var context = this;

        var moment = require('moment-timezone');
        var nowGMT = moment().tz("America/Los_Angeles");

        // been asked to turn this off temporarily
        callback(null, "success");

        League.findOne({ _id: leagueId }, { Teams: 1 }, function (err, league) {
            if (err) {
                callback(err, "failed");
            } else {

                // find players with intrateam moves

                // run the iterator now
                context.doOneTeamsIntraTeamMoves(0, league, nowGMT, function (err, message, tradeList) {
                    var now = moment().tz("America/Los_Angeles");
                    var nowFormatted = now.format("HH:mm");
                    if (!gameDay.Transactions) {
                        gameDay["Transactions"] = {};
                    }
                    gameDay.Transactions["IntraTeamMovesUpdated"] = nowFormatted;
                    gameDay.Transactions["IntraTeamMovesStatus"] = "success";

                    gameDay.markModified('Transactions');
                    gameDay.save(function (err, results) {
                        callback(err, "success");
                    })
                });
            }
        })
    },


    makeRosterMoves: function (leagueId, gameDay, keyDates, autoExecuteExpiredRequests, callback) {
        var context = this;

        if (!gameDay.Transactions) {
            gameDay["Transactions"] = {};

        }

        if (autoExecuteExpiredRequests == false || keyDates.Hour < 8 || keyDates.Hour > 11 || (gameDay.Transactions && gameDay.Transactions.TradesStatus && gameDay.TradesStatus == "success" && gameDay.Transactions.IntraTeamMoveStatus && gameDay.Transactions.IntraTeamMovesStatus == "success")) {
            callback(null, null);
            return;
        }

        // something not done  yet and it's late enough in the day...
        // first see if trade requests are done
        if (!gameDay.Transactions.TradesStatus && gameDay.Transactions.TradesStatus != "success") {
            // then do the trades status work now
            context.executeExpiredTradeRequests(leagueId, gameDay, keyDates, function (err, message) {
                // do the intra-team moves now if possible
                if (!gameDay.Transactions.IntraTeamMoves && gameDay.Transactions.IntraTeamMoves != "success") {
                    context.executeExpiredIntraTeamMoves(leagueId, gameDay, keyDates, function (err, message) {
                        callback();
                    })
                } else {
                    // done
                    callback();
                }
            })
        } else {
            // do the intra-team moves now if possible
            if (!gameDay.Transactions.IntraTeamMoves && gameDay.Transactions.IntraTeamMoves != "success") {
                context.executeExpiredIntraTeamMoves(leagueId, gameDay, keyDates, function (err, message) {
                    callback();
                })
            } else {
                // done
                callback();
            }
        }
    },

    getGameDaySchedule: function (leagueId, keyDates, callback) {
        var lid = leagueId.toString();
        query = [
            { $match: { LeagueId: lid } },
            {
                $project: {
                    Games: {
                        $filter: {
                            input: "$Games",
                            as: "game",
                            cond: { $eq: ["$$game.simpleDate", keyDates.GameDate] }
                        }
                    }
                }
            }
        ];
        Schedule.aggregate(query, function (err, schedule) {
            if (err) {
                callback("error", []);
            } else {
                callback(null, schedule[0].Games);
            }
        })
    },

    // 
    // playAllGames (for either the 1st games of the day or the 2nd games of double headers - the nightcaps )
    // note: gamesToPlay is either "game1" or "nightcap" to help with which game we're playing in double headers
    //
    playAllGames: function (leagueId, gameDay, keyDates, gameHour, gamesToPlay, autoSave, callback) {

        // first see if looking to play game1 games and if game1 games are already done!
        if (gamesToPlay == "game1" && gameDay.GamesPlayed && gameDay.GamesPlayed.status && gameDay.GamesPlayed.status !== false && gameDay.GamesPlayed.status !== "partial") {
            callback(false, "success");
            return;
        }

        // next see if nightcap games are already done (only true if game1's are already done too)
        if (gamesToPlay == "nightcap" && gameDay.GamesPlayedNC && gameDay.GamesPlayedNC.status && gameDay.GamesPlayedNC.status !== false) {
            callback(false, "success");
            return;
        }


        // next see if it's too early
        if (Number(keyDates.Hour) < Number(gameHour)) {
            callback(false, "success");
            return;
        }

        context = this;

        // set the actives one more time....
        console.log("ready to play games.. re-run setActives()");
        context.setActives2(gameDay, keyDates, function (err, messages) {
            if (err) {
                gameDay.Status = "set actives 2 failed for " + gamesToPlay;
            } else {
                gameDay.Status = "actives 2 complete for " + gamesToPlay;
            }
            console.log("setActives2 completed: " + gameDay.Status + " for " + gamesToPlay);
            if (messages) {
                gameDay.EligibilityActive2 = messages;
                var now = moment().tz("America/Los_Angeles");
                var nowFormatted = now.format("HH:mm");
                gameDay.EligibilityActive2["Completed"] = nowFormatted;
                gameDay.LastUpdate = nowFormatted;

                gameDay.markModified('EligibilityActive2');
                gameDay.save(function (err, results) {
                    var e = err;
                    console.log("completed attempt to set actives eligibility immediately before gameplay");


                    // get the schedule
                    context.getGameDaySchedule(leagueId, keyDates, function (error, sgames) {

                        // play all of today's games
                        context.playNextGame(0, leagueId, sgames, gameDay, keyDates, gamesToPlay, autoSave, function (err, gameMessage, count) {

                            if (count > 0) {
                                context.setActivesPostGame(gameDay, keyDates, function (err, messages) {
                                    if (err) {
                                        gameDay.Status = "post game failed";
                                    } else {
                                        gameDay.Status = "post game complete";
                                    }
                                    console.log("Post Game setActives completed: " + gameDay.Status);
                                    if (messages) {
                                        gameDay.EligibilityActivePostGame = messages;
                                        var now = moment().tz("America/Los_Angeles");
                                        var nowFormatted = now.format("HH:mm");
                                        gameDay.EligibilityActivePostGame["Completed"] = nowFormatted;
                                        gameDay.LastUpdate = nowFormatted;

                                        gameDay.markModified('EligibilityActivePostGame');
                                    }
                                    gameDay.save(function (err, results) {
                                        var e = err;
                                        console.log("completed attempt to set actives eligibility post game");
                                        callback(err, gameMessage);
                                    });
                                });
                            } else {
                                // count is 0, no need to re-run eligilibity
                                console.log("No nightcaps to play.  Did not run eligbility again;");
                                callback(err, gameMessage);
                            }
                        })
                    })
                })
            } else {
                // get the schedule
                context.getGameDaySchedule(leagueId, keyDates, function (error, sgames) {

                    // play all of today's games
                    context.playNextGame(0, leagueId, sgames, gameDay, keyDates, gamesToPlay, autoSave, function (err, gameMessage, count) {
                        if (count > 0) {
                            context.setActivesPostGame(gameDay, keyDates, function (err, messages) {
                                if (err) {
                                    gameDay.Status = "post game failed";
                                } else {
                                    gameDay.Status = "post game complete";
                                }
                                console.log("Post Game setActives completed: " + gameDay.Status);
                                if (messages) {
                                    gameDay.EligibilityActivePostGame = messages;
                                    var now = moment().tz("America/Los_Angeles");
                                    var nowFormatted = now.format("HH:mm");
                                    gameDay.EligibilityActivePostGame["Completed"] = nowFormatted;
                                    gameDay.LastUpdate = nowFormatted;

                                    gameDay.markModified('EligibilityActivePostGame');
                                }
                                gameDay.save(function (err, results) {
                                    var e = err;
                                    console.log("completed attempt to set actives eligibility post game");
                                    callback(err, gameMessage);
                                });
                            });
                        } else {
                            // count is 0, no need to re-run eligilibity
                            console.log("No nightcaps to play.  Did not run eligbility again;");
                            callback(err, gameMessage);
                        }
                    })
                })
            }
        })
    },

    makeGameOfficial: function (leagueId, boxscore, callback) {
        Boxscore.findOne({ _id: boxscore._id }, function (err, finalscore) {
            if (err || finalscore == null) {
                callback(true, "Server error: couldn't find game");
            } else {

                // ************** update all the stats based on this game:
                // 1) w/l records for the team in the standings for both teams
                // 2) individual player stats for both rosters
                // 3) used games for each player that played.

                var season = finalscore.Season;
                if (finalscore.Status == "Official") {
                    callback(true, "NO UPDATE. This Game is already Official.");
                } else {

                    leagueHelper.updateWonLossRecords(finalscore.LeagueId, 2017, finalscore, function (err) {
                        teamHelper.saveGameStats(finalscore, function (message) {
                            if (message != "success") {
                                callback(true, "Error accumulating player stats");

                            }
                            if (err) {
                                callback(true, "Error");
                            } else {
                                // finally, make the game official
                                finalscore.Status = "Official";
                                finalscore.save(function (err, response) {
                                    if (err) {

                                    }
                                    callback(null, "success");
                                })

                            }
                        })

                    })
                }
            }
        })
    },

    playNextGame: function (index, leagueId, schedule, gameDay, keyDates, gamesToPlay, autoSave, callback) {

        if (index >= schedule.length) {
            // check to see if all games were completed
            var success = true;

            if (gamesToPlay == "game1") {
                for (let s = 0; s < schedule.length; s++) {
                    if (schedule[s].isDoubleHeaderNC !== true) {
                        var nextId = schedule[s].gameId;
                        if (!gameDay.GamesPlayed || !gameDay.GamesPlayed[nextId] || gameDay.GamesPlayed[nextId] === false) {
                            success = false;
                            break;
                        }
                    }
                }
                if (success) {
                    if (!gameDay.GamesPlayed)
                        gameDay["GamesPlayed"] = {};
                    gameDay.GamesPlayed["status"] = "completed";
                    gameDay.GamesPlayed["Official"] = autoSave;
                    console.log("All Game1 Games Played");
                } else {
                    gameDay.GamesPlayed["status"] = "partial";
                    console.log("Some/All games failed");
                }
                var moment = require('moment-timezone');
                var now = moment().tz("America/Los_Angeles");
                var nowFormatted = now.format("HH:mm");
                gameDay.GamesPlayed["Time"] = nowFormatted;

                gameDay.markModified("GamesPlayed");
                gameDay.save(function (err, results) {
                    if (success) {

                        var nowFormatted = now.format("MMMM D, Y");
                        messageHelper.createAndSendMessageToLeague(leagueId, "Games Completed for " + nowFormatted,
                            "Today's games are now complete. Please sign in to see the final scores and standings.",
                            "Games are complete for " + nowFormatted, function (err) {
                                callback(!success, gameDay.GamesPlayed.status, 1);
                            });
                    } else {
                        callback(!success, gameDay.GamesPlayed.status, 1);
                    }
                })
            } else {
                // nightcaps
                var countNC = 0;
                for (let s = 0; s < schedule.length; s++) {
                    if (schedule[s].isDoubleHeaderNC === true) {
                        var nextId = schedule[s].gameId;
                        if (!gameDay.GamesPlayedNC || !gameDay.GamesPlayedNC[nextId] || gameDay.GamesPlayedNC[nextId] === false) {
                            success = false;
                            break;
                        } else {
                            countNC++;
                        }
                    }
                }
                if (success) {
                    if (!gameDay.GamesPlayedNC)
                        gameDay["GamesPlayedNC"] = {};
                    gameDay.GamesPlayedNC["status"] = "completed";
                    gameDay.GamesPlayedNC["NightcapGameCount"] = countNC;
                    gameDay.GamesPlayedNC["Official"] = autoSave;

                    console.log("All Nightcap Games Played");
                } else {
                    gameDay.GamesPlayedNC["status"] = "partial";
                    console.log("Some/All nightcap games failed");
                }
                var moment = require('moment-timezone');
                var now = moment().tz("America/Los_Angeles");
                var nowFormatted = now.format("HH:mm");
                gameDay.GamesPlayedNC["Time"] = nowFormatted;

                gameDay.markModified("GamesPlayedNC");
                gameDay.save(function (err, results) {
                    if (success && countNC > 0) {
                        var nowFormatted = now.format("MMMM D, Y");
                        messageHelper.createAndSendMessageToLeague(leagueId, "Nightcap Games Completed for " + nowFormatted,
                            "Today's doubleheader nightcap games are now complete. Please sign in to see the final scores and standings.",
                            "Doublheader nightcap games are complete for " + nowFormatted, function (err) {
                                callback(!success, gameDay.GamesPlayed.status, 1);
                            });
                    } else {
                        callback(!success, gameDay.GamesPlayed.status, 1);
                    }
                })
            }


        } else {
            // not done with the iterator:
            // play the next game1 or nightcap game.
            var context = this;

            var game = schedule[index];

            // only play this next game if it's the right type of game for gamesToPlay variable
            if ((gamesToPlay == "game1" && game.isDoubleHeaderNC !== true)
                || (gamesToPlay == "nightcap" && game.isDoubleHeaderNC === true)) {
                game.leagueId = leagueId.toString();;
                var gameRange = { From: 20170301, To: 20170510 };
                if (keyDates.From) {
                    gameRange.From = keyDates.From;
                }
                if (keyDates.To) {
                    gameRange.To = keyDates.To;
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

                // insure the correct object exists...
                if (!gameDay._doc.GamesPlayed) {
                    gameDay._doc["GamesPlayed"] = {};
                }

                if (gamesToPlay == "nightcap" && !gameDay._doc.GamesPlayedNC) {
                    gameDay._doc["GamesPlayedNC"] = {};
                }


                if ((gamesToPlay == "game1" && (!gameDay.GamesPlayed[game.gameId] || gameDay.GamesPlayed[game.gameId] !== true))
                    || (gamesToPlay == "nightcap" && (!gameDay.GamesPlayedNC[game.gameId] || gameDay.GamesPlayedNC[game.gameId] !== true))) {
                    if (gamesToPlay == "game1")
                        gameDay._doc["GamesPlayed"][game.gameId] = false;
                    else
                        gameDay._doc["GamesPlayedNC"][game.gameId] = false;

                    // play the game now...
                    sim.getLineupsAndPlayGame(game, gameRange, Override, function (err, boxscore, visitTeam, visitRoster, homeTeam, homeRoster) {
                        if (err) {
                            // something went wrong with the game...
                            if (gamesToPlay == "game1") {
                                gameDay.GamesPlayed[game.gameId] = false;
                                console.log("GAME FAILED: " + game.gameId);
                                gameDay.GamesPlayed["status"] = "partial";
                                gameDay.markModified("GamesPlayed");
                            } else {
                                gameDay.GamesPlayedNC[game.gameId] = false;
                                console.log("NIGHT CAP GAME FAILED: " + game.gameId);
                                gameDay.GamesPlayedNC["status"] = "partial";
                                gameDay.markModified("GamesPlayedNC");
                            }
                            gameDay.save(function (err, results) {
                                context.playNextGame(++index, leagueId, schedule, gameDay, keyDates, gamesToPlay, autoSave, callback);
                            })
                        } else {

                            // store the boxscore now.
                            var teamData = {};
                            teamData["visitTeam"] = visitTeam;
                            teamData["homeTeam"] = homeTeam;
                            teamData["visitRoster"] = visitRoster;
                            teamData["homeRoster"] = homeRoster;

                            // this is forced to null so never executes...
                            if (Override) {
                                function putInDecision(boxscore, decision, letter) {

                                    if (Override.Decision[decision] && Override.Decision[decision] != "") {
                                        var found = false;
                                        for (var p = 0; p < boxscore.Home.Pitchers.length; p++) {
                                            if (boxscore.Home.Pitchers[p].FullName == Override.Decision[decision]) {
                                                found = true;
                                                boxscore.summary.Decision[decision] = Override.Decision[decision];
                                                boxscore.summary.Decision[decision + "Id"] = boxscore.Home.Pitchers[p].PlayerId;
                                                if (boxscore.Home.Pitchers[p].Decision == "BS" && letter == "L") {
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
                                                    if (boxscore.Visit.Pitchers[p].Decision == "BS" && letter == "L") {
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
                                    if (Override.Decision.Win && Override.Decision.Win != "") {
                                        // first clear out any previous winners...
                                        for (var p = 0; p < boxscore.Home.Pitchers.length; p++) {
                                            boxscore.Home.Pitchers[p].Decision = "";
                                        }
                                        for (var p = 0; p < boxscore.Visit.Pitchers.length; p++) {
                                            boxscore.Visit.Pitchers[p].Decision = "";
                                        }
                                        putInDecision(boxscore, "Win", "W");
                                        putInDecision(boxscore, "Blown", "BS"); // do this first in case he also lost.
                                        putInDecision(boxscore, "Loss", "L");
                                        putInDecision(boxscore, "Save", "SV");
                                        putInHold(boxscore, Override.Decision.Hold);
                                    }
                                }
                            } // end of override...

                            // play the game.
                            sim.storeBoxscore(schedule[index], boxscore, teamData, null, function (err, boxScoreDocument) {

                                if (!err || err.type == "Was Official") {
                                    if (gamesToPlay == "game1")
                                        gameDay.GamesPlayed[game.gameId] = true;
                                    else
                                        gameDay.GamesPlayedNC[game.gameId] = true;
                                    if (!err)
                                        console.log("Game Played: " + game.gameId);
                                    else
                                        console.log("This game was already official: " + game.gameId)
                                } else {
                                    if (gamesToPlay == "game1")
                                        gameDay.GamesPlayed[game.gameId] = false;
                                    else
                                        gameDay.GamesPlayedNC[game.gameId] = false;
                                    console.log("GAME FAILED: " + game.gameId);
                                }
                                if (gamesToPlay == "game1") {
                                    gameDay.GamesPlayed["status"] = "partial";
                                    gameDay.markModified("GamesPlayed");
                                } else {
                                    gameDay.GamesPlayedNC["status"] = "partial";
                                    gameDay.markModified("GamesPlayedNC");
                                }
                                gameDay.markModified("GamesPlayedNC");
                                gameDay.save(function (err, results) {

                                    // if autoscore == true, then make the game official and accumulate the stats.
                                    if (autoSave == true) {
                                        context.makeGameOfficial(leagueId, boxScoreDocument, function (err, response) {
                                            context.playNextGame(++index, leagueId, schedule, gameDay, keyDates, gamesToPlay, autoSave, callback);
                                        })

                                    } else {
                                        context.playNextGame(++index, leagueId, schedule, gameDay, keyDates, gamesToPlay, autoSave, callback);
                                    }
                                })

                            });
                        }

                    });
                } else {
                    // this game already played
                    // do the next game
                    context.playNextGame(++index, leagueId, schedule, gameDay, keyDates, gamesToPlay, autoSave, callback);
                }
            } else {
                // skip this game as it's the wrong type of game
                // try the next game
                context.playNextGame(++index, leagueId, schedule, gameDay, keyDates, gamesToPlay, autoSave, callback);
            }
        }
    }
}