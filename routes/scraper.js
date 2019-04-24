var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var moment = require('moment');

var accessHelper = require('../helpers/accessHelper');
var utils = require('../helpers/utils');
var rosterHelper = require('../helpers/rosterHelper');
var ftp = require('../helpers/ftp');
var cloudstorage = require('../helpers/cloudstorage');

var mongoose = require('mongoose');
var Stat = mongoose.model('Stat');

var Pitching = mongoose.model('Pitching');
var Baserunning = mongoose.model('Baserunning');
var Fielding = mongoose.model('Fielding');
var Batting = mongoose.model('Batting');
var YtdFielding = mongoose.model('YtdFielding');

var PitchingMinors = mongoose.model('PitchingMinors');
var BaserunningMinors = mongoose.model('BaserunningMinors');
var FieldingMinors = mongoose.model('FieldingMinors');
var BattingMinors = mongoose.model('BattingMinors');
var YtdFieldingMinors = mongoose.model('YtdFieldingMinors');

var Player = mongoose.model('Player');
var FreeAgent = mongoose.model('FreeAgent');
var Team = mongoose.model('Team');
var Roster = mongoose.model('Roster');
var League = mongoose.model('League');
var PlayerDaily = mongoose.model('PlayerDaily');
var Calendar = mongoose.model('Calendar');

var State = mongoose.model('State');
var YtdStats = mongoose.model('YtdStats');
var MasterPlayer = mongoose.model('MasterPlayer');
var Content = mongoose.model('Content');
var Schedule = mongoose.model('Schedule');
var TempMasterPlayers = mongoose.model('TempMasterPlayers');

var csvHelper = require('../helpers/csvHelper');
var importHelper = require('../helpers/importHelper');
var TeamInfo = require('../helpers/teamInfo');

var fs = require('fs');

var Client = require('node-rest-client').Client;
var client = new Client();

var request = require('request');
var cheerio = require('cheerio');

var playerHelper = require('../helpers/playerHelper');

var http = require('http');
var moment = require('moment');

var Browser = require('zombie');
var path = require('path');

var multer = require('multer'); 
var isProd = false;
var isDev = false;
var filePath = "/tmp/";

var storage = multer.diskStorage({ //multers disk storage settings
    destination: function (req, file, cb) {
        if( isProd ) {
        cb(null, filePath); // path.join(__dirname, "../uploads/"))
        } else if (isDev ) {
            
            cb(null, filePath); // path.join(__dirname, "../uploads/"))
        } else {
            filePath =  path.join(__dirname, "../uploads/")
            cb(null, filePath);            
        }
    },
    filename: function (req, file, cb) {
        var datetimestamp = Date.now();
        cb(null, file.fieldname + '-' + datetimestamp + '.' + file.originalname.split('.')[file.originalname.split('.').length - 1])
    }
});
var upload = multer({ //multer settings
    storage: storage
})


function checkDailyGames( date, res ) {
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate())).slice(-2);

    var message = "For date: " + dateString;
    var isError = false;
    Pitching.findOne({ GameDate: dateString }, function (err, stat) {
        if (err) {
            message += " pitching data NOT found.";
            isError = true;
            console.log(err)
        }
        else if (!stat || !stat.Stats || stat.Stats.length==0) {
            message += " pitching data EMPTY. Please report to dev team";
            isError = true;
        } else {
            message += " pitching games found.";
        }
        Batting.findOne({ GameDate: dateString },  function (err, stat) {
            if (err) {
                message += ". Batting data NOT found.";
                isError = true;
                console.log(err)
            }
            else if (!stat || !stat.Stats || stat.Stats.length==0) {
                message += ". Batting data EMPTY. Please report to dev team";
                isError = true;
            } else {
                message += ". Batting games found.";
            }
            Baserunning.findOne({ GameDate: dateString },  function (err, stat) {
                if (err) {
                    message += ". Baserunning data NOT found.";
                    isError = true;
                    console.log(err)
                }
                else if (!stat || !stat.Stats || stat.Stats.length==0) {
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
                    else if (!stat || !stat.Stats || stat.Stats.length == 0) {
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
                        else if (!stat || !stat.Stats || stat.Stats.length == 0) {
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
                            else if (!stat || !stat.Stats || stat.Stats.length == 0) {
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
                                else if (!stat || !stat.Stats || stat.Stats.length == 0) {
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
                                    else if (!stat || !stat.Stats || stat.Stats.length == 0) {
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
                                    res.status(200).json({ status: 200, msg: message });
                                });
                            });
                        });
                    });
                });
            });
        });
    });

}

// ****************************************** GET BIS DATA FOR MAJOR LEAGUE GAMES *************************
function getYTDFielding(date, res, callback) {
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate())).slice(-2);

    if (dateString == "20170705") {
        //res.status(200).json({ status: 200, msg: "complete" });
        //return;
    }

    var season = date.getFullYear();
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
            if( dateString == "20180329")
                fileName = fixFilename;

            ftp.syncFile(fileName, function (data) {
                if (!data || data.length == 0) {
                    if (callback) {
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "YTD Fielding FTP error" });
                    }

                } else  {
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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "ytd fielding complete" });
                    }
                });
            }
            });

        }
        else {
            // date = new Date(date.setDate(date.getDate() + 1));

            if (callback) {
                callback(date, res, callback)
            }
            else {
                res.status(200).json({ status: 200, msg: "ytd fielding already loaded" });
            }
        }
    }).limit(2);
}

function getPitching(date, res, callback) {
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate())).slice(-2);

    if (dateString == "20170705") {
        //res.status(200).json({ status: 200, msg: "complete" });
        //return;
    }

    var season = date.getFullYear();
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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "Pitching FTP Error" });
                    }
                } else  {
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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "complete" });
                    }
                });
            }
            });

        }
        else {
           // date = new Date(date.setDate(date.getDate() + 1));

            if (callback) {
                callback(date, res, callback)
            }
            else {
                res.status(200).json({ status: 200, msg: "pitching already loaded" });
            }
        }
    });
}

function getBaserunning(date, res, callback) {
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate())).slice(-2);

    if (dateString == "20170705") {
        // res.status(200).json({ status: 200, msg: "complete" });
        // return;
    }


    var season = date.getFullYear();

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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "Baserunning FTP Error" });
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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "completed last step" });
                    }
                });
            }
            });

        }
        else {
          //  date = new Date(date.setDate(date.getDate() + 1));

            if (callback) {
                callback(date, res, callback)
            }
            else {
                res.status(200).json({ status: 200, msg: "complete last step" });
            }
        }
    });
}

function getBatting(date, res, callback) {
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate())).slice(-2);

    if (dateString == "20170705") {
        //res.status(200).json({ status: 200, msg: "complete" });
        // return;
    }


    var season = date.getFullYear();

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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "Batting FTP Error" });
                    }
                } else {
                    stat.Stats = data;
                    Batting.create(stat, function (err, result) {
                        if (err) {
                            console.log(err)
                        }
                        else {
                            console.log(dateString);
                        }
                       // date = new Date(date.setDate(date.getDate() + 1));

                        if (callback) {
                            callback(date, res, callback)
                        }
                        else {
                            res.status(200).json({ status: 200, msg: "complete" });
                        }
                    });
                }

            });

        }
        else {
           // date = new Date(date.setDate(date.getDate() + 1));

            if (callback) {
                callback(date, res, callback)
            }
            else {
                res.status(200).json({ status: 200, msg: "batting complete" });
            }
        }
    });
}

function putZoneDataInFielding(dateString, playerStats, callback) {
    // get two most recent games for this player and create 
    // Outs Out of Zone and Missed Balls in Zone for the fielding data.
    // get the two most recent  documents...

    YtdFielding.find({GameDate: {$lte: dateString}}, function (err, fStats) {
        if (err || !fStats || fStats.length == 0 || !playerStats.Stats) {
            callback();
        } else {
            for (let p = 0; p < playerStats.Stats.length; p++) {
                var nextPlayer = playerStats.Stats[p];
                if( nextPlayer.LastName == "Ozuna") {
                    var ballz = nextPlayer.BallsInZone;
                }
                var oldStats = null;
                var newStats = null;

                // 0th is most recent
                for (var f0 = 0; f0 < fStats[0].Stats.length; f0++) {
                    // match the player!
                    if (fStats[0].Stats[f0].MLBId == nextPlayer.MLBId) {
                        var newStats = fStats[0].Stats[f0];
                        break;
                    }
                }
                if (fStats.length == 2) {
                    for (var f1 = 0; f1 < fStats[1].Stats.length; f1++) {
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
                if( nextPlayer.LastName == "Molina") {
                    var molina = true;
                }
                if( oldStats ) {
                    ballsInZone = oldStats.BallsInZone;
                    outsOutOfZone = oldStats.OutsOutOfZone;
                    missedBallsInZone = oldStats.MissedBallsInZone;
                    cFraming = oldStats.CFramingRuns != null ? oldStats.CFramingRuns : 0;
                    cBlocking = oldStats.CBlockingRuns != null ? oldStats.CBlockingRuns : 0;


                    if( newStats) {
                        // have both, subtract them.
                            ballsInZone = newStats.BallsInZone - oldStats.BallsInZone;
                        if( ballsInZone < 0)
                            ballsInZone = newStats.BallsInZone;
                   
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
                      cFraming = newStats.CFramingRuns != null ? newStats.CFramingRuns : cFraming ;
                      //  cBlocking = newStats.cBlockingRuns != null ? newStats.CBlockingRuns - cBlocking : cBlocking;;  
                      cBlocking = newStats.CBlockingRuns != null ? newStats.CBlockingRuns : cBlocking;

                    }
                } else {
                    if( newStats) {
                        // if only new stats, use them.
                        ballsInZone = newStats.BallsInZone;
                        outsOutOfZone = newStats.OutsOutOfZone;
                        missedBallsInZone = newStats.MissedBallsInZone;    
                        cFraming = newStats.CFramingRuns != null ? newStats.CFramingRuns  : cFraming;  
                        cBlocking = newStats.CBlockingRuns != null ? newStats.CBlockingRuns  : cBlocking;                     
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

}

function getFielding(date, res, callback) {
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate())).slice(-2);

    if (dateString == "20170705") {
        // res.status(200).json({ status: 200, msg: "complete" });
        // return;
    }


    var season = date.getFullYear();
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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "Fielding FTP error" });
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

                        putZoneDataInFielding(dateString, newPlayerStats, function () {

                            newPlayerStats.markModified("Stats");
                            newPlayerStats.save(function (err, result) {
                                if (callback) {
                                    callback(date, res, callback)
                                }
                                else {
                                    res.status(200).json({ status: 200, msg: "complete" });
                                }
                            })
                        });
                    });
                }
            });

        }
        else {
       //     date = new Date(date.setDate(date.getDate() + 1));

            putZoneDataInFielding(dateString, playerStats, function () {
                playerStats.markModified("Stats");
                playerStats.save( function(err, result){        
            if (callback) {
                callback(date, res, callback)
            }
            else {
                res.status(200).json({ status: 200, msg: "fielding complete" });
            }
        });
    });
        }
    });
}

//
// ****************************************** GET BIS DATA FOR MINOR LEAGUE GAMES *************************
//
function getYTDFieldingMinors(date, res, callback) {
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate())).slice(-2);

    if (dateString == "20170705") {
        //res.status(200).json({ status: 200, msg: "complete" });
        //return;
    }

    var season = date.getFullYear();
    YtdFieldingMinors.findOne({GameDate: {$lte: dateString} }, function (err, stat) {
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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "YTD Fielding Minors FTP error" });
                    }

                } else  {
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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "ytd fielding minors complete" });
                    }
                });
            }
            });

        }
        else {
            // date = new Date(date.setDate(date.getDate() + 1));

            if (callback) {
                callback(date, res, callback)
            }
            else {
                res.status(200).json({ status: 200, msg: "ytd fielding already loaded" });
            }
        }
    }).limit(2);
}

function getPitchingMinors(date, res, callback) {
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate())).slice(-2);

    if (dateString == "20170705") {
        //res.status(200).json({ status: 200, msg: "complete" });
        //return;
    }

    var season = date.getFullYear();
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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "Pitching Minors FTP Error" });
                    }
                } else  {
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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "complete" });
                    }
                });
            }
            });

        }
        else {
         //   date = new Date(date.setDate(date.getDate() + 1));

            if (callback) {
                callback(date, res, callback)
            }
            else {
                res.status(200).json({ status: 200, msg: "pitching already loaded" });
            }
        }
    });
}

function getBaserunningMinors(date, res, callback) {
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate())).slice(-2);

    if (dateString == "20170705") {
        // res.status(200).json({ status: 200, msg: "complete" });
        // return;
    }


    var season = date.getFullYear();

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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "Baserunning Minors FTP Error" });
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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "complete" });
                    }
                });
            }
            });

        }
        else {
         //   date = new Date(date.setDate(date.getDate() + 1));

            if (callback) {
                callback(date, res, callback)
            }
            else {
                res.status(200).json({ status: 200, msg: "all complete" });
            }
        }
    });
}

function getBattingMinors(date, res, callback) {
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate())).slice(-2);

    if (dateString == "20170705") {
        //res.status(200).json({ status: 200, msg: "complete" });
        // return;
    }


    var season = date.getFullYear();

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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "Batting Minors FTP Error" });
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
                     //   date = new Date(date.setDate(date.getDate() + 1));

                        if (callback) {
                            callback(date, res, callback)
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
                callback(date, res, callback)
            }
            else {
                res.status(200).json({ status: 200, msg: "batting complete" });
            }
        }
    });
}


function getFieldingMinors(date, res, callback) {
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate())).slice(-2);

    if (dateString == "20170705") {
        // res.status(200).json({ status: 200, msg: "complete" });
        // return;
    }


    var season = date.getFullYear();
    FieldingMinors.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
        if (!stat) {
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
                        callback(date, res, callback)
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "Fielding Minors FTP Error" });
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
                    putZoneDataInFielding(dateString, newPlayerStats, function () {

                        newPlayerStats.markModified("Stats");
                        newPlayerStats.save(function (err, result) {
                            if (callback) {
                                callback(date, res, callback)
                            }
                            else {
                                res.status(200).json({ status: 200, msg: "complete" });
                            }
                        })
                    });

                });
            }
            });

        }
        else {
       //     date = new Date(date.setDate(date.getDate() + 1));

            if (callback) {
                callback(date, res, callback)
            }
            else {
                res.status(200).json({ status: 200, msg: "fielding complete" });
            }
        }
    });
}

// ****************************************** router entry points ***********************************
// unused...
router.get('/api/scraper/stats/historical/pitching', function (req, res) {
    var date = new Date("04/03/2017");
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate())).slice(-2);

    getPitching(date, res, getPitching);
});


router.post('/api/scraper/stats/date/check', function (req, res) {

    var momentDate = moment(req.body.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);
    checkDailyGames(date, res, null);
});


router.post('/api/scraper/stats/date/pitching', function (req, res) {

    var momentDate = moment(req.body.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);
    getPitching(date, res, null);
});


router.post('/api/scraper/stats/date/pitchingMinors', function (req, res) {

    var momentDate = moment(req.body.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);
    getPitchingMinors(date, res, null);
});

router.post('/api/scraper/stats/date/getBISData', function (req, res) {

    var momentDate = moment(req.body.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);

    // start with the major leagues...
    getPitching(date, res, function (date, res, callback) {
        getBatting(date, res, function (date, res, callback) {
            getYTDFielding(date, res, function (date, res, callback) {
                getFielding(date, res, function (date, res, callback) {
                    getBaserunning(date, res, function (date, res, callback) {

                        // and now the minors...
                        getPitchingMinors(date, res, function (date, res, callback) {
                            getBattingMinors(date, res, function (date, res, callback) {
                                getYTDFieldingMinors(date, res, function (date, res, callback) {
                                    getFieldingMinors(date, res, function (date, res, callback) {
                                        getBaserunningMinors(date, res, null);
                                    })
                                })
                            })
                        });
                    })
                })
            })
        })
    });
});

router.post('/api/scraper/stats/date/getBISMinorLeagueData', function (req, res) {

    var momentDate = moment(req.body.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);
    getPitchingMinors(date, res, function(date, res, callback){
        getBattingMinors( date,res, function(date, res, callback){
            getFieldingMinors( date, res, function(date, res, callback){
                getBaserunningMinors( date, res, null);
            })
        } )
    });

});


router.post('/api/scraper/stats/date/batting', function (req, res) {

    var momentDate = moment(req.body.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);
    getBatting(date, res, null);
});

router.post('/api/scraper/stats/date/battingMinors', function (req, res) {

    var momentDate = moment(req.body.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);
    getBattingMinors(date, res, null);
});

router.post('/api/scraper/stats/date/baserunning', function (req, res) {

    var momentDate = moment(req.body.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);
    getBaserunning(date, res, null);
});

router.post('/api/scraper/stats/date/baserunningMinors', function (req, res) {

    var momentDate = moment(req.body.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);
    getBaserunningMinors(date, res, null);
});

router.post('/api/scraper/stats/date/fielding', function (req, res) {

    var momentDate = moment(req.body.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);
    getFielding(date, res, null);
});

router.post('/api/scraper/stats/date/fieldingMinors', function (req, res) {

    var momentDate = moment(req.body.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);
    getFieldingMinors(date, res, null);
});

router.post('/api/scraper/stats/date/YTDfielding', function (req, res) {

    var momentDate = moment(req.body.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);
    getYTDFielding(date, res, null);
});

router.post('/api/scraper/stats/date/convertGameStatsToPlayerStats', function (req, res) {
    var momentDate = moment(req.body.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);
    importHelper.savePlayerStats( function(msg ){
        res.status(200).json({ status: 200, msg: "complete" });
    })
});

router.get('/api/scraper/stats/master', function (req, res) {

    var stream = fs.createReadStream('./data/master.csv');

    csvHelper.parse(stream, function (data) {

        for (var i = 0; i < data.length; i++) {
            var player = data[i];

            Player.create(player);
        }

        res.status(200).json({ status: 200, msg: "complete" });
    });
});

router.get('/api/scraper/stats/freeagents', function (req, res) {

    var stream = fs.createReadStream('./data/freeagents.csv');

    csvHelper.parse(stream, function (data) {

        for (var i = 0; i < data.length; i++) {
            var player = data[i];
            player["LeagueId"] = "mlb";
            player["RPlayer"] = {};

            FreeAgent.create(player);
        }

        res.status(200).json({ status: 200, msg: "complete" });
    });
});


router.get('/api/scraper/stats/pitching/latest', function (req, res) {
    var date = new Date();
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate() - 1)).slice(-2);


    var season = date.getFullYear();

    Pitching.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
        if (err) {
            res.status(500).json({ status: 500, error: err });
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
                stat.Stats = data;
                Pitching.create(stat, function (err, result) {
                    if (err) {
                        res.status(500).json({ status: 500, error: err.errmsg });
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "complete" });
                    }
                });
            });

        }
        else {
            res.status(200).json({ status: 201, msg: "complete" });
        }

    });
});

router.get('/api/scraper/stats/baserunning/latest', function (req, res) {
    var date = new Date();
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate() - 1)).slice(-2);

    var season = date.getFullYear();

    Baserunning.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
        if (err) {
            res.status(500).json({ status: 500, error: err });
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
                stat.Stats = data;
                Baserunning.create(stat, function (err, result) {
                    if (err) {
                        res.status(500).json({ status: 500, error: err.errmsg });
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "complete" });
                    }
                });
            });

        }
        else {
            res.status(200).json({ status: 201, msg: "complete" });
        }

    });
});

router.get('/api/scraper/stats/batting/latest', function (req, res) {
    var date = new Date();
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate() - 1)).slice(-2);

    var season = date.getFullYear();

    Batting.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
        if (err) {
            res.status(500).json({ status: 500, error: err });
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
                stat.Stats = data;
                Batting.create(stat, function (err, result) {
                    if (err) {
                        res.status(500).json({ status: 500, error: err.errmsg });
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "complete" });
                    }
                });
            });

        }
        else {
            res.status(200).json({ status: 201, msg: "complete" });
        }

    });
});

router.get('/api/scraper/stats/fielding/latest', function (req, res) {
    var date = new Date();
    var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate() - 1)).slice(-2);

    var season = date.getFullYear();

    Fielding.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
        if (err) {
            res.status(500).json({ status: 500, error: err });
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
            ftp.syncFile("Fielding_" + dateString + ".csv", function (data) {
                stat.Stats = data;
                Fielding.create(stat, function (err, result) {
                    if (err) {
                        res.status(500).json({ status: 500, error: err.errmsg });
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "complete" });
                    }
                });
            });

        }
        else {
            res.status(200).json({ status: 201, msg: "complete" });
        }

    });
});


router.get('/api/scraper/teams', function (req, res) {
    var url = "https://erikberg.com/mlb/teams.json";
    var output = [];

    var args = {
        headers: {
            "Content-Type": "application/json",
            "User-Agent": "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
        }
    };

    client.get(url, args, function (data, response) {
        for (var i = 0; i < data.length; i++) {
            Team.create(data[i], function (err, result) {
                if (err) {
                    console.log(err.errmsg);
                }
            });
        }
        res.status(200).json({ status: 200, data: data });
    });
});

function updateRoster(index, callback) {
    if (index < urls.length) {
        utils.scrapeUrl(urls[index].url, function (error, response, html) {
            if (!error) {
                var $ = cheerio.load(html);

                var tables = $(".data.roster_table");
                var output = [];
                for (var h = 0; h < tables.length; h++) {

                    var data = tables[h];
                    var rows = $(data).find("tr");

                    if (urls[index]) {
                        for (var i = 1; i < rows.length; i++) {
                            output.push({
                                Team: urls[index].team,
                                Abbreviation: urls[index].abbr,
                                Number: $(rows[i]).find(".dg-jersey_number").text(),
                                Image: $(rows[i]).find(".dg-player_headshot img").attr("src"),
                                Name: $(rows[i]).find(".dg-name_display_first_last a").text(),
                                MlbId: $(rows[i]).find(".dg-name_display_first_last a").attr("href").split('/')[2],
                                Inactive: $(rows[i]).find(".dg-name_display_first_last a").hasClass("player-inactive"),
                                NotOnRoster: $(rows[i]).find(".dg-name_display_first_last a").hasClass("player-noton40man")
                            });
                        }
                    }
                    else {
                        callback();
                    }
                }

                rosterHelper.updateTeamRoster(output, function () {
                    updateRoster(++index, callback)
                });
            }
            else {
                callback();
            }
        });
    }
    else {
        callback();
    }
}

router.get('/api/scraper/roster', function (req, res) {
    //var url = "http://m.angels.mlb.com/ana/roster/40-man/";
    urls = [
        { url: "http://m.mlb.com/la/roster/40-man/", abbr: "LAN", team: "Dodgers" },
        { url: "http://m.mlb.com/ari/roster/40-man/", abbr: "ARI", team: "Diamondbacks" },
        { url: "http://m.mlb.com/atl/roster/40-man/", abbr: "ATL", team: "Braves" },
        { url: "http://m.mlb.com/bal/roster/40-man/", abbr: "BAL", team: "Orioles" },
        { url: "http://m.mlb.com/bos/roster/40-man/", abbr: "BOS", team: "Red Sox" },
        { url: "http://m.mlb.com/chc/roster/40-man/", abbr: "CHN", team: "Cubs" },
        { url: "http://m.mlb.com/cws/roster/40-man/", abbr: "CHA", team: "White Sox" },
        { url: "http://m.mlb.com/cin/roster/40-man/", abbr: "CIN", team: "Reds" },
        { url: "http://m.mlb.com/cle/roster/40-man/", abbr: "CLE", team: "Indians" },
        { url: "http://m.mlb.com/col/roster/40-man/", abbr: "COL", team: "Rockies" },
        { url: "http://m.mlb.com/det/roster/40-man/", abbr: "DET", team: "Tigers" },
        { url: "http://m.mlb.com/hou/roster/40-man/", abbr: "HOU", team: "Astros" },
        { url: "http://m.mlb.com/kc/roster/40-man/", abbr: "KC", team: "Royals" },
        { url: "http://m.mlb.com/la/roster/40-man/", abbr: "LAA", team: "Angels" },
        { url: "http://m.mlb.com/nym/roster/40-man/", abbr: "NYN", team: "Mets" },
        { url: "http://m.mlb.com/nyy/roster/40-man/", abbr: "NYA", team: "Yankees" },
        { url: "http://m.mlb.com/mil/roster/40-man/", abbr: "MIL", team: "Brewers" },
        { url: "http://m.mlb.com/mia/roster/40-man/", abbr: "MIA", team: "Marlins" },
        { url: "http://m.mlb.com/min/roster/40-man/", abbr: "MIN", team: "Twins" },
        { url: "http://m.mlb.com/oak/roster/40-man/", abbr: "OAK", team: "Athelics" },
        { url: "http://m.mlb.com/phi/roster/40-man/", abbr: "PHI", team: "Phillies" },
        { url: "http://m.mlb.com/stl/roster/40-man/", abbr: "STL", team: "Cardinals" },
        { url: "http://m.mlb.com/pit/roster/40-man/", abbr: "PIT", team: "Pirates" },
        { url: "http://m.mlb.com/sd/roster/40-man/", abbr: "SD", team: "Padres" },
        { url: "http://m.mlb.com/sf/roster/40-man/", abbr: "SF", team: "Giants" },
        { url: "http://m.mlb.com/sea/roster/40-man/", abbr: "SEA", team: "Mariners" },
        { url: "http://m.mlb.com/tb/roster/40-man/", abbr: "TB", team: "Rays" },
        { url: "http://m.mlb.com/tex/roster/40-man/", abbr: "TEX", team: "Rangers" },
        { url: "http://m.mlb.com/tor/roster/40-man/", abbr: "TOR", team: "Jays" },
        { url: "http://m.mlb.com/oak/roster/40-man/", abbr: "WAS", team: "Nationals" },
    ];


    updateRoster(0, function () {
        res.status(200).json({ status: 200, msg: "complete" });
    });

});


router.get('/api/scraper/affiliations', function (req, res) {
    var url = "http://www.milb.com/milb/info/affiliations.jsp";

    utils.scrapeUrl(url, function (error, response, html) {
        if (!error) {
            var $ = cheerio.load(html);

            var clubs = $(".clubClassList");
            var output = [];
            for (var i = 1; i < clubs.length; i++) {
                var club = $(clubs[i]);
                var parent = $(club.closest("tbody").find("tr")[0]).text().trim();
                var league = $(club.find("li")[0]).text().replace("Roster", "").trim();
                output.push({
                    Parent: parent,
                    Team: $(club.find("a")[0]).text(),
                    TeamUrl: $(club.find("a")[0]).attr("href"),
                    RosterUrl: $(club.find("a")[1]).attr("href"),
                    League: league.substring(league.indexOf("(")),
                    Id: $(club.find("a")[1]).attr("href").substring($(club.find("a")[1]).attr("href").indexOf("cid=")).replace("cid=", "")
                });
            }
            scrapeRosters(0, output, function (err, rosters) {
                res.status(200).json({ status: 200, msg: "complete", rosters: rosters });
            });
        }
        else {
            res.status(200).json({ status: 200, msg: "error", error: error });
        }
    });
});

function scrapeRosters(index, rosters, callback) {
    if (index == rosters.length || index == 1) {
        callback(null, rosters);
    }
    else {
         
        //casper.start("http://www.milb.com/roster/index.jsp?sid=t" + rosters[index].Id);
        //casper.userAgent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.106 Safari/537.36");


        //casper.waitFor(function check() {
        //    return this.evaluate(function () {
        //        return document.querySelectorAll('#rosterPitchers table tbody tr').length > 2;
        //    });
        //}, function then() {
        //    debugger;
        //});

        var browser = new Browser();
        browser.userAgent = 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36';
        //browser.active
        browser.fetch("http://www.milb.com/roster/index.jsp?sid=t" + rosters[index].Id)
            .then(function (response) {
                console.log('Status code:', response.status);
                if (response.status === 200)
                    return response.text();
            })
            .then(function (text) {
                console.log('Document:', text);
                console.log(browser.html("#main"));
            })
            .catch(function (error) {
                console.log('Network error');
            });
        
        //utils.scrapeUrl("http://www.milb.com/roster/index.jsp?sid=t" + rosters[index].Id, function (error, response, html) {
        //    if (!error) {
        //        var $ = cheerio.load(html);
        //        var output = [];
        //        var pitchers = $("#rosterPitchers table tbody tr");
        //        for (var i = 1; i < pitchers.length; i++) {
        //            var pitcher = $(pitchers[i]);
        //            output.push({
        //                Number: "",
        //                Name: "",
        //                Pos: "",
        //                Bat: "",
        //                Thw: "",
        //                Ht: "",
        //                Wt: "",
        //                DOB: "",
        //                Status: "",
        //                Mlb40Man: false
        //            });
        //        }
        //        rosters[index].Pitchers = output;

        //        scrapeRosters(++index, rosters, callback);
        //    }
        //    else {
        //        callback(error, rosters);
        //    }
        //});
         
    }
}

router.get('/api/scraper/front-office', function (req, res) {
    var url = "http://mlb.mlb.com/team/front_office.jsp?c_id=" + req.query.id;
    //var url = "http://m.angels.mlb.com/ana/roster/";


    utils.scrapeUrl(url, function (error, response, html) {
        if (!error) {
            var $ = cheerio.load(html);

            var rows = $("#front_office_list dt");
            var output = [];


            for (var i = 1; i < rows.length; i++) {
                output.push({
                    Name: $(rows[i]).find("dt a").html(),
                    Role: $(rows[i]).next().text(),
                    Team: $(".logobar__logo img").attr("alt").split(' ')[0]
                });
            }


            res.status(200).json({ status: 200, count: output.length, msg: "complete", data: output });
        }
    });
});

router.get('/api/scraper/lineup', function (req, res) {
    var url = "http://www.espn.com/mlb/team/lineup/_/name/ari";


    utils.scrapeUrl(url, function (error, response, html) {
        if (!error) {
            var $ = cheerio.load(html);

            var tables = $(".tablehead");
            var rows = tables[0];
            var output = [];

            // 2017 lineup
            for (var i = 1; i < rows.length; i++) {
                output.push({
                    Name: $(rows[i]).find("dt a").html(),
                    Role: $(rows[i]).next().text(),
                    Team: $(".logobar__logo img").attr("alt").split(' ')[0]
                });
            }

            // weekly lineup



            res.status(200).json({ status: 200, count: output.length, msg: "complete", data: output });
        }
    });
});



router.get('/api/scraper/roster/players', function (req, res) {
    rosterHelper.saveFortyManLineup({ _id: req.query.lid }, { _id: req.query.tid, abbreviation: req.query.abbv }, function (err, lineups) {
        if (err) {
            console.log(err.errmsg);
        }
        else {
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
                    rosterHelper.getPlayerData(req.query.lid, req.query.tid, league.FortyMan);
                }
                else {
                    res.status(200).json({ status: 200, msg: "success", roster: [] });
                }
            });
        }
    });
});

var urls = [];

function scrapeSalary(index, callback) {
    var context = this;
    if (index < urls.length) {
        utils.scrapeUrl(urls[index].url, function (error, response, html) {
            if (!error) {
                var $ = cheerio.load(html);

                var tables = $("table");

                var output = {
                    Team: urls[index].team,
                    Abbreviation: urls[index].abbr,
                    Active: getPlayerDataFromTable($, "Active", $($(tables)[0]).find(".datatable tr")),
                    Injured: getPlayerDataFromTable($, "Injured", $($(tables)[1]).find(".datatable tr")),
                    Retained: getPlayerDataFromTable($, "Retained", $($(tables)[2]).find(".datatable tr")),
                    Minors: getPlayerDataFromTable($, "Minors", $($(tables)[3]).find(".datatable tr"))
                }

                rosterHelper.getSalaryHistory(output, function (data) {
                    rosterHelper.updatePlayerSalary(data, function () {
                        scrapeSalary(++index, callback);
                    });
                });
            }
        });
    }
    else {
        callback();
    }
}

router.get('/api/scraper/salaries/players', function (req, res) {
    //var url = ; 
    //var url = ;
    //var url =;
    urls = [
        //{ url: "http://www.spotrac.com/mlb/los-angeles-dodgers/payroll/", abbr: "LAN", team: "Dodgers" },
        { url: "http://www.spotrac.com/mlb/arizona-diamondbacks/payroll/", abbr: "ARI", team: "Diamondbacks" },
        //{ url: "http://www.spotrac.com/mlb/atlanta-braves/payroll/", abbr: "ATL", team: "Braves" },
        //{ url: "http://www.spotrac.com/mlb/baltimore-orioles/payroll/", abbr: "BAL", team: "Orioles" },
        //{ url: "http://www.spotrac.com/mlb/boston-red-sox/payroll/", abbr: "BOS", team: "Red Sox" },
        //{ url: "http://www.spotrac.com/mlb/chicago-cubs/payroll/", abbr: "CHN", team: "Cubs" },
        //{ url: "http://www.spotrac.com/mlb/chicago-white-sox/payroll/", abbr: "CHA", team: "White Sox" },
        //{ url: "http://www.spotrac.com/mlb/cincinnati-reds/payroll/", abbr: "CIN", team: "Reds" },
        //{ url: "http://www.spotrac.com/mlb/cleveland-indians/payroll/", abbr: "CLE", team: "Indians" },
        //{ url: "http://www.spotrac.com/mlb/colorado-rockies/payroll/", abbr: "COL", team: "Rockies" },
        //{ url: "http://www.spotrac.com/mlb/detroit-tigers/payroll/", abbr: "DET", team: "Tigers" },
        //{ url: "http://www.spotrac.com/mlb/houston-astros/payroll/", abbr: "HOU", team: "Astros" },
        //{ url: "http://www.spotrac.com/mlb/kansas-city-royals/payroll/", abbr: "KC", team: "Royals" },
        //{ url: "http://www.spotrac.com/mlb/los-angeles-angels-of-anaheim/payroll/", abbr: "LAA", team: "Angels" },
        //{ url: "http://www.spotrac.com/mlb/new-york-mets/payroll/", abbr: "NYN", team: "Mets" },
        //{ url: "http://www.spotrac.com/mlb/new-york-mets/payroll/", abbr: "NYA", team: "Yankees" },
        //{ url: "http://www.spotrac.com/mlb/milwaukee-brewers/payroll/", abbr: "MIL", team: "Brewers" },
        //{ url: "http://www.spotrac.com/mlb/miami-marlins/payroll/", abbr: "MIA", team: "Marlins" },
        //{ url: "http://www.spotrac.com/mlb/minnesota-twins/payroll/", abbr: "MIN", team: "Twins" },
        //{ url: "http://www.spotrac.com/mlb/oakland-athletics/payroll/", abbr: "OAK", team: "Athelics" }, 
        //{ url: "http://www.spotrac.com/mlb/philadelphia-phillies/payroll/", abbr: "PHI", team: "Phillies" },
        //{ url: "http://www.spotrac.com/mlb/st.-louis-cardinals/payroll/", abbr: "STL", team: "Cardinals" },
        //{ url: "http://www.spotrac.com/mlb/pittsburgh-pirates/payroll/", abbr: "PIT", team: "Pirates" },
        //{ url: "http://www.spotrac.com/mlb/san-diego-padres/payroll/", abbr: "SD", team: "Padres" },
        //{ url: "http://www.spotrac.com/mlb/san-francisco-giants/payroll/", abbr: "SF", team: "Giants" },
        //{ url: "http://www.spotrac.com/mlb/seattle-mariners/payroll/", abbr: "SEA", team: "Mariners" },
        //{ url: "http://www.spotrac.com/mlb/tampa-bay-rays/payroll/", abbr: "TB", team: "Rays" },
        //{ url: "http://www.spotrac.com/mlb/texas-rangers/payroll/", abbr: "TEX", team: "Rangers" },
        //{ url: "http://www.spotrac.com/mlb/toronto-blue-jays/payroll/", abbr: "TOR", team: "Jays" },
        //{ url: "http://www.spotrac.com/mlb/washington-nationals/payroll/", abbr: "WAS", team: "Nationals" },
    ];

    scrapeSalary(0, function () {
        res.status(200).json({ status: 200, msg: "complete" });
    });

});

router.get('/api/scraper/roster/teams', function (req, res) {
    //var url = "http://www.spotrac.com/mlb/los-angeles-dodgers/payroll/";
    var url = "http://www.spotrac.com/mlb/arizona-diamondbacks/payroll/";

    utils.scrapeUrl(url, function (error, response, html) {
        if (!error) {
            var $ = cheerio.load(html);

            var tables = $("table");

            var output = {
                Team: "Dodgers",
                Abbreviation: "LAN",
                Active: getPlayerDataFromTable($, "Active", $($(tables)[0]).find(".datatable tr")),
                Injured: getPlayerDataFromTable($, "Injured", $($(tables)[1]).find(".datatable tr")),
                Retained: getPlayerDataFromTable($, "Retained", $($(tables)[2]).find(".datatable tr")),
                Minors: getPlayerDataFromTable($, "Minors", $($(tables)[3]).find(".datatable tr"))
            }

            res.status(200).json({ status: 200, count: output.length, players: output });
        }
    });
});

function getPlayerDataFromTable($, playerType, rows) {

    var output = [];
    for (var i = 1; i < rows.length; i++) {
        var cells = $(rows[i]).find("td");
        if ($((cells)[0]).find("a").text().trim().length > 0) {
            output.push({
                Name: $((cells)[0]).find("a").text(),
                Url: $((cells)[0]).find("a").attr("href"),
                Salary: $((cells)[7]).find("span").text().trim(),
                PlayerType: playerType
            });
        }
    }
    return output;
}


router.get('/api/scraper/roster/depth-chart', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var query = {
                $and:
                [
                    {
                        _id: req.query.lid
                    },
                    {
                        Teams: { $elemMatch: { _id: req.query.tid } }
                    }
                ]
            };
            League.findOne(query, { "Teams.$": 1 }, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (roster) {
                    var url = "http://www.espn.com/mlb/team/depth/_/name/" + roster.Teams[0].TeamName.toLowerCase();

                    utils.scrapeUrl(url, function (error, response, html) {
                        if (!error) {
                            var $ = cheerio.load(html);
                            var output = [];
                            var rows = [];
                            $("table tr").each(function (key, value) {
                                if (key > 1) {

                                    var data = {
                                        Position: $($(value).find("td")[0]).text(),
                                        Players: []
                                    }

                                    rows[$($(value).find("td")[0]).text()] = [];
                                    //rows[$($(value).find("td")[0]).text()].push($($(value).find("td")[0]).text())
                                    rows[$($(value).find("td")[0]).text()].push({ FullName: $($(value).find("td")[1]).text(), Url: $($(value).find("td")[1]).attr("href") })
                                    rows[$($(value).find("td")[0]).text()].push({ FullName: $($(value).find("td")[2]).text(), Url: $($(value).find("td")[2]).attr("href") })
                                    rows[$($(value).find("td")[0]).text()].push({ FullName: $($(value).find("td")[3]).text(), Url: $($(value).find("td")[3]).attr("href") })
                                    rows[$($(value).find("td")[0]).text()].push({ FullName: $($(value).find("td")[4]).text(), Url: $($(value).find("td")[4]).attr("href") })
                                    rows[$($(value).find("td")[0]).text()].push({ FullName: $($(value).find("td")[5]).text(), Url: $($(value).find("td")[5]).attr("href") })

                                    data.Players = rows[$($(value).find("td")[0]).text()];
                                    output.push(data);
                                }
                            });

                            playerHelper.getPlayersByName(output, function (result) {
                                res.status(200).json({ status: 200, msg: "success", positions: result });
                            });
                        }
                    });
                }
                else {
                    res.status(200).json({ status: 200, msg: "success", roster: [] });
                }
            });
        } else {
            res.status(401).json({ status: 401, msg: "bad token" });
        }
    });

});

router.get('/api/scraper/players/daily', function (req, res) {
    var url = "http://crunchtimebaseball.com/master.csv";
    var file = './data/master-' + moment().format("MMDDYYYY") + '-' + moment().format("x") + '.csv';
    var stream = fs.createWriteStream(file);

    var request = http.get(url, function (response) {
        response.pipe(stream);
    });
    stream.on('finish', function () {
        cloudstorage.uploadFile(file, "crunchtimebaseball", function (err, upload) {
            if (err) {
                res.status(err.code).json({ status: err.code, msg: "error", err: err });
            }
            else {
                csvHelper.parse(fs.createReadStream(file), function (data) {
                    playerHelper.mergePlayers(0, data, [], function (err, changes) {
                        fs.unlink(file, function (err) {
                            res.status(200).json({ status: 200, msg: "complete", data: changes });
                        });
                    });
                });
            }
        });
    });
});

// ********************* NO LONGER USED
router.get('/api/scraper/team/schedule', function (req, res) {
    var urls = [
        "http://www.espn.com/mlb/teams/printSchedule/_/team/ari/season/2017",
        "http://www.espn.com/mlb/teams/printSchedule/_/team/atl/season/2017",
        "http://www.espn.com/mlb/teams/printSchedule/_/team/bal/season/2017",
        "http://www.espn.com/mlb/teams/printSchedule/_/team/bos/season/2017"];

    callback(0, [], callback);

    function callback(index, output, callback) {
        if (urls[index] && urls.length > index) {
            scrape(index, output, callback);
        }
        else {
            res.status(200).json({ status: 200, count: output.length, schedule: output });
        }
    }

    function scrape(index, output, callback) {
        utils.scrapeUrl(urls[index], function (error, response, html) {
            if (!error) {
                var $ = cheerio.load(html);

                var cells = $("td table tr");

                cells.each(function (key, value) {
                    if (key > 2) {
                        output.push({
                            "Date": moment($($(value).find("td")[0]).text().trim() + " 2017 " + $($(value).find("td")[2]).text().trim()).add(12, 'hours'),
                            "Opponent": $($(value).find("td")[1]).text().trim()
                        });
                    }
                });

                callback(++index, output, callback);
            }
        });
    }
});

// NO LONGER USED
router.get('/api/scraper/schedule', function (req, res) {

    var stream = fs.createReadStream('./data/schedule.csv');

    Calendar.deleteMany({}, function () {

        csvHelper.parse(stream, function (data) {
            var output = [];
            for (var i = 0; i < data.length; i++) {
                data[i].Date = moment(data[i].Date + " 2017").format("YYYYMMDD");
                for (var key in data[i]) {
                    if (key != "Date" && data[i][key].length > 0) {
                        var title = (data[i][key].indexOf("@") > -1) ? key + data[i][key] : data[i][key] + "@" + key;
                        var home = (data[i][key].indexOf("@") > -1) ? key : data[i][key].replace("@", "");
                        var away = (data[i][key].indexOf("@") > -1) ? data[i][key].replace("@", "") : key;

                        var game = { title: title, start: data[i].Date, allDay: true, home: home, away: away };
                        if (output.filter(function (value) {
                            return value.start == game.start && value.title == game.title
                        }).length == 0) {
                            Calendar.create(game);
                            output.push(game);
                        }
                    }
                }
            }

          

            res.status(200).json({ status: 200, msg: "complete", data: data });
        });
    });
});


router.get('/api/scraper/stats/ytd/fielding', function (req, res) {

    var momentDate = moment(req.query.date, "YYYYMMDD").toDate();
    var date = new Date(momentDate);

    YtdFielding.findOne({ GameDate: (req.query.date) }, { "Stats": 0 }, function (err, parsed) {
        if (err) {
            res.status(500).json({ status: 500, msg: err.errmsg });
        }
        else if (!parsed) {

            var fixFilename = "YTDFielding_20180329_Fix.csv";
            var fileName = "YTDFielding_" + req.query.date + ".csv";
            if( req.query.date == "20180329")
                fileName = fixFilename;
            ftp.syncFile( fileName, function (data) {
                for (var i = 0; i < data.length; i++) {
                    data[i].GameDate = req.query.date;
                    data[i].FullName = data[i].PlayerName;

                }
                YtdFielding.create(data, function (err, result) {
                    if (err) {
                        res.status(500).json({ status: 500, msg: err.errmsg });
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "complete", result: result });
                    }
                });
            });
        }
        else {
            res.status(409).json({ status: 409, msg: "already parsed" });
        }
    });
});


router.get('/api/scraper/states', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var url = "https://gist.githubusercontent.com/mshafrir/2646763/raw/8b0dbb93521f5d6889502305335104218454c2bf/states_titlecase.json";
            utils.scrapeUrl(url, function (error, response, html) {
                var output = [];
                if (!error) {
                    var json = eval(html);

                    saveStates(0, json, function (err, states) {
                        if (err) {
                            res.status(500).json({ 'status': 500, error: err });
                        }
                        else {
                            res.status(200).json({ 'status': 200, msg: "success", states: states });
                        }
                    });
                }
                else {
                    res.status(500).json({ 'status': 500, error: error });
                }
            });
        }
    });
});

function saveStates(index, output, callback) {
    if (index == output.length) {
        callback(null, output);
    }
    else {
        State.create(output[index], function (err, result) {
            saveStates(++index, output, callback);
        });
    }
}

// *********************
// upload master schedule csv
// and create mlb schedule /schedule
//
router.post('/api/scraper/loadGameSchedule', upload.single('file'), function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var filename = req.file.filename;
            if (!req.file) {
                res.status(500).json({ status: 500, "msg": "error", "error": err });
            }
            else {
             //   csvHelper.parseFlat(fs.createReadStream(path.join(__dirname, "../uploads/" + filename)), function (data) {
             //   csvHelper.parse(fs.createReadStream(filePath + filename), function (data) {               
                        csvHelper.parse(fs.createReadStream(filePath + filename), function (data) {
                    if (data && data.length > 0) {

                        var gameDate;
                        var gameSimpleDate;

                        var alTeams = [

                        ];
                        class gameEvent {
                            constructor(index, visit, home, isNC ) {
                                var gameId =  gameSimpleDate + "-" + visit+"-"+ home.substring(1);
                                var title =  visit+home;
                                var sdate = gameSimpleDate;
                                if( isNC ) {
                                    gameId = gameId + "-NC";
                                    title  = title + "-NC";
                                    sdate = sdate;
                                }
                                this.day = {
                                    gameId: gameId,
                                    title:title,
                                    start: gameDate,
                                    end: "",
                                    color: "",
                                    description: "",
                                    url: "/#!/fullscore?gameID=" + gameId,
                                    simpleDate: sdate,
                                    isDoubleHeaderNC: isNC,
                                    home: home.substring(1),
                                    visit: visit,
                                    dhGame: false,
                                    extra: {
                                        played: false,
                                        homeScore: 0,
                                        visitScore: 0
                                    }
                                };

                                // figure out dhGame
                                if( TeamInfo.ALTeams.indexOf(this.day.home) >= 0) {
                                    this.day.dhGame = true;
                                }
                            }
                        }

                        newSchedule = new Schedule();
                        newSchedule.LeagueName = "MLB";
                        newSchedule.LeagueId = "MLB";
                        newSchedule.Year ="2018";
                        newSchedule.LastUpdate = new Date().toISOString();

                        var gameId = 0;
                 
                        // for each day....
                        for (var i = 0; i < data.length; i++) {

                            Object.keys(data[i]).forEach(function (game, index) {
                                // game is an object of either date or team name
                                // index: the ordinal position of the key within the object 

                                if( game == "Date") {
                                    gameDate = data[i]["Date"];
                                    var dateParts = gameDate.split("/");
                                    if( dateParts[0].length == 1) {
                                        dateParts[0] = "0" + dateParts[0];
                                    }
                                    if( dateParts[1].length == 1) {
                                        dateParts[1] = "0" + dateParts[1];
                                    }                                   5
                                    gameSimpleDate = "20" + dateParts[2] + dateParts[0] + dateParts[1];
                                    // utc format:
                                    // '2018-02-10T08:00:00',
                                    gameDate = "20" + dateParts[2] + "-" + dateParts[0] + "-" + dateParts[1];
                                    // add in the time
                                    // gameDate = gameDate + "T17:00:00";
                                    //
                                } else {
                                    var isNC = false;       // nc = night cap of double header
                                    if (game.charAt(0) == "D") {
                                        // and if not empty, then  a double header;
                                        if (data[i][game] && data[i][game] != "") {
                                            var teams = data[i][game].split("@");
                                            var team1 = teams[0];   // visitor
                                            var team2 = "@" + teams[1];   // home
                                            var gameName = team1 + team2; + "-NC";  // night cap means double header.
                                            isNC = true;
                                            newSchedule.Games.push(new gameEvent(gameId, team1, team2, isNC).day);
                                        }

                                    } else {

                                        // normal game.. only record if home team is the value and not the key (game)
                                        var team1 = game;
                                        var team2 = data[i][game];

                                        // only record if the home team is team2
                                        if (team2.charAt(0) == '@') {
                                            // team 1 is visitor, team 2 is home
                                            var gameName = team1 + team2;
                                            gameId++;

                                            // figure out if dhGame
                                            newSchedule.Games.push(new gameEvent(gameId, team1, team2, isNC).day);
                                        }
                                    }
                                }


                            });
                        }
/*
   db.restaurant.replaceOne(
      { "name" : "Pizza Rat's Pizzaria" },
      { "_id": 4, "name" : "Pizza Rat's Pizzaria", "Borough" : "Manhattan", "violations" : 8 },
      { upsert: true }
   );
*/
                        delete newSchedule._doc._id;
                        newSchedule.markModified("Games");
                        Schedule.replaceOne({LeagueId:"MLB", LeagueName:"MLB"},newSchedule, {upsert:true}, function (err, schedule) {
                            if (err) {
                                res.status(500).json({ status: 500, "msg": err.msg });

                            } else {
                                res.status(200).json({ 'status': 200, msg: "Schedule loaded.", data: data });
                            }
                        });
                    }
                    else {
                        res.status(500).json({ status: 500, "msg": "error" });
                    }
                });
            }
        }
    });
});

router.post('/api/scraper/parse/free-agent', upload.single('file'), function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var filename = req.file.filename;
            if (!req.file) {
                res.status(500).json({ status: 500, "msg": "error", "error": err });
            }
            else {
                csvHelper.parse(fs.createReadStream(path.join(__dirname, "../uploads/" + filename)), function (data) {
                    if (data && data.length > 0) {
                       

                        FreeAgent.remove({}, function (err, result) {
                            for (var i = 0; i < data.length; i++) {
                                if( !data[i].SigningYear && data[i]["Signing Year"])
                                    data[i].SigningYear = data[i]["Signing Year"];
                                if( !data[i].PriorOR && data[i]["Prior OR"])
                                    data[i].PriorOR = data[i]["Prior OR"];
                                data[i].Image = data[i].MlbId ? "http://mlb.mlb.com/mlb/images/players/head_shot/"+ data[i].MlbId + ".jpg" : "//rsportsbaseball.com/assets/img/avatars/avatar1.jpg";
                                
                                FreeAgent.create(data[i]);
                            }
                        });

                        res.status(200).json({ 'status': 200, msg: "success", data: data });
                    }
                    else {
                        res.status(500).json({ status: 500, "msg": "error"  });
                    }
                });      
            }
        }
    });
});

router.post('/api/scraper/parse/international', upload.single('file'), function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var filename = req.file.filename;
            if (!req.file) {
                res.status(500).json({ status: 500, "msg": "error", "error": err });
            }
            else {
                csvHelper.parse(fs.createReadStream("./uploads/" + filename), function (data) {
                    if (data && data.length > 0) {
                       

                        FreeAgent.remove({}, function (err, result) {
                            for (var i = 0; i < data.length; i++) {
                                data[i].SigningYear = data[i]["Signing Year"];
                                data[i].PriorOR = data[i]["Prior OR"];
                                data[i].Image = data[i].MlbId ? "http://mlb.mlb.com/mlb/images/players/head_shot/"+ data[i].MlbId + ".jpg" : "//rsportsbaseball.com/assets/img/avatars/avatar1.jpg";
                                FreeAgent.create(data[i]);
                            }
                        });

                        res.status(200).json({ 'status': 200, msg: "success", data: data });
                    }
                    else {
                        res.status(500).json({ status: 500, "msg": "error"  });
                    }
                });      
            }
        }
    });
});

router.post('/api/scraper/parse/firstyears', upload.single('file'), function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var filename = req.file.filename;
            if (!req.file) {
                res.status(500).json({ status: 500, "msg": "error", "error": err });
            }
            else {
                csvHelper.parse(fs.createReadStream("./uploads/" + filename), function (data) {
                    if (data && data.length > 0) {
                       

                        FreeAgent.remove({}, function (err, result) {
                            for (var i = 0; i < data.length; i++) {
                                data[i].SigningYear = data[i]["Signing Year"];
                                data[i].PriorOR = data[i]["Prior OR"];
                                data[i].Image = data[i].MlbId ? "http://mlb.mlb.com/mlb/images/players/head_shot/"+ data[i].MlbId + ".jpg" : "//rsportsbaseball.com/assets/img/avatars/avatar1.jpg";
                                FreeAgent.create(data[i]);
                            }
                        });

                        res.status(200).json({ 'status': 200, msg: "success", data: data });
                    }
                    else {
                        res.status(500).json({ status: 500, "msg": "error"  });
                    }
                });      
            }
        }
    });
});

router.post('/api/scraper/parse/financials', upload.single('file'), function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var filename = req.file.filename;
            if (!req.file) {
                res.status(500).json({ status: 500, "msg": "error", "error": err });
            }
            else {
                csvHelper.parse(fs.createReadStream("./uploads/" + filename), function (data) {
                    if (data && data.length > 0) {
                       

                        FreeAgent.remove({}, function (err, result) {
                            for (var i = 0; i < data.length; i++) {
                                data[i].SigningYear = data[i]["Signing Year"];
                                data[i].PriorOR = data[i]["Prior OR"];
                                data[i].Image = data[i].MlbId ? "http://mlb.mlb.com/mlb/images/players/head_shot/"+ data[i].MlbId + ".jpg" : "//rsportsbaseball.com/assets/img/avatars/avatar1.jpg";
                                FreeAgent.create(data[i]);
                            }
                        });

                        res.status(200).json({ 'status': 200, msg: "success", data: data });
                    }
                    else {
                        res.status(500).json({ status: 500, "msg": "error"  });
                    }
                });      
            }
        }
    });
});


router.get('/api/scraper/stats/ytd/baserunning', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var stream = fs.createReadStream('./data/2017stats/baserunning.csv');

            csvHelper.parse(stream, function (data) {
                if (data && data.length > 0) {
                    importHelper.addYtdBaserunning(0, data, function(stats){ 
                        res.status(200).json({ 'status': 200, msg: "success", stats: stats });
                    });
                }
                else {
                    res.status(500).json({ status: 500, "msg": "error" });
                }
            });
        }
    });
});

router.get('/api/scraper/stats/ytd/batter', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var stream = fs.createReadStream('./data/2017stats/batters.csv');

            csvHelper.parse(stream, function (data) {
                if (data && data.length > 0) {
                    importHelper.addYtdBatters(0, data, function (stats) {
                        res.status(200).json({ 'status': 200, msg: "success", stats: stats });
                    });
                }
                else {
                    res.status(500).json({ status: 500, "msg": "error" });
                }
            });
        }
    });
});

router.get('/api/scraper/stats/ytd/sp', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var stream = fs.createReadStream('./data/2017stats/sps.csv');

            csvHelper.parse(stream, function (data) {
                if (data && data.length > 0) {
                    importHelper.addYtdSPs(0, data, function (stats) {
                        res.status(200).json({ 'status': 200, msg: "success", stats: stats });
                    });
                }
                else {
                    res.status(500).json({ status: 500, "msg": "error" });
                }
            });
        }
    });
});

router.get('/api/scraper/stats/ytd/rp', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var stream = fs.createReadStream('./data/2017stats/rps.csv');

            csvHelper.parse(stream, function (data) {
                if (data && data.length > 0) {
                    importHelper.addYtdRPs(0, data, function (stats) {
                        res.status(200).json({ 'status': 200, msg: "success", stats: stats });
                    });
                }
                else {
                    res.status(500).json({ status: 500, "msg": "error" });
                }
            });
        }
    });
});


router.get('/api/scraper/stats/ytd/poverall', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var stream = fs.createReadStream('./data/2017stats/psoverall.csv');

            csvHelper.parse(stream, function (data) {
                if (data && data.length > 0) {
                    importHelper.addYtdPsOverall(0, data, function (stats) {
                        res.status(200).json({ 'status': 200, msg: "success", stats: stats });
                    });
                }
                else {
                    res.status(500).json({ status: 500, "msg": "error" });
                }
            });
        }
    });
});

router.get('/api/scraper/stats/ytd/defense', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var stream = fs.createReadStream('./data/2017stats/defense.csv');

            csvHelper.parse(stream, function (data) {
                if (data && data.length > 0) {
                    importHelper.addYtdDefense(0, data, function (stats) {
                        res.status(200).json({ 'status': 200, msg: "success", stats: stats });
                    });
                }
                else {
                    res.status(500).json({ status: 500, "msg": "error" });
                }
            });
        }
    });
});

router.get('/api/scraper/stats/ytd/catchers', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var stream = fs.createReadStream('./data/2017stats/catchers.csv');

            csvHelper.parse(stream, function (data) {
                if (data && data.length > 0) {
                    importHelper.addYtdCatchers(0, data, function (stats) {
                        res.status(200).json({ 'status': 200, msg: "success", stats: stats });
                    });
                }
                else {
                    res.status(500).json({ status: 500, "msg": "error" });
                }
            });
        }
    });
});

router.get('/api/scraper/stats/ytd/rvalue', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var stream = fs.createReadStream('./data/2017stats/rvalue.csv');

            csvHelper.parse(stream, function (data) {
                if (data && data.length > 0) {
                    importHelper.addYtdRValue(0, data, function (stats) {
                        res.status(200).json({ 'status': 200, msg: "success", stats: stats });
                    });
                }
                else {
                    res.status(500).json({ status: 500, "msg": "error" });
                }
            });
        }
    });
});

router.get('/api/scraper/stats/ytd/zone', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var stream = fs.createReadStream('./data/2017stats/zone.csv');

            csvHelper.parse(stream, function (data) {
                if (data && data.length > 0) {
                    importHelper.addYtdZone(0, data, function (stats) {
                        res.status(200).json({ 'status': 200, msg: "success", stats: stats });
                    });
                }
                else {
                    res.status(500).json({ status: 500, "msg": "error" });
                }
            });
        }
    });
});

router.get('/api/scraper/stats/ytd/fieldingindex', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var stream = fs.createReadStream('./data/2017stats/fieldingindex.csv');

            csvHelper.parse(stream, function (data) {
                if (data && data.length > 0) {
                    importHelper.addYtdFieldingIndex(0, data, function (stats) {
                        res.status(200).json({ 'status': 200, msg: "success", stats: stats });
                    });
                }
                else {
                    res.status(500).json({ status: 500, "msg": "error" });
                }
            });
        }
    });
});

router.get('/api/scraper/stats/ytd/blockframecera', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var stream = fs.createReadStream('./data/2017stats/blockframecera.csv');

            csvHelper.parse(stream, function (data) {
                if (data && data.length > 0) {
                    importHelper.addYtdBlockFrameCEra(0, data, function (stats) {
                        res.status(200).json({ 'status': 200, msg: "success", stats: stats });
                    });
                }
                else {
                    res.status(500).json({ status: 500, "msg": "error" });
                }
            });
        }
    });
});



router.post('/api/scraper/stats/masterplayers', upload.single('file'), function (req, res) {
    utils.addLog("reached master player uploader", 0, "scraper", "stats/masterplayers");

    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var filename = req.file.filename;

            if (!req.file) {
                res.status(500).json({ status: 500, "msg": "error with file name", "error": err });
            }
            else {

                MasterPlayer.remove({}, function (err, result) {
                    csvHelper.parse(fs.createReadStream(filePath + filename), function (data) {
             //           csvHelper.parse(fs.createReadStream("/var/www/uploads/" + filename), function (data) {
                        if (data && data.length > 0) {

                            // drop the players into a temp collection
                            var numberOfPlayers = data.length;

                            TempMasterPlayers.remove({}, function(err, result){
                                TempMasterPlayers.create({MasterPlayers:data}, function(err, result){
                                    if( err ) {
                                        // free up the memory
                                        data = {};
                                        results = {};
                                        res.status(500).json({ 'status': 500, msg: "couldn't save the temp file" });   
                                    } else {
                                        // free up the memory
                                        data = {};
                                        results = {};
                                      
                                        importHelper.addMasterPlayersFromTempPlayers(0, numberOfPlayers, function(err, result){
                                            if( err ) {
                                                res.status(500).json({ 'status': 500, msg: "couldn't transfer from the temp file" });   

                                            } else {
                                                res.status(200).json({ 'status': 200, msg: "players saved from temp file" }); 
                                            }
                                        })
                                    }
                                })
                            })

                            /*
                            // add each player 
                            importHelper.addMasterPlayers(0, data, function (masterPlayers) {

                                // buy back some memory.. done with these.
                                result = null;
                                data = null;

                                // now have each player updated to include their _id for the masterPlayers document
                                
                                
                                var newPlayers = [];                       
                                var newPlayerMsg = "No new players found.";

                                for (i = 0; i < masterPlayers.length; i++) {
                                    if (masterPlayers[i].NEW && masterPlayers[i].NEW != "") {
                                        newPlayers.push(masterPlayers[i]);
                                    }
                                }

                                // if there are new players... add in their contractExtras, put their salaries in, 
                                // and add them to the current leagues' free agent pools
                                masterPlayers = null; // free up memory

                                if (newPlayers.length > 0) {
                                    newPlayerMsg = newPlayers.length + " new player(s) found";

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
                                            for (i = 0; i < newPlayers.length; i++) {
                                                newFreeAgents[i] = rosterHelper.buildOneFreeAgent(newPlayers[i], contractExtras);
                                            }

                                            // ok, add the newFreeAgents to each league
                                            League.find( {}, {FreeAgents:1}, function (err, leagues) {
                                                if (!err) {

                                                    for( i=0; i<leagues.length; i++) {
                                                    for (n = 0; n < newFreeAgents.length; n++) {
                                                        leagues[i]._doc.FreeAgents.unshift(newFreeAgents[n]);
                                                        leagues[i].markModified("FreeAgents");
                                                    }


                                                }

                                                var lastLeague = leagues.length-1;
                                                    leagues[lastLeague].save( function(err, response ){
                                                        if( err ) {
                                                            res.status(401).json({ 'status': 401, msg: "Error creating new free agents"});
                                                        } else {
                                                            res.status(200).json({ 'status': 200, msg: newPlayerMsg });
                                                        }

                                                    })

                                                }
                                            });
                                        }
                                    });
                                } else {
                                    res.status(200).json({ 'status': 200, msg: newPlayerMsg });                                   
                                }

                            });
                            */

                        }
                        else {
                            res.status(500).json({ status: 500, "msg": "error interpreting CSV file" });
                        }
                    });
                });
            }
            }
    });
});


router.get('/api/local/load/masterplayers', function (req, res) {
    utils.addLog("reached emergency master player uploader", 0, "scraper", "stats/masterplayers");

    var filename = './data/2018stats/masterlist.csv';
     MasterPlayer.remove({}, function (err, result) {

        utils.addLog("emergency master player: players removed", 0, "scraper", "stats/masterplayers");
        var stream = fs.createReadStream(filename);
        utils.addLog("emergency master player: opened file " + filename, 0, "scraper", "stats/masterplayers");
            csvHelper.parse(stream, function (data) {
                utils.addLog("emergency master player: csv done", 0, "scraper", "stats/masterplayers");

                if (data && data.length > 0) {

                    // add each player 
                    importHelper.addMasterPlayers(0, data, function (masterPlayers) {

                        // buy back some memory.. done with these.
                        result = null;
                        data = null;

                        // now have each player updated to include their _id for the masterPlayers document


                        var newPlayers = [];
                        var newPlayerMsg = "Master List loaded. No new players found.";

                        for (i = 0; i < masterPlayers.length; i++) {
                            if (masterPlayers[i].NEW && masterPlayers[i].NEW != "") {
                                newPlayers.push(masterPlayers[i]);
                            }
                        }

                        // if there are new players... add in their contractExtras, put their salaries in, 
                        // and add them to the current leagues' free agent pools
                        masterPlayers = null; // free up memory

                        if (newPlayers.length > 0) {
                            newPlayerMsg = "Master List loaded. " + newPlayers.length + " new player(s) found";

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
                                    for (i = 0; i < newPlayers.length; i++) {
                                        newFreeAgents[i] = rosterHelper.buildOneFreeAgent(newPlayers[i], contractExtras);
                                    }

                                    // ok, add the newFreeAgents to each league
                                    League.find({}, { FreeAgents: 1 }, function (err, leagues) {
                                        if (!err) {

                                            for (i = 0; i < leagues.length; i++) {
                                                for (n = 0; n < newFreeAgents.length; n++) {
                                                    leagues[i]._doc.FreeAgents.unshift(newFreeAgents[n]);
                                                }
                                                leagues[0].markModified("FreeAgents");
                                            }


                                            leagues[0].save(function (err, response) {
                                                if (err) {
                                                    res.status(401).json({ 'status': 401, msg: "Error creating new free agents" });
                                                } else {
                                                    res.status(200).json({ 'status': 200, msg: newPlayerMsg });
                                                }

                                            })

                                        }
                                    });
                                }
                            });
                        } else {
                            res.status(200).json({ 'status': 200, msg: newPlayerMsg });
                        }

                    });

                } else {
                    res.status(401).json({ 'status': 401, msg: "Error with file size" });
                      
                }

            });
        });
});
module.exports = router;