
require('./data/mongo'); 
require("./models/pitching");
require("./models/fielding");
require("./models/batting");
require("./models/baserunning"); 


var utils = require('./helpers/utils');
var ftp = require('./helpers/ftp');

var mongoose = require('mongoose'); 
var Pitching = mongoose.model('Pitching');
var Baserunning = mongoose.model('Baserunning');
var Fielding = mongoose.model('Fielding');
var Batting = mongoose.model('Batting'); 


var date = new Date();
var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate() - 1)).slice(-2);

Pitching.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
    if (err) {
        console.log(err);
        process.exit()
    }
    else if (!stat) {
        var stat = {
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
                    console.log(err);
                    process.exit()
                }
                else {

                    Baserunning.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
                        if (err) {
                            console.log(err);
                            process.exit()
                        }
                        else if (!stat) {
                            var stat = {
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
                                        console.log(err);
                                        process.exit()
                                    }
                                    else {
                                        Batting.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
                                            if (err) {
                                                console.log(err);
                                                process.exit()
                                            }
                                            else if (!stat) {
                                                var stat = {
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
                                                            console.log(err);
                                                            process.exit()
                                                        }
                                                        else {

                                                            Fielding.findOne({ GameDate: dateString }, { "Stats": 0 }, function (err, stat) {
                                                                if (err) {
                                                                    console.log(err);
                                                                    process.exit()
                                                                }
                                                                else if (!stat) {
                                                                    var stat = {
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
                                                                                console.log(err);
                                                                            }
                                                                            else {
                                                                                console.log("success");
                                                                                process.exit();
                                                                            }
                                                                        });
                                                                    });

                                                                }
                                                                else {
                                                                    console.log("already parsed");
                                                                    process.exit()
                                                                }

                                                            });
                                                        }
                                                    });
                                                });

                                            }
                                            else {
                                                console.log("already parsed");
                                                process.exit()
                                            }

                                        });
                                    }
                                });
                            });

                        }
                        else {
                            console.log("already parsed");
                            process.exit()
                        }

                    });
                }
            });
        });

    }
    else {
        console.log("already parsed");
        process.exit()
    }

});



