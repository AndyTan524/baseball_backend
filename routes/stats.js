var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var playerHelper = require('../helpers/playerHelper');
var utils = require('../helpers/utils');

var moment = require("moment");

var mongoose = require('mongoose');
var Stat = mongoose.model('Stat');
var Pitching = mongoose.model('Pitching');
var Baserunning = mongoose.model('Baserunning');
var Fielding = mongoose.model('Fielding');
var Batting = mongoose.model('Batting');
var YtdFielding = mongoose.model('YtdFielding');

router.get('/api/stats/parse/read', function (req, res) {
    res.status(200).json({ msg: "Api lives" });
});

router.get('/api/stats/baserunning/list', function (req, res) {

    var page = req.query.pag == null ? 0 : parseInt(req.query.page);
    var limit = req.query.limit == null ? 25 : parseInt(req.query.limit);
    var sort = req.query.sort == null ? 1 : req.query.sort;

    Baserunning.find({}, { Stats: 0 }, function (err, stats) {
        if (err) {
            res.status(500).json(err);
        }
        else {

            res.status(200).json({ status: 200, msg: "Stats found", stats: stats });
        }
    })
        .skip(page * limit)
        .sort({ "GameDate": "desc" })
        .limit(limit);
});

router.get('/api/stats/fielding/list', function (req, res) {

    var page = req.query.pag == null ? 0 : parseInt(req.query.page);
    var limit = req.query.limit == null ? 25 : parseInt(req.query.limit);
    var sort = req.query.sort == null ? 1 : req.query.sort;

    Fielding.find({}, { Stats: 0 }, function (err, stats) {
        if (err) {
            res.status(500).json(err);
        }
        else {

            res.status(200).json({ status: 200, msg: "Stats found", stats: stats });
        }
    })
        .skip(page * limit)
        .sort({ "GameDate": "desc" })
        .limit(limit);
});

router.get('/api/stats/batting/list', function (req, res) {

    var page = req.query.pag == null ? 0 : parseInt(req.query.page);
    var limit = req.query.limit == null ? 25 : parseInt(req.query.limit);
    var sort = req.query.sort == null ? 1 : req.query.sort;

    Batting.find({}, { Stats: 0 }, function (err, stats) {
        if (err) {
            res.status(500).json(err);
        }
        else {

            res.status(200).json({ status: 200, msg: "Stats found", stats: stats });
        }
    })
        .skip(page * limit)
        .sort({ "GameDate": "desc" })
        .limit(limit);
});

router.get('/api/stats/pitching/list', function (req, res) {

    var page = req.query.pag == null ? 0 : parseInt(req.query.page);
    var limit = req.query.limit == null ? 25 : parseInt(req.query.limit);
    var sort = req.query.sort == null ? 1 : req.query.sort;

    Pitching.find({}, { Stats: 0 }, function (err, stats) {
        if (err) {
            res.status(500).json(err);
        }
        else {

            res.status(200).json({ status: 200, msg: "Stats found", stats: stats });
        }
    })
        .skip(page * limit)
        .sort({ "GameDate": "desc" })
        .limit(limit);
});

router.get('/api/stats/pitching/get', function (req, res) {
    var date = req.query.date;
    var player = req.query.playerId;
    var ytd = req.query.ytd;

    if (!date) {
        res.status(500).json({ status: 500, "msg": "Please supply a date" });
    }
    else if (ytd != "true") {
        playerHelper.getDailyPitching(date, 0, [{ MlbId: player }], function (data) {
            res.json({ status: 200, "msg": "success", stat: { Stats: [data[0].Pitching] } });
        });
    }
    else {
        playerHelper.getYtdPitching("20170404", moment().format("YYYYMMDD"), "20170404", 0, [{ MlbId: player }], function (data) {

            res.json({ status: 200, "msg": "success", stat: { Stats: [data[0].Pitching] } });
        });
    }
});

router.get('/api/stats/baserunning/get', function (req, res) {
    var date = req.query.date;
    var player = req.query.playerId;
    var ytd = req.query.ytd;

    if (!date) {
        res.status(500).json({ status: 500, "msg": "Please supply a date" });
    }
    else if (ytd != "true") {
        playerHelper.getDailyBaseRunning(date, 0, [{ MlbId: player }], function (data) {
            res.json({ status: 200, "msg": "success", stat: { Stats: [data[0].Baserunning] } });
        });
    }
    else {
        playerHelper.getYtdBaserunning("20170404", date, "20170404", 0, [{ MlbId: player }], function (data) {

            res.json({ status: 200, "msg": "success", stat: { Stats: [data[0].Baserunning] } });
        });
    }
});

router.get('/api/stats/batting/get', function (req, res) {
    var date = req.query.date;
    var player = req.query.playerId;
    var ytd = req.query.ytd;

    if (!date) {
        res.status(500).json({ status: 500, "msg": "Please supply a date" });
    }
    else if (ytd != "true") {
        playerHelper.getDailyBatting(date, 0, [{ MlbId: player }], function (data) {


            res.json({ status: 200, "msg": "success", stat: { Stats: [stats] } });
        });
    }
    else {
        playerHelper.getYtdBatting("20170404", date, "20170404", 0, [{ MlbId: player }], function (data) {

            res.json({ status: 200, "msg": "success", stat: { Stats: [data[0].Batting] } });
        });
    }
});

router.get('/api/stats/fielding/get', function (req, res) {
    var date = req.query.date;
    var player = req.query.playerId;
    var ytd = req.query.ytd;

    if (!date) {
        res.status(500).json({ status: 500, "msg": "Please supply a date" });
    }
    else if (ytd != "true") {
        playerHelper.getDailyFielding(date, 0, [{ MlbId: player }], function (data) {
            res.json({ status: 200, "msg": "success", stat: { Stats: [data[0].Fielding] } });
        });
    }
    else {
        playerHelper.getYtdFielding("20170404", moment().format("YYYYMMDD"), "20170404", 0, [{ MlbId: player }], function (data) {

            res.json({ status: 200, "msg": "success", stat: { Stats: [data[0].Fielding] } });
        });
    }
});

router.get('/api/stats/player/ytd/fielding/get', function (req, res) {
    var date = req.query.date;
    var playerId = req.query.playerId;

    playerHelper.getYtdFieldingAdditional(date, 0, [{ MLBId: playerId }], function (stats) {
        res.json({ status: 200, "msg": "success", stats: stats });
    });
});

router.get('/api/stats/player/get', function (req, res) {
    var date = req.query.date;
    var playerId = req.query.playerId;
    var ytd = req.query.ytd;

    var stats = {
        player: {},
        batting: {},
        fielding: {},
        pitching: {},
        baserunning: {}
    }

    if (!date) {
        res.status(500).json({ status: 500, "msg": "Please supply a date" });
    }
    else if (ytd != "true") {
        playerHelper.getPlayerByMlbId(playerId, function (err, player) {
            stats.player = player;
            //playerHelper.getDailyFielding(date, 0, [{ MlbId: playerId }], function (data) {
            //stats.fielding.daily = data;
            playerHelper.getYtdFielding("20170404", date, "20170404", 0, [{ MlbId: playerId }], function (fielding) {
                stats.fielding = fielding[0].Fielding;
                playerHelper.getYtdBatting("20170404", date, "20170404", 0, [{ MlbId: playerId }], function (batting) {
                    stats.batting = batting[0].Batting;
                    playerHelper.getYtdBaserunning("20170404", date, "20170404", 0, [{ MlbId: playerId }], function (baserunning) {
                        stats.baserunning = baserunning[0].Baserunning;
                        playerHelper.getYtdPitching("20170404", date, "20170404", 0, [{ MlbId: playerId }], function (pitching) {
                            stats.pitching = pitching[0].Pitching;
                            //playerHelper.getYtdFieldingAdditional(date, 0, [{ MlbId: playerId }], function (ytdFielding) {
                                //stats.pitching.daily = dailyPitching;
                                //playerHelper.getDailyBatting(date, 0, [{ MlbId: playerId }], function (dailyBatting) {
                                //stats.batting.daily = dailyBatting;
                                //playerHelper.getDailyBaseRunning(date, 0, [{ MlbId: playerId }], function (dailyBaserunning) {
                                //stats.baserunning.daily = dailyBaserunning;
                                var speed = 0;
                                var eSpeed = 0;
                                switch (stats.player.Position) {
                                    case "P":
                                        speed = 0;
                                        eSpeed = 0;
                                        break;
                                    case "C":
                                        speed = 1;
                                        eSpeed = 1;
                                        break;
                                    case "1B":
                                        speed = 2;
                                        eSpeed = 2;
                                        break;
                                    case "2B":
                                        speed = (5 / 4) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                        eSpeed = (5 / 4) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                        break;
                                    case "3B":
                                        speed = (4 / 2.65) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                        eSpeed = (4 / 2.65) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                        break;
                                    case "SS":
                                        speed = (4.6 / 7) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                        eSpeed = (4.6 / 7) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                        break;
                                    case "DH":
                                        speed = 0;
                                        eSpeed = 0;
                                        break;
                                    default:
                                        speed = (3) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                        eSpeed = (3) * ((stats.fielding.ytd.PO) / stats.fielding.ytd.G);
                                        break;
                                }

                                stats.batting.ytd.SPEED = (stats.batting.ytd.SBA + stats.batting.ytd.T + stats.batting.ytd.RS + stats.batting.ytd.GIDP + speed) / 6;
                                stats.batting.ytd.ESPEED = (stats.batting.ytd.SBA + stats.batting.ytd.T + stats.batting.ytd.RS + stats.batting.ytd.GIDP + eSpeed) / 6;

                                ytdFielding = stats.fielding.daily[stats.fielding.daily.length-1];

                                if (ytdFielding) {
                                    stats.fielding.ytd.Pos = ytdFielding.Pos;
                                    stats.fielding.ytd.G = ytdFielding.G;
                                    stats.fielding.ytd.GS = ytdFielding.GS;
                                    stats.fielding.ytd.INN = ytdFielding.INN;
                                    stats.fielding.ytd.BallsInZone = ytdFielding.BallsInZone;
                                    stats.fielding.ytd.MissedBallsInZone = ytdFielding.MissedBallsInZone;
                                    stats.fielding.ytd.OutsOutOfZone = ytdFielding.OutsOutOfZone;
                                    stats.fielding.ytd.CBlockingRuns = ytdFielding.CBlockingRuns;
                                    stats.fielding.ytd.CFramingRuns = ytdFielding.CFramingRuns;
                                    stats.fielding.ytd.Cera = ytdFielding.Cera;
                                }
                                res.json({ status: 200, "msg": "success", stats: stats });
                            //});
                            //});
                            //});
                            //});
                        });
                    });
                });
            });
        });
    }
    else {


        res.json({ status: 200, "msg": "success", stats: stats });
    }


});

router.get('/api/stats/get', function (req, res) {
    var query = { $or: [{ _id: req.query._id }, { GameDate: req.query.date }] };
    Stat.findOne(query, function (err, stat) {
        if (err) {
            res.status(500).json(err);
        }
        else {
            res.json({ status: 200, "msg": "success", stat: stat });
        }
    });
});

router.post('/api/stats/date/delete', function (req, res) {

    Pitching.findOneAndRemove({ GameDate: req.body.date }, function (err, result) {
        if (err) {
            res.status(200).json({ "status": 500, "msg": err.message });
        }
        else if (result) {
            Baserunning.findOneAndRemove({ GameDate: req.body.date }, function (err, result) {
                if (err) {
                    res.status(200).json({ "status": 500, "msg": err.message });
                }
                else if (result) {
                    Batting.findOneAndRemove({ GameDate: req.body.date }, function (err, result) {
                        if (err) {
                            res.status(200).json({ "status": 500, "msg": err.message });
                        }
                        else if (result) {
                            Fielding.findOneAndRemove({ GameDate: req.body.date }, function (err, result) {
                                if (err) {
                                    res.status(200).json({ "status": 500, "msg": err.message });
                                }
                                else if (result) {
                                    res.json({ 'status': 200, msg: "stat is removed" });
                                }
                                else {
                                    res.json({ 'status': 500, 'msg': "no stat" });
                                }
                            });
                        }
                        else {
                            res.json({ 'status': 500, 'msg': "no stat" });
                        }
                    });
                }
                else {
                    res.json({ 'status': 500, 'msg': "no stat" });
                }
            });
        }
        else {
            res.json({ 'status': 500, 'msg': "no stat" });
        }
    });
});

router.post('/api/stats/pitching/delete', function (req, res) {
    Pitching.findByIdAndRemove({ _id: req.body._id }, function (err, result) {
        if (err) {
            res.status(200).json({ "status": 500, "msg": err.message });
        }
        else if (result) {
            res.json({ 'status': 200, msg: "stat is removed" });
        }
        else {
            res.json({ 'status': 500, 'msg': "no stat" });
        }
    });
});

router.post('/api/stats/baserunning/delete', function (req, res) {
    Baserunning.findByIdAndRemove({ _id: req.body._id }, function (err, result) {
        if (err) {
            res.status(200).json({ "status": 500, "msg": err.message });
        }
        else if (result) {
            res.json({ 'status': 200, msg: "stat is removed" });
        }
        else {
            res.json({ 'status': 500, 'msg': "no stat" });
        }
    });
});

router.post('/api/stats/batting/delete', function (req, res) {
    Batting.findByIdAndRemove({ _id: req.body._id }, function (err, result) {
        if (err) {
            res.status(200).json({ "status": 500, "msg": err.message });
        }
        else if (result) {
            res.json({ 'status': 200, msg: "stat is removed" });
        }
        else {
            res.json({ 'status': 500, 'msg': "no stat" });
        }
    });
});

router.post('/api/stats/fielding/delete', function (req, res) {
    Fielding.findByIdAndRemove({ _id: req.body._id }, function (err, result) {
        if (err) {
            res.status(200).json({ "status": 500, "msg": err.message });
        }
        else if (result) {
            res.json({ 'status': 200, msg: "stat is removed" });
        }
        else {
            res.json({ 'status': 500, 'msg': "no stat" });
        }
    });
});

router.get('/api/stats/download', function (req, res) {
    var query = { $or: [{ _id: req.query._id }, { GameDate: req.query.date }] };
    Stat.findOne(query, function (err, stat) {
        if (err) {
            res.status(500).json(err);
        }
        else {
            var buffer = new Buffer(JSON.stringify(stat));
            res.writeHead(200, {
                'Content-Type': "application/json",
                'Content-disposition': 'attachment;filename=' + stat.GameDate + ".json"
            });
            res.end(buffer)
        }
    });
});

router.get('/api/stats/get-latest-ingestion-date', function (req, res) {

   // find({}).sort({GameDate:-1}).limit(1)
   var batDate = "";
   var pitDate = "";
   var runDate = "";
   var defDate = "";
   Batting.find({}, function(err, batters){
       if( !err ) {
           batDate = batters[0].GameDate;
       }
       Pitching.find({}, function(err, pitchers){
        if( !err ) {
            pitDate = pitchers[0].GameDate;
        }
        Fielding.find({}, function(err, fielders){
            if( !err ) {
                defDate = fielders[0].GameDate;
            }
            Baserunning.find({}, function(err, runners){
                if( !err ) {
                    runDate = runners[0].GameDate;
                }
                var msg = "Problem reading stats";
                var recentDate = "Not all stats imported through same date";
                if( batDate != "" && batDate==pitDate && batDate==defDate && batDate==runDate) {
                    msg = "Most recent MLB stats data uploaded and ingested on " + batDate;
                    recentDate = batDate;

                } else {
                    bmsg = "No battings stats";
                    if( batDate != "") {
                        bmsg = "Batting updated " + batDate;
                    }
                    pmsg = ", no pitching stats";
                    if( pitDate != "") {
                        pmsg = ", pitching updated " + pitDate;
                    }
                    brmsg = ", no baserunning stats";
                    if( runDate != "") {
                        brmsg = ", baserunning updated " + runDate;
                    }
                    dmsg = ", no fielding stats";
                    if( defDate != "") {
                        dmsg = ", fielding updated " + defDate;
                    }                                                           

                   msg =  "All stats not updated: "  + bmsg + pmsg + brmsg + dmsg + ". Please upload and ingest stats for EACH missing date below and try again.";
 
                }
                res.status(200).json({ status: 200, msg: msg, recentDate: recentDate});
            }).sort({GameDate:-1}).limit(1)
        }).sort({GameDate:-1}).limit(1)
    }).sort({GameDate:-1}).limit(1)
   }).sort({GameDate:-1}).limit(1)
});

module.exports = router;