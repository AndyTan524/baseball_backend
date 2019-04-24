var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var Player = mongoose.model('Player');
var MasterPlayer = mongoose.model('MasterPlayer');
var FreeAgent = mongoose.model('FreeAgent');
var YtdStats = mongoose.model("YtdStats");
var Roster = mongoose.model('Roster');
var AccumulatedStats = mongoose.model('AccumulatedStats');


var Pitching = mongoose.model('Pitching');
var Baserunning = mongoose.model('Baserunning');
var Fielding = mongoose.model('Fielding');
var Batting = mongoose.model('Batting');

router.get('/api/team/get', function (req, res) {
    var qry = {};
    if (req.query.RFBTeam) {
        qry = { MLBTeam: req.query.RFBTeam }
    }
    else {
        qry = { MLBTeam: req.query.MLBTeam };
    }

    Player.find(qry, function (err, players) {
        if (err) {
            res.status(200).json({ status: 200, msg: err });
        }
        else if (players) {
            res.status(200).json({ status: 200, players: players });
        }
        else {
            res.status(200).json({ status: 200, msg: "not found" });
        }
    });
});

router.get('/api/player/get', function (req, res) {
    var playerId = req.query.playerId;
    var date = req.query.date;

    var query = {
        $or: [
            { GameDate: date }
        ]
    };

    if (date == null) {
        query = {};
    }
    else if (date.indexOf(",") > -1) {
        var dates = date.split(',');
        query["$or"] = [];
        for (var i = 0; i < dates.length; i++) {
            query["$or"].push({ GameDate: dates[i] });
        }
    }

    var projection = {
        "Stats":
        {
            $elemMatch:
            {
                "MLBId": playerId
            }
        }
    };

    Player.findOne({ MlbId: playerId }, function (err, player) {
        if (err) {
            res.status(500).json({ status: 500, msg: err });
        }
        else if (player) {
            Pitching.find(query, projection, function (err, stats) {
                player._doc.Pitching = stats ? stats.filter(function (key, value) {
                    return key._doc.Stats ? key._doc.Stats[0].GameId.length > 0 : false
                }) : [];

                Baserunning.find(query, projection, function (err, stats) {
                    player._doc.Baserunning = stats ? stats.filter(function (key, value) {
                        return key._doc.Stats ? key._doc.Stats[0].GameId.length > 0 : false
                    }) : [];

                    Fielding.find(query, projection, function (err, stats) {
                        player._doc.Fielding = stats ? stats.filter(function (key, value) {
                            return key._doc.Stats ? key._doc.Stats[0].GameId.length > 0 : false
                        }) : [];

                        Batting.find(query, projection, function (err, stats) {
                            player._doc.Batting = stats ? stats.filter(function (key, value) {
                                return key._doc.Stats ? key._doc.Stats[0].GameId.length > 0 : false
                            }) : [];

                            res.status(200).json({ status: 200, player: player });
                        });
                    });
                });
            });
        }
        else {
            res.status(200).json({ status: 200, msg: "not found" });
        }
    });




});

// ******************************* MASTERPLAYER/GET - Get one player's master player data ****************
router.get('/api/player/get/masterdata', function (req, res) {

    MasterPlayer.findOne({ MlbId: req.query.MLBId }, function (err, player) {
        if (err) {
            res.status(201).json({ status: 201, msg: "not found", stats: null });
        } else {
            // now get the player's accumulated MLS:
            AccumulatedStats.findOne({ LeagueId: req.query.lid, PlayerId: req.query.playerId }, function (err, accstats) {
                let MLS = 0;
                if (!err && accstats != null ) {
                    if (accstats.DaysList && accstats.DaysList.length > 0) {
                        accstats.DaysList.sort(function (a, b) { return a - b });
                        let high = Math.floor(accstats.DaysList[accstats.DaysList.length - 1]);
                        let low = Math.floor(accstats.DaysList[0]);

                        MLS = high - low;
                    }
                }
                if( player ) {
                    player.RSMLS = MLS;
                    player._doc.RSMLS = MLS;
                }
                res.status(200).json({ status: 200, msg: "success", stats: player });
            })
        }
    })
});


router.get('/api/player/name/query', function (req, res) {

    var query = JSON.parse(req.query.query);
    var filter = JSON.parse(req.query.filter);
    var show = req.query.show;

    var nameQuery = { "FullName": { $regex: ".*" + req.query.q + "*", $options: "ix" } };
    if (req.query.q == "") {
        nameQuery = { "LastName": { "$ne": "" } };
    }

    var positionQuery = [];
    if (filter.pitcher) {
        positionQuery.push({ Position: "P" });
    }
    if (filter.fielder) {
        positionQuery.push({ Position: "CF" });
        positionQuery.push({ Position: "RF" });
        positionQuery.push({ Position: "LF" });
        positionQuery.push({ Position: "OF" });
        positionQuery.push({ Position: "SS" });
        positionQuery.push({ Position: "3B" });
        positionQuery.push({ Position: "2B" });
        positionQuery.push({ Position: "1B" });
        positionQuery.push({ Position: "INF" });
    }
    if (filter.catcher) {
        positionQuery.push({ Position: "CA" });
    }
    if (filter.infield) {
        positionQuery.push({ Position: "SS" });
        positionQuery.push({ Position: "3B" });
        positionQuery.push({ Position: "2B" });
        positionQuery.push({ Position: "1B" });
        positionQuery.push({ Position: "INF" });
    }
    if (filter.outfield) {
        positionQuery.push({ Position: "OF" });
        positionQuery.push({ Position: "CF" });
        positionQuery.push({ Position: "RF" });
        positionQuery.push({ Position: "LF" });
    }

    if (positionQuery.length == 0) {
        positionQuery.push({});     // include every position
    }


    var teamQuery = [];
    if (filter.teams) {
        Object.keys(filter.teams).forEach(function (key, index) {
            // key: the name of the object key
            // index: the ordinal position of the key within the object 
            var team = filter.teams[key];
            if (team == true) {
                teamQuery.push({ MlbTeam: key });
            }
        });
    }
    if (teamQuery.length == 0) {
        teamQuery.push({});
    }

    nonRosterLevels = {
        "ML": "MLB",
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
                levelQuery.push({ Level: nonRosterLevels[key] });
            }
        });
    }
    if (levelQuery.length == 0) {
        levelQuery.push({});
    }
    var qry = { $and: [nameQuery, { $or: positionQuery }, { $or: teamQuery }, { $or: levelQuery }] };

    /*
    var qry = { "FullName": { $regex: ".*" + req.query.q + "*", $options: "ix" } };
    if (req.query.q == "") {
        qry = { "LastName" :  {"$ne": ""}};
    }



    qry.$or = []; 
    var tmp = [];

    if (filter.pitcher) {
        tmp.push({ Position: "P" });
    }
    if (filter.fielder) {
        tmp.push({ POS: "CF" });
        tmp.push({ Position: "RF" });
        tmp.push({ Position: "LF" }); 
        tmp.push({ Position: "OF" }); 
    }
    if (filter.catcher) {
        tmp.push({ Position: "CA" });
    }
    if (filter.infield) {
        tmp.push({ Position: "SS" });
        tmp.push({ Position: "3B" });
        tmp.push({ Position: "2B" });
        tmp.push({ Position: "1B" });
    }
    if (filter.outfield) {
        tmp.push({ Position: "OF" });
    } 
     

    if (filter.team) {
        var team = tmp;
        for (var i = 0; i < tmp.length; tmp++) {
            qry.$or.push({ $and: [ { MLBTeam: filter.team }, tmp[i] ] });
        } 
       
     
    }
    if (filter.level) {
        var level = tmp;
       level.push({ Level: filter.level });
       qry.$or.push({ $and: tmp });
       
    }


    if (qry.$or.length == 0) {
        delete qry.$or;
    }
  

    if (tmp.length == 0) {
 
    }
    else if (tmp.length == 1) {
            qry.$or = [tmp[0]]
    }
    else {
        qry = { $or: [] };  
        qry.$or.push({ $and: tmp });
    }
   

    if( tmp.length > 0 ) {
        if( tmp.length == 1) {
        // db.inventory.find( { $and: [ { price: { $ne: 1.99 } }, { price: { $exists: true } } ] } )
        qry = { "$and": [qry, tmp[0] ]};
        } else {
            /*
            db.inventory.find( {
             $and : [
        { $or : [ { price : 0.99 }, { price : 1.99 } ] },
        { $or : [ { sale : true }, { qty : { $lt : 20 } } ] }
    ]
} )

            qry = { "$and": [qry, {"$or":[tmp] }] };
        }
    }


   // var sort = [];
   // sort[query.sort] = query.dir;
   */
    var limit = query.limit;
    var page = query.page;
    var sort = query.sort;


    MasterPlayer.count(qry, function (err, count) {

        MasterPlayer.find(qry, function (err, players) {
            if (err) {
                res.status(200).json({ status: 200, msg: err });
            }
            else if (players) {
                if (show == "Bstats" || show == "Pstats" || show == "stats" || show == "salaries") {
                    var namelist = [];
                    for (i = 0; i < players.length; i++) {
                        namelist.push(players[i].FullName);
                    }

                    YtdStats.find({ Name: { $in: namelist } }, function (err, stats) {
                        if (err) { }
                        else {
                            for (i = 0; i < players.length; i++) {
                                var nextname = players[i].FullName;
                                players[i]._doc['Stats'] = {};
                                for (s = 0; s < stats.length; s++) {
                                    if (stats[s].Name == nextname) {
                                        players[i]._doc['Stats'] = stats[s];
                                        break;
                                    }

                                }
                            }
                            res.status(200).json({ status: 200, players: players, pages: count / limit, count: count });
                        }
                    });

                } else {

                    res.status(200).json({ status: 200, players: players, pages: count / limit, count: count });
                }
            }
            else {
                res.status(200).json({ status: 200, msg: "not found" });
            }
        }).skip(parseInt(page) * parseInt(limit)).sort(sort).limit(limit);

    });
});

router.get('/api/player/admin/query', function (req, res) {

    var qry = { "FullName": { $regex: ".*" + req.query.q + "*", $options: "ix" } };
    if (req.query.q == "") {
        qry = {};
    }
    var query = JSON.parse(req.query.query);
    var filter = JSON.parse(req.query.filter);

    qry.$or = [];
    var tmp = [];

    if (filter.position) {
        tmp.push({ Position: filter.position });
    }

    if (filter.team) {
        tmp.push({ MLBTeam: filter.team });
    }

    if (filter.level) {
        tmp.push({ Level: filter.level });
    }

    if (tmp.length == 0) {
        qry = {};
    }
    else if (tmp.length == 1) {
        if (req.query.q == "") {
            qry = tmp[0];
        }
        else {
            qry.$or = [tmp[0]]
        }
    }
    else {
        qry = { $or: [] };
        qry.$or.push({ $and: tmp });
    }

    var limit = query.limit;
    var page = query.page;
    var sort = [];
    sort[query.sort] = query.dir;

    Player.count(qry, function (err, count) {

        Player.find(qry, function (err, players) {
            if (err) {
                res.status(200).json({ status: 200, msg: err });
            }
            else if (players) {

                res.status(200).json({ status: 200, players: players, pages: count / limit, count: count });
            }
            else {
                res.status(200).json({ status: 200, msg: "not found" });
            }
        }).skip(parseInt(page) * parseInt(limit)).sort(sort).limit(limit);

    });
});

router.get('/api/player/positions/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            Player.distinct("Position", function (err, positions) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    var filtered = positions.filter(function (value) {
                        return value != "";
                    });
                    res.status(200).json({ status: 200, msg: "", positions: filtered });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.get('/api/player/levels/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            Player.distinct("Level", function (err, positions) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    var filtered = positions.filter(function (value) {
                        return value != "";
                    });
                    res.status(200).json({ status: 200, msg: "", levels: filtered });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.post("/api/player/admin/update", function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            Player.findByIdAndUpdate(req.body._id, req.body, function (err, result) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    res.status(200).json({ status: 200, msg: "success" });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.get('/api/player/stats/ytd', function (req, res) {
    var mlbId = req.query.mid;
    var date = req.query.date;

    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            Player.findOne({ MlbId: mlbId }, function (err, player) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    res.status(200).json({ status: 200, msg: "success", player: player });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.get('/api/player/mlbid/get', function (req, res) {
    var mlbId = req.query.mid;

    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            Player.findOne({ MlbId: mlbId }, function (err, player) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    res.status(200).json({ status: 200, msg: "success", player: player });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.get('/api/player/year-end-stats', function (req, res) {
    var pname = req.query.pname;

    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            YtdStats.find({ Name: pname }, function (err, playerArray) {
                if (err) {
                    res.status(500).json(err);
                }
                else {

                    if (playerArray && playerArray.length > 0) {
                        var Header = [];
                        var Values = [];
                        var hasOverallPitching = false;

                        for (i = 0; i < playerArray.length; i++) {
                            var player = playerArray[i]._doc;

                            if (player.Defense) {
                                Header.push("Position");
                                Values.push(player.Defense.POS);
                            }

                            if (player.POverall) {
                                hasOverallPitching = player.POverall['Overall rPW'];
                            }
                            if (player.SP) {
                                pitch = player.SP;

                                Header.push("Starting Pitching");
                                Values.push("");
                                Header.push("GS");
                                Values.push(pitch.GS);
                                Header.push("IP");
                                Values.push(pitch.IP);
                                Header.push("Hits (SP)");
                                Values.push(pitch.TH);
                                Header.push("ER (SP)");
                                Values.push(pitch.ER);
                                Header.push("BB (SP)");
                                Values.push(pitch.BB);
                                Header.push("HR (SP)");
                                Values.push(pitch.HR);
                                Header.push("SO  (SP)");
                                Values.push(pitch.SO);
                                Header.push("BAvg. Against  (SP)");
                                Values.push(pitch.AVG);
                                Header.push("ERA  (SP)");
                                Values.push(pitch.ERA);
                                /*
                                if (player.POverall) {
                                    Header.push("rPW");
                                    Values.push(player.POverall['rPW (SP)']);
                                }
                                */
                                Header.push("rPW ");
                                Values.push(pitch.rPW);

                            }

                            if (player.RP) {
                                pitch = player.RP;

                                Header.push("Relief Pitching");
                                Values.push("");
                                Header.push("G (RP)");
                                if (pitch.G) {
                                    Values.push(pitch.G);
                                    Header.push("IP (RP)");
                                    Values.push(pitch.IP);
                                    Header.push("Hits (RP)");
                                    Values.push(pitch.H);
                                    Header.push("ER (RP)");
                                    Values.push(pitch.ER);
                                    Header.push("BB (RP)");
                                    Values.push(pitch.BB);
                                    Header.push("HR (RP)");
                                    Values.push(pitch.HR);
                                    Header.push("SO (RP)");
                                    Values.push(pitch.SO);
                                    Header.push("BAvg. Against (RP)");
                                    Values.push(pitch.AVG);
                                    Header.push("ERA (RP)");
                                    Values.push(pitch.ERA);
                                    /*
                                    if (player.POverall) {
                                        Header.push("rPW ");
                                        Values.push(player.POverall['rPW (RP)']);
                                    }
                                    */
                                    Header.push("rPW (RP)");
                                    Values.push(pitch['rPW']);
                                }
                            }
                            if (player.Batter) {
                                Header.push("Batting");
                                Values.push("");
                                Header.push("AB");
                                Values.push(player.Batter.AB);
                                Header.push("H");
                                Values.push(player.Batter.H);
                                Header.push("BB");
                                Values.push(player.Batter.BB);
                                Header.push("HR");
                                Values.push(player.Batter.HR);
                                Header.push("RBI");
                                Values.push(player.Batter.RBI);
                                Header.push("BAvg.");
                                Values.push(player.Batter.AVG);
                                Header.push("OPS");
                                Values.push(player.Batter.OPS);
                                Header.push("SLG");
                                Values.push(player.Batter.SLG);
                            }
                        }
                        if (hasOverallPitching) {
                            Header.push("Overall Pitching");
                            Values.push("");
                            Header.push("rPW");
                            Values.push(hasOverallPitching);
                        }
                        res.status(200).json({ status: 200, msg: "success", player: player, headers: Header, values: Values });
                    } else {
                        res.status(500).json(err);
                    }
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.get('/api/player/updateSalary', function (req, res) {

    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {

            var player = JSON.parse(req.query.player);
            var leagueId = req.query.lid;
            var teamId = req.query.tid;

            // grab the roster!
            var query = {
                $and: [
                    { LeagueId: leagueId },
                    { TeamId: teamId }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: "Roster location error", });
                    return;
                } else {
                    var majorIndex = -1;
                    var minorIndex = -1;
                    var pid = player.PlayerId;

                    editExtras = JSON.parse(req.query.editExtras);
                    player.rSalary.AAV = editExtras.AAV;

                    player.MlbId = editExtras.MlbId;
                    if (player.MlbId != '')
                        player.Image = "http://mlb.mlb.com/mlb/images/players/head_shot/" + player.MlbId + ".jpg";

                    if (editExtras.Position != '') {
                        player.Position = editExtras.Position;
                    }
                    if (editExtras.Age && editExtras.Age != '') {
                        player.Age = editExtras.Age;
                    }
                    if (editExtras.DOB && editExtras.DOB != '') {
                        player.DOB = editExtras.DOB;
                    }
                    if (editExtras.FirstName && editExtras.FirstName != '') {
                        player.FirstName = editExtras.FirstName;
                    }
                    if (editExtras.LastName && editExtras.LastName != '') {
                        player.LastName = editExtras.LastName;
                    }
                    if (editExtras.FullName && editExtras.FullName != '') {
                        player.FullName = editExtras.FullName;
                    }
                    if (editExtras.Bats && editExtras.Bats != '') {
                        player.Bats = editExtras.Bats;
                    }
                    if (editExtras.Throws && editExtras.Throws != '') {
                        player.Throws = editExtras.Throws;
                    }
                    if (editExtras.SigningYear && editExtras.SigningYear != '') {
                        player.SigningYear = editExtras.SigningYear;
                    }
                    if (editExtras.MLS && editExtras.MLS != '') {
                        player.MLS = editExtras.MLS;
                    }
                    if (editExtras.Options && editExtras.Options != '') {
                        player.Options.Options = editExtras.Options;
                    }
                    if (editExtras.PriorOR && editExtras.PriorOR != '') {
                        player.Options.PriorOR = editExtras.PriorOR;
                    }
                    if (editExtras.Rule5 && editExtras.Rule5 != '') {
                        player.Options.Rule5 = editExtras.Rule5;
                    }
                    if (editExtras.DraftExcluded && editExtras.DraftExcluded != '') {
                        player.Options.DraftExcluded = editExtras.DraftExcluded;
                    }
                    if (editExtras.No40 && editExtras.No40 != '') {
                        player.Options.No40 = editExtras.No40;
                    }
                    var lastFirstChar = '$';
                    for (i = 0, y = 2018; i < 10; i++ , y++) {

                        var firstChar = req.query.salaries[i].charAt(0);
                        if (firstChar == 'F' || firstChar == 'A') {
                            player.rSalary[y].Salary = req.query.salaries[i];
                            lastFirstChar = firstChar;
                        } else if (firstChar == '$' && lastFirstChar == '$') {
                            player.rSalary[y].Salary = req.query.salaries[i];
                        } else {
                            player.rSalary[y].Salary = "";
                        }


                    }
                    for (i = 0; i < roster.FortyManNL.length; i++) {
                        if (roster.FortyManNL[i].PlayerId == pid) {
                            majorIndex = i;
                            roster.FortyManNL[i] = player;
                            roster.markModified("FortyManNL");
                            break;
                        }
                    }
                    if (majorIndex == -1) {
                        for (i = 0; i < roster.NonRoster.length; i++) {
                            if (roster.NonRoster[i].PlayerId == pid) {
                                minorIndex = i;
                                roster.NonRoster[i] = player;
                                roster.markModified("NonRoster");
                                break;
                            }
                        }
                    }
                    if (majorIndex == -1 && minorIndex == -1) {
                        res.status(500).json({ status: 500, msg: "Couldn't find player by original PlayerId", });
                    } else {
                        // replaced player
                        roster.save(function (err, roster) {
                            if (err) {
                                res.status(500).json({ status: 500, msg: "Couldn't save player in roster", });
                            } else {
                                res.status(200).json({ status: 200, msg: "success", });
                            }
                        })

                    }
                }
            })
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ****************** TODO: Make this generic for any year
// currently written assuming NO 2017 salary and new 2018 salary
//
router.post('/api/player/savePreArb', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {

            var playerId = req.body.pid;
            var leagueId = req.body.lid;
            var teamId = req.body.tid;
            var newSalary2017 = "$" + req.body.salary2017;
            var newSalary2018 = "$" + req.body.salary2018;

            // grab the roster!
            var query = {
                $and: [
                    { LeagueId: leagueId },
                    { TeamId: teamId }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: "Roster location error", });
                    return;
                } else {
                    var majorIndex = -1;
                    var minorIndex = -1;


                    var player = null;
                    for (i = 0; i < roster.FortyManNL.length; i++) {
                        if (roster.FortyManNL[i].PlayerId == playerId) {
                            majorIndex = i;
                            roster.FortyManNL[i].rSalary[2017] = {
                                Salary: newSalary2017,
                                Buyout: "",
                                Contract: ""
                            };
                            roster.FortyManNL[i].rSalary[2018] = {
                                Salary: newSalary2018,
                                Buyout: "",
                                Contract: ""
                            };
                            roster.FortyManNL[i].rSalary["AAV"] = newSalary2018;
                            roster.markModified("FortyManNL");
                            break;
                        }
                    }
                    if (majorIndex == -1) {
                        for (i = 0; i < roster.NonRoster.length; i++) {
                            if (roster.NonRoster[i].PlayerId == playerId) {

                                minorIndex = i;
                                roster.NonRoster[i].rSalary[2017] = {
                                    Salary: newSalary2017,
                                    Buyout: "",
                                    Contract: ""
                                };
                                roster.NonRoster[i].rSalary[2018] = {
                                    Salary: newSalary2018,
                                    Buyout: "",
                                    Contract: ""
                                };
                                roster.NonRoster[i].rSalary["AAV"] = newSalary2018;

                                roster.markModified("NonRoster");
                                break;
                            }
                        }
                    }
                    if (majorIndex == -1 && minorIndex == -1) {
                        res.status(500).json({ status: 500, msg: "Couldn't find player by original PlayerId", });
                    } else {
                        // replaced player

                        roster.save(function (err, roster) {
                            if (err) {
                                res.status(500).json({ status: 500, msg: "Couldn't save player in roster", });
                            } else {
                                res.status(200).json({ status: 200, msg: "success", });
                            }
                        })

                    }
                }
            })
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

module.exports = router;