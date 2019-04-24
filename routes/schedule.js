// import { EWOULDBLOCK } from 'constants';

var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var Api = mongoose.model('Api');
var Schedule = mongoose.model('Schedule');
var League   = mongoose.model('League');


router.get('/api/schedule/get', function (req, res) {
    utils.validate(req, function (isValid, user) {

        Schedule.findOne({ LeagueId: req.query.lid }, function (err, schedule) {
            if (err) {
                res.status(401).json({ status: 401, msg: err.msg});
            } else {
                for( i=0; i<schedule.Games.length; i++) {
                    if( schedule.Games[i].CalendarDay == 94.2) {
                        var day = schedule.Games[i].CalendarDay;
                    }
                }
                res.status(200).json({ status: 200, msg: "League schedule retrieved", schedule: schedule });
            }
        })
        /*
        if (isValid) {
        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
        */

    });
});

router.get('/api/schedule/getGameDay', function (req, res) {
    utils.validate(req, function (isValid, user) {

        query = [
            {$match: {LeagueId: req.query.lid}} ,
            {$project: {
                 Games: {
                    $filter: {
                       input: "$Games",
                       as: "game",
                       cond: { $eq: [ "$$game.simpleDate" , req.query.date ] }
                    }
                 }
              }
              }
        ];
        Schedule.aggregate(query, function (err, schedule) {
            if (err) {
                res.status(401).json({ status: 401, msg: err.msg});
            } else {
                res.status(200).json({ status: 200, msg: "League schedule retrieved", schedule: schedule && schedule.length > 0 ? schedule[0].Games : [] });
            }
        })
        /*
        if (isValid) {
        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
        */

    });
});

// ***************************************
//
// install Schedule
//
// installs the Actual MLB Schedule into the supplied league
// including double header night caps where 
//
// ****************************************

router.post('/api/schedule/install', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var moment = require('moment-timezone');
            League.findOne({ _id: req.body.lid }, {Name:1, Teams: 1} ,function (err, league) {
                if (err) {
                    res.status(500).json({ status: 500, msg: "couldn't find league" });
                } else {
                    var query = {
                        $and: [
                            { LeagueId: "MLB" },
                            { LeagueName: "MLB" }
                        ]
                    };
                    Schedule.findOne(query, function (err, schedule) {
                        if (err) {
                            res.status(500).json({ status: 500, msg: "couldn't find MLB schedule" });
                        } else {
                            delete schedule._doc._id;

                            // stuff in the team id's based on the team names...
                            // first make an object to make this simpler
                            var teamIds = {};
                            for( t=0; t< league.Teams.length; t++ ) {
                                teamIds[ league.Teams[t].r_name] = league.Teams[t]._id; // matches Roster.TeamId
                            }

                            for( g=0; g<schedule.Games.length; g++) {
                                var cDay = moment(schedule.Games[g].start).dayOfYear();
                                if( schedule.Games[g].isDoubleHeaderNC === true) {
                                    cDay += 0.2;
                                }
                                schedule.Games[g]._doc["CalendarDay"] = cDay;
                                schedule.Games[g]._doc["homeId"] = teamIds[schedule.Games[g].home];
                                schedule.Games[g]._doc["visitId"] = teamIds[schedule.Games[g].visit];
                            }

                            // schedule.LeagueName = league.Name;
                            // schedule.LeagueId = league._id;

                            var newSchedule = new Schedule();
                            newSchedule.LeagueName = league.Name;
                            newSchedule.Season = 2017;
                            newSchedule.LeagueId = league._id;
                            newSchedule.Games = schedule.Games;
                            newSchedule.LastUpdate = new Date().toISOString();

                            delete newSchedule._doc._id;
                            newSchedule.markModified("Games");
                            Schedule.replaceOne({LeagueId:league._id, LeagueName:league.Name},newSchedule, {upsert:true}, function (err, schedule) {                         
       //                     Schedule.create(newSchedule, function (err, schedule) {
                                if (err) {
                                    res.status(500).json({ status: 500, msg: "couldn't create new schedule" });
                                } else {
                                    res.status(200).json({ status: 200, msg: "Schedule installed in league: " + league.Name });
                                }
                            })
                        }
                    })
                }
            })
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});
 
module.exports = router;