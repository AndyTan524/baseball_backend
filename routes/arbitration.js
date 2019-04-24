var express = require('express');
var router = express.Router();
var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var Api = mongoose.model('Api');
var User = mongoose.model('User');
var Content = mongoose.model('Content');
var Roster  = mongoose.model('Roster');
var Arbitration = mongoose.model('Arbitration');

// 
//*************** move the arbitraiton spreadsheet in contents to this league
//
router.post('/api/arbitration/addContentToLeague', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var lid = req.body.lid
            var contentname = "arbitrationTracker";

            Content.findOne({ name: contentname }, function (err, content) {
                if (err) {
                    res.status(500).json({ status: 500, msg: "Tracker not found" });
                } else {

                    // first REMOVE ALL THE ARBITATION PLAYERS!

                    Arbitration.remove({LeagueId: lid}, function (err, result) {
                        var source = content.content.dataset;
                        var players = [];
                        var category = "";
                        var now = new Date().toISOString();

                        // skip the first two lines
                        for (i = 2; i < source.length; i++) {
                            nextplayer = source[i];
                            if (nextplayer[0] == "category") {
                                category = nextplayer[1];
                                // that's it for this source row
                            } else {
                                // next source row should be a player!
                                var at = new Arbitration();
                                at.LeagueId = lid;
                                at.LastUpdateUTC = now;
                                at.Category = category;
                                at.FullName = nextplayer[1];
                                at.Position = nextplayer[2];
                                at.MLS = nextplayer[3];
                                at.rTeamName = nextplayer[4];
                                at.LastYearSalary = nextplayer[5];
                                at.ProjectedRaise = nextplayer[6];
                                at.NextYearForecastedHigh = nextplayer[7];
                                at.NextYearForecastedLow = nextplayer[8];
                                at.AgreedTerms = nextplayer[9];
                                at.MultiYear = nextplayer[10];
                                at.NonTender = nextplayer[11];
                                at.ClubSubmission = nextplayer[12];
                                at.PlayerSubmission = nextplayer[13];

                                if( nextplayer[14]) {
                                    at["MlbId"] = nextplayer[14];
                                }

                                players.push(at);
                            }
                        }

                        Arbitration.insertMany(players, function (err, response) {
                            if (err) {
                                res.status(500).json({ status: 500, msg: "Tracker not inserted" });
                            } else {
                                res.json({ 'status': 200, msg: 'arbitration tracker moved to league' });
                            }
                        })
                    });
                }

            });
        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }

    });
});


// 
//*************** get the arbitration tracker for this league and the categories
//
router.get('/api/arbitration/getPlayers', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var lid = req.query.lid
            var season = 2018;
            var teamId = req.query.tid;

            Arbitration.find({ LeagueId: lid}, function(err, players){
                if( err ) {
                    res.status(500).json({ status: 500, msg: "Tracker not found" });
                } else {
                    // create the category list to return
                    var categories = [
                        "5+ CATCHER CATEGORY",
                        "5+ INFIELDER CATEGORY",
                        "5+ OUTFIELDER CATEGORY",
                        "5+ RELIEF PITCHER CATEGORY",
                        "5+ STARTING PITCHER CATEGORY",
                        "4+ CATCHER CATEGORY",
                        "4+ INFIELDER CATEGORY",
                        "4+ OUTFIELDER CATEGORY",
                        "4+ RELIEF PITCHER CATEGORY",
                        "4+ STARTING PITCHER CATEGORY",
                        "3+ CATCHER CATEGORY",
                        "3+ INFIELDER CATEGORY",
                        "3+ OUTFIELDER CATEGORY",
                        "3+ RELIEF PITCHER CATEGORY",
                        "3+ STARTING PITCHER CATEGORY",
                        "SUPER TWO CATCHER CATEGORY",
                        "SUPER TWO INFIELDER CATEGORY",
                        "SUPER TWO OUTFIELDER CATEGORY",
                        "SUPER TWO RELIEF PITCHER CATEGORY",
                      /*  "SUPER TWO STARTING PITCHER CATEGORY", */

                    ];
                    for(i=0; i<players.length; i++ ) {
                        if( categories.indexOf( players[i].Category) == -1) {
                            categories.push( players[i].Category);
                        }
                    }
                    if( user.Roles.indexOf("Admin") >= 0 ) {
                           res.json({ 'status': 200, players: players, categories: categories, season: season });
                    } else {
                        // get the roster for this team and only notate those players which are on both lists!
                        Roster.findOne( {TeamId: teamId}, function(err, roster){
                            if( err || roster == null) {

                            } else {
                            for( i=0; i<roster.FortyManNL.length; i++) {
                                name = roster.FortyManNL[i].FullName;
                                for( x=0; x<players.length; x++) {
                                    if( players[x].FullName == name ) {
                                        players[x]._doc["onTeam"] = true;
                                        players[x]._doc["PlayerId"] = roster.FortyManNL[i].PlayerId;
                                        break;
                                    }
                                }
                            }
                            for( i=0; i<roster.NonRoster.length; i++) {
                                name = roster.NonRoster[i].FullName;
                                for( x=0; x<players.length; x++) {
                                    if( players[x].FullName == name ) {
                                        players[x]._doc["onTeam"] = true;
                                        players[x]._doc["PlayerId"] = roster.NonRoster[i].PlayerId;
                                        break;
                                    }
                                }
                            }
                        }
                            res.json({ 'status': 200, players: players, categories: categories, season: season });                           
                        })
                    }

                }

            });

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }


    });
});


module.exports = router;