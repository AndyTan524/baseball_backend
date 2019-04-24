var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var teamHelper = require('../helpers/teamHelper');
var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var Transaction = mongoose.model('Transaction');

// ****************************************************
//
// transactions/getList
// --> get list of transactions... 
//
// ******************************************************
router.get('/api/transactions/getList', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var isAdmin = false;
            if( user.Roles.indexOf("Admin") >= 0 ) {
                isAdmin = true;
            }

            var query = { LeagueId: req.query.lid, Teams:{$elemMatch: {id: req.query.tid}}, Archived: false};
            if( !req.query.tid || req.query.tid == "All" ) {
                query = { LeagueId: req.query.lid, Archived: false};
            }
            // example query to get THIS team: {Teams:{$elemMatch: {name:"rAstros"}}}

            var page = req.query.page ? parseInt(req.query.page) : 0;
            var limit = req.query.limit ? parseInt(req.query.limit) : 50;


            Transaction.find(query, function(err,transactions){
                if( err ) {
                    res.status(401).json({ status: 401, msg: "Deal not found." });
                } else {
                    var dates = [];
                    var moment = require('moment-timezone');
                    moment.tz.setDefault("America/Los_Angeles");

                    if (!isAdmin) {
                        // don't show league office moves to users
                        for (i = (transactions.length - 1); i >= 0; i--) {
                            var transaction = transactions[i];
                            if( !transaction._doc.Headline) {
                                transactions.splice(i, 1);                                
                            } else if ( (transaction._doc.Headline.includes("In the League Office"))
                                ||(transaction._doc.Headline.includes("status to Active Roster")) ){
                                transactions.splice(i, 1);
                            }
                        }
                    }
                    transactions.forEach(function (transaction, index) {

                        var date = moment(transaction.DateUTC).format("llll").substring(0, 17);
                        if (dates.indexOf(date) == -1) {
                            dates.push(date);
                        }
                        transaction._doc.date = date;

                    })

                    Transaction.count(query, function (err, count) {
                        res.status(200).json({ status: 200, msg: "success", transactions: transactions, dates: dates, count: count, page: page, limit: limit });
                    });                     
                }
            }).sort({ DateUTC: -1 }).skip(page * limit).limit(limit);

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ****************************************************
//
// transactions/execute
// --> executes ALL moves in a transaction record - a list of one or more player or money transfers
//
// ******************************************************
router.post('/api/transactions/execute', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            Transaction.findByIdAndUpdate(req.body, function (err, result) {
                if (err) {
                    res.status(500).json({ status: 500, "msg": "create user error" });
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

// ****************************************************
//
// transactions/move one player (ADMIN and COMMISSIONER ONLY)
// --> executes a single player move either between teams
//     or between a team and the free agents
//     or between waivers and a team or free agents
//     if moving to a new team put player at same level as before
//     and send message if out of room.
// --> create transaction record of move as a transaction
//
// ******************************************************
router.post('/api/transactions/move', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            force = true;
            if (force || user.Roles.indexOf("Admin") >= 0) {
                var leagueId = req.body.lid;
                var fromTeamId = req.body.fromid;
                var toTeamId = req.body.toid;
                var player = req.body.player;
                var playerId = player.PlayerId;
                var carrySalary = false
                if(  req.body.carrySalary )
                    carrySalary = req.body.carrySalary;

                if( carrySalary ) {
                    teamHelper.movePlayerToTeamOrFA(leagueId, user, fromTeamId, "Free Agents", player, carrySalary, "The {{fromTeam}} released " + player.FullName,
                    function (responseObj) {
                        if (responseObj) {
                            res.status(responseObj.status).json(responseObj);
                            return;
                        }
                        else {
                          res.status(500).json({ status: 500, msg: "Sorry, there's a server error." });
                          return;
                        }

                    })
                } else {

                // first grab and remove player from original list
                // fromTeamId or toTeamId is "Free Agents" if that's the "team" to move them to/from
                teamHelper.removePlayer(leagueId, fromTeamId, player, function (err, msg, playerEStats) {

                    if (err) {
                        res.status(500).json({ status: 500, "msg": msg });
                    } else {
                        var removemsg = msg;
                        teamHelper.addPlayerToTeam(leagueId, toTeamId, player, fromTeamId, playerEStats, function (err, msg) {
                            if (err) {
                                res.status(500).json({ status: 500, "msg": removemsg + " - NOTE -  " + msg });
                            } else {
                                res.status(200).json({ status: 200, "msg": removemsg + " And... " + msg });

                                // now create a new transaction
                                transaction = new Transaction();
                                transaction.Type = "Transfer";
                                transaction.Status = "Complete";
                                transaction.Archived = false;
                                transaction.LeagueId = leagueId;
                                transaction.DateUTC = new Date().toISOString();
                                transaction.DealId = 0;
                                transaction.Teams = [
                                    {name: req.body.fromTeamName, id:req.body.fromid},
                                    {name: req.body.toTeamName, id: req.body.toid}
                                ];

                                 var moment = require('moment-timezone');
                                 moment.tz.setDefault("America/Los_Angeles");
                                transferDate = moment(transaction.DateUTC).format("llll");
                   
                                transaction.Headline = "TRANSFER: " + player.FullName + " transfered from " + req.body.fromTeamName + " to " + req.body.toTeamName + " on " + transferDate + ". Transfer made by " + user._doc.FirstName + " " + user._doc.LastName + " (in the League Office)";

                                Transaction.create( transaction, function(err, t){
                                    if( err) {

                                    }
                                })

                            }
                        });
                    }
                });
              }

            } else {
                // only allowed by admins
                res.status(500).json({ status: 500, "msg": "Move not executed as only Administrators and Commissioners have permsission to move a player directly." });
            }
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ****************************************************
//
// transactions/update 
// --> updates either the Headline or the Archived status (true/false)
//     or between a team and the free agents
//     or between waivers and a team or free agents
//     if moving to a new team put player at same level as before
//     and send message if out of room.
// --> create transaction record of move as a transaction
//
// ******************************************************
router.post('/api/transactions/update', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            if ( user.Roles.indexOf("Admin") >= 0) {
                var tid = req.body.tid;
                var newHeadline = req.body.headline;
                var archive = req.body.archive;

                
                var updatevalue =  {};
                if( "archive" in req.body && (archive === true || archive === false))  {
                    updatevalue = {Archived: archive};
                }
                if( newHeadline && newHeadline != "") {
                    updatevalue = {Headline: newHeadline};
                }
                Transaction.update(
                    { _id: tid }, updatevalue, function (err, newT) {
                        if (err) {
                            res.status(500).json({ status: 500, "msg": "Updating transaction failed." });

                        } else {
                            var a = newT;
                            res.status(200).json({ status: 200, "msg": "Updated transaction." });
                        }
                    });

         } else {
                // only allowed by admins
                res.status(500).json({ status: 500, "msg": "Updating Transactions only available to Administrators ." });
            }
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});



module.exports = router;