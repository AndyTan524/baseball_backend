var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var teamHelper = require('../helpers/teamHelper');
var playerStatus = require('../helpers/playerStatus');
var msgType = require('../helpers/messageTypes');
var dealHelper = require('../helpers/dealHelper');
var messageHelper = require('../helpers/messageHelper');
var leagueHelper = require('../helpers/leagueHelper');
var rosterLineupHelper = require('../helpers/rosterLineupHelper');
var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var Deal = mongoose.model('Deal');
var League = mongoose.model('League');
var Roster = mongoose.model('Roster');
var Transaction = mongoose.model('Transaction');
var Arbitration = mongoose.model('Arbitration');
var RosterLineup = mongoose.model('RosterLineup');

// ****************************************************
//
// deals/get
// --> gets one deal record
//
// ******************************************************
router.get('/api/deals/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            Deal.findOne({ _id: req.query.dealId }, function (err, deal) {
                if (err) {
                    res.status(401).json({ status: 401, msg: "Deal not found." });
                } else {
                    res.status(200).json({ status: 200, msg: "success", deal: deal });
                }
            });

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ****************************************************
//
// deals/getList
// --> get list of deals... 
//
// ******************************************************
router.get('/api/deals/getList', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var moment = require('moment-timezone');
            moment.tz.setDefault("America/Los_Angeles");

            var query = { LeagueId: req.query.lid, Teams: { $elemMatch: { id: req.query.tid } }, ChildDealId: "" };
            if (!req.query.tid || (req.query.tid == "All" && user.Roles.indexOf("Admin") >= 0)) {
                query = { LeagueId: req.query.lid, ChildDealId: "" };
            }

            var page = req.query.page ? parseInt(req.query.page) : 0;
            var limit = req.query.limit ? parseInt(req.query.limit) : 25;

            // example query to get THIS team: {Teams:{$elemMatch: {name:"rAstros"}}}
            Deal.find(query, function (err, deals) {
                if (err) {
                    res.status(401).json({ status: 401, msg: "Deal not found." });
                } else {
                    var dates = [];
                    var status = [];

                    deals.forEach(function (deal, index) {
                        dates.push(moment(deal.DateUpdatedUTC).format("llll"));

                        // figure out the status message
                        var message = "A response/comment/question has been sent.";
                        if (deal.Status == "Rejected") {
                            message = "DEAL REJECTED."
                        } else if (deal.Status == "Approved") {
                            message = "Deal agreed to, approved by League Office and executed."

                        } else if (deal.Status == "Proposed") {
                            message = "New proposal."

                        } else if (deal.Status == "Updated") {
                            message = "Deal has been updated."

                        } else if (deal.Status == "Countered") {
                            message = "A counter offer has been proposed."

                        } else if (deal.Status == "Agreed") {
                            message = "Terms agreed to and awaiting League Office approval."

                        } else if (deal.Status == "Withdrawn") {
                            message = "DEAL WITHDRAWN."

                        }

                        status.push(message);
                    })
                    Deal.count(query, function (err, count) {
                        res.status(200).json({ status: 200, msg: "success", deals: deals, dates: dates, statuses: status, count: count, page: page, limit: limit });
                    });
                    
                }
            }).sort({ DateUpdatedUTC: -1 }).skip(page * limit).limit(limit);

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});


// ****************************************************
//
// deals/getByFilter
// --> get list of deals... filtered by a query
//
// ******************************************************
router.get('/api/deals/getByFilter', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var moment = require('moment-timezone');
            moment.tz.setDefault("America/Los_Angeles");

            var query = { LeagueId: req.query.lid, Teams: { $elemMatch: { id: req.query.tid } }, ChildDealId: "" };
            if (!req.query.tid || (req.query.tid == "All" && user.Roles.indexOf("Admin") >= 0)) {
                query = { LeagueId: req.query.lid, ChildDealId: "" };
            }
            if (req.query.name != "") {
                var name = req.query.name;
                var nameQuery = { "Players.FullName": { $regex: ".*" + name + "*", $options: "ix" } };
                query = { LeagueId: req.query.lid, ChildDealId: "", "Players.FullName": { $regex: ".*" + name + "*", $options: "ix" } };
            }

            if (req.query.teamname != "") {
                query["Teams.name"] = req.query.teamname;

            }
            if (req.query.type && req.query.type != "All") {
                if (req.query.type != "") {
                    query["Type"] = req.query.type;
                }
            }

            var page = req.query.page ? parseInt(req.query.page) : 0;
            var limit = req.query.limit ? parseInt(req.query.limit) : 25;

            // example query to get THIS team: {Teams:{$elemMatch: {name:"rAstros"}}}
            Deal.find(query, function (err, deals) {
                if (err) {
                    res.status(401).json({ status: 401, msg: "Deal not found." });
                } else {
                    var dates = [];
                    var status = [];

                    deals.forEach(function (deal, index) {
                        dates.push(moment(deal.DateUpdatedUTC).format("llll"));

                        // figure out the status message
                        var message = "A response/comment/question has been sent.";
                        if (deal.Status == "Rejected") {
                            message = "DEAL REJECTED."
                        } else if (deal.Status == "Approved") {
                            message = "Deal agreed to, approved by League Office and executed."

                        } else if (deal.Status == "Proposed") {
                            message = "New proposal."

                        } else if (deal.Status == "Updated") {
                            message = "Deal has been updated."

                        } else if (deal.Status == "Countered") {
                            message = "A counter offer has been proposed."

                        } else if (deal.Status == "Agreed") {
                            message = "Terms agreed to and awaiting League Office approval."

                        } else if (deal.Status == "Withdrawn") {
                            message = "DEAL WITHDRAWN."

                        }

                        status.push(message);
                    })
                    Deal.count(query, function (err, count) {
                        res.status(200).json({ status: 200, msg: "success", deals: deals, dates: dates, statuses: status, count: count, page: page, limit: limit });
                    });
                }
            }).sort({ DateUpdatedUTC: -1 }).skip(page * limit).limit(limit);

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});
// ****************************************************
//
// deals/get
// --> gets one deal record
//
// ******************************************************
router.get('/api/deals/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            Deal.findOne({ _id: req.query.dealId }, function (err, deal) {
                if (err) {
                    res.status(401).json({ status: 401, msg: "Deal not found." });
                } else {
                    res.status(200).json({ status: 200, msg: "success", deal: deal });
                }
            });

        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});


// ****************************************************
//
// deals/create
// --> create a deal record 
//
// ******************************************************
router.post('/api/deals/create', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            req.body.deal.Teams[0].approved = true;
            // the deal is coming in with the correct format.
            Deal.create(req.body.deal, function (err, deal) {
                if (err) {
                    res.status(500).json({ status: 500, "msg": "create user error" });
                }
                else {
                    res.status(200).json({ status: 200, msg: "Success! Proposal submitted.", dealId: deal._id });
                    var dealId = deal._id.toString();
                    deal.update({ $set: { OriginalDealId: dealId } }, function (err, response) {
                        if (err) {
                            console.log("couldn't update");
                        } else {
                            // console.log("updated id");
                        }

                        // regardless, send message to interested parties.
                        dealHelper.notifyDealParties(deal, user, true);
                    });
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
// deals/update
// --> update a deal record 
// --> keep track of parents and children
// 
// user actions:
//      Comment
//      Rejected
//      Withdrawn
//      Updated
//      Countered
//      Agreed
//      Approved (admins only)
//
// end statuses:
//     Proposed  (after new offer)
//     Countered (after new offer)
//     Updated   (after updated offer)
//     Agreed (if both parties in a trade agree to terms)
//     Rejected (if either party rejects the deal)
//     Withdrawn (if either pary withdraws their proposed deal)
//     Approved (league office has approved and system has executed the deal)
//
// ******************************************************
router.post('/api/deals/update', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var TestApproved = false; // if false, then don't do tests, do the real thing.

            // first, grab the parent deal
            Deal.findOne({ _id: req.body.deal._id }, function (err, parentDeal) {
                if (err) {
                    res.status(401).json({ status: 401, "msg": "Sorry, there's a problem reading original deal" });
                }
                else {
                    // the deal is coming in with the correct format.
                    // but need to create the parent child, relationship
                    // so it's new!
                    var updatedDeal = req.body.deal;
                    req.body.deal.ParentDealId = updatedDeal._id.toString();
                    delete req.body.deal._id;

                    updatedDeal.DateUpdatedUTC = new Date().toISOString();
                    updatedDeal.SubmittedBy = user.FirstName + " " + user.LastName;
                    updatedDeal.SubmittedById = user._id.toString();
                    updatedDeal.SubmitterTeam = req.body.teamname;


                    // make adjustments based on the user action
                    var action = req.body.action;
                    var isAdmin = false;
                    var teamIndex = false;
                    if (user.Roles.indexOf("Admin") >= 0) {
                        isAdmin = true;
                        updatedDeal.LeagueRepresentative.action = action;
                        updatedDeal.LeagueRepresentative.name = user.FirstName + " " + user.LastName;
                        updatedDeal.LeagueRepresentative.id = updatedDeal.SubmittedById;
                    } else {

                        if (!isAdmin && updatedDeal.Teams[0].name == updatedDeal.SubmitterTeam) {
                            teamIndex = 0;
                        } else if (!isAdmin && updatedDeal.Teams[1].name == updatedDeal.SubmitterTeam) {
                            teamIndex = 1;
                        }
                        updatedDeal.Teams[teamIndex].representative.action = action;
                        updatedDeal.Teams[teamIndex].representative.name = user.FirstName + " " + user.LastName;
                        updatedDeal.Teams[teamIndex].representative.id = updatedDeal.SubmittedById;

                    }


                    // *******************************
                    //
                    // respond to deal actions
                    //
                    // *******************************
                    if (action == "Approved") {
                        if (isAdmin) {
                            if (!TestApproved)
                                updatedDeal.Status = "Approved";
                        } else {
                            // not allowed
                        }
                    } else if (action == "Agreed") {
                        // pass along to the admin.
                        updatedDeal.Status = "Agreed";
                        if (teamIndex) {
                            updatedDeal.Teams[teamIndex].approved = true;
                        }
                        var otherTeamIndex = 1 - teamIndex;
                        if (updatedDeal.Teams[otherTeamIndex]) {
                            if (updatedDeal.Teams[otherTeamIndex].approved == false) {
                                updatedDeal.Status = "Updated";
                            }
                        }

                    } else if (action == "Comment") {
                        // no changes to status or who's doing what
                        // nothing has changed except the deal points sent and the date
                        updatedDeal.Status = "Updated";

                    } else if (action == "Updated") {
                        // no changes to status or who's doing what
                        // nothing has changed except the deal points sent and the date
                        updatedDeal.Status = "Updated";
                        if (teamIndex)
                            updatedDeal.Teams[teamIndex].approved = true;

                    } else if (action == "Rejected" || action == "Withdrawn") {
                        // 
                        updatedDeal.Status = action;
                        if (teamIndex) {
                            updatedDeal.Teams[teamIndex].approved = false;
                        }
                    } else if (action == "Countered") {
                        // switch who is the submitter and the submitting team
                        updatedDeal.Status = "Countered";
                        if (teamIndex) {
                            updatedDeal.Teams[teamIndex].approved = true;
                            var otherTeamIndex = 1 - teamIndex;
                            if (updatedDeal.Teams[otherTeamIndex]) {
                                updatedDeal.Teams[otherTeamIndex].approved = false;
                            }
                        }
                    }

                    // *****************************
                    //
                    // create the updated "child" deal
                    //
                    // *****************************
                    Deal.create(req.body.deal, function (err, childDeal) {
                        if (err) {
                            res.status(401).json({ status: 401, "msg": "Could not create updated deal" });
                        }
                        else {
                            var moment = require('moment-timezone');
                            moment.tz.setDefault("America/Los_Angeles");

                            // created.. now need to update original deal to have a child
                            if (childDeal.Status != "Approved") {
                                res.status(200).json({ status: 200, msg: "Success! Proposal updated.", dealId: childDeal._id, newstatus: childDeal.Status });
                            }

                            // ************ update the parent deal to point to the child deal
                            parentDeal.update({ $set: { ChildDealId: childDeal._doc._id.toString() } }, function (err, response) {
                                if (err) {
                                    console.log("couldn't update");
                                } else {
                                    // console.log("updated id");
                                }

                                // *************************************
                                //
                                // send messages to interested parties
                                //
                                // *************************************
                                // regardless, send message to interested parties.
                                // false means it's not a new deal, but an update
                                if (childDeal.Status != "Approved") {
                                    dealHelper.notifyDealParties(childDeal, user, false);
                                }

                                // *************************************************
                                //
                                // if it's an approved deal, execute the deal!
                                //
                                // *************************************************
                                if (childDeal.Status == "Approved" || TestApproved) {

                                    // first, create the transaction....
                                    // ...create a new transaction
                                    var transaction = new Transaction();
                                    transaction.Type = childDeal.Type;
                                    transaction.Status = "Complete";
                                    transaction.Archived = false;
                                    transaction.LeagueId = childDeal.LeagueId;
                                    transaction.DateUTC = new Date().toISOString();
                                    transaction.DealId = childDeal._id.toString();
                                    transaction.Teams[0] =
                                        {
                                            name: childDeal.Teams[0].name,
                                            id: childDeal.Teams[0].id,
                                            playerList: "",
                                            consisderations: false
                                        };

                                    if (childDeal.Teams[1]) {
                                        transaction.Teams[1] =
                                            {
                                                name: childDeal.Teams[1].name,
                                                id: childDeal.Teams[1].id,
                                                playerList: "",
                                                considerations: false
                                            };
                                    }

                                    transferDate = moment(transaction.DateUTC).format("llll");

                                    var considerations = "";
                                    var playerlist = [];

                                    if (childDeal.Type == "Trade") {

                                        // ****************************************
                                        //
                                        // transaction data for trade
                                        //
                                        // ****************************************
                                        childDeal.Teams[0].dealPoints.forEach(function (dp, index) {
                                            if (dp.type != "Player") {
                                                considerations = " with considerations";
                                            } else {
                                                if (dp.player.FullName != "")
                                                    playerlist.push(dp.player.FullName);
                                            }
                                        });
                                        var tradedplayers = playerlist.join(", ");
                                        transaction.Headline = "The " + childDeal.Teams[0].name + " traded " + tradedplayers + considerations + " to the " + childDeal.Teams[1].name + " for ";
                                        transaction.Teams[0].playerList = tradedplayers;
                                        if (considerations != "") {
                                            transaction.Teams[0].considerations = true;
                                        }

                                        var considerations = "";
                                        var playerlist = [];
                                        childDeal.Teams[1].dealPoints.forEach(function (dp, index) {
                                            if (dp.type != "Player") {
                                                considerations = " and considerations";
                                            } else {
                                                if (dp.player.FullName != "")
                                                    playerlist.push(dp.player.FullName);
                                            }
                                        });
                                        var tradedplayers = playerlist.join(", ");
                                        transaction.Teams[1].playerList = tradedplayers;
                                        if (considerations != "") {
                                            transaction.Teams[1].considerations = true;
                                        }
                                        transaction.Headline = transaction.Headline + tradedplayers + considerations + ".";

                                    } else if (childDeal.Type == "SignFA" || childDeal.Type == "IntlFA") {

                                        // ****************************************
                                        //
                                        // transaction data for signing FA
                                        //
                                        // ****************************************

                                        if (childDeal.Teams[0].dealPoints.level == "Major") {
                                            transaction.Headline = "The " + childDeal.Teams[0].name + " agreed to terms and signed " + childDeal.Players[0].FullName + " to a " + childDeal.Teams[0].dealPoints.years + " year contract";
                                        }
                                        else {
                                            if (childDeal.Type == "SignFA") {
                                                transaction.Headline = "The " + childDeal.Teams[0].name + " signed " + childDeal.Players[0].FullName + " to a minor league contract for the 2018 season";
                                            }
                                            if (childDeal.Type == "IntlFA") {
                                                transaction.Headline = "The " + childDeal.Teams[0].name + " signed international free agent " + childDeal.Players[0].FullName + " to a minor league contract";

                                            }

                                        }
                                        transaction.Teams[0].playerList = childDeal.Players[0].FullName;

                                        transaction.Headline += ".";

                                    } else if (childDeal.Type == "Arbitration") {
                                        transaction.Headline = childDeal.Players[0].FullName + " has signed a new contract with the " + childDeal.Teams[0].name + " after agreeing to terms.";
                                        if (childDeal.Teams[0].dealPoints.arbType == "NonTender") {
                                            transaction.Headline = "The " + childDeal.Teams[0].name + " non-tendered " + childDeal.Players[0].FullName + ".";

                                        }
                                    }

                                    Transaction.create(transaction, function (err, newTransaction) {
                                        if (err) {

                                        }

                                        // ***********************************************
                                        //
                                        // Transaction done, do the deal moves
                                        // finally, move the players and money around!
                                        // grab the league...
                                        //
                                        // ************************************************
                                        League.findOne({ _id: req.body.lid }, function (err, league) {
                                            if (err) {

                                            } else {
                                                var rlist = [childDeal.Teams[0].id];
                                                if (childDeal.Type == "Trade") {
                                                    rlist.push(childDeal.Teams[1].id);
                                                }
                                                var query = { LeagueId: req.body.lid, TeamId: { $in: rlist } }
                                                Roster.find(query, function (err, rosters) {
                                                    if (err) {

                                                    } else {

                                                        // alert all interested parties...
                                                        // find the interested parties in this league about a player situation-change
                                                        // note oldStatus and newStatus are English-language versions of the status change
                                                        // note if oldStatus == "msg", then newStatus is the message to send.
                                                        var currentTId = childDeal.Teams[0].id;

                                                        // this also works for arbirations here...
                                                        var newTId = "Free Agents";
                                                        if (childDeal.Type == "Trade") {
                                                            newTId = childDeal.Teams[1].id;
                                                        }

                                                        messageHelper.alertInterestedParties(user, req.body.lid, newTransaction.Headline,
                                                            currentTId, childDeal.Teams[0].name, newTId, null,
                                                            "msg", newTransaction.Headline,
                                                            false, function () {


                                                                // ****************************************
                                                                //
                                                                // execute a Free Agent signing
                                                                // or update a player's arbitration contract
                                                                //
                                                                // ****************************************
                                                                if (childDeal.Type == "SignFA" || childDeal.Type == "IntlFA" || childDeal.Type == "Arbitration") {
                                                                    var faPlayer;
                                                                    var faId = false;
                                                                    if (childDeal.Players[0].PlayerId)
                                                                        faId = childDeal.Players[0].PlayerId;
                                                                    var faName = childDeal.Players[0].FullName;
                                                                    var arbType = false;

                                                                    var updateDepthChart = false;

                                                                    if (childDeal.Type == "Arbitration") {
                                                                        arbType = childDeal.Teams[0].dealPoints.arbType;

                                                                        // find the player and which list he's on.
                                                                        var majorIndex = -1;
                                                                        var minorIndex = -1;
                                                                        for (i = 0; i < rosters[0].FortyManNL.length; i++) {
                                                                            if (rosters[0].FortyManNL[i].PlayerId == faId ||
                                                                                (!faId && rosters[0].FortyManNL[i].FullName == faName)) {
                                                                                majorIndex = i;
                                                                                faPlayer = rosters[0].FortyManNL[i];

                                                                                if (arbType == "NonTender") {
                                                                                    // remove from this team, and put on the free agents list
                                                                                    rosters[0].FortyManNL.splice(i, 1);
                                                                                    updateDepthChart = true;
                                                                                }
                                                                                rosters[0].markModified("FortyManNL");
                                                                                break;
                                                                            }
                                                                        }
                                                                        if (majorIndex == - 1) {
                                                                            for (i = 0; i < rosters[0].NonRoster.length; i++) {
                                                                                if (rosters[0].NonRoster[i].PlayerId == faId ||
                                                                                    (!faId && rosters[0].NonRoster[i].FullName == faName)) {
                                                                                    minorIndex = i;
                                                                                    faPlayer = rosters[0].NonRoster[i];
                                                                                    rosters[0].markModified("NonRoster");

                                                                                    if (arbType == "NonTender") {
                                                                                        // remove from this team, and put on the free agents list
                                                                                        rosters[0].NonRoster.splice(i, 1);
                                                                                        updateDepthChart = true;
                                                                                    }
                                                                                    break;
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                    else {
                                                                        // must be a free agent deal

                                                                        for (i = 0; i < league.FreeAgents.length; i++) {
                                                                            if (league.FreeAgents[i].PlayerId == faId) {
                                                                                faPlayer = league.FreeAgents[i];
                                                                                if (!TestApproved) {
                                                                                    league.FreeAgents.splice(i, 1);
                                                                                    league.markModified("FreeAgents");
                                                                                }
                                                                                faPlayer.Status = playerStatus.Inactive;
                                                                                faPlayer.TradeStatus = playerStatus.Unknown;
                                                                                updateDepthChart = true;
                                                                                break;
                                                                            }
                                                                        }
                                                                    }

                                                                    // set up all of his deal points here....
                                                                    // but not if non-tendered or deal by hearing
                                                                    if (childDeal.Type == "SignFA"
                                                                        || childDeal.Type == "IntlFA"
                                                                        || (childDeal.Type == "Arbitration" && arbType != "NonTender")) {

                                                                        var deal = childDeal.Teams[0].dealPoints;

                                                                        var thisYear = 2018;
                                                                        var rSalary = {
                                                                            "FreeAgent": "",
                                                                            "Notes": "",
                                                                            "Details": "",
                                                                            "SigningBonus": "",
                                                                            "AAV": "$" + deal.AAV,
                                                                            "Contract": [],
                                                                            "Guaranteed": false, // only true if going to majors
                                                                            "ContractExtras":
                                                                                {
                                                                                    "PBs": [],
                                                                                    "Awards": [],
                                                                                    "Escalators": [],
                                                                                    "TradeBonus": "",
                                                                                    "OptionYear": "",
                                                                                    "OptionPlatformYear": "",
                                                                                    "OptionIncentives": "",
                                                                                    "OptionIncentiveThreshholds": "",
                                                                                    "OptionNotes": "",
                                                                                    "ArbitrationOptIn": "",
                                                                                    "Other": "",
                                                                                    "ContractNotes": childDeal.LeagueRepresentative.note,
                                                                                    "AdditionalClauses": {
                                                                                        "Spring": false,
                                                                                        "Date1": false,
                                                                                        "Date2": false
                                                                                    }
                                                                                }
                                                                        };

                                                                        if (childDeal.Type == "Arbitration" && arbType == "FilingNumber") {
                                                                            // first, find the salary.. who won? default, player's counter offer
                                                                            var arbSalary = deal.playerFilingNumber;
                                                                            if (deal.arbHearingResult == "ClubWins") {
                                                                                arbSalary = deal.filingNumber;
                                                                            }

                                                                            // duplicate some of the effort below to stuff the salary into the right place.
                                                                            // create each year of salary, contract option, buyout
                                                                            for (var i = 0; i < 10; i++) {
                                                                                var key = (thisYear + i).toString();
                                                                                rSalary[key] = {
                                                                                    "Buyout": "",
                                                                                    "Contract": "",
                                                                                    "Salary": ""
                                                                                };
                                                                                if (i == 0) {
                                                                                    rSalary[key].Salary = "$" + arbSalary;
                                                                                }

                                                                                if (i == 1) {
                                                                                    rSalary[key].Salary = "FA";
                                                                                }
                                                                            }
                                                                            rSalary.AAV = arbSalary;
                                                                            faPlayer['rSalary'] = rSalary;

                                                                        } else {
                                                                            // FA or agreed terms salary arbitration
                                                                            if (deal.signingBonus) {
                                                                                rSalary.SigningBonus = deal.signingBonus;
                                                                            }
                                                                            var years = parseInt(deal.years);
                                                                            var contractOptionsSize = deal.contractOptions.length;
                                                                            if (contractOptionsSize > 0) {
                                                                                // check 'em that they're valid
                                                                                if (contractOptionsSize == 2 && (deal.contractOptions[1].options == "" || deal.contractOptions[1].amount == "0.000")) {
                                                                                    contractOptionsSize = 1;
                                                                                }
                                                                                if (contractOptionsSize == 1 && (deal.contractOptions[0].options == "" || deal.contractOptions[0].amount == "0.000")) {
                                                                                    contractOptionsSize = 0;
                                                                                }
                                                                            }
                                                                            var faYear = years + contractOptionsSize;
                                                                            var optOutYear = -1;
                                                                            /*
                                                                            <label ng-if="sign.years==6" class="inline-label">Opt Out: 3 years </label>
                                                                            <label ng-if="sign.years==7" class="inline-label">Opt Out: 4 years </label>
                                                                            <label ng-if="sign.years>7" class="inline-label">Opt Out: 5 years </label>
                                                                           */
                                                                            if (years == 6) optOutYear = 2; // remember,we're counting from 0
                                                                            if (years == 7) optOutYear = 3;
                                                                            if (years > 7) optOutYear = 4;

                                                                            // create each year of salary, contract option, buyout
                                                                            for (var i = 0; i < 10; i++) {
                                                                                var key = (thisYear + i).toString();
                                                                                rSalary[key] = {
                                                                                    "Buyout": "",
                                                                                    "Contract": "",
                                                                                    "Salary": ""
                                                                                };
                                                                                if (i < years) {
                                                                                    rSalary[key].Salary = "$" + deal.salary[i];
                                                                                }

                                                                                if (i == optOutYear) {
                                                                                    rSalary[key].Contract = "Opt Out";
                                                                                }
                                                                                if (i == faYear) {
                                                                                    rSalary[key].Salary = "FA";
                                                                                }
                                                                                if (i == years && contractOptionsSize > 0) {
                                                                                    // put  in first option here...
                                                                                    rSalary[key].Salary = "$" + deal.contractOptions[0].amount;
                                                                                    rSalary[key].Contract = deal.contractOptions[0].option;
                                                                                    rSalary[key].Buyout = "$" + deal.buyout;
                                                                                }
                                                                                if (((i - 1) == years) && contractOptionsSize == 2) {
                                                                                    // put  in 2nd option here...    
                                                                                    rSalary[key].Salary = "$" + deal.contractOptions[1].amount;
                                                                                    rSalary[key].Contract = deal.contractOptions[1].option;
                                                                                    rSalary[key].Buyout = "$" + deal.buyout;
                                                                                }
                                                                            }

                                                                            // add in the performance, award, and escalators
                                                                            for (i = 0; i < deal.incentives.length; i++) {
                                                                                var incentive = deal.incentives[i].incentive;
                                                                                if (incentive == "Performance") {
                                                                                    rSalary.ContractExtras.PBs.push(deal.incentives[i]);

                                                                                } else if (incentive == "Trade") {
                                                                                    rSalary.ContractExtras.TradeBonus = "$" + deal.incentives[i].amount;

                                                                                } else if (incentive == "Award") {
                                                                                    rSalary.ContractExtras.Awards.push(deal.incentives[i]);

                                                                                } else if (incentive == "Escalator") {
                                                                                    rSalary.ContractExtras.Escalators.push(deal.incentives[i]);
                                                                                }
                                                                            }

                                                                            // add in the minor leaguer's clauses
                                                                            rSalary.ContractExtras.AdditionalClauses.spring = deal.clauses.spring;
                                                                            if (deal.clauses.optOut1)
                                                                                rSalary.ContractExtras.AdditionalClauses.Date1 = deal.clauses.optOutDate1;
                                                                            if (deal.clauses.optOut2)
                                                                                rSalary.ContractExtras.AdditionalClauses.Date2 = deal.clauses.optOutDate2;

                                                                            faPlayer['rSalary'] = rSalary;
                                                                        }
                                                                    }
                                                                    // insure they have an age.
                                                                    if (!faPlayer.Age || faPlayer.Age == "") {
                                                                        var moment = require('moment-timezone');
                                                                        var dob = faPlayer.DOB
                                                                        if (dob != "") {
                                                                            var isoDate = new Date(dob).toISOString();
                                                                            var d = moment(isoDate);
                                                                            var age = moment().diff(d, 'years', true);
                                                                            faPlayer['Age'] = Math.floor(age);
                                                                        }
                                                                    }
                                                                    if (childDeal.Type == "Arbitration") {
                                                                        //need to update the arbitration list!
                                                                        // see below.
                                                                        if (arbType == "NonTender") {
                                                                            // put into the free agents
                                                                            league.FreeAgents.unshift(faPlayer);
                                                                            league.markModified("FreeAgents");
                                                                        }

                                                                    } else {
                                                                        // now see if he goes majors or minors...
                                                                        if (deal.level == "Major") {
                                                                            faPlayer.Level = "ML";
                                                                            faPlayer.rSalary.Guaranteed = true;
                                                                            faPlayer['Status'] = 1;
                                                                            faPlayer.onFortyMan = true;
                                                                            rosters[0].FortyManAL.unshift(faPlayer);
                                                                            rosters[0].FortyManNL.unshift(faPlayer);
                                                                            rosters[0].markModified("FortyManAL");
                                                                            rosters[0].markModified("FortyManNL");
                                                                            updateDepthChart = true;
                                                                        } else {
                                                                            // put on minors
                                                                            faPlayer.Level = "Triple-A";
                                                                            if (childDeal.Type == "IntlFA") {
                                                                                faPlayer.Level = "DSL";
                                                                            }
                                                                            faPlayer['Status'] = 2;
                                                                            rosters[0].NonRoster.unshift(faPlayer);
                                                                            rosters[0].markModified("NonRoster");
                                                                            updateDepthChart = true;
                                                                        }
                                                                    }


                                                                    if (!TestApproved) {
                                                                        // update the free agent/arbitration data
                                                                        // update the depth charts and batting orders now
                                                                        // player has been moved to/from the 40man/non roster lists
                                                                        var newPlayer = null;
                                                                        if( childDeal.Type != "Arbitration" && updateDepthChart === true ) {
                                                                            newPlayer = faPlayer;
                                                                        }
                                                                        rosterLineupHelper.addPlayerToRosterLineup(newPlayer, rosters[0], false, function(err, response ){
                                                                         rosters[0].save(function (err, rosters) {
                                                                            if (err) {
                                                                                res.status(401).json({ status: 401, msg: "couldn't update roster with new player" });
                                                                            } else {
                                                                                // 
                                                                                // save the league with the free agent removed
                                                                                //
                                                                                league.save(function (err, league) {
                                                                                    // 
                                                                                    // update the financials for this team.
                                                                                    //
                                                                                    var save = true;
                                                                                    leagueHelper.updateFinancials(req.body.lid, childDeal.Teams[0].id, "", save, function (err, message, finArray) {
                                                                                        if (err) {
                                                                                            res.status(401).json({ status: 401, msg: message });
                                                                                        } else {
                                                                                            res.status(200).json({ status: 200, msg: message, newstatus: "DEAL EXECUTED.", finArray: finArray });
                                                                                        }
                                                                                        // if arbitration deal is done, then update the arbitration data for this player...
                                                                                        if (childDeal.Type == "Arbitration") {
                                                                                            var nontendered = false;
                                                                                            var multiyear = false;
                                                                                            var agreed = false;
                                                                                            if (arbType == "NonTender") {
                                                                                                nontendered = true;
                                                                                            } else if (arbType == "FilingNumber") {
                                                                                                agreed = deal.arbHearingResult;
                                                                                            } else {
                                                                                                agreed = true;
                                                                                                if (years > 1)
                                                                                                    multiyear = true;

                                                                                            }

                                                                                            Arbitration.update(
                                                                                                { FullName: childDeal.Players[0].FullName },
                                                                                                {
                                                                                                    AgreedTerms: agreed,
                                                                                                    MultiYear: multiyear,
                                                                                                    NonTender: nontendered,
                                                                                                    LastUpdateUTC: new Date().toISOString()
                                                                                                }, function (err, arb) {
                                                                                                    if (err) {

                                                                                                    } else {
                                                                                                        var a = arb;
                                                                                                    }
                                                                                                });
                                                                                        }
                                                                                    })
                                                                                });
                                                                            }
                                                                        })

                                                                      })
                                                                    } else {

                                                                    }


                                                                } else {

                                                                    // ****************************************
                                                                    //
                                                                    // execute a Trade Deal here
                                                                    //
                                                                    // manage the trade mechanics here.
                                                                    // get the from and to players the rosters
                                                                    //
                                                                    // ***************************************
                                                                    var team0RosterIndex = 0;
                                                                    var team1RosterIndex = 1;
                                                                    if (rosters[0].TeamId == childDeal.Teams[1].id) {
                                                                        // switch 'em
                                                                        team0RosterIndex = 1;
                                                                        team1RosterIndex = 0;

                                                                    }


                                                                    // now grab the rosterLineups
                                                                    RosterLineup.find(query, function (err, rosterLineups) {
                                                                        var team0RLIndex = 0;
                                                                        var team1RLIndex = 1;
                                                                        if (rosterLineups[0].TeamId == childDeal.Teams[1].id) {
                                                                            // switch 'em
                                                                            team0RLIndex = 1;
                                                                            team1RLIndex = 0;
    
                                                                        } 
                                                                    teamHelper.executeDealPoints(league, childDeal.Teams[0].dealPoints, childDeal.Teams[0], childDeal.Teams[1], rosters[team0RosterIndex], rosters[team1RosterIndex], rosterLineups[team0RLIndex], rosterLineups[team1RLIndex]);
                                                                    teamHelper.executeDealPoints(league, childDeal.Teams[1].dealPoints, childDeal.Teams[1], childDeal.Teams[0], rosters[team1RosterIndex], rosters[team0RosterIndex], rosterLineups[team1RLIndex], rosterLineups[team0RLIndex]);

                                                                    rosterLineupHelper.makeLegalDepthChartAndMarkModified(rosters[team0RosterIndex], rosterLineups[team0RLIndex]);
                                                                    rosterLineupHelper.makeLegalDepthChartAndMarkModified(rosters[team1RosterIndex], rosterLineups[team1RLIndex]);
                                                                    
                                                                    league.markModified("Teams");
                                                                    rosters[0].markModified("FortyManAL");
                                                                    rosters[0].markModified("FortyManNL");
                                                                    rosters[0].markModified("NonRoster");
                                                                    rosters[1].markModified("FortyManAL");
                                                                    rosters[1].markModified("FortyManNL");
                                                                    rosters[1].markModified("NonRoster");

                                                                    league.save(function (err, league) {
                                                                        if (err) {

                                                                        } else {
                                                                            rosters[0].save(function (err, roster0) {
                                                                                if (err) {

                                                                                } else {
                                                                                    rosters[1].save(function (err, roster1) {
                                                                                        if (err) {

                                                                                        } else {
                                                                                            // 
                                                                                            // update the financials for this team.
                                                                                            //
                                                                                            var save = true;
                                                                                            leagueHelper.updateFinancials(req.body.lid, "list", [childDeal.Teams[0].id, childDeal.Teams[1].id], save, function (err, message, finArray) {
                                                                                                if (err) {
                                                                                                    res.status(401).json({ status: 401, msg: message });
                                                                                                } else {
                                                                                                    res.status(200).json({ status: 200, msg: message, newstatus: "TRADE EXECUTED.", finArray: finArray });
                                                                                                }

                                                                                            });

                                                                                       
                                                                                        }
                                                                                    })
                                                                                }
                                                                            }) 
                                                                            // save the rosterLineups here.. ok that they're async
                                                                            rosterLineups[0].save( function(err, results){
                                                                                if( err ) {}
                                                                            });
                                                                            rosterLineups[1].save( function(err, results) {
                                                                                if( err ) {}
                                                                            });
                                                                        }
                                                                    })
                                                                });
                                                                } // end of executing a trade
                                                            });
                                                    }
                                                })
                                            }
                                        })
                                    })


                                }
                            });
                        }
                    });
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
// deals/getDealObjects
// --> returns the objects for a request... 
//
// ******************************************************
router.post('/api/deals/getDealObjects', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var team = req.body.team;

            var newDeal = dealHelper.createDeal(req.body.type, req.body.lid);
            newDeal.TeamsInvolved.push(team._id);

            var proposalObject = dealHelper.getBlankProposal();
            proposalObject.fromTeam.name = team.r_name;
            proposalObject.fromTeam.id = team._id;
            proposalObject.fromTeam.abbreviation = team.abbreviation;
            proposalObject.fromTeam.color = team.colorPrimary;
            proposalObject.fromTeam.textcolor = team.colorText;
            proposalObject.fromTeam.representative.name = user.FirstName + " " + user.LastName;;
            proposalObject.fromTeam.representative.id = user._id.toString();
            proposalObject.fromTeam.representative.role = req.body.userrole;

            var transactionObj = dealHelper.getTransactionObject();
            if (req.body.player) {
                transactionObj.name = req.body.player.FullName;
                transactionObj.playerId = req.body.player.PlayerId;
            }

            res.status(200).json({ status: 200, deal: newDeal, proposal: proposalObject, transaction: transactionObj });

        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ****************************************************
//
// TODO: LOOKS DEPRICATED!!!!!
//
// deals/submitDeal
// --> assume all deal points are here for one dealpoint and one detail from each team
// --> create the deal object, send messages to any/all others involved in deal
// --> assume TO team is the submitter.
// --> from team can be "Free Agents" (teamId = -1)
// ******************************************************
router.post('/api/deals/submitDeal', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var toTeam = req.body.toTeam;
            var fromTeam = req.body.fromTeam;

            var newDeal = dealHelper.createDeal(req.body.type, req.body.lid, toTeam._id, user, req.body.userrole);
            newDeal.TeamsInvolved.push(toTeam._id);

            if (fromTeam.TeamName != "Free Agents") {
                newDeal.TeamsInvolved.push(fromTeam._id);
            } else {
                fromTeam["_id"] = "-1";
            }

            var dealPoint = dealHelper.getDealPoint(fromTeam, toTeam);


            var detail = dealHelper.getDetailObject();
            detail.consideration = req.body.consideration; // error check this.
            detail.note = req.body.note;
            if (req.body.player) {
                detail.name = req.body.player.FullName;
                detail.playerId = req.body.player.PlayerId;
                detail.position = req.body.player.Position;

                if (newDeal.PlayersInvolved.indexOf(detail.playerId) == -1) {
                    newDeal.PlayersInvolved.push(detail.playerId);
                }
            } else {
                detail.type = "cash";
            }

            dealPoint.details.push(detail);
            newDeal.DealPoints.push(dealPoint);


            // now create the deal

            Deal.create(newDeal, function (err, deal) {

                // now send the message to the recipient of the deal offer
                if (err) {
                    res.status(500).json({ status: 500, deal: response });
                } else {
                    League.findOne({ _id: req.body.lid }, { FreeAgents: 1 }, function (err, league) {
                        if (err) {
                            res.status(500).json({ status: 500, deal: "" });
                        } else {

                            // in case this is a free agent deal, update the status of the free agent.
                            var freeagents = league.FreeAgents;
                            for (i = 0; i < freeagents.length; i++) {
                                if (freeagents[i].PlayerId == req.body.player.PlayerId) {
                                    freeagents[i].Status = playerStatus.FreeAgentOfferedContract;
                                    league.markModified("FreeAgents");
                                    break;
                                }
                            }

                            league.save(function (err, response) {
                                if (err) {
                                    res.status(500).json({ status: 500, deal: "" });
                                } else {
                                    res.status(200).json({ status: 200, deal: response });

                                    // send message to interested party(s)...

                                    var messageBody = {
                                        Type: msgType.Deal.index,
                                        FromUserId: user._id.toString(),
                                        Subject: "Deal tendered for " + req.body.player.FullName,
                                        Message: user.FirstName + " " + user.LastName + " (" + req.body.userrole + " of " + toTeam.r_name + ") tendered an offer to sign free agent " + req.body.player.FullName,
                                        ReferenceId: deal._doc._id.toString(),
                                        ReferenceType: deal._doc.Type,
                                        CreatedUTC: new Date().toISOString()
                                    }

                                    // this message is to the admins... 
                                    var message = {
                                        Type: msgType.ToAdmins.index,
                                        ToUserId: "",
                                        FromUserId: user._id.toString(),
                                        UserLeagueId: req.body.lid,
                                        UserTeamId: toTeam._id.toString(),
                                        CreatedUTC: new Date().toISOString(),

                                        MessageBodyId: "",
                                        ReadUTC: "",
                                        ThreadId: "",
                                        ThreadCount: 0,
                                        ParentId: ""
                                    }


                                    var toList = [];
                                    if (fromTeam.TeamName != "Free Agents") {
                                        // send to fromTeam's owners
                                    } else {
                                        // it's a free agent request, send to admins
                                        messageHelper.createAndSendPM(message, messageBody, user, toList, function (response) {
                                            return;
                                            // res.status(response.status).json(response);
                                        })
                                    }

                                }
                            });
                        }
                    });
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
// deals/claimFromWaivers
// --> puts a team's claim into the waivers as a deal request
//
// ******************************************************
router.post('/api/deals/claimFromWaivers', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            // 1) find out if this team has already tried to claim this player!
            Roster.findOne({ LeagueId: req.body.lid, TeamId: req.body.tid }, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, "msg": "roster not found" });
                } else {
                    // 2) find the player on either the NL roster or the Minor League Roster
                    var MLIndex = -1;
                    var MinorsIndex = -1;
                    var player = null;
                    for (i = 0; i < roster.FortyManNL.length; i++) {
                        if (roster.FortyManNL[i].PlayerId == req.body.pid) {
                            MLIndex = i;
                            player = roster.FortyManNL[i];
                            break;
                        }
                    }
                    if (MLIndex < 0) {
                        for (i = 0; i < roster.NonRoster.length; i++) {
                            if (roster.NonRoster[i].PlayerId == req.body.pid) {
                                MinorsIndex = i;
                                player = roster.NonRoster[i];
                                break;
                            }
                        }
                    }
                    if (player == null) {
                        res.status(500).json({ status: 500, "msg": "player not found" });
                    } else {
                        if (!player.ClaimingTeams) {
                            player.ClaimingTeams = [];
                        }
                        player.ClaimingTeams.push(
                            {
                                claimingTeamId: req.body.claimingTeamId,
                                claimingTeam: req.body.claimingTeamName,
                                representative: user.FirstName + " " + user.LastName,
                                representativeId: user._id.toString(),
                                dateUTC: new Date().toISOString()
                            });
                        if (MLIndex >= 0) {
                            roster.FortyManNL[MLIndex] = player;
                            roster.FortyManAL[MLIndex] = player;
                            roster.markModified("FortyManNL");
                            roster.markModified("FortyManAL");
                        } else {
                            roster.NonRoster[MinorsIndex] = player;
                            roster.markModified("NonRoster");
                        }
                        roster.save(function (err, response) {
                            if (err) {
                                res.status(500).json({ status: 500, "msg": "failed to claim player" });
                            } else {
                                res.status(200).json({ status: 200, "msg": "Player claimed." });
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


// ****************************************************
//
// /api/deals/assignPlayerFromWaivers
// --> executes claim from waivers to a new team.
//     previous team information in player data
// 
//     For players claimed on outright or release waivers...If the player is out of options 
//      he must go on the active roster. If he has options, the club can assign the player 
//      to the active roster or on optional assignment to AAA or AA. Any player claimed on waivers 
//      goes on the 40-man roster. 
//      if they are a 99 or 0 they are out of options.

//      If a player is claimed on Trade Waivers and the claim is not withdrawn, then 
//      he must go on 40-man and active rosters.
//
//
// ******************************************************
router.post('/api/deals/assignPlayerFromWaivers', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var fromTeamId = req.body.player.TeamId;

            // clear out his status
            req.body.player.Status = playerStatus.ActiveRoster;
            req.body.player.TradeStatus = playerStatus.UnavailableForTrade;
            req.body.player["ClaimingTeams"] = [];

            // see how many options he has...
            var outOfOptions = false;;
            if (req.body.player.Options && req.body.player.Options.Options) {
                if (req.body.player.Options.Options == 0 || req.body.player.Options.Options) {
                    outOfOptions = true;
                }
            }

            // all players signed from waivers go on FortyMan Roster
            // push onto the ML active roster
            req.body.player.onFortyMan = true;
            req.body.player.Level = "ML";


            teamHelper.movePlayerToTeamOrFA(req.body.lid, user, fromTeamId, req.body.claimingTeamId, req.body.player, false,
                "{{fromTeam}}' {{player}} claimed from waivers by {{toTeam}}.", function (response) {
                    res.status(response.status).json(response);
                });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ****************************************************
//
// deals/execute
// --> executes ALL moves in a deal record - a list of one or more player or money transfers
//
// ******************************************************
router.post('/api/deals/execute', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            deal.findByIdAndUpdate(req.body, function (err, result) {
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

module.exports = router;