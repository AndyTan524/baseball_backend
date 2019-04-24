
var moment = require('moment-timezone');
var mongoose = require('mongoose');
var League = mongoose.model('League');
var Team = mongoose.model('Team');
var Roster = mongoose.model('Roster');
var Player = mongoose.model('Player');
var Log = mongoose.model('Log');

var sendGridHelper = require('../helpers/sendGridHelper');
var msgType = require('../helpers/messageTypes');
var messageHelper = require('../helpers/messageHelper');

var playerStatus = require('../helpers/playerStatus');

// ***********************************
//
// objects that define a deal
//
// ***********************************

module.exports = {

    createDeal: function (type, lid, toTeamId, user, userrole, callback) {


        var newDeal = new Deal();
        /*
        {
            Type: type,         // Waiver, freeagent, arbitration, trade, multiteam, commissioner (manual move)
            Status: "requested",    // requested, counter, rejected, finalapproval, approved, completed
            LeagueId: lid,
            CreateDateUTC: new Date().toISOString(),        // original date of first submission
            LastExchangeDateUTC: new Date().toISOString(),        // date of last activity
            CompletedDataUTC: "",      // date of final activity
            TeamsInvolved: [],
            SubmittedBy: {},
            PlayersInvolved: [],
            DealPoints: []
        } */

        newDeal.SubmittedBy[toTeamId] = {
            name: user.FirstName + " " + user.LastName,
            userId: user._doc._id.toString(),
            userRole: userrole
        };

        if (callback) {
            callback(newDeal)
        } else {
            return (newDeal);
        }
    },

    getBlankDealPoint: function () {
        var dealPoint = {
            fromTeam: {
                //   name: "", id: "", abbreviation: "", color: "", textcolor: "", approved: false, note: "", representative: { name: "", role: "", id: "" }

            },       // team that currently owns player.  "freeagent" if player is free agent. Used for arbitraion
            toTeam: {
                //   name: "", id: "", abbreviation: "", color: "", textcolor: "", approved: false, note: "", representative: { name: "", role: "", id: "" }

            },         // team that player will go to.  Used for team trying to get a free agent
            counter: {
                // counter offer by agent, commissioner, or system
                //      name: "Agent", offer: "$2,000,000", approved: false, note: "", representative: { name: "", role: "", id: "" }

            },        // this is a counter offer from the "agent" or "commsissioner" or "system"
            details: []
        };

        return (dealPoint);
    },

    getDealPoint: function (fromTeam, toTeam, user) {
        var dealPoint = this.getBlankDealPoint();

        if (fromTeam.TeamName != "Free Agents") {
            dealPoint.fromTeam.name = fromTeam.r_name;
            dealPoint.fromTeam.id = fromTeam._id;
            dealPoint.fromTeam.abbreviation = fromTeam.abbreviation;
            dealPoint.fromTeam.color = fromTeam.colorPrimary;
            dealPoint.fromTeam.textcolor = fromTeam.colorText;
        }

        dealPoint.toTeam.name = toTeam.r_name;
        dealPoint.toTeam.id = toTeam._id;
        dealPoint.toTeam.abbreviation = toTeam.abbreviation;
        dealPoint.toTeam.color = toTeam.colorPrimary;
        dealPoint.toTeam.textcolor = toTeam.colorText;

        return (dealPoint);
    },


    createDealPoint: function (deal, fromTeam, representative, toTeam, callback) {

        var newDealPoint = getBlankDealPoint();

        deal.DealPoints.push(newDealPoint);

        if (callback) {
            callback(deal)
        } else {
            return (deal);
        }

    },

    getDetailObject: function () {
        var detailObject = {
            type: "player",
            name: "",
            position: "",
            playerId: "",
            consideration: "",
            eligible: "",
            note: ""
        };
        return (detailObject);
    },

    addDealPointDetail: function (dealpoint, type, player, consideration, note, callback) {

        var detail = {
            fromTeam: {},       // team that currently owns player.  "freeagent" if player is free agent. Used for arbitraion
            toTeam: {},         // team that player will go to.  Used for team trying to get a free agent
            counter: {},        // this is a counter offer from the "agent" or "commsissioner" or "system"
            deals: []
        };

        dealpoint.details.push(detail);

        if (callback) {
            callback(dealpoint)
        } else {
            return (dealpoint);
        }

    },

    notifyDealParties: function (deal, user, isNew) {

        // generic message... good for free agents and arbitration
        var subject = "Deal proposal";
        var message = deal.SubmittedBy + " proposed a deal";
    

        if( deal.Players.length > 0 ) {
            subject += " for "+ deal.Players[0].FullName;
            message += " for " + deal.Players[0].FullName;
        }
        var messageBody = {
            Type: msgType.Deal.index,
            FromUserId: deal.SubmittedById,
            Subject: subject,
            Message: message,
            ReferenceId: deal._id.toString(),
            ReferenceType: deal.Type,
            CreatedUTC: new Date().toISOString()
        }
        var message = {
            Type: msgType.ToList.index,
            ToUserId: "",
            FromUserId: deal.SubmittedById,
            UserLeagueId: deal.LeagueId,
            UserTeamId: deal.Teams[0].id,
            CreatedUTC: new Date().toISOString(),

            MessageBodyId: "",
            ReadUTC: "",
            ThreadId: "",
            ThreadCount: 0,
            ParentId: ""
        }


        var toList = [];
        var teamId2 = "";
        if( deal.Teams[1]) {
            teamId2 = deal.Teams[1].id;
        }
        messageHelper.getInterestedPartiesList(user, deal.LeagueId, deal.Teams[0].id, teamId2, false, function(err, list){
            if( err ) {

            } else {
                toList = list;
            }
            if (deal.Type != "SignFA" && deal.Type != "IntlFA" && deal.Type != "Arbitration") {
                // it's a trade...
                var verb = "proposed";
                if (!isNew) {
                    verb = "updated";
                }
                messageBody.Subject = "Trade " + verb + " between " + deal.Teams[0].name + " and " + deal.Teams[1].name;

                var submitter = deal.SubmitterTeam.length == 0 ? (deal.SubmittedBy) : (deal.SubmittedBy + " from the " + deal.SubmitterTeam);

                messageBody.Message = submitter + " " + verb + " a trade deal with the " + deal.Teams[1].name + ". Players include ";
                var players = [];
                var considerations = false;
                deal.Teams[0].dealPoints.forEach(function (dealpoint, index) {
                    if (dealpoint.type == "Player") {
                        players.push(dealpoint.player.FullName);
                    } else {
                        considerations = true;
                    }
                });
                messageBody.Message += players.join(", ");
                if (considerations) {
                    messageBody.Message += " and considerations";
                }
    
                var players = [];
                var considerations = false;
                deal.Teams[1].dealPoints.forEach(function (dealpoint, index) {
                    if (dealpoint.type == "Player") {
                        players.push(dealpoint.player.FullName);
                    } else {
                        considerations = true;
                    }
                });
                messageBody.Message += " for " + players.join(", ");
                if (considerations) {
                    messageBody.Message += " and considerations";
                }
                messageBody.Messsage += ".";
    
                // need to see if it's new or from the team owner (i.e. not from the admin)
                    messageHelper.createAndSendPM(message, messageBody, user, toList, function (response) {
                        return;
                    });
            } else {
                // it's a free agent request (standards or intl) or arbitration, send to admins
                // need to see if it's new or from the team owner (i.e. not from the admin)
                if (!isNew) {
                    messageBody.Message = messageBody.Message.replace("proposed", "updated");
                    messageBody.Message = messageBody.Message.replace("proposal", "proposal updated");
                }
    
    
               messageHelper.createAndSendPM(message, messageBody, user, toList, function (response) {
                        return;
               });
            }
        })


    }

}