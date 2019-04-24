
var mongoose = require('mongoose');

var moment = require('moment-timezone');
var User = mongoose.model('User');
var Message = mongoose.model('Message');
var MessageBody = mongoose.model('MessageBody');
var msgType = require('../helpers/messageTypes');
var League = mongoose.model("League");

var msgTypes = require('../helpers/messageTypes');

var utils = require('../helpers/utils'); 
var leagueHelper   = require('../helpers/leagueHelper');
var sendGridHelper = require('../helpers/sendGridHelper');

module.exports = {
    getAdmins: function (adminType,  callback) {

        var query = {Roles:"Admin"};
        if( adminType == "SuperAdmin") {
            query  = {Roles:"SuperAdmin"};
        }
        if( adminType == "QATest") {
            query  = {Roles:"QATest"};
        }
             User.find( query, function (err, admins) {

                var toList = [];
                if (err) {
                }
                else {
                    for( i=0; i<admins.length; i++) {
                        toList.push( admins[i]);
                    }
                }
                callback(null, admins, toList);
 
            });
 
    }, 

    /*
    getFromUser: function (messages, index, callback) {
        var context = this;
        if (messages.length == index) {
            callback(null, messages);
        }
        else {
            User.findOne({ _id: messages[index].FromUserId }, { FirstName: 1, LastName: 1, Email: 1 }, function (err, user) {
                if (err) {
                    messages[index]._doc.FromUser = {};
                }
                else {
                    messages[index]._doc.FromUser = user; 
                }
                context.getFromUser(messages, ++index, callback);
            });
        }
    }, 
    */

    getTeamAffiliationList: function( league ) {
        var affiliations = {};
        if( !league ) {
            return {};
        }
        for( i=0; i<league.Teams.length; i++) {
            var owners = league.Teams[i].Owners;
            for( oi = 0; oi<owners.length; oi++) {
                var owner = owners[oi];
                affiliations[owner.UserId] = {TeamName: league.Teams[i].r_name, TeamId: league.Teams[i].TeamId};
            }
        }
        return( affiliations);
    },

    getFromUser: function (league, messages, index, callback) {
        var moment = require('moment-timezone');
        moment.tz.setDefault("America/Los_Angeles");

        var affiliations = this.getTeamAffiliationList(league);

        var context = this;
        if (messages.length == index) {
            callback(null, messages);
        }
        else {
            User.findOne({ _id: messages[index].FromUserId }, { FirstName: 1, LastName: 1, Email: 1 }, function (err, user) {
                if (err) {
                    messages[index]._doc.FromUser = {};
                }
                else {
                   if( affiliations[user._id] )
                        user._doc["TeamName"] = affiliations[user._id].TeamName;
                    messages[index]._doc.FromUser = user;


                    // assign the team name here.

                }
                messages[index]._doc.SentTime = moment(messages[index]._doc.CreatedUTC).format("llll");
                context.getFromUser(league, messages, ++index, callback);
            });
        }
    }, 

    getToUser: function (messages, index, callback) {
        var context = this;
        var moment = require('moment-timezone');
        moment.tz.setDefault("America/Los_Angeles");

        if (messages.length == index) {
            callback(null, messages);
        }
        else {
            if (!messages[index]._doc.ToName) {
                User.findOne({ _id: messages[index].ToUserId }, { FirstName: 1, LastName: 1, Email: 1 }, function (err, user) {
                    if (err || user==null) {
                        messages[index]._doc.ToUser = {};
                        messages[index]._doc.ToName = "";
                    }
                    else {
                        if( user ) {
                        messages[index]._doc.ToUser = user;
                        if( user.FirstName && user.LastName) {
                            messages[index]._doc.ToName = user.FirstName + " " + user.LastName;
                        } else {
                            messages[index]._doc.ToName = "";
                        }
                        }
                    }

                    messages[index]._doc.SentTime = moment(messages[index]._doc.CreatedUTC).format("llll");
                    context.getToUser(messages, ++index, callback);
                });
            } else {
                messages[index]._doc.ToUser = {};
                var moment = require('moment-timezone');
                messages[index]._doc.SentTime = moment(messages[index]._doc.CreatedUTC).format("llll");
                context.getToUser(messages, ++index, callback);

            }
        }
    }, 

    getMessageBody: function (messages, index, callback) {
        var context = this;
        
        if (messages.length == index) {
            callback(null, messages);
        }
        else {
            MessageBody.findOne({ _id: messages[index].MessageBodyId }, function (err, mbody) {
                if (err) {
                    messages[index]._doc["Body"] = {};
                }
                else {
                    messages[index]._doc["Body"] = mbody;
                }
                context.getMessageBody(messages, ++index, callback);
            });
        }
    }, 

    sendPM: function( sender,users, subject, message, index, callback) {
        var context = this;
        var senderName = sender.FirstName + " " + sender.LastName;
        
        if( users.length == index) {
                if( users.length==0) {
                    //  no users, just return
                    callback( null, users);
                    return;

                }
                // finished sending private messages..
                // now send email to user(s)
                    var emailTemplate = "pmSentToYou";
                    var customterms = {};

                    customterms["sender"] = senderName;
                    customterms["messageSubject"] = subject;

                    customterms["Subject"] = "New message from " + senderName + " at RSports Baseball";
                    if( message.EmailTemplate != '' && message.Subject) {
                        // then use the message subject for the subject of the email
                        customterms["Subject"] = message.Subject;
                    }


                    customterms["toAddress"] = [];
                    customterms["toName"] = [];
                    for(i=0; i<users.length; i++) {
                        if( users[i].Email ) {
                            customterms.toAddress.push(users[i].Email);
                            customterms.toName.push(users[i].FirstName + " " + users[i].LastName);
                        }
                    }
                    
                    sendGridHelper.sendEmail( emailTemplate, null, customterms, function (err, response) {
                            if (err) {
                                // res.json({ status: 500, msg: "Something went wrong with email", err: err });
                               // context.sendPM(sender, users, subject, message, ++index, callback);
                            }
                            else {
                                //  res.status(200).json({ status: 200, msg: "Email sent.", user: result });
                            }
                            callback( null, users);
                        });

        } else {
            // send next private email...
            var userEmail;
            var userName;

            if (users[index] && users[index]._doc) {
                message.ToUserId = users[index]._doc._id.toString();
                userEmail = users[index]._doc.Email;
                userName = users[index]._doc.FirstName + " " + users[index]._doc.LastName;
            } else {
                message.ToUserId = users[index];
                if( users[index].Email) {
                    userEmail = users[index].Email;
                    userName = users[index].FirstName + " " + users[index].LastName;
                }
            }
            Message.create( message, function(err, dbMessage){
                if( err ) {

                } else {
                    context.sendPM(sender, users, subject, message, ++index, callback);
                }

            });
        }
    },

    
    // toList = [{}]
    createAndSendPM: function(message, messageBody, sender, toList, callback) {
        var context = this;
        var toType = message.Type;
        if( !sender || (sender.Roles.indexOf("Admin")>=0 )) {
            sender = {
                FirstName: "The League",
                LastName: "Office"
            }
        }

        // create the message body (only once so we can send lots, but only keep one copy around)
        MessageBody.create(messageBody, function (err, dbMessageBody) {
            if (err) {
                callback({ 'status': 500, response: err });
            }
            else {
                // attach the message body to the message (heaer)
                message.MessageBodyId = dbMessageBody._doc._id.toString();

                // default: get the users in the toList
                var query = { _id: {$in:toList}};
                if( toType == msgTypes.ToAdmins.index || toType == msgTypes.ToSupport.index || toType==msgTypes.ToCommissioner.index) {
                    query = {Roles:"Admin"};
                } else if( toType == msgTypes.ToAll.index || toType == msgTypes.ToAllFans.index) {
                    query = {};
                } else  if( toType == msgTypes.ToDevTest.index) {
                    query = {LastName:"Dombrower"};
                } else if( toType != msgTypes.ToOne.index ) {
                   //  callback({ 'status': 500, message: "This message destination not implemented yet" });
                   // return;
                } 

                if( query ) {
                    User.find(  query  ,function(err, dbUsers){
                        context.sendPM( sender, dbUsers,  dbMessageBody._doc.Subject,message, 0, function(){
                            callback({ 'status': 200, message: "Message(s) Sent", subject: dbMessageBody._doc.Subject, copy: dbMessageBody._doc.Message });
                        });
                    })
                } else {
                    context.sendPM( sender, users, dbMessageBody._doc.Subject, message, 0, function(){
                        callback({ 'status': 200, message: "Message(s) Sent" , subject: dbMessageBody._doc.Subject, copy: dbMessageBody._doc.Message });
                });
            }
            }
        });
    },

    createAndSendMessageToLeague: function( leagueId, subject, synopsis, message, callback ) {
        var messageBody = {
            Type: msgType.ToAll.index,
            FromUserId: "",
            Subject: subject,
            Message: message,
            ReferenceId: "",
            ReferenceType: "",
            CreatedUTC: new Date().toISOString()
        }
    
        // this message is to the admins... 
        var message = {
            Type: msgType.ToAll.index,
            ToUserId: "",
            FromUserId: "",
            UserLeagueId: leagueId,
            UserTeamId: "",
            CreatedUTC: new Date().toISOString(),
    
            MessageBodyId: "",
            ReadUTC: "",
            ThreadId: "",
            ThreadCount: 0,
            ParentId: ""
        }
    
    
        var toList = [];
        var user = null;
            // it's a free agent request, send to admins
            this.createAndSendPM(message, messageBody, user, toList, function (response) {
                callback();
            })

    },


    getInterestedPartiesList: function( user, lid, teamId1, teamId2, skipAdmins, callback ) {
  
        // find the parties that might be interested
        // admins are ALWAYS interested
        this.getAdmins("Admin", function(admins, toList ){

            leagueHelper.getOwners(lid, teamId1, function(err, currentOwners, currentTeamName){
                if( err ) {
                    callback( err );
                } else {

                    // combine the lists
                    if( skipAdmins ) {
                        toList = currentOwners;
                    } else {
                        // add admins to current owner list
                        toList = toList.concat(currentOwners);
                    }

                    // find the other team (if it exists)
                    leagueHelper.getOwners(lid, teamId2, function (err, newOwners, newTeamName) {
                        if (err) {
                            callback(err);
                        } else {
                            //now have all 3 lists!
                            // combine the lists
                            toList = toList.concat(newOwners);

                            var toIdList = [];
                            for (i = 0; i < toList.length; i++) {
                                if ('UserId' in toList[i]) {
                                    var id = toList[i].UserId;
                                    if (id.match(/^[0-9a-fA-F]{24}$/)) {
                                        // Yes, it's a valid ObjectId, proceed with `findById` call.
                                        toIdList.push(id);
                                    }

                                } else if ("_doc" in toList[i]) {
                                    toIdList.push(toList[i]._id);
                                }
                            }
                            callback( null, toIdList );
                        }
                    });
                }
            });
        });
    },


    // find the interested parties in this league about a player situation-change
    // note oldStatus and newStatus are English-language versions of the status change
    // note if oldStatus == "msg", then newStatus is the message to send.
    // note if currentTid == "All", then send the message to everyone.
    //      if oldStatus == "emailTemplate", then newStatus is the name of the template
    //              in this case, if player.messageCopy is defined, it is the message copy
    alertInterestedParties: function(user, lid, headline, currentTid, oldTeamName, newTid, player, oldStatus, newStatus, skipAdmins, callback ) {

        context = this;

        // first find the parties that might be interested
        // admins are ALWAYS interested
        this.getAdmins("Admin", function(admins, toList ){

            leagueHelper.getOwners(lid, currentTid, function(err, currentOwners, currentTeamName){
                if( err ) {
                    callback(err);
                } else {

                    if (currentTeamName == "" || currentTeamName == "All") {
                        currentTeamName = oldTeamName;
                    }
                    // combine the lists
                    if (skipAdmins) {
                        toList = currentOwners;
                    } else {
                        // add admins to current owner list
                        toList = toList.concat(currentOwners);
                    }

                    // find the other team (if it exists)
                    leagueHelper.getOwners(lid, newTid, function (err, newOwners, newTeamName) {
                        if (err) {
                            callback(err);
                        } else {
                            //now have all 3 lists!
                            // combine the lists
                            toList = toList.concat(newOwners);

                            // send PM, which also sends the email
                            var msg;
                            var subject = "";
                            var emailTemplate = "";

                            if (headline && headline != "") {
                                msg = headline;
                                if (oldStatus == "emailTemplate") {
                                    emailTemplate = newStatus;
                                    subject = headline;

                                    // double booked the player to  have a message body message
                                    if (player.messageCopy)
                                        msg = player.messageCopy;
                                }
                            } else {
                                // headline doesn't exist or is empty
                                if (oldStatus == "msg") {
                                    msg = newStatus;
                                } else {
                                    msg = player.FullName + " on the " + currentTeamName + " has changed from " + oldStatus + " to " + newStatus;
                                    if (currentTeamName != newTeamName) {
                                        msg = player.FullName + " has been traded/moved from the " + currentTeamName + " to the " + newTeamName;
                                    }
                                }
                            }


                            if (subject == "") {
                                // subject hasn't been loaded in yet...

                                if (player) {
                                    subject = "PLAYER NEWS about the " + currentTeamName + " " + player.FullName;
                                } else {
                                    if (currentTeamName == "Free Agents" || newTeamName == "Free Agents") {
                                        subject = "Free Agent Signing";
                                    } else {
                                        subject = "There's Been a Trade!"
                                    }
                                }
                            }
                            if (headline && headline != "") {
                                if (headline.includes("non-tendered")) {
                                    subject = "Player Non-Tendered";
                                }
                                if (headline.includes("new contract")) {
                                    subject = "Player Arbitration Concluded";
                                }
                            }

                            var userId = "System";
                            if( user && user._id) {
                                userId = user._id.toString()
                            }

                            var messageBody = {
                                Type: msgType.StatusChange.index,
                                FromUserId: userId,
                                Subject: subject,
                                Message: msg,
                                ReferenceId: "",
                                ReferenceType: "",
                                CreatedUTC: new Date().toISOString(),
                                EmailTemplate: emailTemplate
                            }
                            var message = {
                                Type: msgType.ToList.index,
                                ToUserId: "",
                                FromUserId: userId,
                                UserLeagueId: lid,
                                UserTeamId: currentTid,
                                CreatedUTC: new Date().toISOString(),

                                MessageBodyId: "",
                                ReadUTC: "",
                                ThreadId: "",
                                ThreadCount: 0,
                                ParentId: "",
                                EmailTemplate: emailTemplate,    // not stored in db
                                Subject: subject                // not stored indb
                            }
                            var toIdList = [];
                            for (i = 0; i < toList.length; i++) {
                                if ('UserId' in toList[i]) {
                                    var id = toList[i].UserId;
                                    if (id.match(/^[0-9a-fA-F]{24}$/)) {
                                        // Yes, it's a valid ObjectId, proceed with `findById` call.
                                        toIdList.push(id);
                                    }

                                } else if ("_doc" in toList[i]) {
                                    toIdList.push(toList[i]._id);
                                }
                            }
                            if (toIdList.length > 0) {
                                context.createAndSendPM(message, messageBody, user, toIdList, function (response) {
                                    callback(response);
                                })
                            } else {
                                callback("no do list");
                            }
                        }
                    })
                }
            })
        });

    },

    alertSetOfInterestedParties: function (index, user, messageArray, type, skipAdmins, callback) {
        var context = this;
        if (index >= messageArray.length) {
            if (skipAdmins) {
                // send a summary...
                skipAdmins = false;
                var messageHeadline = "Team Alerts Sent";
                var teamsList = [];
                for( i=0; i<messageArray.length; i++) {
                    teamsList.push( messageArray[i].teamName );
                }
                var mcopy = "The following teams have roster overages and have been notified: " + teamsList.join(", ") + ".";
                var copy = {messageCopy: mcopy};
                context.alertInterestedParties(user, leagueId,
                    messageHeadline, "", "", "", copy,
                    "emailTemplate", "rostertemplate", skipAdmins, function (response) {
                        // iterate to the next team.
                        callback("succes");
                        return;
                    })
            }

        } else {
            // send next message to it's interested parties on next team;
            var leagueId = messageArray[index].leagueId;
            var teamName = messageArray[index].teamName;
            var teamId = messageArray[index].teamId;
            var teamMessages = messageArray[index].messages;
            var messageHeadline = "The " + teamName + " Rosters Need Attention";
            var mcopy =  "The " + teamName + " have the following roster(s) over the allowed size: ";
            var sizes = [];
            for( i=0; i<teamMessages.length; i++) {
                sizes.push( teamMessages[i].shortMsg);
            }

            mcopy += sizes.join(", ") + ". Please adjust the " + teamName + " rosters promptly."
            var copy = {messageCopy: mcopy};

            context.alertInterestedParties(user, leagueId,
                messageHeadline, teamId, teamName, "", copy,
                "emailTemplate", "rostertemplate", skipAdmins, function (response) {
                    // iterate to the next team.
                    context.alertSetOfInterestedParties(++index, user, messageArray, type, skipAdmins, callback);

                })
        }
    }
};