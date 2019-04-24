var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var sendGridHelper = require('../helpers/sendGridHelper');
var messageHelper = require('../helpers/messageHelper');
var teamHelper = require('../helpers/teamHelper');

var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var Api = mongoose.model('Api');
var User = mongoose.model('User');
var Content = mongoose.model("Content");
var Message = mongoose.model('Message');
var MessageBody = mongoose.model('MessageBody');
var League = mongoose.model('League');
var msgTypes = require('../helpers/messageTypes');

var firebaseHelper = require('../helpers/firebaseHelper')();

// ************************************************
//
// messaging/send
// ==> sends fb or twitter message
//
// ************************************************
router.post('/api/messaging/send', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var sessionId = utils.randomString(16, '0123456789abcdefghijklmnopqrstuvwxyz');

            var data = req.body;
            var tokens = data.devices;
            var payload = data.payload;
            payload.data.title = "New PM from " + user.FirstName + " " + user.LastName;
            var options = data.options;

            if (tokens.length > 0) {
                firebaseHelper.sendToDevices(tokens, payload, options, function (err, response) {
                    if (err) {
                        res.json({ 'status': 500, response: err });
                    }
                    else {
                        User.findOne({ fbtoken: tokens[0] }, function (err, dbUser) {
                            var message = {
                                ToUserId: dbUser._id,
                                FromUserId: user._id,
                                LeagueId: "",
                                Subject: payload.data.title,
                                Body: payload.data.body,
                                CreatedUTC: new Date().toISOString(),
                                SentUTC: new Date().toISOString()
                            }
                            Message.create(message, function (err, result) {
                                if (err) {
                                    res.json({ 'status': 500, response: err });
                                }
                                else {
                                    res.json({ 'status': 200, response: response });
                                }
                            });
                        });
                    }
                });
            }
            else {
                firebaseHelper.sendToDevices(tokens, payload, options, function (err, response) {
                    if (err) {
                        res.json({ 'status': 500, response: err });
                    }
                    else {
                        res.json({ 'status': 200, response: response });
                    }
                });
            }
        }
        else {
            res.status(401).json({ 'status': 401, error: "Unauthorized" });
        }
    });
});


router.post('/api/messaging/token', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            user.fbtoken = req.body.token;
            user.save(function (err, result) {
                if (err) {
                    res.status(500).json({ 'status': 500, error: err });
                }
                else {
                    res.json({ 'status': 200, result: result });
                }
            });
        }
        else {
            res.status(401).json({ 'status': 401, error: "Unauthorized" });
        }
    });
});

// *******************************************************
//
// messaging/sendPM
// ==> send PM in RSports system
//
// *******************************************************
router.post('/api/messaging/sendPM', function (req, res) {
    utils.validate(req, function (isValid, sender) {
        if (isValid) {

            var lid = req.body.LeagueId;

            League.findOne({ _id: lid }, function (err, league) {
                if (err) { }
                else {

                    var data = req.body;

                    var subject = data.Subject;
                    var fullMessage = data.Message;
                    var thread = data.ThreadId;
                    var threadcount = data.ThreadCount;
                    var parent = data.ParentId;
                    var toType = data.ToType;
                    var to = data.To;
                    var toList = data.ToList;

                    var tid = data.TeamId;
                    var toTeam = null;
                    if (data.ToTeam) {
                        toTeam = data.ToTeam; // {name: string, id:string}
                    }
                    var bodyType = data.BodyType;
                    var messageBody = {
                        Type: bodyType,
                        FromUserId: sender._id,
                        Subject: subject,
                        Message: fullMessage,
                        ReferenceId: null,
                        ReferenceType: "",
                        CreatedUTC: new Date().toISOString()
                    }
                    MessageBody.create(messageBody, function (err, dbMessageBody) {
                        if (err) {
                            res.json({ 'status': 500, response: err });
                        }
                        else {
                            var message = {
                                Type: toType,
                                ToUserId: "",
                                ToTeamName: "",
                                FromUserId: sender._id,
                                UserLeagueId: lid,
                                UserTeamId: tid,
                                CreatedUTC: new Date().toISOString(),

                                MessageBodyId: dbMessageBody._doc._id.toString(),
                                ReadUTC: "",
                                ThreadId: thread,
                                ThreadCount: threadcount,
                                ParentId: parent,
                                ChildId: ""
                            }

                            // default: get the users in the toList
                            var query = { _id: { $in: toList } };
                            if (toType == msgTypes.ToAdmins.index
                                || toType == msgTypes.ToSupport.index
                                || toType == msgTypes.ToCommissioner.index
                                || toType == msgTypes.ToAgents.index
                            ) {
                                query = { Roles: "Admin" };
                            }
                            else if (toType == msgTypes.ToAll.index) {
                                query = {};
                            }
                            else if (toType == msgTypes.ToDevTest.index) {
                                query = { LastName: "Dombrower" };
                            }
                            else if (toType == msgTypes.ToMyLeague.index) {
                                // find all the players in this league!
                                toList = [];

                                message.ToTeamName = league.Name; // force this into the message header

                                for (t = 0; t < league.Teams.length; t++) {
                                    var pmTeam = league.Teams[t];
                                    if (pmTeam.Owners && pmTeam.Owners.length > 0) {
                                        for (i = 0; i < pmTeam.Owners.length; i++) {
                                            var id = pmTeam.Owners[i].UserId
                                            if (id.match(/^[0-9a-fA-F]{24}$/)) {
                                                // Yes, it's a valid ObjectId, proceed with `findById` call.
                                                toList.push(id);
                                            } else {
                                                console.log("invalid ID:" + id + " for " + pmTeam.Owners.Name);
                                            }

                                        }
                                    }
                                }
                                //  query = { _id: { $in: toList } };

                                // include the admins:
                                query = { $or: [{ "Roles": "Admin" }, { _id: { $in: toList } }] };

                                if (toList.length == 0) {
                                    res.json({ 'status': 500, message: "Sorry, cannot find any representatives in the league " + league.Name });
                                    return;
                                }

                            }
                            else if (toType == msgTypes.ToOneTeam.index) {
                                // find all the players on this team!
                                toList = [];

                                message.ToTeamName = toTeam.name; // force this into the message header

                                var pmTeam = league.Teams[teamHelper.findTeamIndexInLeague(league, toTeam.id)];
                                if (pmTeam && pmTeam.Owners && pmTeam.Owners.length > 0) {
                                    for (i = 0; i < pmTeam.Owners.length; i++) {
                                        toList.push(pmTeam.Owners[i].UserId);
                                    }
                                    query = { _id: { $in: toList } };
                                } else {

                                    res.json({ 'status': 500, message: "Sorry, cannot find any representatives for the " + toTeam.name });
                                    return;
                                }

                            }
                            else if (toType == msgTypes.ToAllFans.index) {
                                query = { Roles: { $size: 1 } }

                            }
                            else if (toType != msgTypes.ToOne.index) {
                                query = { _id: ToUserId };
                                res.json({ 'status': 500, message: "This message destination not implemented yet" });
                                return;
                            }

                            var responseSent = false;
                            if (query) {
                                User.find(query, function (err, dbUsers) {
                                    if (err) { }
                                    else {
                                        messageHelper.sendPM(sender, dbUsers, dbMessageBody._doc.Subject, message, 0, function (err, msg) {
                                            if (!responseSent) {
                                                res.json({ 'status': 200, message: "Message(s) Sent" });
                                            }
                                            responseSent = true;
                                        });
                                    }
                                })
                            } else {
                                messageHelper.sendPM(sender, users, dbMessageBody._doc.Subject, message, 0, function () {
                                    if (!responseSent) {
                                        res.json({ 'status': 200, message: "Message(s) Sent" });
                                    }
                                    responseSent = true;
                                });
                            }
                        }
                    });
                }
            });
        }
        else {
            res.status(401).json({ 'status': 401, error: "Unauthorized" });
        }
    });
});

// *******************************************************
//
// messaging/sendPMReply
// ==> reply to a PM in RSports system
//
// *******************************************************
router.post('/api/messaging/sendPMReply', function (req, res) {
    utils.validate(req, function (isValid, sender) {
        if (isValid) {

            var originalMessage = req.body.original;
            var lid = originalMessage.UserLeagueId;
            var tid = originalMessage.UserTeamId;
            var newReply = req.body.reply;

            League.findOne({ _id: lid }, function (err, league) {
                if (err) { }
                else {

                    var data = originalMessage;

                    var subject = data.Body.Subject;
                    if (!subject.match(/^RE: /))
                        subject = "RE: " + subject;

                    var fullMessage = newReply;
                    var thread = data.ThreadId;
                    var threadcount = data.ThreadCount;
                    var parent = data.ParentId;
                    if (!parent || parent == "" || parent == 0) {
                        parent = data._id;
                    }
                    var toType = data.Type;
                    var to = data.To;
                    var toList = [data.ToUserId, data.FromUserId]; //  = data.ToList;

                    var tid = data.TeamId;
                    var toTeam = null;
                    if (data.ToTeamName) {
                        toTeam = data.ToTeamName; // {name: string, id:string}
                    }
                    var bodyType = data.BodyType;
                    var messageBody = {
                        Type: bodyType,
                        FromUserId: sender._id,
                        Subject: subject,
                        Message: fullMessage,
                        ReferenceId: null,
                        ReferenceType: "",
                        CreatedUTC: new Date().toISOString()
                    }

                    MessageBody.create(messageBody, function (err, dbMessageBody) {
                        if (err) {
                            res.json({ 'status': 500, response: err });
                        }
                        else {

                            // have created the message body, now create the two messages.
                            // this is the new reply message
                            var message = {
                                Type: toType,
                                ToUserId: "",
                                ToTeamName: "",
                                FromUserId: sender._id,
                                UserLeagueId: lid,
                                UserTeamId: tid,
                                ChildId: "",
                                CreatedUTC: new Date().toISOString(),

                                MessageBodyId: dbMessageBody._doc._id.toString(),
                                ReadUTC: "",
                                ThreadId: thread,
                                ThreadCount: threadcount,
                                ParentId: parent
                            }

                            // default: get the users in the toList
                            var query = { _id: { $in: toList } };
                            if (toType == msgTypes.ToAdmins.index
                                || toType == msgTypes.ToSupport.index
                                || toType == msgTypes.ToCommissioner.index
                                || toType == msgTypes.ToAgents.index
                            ) {
                                query = { $or: [{ "Roles": "Admin" }, { _id: { $in: toList } }] };

                            }
                            else if (toType == msgTypes.ToOne.index) {
                                query = { _id: originalMessage.ToUserId };
                                if (sender._id.toString() == originalMessage.ToUserId) {
                                    query = { _id: originalMessage.FromUserId };
                                }
                            }
                            else if (toType == msgTypes.ToAll.index) {
                                query = {};
                            }
                            else if (toType == msgTypes.ToDevTest.index) {
                                query = { LastName: "Dombrower" };
                            }
                            else if (toType == msgTypes.ToMyLeague.index) {
                                // find all the players in this league!
                                toList = [];

                                message.ToTeamName = league.Name; // force this into the message header

                                for (t = 0; t < league.Teams.length; t++) {
                                    var pmTeam = league.Teams[t];
                                    if (pmTeam.Owners && pmTeam.Owners.length > 0) {
                                        for (i = 0; i < pmTeam.Owners.length; i++) {
                                            var id = pmTeam.Owners[i].UserId
                                            if (id.match(/^[0-9a-fA-F]{24}$/)) {
                                                // Yes, it's a valid ObjectId, proceed with `findById` call.
                                                toList.push(id);
                                            } else {
                                                console.log("invalid ID:" + id + " for " + pmTeam.Owners.Name);
                                            }

                                        }
                                    }
                                }
                                //  query = { _id: { $in: toList } };

                                // include the admins:
                                query = { $or: [{ "Roles": "Admin" }, { _id: { $in: toList } }] };

                                if (toList.length == 0) {
                                    res.json({ 'status': 500, message: "Sorry, cannot find any representatives in the league " + league.Name });
                                    return;
                                }

                            }
                            else if (toType == msgTypes.ToOneTeam.index) {
                                // find all the players on this team!

                                message.ToTeamName = toTeam; // force this into the message header

                                var pmTeam = league.Teams[teamHelper.findTeamIndexInLeagueByRName(league, toTeam)];
                                if (pmTeam.Owners && pmTeam.Owners.length > 0) {
                                    for (i = 0; i < pmTeam.Owners.length; i++) {
                                        toList.push(pmTeam.Owners[i].UserId);
                                    }
                                    query = { _id: { $in: toList } };
                                } else {
                                    return;
                                    res.json({ 'status': 500, message: "Sorry, cannot find any representatives for the " + toTeam.name });

                                }

                            }
                            else if (toType == msgTypes.ToAllFans.index) {
                                query = { Roles: { $size: 1 } }
                                query = { $or: [{ Roles: { $size: 1 } }, { _id: { $in: toList } }] };
                            }
                            else if (toType != msgTypes.ToOne.index) {
                                // if the original sender, this works, but not the other person!
                                if (ToUserId == sender._id.toString()) {
                                    ToUserId = data.FromUserId;
                                }
                                query = { _id: ToUserId };
                                res.json({ 'status': 500, message: "This message destination not implemented yet" });
                                return;
                            }

                            var responseSent = false;
                            if (query) {
                                User.find(query, function (err, dbUsers) {
                                    if (err) { }
                                    else {

                                        // remove the sender from the list!!!
                                        for (i = 0; i < dbUsers.length; i++) {
                                            var id = dbUsers[i]._id.toString();
                                            if (sender._id == id) {
                                                dbUsers.splice(i, 1);
                                                break;
                                            }
                                        }
                                        if (dbUsers.length == 0) {
                                            res.status(500).json({ 'status': 500, error: "Error processing return addresses" });
                                            return;
                                        }
                                        messageHelper.sendPM(sender, dbUsers, dbMessageBody._doc.Subject, message, 0, function (err, msg) {
                                            if (!responseSent) {
                                                res.json({ 'status': 200, message: "Message(s) Sent" });
                                            }
                                            responseSent = true;
                                        });
                                    }
                                })
                            } else {
                                messageHelper.sendPM(sender, users, dbMessageBody._doc.Subject, message, 0, function () {
                                    if (!responseSent) {
                                        res.json({ 'status': 200, message: "Message(s) Sent" });
                                    }
                                    responseSent = true;
                                });
                            }
                        }
                    });
                }
            });
        }
        else {
            res.status(401).json({ 'status': 401, error: "Unauthorized" });
        }
    });
});


router.get('/api/messaging/inbox', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var page = req.query.page ? parseInt(req.query.page) : 0;
            var limit = req.query.limit ? parseInt(req.query.limit) : 5;

            var query = { ToUserId: user._id };
            if (req.query.new && req.query.new == true) {
                query = { "$and": [query, { ReadUTC: { "$ne": "" } }] };
            }

            League.findOne({ _id: req.query.lid }, function (err, league) {
                Message.find(query, function (err, messages) {
                    if (err) {
                        res.status(500).json({ 'status': 500, error: err });
                    }
                    else {
                        messageHelper.getFromUser(league, messages, 0, function (err, dbMessages) {
                            messageHelper.getMessageBody(dbMessages, 0, function (err, dbMessages) {
                                Message.count(query, function (err, count) {
                                    res.json({ 'status': 200, messages: dbMessages, count: count, page: page, limit: limit });
                                });
                            });
                        });
                    }

                }).sort({ CreatedUTC: -1 }).skip(page * limit).limit(limit);

            })
        }
        else {
                    res.status(401).json({ 'status': 401, error: "Unauthorized" });
                }
    });
});

router.get('/api/messaging/toLeague', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            League.findOne({ _id: req.query.lid }, function (err, league) { 
                var query = [
                    { $match: { Type: "3", UserLeagueId: req.query.lid } },
                    { $sort: { _id: 1, MessageBodyId: 1 } },
                    {
                        $group:
                        {
                            _id: "$MessageBodyId",
                            uniqueMessageBody: { $first: "$MessageBodyId" },
                            messageId: { $push: "$_id" }
                        }
                    }
                ];
                Message.aggregate(query, function (err, messageList) {
                    if (err || messageList.length == 0) {
                        res.status(500).json({ 'status': 500, error: "No messages found for this league", err: err });
                    } else {

                        // have a unique set of messageBodyId's
                        var messageIds = [];
                        for (i = 0; i < messageList.length; i++) {
                            messageIds.push(messageList[i].messageId[0]);
                        }

                        var page = req.query.page ? parseInt(req.query.page) : 0;
                        var limit = req.query.limit ? parseInt(req.query.limit) : 5;

                        Message.find({ _id: { $in: messageIds } }, function (err, messages) {
                            if (err) {
                                res.status(500).json({ 'status': 500, error: "No matching messages", err: err });
                            } else {
                                messageHelper.getFromUser(league, messages, 0, function (err, dbMessages) {
                                    messageHelper.getMessageBody(dbMessages, 0, function (err, dbMessages) {
                                        //Message.count(query, function (err, count) {
                                        res.json({ 'status': 200, messages: dbMessages, count: messageList.length, page: page, limit: limit });
                                        //});
                                    });
                                });
                            }
                        }).sort({ CreatedUTC: -1 }).skip(page * limit).limit(limit);
                    }
                });
            });
        }
        else {
            res.status(401).json({ 'status': 401, error: "Unauthorized" });
        }
    });
});

router.get('/api/messaging/getMessageTypes', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            res.json({ 'status': 200, types: msgTypes });
        }

        else {
            res.status(401).json({ 'status': 401, error: "Unauthorized" });
        }
    });
});

router.get('/api/messaging/outbox', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var msgTypeNames = [];
            for (var key in msgTypes) {
                // skip loop if the property is from prototype
                if (!msgTypes.hasOwnProperty(key)) continue;

                var name = msgTypes[key].menu;
                msgTypeNames.push(name);
            }

            League.findOne({ _id: req.query.lid }, function (err, league) {

                var page = req.query.page ? parseInt(req.query.page) : 0;
                var limit = req.query.limit ? parseInt(req.query.limit) : 5;

                var query = { FromUserId: user._id.toString() };
                if (req.query.new && req.query.new == true) {
                    query = { "$and": [query, { ReadUTC: { "$ne": "" } }] };
                }
                Message.find(query, function (err, messages) {
                    if (err) {
                        res.status(500).json({ 'status': 500, error: err });
                    }
                    else {
                        var msgIds = [];
                        var uniqueMessages = [];
                        for (i = 0; i < messages.length; i++) {
                            if (msgIds.indexOf(messages[i].MessageBodyId) == -1) {
                                msgIds.push(messages[i].MessageBodyId);
                                if (messages[i].Type != msgTypes.ToOne.index)
                                    messages[i]._doc.ToName = msgTypeNames[messages[i].Type];
                                uniqueMessages.push(messages[i]);
                            }
                        }
                        messageHelper.getToUser(uniqueMessages, 0, function (err, dbMessages) {
                            messageHelper.getMessageBody(dbMessages, 0, function (err, dbMessages) {
                                Message.count(query, function (err, count) {
                                    res.json({ 'status': 200, messages: dbMessages, count: count, page: page, limit: limit });
                                });
                            });
                        });
                    }
                }).sort({ CreatedUTC: -1 }).skip(page * limit).limit(limit);
            });
        }
        else {
            res.status(401).json({ 'status': 401, error: "Unauthorized" });
        }
    });
});

/*
router.get('/api/messaging/sent', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            Message.find({ FromUserId: user._id }, function (err, messages) {
                if (err) {
                    res.status(500).json({ 'status': 500, error: err });
                }
                else {
                    messageHelper.getFromUser(league, messages, 0, function (err, dbMessages) {
                        res.json({ 'status': 200, messages: dbMessages });
                    });
                }
            }).limit(25);
        }
        else {
            res.status(401).json({ 'status': 401, error: "Unauthorized" });
        }
    });
});
*/

module.exports = router;