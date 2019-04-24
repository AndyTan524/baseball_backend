var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var sendGridHelper = require('../helpers/sendGridHelper');
var messageHelper = require('../helpers/messageHelper');
var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var Api = mongoose.model('Api');
var User = mongoose.model('User');
var Message = mongoose.model('Message');
var Request = mongoose.model('Request');


router.get('/api/requests/get-applications', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var moment = require('moment-timezone');
            moment.tz.setDefault("America/Los_Angeles");

            Request.find({ Type: "Application" }, function (err, applications) {

                for( i=0; i<applications.length; i++) {
                    applications[i]._doc.SentTime = moment(applications[i]._doc.CreatedUTC).format("llll");
                }
                res.status(200).json({ 'status': 200, 'applications': applications });

            }).sort({ CreatedUTC: -1});;

        }
        else {
            res.status(401).json({ 'status': 401, error: "Unauthorized" });
        }
    });
});

router.get('/api/requests/get-application-details', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var moment = require('moment-timezone');
            moment.tz.setDefault("America/Los_Angeles");

            var apid = req.query.apid;
            Request.findOne({ Type: "Application" , _id: req.query.apid}, function (err, application) {
                if( err ) {
                    res.status(401).json({ 'status': 401, error: "Not found" }); 
                } else {

                    // create the html now:
  
                    // array of object keys and their associated questions
                    var keyQuestions = [
                        { key: "Owner", question: "Want to be an owner?" },
                        { key: "OwnerRoles", question: "What type of owner?" },

                        { key: "GM", question: "Want to work in front office?" },
                        { key: "FORoles", question: "What front office jobs?" },

                        { key: "Played", question: "Did you play baseball?" },
                        { key: "PlayLevel",question: "Highest level?" },
                        { key: "Coached",question: "Did you coach?" },
                        { key: "CoachLevel",question: "Highest level?" },
                        { key: "SelfRateHistory",question: "Baseball History" },
                        { key: "SelfRateStrategy",question: "Baseball Strategy" },
                        { key: "SelfRateRules",question: "Management Rules" },
                        { key: "SelfRateMetrics",question: "Advanced Stats" },
           
                        { key: "WorkedInBB",question: "Have you worked in baseball?" },
                        { key: "WorkedInRole",question: "In what role(s)?" },
                        { key: "WantToWorkInBB",question: "Do you want to work in baseball?" },
                        { key: "WouldWantRole",question: "In what role(s)?" },

                        { key: "FanLevel",question: "Rate yourself as a fan" },
                        { key: "QuestioningLevel",question: "Do you constantly question decisions?" },
           
                        { key: "RateOwning",question: "Owning as a dream job (1-10)" },
                        { key: "RateManaging",question: "GM as a dream job (1-10)" },
           
                        { key: "PlayedFantasy",question: "Have you played fantasy baseball?" },
                        { key: "FantasyYears",question: "How many years?" },
                        { key: "TypeOfFB",question: "What type(s)?" },
                        { key: "LikeFantasy",question: "What do like?" },
                        { key: "DislikeFantasy",question: "What don't you like?" },
           
                        { key: "Profession",question: "Your profession" },
                        { key: "LikeJob",question: "What do you like about your job?" },
                        { key: "DislikeJob",question: "What don't you like about your job?" },
           
                        { key: "FavTeam1",question: "Your 1st favorite team" },
                        { key: "FavTeam2",question: "Your 2nd favorite team" },
                        { key: "FavTeam3",question: "Your 3rd favorite team" },

                        { key: "OwnTeam1",question: "Your 1st choice to own/manage" },
                        { key: "OwnTeam2",question: "Your 2nd choice to own/manage" },
                        { key: "OwnTeam3",question: "Your 3rdchoice to own/manage" },

                        { key: "MustOwnMyTeam",question: "Important to own/mange one of these?" },

                        { key: "OtherTeamsOK",question: "Would you consider a different team?" },

                        { key: "Willing",question: "Willing to be interviewed" },
                        { key: "Time1",question: "Available time" },
                        { key: "Time2",question: "Available time" },
                        { key: "Time3", question: "Available time" }
             
               ];
           
                    html = "<table>";
                    html += "<tr><td >NAME:</td><td>" + application.FromFirstName + " " + application.FromLastName + "</td></tr>";
                    html += "<tr><td >DATE:</td><td>" + moment(application.CreatedUTC).format("llll") + "</td></tr>";
                    html += "<tr><td >EMAIL:</td><td><a href='mailto:"+application.FromEmail+"?Subject=Thank you for your application to RSports Baseball'>" + application.FromEmail + "</a></td></tr>";                
                    keyQuestions.forEach(function (item, index) {
                        // key: the name of the object key
                        // index: the ordinal position of the key within the object 
                        var answer = application.Details[item.key];
                        
                        if( Array.isArray(answer) ) {
                            html += "<tr><td >" + item.question + "</td><td>";  
                            comma = "";

                            for( i=0; i<answer.length; i++) {
                                var istrue = answer[i].value;
                                if( istrue ) {
                                     html += comma + answer[i].name;  
                                     comma = ", ";
                                }
                            }

                            html += "</td></tr>";
        
                        } else {
                            // not an array
                            html += "<tr><td >" + item.question + "</td><td>" + answer + "</td></tr>";                
                            
                        }
                    });
                    /* 
                    Object.keys(application.Details).forEach(function (key, index) {
                        // key: the name of the object key
                        // index: the ordinal position of the key within the object 
                        var term = application.Details[key];
                        
                        if( Array.isArray(term) ) {
                            for( i=0; i<term.length; i++) {
                                var istrue = term[i].value;
                                if( istrue )
                                html += "<tr><td></td><td>" + term[i].name + "</td></tr>";  
                            }
        
                        } else {
                            // not an array
                            html += "<tr><td >" + key+ ":</td><td>" + term + "</td></tr>";                
                            
                        }
                    });
                    */

                    html += "</table>";

                res.status(200).json({ 'status': 200, 'application': application, 'html' : html });
                }

            });

        }
        else {
            res.status(401).json({ 'status': 401, error: "Unauthorized" });
        }
    });
});
 
router.get('/api/requests/contact-us', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            Request.find({ Type: "Contact-Us" }, function (err, requests) {
                var moment = require('moment-timezone');
                moment.tz.setDefault("America/Los_Angeles");

                for( i=0; i<requests.length; i++) {
                    requests[i]._doc.SentTime = moment(requests[i]._doc.CreatedUTC).format("llll");
                }
                res.status(200).json({ 'status': 200, 'requests': requests });

            }).sort({ CreatedUTC: -1});;

        }
        else {
            res.status(401).json({ 'status': 401, error: "Unauthorized" });
        }
    });
});

router.get('/api/requests/news-requests', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            Request.find({ Type: "Newsletter" }, function (err, requests) {
        var moment = require('moment-timezone');
        moment.tz.setDefault("America/Los_Angeles");
        
                for( i=0; i<requests.length; i++) {
                    requests[i]._doc.SentTime = moment(requests[i]._doc.CreatedUTC).format("llll");
                }
                res.status(200).json({ 'status': 200, 'requests': requests });

            }).sort({ CreatedUTC: -1});;

        }
        else {
            res.status(401).json({ 'status': 401, error: "Unauthorized" });
        }
    });
});

// *******************************************
//
// requests/general-form
//
// Types:
//      General (Default)
//      Newsletter
//      Contact-Us
//      Request-Info
//      Feedback
//      Application
//      Application
//      Referral
//
// *******************************************

router.post('/api/requests/general-form', function (req, res) {
            var form = new Request();
            /*
            var requestSchema = new mongoose.Schema({
                Type: String,
                FromFirstName: String,
                FromLastName: String,
                FromEmail: String,
                FromPage: String,
                CreatedUTC: String,
                Details: Object
            });
            */
            var formerrors = "";

            if (req.body.Type) {
                form.Type = req.body.Type;
            } else {
                form.Type = "General";
            }

            if (req.body.Email) {
                form.FromEmail = req.body.Email;
            } else {
                formerrors += "Missing email.";
            }

            if (form.Type != "Newsletter") {
                // then must have all the names filled in!
                if (req.body.FirstName) {
                    form.FromFirstName = req.body.FirstName;
                } else {
                    formerrors += "Missing first name.";
                }
                if (req.body.LastName) {
                    form.FromLastName = req.body.LastName;
                } else {
                    formerrors += "Missing last name.";
                }
            }

            if (req.body.Page) {
                form.FromPage = req.body.Page;
            } else {
                form.FromPage = "Unknown";
            }
            form.CreatedUTC = new Date().toISOString();

            if (req.body.Details) {
                form.Details = req.body.Details;
            }

            if (formerrors.length > 0) {
                res.status(500).json({ 'status': 500, error: formerrors });
            } else {
                Request.create(form, function (err) {
                    if (err) {
                        res.status(500).json({ 'status': 500, message: "DB Failure" });

                    } else {
                        res.status(200).json({ 'status': 200, message: "Success" });

                        // now send email to admin team
                        var notificationType = "adminNotifyNewsletterRequest";
                        var subject = "rSports Admin Notification";
                        var customterms = {};
                        if (form.Type == "Contact-Us") {
                            notificationType = "adminNotifyContactUs";
                            subject = "rsports Admin - Contact Us";
                        }

                        // find the user who's logged in....
                        // 
                        var user = null;
                        if (req.body.user)
                            user = req.body.user;

                        // here's the db data:
                        /*
                        $scope.contactMsg = {
                            Type: "",
                            FirstName: "",
                            LastName: "",
                            Email: "",
                            Page: "",
                            Details: {
                                Purpose: "",
                                Message: ""
                            }
                        };
                        */

                        var customterms = {};
                        customterms["username"] = form.FromFirstName + " " + form.FromLastName;
                        customterms["useremail"] = form.FromEmail;
                        customterms["page"] = form.Page;
                        customterms["dateregistered"] = form.CreatedUTC;

                        if (form.Type == "Contact-Us") {
                            customterms["purpose"] = form.Details.Purpose;
                            customterms["datesent"] = form.CreatedUTC;
                            if (form.Details.Message)
                                customterms["message"] = form.Details.Message;
                        }

                        if (form.Type == "Refer-Friend") {
                            var notificationType = "adminNotifyReferFriend";
                            customterms["purpose"] = "Referral";
                            customterms["datesent"] = form.CreatedUTC;
                            customterms["referralinfo"] = form.Details.FriendFirstName + " " + form.Details.FriendLastName + ". Email: " + form.Details.FriendEmail;
                            if (form.Details.Message)
                                customterms["message"] = form.Details.Message;
                        }

                        if( form.Type == "Application") {
                            var notificationType = "adminNotifyApplication";
                            customterms["phone"] = form.Details.Phone;
                            customterms['rawdata'] = JSON.stringify(req.body.Details);
                        }

                        customterms["toAddress"] = [];
                        customterms["toName"] = [];
                        customterms.toAddress.push("eddie@dombrower.com");
                        customterms.toName.push("Eddie (Personal)");
                   
                        customterms.toAddress.push("admin@rsportsbaseball.com");
                        customterms.toName.push("RSports Administrator");


                       
                            sendGridHelper.sendEmail(notificationType, user, customterms, function (err, response) {
                                if (err) {
                                    //  res.json({ status: 500, msg: "Something went wrong with email", err: err });
                                }
                                else {
                                    //  res.status(200).json({ status: 200, msg: "Email sent.", user: result });
                                }
                            });
                    }

                });
            }
});

module.exports = router;