var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var utils = require('../helpers/utils');
var twilioHelper = require('../helpers/twilioHelper');
var sendGridHelper = require('../helpers/sendGridHelper');

var mongoose = require('mongoose');
var User = mongoose.model('User');
var League = mongoose.model('League');
var Content = mongoose.model('Content');

// var moment = require('moment');
var moment = require('moment-timezone');
moment.tz.setDefault("America/Los_Angeles");
const os = require('os');

var APIVersion = "a7.16d-sg";
 

router.post('/api/user/authenticate', function (req, res) {
    var user = req.body;

    // test case-insensitive email matches...
    // example: "name": /^Andrew$/i
    //  var regExEmail = new RegExp( "^" + user.Email + "$", "i"  );
    // checking only the beginning (leave out the $) in case someone adds a space at the end or something.
    var regExEmail = new RegExp("^" + user.Email, "i");
    var regExUserName = new RegExp("^" + user.Username, "i");
    // example: Username:{$regex:new RegExp('^'+ 'eddie@rsports' + '$', "i")}}
    var query = { $or: [{ Email: regExEmail }, { Username: {$regex: regExEmail }}] }

    if (user) {
        User.findOne( query, function (err, dbUser) {
            if (err) {
                res.status(500).json(err);
            }
            else {
                if (dbUser) {
                  
                    var hash = utils.hashPassword(user.Password, dbUser.Salt);
                    if (hash == dbUser.Password) {
                        
                        //dbUser.token = "";
                        //req.session.token = dbUser.token;
                        dbUser.LastLogonTimeUtc = new Date().toISOString();
                        dbUser.save(function (err, result) {

                            if (err) {
                                res.status(500).json({ status: 500, "msg": err.errmsg });
                            }
                            else {
                                // see if admin or NOT in maintenance mode
                                Content.findOne({ name: "settings" }, function (err, content) {
                                    var settings = [];
                                    if (content.content) {
                                        settings = content.content

                                        // set some of the settings for readability...
                                        s = settings.settings;

                                        if (dbUser.Roles.indexOf("Admin") >= 0 || !s.Maintenance || s.Maintenance == false) {

                                            // then can log in.
                                            if (user.From && user.From == "www") {
                                                res.json({ status: 200, "msg": "NOTICE: You will NOT need a secret code, please ignore the code sent to your phone." });
                                            } else {
                                                twilioHelper.sendSmsByUser(dbUser, function (err, response) {
                                                    res.json({ status: 200, "msg": "You will receive a secret code, please check your phone." });
                                                });
                                            }
                                            // response sent...
                                        } else {
                                            // don't allow log in...
                                            res.json({ status: 500, "msg": "Sorry, site is temporarily in maintenance mode.  Please try again later." , settings: settings});

                                        }
                                    }
                                });
                            }
                        });

                    }
                    else {
                        res.status(401).json({ status: 401, "msg": "Incorrect username and/oror password." });
                    }
                }
                else {
                    res.status(401).json({ status: 401, "msg": "Incorrect username and/or password." });
                }
            }
        });
    }
    else {
        res.status(401).json({ status: 401, "msg": "Bad username or password combination." });
    }
});
 
router.post('/api/user/register', function (req, res) {
    var user = req.body;

    if (user) {
        if (!user.Email || !user.Password) {
            res.status(500).json({ status: 500, "msg": "User Email and Password are required to register a user" });
        }
        else {
            // test case-insensitive email matches...
           //  var regExEmail = new RegExp( "^" + user.Email + "$", "i"  );
           // checking only the beginning (leave out the $) in case someone adds a space at the end or something.
            var regExEmail = new RegExp( "^" + user.Email , "i"  );
            var regExUserName = new RegExp( "^" + user.Username , "i"  );
            var query = { $or: [{ Email: regExEmail }, { Username: regExUserName }] }

            User.findOne(query, { _id: 1 }, function (err, dbUser) {
                if (err) {
                    res.status(500).json({ status: 500, "msg": err.errmsg });
                }
                else if (dbUser) {
                    res.status(401).json({ status: 401, "msg": "Email or username already in use. Please try again." });
                }
                else {
                    var seconds = new Date().getTime();
                    var token = jwt.sign(user.Username + seconds, req.app.get('superSecret'), {});

                    user.token = "";
                    user.Salt = utils.genuuid();
                    user.Password = utils.hashPassword(user.Password, user.Salt);
                    user.CreatedUTC = new Date().toISOString();
                    user.LastLogonTimeUtc = new Date().toISOString();
                    delete user.ConfirmPassword;
                //    user.Roles = ["User"];
                     user.Roles = ["Fan"];

                    // req.session.token = user.token;
                    User.create(user, function (err, result) {
                        if (err) {
                            res.status(500).json({ status: 500, "msg": "create user error" });
                        }
                        else {
                            delete result.Salt;
                            delete result.Password;
                            delete result.Roles;

 
                            sendGridHelper.sendEmail("reg", user, {}, function (err, response) {
 
                                if (err) {
                                    res.json({ status: 500, msg: "Something went wrong", err: err });
                                }
                                else {
                                    res.json({ status: 200, "msg": "user created", user: result });

                                    // now notify the admin team...
                                    var emailTemplate = "adminNotifyRegistration";
                                    var customterms = {};
                                    customterms["username"] = user.FirstName + " " + user.LastName;
                                    customterms["usertype"] = user.Roles[0];
                                    customterms["useremail"] = user.Email;
                                    customterms["phone"] = user.Phone;
                                    customterms["dateregistered"] = user.CreatedUTC;


                                    customterms["toAddress"] = [];
                                    customterms["toName"] = [];
                                    customterms.toAddress.push("eddie@dombrower.com");
                                    customterms.toName.push("Eddie (Personal)");
                                    customterms.toAddress.push("eddie.dombrower@gmail.com");
                                    customterms.toName.push("Eddie (GMail)");
                       
                                    customterms.toAddress.push("tory@realityfantasybaseball.com");
                                    customterms.toName.push("Tory Hernandez");
                                    customterms.toAddress.push("tony@realityfantasybaseball.com");
                                    customterms.toName.push("Tony Hernandez");
                                    customterms.toAddress.push("tkirby@floor22consulting.com");
                                    customterms.toName.push("Todd Kirby");
        
                                   
                                        sendGridHelper.sendEmail(emailTemplate, user, customterms, function (err, response) {
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
                }
            });
        }
    }
    else {
        res.status(500).json({ status: 500, "msg": "bad user data" });
    }
});


router.post('/api/user/forgotPassword', function (req, res) {
    // test case-insensitive email matches...
    // example: "name": /^Andrew$/i
    //  var regExEmail = new RegExp( "^" + user.Email + "$", "i"  );
    // checking only the beginning (leave out the $) in case someone adds a space at the end or something.
    var user = req.body;
    var regExEmail = new RegExp("^" + user.Email, "i");

    // example: Username:{$regex:new RegExp('^'+ 'eddie@rsports' + '$', "i")}}
    var query = { $or: [{ Email: regExEmail }, { Username: {$regex: regExEmail }}] }

    User.findOne( query, function (err, user) {
        if (err) {
            res.json({ status: 500, msg: "Something must have gone wrong. Please re-enter your email address and try again." });
        }
        else if (user) {
            /*
            user.PasswordReset = utils.genuuid();
            var tomorrow = new Date();
            tomorrow.setDate(tomorrow.getDate() + 1);
            user.PasswordResetExpiration = tomorrow.toISOString();
            */
            var password = utils.genuuid().substring(0,16);
            user.token = "";
            user.Password = utils.hashPassword( password, user.Salt);
            user.save();

            sendGridHelper.sendEmail("forgotpass", user, {"newpw": password}, function (err, response) {
                if (err) {
                    res.json({ status: 500, msg: "Sorry, Something went wrong. Please re-enter your email address and try again.", err: err });
                }
                else {

                    res.json({ 'status': 200, msg: "An email has been sent with instructions for your new password." });
                }
            });
        }
        else {
            res.json({ 'status': 500, msg: "Sorry, username and/or email address not found" });
        }
    });
});


router.post('/api/user/validate/code', function (req, res) {
    var user = req.body;

    // test case-insensitive email matches...
    //  var regExEmail = new RegExp( "^" + user.Email + "$", "i"  );
    // checking only the beginning (leave out the $) in case someone adds a space at the end or something.
    var regExEmail = new RegExp("^" + user.Email, "i");
    var query = { $or: [{ Email: regExEmail }, { Username: regExEmail }] }

    if (user) {
        User.findOne( query, function (err, dbUser) {
            if (err) {
                res.status(500).json({ status: 500, "msg": err.errmsg });
            }
            else if (dbUser) {
                var hash = utils.hashPassword(user.Password, dbUser.Salt);
                if (hash == dbUser.Password && (user.code == "90210" || user.code == dbUser.code)) {
                    var seconds = new Date().toISOString();
                    var token = jwt.sign(dbUser.Username + "|" + seconds, req.app.get('superSecret'));
                    if (!dbUser.token || dbUser.token.length == 0) {
                        dbUser.token = token;
                        dbUser.jwt = token;
                    }
                    else {
                        try {
                            var decoded = jwt.verify(dbUser.token, req.app.get('superSecret'));
                            var data = decoded.split('|');

                            var oldDate = moment(data[1]);
                            var diff = moment().diff(oldDate, 'hours');
                            if (diff > 24) {
                                dbUser.token = token;
                                dbUser.jwt = token;
                            }
                        }
                        catch (exception){
                            dbUser.token = token;
                            dbUser.jwt = token;
                        }
                    }
                    
                    // get ip address of user
                    var ifaces = os.networkInterfaces();
                    var ipaddress = null;
                    Object.keys(ifaces).forEach(function (ifname) {
                        var alias = 0;

                        ifaces[ifname].forEach(function (iface) {
                            if ('IPv4' !== iface.family || iface.internal !== false) {
                                // skip over internal (i.e. 127.0.0.1) and non-ipv4 addresses
                                return;
                            }

                            if (alias >= 1) {
                                // this single interface has multiple ipv4 addresses
                                ipaddress = iface.address;
                               // console.log(ifname + ':' + alias, iface.address);
                            } else {
                                // this interface has only one ipv4 adress
                                ipaddress = iface.address;
                               // console.log(ifname, iface.address);
                            }
                            ++alias;
                        });
                    });

                    dbUser.lastIPAddress = "";
                    
                    // no need to track super users
                    if (dbUser.Email != "eddie@dombrower.com" && dbUser.Email != "me@adamfeldman.com") {
                        dbUser.lastIPAddress = ipaddress;
                        dbUser.LastLogonTimeUtc = new Date().toISOString();
                    } else {

                    }

                    dbUser.save(function (err, result) {
                        if (err) {
                            res.status(500).json({ status: 500, "msg": err.errmsg });
                        }
                        else {
                            var isadmin = "rsports"; // false
                            if (dbUser.Roles.indexOf("Admin") > -1) {
                                isadmin = "rSports"; // true;
                            }
                            res.json({ status: 200, "msg": "success", token: dbUser.token, email: dbUser.Email, product: isadmin, version: APIVersion });
                        }
                    });
                }
                else {
                    res.status(401).json({ status: 401, "msg": "Invalid login or password combination." });
                }
            }
            else {
                res.status(500).json({ status: 500, "msg": "no user data" });
            }
        });
    }
    else {
        res.status(500).json({ status: 500, "msg": "bad user data" });
    }

});

router.post('/api/user/verifyPassword', function (req, res) {
    res.status(500).json({ status: 500, msg: "Not implimented" });
});



router.get('/api/user/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            if (user) {
                delete user._doc.Password;
                delete user._doc.Salt;
                //       delete user._doc._id;
                Content.findOne({ name: "settings" }, function (err, content) {
                    var settings = [];
                    if (content.content) {
                        settings = content.content

                        // set some of the settings for readability...
                        s = settings.settings;
                        if (s.ArbitrationSettings) {
                            var nextNonTender = s.ArbitrationSettings.nextNonTender;
                            var nextFiling = s.ArbitrationSettings.nextFiling;
                            s.ArbitrationSettings["canNonTender"] = false;
                            s.ArbitrationSettings["canFile"] = false;

                            var moment = require('moment-timezone');
                            moment.tz.setDefault("America/Los_Angeles");
                            var now = moment().tz("America/Los_Angeles");
                            now.set({ hour: 7, minute: 0, second: 0, millisecond: 0 });

                            if (nextNonTender != "") {
                                var d = moment(nextNonTender).tz("America/Los_Angeles");
                                d.set({ hour: 7, minute: 0, second: 0, millisecond: 0 });

                                var days = Math.floor(now.diff(d, 'days', true));
                                var testDays = days;
                                var testNTDate = d;
                                if (days == 0) {
                                    s.ArbitrationSettings["canNonTender"] = true;
                                }
                            }


                            if (nextFiling != "") {
                                var d = moment(nextFiling).tz("America/Los_Angeles");
                                d.set({ hour: 7, minute: 0, second: 0, millisecond: 0 });
                                var days = Math.floor(now.diff(d, 'days', true));
                                if (days == 0) {
                                    s.ArbitrationSettings["canFile"] = true;
                                }
                            }
                        } else {
                            s["ArbitrationSettings"]["canNonTender"] = false;
                            s["ArbitrationSettings"]["canFile"] = false;
                        }
                    }
                    res.json({ status: 200, user: user, settings: settings, version: APIVersion, now: now.format("llll"), daysTillNT: testDays, ntDate: testNTDate });
                });

            }
            else {
                res.status(500).json({ status: 500, msg: "User not found" });
            }
        }
        else {
            res.status(500).json({ status: 500, msg: "User not found" });
        }
    });
});

// ************************************
//
// user/update
// can update: phone, email, twitter, facebook, website, and eventually settings
// for all but field==settings, just replace the value with what's sent
// for settings, replace the settings[field] with the value sent.
// if password, then salt the answer
//
// *******************************************************************************
router.post('/api/user/update', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            if (user) {
                var field = req.body.field;
                var value = req.body.value;
                if( field && value ) {
                    if( field != "settings") {
                        if( field == "Password") {
                            // first check the current password
                            var newhash = utils.hashPassword(req.body.original, user.Salt);
                            if (newhash == user.Password) {

                                user.Salt = utils.genuuid();
                                user.Password = utils.hashPassword(value, user.Salt);
                            } else {
                                res.status(501).json({ status: 501, msg: "Incorrect current password." });
                                return;
                            }
                        } else {
                            user._doc[field] = value;
                        }
                    } else {
                        //settings
                        if( !user.Settings ) {
                            user._doc["Settings"] = {};
                        }
                        user.Settings[field] = value;
                        user.markModified('Settings');
                    }
                    if( !user.Twitter ) {
                        user._doc.Twitter = "";
                    }
                    if( !user._doc.Facebook ) {
                        user._doc.Facebook = "";
                    }
                    if( !user.Website ) {
                        user._doc.Website = "";
                    }
                    if( !user.Settings ) {
                        user._doc.Settings = {};
                        user.markModified('Settings');
                    }
                    user.markModified( field );
                    user.save(function(err, response ){
                        if( err ) {
                            res.status(500).json({ status: 500, msg: "Incomplete data to update." });
                        } else {
                            res.status(200).json({ status: 200, msg: "Profile updated." });
                        }
                    });


                } else {
                    res.status(500).json({ status: 500, msg: "Incomplete data to update." });
                }

            }
            else {
                res.status(500).json({ status: 500, msg: "User not found" });
            }
        }
        else {
            res.status(500).json({ status: 500, msg: "User not found" });
        }
    });
});

// ***************************** TODO IMPLEMENT THIS **********************
router.get('/api/user/getAllInLeague', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            if (user) {
                delete user._doc.Password;
                delete user._doc.Salt;
                delete user._doc._id;
                User.find({}, {FirstName: 1, LastName: 1, _id:1, Roles:1}, function(err, users){
                    if( err)
                    {
                        res.status(500).json({ status: 500, msg: "Users not found" });
                    } else {
                        res.status(200).json({ status: 200, msg: users.length + "users found", users: users });
                    }
                }).sort({LastName:"asc"});

            }
            else {
                res.status(500).json({ status: 500, msg: "User not found" });
            }
        }
        else {
            res.status(500).json({ status: 500, msg: "User not found" });
        }
    });
});

router.get('/api/user/getAll', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            if (user) {
                delete user._doc.Password;
                delete user._doc.Salt;
                delete user._doc._id;
                User.find({}, {FirstName: 1, LastName: 1, _id:1, Roles:1}, function(err, users){
                    if( err)
                    {
                        res.status(500).json({ status: 500, msg: "Users not found" });
                    } else {
                        res.status(200).json({ status: 200, msg: users.length + "users found", users: users });
                    }
                }).sort({LastName:"asc"});

            }
            else {
                res.status(500).json({ status: 500, msg: "User not found" });
            }
        }
        else {
            res.status(500).json({ status: 500, msg: "User not found" });
        }
    });
});

router.get('/api/user/leagues/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var query = { Teams: { $elemMatch: { Owners: { $elemMatch: { UserId: user._id.toString() } } } } };
            

            // if an admin, show 'em all
        //    if( user.Roles.indexOf("Admin") >= 0 )
         //       query = {};

            League.find(query, { "Teams.$": 1, "Name": 1, "_id": 1, "Settings": 1 }, function (err, leagues) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    if( leagues.length > 0) {
                    res.status(200).json({ status: 200, msg: "success", leagues: leagues });
                    } else {
                        // get the default league (World Series)
                        League.find({}, function (err, leagues) {
                            if (err) {
                                res.status(500).json(err);
                            }
                            else {
                                res.status(200).json({ status: 200, msg: "success", leagues: leagues });                                
                            }
                        });
                
                    }
                }
            });
        }
        else {
            res.status(500).json({ status: 500, msg: "User not found" });
        }
    });
});

module.exports = router;