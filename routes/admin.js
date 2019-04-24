var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var User = mongoose.model('User');
var Email = mongoose.model('Email');

var Log = mongoose.model('Log');

router.get('/', function (req, res) {
    res.status(200).json({ status: 200, msg: "Version 0.5a" });
});

router.get('/api/admin/users/list', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid && user.Roles.indexOf("Admin") > -1) {

            var moment = require('moment-timezone');
            moment.tz.setDefault("America/Los_Angeles");

            User.find({}, function (err, users) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    for( i=0; i<users.length; i++ ) {
                        users[i]._doc["createTime"] = moment(users[i]._doc.CreatedUTC).format("llll");
                        users[i]._doc["loginTime"] = moment(users[i]._doc.LastLogonTimeUtc).format("llll");
                    }
                    res.status(200).json({ status: 200, msg: "Users found", users: users });

                }
            }).skip(parseInt(req.body.page) * parseInt(req.body.limit))
                .sort(req.body.sort)
                .limit(req.body.limit);
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.get('/api/admin/user/get', function (req, res) {

    utils.validate(req, function (isValid, user) {
        if (isValid && user.Roles.indexOf("Admin") > -1) {
            User.findOne({ _id: req.query._id }, function (err, user) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    res.json({ status: 200, "msg": "success", user: user });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    }); 
});

router.post('/api/admin/user/update', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid && user.Roles.indexOf("Admin") > -1) {
            User.findOneAndUpdate({ _id: req.body._id }, req.body, { new: true, upsert: true, setDefaultsOnInsert: true }, function (err, result) {
                if (err) {
                    res.status(200).json({ "status": 500, "msg": err.message });
                }
                else if (result) {
                    res.json({ 'status': 200, user: result });
                }
                else {
                    res.json({ 'status': 500, 'msg': "no user" });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    }); 
});

router.post('/api/admin/user/delete', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid && user.Roles.indexOf("Admin") > -1) {
            User.findByIdAndRemove({ _id: req.body._id }, function (err, result) {
                if (err) {
                    res.status(200).json({ "status": 500, "msg": err.message });
                }
                else if (result) {
                    res.json({ 'status': 200, result: result });
                }
                else {
                    res.json({ 'status': 500, 'msg': "no user" });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    }); 
});

router.post('/api/admin/user/updatePassword', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid && user.Roles.indexOf("Admin") > -1) {
            User.findOne({ _id: req.body._id }, function (err, user) {
                if (err) {
                    res.status(200).json({ "status": 500, "msg": err.message });
                }
                else if (user) {
                    user.Password = utils.hashPassword(req.body.Password, user.Salt);
                    user.save();
                    res.json({ 'status': 200, user: user });
                }
                else {
                    res.json({ 'status': 500, 'msg': "no user" });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    }); 
});

router.get('/api/admin/user/generatePassword', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid && user.Roles.indexOf("Admin") > -1) {
            User.findOne({ _id: req.query.uid }, function (err, dbUser) {
                if (err) {
                    res.status(200).json({ "status": 500, "msg": err.message });
                }
                else if (dbUser) {
                    var password = utils.genuuid().substring(0,8);
                    dbUser.token = "";
                    dbUser.Password = utils.hashPassword( password, dbUser.Salt);
                    dbUser.save();
                    res.json({ 'status': 200, user: dbUser, password: password });
                }
                else {
                    res.json({ 'status': 500, 'msg': "no user" });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    }); 
});

router.get('/api/admin/emails/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid && user.Roles.indexOf("Admin") > -1) {
            Email.find({}, function (err, emails) {
                if (err) {
                    res.status(200).json({ "status": 500, "msg": err.message });
                }
                else {
                    res.json({ 'status': 200, templates: emails });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.post('/api/admin/upload/csv', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid && user.Roles.indexOf("Admin") > -1) {
            var body = req.body;

                    res.status(200).json({ "status": 200, "msg":"no message" });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// /api/admin/logs/get
router.get('/api/admin/logs/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid && user.Roles.indexOf("Admin") > -1) {
            Log.find({}, function (err, logs) {
                if (err) {
                    res.status(200).json({ "status": 500, "msg": err.message });
                }
                else {
                    res.json({ 'status': 200, logs: logs });
                }
            }).sort({CreatedUTC: -1}).skip(0).limit(200);
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});
module.exports = router;