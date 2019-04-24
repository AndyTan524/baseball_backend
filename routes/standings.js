var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var playerHelper = require('../helpers/playerHelper');
var utils = require('../helpers/utils');

var moment = require("moment");

var mongoose = require('mongoose');
var Standings = mongoose.model('Standings'); 

router.get('/api/standings/get', function (req, res) {
    Standings.find({}, function (err, standings) {
        if (err) {
            res.status(500).json(err);
        }
        else {

            res.status(200).json({ status: 200, msg: "Stats found", standings: standings });
        }
    })
});

router.post('/api/standings/update', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid && user.Roles.indexOf("Admin") > -1) {
            var updateStanding = req.body;
            Standings.findOne({ _id: updateStanding._id }, function (err, standing) {
                if (err) {
                    res.status(500).json({ status: 500, msg: "error", error, err });
                }
                else if (standing) {
                    standing.Conferences = updateStanding.Conferences;
                    standing.markModified("Conferences");
                    standing.save(function (err, result) {
                        if (err) {
                            res.status(500).json({ status: 500, msg: "error", error, err });
                        }
                        else {
                            res.status(200).json({ status: 200, msg: "Found and udpated", standing: standing });
                        }
                    });
                }
                else {
                    res.status(500).json({ status: 500, msg: "not found" });
                }
            })
        }
        else {
            res.status(401).json({ status: 401, msg: "not authorized" });
        }
    });
});

module.exports = router;