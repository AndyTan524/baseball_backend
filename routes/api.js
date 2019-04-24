var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var Api = mongoose.model('Api');


router.get('/api/routes/get', function (req, res) {
    var query = {};
    if (req.query._id) {
        query = {
            _id: req.query._id
        };
    }

    Api.find(query, function (err, routes) {
        if (err) {
            res.status(500).json(err);
        }
        else {

            res.status(200).json({ status: 200, msg: "", routes: routes });

        }
    });
});

router.post('/api/routes/create', function (req, res) {
    Api.create(req.body, function (err, response) {
        if (err) {
            res.status(500).json(err);
        }
        else {
            res.status(200).json({ status: 200, msg: "route added", route: response });
        }
    });
});

module.exports = router;