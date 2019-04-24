var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var State = mongoose.model('State');
var Progress = mongoose.model('Progress');
var Content = mongoose.model('Content');


router.get('/api/common/states', function (req, res) {
    State.find({}, function (err, states) {
        res.json({ status: 200, "msg": "success", "states": states });
    });
});

router.get('/api/common/progress/get', function( req,res){
    /*
    Progress.find({}, function( err, progress) {
        if( err ) {
            res.json({ status: 201, "msg": "fail", "progress": null }); 
        } else {
            res.json({ status: 200, "msg": "success", "progress": progress });           
        }
    })
    */
    utils.getProgress(function (err, percentage) {
        if (err) {
            res.json({ status: 201, "msg": "fail", "progress": null });
        } else {
            res.json({ status: 200, "msg": "success", "progress": progress });
        }
    })
})

module.exports = router;