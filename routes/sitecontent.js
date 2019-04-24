var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var Api = mongoose.model('Api');
var User = mongoose.model('User');
var Content =  mongoose.model('Content');

var ownershipMatrix = require('../helpers/ownershipMatrix');

var csvHelper = require('../helpers/csvHelper');
var fs = require('fs');
var multer = require('multer'); 

var path = require('path');

var storage = multer.diskStorage({ //multers disk storage settings
    destination: function (req, file, cb) {
        cb(null, path.join(__dirname, "../uploads/"))
    },
    filename: function (req, file, cb) {
        var datetimestamp = Date.now();
        cb(null, file.fieldname + '-' + datetimestamp + '.' + file.originalname.split('.')[file.originalname.split('.').length - 1])
    }
});
var upload = multer({ //multer settings
    storage: storage
})

router.get('/api/sitecontent/ownership-data', function (req, res) {
    var tab = req.query.tab;
    var source = ownershipMatrix[tab];
  
   // res.json({ 'status': 200, dataset: source });

   var contentname = "ownermatrix" + tab;
   Content.findOne({name: contentname}, function( err, content){
       res.json({ 'status': 200, dataset: content.content.dataset });
   })

});


router.get('/api/sitecontent/content', function( req, res ) {

    Content.findOne({name: req.query.name }, function (err, content) {
        res.json({ status: 200, "msg": "success", "content": content });
    });

});

router.get('/api/sitecontent/get-settings', function( req, res ) {

    Content.findOne({name: 'settings' }, function (err, content) {
        res.json({ status: 200, "msg": "success", "settings": content.content.settings });
    });

});

router.post('/api/sitecontent/set-settings', function( req, res ) {

    Content.findOne({name: 'settings' }, function (err, content) {
        content.content.settings[req.body.setting] = req.body.value;
        content.markModified("content");
        content.save( function(err, content) {
            res.json({ status: 200, "msg": "success", "settings": content.content.settings });
        });


    });

});

router.post('/api/sitecontent/set-all-settings', function( req, res ) {

    Content.findOne({name: 'settings' }, function (err, content) {
        content.content.settings = req.body.settings;
        content.markModified("content");
        content.save( function(err, content) {
            if( err ) {
                res.json({ status: 200, "msg": "ERROR!", "settings": content.content.settings });
            } else {
            res.json({ status: 200, "msg": "successfully updated settings", "settings": content.content.settings });
            }
        });


    });

});

router.post('/api/sitecontent/upload-ownershipMatrix', upload.single('file'), function (req, res) {


    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var filename = req.file.filename;
            var tab = req.query.tab;

            var ownershipTabNames = [
                { name: "Investment", dataset: "Investment"},
                { name: "Team Earnings", dataset: "PotentialEarnings2017" },
                { name: "Earnings Details", dataset: "Earnings2017"},
                { name: "Records", dataset: "Winnings2017"},
                { name: "Farm System", dataset: "FarmSystem"},
            ];
        
            var contentname = "ownermatrix" + ownershipTabNames[tab].dataset;

           utils.addLog("Begin uploading file: " + contentname, 0, "/api/sitecontent/upload-ownershipMatrix", "sitecontent.js");

            if (!req.file) {
                res.status(500).json({ status: 500, "msg": "Could not find filename", "error": err });
            }
            else {
                csvHelper.parseFlat(fs.createReadStream(path.join(__dirname, "../uploads/" + filename)), function (data) {
                    if (data && data.length > 0) {

                        Content.findOne({ name: contentname }, function (err, result) {
                            if (err) {
                                res.status(500).json({ status: 500, "msg": "Couldn't find the collection" });
                            }
                            else {
                                result.content.dataset = data;
                                result.markModified("content");
                                result.save(function (err, response) {
                                    if (err) {
                                        res.status(500).json({ status: 500, "msg": "Couldn't save changes" });
                                    } else {
                                        res.json({ 'status': 200, msg: contentname + " uploaded and saved." });
                                    }
                                });

                            }
                        })
                    }
                    else {
                        res.status(500).json({ status: 500, "msg": "No data parsed in the csv" });
                    }
                });
            }
        }
    });
});

router.post('/api/sitecontent/upload-flat', upload.single('file'), function (req, res) {

    utils.validate(req, function (isValid, user) {
        if (isValid || true) {
            var filename = req.file.filename;
            var destination = req.query.type;
  
           utils.addLog("Begin uploading file: " + filename, 0, "/api/sitecontent/upload-flat", "to: " + destination);

            if (!req.file) {
                res.status(500).json({ status: 500, "msg": "Could not find filename", "error": err });
            }
            else {
                csvHelper.parseFlat(fs.createReadStream(path.join(__dirname, "../uploads/" + filename)), function (data) {
                    if (data && data.length > 0) {

                        Content.findOne({ name: destination }, function (err, result) {
                            if( err || result==null ) {
                                res.status(500).json({ status: 500, "msg": "Couldn't find the collection" });
                            }
                            else {
                            result.content.dataset = data;
                            result.markModified("content");
                            result.save(function (err, response) {
                                if (err) {
                                    res.status(500).json({ status: 500, "msg": "Couldn't save changes" });
                                } else {
                                    res.json({ 'status': 200, msg: destination + " uploaded and saved." });
                                }
                            });
                        }

                        })
                    }
                    else {
                        res.status(500).json({ status: 500, "msg": "No data parsed in the csv" });
                    }
                });
            }
        }
    });
});

router.post('/api/sitecontent/upload-json', upload.single('file'), function (req, res) {
    
        utils.validate(req, function (isValid, user) {
            if (isValid || true) {
                var filename = req.file.filename;
                var destination = req.query.type;
      
               utils.addLog("Begin uploading file: " + filename, 0, "/api/sitecontent/upload-flat", "to: " + destination);
    
                if (!req.file) {
                    res.status(500).json({ status: 500, "msg": "Could not find filename", "error": err });
                }
                else {
                    csvHelper.parse(fs.createReadStream(path.join(__dirname, "../uploads/" + filename)), function (data) {
                        if (data && data.length > 0) {
    
                            Content.findOne({ name: destination }, function (err, result) {
                                if( err || result==null ) {
                                    res.status(500).json({ status: 500, "msg": "Couldn't find the collection" });
                                }
                                else {
                                result.content.dataset = data;
                                result.markModified("content");
                                result.save(function (err, response) {
                                    if (err) {
                                        res.status(500).json({ status: 500, "msg": "Couldn't save changes" });
                                    } else {
                                        res.json({ 'status': 200, msg: destination + " uploaded and saved." });
                                    }
                                });
                            }
    
                            })
                        }
                        else {
                            res.status(500).json({ status: 500, "msg": "No data parsed in the csv" });
                        }
                    });
                }
            }
        });
    });

module.exports = router;