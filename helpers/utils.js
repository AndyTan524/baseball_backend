var crypto = require('crypto');
var jwt = require('jsonwebtoken');
var request = require('request');
var fs = require('fs');

var mongoose = require('mongoose');
var User = mongoose.model('User');
var Log = mongoose.model('Log');
var Progress = mongoose.model('Progress');

var moment = require('moment-timezone');

module.exports = {

    addLog: function (msg, level, route, method, callback) {
        var log = {
            Message: msg,
            Level: level,
            Route: route,
            Method: method,
            CreatedUTC: new Date().toISOString()
        };
        Log.create(log, function (err, result) {
            if (callback) {
                callback(err, result);
            }
        });
    },

    hashPassword: function (password, salt) {
        var sum = crypto.createHash('sha256');
        sum.update(password + salt);
        return sum.digest('base64');
    },

    genuuid: function (callback) {
        if (typeof (callback) !== 'function') {
            return this.uuidFromBytes(crypto.randomBytes(16));
        }

        crypto.randomBytes(16, function (err, rnd) {
            if (err) return callback(err);
            callback(null, this.uuidFromBytes(rnd));
        });
    },

    uuidFromBytes: function (rnd) {
        rnd[6] = (rnd[6] & 0x0f) | 0x40;
        rnd[8] = (rnd[8] & 0x3f) | 0x80;
        rnd = rnd.toString('hex').match(/(.{8})(.{4})(.{4})(.{4})(.{12})/);
        rnd.shift();
        return rnd.join('-');
    },

    getToken: function (req) {
        return typeof (req.headers['auth-token']) != "undefined" ? req.headers['auth-token'] : typeof (req.query.token) != "undefined" ? req.query.token : typeof (req.body) != "undefined" ? req.body.token : "";
    },

    generateRandomInteger: function (min, max) {
        return Math.floor(max - Math.random() * (max - min))
    },

    generateRandomNumeric: function (min, max) {
        return max - Math.random() * (max - min)
    },

    validate: function (req, callback) {
        var token = this.getToken(req);

        User.findOne({ token: token }, function (err, user) {
            if (err) {
                callback(false, err);
            }
            else {
                callback(user != null, user);
            }
        });

    },

    scrapeUrl: function (url, callback) {
        request({
            url: url,
            method: 'GET',
            headers: {
                'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36'
            }
        }, callback);
    },

    getTeamFromGameString: function (gameString, isHome) {
        if (gameString.indexOf("@") == -1) {
            return "";
        }
        var array = gameString.split('@');

        if (isHome) {
            if (array[0].indexOf("Yankees") > -1) {
                return "NYY";
            }
            else if (array[0].indexOf("Red Sox") > -1) {
                return "BOS"
            }
            else if (array[0].indexOf("Mariners") > -1) {
                return "SEA"
            }
            else if (array[0].indexOf("Tigers") > -1) {
                return "DET"
            }
            else if (array[0].indexOf("Padres") > -1) {
                return "SD"
            }
            else if (array[0].indexOf("Diamondbacks") > -1) {
                return "ARI"
            }
            else if (array[0].indexOf("Astros") > -1) {
                return "HOU"
            }
            else if (array[0].indexOf("Indians") > -1) {
                return "CLE"
            }
            else if (array[0].indexOf("Dodgers") > -1) {
                return "LAD"
            }
            else if (array[0].indexOf("Giants") > -1) {
                return "SF"
            }
            else if (array[0].indexOf("Jays") > -1) {
                return "TOR"
            }
            else if (array[0].indexOf("Cardin") > -1) {
                return "STL"
            }
            else if (array[0].indexOf("Marlins") > -1) {
                return "MIA"
            }
            else if (array[0].indexOf("Phillies") > -1) {
                return "PHI"
            }
            else if (array[0].indexOf("Marlins") > -1) {
                return "ATL"
            }
            else if (array[0].indexOf("Phillies") > -1) {
                return "NYM"
            }
            else if (array[0].indexOf("Nationals") > -1) {
                return "WAS"
            }
            else if (array[0].indexOf("Rockies") > -1) {
                return "COL"
            }
            else if (array[0].indexOf("Athletics") > -1) {
                return "OAK"
            }
            else if (array[0].indexOf("Angels") > -1) {
                return "LAA"
            }
            else if (array[0].indexOf("Brewers") > -1) {
                return "MIL"
            }
            else if (array[0].indexOf("Orioles") > -1) {
                return "BAL"
            }
            else if (array[0].indexOf("Twins") > -1) {
                return "MIN"
            }
            else if (array[0].indexOf("Royals") > -1) {
                return "KC"
            }
            else if (array[0].indexOf("Cubs") > -1) {
                return "CHC"
            }
            else if (array[0].indexOf("White Sox") > -1) {
                return "CHW"
            }
            else if (array[0].indexOf("Pirates") > -1) {
                return "PIT"
            }
            else if (array[0].indexOf("Reds") > -1) {
                return "CIN"
            }
            else if (array[0].indexOf("Rays") > -1) {
                return "TB"
            }
            else if (array[0].indexOf("Mets") > -1) {
                return "NYM"
            }
            else if (array[0].indexOf("Braves") > -1) {
                return "ATL"
            }
            else if (array[0].indexOf("Rangers") > -1) {
                return "TEX"
            }
        }
        else {
            if (array[1].indexOf("Yankees") > -1) {
                return "NYY";
            }
            else if (array[1].indexOf("Red Sox") > -1) {
                return "BOS"
            }
            else if (array[1].indexOf("Mariners") > -1) {
                return "SEA"
            }
            else if (array[1].indexOf("Tigers") > -1) {
                return "DET"
            }
            else if (array[1].indexOf("Padres") > -1) {
                return "SD"
            }
            else if (array[1].indexOf("Diamondbacks") > -1) {
                return "ARI"
            }
            else if (array[1].indexOf("Astros") > -1) {
                return "HOU"
            }
            else if (array[1].indexOf("Indians") > -1) {
                return "CLE"
            }
            else if (array[1].indexOf("Dodgers") > -1) {
                return "LAD"
            }
            else if (array[1].indexOf("Giants") > -1) {
                return "SF"
            }
            else if (array[1].indexOf("Jays") > -1) {
                return "TOR"
            }
            else if (array[1].indexOf("Cardin") > -1) {
                return "STL"
            }
            else if (array[1].indexOf("Marlins") > -1) {
                return "MIA"
            }
            else if (array[1].indexOf("Phillies") > -1) {
                return "PHI"
            }
            else if (array[1].indexOf("Marlins") > -1) {
                return "ATL"
            }
            else if (array[1].indexOf("Phillies") > -1) {
                return "NYM"
            }
            else if (array[1].indexOf("Nationals") > -1) {
                return "WAS"
            }
            else if (array[1].indexOf("Rockies") > -1) {
                return "COL"
            }
            else if (array[1].indexOf("Athletics") > -1) {
                return "OAK"
            }
            else if (array[1].indexOf("Angels") > -1) {
                return "LAA"
            }
            else if (array[1].indexOf("Brewers") > -1) {
                return "MIL"
            }
            else if (array[1].indexOf("Orioles") > -1) {
                return "BAL"
            }
            else if (array[1].indexOf("Twins") > -1) {
                return "MIN"
            }
            else if (array[1].indexOf("Royals") > -1) {
                return "KC"
            }
            else if (array[1].indexOf("Cubs") > -1) {
                return "CHC"
            }
            else if (array[1].indexOf("White Sox") > -1) {
                return "CHW"
            }
            else if (array[1].indexOf("Pirates") > -1) {
                return "PIT"
            }
            else if (array[1].indexOf("Reds") > -1) {
                return "CIN"
            }
            else if (array[1].indexOf("Rays") > -1) {
                return "TB"
            }
            else if (array[1].indexOf("Mets") > -1) {
                return "NYM"
            }
            else if (array[1].indexOf("Braves") > -1) {
                return "ATL"
            }
            else if (array[1].indexOf("Rangers") > -1) {
                return "TEX"
            }
        }
    },

    randomString: function (length, chars) {
        var result = '';
        for (var i = length; i > 0; --i) result += chars[Math.round(Math.random() * (chars.length - 1))];
        return result;
    },

    /*
    startProgress: function( leagueId, itemId, itemName, callback ) {
        var now = moment().tz("America/Los_Angeles");
        var nowF = now.format("llll") + " PST";
        Progress.findOneAndUpdate(
            { LeagueId: leagueId, ItemId: itemId, ItemName: itemName },
            {
                LeagueId: leagueId,
                ItemId: itemId,
                ItemName: itemName,
                Status: "Running",
                PercentComplete: 0,
                StartTime: nowF,
                EndTime: ""
            },
            { upsert: true , new: true}, 
            function (err, response) {
                if (err) {
                    if( callback)
                        callback(err, null)
                } else {
                    var id = response._id.toString();
                    if( callback )
                        callback(null, id );
                }
            }
        );
    },

    updateProgress: function( progressId, leagueId, itemId, itemName, increase, callback ) {
        var now = moment().tz("America/Los_Angeles");
        var nowF = now.format("llll") + " PST";
        if( progressId ) {
            var query = {_id: progressId};
        } else {
            var query =  { LeagueId: leagueId, ItemId: itemId, ItemName: itemName };
        }

        Progress.findOne( query,function (err, progress) {
                if (err) {
                    if( callback || progress==null )
                        callback(err, null)
                } else {
                    var percent = progress.PercentComplete + increase;
                    if( percent >= 100) {
                        percent = 100;
                        progress.Status = "Complete";
                        progress.EndTime = nowF;
                    }
                    progress.PercentComplete = percent;
                    progress.save( function(err, response) {
                        if( callback)
                            callback(err, response);
                    });
                }
            }
        );
    }
    */


    filePath: "/tmp/",

    startProgress: function (leagueId, itemId, itemName, callback) {
        var now = moment().tz("America/Los_Angeles");
        var nowF = now.format("llll") + " PST";

        fs.writeFile('eligibilityProgress.txt', "0", function (err) {
            if (err) throw err;
            console.log('Progress Saved!');
            if (callback)
                callback();
        });
    },

    getProgress: function (type, callback) {
        fs.readFile('eligibilityProgress.txt', 'utf8', function (err, data) {
            callback(err, data);
        });
    },

    updateProgress: function (progressId, leagueId, itemId, itemName, increase, callback) {
        var now = moment().tz("America/Los_Angeles");
        var nowF = now.format("llll") + " PST";
        fs.readFile('eligibilityProgress.txt', 'utf8', function (err, data) {
            var percent = 0;
            if (err || !data) {

            } else {
                if (data != "100") {
                    // then ok to increase the percentage
                    percent = Number(data) + increase;
                    if( percent > 100)
                        percent = 100;
                    fs.writeFile('eligibilityProgress.txt', percent, function (err) {
                        if (err) throw err;
                        console.log('Progress Saved: ' + percent);
                        if (callback)
                            callback();
                    });
                }
            }
        });


    }
};
