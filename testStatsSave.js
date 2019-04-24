
/* *********************************
 *
 *         test Stats Save
 * 
 * ********************************* */

require('./data/mongo');
require("./models/user");
require("./models/api");
require("./models/stat");
require("./models/pitching");
require("./models/fielding");
require("./models/batting");
require("./models/baserunning");
require("./models/ytdfielding");
require("./models/pitchingMinors");
require("./models/fieldingMinors");
require("./models/battingMinors");
require("./models/baserunningMinors");
require("./models/ytdfieldingMinors");

require("./models/playerBatting");
require("./models/playerBattingMinors");
require("./models/playerBaserunning");
require("./models/playerBaserunningMinors");
require("./models/playerFielding");
require("./models/playerFieldingMinors");
require("./models/playerPitching");
require("./models/playerPitchingMinors");

require("./models/player");
require("./models/freeagent");
require("./models/email");
require("./models/league");
require("./models/team");
require("./models/schedule");
require("./models/standings");
require("./models/roster");
require("./models/log");
require("./models/deal");
require("./models/transaction");
require("./models/playerdaily");
require("./models/calendar");
require("./models/simPlayer");
require("./models/message");
require("./models/state");
require("./models/request");
require("./models/content");
require("./models/message");
require("./models/messageBody");
require("./models/arbitration");
require("./models/progress");
// for tracking season games used and stats
require("./models/gameDay");
require("./models/usedGame");
require("./models/rosterLineup");
require("./models/boxscore");
require("./models/accumulatedStats");
require("./models/tempMasterPlayer");
//ytd
require("./models/ytdstats");
require("./models/masterPlayer");


var utils = require('./helpers/utils');
var gameDayHelper = require('./helpers/gameDayHelper');
var importHelper = require('./helpers/importHelper');

var mongoose = require('mongoose');
var User = mongoose.model('User');
var Log = mongoose.model('Log');


var moment = require('moment-timezone');

var date = new Date();
var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate() - 1)).slice(-2);

var now = moment().tz("America/Los_Angeles");
var nowF = now.format("llll") + " PST";

console.log("Test Stats Save running:  " + nowF);

/*
Log.create({
    Message: "Test Stats Save: " + nowF,
    Level: "Cron",
    Route: "Cron",
    Method: "Cron.Hourly",
    CreatedUTC: new Date().toISOString()
});

console.log("wrote to db log");
*/


importHelper.savePlayerStats(function (msg) {
    var now = moment().tz("America/Los_Angeles");
    var nowF = now.format("llll") + " PST";
    console.log("Finished saving player stats at " + nowF + ". Message: " + msg);
    process.exit();
});

// done.