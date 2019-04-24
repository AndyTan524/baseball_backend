
/* *********************************
 *
 *          chron-hourly
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

var mongoose = require('mongoose');
var User = mongoose.model('User');
var Log = mongoose.model('Log');

var moment = require('moment-timezone');

var date = new Date();
var dateString = date.getFullYear() + "" + ("0" + (date.getMonth() + 1)).slice(-2) + "" + ("0" + (date.getDate() - 1)).slice(-2);

var now = moment().tz("America/Los_Angeles");
var nowF = now.format("llll") + " PST";

console.log("Cron running at " + nowF);


Log.create({
    Message: "Hourly Cron Running at: " + nowF,
    Level: "Cron",
    Route: "Cron",
    Method: "Cron.Hourly",
    CreatedUTC: new Date().toISOString()
});

console.log("wrote to db log");


/// gameDayHelper manages the game day sequence:
/*
    all times PST
    4am - import all the daily stats from the previous day MLB and Minors
    4:20 - run active player eligilbity
    4:40 - run inactive player eligilibty
    9am  - trigger all 9am roster moves
    10:30am - play all games, register wins, update standings, accumulate stats, send notifications
*/

gameDayHelper.nextGameDayActivity(function (msg) {
    var now = moment().tz("America/Los_Angeles");
    var nowF = now.format("llll") + " PST";
    console.log("Finished at " + nowF + ". Message: " + msg);
    process.exit();
});

// done.