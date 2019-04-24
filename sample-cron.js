require('./data/mongo');
require("./models/user");
require("./models/log"); 

var utils = require('./helpers/utils');

utils.addLog("cron job success: " + new Date().toISOString(), 0, "cron-sample", "", function (err, result) {
    if (err) {
        console.log(err);
    }
    else {
        process.exit();
    }
});