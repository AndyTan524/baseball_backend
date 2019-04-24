var express = require('express'),
    app = express(),
    engines = require('consolidate'),
    assert = require('assert'); 
var session = require('express-session'); 


// **************** DB MODELS 1 of 2
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

var bodyParser = require('body-parser');

var sess = {
    secret: ',a,S^6@q_!J=GyBf',
    cookie: {},
    genid: function (req) {
        return utils.genuuid() 
    },
    saveUninitialized: false,
    resave: false
};

app.use(bodyParser.json({ limit: "50mb" }));
app.use(bodyParser.urlencoded({ limit: "50mb", extended: true, parameterLimit: 500000 }));
app.use(bodyParser.json());
app.use(bodyParser.json({ type: 'application/*+json' }));


app.use(session(sess));

app.set('superSecret', ")ct<vxYE72U+/Mb'");

app.use(function (req, res, next) {
    res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS, PUT, PATCH, DELETE');
    res.setHeader('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept, auth-token, Boundary');
    res.setHeader('Access-Control-Allow-Credentials', true);
    next();
}); 

var server = app.listen(8000, function () {
    var port = server.address().port;
    console.log('Express server listening on port %s.', port);

    var mongoose = require('mongoose');
    var Log = mongoose.model('Log');
    Log.create({
        Message: "App started.",
        Level: "App",
        Route: "App",
        Method: "App.server.listen",
        CreatedUTC: new Date().toISOString()
    });
});


// **** API SERVICES ROUTES 2 of 2
var user = require('./routes/user');
app.use('/', user);  
var stats = require('./routes/stats');
app.use('/', stats);  
var api = require('./routes/api');
app.use('/', api);  
var admin = require('./routes/admin');
app.use('/', admin);  
var schedule = require('./routes/schedule');
app.use('/', schedule);  
var scraper = require('./routes/scraper');
app.use('/', scraper);  
var player = require('./routes/player');
app.use('/', player);  
var leagues = require('./routes/leagues');
app.use('/', leagues);  
var owners = require('./routes/owners');
app.use('/', owners);  
var teams = require('./routes/teams');
app.use('/', teams);  
var deals = require('./routes/deals');
app.use('/', deals);  
var transactions = require('./routes/transactions');
app.use('/', transactions);  
var simulation = require('./routes/simulation');
app.use('/', simulation); 
var messaging = require('./routes/messaging');
app.use('/', messaging); 
var requests = require('./routes/requests');
app.use('/', requests); 
var rosters = require('./routes/rosters');
app.use('/', rosters);  
var common = require('./routes/common');
app.use('/', common); 
var sitecontent = require('./routes/sitecontent');
app.use('/', sitecontent); 
var arbitration = require('./routes/arbitration');
app.use('/', arbitration); 
var accumulatedStats = require('./routes/accumulatedStats');
app.use('/', accumulatedStats);
var standings = require('./routes/standings');
app.use('/', standings);

module.exports = app;
