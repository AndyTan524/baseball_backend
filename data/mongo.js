var os = require('os');

var devPcs = [
    "55ceafab6158",
    "DESKTOP-9Q6Q2LS",
    "Luz-PC"
];

var isProd = false; // for production api

// version for "sandbox" or free mlab dev db
databaseServers = ["mongodb://service:ZpNnY1Nr9mM0OjxD@ds243085.mlab.com:43085/rfb", "mongodb://192.168.11.37/rfb", "mongodb://192.168.11.37/rfb", "mongodb://localhost/rfb"]

// version for paid mlab dev db
databaseServers = ["mongodb://rSportsDevTeam:aoN1a#OGZrBi#Kz*EpB0#HT!@ds131448-a0.mlab.com:31448,ds131448-a1.mlab.com:31448/rfb?replicaSet=rs-ds131448", "mongodb://192.168.11.37/rfb", "mongodb://192.168.11.37/rfb", "mongodb://localhost/rfb"]


devIndex = devPcs.indexOf(os.hostname()) > -1 ? devPcs.indexOf(os.hostname()) : 3;


// test on dev service.. set this to 0 when isProd == false;
devIndex = 0;

if (isProd) {
    databaseServers[0] = "mongodb://service:ZpNnY1Nr9mM0OjxD@ds147845-a0.mlab.com:47845,ds147845-a1.mlab.com:47845/rfb?replicaSet=rs-ds147845";
    devIndex = 0;
}

var mongoose = require('mongoose');
var gracefulShutdown;
var dbURI = databaseServers[devIndex];
mongoose.Promise = require('bluebird');
mongoose.connect(dbURI, { useMongoClient: true });

// CONNECTION EVENTS
mongoose.connection.on('connected', function () {
    console.log('Mongoose connected to ' + dbURI);
});
mongoose.connection.on('error', function (err) {
    console.log('Mongoose connection error: ' + err);
});
mongoose.connection.on('disconnected', function () {
    console.log('Mongoose disconnected');
});



// CAPTURE APP TERMINATION / RESTART EVENTS
// To be called when process is restarted or terminated
gracefulShutdown = function (msg, callback) {
    mongoose.connection.close(function () {
        console.log('Mongoose disconnected through ' + msg);
        callback();
    });
};
// For nodemon restarts
process.once('SIGUSR2', function () {
    gracefulShutdown('nodemon restart', function () {
        process.kill(process.pid, 'SIGUSR2');
    });
});
// For app termination
process.on('SIGINT', function () {
    gracefulShutdown('app termination', function () {
        process.exit(0);
    });
});
// For Heroku app termination
process.on('SIGTERM', function () {
    gracefulShutdown('Heroku app termination', function () {
        process.exit(0);
    });
});
 