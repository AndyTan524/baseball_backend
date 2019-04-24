// **********************************************************
//
// File: players.js 
// RFB Simulator: functions for getting the raw stats for players
//   getPlayers() gets all the raw MLB player batting data game-by-game
//   getBaserunning() gets all the raw MLB player baserunning data game-by-game
//   getFielding() gets all the raw MLB player fielding data game-by-game
//   getPitchers() gets all the raw MLB player pitching data game-by-game
// By: Eddie Dombrower and Adam Feldman
//
// **********************************************************


getPlayers = function (teamData) {
    var gr = getGameRequest();

    var team = teamData.name;
    var rdata = { Stats: [] };

    if (gr.useFakeData) {
        // read from local file

        var fulldata = require("./sampleDataFromMongo/batting.json");

        var count = 0;

        for (i = 0; i < fulldata.Stats.length; i++) {
            if (fulldata.Stats[i].Team == team) {
                if (fulldata.Stats[i].G != "0") {
                    // then appeared in the game
                    rdata.Stats.push(fulldata.Stats[i]);
                    count++;
                }
            }
        }

        fulldata = null;


    } else {

        // read from db

    }

    return (rdata);
}

getFielding = function (teamData) {
    var gr = getGameRequest();

    var team = teamData.name;
    var fdata = { Stats: [] };

    if (gr.useFakeData) {
        // read from local file

        var fulldata = require("./sampleDataFromMongo/fielding.json");

        var count = 0;

        for (i = 0; i < fulldata.Stats.length; i++) {
            if (fulldata.Stats[i].Team == team) {
                if (fulldata.Stats[i].G != "0") {
                    // then appeared in the game
                    fdata.Stats.push(fulldata.Stats[i]);
                    count++;
                }
            }
        }
        fulldata = null;

    } else {

        // read from db

    }

    return (fdata);
}

getBaserunning = function (teamData) {
    var gr = getGameRequest();

    var team = teamData.name;
    var bdata = { Stats: [] };

    if (gr.useFakeData) {
        // read from local file

        var fulldata = require("./sampleDataFromMongo/baserunning.json");

        var count = 0;

        for (i = 0; i < fulldata.Stats.length; i++) {
            if (fulldata.Stats[i].Team == team) {
                if (fulldata.Stats[i].G != "0") {
                    // then appeared in the game
                    bdata.Stats.push(fulldata.Stats[i]);
                    count++;
                }
            }
        }
        fulldata = null;


    } else {

        // read from db

    }

    return (bdata);
}

getPitchers = function (teamData) {
    var gr = getGameRequest();

    var team = teamData.name;
    var pdata = { Stats: [] };
    if (gr.useFakeData) {
        // read from local file

        var fulldata = require("./sampleDataFromMongo/pitching.json");

        var count = 0;

        for (i = 0; i < fulldata.Stats.length; i++) {
            if (fulldata.Stats[i].Team == team) {
                if (fulldata.Stats[i].BFP != "") {
                    // then faced at least 1 batter
                    pdata.Stats.push(fulldata.Stats[i]);
                    count++;
                }
            }
        }
        fulldata = null;

    } else {

        // read from db

    }

    return (pdata);
}