
"use strict";
// **********************************************************
//
// File: schedule.js 
// RFB Simulator: functions for getting the schedule for the requested games
// By: Eddie Dombrower and Adam Feldman
//
// **********************************************************

// ****************************
// team object
class team {
    constructor(teamName, id) {
        this.abbreviation = teamName;
        this.id = id;
        this.name = teamName;
        this.color = "blue";
        this.textcolor = "white";
        this.accentcolor = "gray";
    }

};
// ***************************
// schedule object
class scheduleObj {
    constructor(request) {
        this.date = request.gameDay;
        this.games = [
            {
                __id: "0000-aaaa",
                home: new team("ARI", "4f88f73b-28b2-4ead-853e-dd20c46d8350"),
                visit: new team("ATL", "2d170399-070f-415f-b7d0-3490eee0d282"),
                dhGame: false,
                playedOn: "",
                score: "",
                boxScoreId: ""
            },
            {
                __id: "1111-bbbb",
                home: new team("BAL", "7fa97df7-b19a-49b9-a01b-e6e963da9978"),
                visit: new team("BOS", "590857c7-fa30-45dc-9e96-d4a8a7f913d6"),
                dhGame: true,
                playedOn: "20170408",
                score: "5-3",
                boxScoreId: "1234-abcd"
            }
        ];

        // list of teams (only list once) that appear in this day's games
        this.teams = [
            "BAL", "TOR", "LAD", "SFG"
        ];
        this.request = request;
    };
}

function getSchedule (request) {

    var sdata = {};
    if (request.useFakeData) {
        // read from local file

        // sdata = require("./sampleDataFromMongo/schedule.json");

        sdata = new scheduleObj(request);



    } else {

        // read from db

    }

    return (sdata);
}
