// **********************************************************
//
// File: rosters.js 
// RFB Simulator: functions for getting, setting the rosters and lineups
// By: Eddie Dombrower and Adam Feldman
//
// **********************************************************


function getTeamWithRoster( team ) {
    var gr = getGameRequest( );

    var name = team.name;
    var rdata = {};
    if( gr.useFakeData ) {
        // read from local file

       // test: rdata = require("./sampleDataFromMongo/rosters6.json");

       rdata = require("./sampleDataFromMongo/teamStats-" + name + ".json");

        // 0-0-0-0-0-0-0-0-0-0-0-0-0-0-0
        //
        // TODO: Add in YTD Stats for fielding and batting only
        //       
        // 0-0-0-0-0-0-0-0-0-0-0-0-0-0-0
        
        
    } else {

        // read from db

    }

    return( rdata );
}