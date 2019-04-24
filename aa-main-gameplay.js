// **********************************************************
//
// File: a-main-gameplay.js ("a" is to keep it at the top of the list alphabeticall only)
// RFB Simulator: Main Code Entry
// By: Eddie Dombrower and Adam Feldman
//
// **********************************************************
require('./data/mongo');
require("./simulator/schedule");            // Functions for managing the schedule
 
require("./simulator/rosters");       // Step 1: get the full team for the two teams playing
require("./simulator/players");       // Step 2: get the raw mlb stats for all players (batting, fielding, running) and pitchers

require("./simulator/set-pitchers");  // Step 3: set up the pitching staff
require("./simulator/set-lineup");    // Step 4: set up the batting order and fielders

require("./simulator/sim-game");        // step 5: calculate the bonus Runs (Arw) and RBI (Abrw) values for each batter

require("./simulator/play-game");
require("./simulator/record-game");

require("./simulator/render-CSV");

// **************************************************
// 
// utility functions and data
//
// *************************************************
require("./simulator/utils");

// **************************************************
// 
// global array of customizable "constants" for equations
//
// *************************************************
algorithms = require("./simulator/algorithms.json");

// **************************************************
// 
// global template of box score
//
// *************************************************
boxscoreTemplate = require("./simulator/boxscore-template.json");

// **********************************************************
//
// main gameplay entry point
//
// **********************************************************

// **********************************************************
//
// global settings
//
// **********************************************************
var gameRequest = {};

    setGameRequest = function(request) {
        gameRequest.request = request;
    }

    getGameRequest = function() {
         return( gameRequest.request );
    }

var FAKE_DATA = true;

// **********************************************************
//
// figure out which games to play and how to play them
//
// **********************************************************

if( FAKE_DATA) {
    gamePlayRequest = {
        gameDay: "today",   // either "today" or date in "2017-08-09T02:29:40.399Z" format
        games: ["all"],     // array of games to play or games[0] == "all" for all games
        useFakeData: true,
        requireAvailableStats: false, // default: TRUE for real games (only use "unused" PA and BF games if true)
        requireMLE: false,   // default: TRUE for real games (Major Leauge Equivalent Calculations used if true)
        rerun: true,         // true means replay any played games and play unplayed games
                            // false means ONLY play unplayed games.
        saveCSVTestFile: true
    }
} else {
    // read gamePlayRequest from DB or caller
}

setGameRequest( gamePlayRequest );

// **********************************************************
//
// read in schedule for games to play
//
// **********************************************************
  
 var schedule = getSchedule( gamePlayRequest );


// **********************************************************
//
// for each game, set pitchers, lineups, and play the games
//
// **********************************************************

var numGames = schedule.games.length;

for( var g=0; g<numGames; g++ ) {

    // **********************************************************
    //
    // using rosters.js, create team object, and get each team's RFB player data in one object
    // including usage data 
    // note  creates a team object (found in game-objects.js)
    // 
    // NOTE: might need these to grab the minor leaguers AS NEEDED.
    // **********************************************************

    var game = schedule.games[g];
    var homeTeam = game.home;
    var visitTeam = game.visit;

// **********************************************************
    //
    // using rosters.js, read in the raw game-by-game stats for each team
    //      
    // NOTE: might need these to grab the minor leaguers AS NEEDED.
    //
    // NOTE: don't currently have YTD stats
    // **********************************************************

    var homeRoster  = getTeamWithRoster( homeTeam ); 
    var visitRoster = getTeamWithRoster( visitTeam );

  
    // **********************************************************
    //
    // set pitchers.  Visitors first
    // using lineup-pitcher.js, get each team's pitchers in one object
    // includes starters, relievers, closer and pen
    // results are added to the team objects (visitTeam, homeTeam)
    // NOTE: this is where the availability kicks in
    //
    // **********************************************************

    setPitchers( game, visitTeam, visitRoster );
    setPitchers( game, homeTeam, homeRoster ); 

    // **********************************************************
    //
    // set batting order.  Visitors first
    // using lineup-batters.js, get each team's position players in one object
    // includes batting order, and bench (including pinch hitters)
    // NOTE: This is where availability kicks in
    //
    // final batting order put into homeTeam and visitingTeam Objects as array of:
    // [playerID, playerName, position]
    // **********************************************************

  
    setLineup( game, visitTeam, visitRoster);
    setLineup( game, homeTeam, homeRoster); 

    // **********************************************************
    //
    // get Bonus
    // using get-bonus.js, get each team's offensive bonuses ( for calculating ArW and AbrW)
    // only for batters
    //
    // **********************************************************

    simulateOffense( visitTeam.batters.battingOrder, visitRoster);
    simulateOffense( homeTeam.batters.battingOrder, homeRoster);

    // **********************************************************
    //
    // play this game
    // using play-game.js, get each team's pitchers in one object
    // includes starters, relievers, closer and pen
    //
    // **********************************************************
    var boxScore = playGame( game, visitTeam, visitRoster, homeTeam, homeRoster);

    // **********************************************************
    //
    // record the game in the db and send notifications as necessary
    // using record-game.js
    // note boxScore has the gameID from the schedule in it
    //
    // **********************************************************
    recordGame( boxScore, game, g );
    

    // end of this game #g
}  

var theEnd = true;
// **********************************************************
//
// all games played.  If there's a wrap up. do it here.
//
// **********************************************************

