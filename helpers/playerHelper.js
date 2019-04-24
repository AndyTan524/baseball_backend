var utils = require('../helpers/utils');
var request = require('request');
var cheerio = require('cheerio');
var moment = require('moment-timezone');
var mongoose = require('mongoose');
var Team = mongoose.model('Team');
var Roster = mongoose.model('Roster');
var Player = mongoose.model('Player');
var Log = mongoose.model('Log');

/// these are current season's MLB stats
var Batting = mongoose.model('Batting');
var Pitching = mongoose.model('Pitching');
var Fielding = mongoose.model('Fielding');
var Baserunning = mongoose.model('Baserunning');

/// these are current season's MinorLeague stats
var BattingMinors = mongoose.model('BattingMinors');
var PitchingMinors = mongoose.model('PitchingMinors');
var FieldingMinors = mongoose.model('FieldingMinors');
var BaserunningMinors = mongoose.model('BaserunningMinors');

/// these are the MLB gameday stats organized by player's MLBId
var PlayerBatting = mongoose.model('PlayerBatting');
var PlayerBattingMinors = mongoose.model('PlayerBattingMinors');
var PlayerBaserunning = mongoose.model('PlayerBaserunning');
var PlayerBaserunningMinors = mongoose.model('PlayerBaserunningMinors');
var PlayerFielding = mongoose.model('PlayerFielding');
var PlayerFieldingMinors = mongoose.model('PlayerFieldingMinors');
var PlayerPitching = mongoose.model('PlayerPitching');
var PlayerPitchingMinors = mongoose.model('PlayerPitchingMinors');

// not sure what this is used for
var YtdFielding = mongoose.model('YtdFielding');

// previous season's stats
var YTDStats = mongoose.model("YtdStats");

var AccumulatedStats = mongoose.model('AccumulatedStats');

var simulationHelper = require('../helpers/simulationHelper');
var leagueHelper = require('../helpers/leagueHelper');
var playerStatus = require('../helpers/playerStatus');
var teamHelper = require('../helpers/teamHelper');
var messageHelper = require('../helpers/messageHelper');
var rosterLineupHelper = require('../helpers/rosterLineupHelper');

var Transaction = mongoose.model('Transaction');

require("../simulator/set-pitchers");  // Step 3: set up the pitching staff
require("../simulator/set-lineup");    // Step 4: set up the batting order and fielders

module.exports = {
    generateDefaultPlayersByTeam: function (league, callback) {
        var qry = { MlbTeam: league.Teams[0].TeamName, $and: [{ MlbDepth: { $exists: true } }, { MlbDepth: { $ne: "" } }] };
        Player.find(qry, function (err, players) {
            var roster = {
                LeagueId: league._id,
                TeamId: league.Teams[0]._id,
                CreatedUTC: new Date().toISOString(),
                TeamAbbr: league.Teams[0].TeamName,
                FortyManNL: [],
                FortyManAL: []
            }
            var bat = 0;
            var bench = 0;
            for (var i = 0; i < players.length; i++) {
                var battingOrder = players[i].MlbDepth.indexOf("1") == -1 && players[i].MlbDepth.indexOf("11") == -1 ? -1 : bat++;
                if (battingOrder > 7) {
                    battingOrder = -1;
                }
                if (players[i].Position == "P" && battingOrder > -1) {
                    battingOrder = 8;
                    bat--;
                }
                var benchOrder = battingOrder == -1 ? bench++ : -1;
                roster.FortyManNL.push({ "Image": players[i].Image ? players[i].Image : "http://gdx.mlb.com/images/gameday/mugshots/mlb/" + players[i].MlbId + "@2x.jpg", "PlayerId": players[i]._id.toString(), "MlbId": players[i].MlbId, MlbDepth: players[i].MlbDepth, "FullName": players[i].FullName, "Position": players[i].Position, "Status": 0, "TradeStatus": false, "BattingOrder": battingOrder, "Rdepth": players[i].MlbDepth, "MlbDepth": players[i].MlbDepth, "BenchOrder": benchOrder });
                roster.FortyManAL.push({ "Image": players[i].Image ? players[i].Image : "http://gdx.mlb.com/images/gameday/mugshots/mlb/" + players[i].MlbId + "@2x.jpg", "PlayerId": players[i]._id.toString(), "MlbId": players[i].MlbId, MlbDepth: players[i].MlbDepth, "FullName": players[i].FullName, "Position": players[i].Position, "Status": 0, "TradeStatus": false, "BattingOrder": battingOrder, "Rdepth": players[i].MlbDepth, "MlbDepth": players[i].MlbDepth, "BenchOrder": benchOrder });
            }

            var query = {
                $and: [
                    { LeagueId: league._id },
                    { TeamId: league.Teams[0]._id },
                ]
            };
            Roster.findOne(query, function (err, dbRoster) {
                if (dbRoster) {
                    dbRoster.TeamAbbr = league.Teams[0].TeamName;
                    dbRoster.FortyManNL = roster.FortyManNL;
                    dbRoster.markModified("FortyManNL");
                    dbRoster.FortyManAL = roster.FortyManAL;
                    dbRoster.markModified("FortyManAL");
                    dbRoster.save(function (err, response) {
                        if (err) {
                            console.log(err.message);
                        }
                        var isNL = simulationHelper.isTeamNL(league.Teams[0].TeamName);
                        dbRoster.FortyMan = isNL ? dbRoster.FortyManNL : dbRoster.FortyManAL;
                        callback(dbRoster.FortyMan);
                    });
                }
                else {
                    Roster.create(roster, function (err, result) {
                        if (err) {
                            console.log(err.message);
                        }
                        var isNL = simulationHelper.isTeamNL(league.Teams[0].TeamName);
                        roster.FortyMan = isNL ? roster.FortyManNL : roster.FortyManAL;
                        callback(roster.FortyMan);
                    });
                }
            });
        });
    },

    ids: [],
    players: [],

    getPlayersByIds: function (ids, callback) {
        if (ids) {
            this.ids = ids;
            this.players = [];
            this.getPlayer(0, [], callback);
        }
        else {
            callback([]);
        }
    },

    getPlayer: function (index, output, callback) {
        var context = this;
        if (index < this.ids.length) {
            Player.findOne({ _id: this.ids[index] }, function (err, player) {
                output.push(player);
                context.getPlayer(++index, output, callback);
            });
        }
        else {
            callback(output);
        }
    },

    getPlayersByName: function (data, callback) {
        if (data) {
            this.getPlayerName(0, data, callback);
        }
        else {
            callback([]);
        }
    },

    getPlayerName: function (index, source, callback) {
        var context = this;
        if (index < source.length) {
            Player.findOne({ FullName: source[index].Players[0].FullName }, function (err, player) {
                if (player) {
                    source[index].Players[0]._id = player._id;
                }
                Player.findOne({ FullName: source[index].Players[1].FullName }, function (err, player) {
                    if (player) {
                        source[index].Players[1]._id = player._id;
                    }
                    Player.findOne({ FullName: source[index].Players[2].FullName }, function (err, player) {
                        if (player) {
                            source[index].Players[2]._id = player._id;
                        }
                        Player.findOne({ FullName: source[index].Players[3].FullName }, function (err, player) {
                            if (player) {
                                source[index].Players[3]._id = player._id;
                            }
                            Player.findOne({ FullName: source[index].Players[4].FullName }, function (err, player) {
                                if (player) {
                                    source[index].Players[4]._id = player._id;
                                }
                                context.getPlayerName(++index, source, callback);
                            });
                        });
                    });
                });
            });
        }
        else {
            callback(source);
        }
    },


    // *******************************************************
    //
    // getEligiblePositions
    // Notes:
    //  1) we ignore the position in the master player list and the roster
    //  2) first we look at the current season stats to see what position the player can play (primary and secondary)
    //  3) then look at this season AND the previous season stats to find any tertiary posiitons
    // ******************************************************
    AllFieldingStats: null,
    AllDBStats: null,
    PlayerIds: null,

    getAllFieldingStats: function( index, level, callback ) {
        if (index == 0 || this.AllDBStats == null) {
            var context = this;
            var FieldDB = Fielding;
            var PlayerDB = PlayerFielding;
            if (level != "ML") {
                FieldDB = FieldingMinors;
                PlayerDB = PlayerFieldingMinors;
            }

            PlayerDB.find({ MLBId: { $in: context.PlayerIds } }, function (err, results) {
                context.AllDBStats = results;
                results = null;
                callback(err, context.AllDBStats);
                
            })
        } else {
            callback(null, this.AllDBStats);
        }
    },

    getEligiblePositions: function (index, players, gameRange, callback) {
        var context = this;

        var gameRangeFrom = 20170401;
        var gameRangeTo = 20170501;
        if (gameRange) {
            gameRangeFrom = gameRange.From;
            gameRangeTo = gameRange.To;
        }

        if (index > 200) {
            var bigIndex = index;
        }
        if (players[index] && index < players.length) {
            var player = players[index];

            // 1) get player stats for this season...
            var leagueId = player.LeagueId;
            var season = player.Season;

            // TESTING PRESEASON: 
            // season = "2017";
            // season handled in the gameRange.From...

            // set up player's primary, secondary, tertiary positions...
            var positionCount = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];  // position 0 won't get used
            var totalGames = 0;



            /*
            db.getCollection('fielding').aggregate([
                {
                    $project: {
                        GameDate: 1,
                      Stats: {
                         $filter: {
                            input: "$Stats",
                            as: "s",
                            cond: { $eq: [ "$$s.PlayerName", "Torres Gleyber" ] }
                      }
                   }
                }
                }
             ])
             */

      //      Fielding.find({ /* Season: season,  */ "Stats.MLBId": player.MlbId }, { "Stats.$": 1, "GameDate": 1 }, function (err, stats) {
      //          if (err || stats == null || stats.length == 0) {
     //   Fielding.find({ /* Season: season,  */ "Stats.MLBId": player.MlbId }, { "Stats": 1, "GameDate": 1 }, function (err, AllStats) {

     /*
      Fielding.aggregate([
            {
                $project: {
                    GameDate: 1,
                  Stats: {
                     $filter: {
                        input: "$Stats",
                        as: "s",
                        cond: { $eq: [ "$$s.MLBId", player.MlbId ] }
                  }
               }
            }
            }
         ], function (err, stats ) {
            if (err || stats == null || stats.length == 0) {
             */
            // do this the FIRST TIME we see the ELIGIBLE POSITIONS and it holds for all the other eligilibities below...
            if( index == 0 ) {
                context.PlayerIds = [];
                for( var p=0; p < players.length; p++) {
                    context.PlayerIds.push( players[p].MlbId);
                }
            }

            context.getAllFieldingStats(index, "ML", function(err, AllStats){
                if (err || AllStats == null || AllStats.length == 0) {
              

                } else {


                    var stats = [];
                    /*
                    for (var i = 0; i < AllStats.length; i++) {
                        for (var s = 0; s < AllStats[i].Stats.length; s++) {
                            var id = AllStats[i].Stats[s].MLBId;
                            if (AllStats[i].Stats[s].MLBId == player.MlbId) {
                                stats.push({ GameDate: AllStats[i].GameDate, Stats: AllStats[i].Stats[s] });
                            }
                        }
                    }
                    */
                    for (var i = 0; i < AllStats.length; i++) {
                        var id = AllStats[i].MLBId;
                        if (id == player.MlbId) {
                            for (var g = 0; g < AllStats[i].Games.length; g++) {
                                stats.push({ GameDate: AllStats[i].Games[g].GameDate, Stats: AllStats[i].Games[g].Stats });
                            }
                        }
                    }
                       
                    for (var i = 0; i < stats.length; i++) {
                      // for (s = 0; s < stats[i].Stats.length; s++) {
               {

                            // treat "" or null position as DH...
                            var gameDate = stats[i].GameDate;
                            var inrange = false;         // use YTD for positions.
                            if (gameDate >= gameRangeFrom) {
                                inrange = true;
                            }
                          // if (stats[i].Stats[s].Pos == "") {
                               if (stats[i].Stats.Pos == "") {
                                // stats[i].Stats[s].Pos = 10;
                                // stats[i].Stats[s].G = 1;
                                //   stats[i].Stats.Pos = 10;
                                //   stats[i].Stats.G = 1;
                            }

                          // if (inrange && stats[i].Stats[s].Pos && stats[i].Stats[s].G >= 1) {
                               if (inrange && stats[i].Stats.Pos && stats[i].Stats.G >= 1) {

                                totalGames++;
                            //var pos = stats[i].Stats[s].Pos;
                             var pos = stats[i].Stats.Pos;
                        
                                   if (pos == "" || pos == null) {
                                    pos = 10;    // DH
                                }
                                pos = Number(pos);
                                if (!isNaN(pos)) {
                                    positionCount[pos]++;
                                }
                            }

                        }
                    }
                }
                // ok, have positions counts
                var primary = [];
                var secondary = [];
                var tertiary = [];
                var positions = ["", "P", "CA", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "DH"];
                if (totalGames > 0) {
                    for (i = 1; i < 11; i++) {
                        var pct = positionCount[i] / totalGames;
                        if (pct >= 0.33) {
                            primary.push(positions[i]);
                        } else if (pct >= 0.25) {
                            secondary.push(positions[i]);
                        } else if (positionCount[i] > 0) {
                            tertiary.push(positions[i]);
                        }
                    }
                }

                // add in special tertiaries for SS, LF, CF, RF
                if (primary.indexOf("SS") >= 0) {
                    // can tertiary, 2b, 3b
                    if (primary.indexOf("2B") == -1 && secondary.indexOf("2B") == -1 && tertiary.indexOf("2B") == -1) {
                        tertiary.push("2B");
                    }
                    if (primary.indexOf("3B") == -1 && secondary.indexOf("3B") == -1 && tertiary.indexOf("3B") == -1) {
                        tertiary.push("3B");
                    }
                }

                // primary to primary...
                // add in special tertiaries for SS, LF, CF, RF
                if (primary.indexOf("LF") >= 0) {
                    // can tertiary, RF
                    if (primary.indexOf("RF") == -1) //  && secondary.indexOf("RF") == -1 && tertiary.indexOf("RF") == -1) {
                    {
                        primary.push("RF");
                    }
                }

                // add in special tertiaries for SS, LF, CF, RF
                if (primary.indexOf("RF") >= 0) {
                    // can tertiary, LF
                    if (primary.indexOf("LF") == -1)  //  && secondary.indexOf("LF") == -1 && tertiary.indexOf("LF") == -1) {
                    {
                        primary.push("LF");
                    }
                }

                // add in special tertiaries for SS, LF, CF, RF
                if (primary.indexOf("CF") >= 0) {
                    // can tertiary, LF, RF
                    if (primary.indexOf("LF") == -1) // && secondary.indexOf("LF") == -1 && tertiary.indexOf("LF") == -1) {
                    {
                        primary.push("LF");
                    }
                    if (primary.indexOf("RF") == -1) // && secondary.indexOf("RF") == -1 && tertiary.indexOf("RF") == -1) {
                    {
                        primary.push("RF");
                    }
                }

                // secondary to secondary...
                // add in special tertiaries for SS, LF, CF, RF
                if (secondary.indexOf("LF") >= 0) {
                    // can tertiary, RF
                    if (primary.indexOf("RF") == -1 && secondary.indexOf("RF") == -1) // && tertiary.indexOf("RF") == -1) {
                    {
                        secondary.push("RF");
                    }
                }

                // add in special tertiaries for SS, LF, CF, RF
                if (secondary.indexOf("RF") >= 0) {
                    // can tertiary, LF
                    if (primary.indexOf("LF") == -1 && secondary.indexOf("LF") == -1) //  && tertiary.indexOf("LF") == -1) {
                    {
                        secondary.push("LF");
                    }
                }

                // add in special tertiaries for SS, LF, CF, RF
                if (secondary.indexOf("CF") >= 0) {
                    // can tertiary, LF, RF
                    if (primary.indexOf("LF") == -1 && secondary.indexOf("LF") == -1) // && tertiary.indexOf("LF") == -1) {
                    {
                        secondary.push("LF");
                    }
                    if (primary.indexOf("RF") == -1 && secondary.indexOf("RF") == -1) // && tertiary.indexOf("RF") == -1) {
                    {
                        secondary.push("RF");
                    }
                }

                // and tertiary to to tertiary...
                // add in special tertiaries for SS, LF, CF, RF
                if (tertiary.indexOf("LF") >= 0) {
                    // can tertiary, RF
                    if (primary.indexOf("RF") == -1 && secondary.indexOf("RF") == -1 && tertiary.indexOf("RF") == -1) {
                        tertiary.push("RF");
                    }
                }

                // add in special tertiaries for SS, LF, CF, RF
                if (tertiary.indexOf("RF") >= 0) {
                    // can tertiary, LF
                    if (primary.indexOf("LF") == -1 && secondary.indexOf("LF") == -1 && tertiary.indexOf("LF") == -1) {
                        tertiary.push("LF");
                    }
                }

                // add in special tertiaries for SS, LF, CF, RF
                if (tertiary.indexOf("CF") >= 0) {
                    // can tertiary, LF, RF
                    if (primary.indexOf("LF") == -1 && secondary.indexOf("LF") == -1 && tertiary.indexOf("LF") == -1) {
                        tertiary.push("LF");
                    }
                    if (primary.indexOf("RF") == -1 && secondary.indexOf("RF") == -1 && tertiary.indexOf("RF") == -1) {
                        tertiary.push("RF");
                    }
                }

                // try to buy memory...
                stats = null;
                AllStats = null;

                // now look in the previous year's fielding data to get potential tertiary positions.
                YTDStats.find({ Name: player.FullName }, { Catcher: 1, FieldingIndex: 1 }, function (err, fielding) {
                    if (err || !fielding || fielding.length == 0) {

                    } else {
                        for (i = 0; i < fielding.length; i++) {
                            if (fielding[i].FieldingIndex && fielding[i].FieldingIndex.length) {
                                for (f = 0; f < fielding[i].FieldingIndex.length; f++) {
                                    if (fielding[i].FieldingIndex[f].G >= 20) {
                                        var pos = fielding[i].FieldingIndex[f].POS;

                                        // if not already in one of the lists... make it the tertiary position
                                        if (primary.indexOf(pos) == -1 && secondary.indexOf(pos) == -1 && tertiary.indexOf(pos) == -1) {
                                            tertiary.push(pos);
                                        }
                                    }
                                }
                            }
                            if (fielding[0].Catcher) {
                                if (fielding[0].Catcher.G >= 20) {
                                    // if not already in one of the lists... make it the tertiary position
                                    if (primary.indexOf("CA") == -1 && secondary.indexOf("CA") == -1 && tertiary.indexOf("CA") == -1) {
                                        tertiary.push("CA");
                                    }
                                }
                            }
                            // and tertiary to to tertiary...
                            // add in special tertiaries for SS, LF, CF, RF
                            if (tertiary.indexOf("LF") >= 0) {
                                // can tertiary, RF
                                if (primary.indexOf("RF") == -1 && secondary.indexOf("RF") == -1 && tertiary.indexOf("RF") == -1) {
                                    tertiary.push("RF");
                                }
                            }

                            // add in special tertiaries for SS, LF, CF, RF
                            if (tertiary.indexOf("RF") >= 0) {
                                // can tertiary, LF
                                if (primary.indexOf("LF") == -1 && secondary.indexOf("LF") == -1 && tertiary.indexOf("LF") == -1) {
                                    tertiary.push("LF");
                                }
                            }

                            // add in special tertiaries for SS, LF, CF, RF
                            if (tertiary.indexOf("CF") >= 0) {
                                // can tertiary, LF, RF
                                if (primary.indexOf("LF") == -1 && secondary.indexOf("LF") == -1 && tertiary.indexOf("LF") == -1) {
                                    tertiary.push("LF");
                                }
                                if (primary.indexOf("RF") == -1 && secondary.indexOf("RF") == -1 && tertiary.indexOf("RF") == -1) {
                                    tertiary.push("RF");
                                }
                            }
                        }
                    }

                    // ok.. filled out everything.. time to finish...
                    player["Primary"] = primary;
                    player["Secondary"] = secondary;
                    player["Tertiary"] = tertiary;
                    players[index] = player;

                    // try to save memory
                    fielding = null;

                    // finally, see if this player has accumulated stats, in which case, put them in the used and fully used games area.
                    AccumulatedStats.findOne({ LeagueId: players[index].LeagueId, PlayerId: players[index].PlayerId }, function (err, astats) {
                        if (err || !astats) {
                            context.getEligiblePositions(++index, players, gameRange, callback);
                            // if there is an incremental value, update the progress
                            if (gameRange.increment) {
                                utils.updateProgress(null, gameRange.leagueId, gameRange.teamId, gameRange.teamName, gameRange.increment, null);
                            }
                        } else {
                            for (var s = 0; s < astats.FullyUsedGames.length; s++) {
                                if (astats.FullyUsedGames[s].Season == 2018) {
                                    if (astats.FullyUsedGames[0].fullGames)
                                        players[index].FullGames = astats.FullyUsedGames[0].fullGames;
                                }
                            }
                            for (var s = 0; s < astats.PartialGames.length; s++) {
                                if (astats.PartialGames[s].Season == 2018) {
                                    if (astats.PartialGames[0].partialGames)
                                        players[index].PartialGames = astats.PartialGames[0].partialGames;
                                }
                            }

                            // drop in next days rest and next available game date
                            players[index].LastDayPlayed = astats.LastDayPlayed
                            players[index].NextAvilableGame = astats.NextAvilableGame;

                            // calculate how many games in a row they've played...
                            var GamesPlayedInARow = 0;
                            if (astats.DaysList.length > 0) {

                                GamesPlayedInARow = 1;
                                var previousGame = astats.DaysList[0];
                                for (let d = 1; d < astats.DaysList.length; d++) {
                                    if (previousGame - astats.DaysList[d] <= 1) {
                                        previousGame = astats.DaysList[d];
                                        GamesPlayedInARow++
                                    } else {
                                        // more than 1 game away
                                        break;
                                    }
                                }
                            }
                            players[index].GamesPlayedInARow = GamesPlayedInARow;

                            var day = moment().dayOfYear(astats.NextAvilableGame);
                            players[index].NextAvilableGameDate = day.format("MM/DD");
                            astats = null;
                            context.getEligiblePositions(++index, players, gameRange, callback);

                            // if there is an incremental value, update the progress
                            if (gameRange.increment) {
                                utils.updateProgress(null, gameRange.leagueId, gameRange.teamId, gameRange.teamName, gameRange.increment, null);
                            }
                        }
                    })

                })
            });
        } else {
            AllFieldingStats = null; // try to buy back memory
            callback(players);
        }
    },

    //
    // **************************** getAvailableBatting (for a roster) *****************************
    //
    /* example "exclusion" query
      db.getCollection('batting').find({
 
              $and:
          [
              { "GameDate": {$nin:["20170404", "20170405"]} },
              {
                  "Stats":
                  {
                      $elemMatch: 
                          {
                          "MLBId":  "605177"
                          } 
                  }
              }
          ]
 
}, { "Stats.$": 1, "GameDate": 1 })

TRANSLATES TO: 

      var query = {
          $and:
          [
              { "GameDate": {$nin: skipgamelist } },
              {
                  "Stats":
                  {
                      $elemMatch: 
                          {
                          "MLBId": players[index].MlbId 
                          } 
                  }
              }
          ]
      };
      */

     AllBattingStats: null,

     getAllBattingStats: function( index, level, callback ) {
         if( index==0  || this.AllDBStats == null) {
             var context = this;
             var BatDB = Batting;
             PlayerDB = PlayerBatting;
             if (level != "ML") {
                FieldDB = FieldingMinors;
                PlayerDB = PlayerBattingMinors;
            }

            PlayerDB.find({ MLBId: { $in: context.PlayerIds } }, function (err, results) {
                context.AllDBStats = results;
                results = null;
                callback(err, context.AllDBStats);
                
            })
            /*
             BatDB.find({  }, { "Stats": 1, "GameDate": 1 }, function (err, AllStats) {
                 context.AllDBStats = AllStats;
                 callback(err,AllStats);
             })
             */
         } else {
             callback(null,this.AllDBStats);
         }
     },

    getAvailableBatting: function (index, players, level, gameRange, callback) {
        var context = this;

        var gameRangeFrom = 20170401;
        var gameRangeTo = 20170501;
        if (gameRange) {
            gameRangeFrom = gameRange.From;
            gameRangeTo = gameRange.To;
        }

        if (players[index] && index < players.length) {

            var skipgamelist = players[index].FullGames;
            if (!skipgamelist) {
                skipgamelist = [];
            }


            var getYTDTotals = true;

            if (getYTDTotals) {
                // get all games
                var query = {
                    $and:
                        [
                            {
                                "Stats":
                                {
                                    $elemMatch:
                                    {
                                        "MLBId": players[index].MlbId
                                    }
                                }
                            }
                        ]
                };

            } else {
                // GET ONLY ELIGIBLE GAMES..
                var query = {
                    $and:
                        [
                            { "GameDate": { $nin: skipgamelist } },
                            {
                                "Stats":
                                {
                                    $elemMatch:
                                    {
                                        "MLBId": players[index].MlbId
                                    }
                                }
                            }
                        ]
                };
            }
            var projection = { "Stats.$": 1, "GameDate": 1 };


            var BatDB = Batting;
            if (level != "ML")
                BatDB = BattingMinors;

            // get ALL the unplayed games for a single player, then add them up...
        //     BatDB.find(query, projection, function (err, stats) {
            /*
            BatDB.aggregate([
                {
                    $project: {
                        GameDate: 1,
                      Stats: {
                         $filter: {
                            input: "$Stats",
                            as: "s",
                            cond: { $eq: [ "$$s.MLBId", players[index].MlbId ] }
                      }
                   }
                }
                }
             ], function(err, stats) {
                 */
                context.getAllBattingStats(index, level, function(err, AllStats){
                    if (err || AllStats == null || AllStats.length == 0) {
                  

                    } else {

                        var stats = [];
                        var player = players[index];
                        /*
                        for (var i = 0; i < AllStats.length; i++) {
                            for (var s = 0; s < AllStats[i].Stats.length; s++) {
                                var id = AllStats[i].Stats[s].MLBId;
                                if (AllStats[i].Stats[s].MLBId == player.MlbId) {
                                    stats.push({ GameDate: AllStats[i].GameDate, Stats: AllStats[i].Stats[s] });
                                }
                            }
                        }
                        */
                        for (var i = 0; i < AllStats.length; i++) {
                            var id = AllStats[i].MLBId;
                            if (id == player.MlbId) {
                                for (var g = 0; g < AllStats[i].Games.length; g++) {
                                    stats.push({ GameDate: AllStats[i].Games[g].GameDate, Stats: AllStats[i].Games[g].Stats });
                                }
                            }
                        }



                delete players[index].Salary;
                if (stats && stats.length > 0) {


                    if (level == "ML") {
                        players[index].PA1 = [];
                        players[index].PA2 = [];
                        players[index].FirstFullOffensiveGame = "";
                        // count on this being the first availability to get called so it clears it out once.
                        players[index].CanStart = false;
                    } else {
                        players[index].PA1Minors = [];
                        players[index].PA2Minors = [];
                    }

                    var firstPA3 = -1;
                    var firstPA3Minors = -1;
                    var highestPA = 0;
                    var highestPAMinors = 0;

                    var YTDGames = { eStats: {} };

                    for (var i = 0; i < stats.length; i++) {

                        var gameDate = stats[i].GameDate;
                        var actualGameDate = gameDate;  // for compare below
                        if (level != "ML")
                            gameDate = gameDate + "Minors";

                        // for each day's available stats...
                      //  if (stats[i].Stats && stats[i].Stats.length > 0 && stats[i].Stats[0].G != 0 && stats[i].Stats[0].G != "") {
                          //  for( var s=0; s<stats[i].Stats.length; s++) {
                          //  var data = stats[i].Stats[s];
                          if (stats[i].Stats && stats[i].Stats.G != 0 && stats[i].Stats.G != "") {
                            {
                                var data = stats[i].Stats;

                            delete data.TeamNbr;
                            delete data.Team;
                            delete data.PlayerId;
                            delete data.PlayerName;
                            delete data.MLBId;
                            delete data.FirstName;
                            delete data.LastName;

                            if (getYTDTotals) {
                                if (YTDGames.eStats[gameDate]) {
                                    gameDate = gameDate + "-NC"; // for double header (though this game might be first)
                                }
                                YTDGames.eStats[gameDate] = { Batting: data };
                            }

                            var inrange = false;
                            if (actualGameDate >= gameRangeFrom && actualGameDate <= gameRangeTo) {
                                inrange = true;
                            }
                            if (inrange && skipgamelist.indexOf(gameDate) == -1) {

                                // ((((((((((((((((((((((((((((( TODO: EXCLUDE PA'S FROM PARTIAL GAMES! )))))))))))))))))))))))))))))
                                // not skipping this game
                                if (!players[index].eStats) {
                                    players[index]['eStats'] = {};
                                }
                                if (!players[index].gamesList) {
                                    players[index].gamesList = [];
                                }
                                if (players[index].eStats[gameDate]) {
                                    gameDate = gameDate + "-NC"; // for double header (though this game was first)
                                }

                                // put in level here...
                                data["Level"] = level;

                                players[index].eStats[gameDate] = { Batting: data };
                                players[index].gamesList.push(gameDate);


                                // get remaining PA
                                var remainingPA = data.PA;


                                // see if there are partially used PA in this gamedate
                                if (remainingPA > 0) {
                                    // example of partial games
                                    // PartialGames: {"20170405": {id:"1234", paTotal: 6, paUsed:3, ipUsed:0} , "20170409": {id:"1234", paTotal: 9, paUsed:3, ipUsed:0} },
                                    if (players[index].PartialGames[gameDate]) {
                                        remainingPA = remainingPA - players[index].PartialGames[gameDate].paUsed;
                                    }
                                }
                                data["PAAvailable"] = remainingPA;

                                if (remainingPA >= 1 && players[index].Primary && players[index].Primary.length == 0) {
                                    // allow him to dh
                                    players[index].Primary[0] = "DH";
                                }

                                if (level == "ML") {
                                    if (remainingPA > highestPA) {
                                        players[index]["highestPA"] = gameDate;
                                        highestPA = remainingPA;
                                    }
                                    if (remainingPA == 1)
                                        players[index].PA1.push(gameDate);
                                    if (remainingPA == 2)
                                        players[index].PA2.push(gameDate);


                                    if (remainingPA >= 3) {
                                        // then could be first available full game...
                                        // if first, or more recent...
                                        if (players[index].FirstFullOffensiveGame == "" || (Number(gameDate) > Number(players[index].FirstFullOffensiveGame))) {
                                            players[index].FirstFullOffensiveGame = gameDate;
                                        }
                                        if (players[index].FirstFullOffensiveGame == "" || (gameDate > players[index].FirstFullOffensiveGame)) {
                                            players[index].FirstFullOffensiveGame = gameDate;
                                        }

                                        players[index].CanStart = true;
                                    }
                                } else {

                                    // same but for minors...
                                    if (remainingPA > highestPAMinors) {
                                        players[index]["highestPA"] = gameDate;
                                        highestPAMinors = remainingPA;
                                    }
                                    if (remainingPA == 1)
                                        players[index].PA1Minors.push(gameDate);
                                    if (remainingPA == 2)
                                        players[index].PA2Minors.push(gameDate);


                                    if (remainingPA >= 3) {
                                        // then could be first available full game...
                                        // assumes this is AFTER we do the major leagues...
                                        if (players[index].FirstFullOffensiveGame == "") {
                                            players[index].FirstFullOffensiveGame = gameDate;
                                        }
                                    }
                                }
                            }
                        }
                        }
                    }

                    // see if he can start..
                    if (players[index].CanStart == false && players[index].PA2 && pa1 && players[index].PA1) {
                        var pa2 = 2 * players[index].PA2.length * 2;
                        var pa1 = players[index].PA1.length;
                        if (pa1 + pa2 >= 3) {
                            players[index].CanStart = true;
                        }
                    }

                    // total up all the unused game stats here
                    context.totalUnusedBatting(players[index], level);

                    if (getYTDTotals) {
                        // total up the entire year of mlb game stats.
                        context.totalUnusedBatting(YTDGames, level);
                    }
                    if (getYTDTotals) {
                        if (players[index].eStats) {
                            if (level == "ML") {
                                players[index].eStats["YTDTotalBatting"] = teamHelper.calculateYTDStats(YTDGames.eStats.TotalBatting, false);
                            }
                            else {
                                players[index].eStats["YTDTotalBattingMinors"] = teamHelper.calculateYTDStats(YTDGames.eStats.TotalBattingMinors, false);

                            }
                        }
                    }

                } else {
                    //put in empty stats...
                }

                stats = null;
                AllStats = null;
                }

                context.getAvailableBatting(++index, players, level, gameRange, callback);
                // if there is an incremental value, update the progress
                if (gameRange.increment) {
                    utils.updateProgress(null, gameRange.leagueId, gameRange.teamId, gameRange.teamName, gameRange.increment, null);
                }


            });
        } else {
            AllBattingStats = null;
            callback(players);
        }
    },

    skipStatsToAdd: ["Pos", "Level", "Season", "FullName", "PlayerId", "GameDate", "cERA", "MlbId", "GameString", "GameId", "Decision", "IPG", "CalendarDay", "IPPG"],

    totalUnusedBatting: function (player, level) {
        // players[index].eStats[gameDate] = {Batting: data};


        if (player.eStats) {

            var TOTALFIELD = "TotalBatting";
            if (level != "ML") {
                TOTALFIELD = "TotalBattingMinors";
            }

            if (!player.eStats[TOTALFIELD]) {
                player.eStats[TOTALFIELD] = {};
            }
            for (gameDate in player.eStats) {
                if (gameDate.charAt(0) == "2" && player.eStats[gameDate].Batting) {
                    var nextStats = player.eStats[gameDate].Batting;
                    for (stat in nextStats) {
                        // ok, special values taken care of.
                        //
                        if (this.skipStatsToAdd.indexOf(stat) == -1) {
                            // don't skip
                            value = nextStats[stat];
                            if (!player.eStats[TOTALFIELD][stat]) {
                                player.eStats[TOTALFIELD][stat] = 0;
                            }
                            if (value == null || value === false || isNaN(value))
                                value = 0;
                            player.eStats[TOTALFIELD][stat] = Number(player.eStats[TOTALFIELD][stat]) + Number(value);
                        }
                    }

                }
            }
            // calculate averages.
            teamHelper.calculateYTDStats(player.eStats[TOTALFIELD], false);
        }
    },


    //
    // **************************** getAvailablePitching (for a roster) *****************************
    //

    AllPitchingStats: null,

    getAllPitchingStats: function( index, level, callback ) {
        if( index==0  || this.AllDBStats == null) {
            var context = this;
            var PitchDB = Pitching;

            PlayerDB = PlayerPitching;
            if (level != "ML") {
               FieldDB = FieldingMinors;
               PlayerDB = PlayerPitchingMinors;
           }

           PlayerDB.find({ MLBId: { $in: context.PlayerIds } }, function (err, results) {
               context.AllDBStats = results;
               results = null;
               callback(err, context.AllDBStats);
               
           })
        } else {
            callback(null,this.AllDBStats);
        }
    },

    getAvailablePitching: function (index, players, level, gameRange, callback) {
        var context = this;

        var gameRangeFrom = 20170401;
        var gameRangeTo = 20170501;
        if (gameRange) {
            gameRangeFrom = gameRange.From;
            gameRangeTo = gameRange.To;
        }
        var nextCalendarGameDay = moment(gameRangeTo).dayOfYear(); // this is the last day of stats to grab.. therefore...
        nextCalendarGameDay++;

        if (players && players[index] && index < players.length) {

            var skipgamelist = players[index].FullGames;
            if (!skipgamelist) {
                skipgamelist = [];
            }


            var getYTDTotals = true;

            if (getYTDTotals) {
                // get all games
                /*
                var query = {
                    $and:
                        [
                            {
                                "Stats":
                                    {
                                        $elemMatch:
                                            {
                                                "MLBId": players[index].MlbId
                                            }
                                    }
                            }
                        ]
                };
                */
                var query = {
                    "Stats": {
                        $elemMatch:
                        {
                            "MLBId": players[index].MlbId
                        }
                    }
                };

            } else {
                // GET ONLY ELIGIBLE GAMES..            
                var query = {
                    $and:
                        [
                            { "GameDate": { $nin: skipgamelist } },
                            {
                                "Stats":
                                {
                                    $elemMatch:
                                    {
                                        "MLBId": players[index].MlbId
                                    }
                                }
                            }
                        ]
                };
            }

            var projection = { "Stats.$": 1, "GameDate": 1 };


            var PitchDB = Pitching;
            if (level != "ML")
                PitchDB = PitchingMinors;
           // PitchDB.find(query, projection, function (err, stats) {
               /*
               PitchDB.aggregate([
                {
                    $project: {
                        GameDate: 1,
                      Stats: {
                         $filter: {
                            input: "$Stats",
                            as: "s",
                            cond: { $eq: [ "$$s.MLBId", players[index].MlbId ] }
                      }
                   }
                }
                }
             ], function(err, stats) {
                 */
                context.getAllPitchingStats(index, level, function(err, AllStats){
                    if (err || AllStats == null || AllStats.length == 0) {
                  
    
                    } else {

                        var stats = [];
                        var player = players[index];
                        /*
                        for (var i = 0; i < AllStats.length; i++) {
                            for (var s = 0; s < AllStats[i].Stats.length; s++) {
                                var id = AllStats[i].Stats[s].MLBId;
                                if (AllStats[i].Stats[s].MLBId == player.MlbId) {
                                    stats.push({ GameDate: AllStats[i].GameDate, Stats: AllStats[i].Stats[s] });
                                }
                            }
                        }
                        */
                        for (var i = 0; i < AllStats.length; i++) {
                            var id = AllStats[i].MLBId;
                            if (id == player.MlbId) {
                                for (var g = 0; g < AllStats[i].Games.length; g++) {
                                    stats.push({ GameDate: AllStats[i].Games[g].GameDate, Stats: AllStats[i].Games[g].Stats });
                                }
                            }
                        }


                delete players[index].Salary;

                if (stats && stats.length > 0) {
                    // has pitching stats... don't assume he can start:
                    players[index].CanStart = false;


                    if (level == "ML") {
                        players[index].OutsPerGame = new Array(27);
                        players[index].FirstFullPitchingGame = "";
                        players[index].CanRelieve = false;
                    } else {
                        players[index].OutsPerGameMinors = new Array(27);
                    }

                    var highestIP = 0;
                    var highestIPMinors = 0;

                    var YTDGames = { eStats: {} };

                    for (var i = 0; i < stats.length; i++) {

                        var gameDate = stats[i].GameDate;
                        var actualGameDate = gameDate;  // for compare below
                        if (level != "ML")
                            gameDate = gameDate + "Minors";

                        // for each day's available stats...
                    //    if (stats[i].Stats && stats[i].Stats.length > 0 && stats[i].Stats[0].G != 0 && stats[i].Stats[0].G != "") {
                    //        for( var s=0; s<stats[i].Stats.length; s++) {

                     //           var data = stats[i].Stats[s];
                     if (stats[i].Stats && stats[i].Stats.G != 0 && stats[i].Stats.G != "") {
                        {
                            var data = stats[i].Stats;

                                  if (data.G != "") {
                                delete data.TeamNbr;
                                delete data.Team;
                                delete data.PlayerId;
                                delete data.PlayerName;
                                delete data.MLBId;
                                delete data.FirstName;
                                delete data.LastName;

                                if (getYTDTotals) {
                                    if (YTDGames.eStats[gameDate]) {
                                        gameDate = gameDate + "-NC"; // for double header (though this game was first)
                                    }
                                    YTDGames.eStats[gameDate] = { Pitching: data };
                                }
                                var inrange = false;
                                if (actualGameDate >= gameRangeFrom && actualGameDate <= gameRangeTo) {
                                    inrange = true;
                                }
                                if (inrange && skipgamelist.indexOf(gameDate) == -1) {

                                    // not skipping this game
                                    if (!players[index].eStats) {
                                        players[index]['eStats'] = {};
                                    }
                                    if (!players[index].gamesList) {
                                        players[index].gamesList = [];
                                    }
                                    if (players[index].eStats[gameDate] && players[index].eStats[gameDate].Pitching) {
                                        gameDate = gameDate + "-NC"; // for double header (though this game was first)
                                    }
                                    if (!players[index].eStats[gameDate]) {
                                        players[index].eStats[gameDate] = {};
                                    }

                                    // put in level here...
                                    data["Level"] = level;

                                    var remainingIP = data.IP;
                                    var baseOuts = Math.floor(remainingIP * 3);
                                    var remainingOuts = baseOuts + (remainingIP % 1 == 0 ? 0 : remainingIP % 1 >= 0.2 ? 2 : 1);


                                    // see if there are partially used IP in this gamedate
                                    // example of partial games
                                    // PartialGames: {"20170405": {id:"1234", paTotal: 6, paUsed:3, outsTotal: 4, outsUsed:0} , "20170409": {id:"1234", paTotal: 9, paUsed:3, ipUsed:0} },
                                    if (players[index].PartialGames[gameDate]) {
                                        if (players[index].PartialGames[gameDate].outsUsed)
                                            remainingOuts = remainingOuts - players[index].PartialGames[gameDate].outsUsed;
                                    }

                                    data["OutsAvailable"] = remainingOuts;
                                    players[index].eStats[gameDate]["Pitching"] = data;
                                    if (players[index].gamesList.indexOf(gameDate) == - 1) {
                                        players[index].gamesList.push(gameDate);
                                    }


                                    if (level == "ML") {
                                        if (remainingOuts && remainingOuts > highestIP) {
                                            players[index]["highestIP"] = gameDate;
                                            highestPA = remainingOuts;
                                        }

                                        if (remainingOuts > 0) {
                                            if (!players[index].OutsPerGame[remainingOuts] || (Number(gameDate) > Number(players[index].OutsPerGame[remainingOuts]))) {
                                                players[index].OutsPerGame[remainingOuts] = gameDate;
                                            }
                                            if (data.GS >= 1) {
                                                // then could be first available full game...
                                                if (players[index].FirstFullPitchingGame == "" || (Number(gameDate) > Number(players[index].FirstFullPitchingGame))) {
                                                    // if they've pitched an inning, from this game, they cannot start
                                                    players[index].FirstFullPitchingGame = gameDate;
                                                }
                                                if (!players[index].NextAvilableGame || players[index].NextAvilableGame <= nextCalendarGameDay)
                                                    players[index].CanStart = true;
                                            } else {
                                                if (!players[index].NextAvilableGame || players[index].NextAvilableGame <= nextCalendarGameDay)
                                                    players[index].CanRelieve = true;
                                            }
                                        }
                                    } else {
                                        // same for minors
                                        if (remainingOuts && remainingOuts > highestIPMinors) {
                                            if (!players[index].Primary)
                                                players[index]["Primary"] = [];
                                            if (players[index].Primary.indexOf("P") == -1)
                                                players[index].Primary.push("P");
                                            players[index]["highestIPMinors"] = gameDate;
                                            highestPAMinors = remainingOuts;
                                        }

                                        /*
                                        if (remainingOuts > 0) {
                                            if (data.GS >= 1) {
                                                // then could be first available full game...
                                                if (players[index].FirstFullPitchingGame == "" || (Number(gameDate) > Number(players[index].FirstFullPitchingGame))) {
                                                    // if they've pitched an inning, from this game, they cannot start
                                                    players[index].FirstFullPitchingGame = gameDate;
                                                }
                                                players[index].CanStart = true;
                                            } else {
                                                players[index].CanRelieve = true;
                                            }
                                        }
                                        */
                                    }

                                }
                            }
                        }
                      }

                    }
                    // total up all the unused game stats here
                    context.totalUnusedPitching(players[index], level);

                    // look through the pitcher's batting and see if he batted when he didn't pitch
                    if (players[index].eStats && players[index].eStats.YTDTotalBatting && YTDGames.eStats && Object.keys(YTDGames.eStats).length > 0) {
                        // then he's batted... if he's batted more than he's pitched.. put him in at DH
                        if (players[index].eStats.YTDTotalBatting.G > Object.keys(YTDGames.eStats).length) {
                            players[index].Tertiary.push("DH");
                        }
                    }

                    if (getYTDTotals) {
                        // total up the entire year of mlb game stats.
                        context.totalUnusedPitching(YTDGames, level);
                    }
                    if (getYTDTotals) {
                        if (players[index].eStats)
                            if (level == "ML")
                                players[index].eStats["YTDTotalPitching"] = teamHelper.calculateYTDStats(YTDGames.eStats.TotalPitching, true);
                            else
                                players[index].eStats["YTDTotalPitchingMinors"] = teamHelper.calculateYTDStats(YTDGames.eStats.TotalPitchingMinors, true);

                    }
                } else {
                    //put in empty stats...
                }
            }
                stats = null;
                AllStats =  null;

                context.getAvailablePitching(++index, players, level, gameRange, callback);
                // if there is an incremental value, update the progress
                if (gameRange.increment) {
                    utils.updateProgress(null, gameRange.leagueId, gameRange.teamId, gameRange.teamName, gameRange.increment, null);
                }
            });
        } else {
            AllPitchingStats = null;
            callback(players);
        }
    },

    totalUnusedPitching: function (player, level) {
        // players[index].eStats[gameDate] = {Batting: data};

        var TOTALFIELD = "TotalPitching";
        if (level != "ML") {
            TOTALFIELD = "TotalPitchingMinors";
        }
        if (player.eStats) {

            if (!player.eStats[TOTALFIELD]) {
                player.eStats[TOTALFIELD] = {};
            }
            for (gameDate in player.eStats) {
                if (gameDate.charAt(0) == "2" && player.eStats[gameDate].Pitching) {
                    var nextStats = player.eStats[gameDate].Pitching;
                    for (stat in nextStats) {
                        // ok, special values taken care of.
                        //
                        if (this.skipStatsToAdd.indexOf(stat) == -1) {
                            // don't skip
                            value = nextStats[stat];
                            if (!player.eStats[TOTALFIELD][stat]) {
                                player.eStats[TOTALFIELD][stat] = 0;
                            }
                            if (value == null || value === false || isNaN(value))
                                value = 0;
                            player.eStats[TOTALFIELD][stat] = Number(player.eStats[TOTALFIELD][stat]) + Number(value);
                        }
                    }

                }
            }
            // calculate averages.
            teamHelper.calculateYTDStats(player.eStats[TOTALFIELD], true);
        }
    },

    //
    // **************************** getAvailableFielding (for a roster) *****************************
    //
    getAvailableFielding: function (index, players, level, gameRange, callback) {
        var context = this;
        var gameRangeFrom = 20170401;
        var gameRangeTo = 20170501;
        if (gameRange) {
            gameRangeFrom = gameRange.From;
            gameRangeTo = gameRange.To;
        }

        if (players[index] && index < players.length) {

            var skipgamelist = players[index].FullGames;
            if (!skipgamelist) {
                skipgamelist = [];
            }


            var getYTDTotals = true;

            if (getYTDTotals) {
                // get all games
                var query = {
                    $and:
                        [
                            {
                                "Stats":
                                {
                                    $elemMatch:
                                    {
                                        "MLBId": players[index].MlbId
                                    }
                                }
                            }
                        ]
                };

            } else {
                // GET ONLY ELIGIBLE GAMES..


                var query = {
                    $and:
                        [
                            { "GameDate": { $nin: skipgamelist } },
                            {
                                "Stats":
                                {
                                    $elemMatch:
                                    {
                                        "MLBId": players[index].MlbId
                                    }
                                }
                            }
                        ]
                };
            }
            var projection = { "Stats.$": 1, "GameDate": 1 };

            var FieldDB = Fielding;
            if (level != "ML")
                FieldDB = FieldingMinors;

          //  FieldDB.find(query, projection, function (err, stats) {
              /*
            FieldDB.aggregate([
                {
                    $project: {
                        GameDate: 1,
                      Stats: {
                         $filter: {
                            input: "$Stats",
                            as: "s",
                            cond: { $eq: [ "$$s.MLBId", players[index].MlbId ] }
                      }
                   }
                }
                }
             ], function(err, stats) {
                 */
                context.getAllFieldingStats(index, level, function(err, AllStats){
                    if (err || AllStats == null || AllStats.length == 0) {
                  
    
                    } else {


                        var stats = [];
                        var player = players[index];

                        /*
                        for (var i = 0; i < AllStats.length; i++) {
                            if (AllStats[i].Stats) {
                                for (var s = 0; s < AllStats[i].Stats.length; s++) {
                                    var id = AllStats[i].Stats[s].MLBId;
                                    if (AllStats[i].Stats[s].MLBId == player.MlbId) {
                                        stats.push({ GameDate: AllStats[i].GameDate, Stats: AllStats[i].Stats[s] });
                                    }
                                }

                            } else {
                                var nostats = i;
                            }
                        }
                        */
                       for (var i = 0; i < AllStats.length; i++) {
                        if (AllStats[i].Games) {
                            var id = AllStats[i].MLBId;
                            if (id == player.MlbId) {
                                for (var g = 0; g < AllStats[i].Games.length; g++) {
                                    if( AllStats[i].Games[g].Stats )
                                        stats.push({ GameDate: AllStats[i].Games[g].GameDate, Stats: AllStats[i].Games[g].Stats });
                                    else
                                        nostats = 1;
                                }
                            }

                        } else {
                            var nostats = i;
                        }
                    }


                delete players[index].Salary;
                if (stats && stats.length > 0) {

                    var YTDGames = { eStats: {} };

                    for (var i = 0; i < stats.length; i++) {

                        var gameDate = stats[i].GameDate;

                        // for each day's available stats...
                     //   if (stats[i].Stats && stats[i].Stats.length > 0 && stats[i].Stats[0].G != 0 && stats[i].Stats[0].G != "") {
                      //      for( var s=0; s<stats[i].Stats.length; s++) {

                     //           var data = stats[i].Stats[s];
                     if (stats[i].Stats && stats[i].Stats.G != 0 && stats[i].Stats.G != "") {
                        {
                            var data = stats[i].Stats;
      
                            delete data.TeamNbr;
                            delete data.Team;
                            delete data.PlayerId;
                            delete data.PlayerName;
                            delete data.MLBId;
                            delete data.FirstName;
                            delete data.LastName;

                            // put in level here...
                            data["Level"] = level;

                            var gameDate = stats[i].GameDate;
                            var actualGameDate = gameDate;  // for compare below
                            var isDoubleHeaderNC = false;
                            var sameDayData = null;
                            if (level != "ML")
                                gameDate = gameDate + "Minors";

                            if (getYTDTotals) {
                                if (YTDGames.eStats[gameDate]) {
                                    // normally, this would indicate a double header game, but it just might
                                    // indicate multiple positions in one game!
                                    if( YTDGames.eStats[gameDate].Fielding) {
                                        var gameId = YTDGames.eStats[gameDate].Fielding.GameId;
                                        if( data.GameId == gameId) {
                                            // then same game, and have to combine stats, not create 2nd game
                                            sameDayData = YTDGames.eStats[gameDate].Fielding;
                                        } else {
                                            // not the same gameId, so it's a 2nd game
                                            gameDate = gameDate + "-NC"; // for double header (though this game was first)
                                            // now, see if he's already GOT a 2nd game
                                            if( YTDGames.eStats[gameDate] 
                                               && YTDGames.eStats[gameDate].Fielding
                                                &&  YTDGames.eStats[gameDate].Fielding.GameId == gameId) {
                                                    sameDayData = YTDGames.eStats[gameDate].Fielding;
                                            }
 
                                            isDoubleHeaderNC = true;
                                        }
                                    } else {
                                        // didn't field in the gamedate, so must be a 2nd game
                                        gameDate = gameDate + "-NC"; // for double header (though this game was first)
                                        isDoubleHeaderNC = true;
                                    }
                                } 

                                if( sameDayData ) {

                                    var skipStat = ["G","GameDate", "GameId", "GameString","Level","PrevGameDaysPlayed", "FPct", "FPctC","FPctP"]
                                    for (var stat in sameDayData ) {

                                        // if a 2nd position, add it in here... 
                                        if( stat=="Pos") {
                                            data["Pos2"] = sameDayData.Pos;
                                        } else if( skipStat.indexOf( stat ) == -1 ) {
                                            // not position, and not skipped, then can add this stat
                                            if( isNaN(sameDayData[stat]) ) {
                                                sameDayData[stat] = 0;
                                            }
                                            if( !data[stat] || isNaN(data[stat])) {
                                                data[stat] = 0;
                                            }
                                            data[stat] = Number(data[stat]) + Number( sameDayData[stat]);
                                            var sdd = sameDayData[stat];
                                        }
                                    }
                                }
                                YTDGames.eStats[gameDate] = { Fielding: data };
                            }

                            var inrange = false;
                            if (actualGameDate >= gameRangeFrom && actualGameDate <= gameRangeTo) {
                                inrange = true;
                            }
                            if (inrange && skipgamelist.indexOf(gameDate) == -1) {


                                if (!players[index].eStats) {
                                    players[index]['eStats'] = {};
                                }
                                if (!players[index].gamesList) {
                                    players[index].gamesList = [];
                                }
                                if (players[index].eStats[gameDate] && players[index].eStats[gameDate].Fielding) {
                                    if ( isDoubleHeaderNC)
                                        gameDate = gameDate + "-NC"; // for double header (though this game was first)
                                }
                                if (!players[index].eStats[gameDate]) {
                                    players[index].eStats[gameDate] = {};
                                }
                                players[index].eStats[gameDate]["Fielding"] = data;
                                if (players[index].gamesList.indexOf(gameDate) == - 1) {
                                    players[index].gamesList.push(gameDate);
                                }
                            }
                        }
                      }
                    }
                    // total up all the unused game stats here
                    context.totalUnusedFielding(players[index], level);

                    if (getYTDTotals) {
                        // total up the entire year of mlb game stats.
                        context.totalUnusedFielding(YTDGames, level);
                    }


                    if (getYTDTotals) {
                        if (players[index].eStats) {
                            // a bit weird, but the only place to do this...
                            // first, calculate the player's speed based on their batting and fielding data
                            // then put it in their batting data
                            teamHelper.calculateYTDSpeed(players[index].eStats.YTDTotalBatting,
                                YTDGames.eStats.TotalFielding,
                                players[index].Primary, players[index].Secondary, players[index].Tertiary
                            );

                            if (level == "ML") {
                                players[index].eStats["YTDTotalFielding"] = teamHelper.calculateYTDStats(YTDGames.eStats.TotalFielding, false);
                            }
                            else {
                                players[index].eStats["YTDTotalFieldingMinors"] = teamHelper.calculateYTDStats(YTDGames.eStats.TotalFieldingMinors, false);

                            }

                        }


                    }

                } else {
                    //put in empty stats...
                }
            }
                stats = null;
                AllStats = null;
                context.getAvailableFielding(++index, players, level, gameRange, callback);

                // if there is an incremental value, update the progress
                if (gameRange.increment) {
                    utils.updateProgress(null, gameRange.leagueId, gameRange.teamId, gameRange.teamName, gameRange.increment, null);
                }
            });
        }
        else {
            callback(players);
        }
    },

    totalUnusedFielding: function (player, level) {
        // players[index].eStats[gameDate] = {Fielding: data};

        if (player.eStats) {

            var TOTALFIELD = "TotalFielding";
            if (level != "ML") {
                TOTALFIELD = "TotalFieldingMinors";
            }

            if (!player.eStats[TOTALFIELD]) {
                player.eStats[TOTALFIELD] = {};
            }
            for (gameDate in player.eStats) {
                if (gameDate.charAt(0) == "2" && player.eStats[gameDate].Fielding) {
                    var nextStats = player.eStats[gameDate].Fielding;
                    for (stat in nextStats) {
                        // ok, special values taken care of.
                        //
                        if (this.skipStatsToAdd.indexOf(stat) == -1) {
                            // don't skip
                            value = nextStats[stat];
                            if (!player.eStats[TOTALFIELD][stat]) {
                                player.eStats[TOTALFIELD][stat] = 0;
                            }
                            if (value == null || value === false || isNaN(value))
                                value = 0;
                            player.eStats[TOTALFIELD][stat] = Number(player.eStats[TOTALFIELD][stat]) + Number(value);
                        } else if (stat == "Pos") {
                            // put their most recent position in here.
                            player.eStats[TOTALFIELD][stat] = value;
                        }
                    }

                }
            }
            // calculate averages.
        }
    },

    //
    // **************************** getAvailableBaserunning (for a roster) *****************************
    //
    AllBRStats: null,

    getAllBRStats: function( index, level, callback ) {
        if( index==0  || this.AllDBStats == null) {
            var context = this;

            var BRDB = Baserunning;
            PlayerDB = PlayerBaserunning;
            if (level != "ML") {
               FieldDB = FieldingMinors;
               PlayerDB = PlayerBaserunningMinors;
           }

           PlayerDB.find({ MLBId: { $in: context.PlayerIds } }, function (err, results) {
               context.AllDBStats = results;
               results = null;
               callback(err, context.AllDBStats);
               
           })
           /*
            BRDB.find({  }, { "Stats": 1, "GameDate": 1 }, function (err, AllStats) {
                context.AllDBStats = AllStats;
                callback(err,AllStats);
            })
            */
        } else {
            callback(null,this.AllDBStats);
        }
    },

    getAvailableBaserunning: function (index, players, level, gameRange, callback) {
        var context = this;

        var gameRangeFrom = 20170401;
        var gameRangeTo = 20170501;
        if (gameRange) {
            gameRangeFrom = gameRange.From;
            gameRangeTo = gameRange.To;
        }
        if (players[index] && index < players.length) {

            var skipgamelist = players[index].FullGames;
            if (!skipgamelist) {
                skipgamelist = [];
            }


            var getYTDTotals = true;

            if (getYTDTotals) {
                // get all games
                var query = {
                    $and:
                        [
                            {
                                "Stats":
                                {
                                    $elemMatch:
                                    {
                                        "MLBId": players[index].MlbId
                                    }
                                }
                            }
                        ]
                };

            } else {
                // GET ONLY ELIGIBLE GAMES..
                var query = {
                    $and:
                        [
                            { "GameDate": { $nin: skipgamelist } },
                            {
                                "Stats":
                                {
                                    $elemMatch:
                                    {
                                        "MLBId": players[index].MlbId
                                    }
                                }
                            }
                        ]
                };
            }
            var projection = { "Stats.$": 1, "GameDate": 1 };

            var BRDB = Baserunning;
            if (level != "ML")
                BRDB = BaserunningMinors;

           // BRDB.find(query, projection, function (err, stats) {
               /*
               BRDB.aggregate([
                {
                    $project: {
                        GameDate: 1,
                      Stats: {
                         $filter: {
                            input: "$Stats",
                            as: "s",
                            cond: { $eq: [ "$$s.MLBId", players[index].MlbId ] }
                      }
                   }
                }
                }
             ], function(err, stats) {
                 */
                context.getAllBRStats(index, level, function(err, AllStats){
                    if (err || AllStats == null || AllStats.length == 0) {
                  
    
                    } else {


                        var stats = [];
                        var player = players[index];
                        /*
                        for (var i = 0; i < AllStats.length; i++) {
                            for (var s = 0; s < AllStats[i].Stats.length; s++) {
                                var id = AllStats[i].Stats[s].MLBId;
                                if (AllStats[i].Stats[s].MLBId == player.MlbId) {
                                    stats.push({ GameDate: AllStats[i].GameDate, Stats: AllStats[i].Stats[s] });
                                }
                            }
                        }
                        */
                        for (var i = 0; i < AllStats.length; i++) {
                            var id = AllStats[i].MLBId;
                            if (id == player.MlbId) {
                                for (var g = 0; g < AllStats[i].Games.length; g++) {
                                    stats.push({ GameDate: AllStats[i].Games[g].GameDate, Stats: AllStats[i].Games[g].Stats });
                                }
                            }
                        }

                delete players[index].Salary;
                if (stats && stats.length > 0) {

                    var YTDGames = { eStats: {} };

                for (var i = 0; i < stats.length; i++) {

                        var gameDate = stats[i].GameDate;
                        var actualGameDate = gameDate;  // for compare below
                        if (level != "ML")
                            gameDate = gameDate + "Minors";

                        // for each day's available stats...
                     //   if (stats[i].Stats && stats[i].Stats.length > 0 && stats[i].Stats[0].G != 0 && stats[i].Stats[0].G != "") {
                    //        for( var s=0; s<stats[i].Stats.length; s++) {

                     //           var data = stats[i].Stats[s];
                     if (stats[i].Stats && stats[i].Stats.G != 0 && stats[i].Stats.G != "") {
                        {
                            var data = stats[i].Stats;
      
                            delete data.TeamNbr;
                            delete data.Team;
                            delete data.PlayerId;
                            delete data.PlayerName;
                            delete data.MLBId;
                            delete data.FirstName;
                            delete data.LastName;

                            // put in level here...
                            data["Level"] = level;

                            if (getYTDTotals) {
                                if (YTDGames.eStats[gameDate]) {
                                    gameDate = gameDate + "-NC"; // for double header (though this game was first)
                                }
                                YTDGames.eStats[gameDate] = { Baserunning: data };
                            }
                            var inrange = false;
                            if (actualGameDate >= gameRangeFrom && actualGameDate <= gameRangeTo) {
                                inrange = true;
                            }
                            if (inrange && skipgamelist.indexOf(gameDate) == -1) {

                                if (!players[index].eStats) {
                                    players[index]['eStats'] = {};
                                }
                                if (!players[index].gamesList) {
                                    players[index].gamesList = [];
                                }
                                if (players[index].eStats[gameDate] && players[index].eStats[gameDate].Baserunning) {
                                    gameDate = gameDate + "-NC"; // for double header (though this game was first)
                                }
                                if (!players[index].eStats[gameDate]) {
                                    players[index].eStats[gameDate] = {};
                                }
                                players[index].eStats[gameDate]["Baserunning"] = data;
                                if (players[index].gamesList.indexOf(gameDate) == - 1) {
                                    players[index].gamesList.push(gameDate);
                                }
                            }
                        }
                      }
                    }
                    context.totalUnusedBaserunning(players[index], level);

                    if (getYTDTotals) {
                        // total up the entire year of mlb game stats.
                        context.totalUnusedBaserunning(YTDGames, level);
                        if (players[index].eStats && YTDGames && YTDGames.eStats.TotalBaserunning) {
                            if (level == "ML") {
                                players[index].eStats["YTDTotalBaserunning"] = teamHelper.calculateYTDStats(YTDGames.eStats.YTDTotalBaserunning, false);
                            }
                            else {
                                players[index].eStats["YTDTotalBaserunningMinors"] = teamHelper.calculateYTDStats(YTDGames.eStats.YTDTotalBaserunningMinors, false);

                            }
                        }
                    }
                } else {
                    //put in empty stats...
                }
            }   
                stats = null;
                AllStats = null;

                context.getAvailableBaserunning(++index, players, level, gameRange, callback);

                // if there is an incremental value, update the progress
                if (gameRange.increment) {
                    utils.updateProgress(null, gameRange.leagueId, gameRange.teamId, gameRange.teamName, gameRange.increment, null);
                }
            });
        }
        else {
            this.AllBRStats = null;
            callback(players);
        }
    },

    totalUnusedBaserunning: function (player, level) {
        // players[index].eStats[gameDate] = {Baserunning: data};

        if (player.eStats) {

            var TOTALFIELD = "TotalBatting";
            if (level != "ML") {
                TOTALFIELD = "TotalBattingMinors";
            }


            if (!player.eStats[TOTALFIELD]) {
                player.eStats[TOTALFIELD] = {};
            }
            for (gameDate in player.eStats) {
                if (gameDate.charAt(0) == "2" && player.eStats[gameDate].Baserunning) {
                    var nextStats = player.eStats[gameDate].Baserunning;
                    for (stat in nextStats) {
                        // ok, special values taken care of.
                        //
                        if (this.skipStatsToAdd.indexOf(stat) == -1) {
                            // don't skip
                            value = nextStats[stat];
                            if (!player.eStats[TOTALFIELD][stat]) {
                                player.eStats[TOTALFIELD][stat] = 0;
                            }
                            if (value == null || value === false || isNaN(value))
                                value = 0;
                            player.eStats[TOTALFIELD][stat] = Number(player.eStats[TOTALFIELD][stat]) + Number(value);
                        }
                    }

                }
            }
            // calculate averages.
        }
    },


    mapObject: function (firstObject, secondObject) {
        for (var k in firstObject) secondObject[k] = firstObject[k];
        return secondObject;
    },
/*
    mergePlayers: function (index, players, changes, callback) {
        var context = this;
        if (players[index] && index < players.length) {
            var query = {
                MlbId: players[index].mlb_id
            }
            Player.findOne(query, function (err, player) {
                if (err) {
                    context.mergePlayers(++index, players, changes, callback);
                }
                if (player) {
                    var isUpdate = false;
                    if (players[index].mlb_team == "WSH") {
                        player.MlbTeam = "WAS";
                        isUpdate = true;
                    }
                    else if (players[index].mlb_team == "CWS") {
                        player.MlbTeam = "CHW";
                        isUpdate = true;
                    }
                    else if (player.MlbTeam != players[index].mlb_team) {
                        player.MlbTeam = players[index].mlb_team;
                        isUpdate = true;
                    }
                    if (player.Position != players[index].mlb_pos) {
                        player.Position = players[index].mlb_pos;
                        isUpdate = true;
                    }
                    if (player.EspnId != players[index].espn_id) {
                        player.EspnId = players[index].espn_id;
                        isUpdate = true;
                    }
                    if (player.MlbDepth != players[index].mlb_depth) {
                        player.MlbDepth = players[index].mlb_depth;
                        isUpdate = true;
                    }
                    if (player.CbsId != players[index].cbs_id) {
                        player.CbsId = players[index].cbs_id;
                        isUpdate = true;
                    }
                    if (player.FullName != players[index].mlb_name) {
                        player.FullName = players[index].mlb_name;
                        isUpdate = true;
                    }

                    if (isUpdate) {
                        player.save(function (err, result) {
                            if (err) {
                                console.log(err);
                            }
                            else {
                                changes.push(player);
                            }
                        });
                    }

                    context.mergePlayers(++index, players, changes, callback);
                }
                else {
                    var player = {
                        MlbTeam: players[index].mlb_team,
                        Position: players[index].mlb_pos,
                        EspnId: players[index].espn_id,
                        MlbDepth: players[index].mlb_depth,
                        CbsId: players[index].cbs_id,
                        FullName: players[index].mlb_name,
                        Bats: players[index].bats,
                        Throws: players[index].throws,
                        DOB: players[index].birth_year,
                        YearSigned: players[index].debut,
                        Level: "ML",
                        MlbId: players[index].mlb_id,
                        Image: "http://gdx.mlb.com/images/gameday/mugshots/mlb/" + players[index].mlb_id + "@2x.jpg",
                        Salary: {}
                    };
                    Player.create(player, function (err, result) {
                        changes.push(player);
                        context.mergePlayers(++index, players, changes, callback);
                    });
                }
            });
        }
        else {
            callback(changes);
        }
    },

    getYtdBaserunning: function (startDate, endDate, currentDate, index, players, callback) {
        var context = this;
        if (players[index] && index < players.length) {
            if (!players[index].Baserunning) {
                players[index].Baserunning = [];
            }

            if (moment(currentDate) < moment(endDate)) {

                var query = { $and: [{ "GameDate": currentDate }, { "Stats": { $elemMatch: { "MLBId": players[index].MlbId } } }] };
                var projection = { "Stats.$": 1, "GameDate": 1 };

                Baserunning.findOne(query, projection, function (err, stats) {
                    if (stats && stats.Stats[0].GameId.length > 0) {
                        var data = stats.Stats[0];
                        delete data.TeamNbr;
                        delete data.Team;
                        delete data.PlayerId;
                        delete data.PlayerName;
                        delete data.MLBId;
                        delete data.FirstName;
                        delete data.LastName;
                        //delete data.GameDate;
                        delete data.GameString;
                        //delete data.GameId;

                        data.SecondToHome1B = parseInt(data.SecondToHome1B) || 0;
                        data.FirstToHome2B = parseInt(data.FirstToHome2B) || 0;
                        data.FirstToHome1B = parseInt(data.FirstToHome1B) || 0;
                        data.FirstToThird1B = parseInt(data.FirstToThird1B) || 0;
                        data.AdvancedOnFlyBall = parseInt(data.AdvancedOnFlyBall) || 0;

                        data.BasesTaken = parseInt(data.BasesTaken) || 0;
                        data.AdvOnWpPbOut = parseInt(data.AdvOnWpPbOut) || 0;
                        data.DoubledOffLD = parseInt(data.DoubledOffLD) || 0;
                        data.AdvOnFlyBallOut = parseInt(data.AdvOnFlyBallOut) || 0;
                        data.BatterAdvOnHitOut = parseInt(data.BatterAdvOnHitOut) || 0;

                        data.OutsOnBase = parseInt(data.OutsOnBase) || 0;
                        data.Pickoffs = parseInt(data.Pickoffs) || 0;
                        data.CaughtStealingHP = parseInt(data.CaughtStealingHP) || 0;
                        data.CaughtStealing3B = parseInt(data.CaughtStealing3B) || 0;
                        data.CaughtStealing2B = parseInt(data.CaughtStealing2B) || 0;

                        data.StolenBasesHP = parseInt(data.StolenBasesHP) || 0;
                        data.StolenBases3B = parseInt(data.StolenBases3B) || 0;
                        data.StolenBases2B = parseInt(data.StolenBases2B) || 0;
                        data.G = parseInt(data.G) || 0;
                        data.PrevDayGamesPlayed = parseInt(data.PrevDayGamesPlayed) || 0;

                        players[index].Baserunning.push(data);
                    }
                    context.getYtdBaserunning(startDate, endDate, moment(currentDate).add(1, 'day').format("YYYYMMDD"), index, players, callback);
                });
            }
            else {
                context.getYtdBaserunning(startDate, endDate, startDate, ++index, players, callback);
            }
        }
        else {
            for (var i = 0; i < players.length; i++) {
                var ytdStats = {};
                for (var key in players[i].Baserunning) {
                    for (var prop in players[i].Baserunning[key]) {
                        if (!ytdStats[prop]) {
                            ytdStats[prop] = 0;
                        }
                        ytdStats[prop] += parseInt(players[i].Baserunning[key][prop]);
                    }
                }

                var daily = players[i].Baserunning;
                players[i].Baserunning = {};
                if (players[i].Baserunning) {
                    players[i].Baserunning.ytd = ytdStats;
                    players[i].Baserunning.daily = daily;
                }
            }
            callback(players);
        }
    },

    getYtdFieldingAdditional: function (date, index, players, callback) {
        var context = this;
        if (players[index] && index < players.length) {

            YtdFielding.findOne({ $and: [{ GameDate: date }, { MLBId: players[index].MLBId }] }, function (err, stats) {
                if (stats) {
                    players[index].YtdFielding = stats;
                }
                else {
                    players[index].YtdFielding = {};
                }
                context.getYtdFieldingAdditional(date, ++index, players, callback);
            });
        }
        else {
            callback(players);
        }
    },

    getYtdFielding: function (startDate, endDate, currentDate, index, players, callback) {
        var context = this;
        if (players[index] && index < players.length) {
            if (players[index]._doc) {
                players[index] = players[index]._doc;
            }
            if (!players[index].Fielding) {
                players[index].Fielding = [];
            }

            if (moment(currentDate) < moment(endDate)) {

                var query = { $and: [{ "GameDate": currentDate }, { "Stats": { $elemMatch: { "MLBId": players[index].MlbId } } }] };
                var projection = { "Stats.$": 1, "GameDate": 1 };

                Fielding.findOne(query, projection, function (err, stats) {
                    if (stats && stats.Stats[0].GameId.length > 0) {
                        var data = stats.Stats[0];
                        delete data.TeamNbr;
                        delete data.Team;
                        delete data.PlayerId;
                        delete data.PlayerName;
                        delete data.MLBId;
                        delete data.FirstName;
                        delete data.LastName;
                        //delete data.GameDate;
                        delete data.GameString;
                        //delete data.GameId;

                        data.Ethrow = parseInt(data.Ethrow) || 0;
                        data.Efield = parseInt(data.Efield) || 0;
                        data.Cera = parseInt(data.Cera) || 0;
                        data.Cer = parseInt(data.Cer) || 0;
                        data.Cint = parseInt(data.Cint) || 0;

                        data.Pivots2B = parseInt(data.Pivots2B) || 0;
                        data.FPct = parseInt(data.FPct) || 0;
                        data.Zone = parseFloat(data.Zone) || 0;
                        data.Chances = parseInt(data.Chances) || 0;
                        data.Outs = parseInt(data.Outs) || 0;

                        data.FPtcC = parseFloat(data.FPtcC) || 0;
                        data.PKOF = parseInt(data.PKOF) || 0;
                        data.CS = parseInt(data.CS) || 0;
                        data.SB = parseInt(data.SB) || 0;
                        data.FPctP = parseInt(data.FPctP) || 0;

                        data.PB = parseInt(data.PB) || 0;
                        data.TP = parseInt(data.TP) || 0;
                        data.DP = parseInt(data.DP) || 0;
                        data.E = parseInt(data.E) || 0;
                        data.A = parseInt(data.A) || 0;

                        data.PO = parseInt(data.PO) || 0;
                        data.INN = parseFloat(data.INN) || 0;
                        data.GS = parseInt(data.GS) || 0;
                        data.G = parseInt(data.G) || 0;

                        context.getYtdFieldingAdditional(currentDate, 0, [{ MLBId: players[index].MlbId }], function (ytdFielding) {
                            ytdFielding = ytdFielding[0].YtdFielding;

                            data.Pos = ytdFielding.Pos;
                            data.G = ytdFielding.G;
                            data.GS = ytdFielding.GS;
                            data.INN = ytdFielding.INN;
                            data.BallsInZone = ytdFielding.BallsInZone;
                            data.MissedBallsInZone = ytdFielding.MissedBallsInZone;
                            data.OutsOutOfZone = ytdFielding.OutsOutOfZone;
                            data.CBlockingRuns = ytdFielding.CBlockingRuns;
                            data.CFramingRuns = ytdFielding.CFramingRuns;
                            data.Cera = ytdFielding.Cera;

                            if (players[index]._doc) {
                                players[index]._doc.Fielding.push(data);
                            }
                            else {
                                players[index].Fielding.push(data);
                            }
                            context.getYtdFielding(startDate, endDate, moment(currentDate).add(1, 'day').format("YYYYMMDD"), index, players, callback);
                        });
                    }
                    else {
                        context.getYtdFielding(startDate, endDate, moment(currentDate).add(1, 'day').format("YYYYMMDD"), index, players, callback);
                    }
                });
            }
            else {
                context.getYtdFielding(startDate, endDate, startDate, ++index, players, callback);
            }
        }
        else {
            for (var i = 0; i < players.length; i++) {
                var ytdStats = {};
                for (var key in players[i].Fielding) {
                    for (var prop in players[i].Fielding[key]) {
                        if (!ytdStats[prop]) {
                            ytdStats[prop] = 0;
                        }
                        if (prop != "Zone") {
                            ytdStats[prop] += parseInt(players[i].Fielding[key][prop]);
                        }
                    }
                }

                //  (put outs + assists) / (put outs + assists + errors).
                ytdStats.FPct = (ytdStats.PO + ytdStats.A) / (ytdStats.PO + ytdStats.A + ytdStats.E)

                var daily = players[i].Fielding;
                players[i].Fielding = {};
                if (players[i].Fielding) {
                    players[i].Fielding.ytd = ytdStats;
                    players[i].Fielding.daily = daily;
                }
            }
            callback(players);
        }
    },

    getYtdPitching: function (startDate, endDate, currentDate, index, players, callback) {
        var context = this;
        if (players[index] && index < players.length) {
            if (players[index]._doc) {
                players[index] = players[index]._doc;
            }
            if (!players[index].Pitching) {
                players[index].Pitching = [];
            }

            if (moment(currentDate) < moment(endDate)) {

                var query = { $and: [{ "GameDate": currentDate }, { "Stats": { $elemMatch: { "MLBId": players[index].MlbId } } }] };
                var projection = { "Stats.$": 1, "GameDate": 1 };

                Pitching.findOne(query, projection, function (err, stats) {
                    if (stats && stats.Stats[0].GameId.length > 0) {
                        var data = stats.Stats[0];
                        delete data.TeamNbr;
                        delete data.Team;
                        delete data.PlayerId;
                        delete data.PlayerName;
                        delete data.MLBId;
                        delete data.FirstName;
                        delete data.LastName;
                        //delete data.GameDate;
                        delete data.GameString;
                        //delete data.GameId;

                        data.TrP = parseInt(data.TrP) || 0;
                        data["DP_noGIDP"] = parseInt(data["DP_noGIDP"]) || 0;
                        data.GIDP = parseFloat(data.GIDP) || 0;
                        data["K_L"] = parseInt(data["K_L"]) || 0;
                        data["K_S"] = parseInt(data["K_S"]) || 0;

                        data.SVO = parseInt(data.SVO) || 0;
                        //data.ERA = parseInt(data.ERA) || 0;
                        data.RelIP = parseFloat(data.RelIP) || 0;
                        data.StIP = parseFloat(data.StIP) || 0;
                        data.PKO = parseInt(data.PKO) || 0;

                        data.CS = parseInt(data.CS) || 0;
                        data.SB = parseInt(data.SB) || 0;
                        data.POPU = parseInt(data.POPU) || 0;
                        data.LD = parseInt(data.LD) || 0;
                        data["GB/FB"] = parseInt(data["GB/FB"]) || 0;

                        data.FB = parseInt(data.FB) || 0;
                        data.GB = parseInt(data.GB) || 0;
                        data.IRS = parseInt(data.IRS) || 0;
                        data.IR = parseInt(data.IR) || 0;
                        data.PerfectG = parseInt(data.PerfectG) || 0;

                        data.NoH = parseInt(data.NoH) || 0;
                        data.BLK = parseInt(data.BLK) || 0;
                        data.WP = parseFloat(data.WP) || 0;
                        data.K = parseInt(data.K) || 0;
                        data["BB_noIBB"] = parseInt(data["BB_noIBB"]) || 0;

                        data.IBB = parseInt(data.IBB) || 0;
                        data.BB = parseInt(data.BB) || 0;
                        data.HBP = parseInt(data.HBP) || 0;
                        data.SF = parseInt(data.SF) || 0;
                        data.SH = parseInt(data.SH) || 0;

                        data.ER = parseInt(data.ER) || 0;
                        data.R = parseInt(data.R) || 0;
                        data.HR = parseInt(data.HR) || 0;
                        data["3B"] = parseInt(data["3B"]) || 0;
                        data["2B"] = parseInt(data["2B"]) || 0;
                        data["1B"] = parseInt(data["1B"]) || 0;

                        data.H = parseInt(data.H) || 0;
                        data.BFP = parseInt(data.BFP) || 0;
                        data.IP = parseFloat(data.IP) || 0;
                        data.SHO = parseInt(data.SHO) || 0;
                        data.QS = parseInt(data.Qs) || 0;

                        data.GF = parseInt(data.GF) || 0;
                        data.CG = parseInt(data.CG) || 0;
                        data.GS = parseInt(data.GS) || 0;
                        data.G = parseInt(data.G) || 0;
                        data.HLD = parseInt(data.HLD) || 0;

                        data.BS = parseInt(data.BS) || 0;
                        data.Sv = parseInt(data.Sv) || 0;
                        data.L = parseInt(data.L) || 0;
                        data.W = parseInt(data.W) || 0;


                        if (players[index]._doc) {
                            players[index]._doc.Pitching.push(data);
                        }
                        else {
                            players[index].Pitching.push(data);
                        }
                    }
                    context.getYtdPitching(startDate, endDate, moment(currentDate).add(1, 'day').format("YYYYMMDD"), index, players, callback);
                });
            }
            else {
                context.getYtdPitching(startDate, endDate, startDate, ++index, players, callback);
            }
        }
        else {
            for (var i = 0; i < players.length; i++) {
                var ytdStats = {};
                for (var key in players[i].Pitching) {
                    for (var prop in players[i].Pitching[key]) {
                        if (!ytdStats[prop]) {
                            ytdStats[prop] = 0;
                        }

                        ytdStats[prop] += parseInt(players[i].Pitching[key][prop]);
                    }

                }

                ytdStats.ERA = (ytdStats.ER / ytdStats.IP) * 9;


                var daily = players[i].Pitching;
                players[i].Pitching = {};
                if (players[i].Pitching) {
                    players[i].Pitching.ytd = ytdStats;
                    players[i].Pitching.daily = daily;
                }
            }
            callback(players);
        }
    },

    getYtdBatting: function (startDate, endDate, currentDate, index, players, callback) {
        var context = this;
        if (players[index] && index < players.length) {
            if (players[index]._doc) {
                players[index] = players[index]._doc;
            }
            if (!players[index].Batting) {
                players[index].Batting = [];
            }

            if (moment(currentDate) <= moment(endDate)) {

                var query = { $and: [{ "GameDate": currentDate }, { "Stats": { $elemMatch: { "MLBId": players[index].MlbId } } }] };
                var projection = { "Stats.$": 1, "GameDate": 1 };

                Batting.findOne(query, projection, function (err, stats) {
                    if (stats && stats.Stats[0].GameId.length > 0) {
                        var data = stats.Stats[0];
                        delete data.TeamNbr;
                        delete data.Team;
                        delete data.PlayerId;
                        delete data.PlayerName;
                        delete data.MLBId;
                        delete data.FirstName;
                        delete data.LastName;
                        //delete data.GameDate;
                        delete data.GameString;
                        //delete data.GameId;

                        data.H = parseInt(data.H) || 0;
                        data["1B"] = parseInt(data["1B"]) || 0;
                        data["2B"] = parseInt(data["2B"]) || 0;
                        data["3B"] = parseInt(data["3B"]) || 0;
                        data["HR"] = parseInt(data.HR) || 0;
                        data["AB"] = parseInt(data.AB) || 0;
                        data["FB"] = parseInt(data.FB) || 0;
                        data["GB"] = parseInt(data.GB) || 0;

                        data.POuts = parseInt(data.POuts) || 0;
                        data["DP_noGIDP"] = parseInt(data["DP_noGIDP"]) || 0;
                        data["K_L"] = parseInt(data["K_L"]) || 0;
                        data["K_S"] = parseInt(data["K_S"]) || 0;
                        data.IFH = parseInt(data.IFH) || 0;
                        data.PA = parseInt(data.PA) || 0;
                        data.SLG = parseInt(data.SLG) || 0;

                        data.AVG = parseInt(data.AVG) || 0;
                        data.BHAtt = parseInt(data.BHAtt) || 0;
                        data.BH = parseInt(data.BH) || 0;
                        data.POPU = parseInt(data.POPU) || 0;
                        data.LD = parseInt(data.LD) || 0;

                        data["GB/FB"] = parseInt(data["GB/FB"]) || 0;
                        data.FB = parseInt(data.FB) || 0;
                        data.GB = parseInt(data.GB) || 0;
                        data.HFC = parseInt(data.HFC) || 0;
                        data.GIDP = parseInt(data.GIDP) || 0;

                        data.CS = parseInt(data.CS) || 0;
                        data.SB = parseInt(data.SB) || 0;
                        data.K = parseInt(data.K) || 0;
                        data.IBB = parseInt(data.IBB) || 0;
                        data.BB = parseInt(data.BB) || 0;

                        data.HBP = parseInt(data.HBP) || 0;
                        data.SF = parseInt(data.SF) || 0;
                        data.SAC = parseInt(data.SAC) || 0;
                        data.RBI = parseInt(data.RBI) || 0;
                        data.R = parseInt(data.R) || 0;

                        players[index].Batting.push(data);
                    }
                    context.getYtdBatting(startDate, endDate, moment(currentDate).add(1, 'day').format("YYYYMMDD"), index, players, callback);
                });
            }
            else {
                context.getYtdBatting(startDate, endDate, startDate, ++index, players, callback);
            }
        }
        else {
            for (var i = 0; i < players.length; i++) {
                var ytdStats = {};
                var lastKey = 0;
                for (var key in players[i].Batting) {
                    for (var prop in players[i].Batting[key]) {
                        if (!ytdStats[prop]) {
                            ytdStats[prop] = 0;
                        }
                        switch (prop) {
                            case "AVG":
                                ytdStats[prop] = (((players[i].Batting[key]["H"] + (ytdStats["H"] || 0))) / (players[i].Batting[key]["AB"] + (ytdStats["AB"] || 0))).toFixed(3);
                                break;
                            case "SLG":
                                var historicalHits = ytdStats["1B"] + ytdStats["2B"] + (2 * ytdStats["3B"]) + (3 * ytdStats["HR"]);
                                var currentHits = players[i].Batting[key]["1B"] + players[i].Batting[key]["2B"] + (2 * players[i].Batting[key]["3B"]) + (3 * players[i].Batting[key]["HR"]);

                                var hits = 0;
                                if (parseInt(historicalHits)) {
                                    hits = parseInt(historicalHits) + parseInt(currentHits);
                                }
                                else {
                                    hits = parseInt(currentHits);
                                    ytdStats["AB"] = 0;
                                    ytdStats["SLG"] = 0;
                                }

                                ytdStats[prop] = ((hits) / (players[i].Batting[key]["AB"] + ytdStats["AB"])).toFixed(3);
                                break;

                            default:
                                ytdStats[prop] += parseInt(players[i].Batting[key][prop]);
                                break;
                        }

                    }

                    ytdStats.SBP = 20 * (((ytdStats.SB + 3) / (ytdStats.SB + ytdStats.CS + 7)) - 0.4).toFixed(3) || 0;
                    ytdStats.SBA = 1 / 0.07 * Math.sqrt(((ytdStats.SB + ytdStats.CS) / (ytdStats["1B"] + ytdStats.BB + ytdStats.HBP))) || 0;
                    ytdStats.T = 1 / 0.0016 * (ytdStats["3B"] / (ytdStats.AB - ytdStats.HR - ytdStats.K)) || 0;
                    ytdStats.RS = 25 * (((ytdStats.R - ytdStats.HR) / (ytdStats.H + ytdStats.BB + ytdStats.HBP - ytdStats.HR)) - 0.1) || 0;
                    ytdStats.GDP = (1 / 0.007) * (0.063 - (ytdStats.GIDP / ytdStats.AB - ytdStats.HR - ytdStats.K)) || 0;


                    if (players[i].Batting[lastKey].G != players[i].Batting[key].G) {
                        ytdStats.LastGame = key;
                    }
                }

                var daily = players[i].Batting;
                players[i].Batting = {};
                if (players[i].Batting) {
                    players[i].Batting.ytd = ytdStats;
                    players[i].Batting.daily = daily;
                }
            }
            callback(players);
        }
    },
*/
    getPlayerByMlbId: function (playerId, callback) {
        Player.findOne({ MlbId: playerId }, function (err, player) {
            if (err) {
                callback(err, null);
            }
            else {
                callback(null, player);
            }
        });
    },

    getCurrentData: function (date, playerData) {
        var index = this.findWithAttr(playerData, 'GameDate', moment(date).format("M/DD/YYYY"));
        if (index > 2 && playerData.length > 2) {
            playerData = playerData.splice(index - 3, 3);
        }
        else if (index < 3 && playerData.length > 2) {
            playerData = playerData.splice(0, 3);
        }
        playerData.sort(function (a, b) {
            return moment(b.GameDate).diff(moment(a.GameDate))
        });
        return playerData;
    },

    findWithAttr: function (array, attr, value) {
        for (var i = 0; i < array.length; i += 1) {
            if (array[i][attr] === value) {
                return i;
            }
        }
        return -1;
    },

    getPlayerStatusKeyByValue: function (value) {
        for (var prop in playerStatus) {
            if (playerStatus.hasOwnProperty(prop)) {
                if (playerStatus[prop] === value)
                    return prop;
            }
        }
    },

    intraTeamMove: function (user, leagueId, teamId, player, from, to, status, newStatusName, callback) {
        // move player between "lists" on the roster
        // note the "ActionDate" in the move
        // if a move that is time-bound, store the DueDate
        // if a player move takes them off the 40-Man Roster, set onFortyMan to false
        // if a player is moved to/from the Acitve/ML Roster, touch both AL/NL rosters and set Status Active/InActive as applicable
        // if a player is moved to/from the Active (ML) Roster, or to/from 40-Man, set rSalary.Guaranteed to true
        // get this roster
        var moment = require('moment-timezone');
        moment.tz.setDefault("America/Los_Angeles");

        let now = moment().format('M/D/YY');

        var setPlayerDueDates = this.setPlayerDueDates;

        var fromStatusKey = this.getPlayerStatusKeyByValue(player.Status);
        var toStatusKey = this.getPlayerStatusKeyByValue(status);

        var fromStatusText = playerStatus.Details[fromStatusKey].Text;
        var toStatusText = playerStatus.Details[toStatusKey].Text;

        var broadcastMessageToLeague = false;
        // if new status is waivers, note the time.
        var actualStatus = player.Status;
        if (actualStatus == playerStatus.OutrightWaivers || actualStatus == playerStatus.OutrightWaiversAA) {
            player["Options"]["PriorOR"] = now;
            broadcastMessageToLeague = true;
        }

        // if any other waivers or moves to free agency, alert everyone
        if (status == playerStatus.Release
            || status == playerStatus.FreeAgent
            || status == playerStatus.TradeWaiversPending
            || status == playerStatus.TradeWaiversCleared) {
            broadcastMessageToLeague = true;
        }

        var transactionTemplate = playerStatus.Details[toStatusKey].msg;

        // before we go any further, see if moving against a time-restricted situation...
        // allow admins to do it any time.
        // NEW RULE AS OF 8/3/18: ALLOW OWNERS TO MOVE Rehab players regardless of time
        if (user != null && user.Roles.indexOf("Admin") == -1 && player.Status != playerStatus.DLRehab ) {
            if (player.Status > playerStatus.DL && player.Status < playerStatus.WaiversEnd) {
                var minTime = player.MinTime;
                if (minTime != "") {

                    var minDate = moment(minTime);
                    var diff = moment().diff(minDate, 'hours');
                    if (diff < 0) {
                        callback(null, { status: 500, msg: "Cannot recall " + player.FullName + " until " + moment(minTime).format("llll") });

                        return;
                    }
                }
            }
        }

        var sendNotification = false;
        if (from != to || broadcastMessageToLeague) {
            fromStatusText = from;
            toStatusText = to;
            sendNotification = true;
        }

        var query = {
            $and: [
                { LeagueId: leagueId },
                { TeamId: teamId }
            ]
        };
        Roster.findOne(query, function (err, roster) {
            if (err) {
                callback(err);
            }
            else if (roster) {

                // find the player

                var markFromList = "FortyManNL";
                var wasGuaranteed = false;
                if (player.rSalary && ("Guaranteed" in player.rSalary) && player.rSalary.Guaranteed == true) {
                    wasGuaranteed = true;
                }
                var playerList = [];
                switch (from) {
                    case "ML":
                        playerList = roster.FortyManNL;
                        break;
                    case "DL":
                        markFromList = "DL";
                        if (roster.DL)
                            playerList = roster.DL;
                        break;
                    case "Waivers":
                        markFromList = "Waivers";
                        if (roster.Waivers)
                            playerList = roster.Waivers;
                        break;
                    case "OtherLists":
                        markFromList = "OtherLists";
                        if (roster.OtherLists)
                            playerList = roster.OtherLists;
                        break;
                    default:
                        // nonroster
                        markFromList = "NonRoster";
                        if (roster.NonRoster)
                            playerList = roster.NonRoster;
                        break;
                }

                if (playerList.length == 0) {
                    err = { errmsg: "'From' roster is not valid." };
                    callback(err);
                } else {

                    var playerIndex = null;
                    for (i = 0; i < playerList.length; i++) {
                        if (playerList[i].PlayerId == player.PlayerId) {
                            playerIndex = i;
                            break;
                        }
                    }

                    if (playerIndex == null) {
                        err = { errmsg: "Player not found in roster." };
                        callback(err);
                    } else {
                        // ok.. have the player's FROM list
                        var playerToList = null;

                        var removeOnForty = false;
                        var removeActive = false;
                        var toStatus = null;
                        if (status && status != "")
                            toStatus = status;

                        var markToList = "FortyManNL";
                        playerToList = roster.FortyManNL;
                        var toType = null;
                        if (toStatus) {
                            toType = to;
                            switch (toStatus) {
                                case playerStatus.ActiveRoster:
                                    break;

                                /* DL lists */
                                case playerStatus.DL60:
                                    removeOnForty = true;
                                // fall through
                                case playerStatus.DL10:
                                case playerStatus.DLConcussion:
                                case playerStatus.DLRehab:
                                    if (toStatus > playerStatus.DL10) {
                                        removeActive = true;
                                    }
                                    break;

                                /* outright waivers */
                                case playerStatus.OutrightWaivers:
                                case playerStatus.OutrightWaiversAA:
                                case playerStatus.Release:
                                    // markToList = "Waivers";
                                    /*
                                    if (roster.Waivers)
                                        playerToList = roster.Waivers;
                                        */
                                    removeOnForty = true;
                                    removeActive = true;
                                    sendNotification = true;
                                    break;

                                /* time off lists */
                                case playerStatus.Restricted:
                                case playerStatus.DesignateForAssignment:
                                    removeOnForty = true;
                                // fall through...
                                case playerStatus.Paternity:
                                case playerStatus.Bereavement:
                                case playerStatus.Suspended:

                                    // markToList = "OtherLists";
                                    removeActive = true;
                                    /*
                                    if (roster.OtherLists)
                                        playerToList = roster.OtherLists;
                                    */
                                    break;
                                /*
                            default:
                                // nonroster
                                markToList = "NonRoster";
                                if (roster.NonRoster)
                                    playerToList = roster.NonRoster;
                                if (toStatus == playerStatus.FortyManInactive) {

                                } else {
                                    removeOnForty = true;
                                }
                                if (toType == "RK" || toType == "ADV-RK" || toType == "DSL" || toType == "EST") {
                                    removeActive = true;
                                }
                                break;
                                */
                            }
                        }
                        if (to != "ML") {
                            markToList = "NonRoster";
                            removeActive = true;
                            if (roster.NonRoster)
                                playerToList = roster.NonRoster;
                            if (toStatus == playerStatus.FortyManInactive) {

                            } else {
                                removeOnForty = true;
                            }
                            if (to == "RK" || to == "ADV-RK" || to == "DSL" || to == "EST") {
                                removeActive = true;
                            }
                        }

                        if (playerToList == null) {
                            err = { errmsg: "'To' roster is not valid." };
                            callback(err);

                        } else {

                            // have the player and the from and to rosters.. make the change now.
                            // adjust the player's data

                            var wasML = false;
                            if (player.Level == "ML") {
                                // need to move off of both lists
                                wasML = true;
                                var alList = roster.FortyManAL;
                                var alIndex = null;
                                for (i = 0; i < alList.length; i++) {
                                    if (alList[i].PlayerId == player.PlayerId) {
                                        alIndex = i;
                                        break;
                                    }
                                }
                            }

                            // change key fields:
                            player.Level = toType;
                            if (from == "ML") {
                                player["rSalary"]["Guaranteed"] = true;
                            }
                            if (removeOnForty) {
                                if (player["onFortyMan"]) {
                                    player["rSalary"]["Guaranteed"] = true;
                                }
                                player["onFortyMan"] = false;
                            }

                            if (toStatus == playerStatus.FortyManInactive) {
                                if (!removeOnForty) {
                                    // moving to inactive roster
                                    // if we're not removing him from 40-man, then be sure he's still on 40-man
                                    player["onFortyMan"] = true;
                                    player["rSalary"]["Guaranteed"] = true;
                                }
                            } else if (toStatus) {
                                player.Status = toStatus;
                                if (toStatus == playerStatus.ActiveRoster) {
                                    player["onFortyMan"] = true;
                                    player["rSalary"]["Guaranteed"] = true;
                                }
                            }
                            if (removeActive && player.Status == playerStatus.ActiveRoster) {
                                player.Status = playerStatus.Inactive;
                                player["rSalary"]["Guaranteed"] = true;
                            }

                            // add in the action date, minDate, dueDate
                            player = setPlayerDueDates(player, newStatusName);

                            // add to new list.
                            if (playerToList == playerList) {
                                // then don't need to move the player
                                if (to == "ML") {
                                    // need to copy fields to AL roster

                                    // ((((((((((( TODO: Make this a playerHelper function)))))))))))
                                    fortyman = roster.FortyManAL;
                                    var orIndex = -1;
                                    for (i = 0; i < fortyman.length; i++) {

                                        if (fortyman[i].PlayerId == player.PlayerId) {
                                            // found the matching player
                                            orIndex = i;
                                            break;
                                        }
                                    }
                                    if (orIndex >= 0) {
                                        // grab the stuff that isn't the same!
                                        var otherself = fortyman[orIndex];
                                        var batting = -1;
                                        if (otherself.BattingOrder) {
                                            batting = otherself.Batting;
                                        }
                                        var bench = -1;
                                        if (otherself.BenchOrder) {
                                            bench = otherself.BenchOrder;
                                        }
                                        var rd = "";
                                        if (otherself.Rdepth) {
                                            rd = otherself.Rdepth;
                                        }

                                        // ok, duplicate from one league to the other!
                                        otherself = player;
                                        otherself["BattingOrder"] = batting;
                                        otherself["BenchOrder"] = bench;
                                        otherself["Rdepth"] = rd;

                                        fortyman[orIndex] = otherself;

                                    }
                                    playerList[playerIndex] = player;
                                } else {
                                    playerToList[playerIndex] = player;
                                }
                            } else {
                                // changing lists

                                playerToList.unshift(player);
                                if (to == "ML") {
                                    roster.FortyManAL.unshift(player);
                                }

                                // remove from old list
                                playerList.splice(playerIndex, 1);
                                if (wasML == true && player.Level != "ML")
                                    alList.splice(alIndex, 1);
                            }

                            // save the roster
                            roster.markModified(markFromList);
                            if (markFromList == "FortyManNL")
                                roster.markModified("FortyManAL");

                            roster.markModified(markToList);
                            if (markToList == "FortyManNL") {
                                roster.markModified("FortyManAL");
                            }


                            rosterLineupHelper.updateDepthChartsFromRoster(roster, false, false, function () {


                                roster.save(function (err, response) {
                                    if (err) {
                                        console.log(err.message);
                                    } else {
                                        console.log("Player moved. Roster saved ");


                                        // send notifiation of change to transaction collection
                                        // send messages to interested parties

                                        // now create a new transaction

                                        leagueHelper.getTeamById(leagueId, teamId, function (err, teamData) {

                                            if (from == "ML" && (to == "Triple-A" || to == "Double-A")) {

                                                // moving to AAA or AA.. see if from majors
                                                if (from == "ML") {
                                                    transactionTemplate = "{{name}} cleared waivers and was assigned to the " + to + " ";
                                                    if (to == "Triple-A") {
                                                        transactionTemplate += teamData.affiliates.AAA;
                                                    } else {
                                                        transactionTemplate += teamData.affiliates.AA;
                                                    }
                                                }
                                            }
                                            if (to != "ML") {

                                                levelList = {
                                                    "AAA": { level: "Triple-A", key: "AAA" },
                                                    "AA": { level: "Double-A", key: "AA" },
                                                    "R": { level: "Rookie", key: "Rookie" },
                                                    "RA": { level: "Advanced Rookie", key: "Rookie-Advanced" },
                                                    "A-": { level: "Low-A", key: "A-Low" },
                                                    "A+": { level: "High-A", key: "A-High" },
                                                    "DSL": { level: "DSL", key: "" },
                                                    "EST": { level: "EST", key: "" },

                                                    // new fashioned names...
                                                    "Triple-A": { level: "Triple-A", key: "AAA" },
                                                    "Double-A": { level: "Double-A", key: "AA" },
                                                    "ADV-RK": { level: "Rookie", key: "Rookie" },
                                                    "RK": { level: "Advanced Rookie", key: "Rookie-Advanced" },
                                                    "Low-A": { level: "Low-A", key: "A-Low" },
                                                    "High-A": { level: "High-A", key: "A-High" },
                                                }
                                                transactionTemplate = transactionTemplate.replace("{{level}}", levelList[to].level);
                                                var minorteam = "";
                                                if (levelList[to].key != "") {
                                                    minorteam = teamData.affiliates[levelList[to].key];
                                                }
                                                transactionTemplate = transactionTemplate.replace("{{minorteamname}}", minorteam);

                                            }

                                            var transaction = new Transaction();
                                            transaction.Type = "IntraTeamMove";
                                            transaction.Status = "Complete";
                                            transaction.Archived = false;
                                            transaction.LeagueId = leagueId;
                                            transaction.DateUTC = new Date().toISOString();
                                            transaction.DealId = 0;
                                            transaction.Teams = [
                                                { name: teamData.r_name, id: teamId }
                                            ];

                                            transferDate = moment(transaction.DateUTC).format("llll");

                                            transaction.Headline = "The " + teamData.r_name + " placed  " + player.FullName + toStatusText;

                                            transaction.Headline = "The " + teamData.r_name + ' ' + transactionTemplate.replace("{{name}}", player.FullName);

                                            if (actualStatus == playerStatus.OutrightWaivers || actualStatus == playerStatus.OutrightWaiversAA) {
                                                transaction.Headline = teamData.r_name + " " + player.FullName + " cleared waivers and was assigned outright to ";
                                                if (actualStatus == playerStatus.OutrightWaivers) {
                                                    transaction.Headline += "Triple-A";
                                                } else {
                                                    transaction.Headline += "Double-A";
                                                }

                                            }
                                            Transaction.create(transaction, function (err, t) {
                                                if (err) {
                                                } else {

                                                    if (sendNotification) {
                                                        // send an email!
                                                        skipAdmins = false;
                                                        var toTeamId = teamId;
                                                        if (broadcastMessageToLeague) {
                                                            toTeamId = "All";
                                                        }
                                                        messageHelper.alertInterestedParties(user, leagueId, t.Headline, toTeamId, teamData.r_name, "", player, fromStatusText, toStatusText, skipAdmins, function (response) {


                                                            // see if we need to change the financials!
                                                            // only do this if the player wasn't guaranteed and now is
                                                            if (wasGuaranteed == false && player.rSalary && ("Guaranteed" in player.rSalary) && player.rSalary.Guaranteed == true) {
                                                                save = true;
                                                                leagueHelper.updateFinancials(leagueId, teamId, "", save, function (err, message, finArray) {
                                                                    if (err) {
                                                                        callback(null, { status: 200, msg: player.FullName + " moved from " + markFromList + " to " + markToList + " and financials FAILED TO UPDATE" });
                                                                    } else {
                                                                        callback(null, { status: 200, msg: player.FullName + " moved from " + markFromList + " to " + markToList + " and financials updated" });
                                                                    }

                                                                })

                                                            } else {
                                                                // we're all done,
                                                                // respond to the user

                                                                callback(null, { status: 200, msg: player.FullName + " moved from " + markFromList + " to " + markToList });

                                                                // ((((((((((((((((((((( TODO )))))))))))))))))))))
                                                                // send notifiation of transaction to transaction collection
                                                                // send messages to interested parties
                                                            }

                                                        });
                                                    } else {
                                                        // see if we need to change the financials!
                                                        // only do this if the player wasn't guaranteed and now is
                                                        if (wasGuaranteed == false && player.rSalary && ("Guaranteed" in player.rSalary) && player.rSalary.Guaranteed == true) {
                                                            save = true;
                                                            leagueHelper.updateFinancials(leagueId, teamId, "", save, function (err, message, finArray) {
                                                                if (err) {
                                                                    callback(null, { status: 200, msg: player.FullName + " moved from " + markFromList + " to " + markToList + " and financials FAILED TO UPDATE" });
                                                                } else {
                                                                    callback(null, { status: 200, msg: player.FullName + " moved from " + markFromList + " to " + markToList + " and financials updated" });
                                                                }

                                                            })

                                                        } else {
                                                            // we're all done,
                                                            // respond to the user

                                                            callback(null, { status: 200, msg: player.FullName + " moved from " + markFromList + " to " + markToList });

                                                            // ((((((((((((((((((((( TODO )))))))))))))))))))))
                                                            // send notifiation of transaction to transaction collection
                                                            // send messages to interested parties
                                                        }

                                                    }

                                                }

                                            });
                                        });
                                    };
                                });
                            });
                        }
                    }
                }
            }
        });
    },

    setPlayerStatus: function (user, leagueId, teamId, player, from, status, newStatusName, callback) {
        // set the player's trade status without moving him between lists
        // note the "ActionDate" in the move
        // it is a move that is time-bound, store the DueDate
        var moment = require('moment-timezone');
        moment.tz.setDefault("America/Los_Angeles");

        var fromStatusKey = this.getPlayerStatusKeyByValue(player.Status);
        var toStatusKey = this.getPlayerStatusKeyByValue(status);

        var fromStatusText = playerStatus.Details[fromStatusKey].Text;
        var toStatusText = playerStatus.Details[toStatusKey].Text;

        // get this player's roster
        var setPlayerDueDates = this.setPlayerDueDates;
        var query = {
            $and: [
                { LeagueId: leagueId },
                { TeamId: teamId }
            ]
        };
        Roster.findOne(query, function (err, roster) {
            if (err) {
                callback(err);
            }
            else if (roster) {

                // find the player' list and index in that list
                var playerList = [];
                var modifiedFrom = "FortyManNL";

                switch (from) {
                    case "ML":
                        playerList = roster.FortyManNL;
                        break;
                    default:
                        // nonroster
                        modifiedFrom = "NonRoster";
                        if (roster.NonRoster)
                            playerList = roster.NonRoster;
                        break;
                }

                if (playerList.length == 0) {
                    err = { errmsg: "Roster has no players." };
                    callback(err);
                } else {

                    // find the index of the player in their list
                    var playerIndex = null;
                    for (i = 0; i < playerList.length; i++) {
                        if (playerList[i].PlayerId == player.PlayerId) {
                            playerIndex = i;
                            break;
                        }
                    }

                    if (playerIndex == null) {
                        err = { errmsg: "Player not found in roster." };
                        callback(err);
                    } else {
                        // ok.. have the player's index

                        // have the player and correct roster list and index.. see if in the ML Game Player List
                        var wasML = false;
                        var wasGuaranteed = false;
                        if (player.rSalary && ("Guaranteed" in player.rSalary) && player.rSalary.Guaranteed == true) {
                            wasGuaranteed = true;
                        }
                        if (player.Level == "ML") {
                            // need to change on both lists!
                            wasML = true;
                            var alList = roster.FortyManAL;
                            var alIndex = null;
                            for (i = 0; i < alList.length; i++) {
                                if (alList[i].PlayerId == player.PlayerId) {
                                    alIndex = i;
                                    break;
                                }
                            }
                        }

                        // grab the new status.. and make the change
                        var newStatus = playerStatus.Inactive;
                        if (status) {
                            // removing from list
                            newStatus = status;
                        }

                        // change key fields and set the action and due dates
                        var field = "Status";
                        var minTime = "";
                        var dueTime = "";
                        var removeOnForty = false;


                        if (newStatus >= playerStatus.TradeWaivers && newStatus < playerStatus.TradingBlockEnd) {
                            field = "TradeStatus";

                            if (newStatus == playerStatus.TradeWaivers) {
                                newStatus = playerStatus.TradeWaiversPending;
                            }

                            if (newStatus == playerStatus.CancelTrading) {
                                newStatus = playerStatus.UnavailableForTrade;
                            }

                        } else {

                        }

                        /* remove these players from active:
                            DL10,
                            DL60,
                            DLConc,
                            DLRehab,
                            Bereavement,
                            Paternity,
                            Restricted,
                            Suspended,
                            Outright and OutrightAA,
                            Release,
                            DFA,

                        */

                        /* Trade Waivers players REMAIN on the active roster */

                        /* remove these players from 40-man:
                            DL60,
                            Restricted,
                            Outright and OutrightAA,
                            Release,
                            DFA
                        */
                        // see if removing from 40-man roster
                        if ((newStatus >= playerStatus.Waivers && newStatus < playerStatus.WaiversEnd)
                            || newStatus == playerStatus.DL60
                            || newStatus == playerStatus.Restricted
                            || newStatus == playerStatus.OutrightWaivers
                            || newStatus == playerStatus.OutrightWaiversAA
                            || newStatus == playerStatus.DesignateForAssignment) {
                            removeOnForty = true;
                        }


                        // add in the action date, minDate, dueDate
                        if (newStatusName != "Return") {
                            player = setPlayerDueDates(player, newStatusName);
                        }


                        if (newStatus == playerStatus.FortyManInactive) {
                            field = "onFortyMan";
                            newStatus = true;
                        }


                        if (newStatus == playerStatus.ActiveRoster || newStatus == playerStatus.FortyManInactive) {
                            field = "onFortyMan";
                            newStatus = true;
                            player["rSalary"]["Guaranteed"] = true;
                        }


                        if (newStatus == playerStatus.RemoveWaivers) {
                            player["ClaimingTeams"] = [];
                            newStatus = playerStatus.Inactive;
                            field = "Status";
                            if (playerList == roster.FortyManNL)
                                newStatus = playerStatus.ActiveRoster;
                            player["TradeStatus"] = playerStatus.UnavailableForTrade;
                            player["onFortyMan"] = true;
                        }

                        if (newStatusName == "Return") {
                            newStatus = playerStatus.ActiveRoster;
                            player["onFortyMan"] = true;
                        }

                        // AND NOW, DROP IN THE NEW STATUS!
                        player[field] = newStatus;
                        if (removeOnForty == true) {
                            player["onFortyMan"] = false;
                        }

                        // if moving to waivers, clear out any claiming teams
                        if (newStatus == playerStatus.OutrightWaiversAA
                            || newStatus == playerStatus.OutrightWaivers
                            || newStatus == playerStatus.TradeWaiversPending
                            || newStatus == playerStatus.TradeWaiversCleared
                        ) {
                            player["ClaimingTeams"] = [];
                        }



                        // replace in list(s)
                        playerList[playerIndex] = player;
                        if (wasML) {
                            if( alIndex ) {
                            alList[alIndex][field] = newStatus;
                            alList[alIndex]["ActionTime"] = player.ActionTime;
                            alList[alIndex]["MinTime"] = player.MinTime;
                            alList[alIndex]["DueTime"] = player.DueTime;
                            }
                        }

                        // save the roster
                        roster.markModified(modifiedFrom);
                        if (modifiedFrom == "FortyManNL") {
                            roster.markModified("FortyManAL");
                        }

                        rosterLineupHelper.updateDepthChartsFromRoster(roster, false, false, function () {


                            roster.save(function (err, response) {
                                if (err) {
                                    console.log(err.message);
                                } else {
                                    console.log("Player Status changed: " + player.FullName + ". Roster saved ");

                                    // send notifiation of transaction to transaction collection
                                    // send messages to interested parties
                                    // now create a new transaction
                                    var transaction = new Transaction();
                                    transaction.Type = "StatusChange";
                                    transaction.Status = "Complete";
                                    transaction.Archived = false;
                                    transaction.LeagueId = leagueId;
                                    transaction.DateUTC = new Date().toISOString();
                                    transaction.DealId = 0;
                                    transaction.Teams = [
                                        { name: roster.TeamAbbr, id: teamId }
                                    ];

                                    transferDate = moment(transaction.DateUTC).format("llll");

                                    transaction.Headline = "STATUS CHANGE (" + roster.TeamAbbr + "): " + player.FullName + " changed status to " + toStatusText;

                                    if (newStatus >= playerStatus.TradingBlock && newStatus < playerStatus.TradingBlockEnd) {
                                        skipAdmins = false;
                                        messageHelper.alertInterestedParties(user, leagueId, transaction.Headline, teamId, "", "", player, fromStatusText, toStatusText, skipAdmins, function (response) {
                                            // all done,                               
                                            // respond to the user
                                            callback(null, { status: 200, msg: player.FullName + " status changed to " + newStatus });
                                            return;
                                        });
                                        return;
                                    }
                                    Transaction.create(transaction, function (err, t) {
                                        if (err) {
                                        } else {
                                            // next PM and email those folks who care about certain changes
                                            if ((newStatus >= playerStatus.Waivers && newStatus < playerStatus.WaiversEnd)
                                                || (newStatus >= playerStatus.TradingBlock && newStatus < playerStatus.TradingBlockEnd)) {

                                                // send an email!
                                                skipAdmins = false;
                                                messageHelper.alertInterestedParties(user, leagueId, t.Headline, teamId, "", "", player, fromStatusText, toStatusText, skipAdmins, function (response) {

                                                    // finally if the salary was guaranteed, then update the financials
                                                    if (wasGuaranteed == false && player.rSalary && ("Guaranteed" in player.rSalary) && player.rSalary.Guaranteed == true) {
                                                        save = true;
                                                        leagueHelper.updateFinancials(leagueId, teamId, "", save, function (err, message, finArray) {
                                                            if (err) {
                                                                callback(null, { status: 200, msg: player.FullName + " status changed to " + newStatus + " and financials NOT UPDATED" });
                                                            } else {
                                                                callback(null, { status: 200, msg: player.FullName + " status changed to " + newStatus + " and financials updated" });
                                                            }

                                                        })
                                                    } else {
                                                        // all done,                               
                                                        // respond to the user
                                                        callback(null, { status: 200, msg: player.FullName + " status changed to " + newStatus });


                                                    }
                                                })
                                            } else {
                                                // finally if the salary was guaranteed, then update the financials
                                                if (wasGuaranteed == false && player.rSalary && ("Guaranteed" in player.rSalary) && player.rSalary.Guaranteed == true) {
                                                    save = true;
                                                    leagueHelper.updateFinancials(leagueId, teamId, "", save, function (err, message, finArray) {
                                                        if (err) {
                                                            callback(null, { status: 200, msg: player.FullName + " status changed to " + newStatus + " and financials NOT UPDATED" });
                                                        } else {
                                                            callback(null, { status: 200, msg: player.FullName + " status changed to " + newStatus + " and financials updated" });
                                                        }

                                                    })
                                                } else {
                                                    // all done,                               
                                                    // respond to the user
                                                    callback(null, { status: 200, msg: player.FullName + " status changed to " + newStatus });


                                                }
                                            }
                                        }
                                    })

                                }
                            });

                        });
                    }
                }
            }
        });
    },

    setPlayerDueDates: function (player, newStatusName) {

        var moment = require('moment-timezone');
        moment.tz.setDefault("America/Los_Angeles");

        var actionTime = moment.tz().format();
        var startTime = moment.tz();
        var minTime = "";
        var dueTime = "";
        var newStatusID = playerStatus[newStatusName];

        if (playerStatus.Details[newStatusName]) {
            details = playerStatus.Details[newStatusName];

            if (details.Start != "") {
                // ok.. need to pick start time based on status

                var nextHour = moment().tz("America/Los_Angeles").endOf("hour").add(1, "millisecond");
                var nextHourF = nextHour.format();
                var startTimeH = moment().tz("America/Los_Angeles").format("H");
                var startTimeM = moment().tz("America/Los_Angeles").format("m");


                if (details.Start == "-3 days" || details.Start == "tomorrow") {
                    // default, start this morning at 9am
                    startTime = moment.tz("America/Los_Angeles").startOf('day').hour(9).minute(0);
                    var stf = startTime.format();

                    // 1) see when last rSports game was (((( TODO ))))
                    if (details.Start == "-3 days") {
                        // if so, startTime = startTime.subtract(#,"days");
                    }

                    // 2) set to 9am today or the next morning
                    if (startTimeH < 9 || (startTimeH == 9 && startTimeM == 0)) {
                        // then THIS morning PT.. and convert to GMT
                        // that's the default!
                    } else {
                        // start tomorrow morning
                        startTime = moment.tz("America/Los_Angeles").startOf('day').add(1, "day").hour(9).minute(0);
                        stf = startTime.format();
                    }

                    // finally convert the start time to GMT
                    startTime = startTime.tz("GMT");
                }
                if (details.Start == "-3 days minus DL") {
                    // default, start this morning at 9am
                    startTime = moment.tz("America/Los_Angeles").startOf('day').hour(9).minute(0);

                    // 1) see when last rSports game was (((( TODO ))))
                    // if so, startTime = startTime.subtract(#,"days");

                    // 2) set to 9am today or the next morning
                  //  if (startTimeH < 9 || (startTimeH == 9 && startTimeMin == 0)) {
                    if (startTimeH <= 9 ) {
                        // then THIS morning PT.. and convert to GMT
                        // that's the default!
                    } else {
                        // start tomorrow morning
                        startTime = moment.tz("America/Los_Angeles").startOf('day').add(1, "day").hour(9).minute(0);
                    }

                    // finally convert the start time to GMT
                    startTime = startTime.tz("GMT");
                }

                if (details.Start == "nexthour") {
                    // default, start at the top of the next hour
                    startTime = nextHour;
                    if (startTimeM == 0) {
                        // right now is the top of the hour... start right now.
                        startTime = startTime.subtract(1, "hour");
                    }
                    // finally convert the start time to GMT
                    startTime = startTime.tz("GMT");
                }

                // finally convert the start time to GMT
                startTime = startTime.tz("GMT");

                if (details.Start == "now") {
                    // Start right now. This is for transfer of status
                    startTime = moment(player.DueTime);
                }

            }


            if (details.Preseason && details.Preseason != "") {
                minTime = details.Preseason;
                dueTime = details.Preseason;
            } else {
                // not preseason, proceed normally...

                if (details.Min != "") {
                    // then need to select min time and due time
                    // times in <units>:<count>  e.g. "hours:48"
                    var min = details.Min.split(":");
                    var max = details.Max.split(":");

                    minTime = startTime.clone();
                    if (min.length == 2 || player.Position != "P") {
                        minTime = minTime.add(min[1], min[0]).format();
                    } else {
                        // rehab for pitchers
                        minTime = minTime.add(min[2], min[0]).format();
                    }
                    dueTime = startTime.clone();
                    if (max.length == 2 || player.Position != "P") {
                        dueTime = dueTime.add(max[1], max[0]).format();
                    } else {
                        // for rehab
                        dueTime = dueTime.add(max[2], max[0]).format();
                    }
                }
            }

        }

        // where to put this...
        var minKey = "MinTime";
        var dueKey = "DueTime";
        if (newStatusID >= playerStatus.TradeWaivers && newStatusID < playerStatus.TradingBlockEnd) {
            var minKey = "TradeMinTime";
            var dueKey = "TradeDueTime";
        }
        player["ActionTime"] = actionTime;
        player[minKey] = minTime;
        player[dueKey] = dueTime;
        return player;
    },

    getPlayerStatusMsg: function (player) {

        var moment = require('moment-timezone');

        var msg = "";
        var statusKey = this.getPlayerStatusKeyByValue(player.Status);

        var endtime = "???";

        if (player.DueTime) {
            var duetime = moment(player.DueTime);
            endtime = duetime.tz("America/Los_Angeles").format("dd MM/DD @ h:00 A");
            test = endtime;

        }

        if (player.Status > playerStatus.Waivers && player.Status < playerStatus.WaiversEnd) {
            msg = playerStatus.Details[statusKey].Text + ". Clears " + endtime;
            if (player.ClaimingTeams && player.ClaimingTeams.length > 0) {
                msg = player.ClaimingTeams.length + " CLAIM(S) MADE  ◆  " + msg;
            }
        }


        if (player.TradeStatus && player.TradeStatus >= playerStatus.TradeWaivers) {
            var tradeduetime = moment(player.TradeDueTime);
            endtradetime = tradeduetime.tz("America/Los_Angeles").format("dd MM/DD @ h:00 A");
            test = endtime;

            if (msg != "") {
                msg = msg + ". ";
            }

            if (player.TradeStatus == playerStatus.TradeWaiversPending) {
                msg += "Claims through: " + endtradetime;
            } else {
                if (player.TradeStatus == playerStatus.OnTradingBlock) {
                    msg = "Available for Trade";
                } else {
                    msg += "Trade Availability: " + endtradetime;
                }
            }

        }

        if (player.Status >= playerStatus.DL && player.Status < playerStatus.DLEnd) {
            if (msg != "") {
                msg += "DL through " + endtime + ". " + msg;
            } else {
                msg = "Through " + endtime;
            }
        }

        if (player.Status >= playerStatus.LeaveLists && player.Status < playerStatus.LeaveEnd) {
            if (msg != "") {
                msg = playerStatus.Details[statusKey].Text + " until " + endtime + ". " + msg;
            } else {
                msg = "Through " + endtime;
            }
        }
        if (player.Status == playerStatus.FreeAgentOfferedContract) {
            msg = "Contract offer(s) tendered";
        }

        return (msg);
    },

    // ************************************
    //
    // executeNextAction
    // user, lid, tid, player, forceAction
    // called by admin or chron job.. not owners.
    // execute the move, create the transaction, email the interested parties
    //
    // *****************************************************************************
    executeNextAction: function (user, lid, tid, player, forceAction, callback) {

        var context = this;
        var currentStatus = player.Status;
        var tradeStatus = false;
        if (player.TradeStatus && player.TradeStatus > playerStatus.TradeWaiversPending && player.TradeStatus <= playerStatus.TradingBlockEnd) {
            tradeStatus = player.TradeStatus;
        }

        var newStatus;
        var newStatusName;

        // first see if there are two actions...
        var due = "";
        if (player.DueTime) {
            due = player.DueTime
        };

        var tdue = "";
        if (tradeStatus && player.TradeDueTime) {
            tdue = player.TradeDueTime;
        }

        if (due != "" && tdue != "") {
            // figure which comes FIRST
            var moment = require('moment-timezone');
            var sm = moment(due);
            var tm = moment(tdue);
            var diff = sm.diff(tm, 'minutes');

            if (diff > 0) {
                // then status due is later than trade do... do trade first
                currentStatus = tradeStatus;
            } else {
                // then trade due is later, so do status first
            }
        } else {
            if (tradeStatus)
                currentStatus = tradeStatus
        }

        var key = context.getPlayerStatusKeyByValue(currentStatus);
        var details = playerStatus.Details[key];

        var newStatusName = "";
        if (details.NextStatus) {
            newStatusName = details.NextStatus;
        }

        var newStatus = currentStatus;
        if (newStatusName == "" || newStatusName == "Return") {

        } else {
            newStatus = playerStatus[newStatusName];
        }

        var fromLevel = player.Level;
        var toLevel = player.Level;

        // now do manual adjustments
        if (currentStatus == playerStatus.OutrightWaivers) {
            toLevel = "Triple-A";
        }
        if (currentStatus == playerStatus.OutrightWaiversAA) {
            toLevel = "Double-A";
        }

        if (newStatusName == "FreeAgent") {
            carriedSalary = true;
            teamHelper.movePlayerToTeamOrFA(lid, user, tid, "Free Agents", player, carriedSalary, "{{fromTeam}} " + player.FullName + " released to Free Agency",
                function (responseObj) {
                    if (responseObj)
                        // res.status(responseObj.status).json(responseObj);
                        // return to caller
                        callback();
                    else
                        //  res.status(500).json({ status: 500, msg: "Sorry, there's a server error." });
                        callback();
                })
        } else {
            if (fromLevel == toLevel) {
                // stay in roster list and just update the status
                context.setPlayerStatus(user, lid, tid, player, fromLevel, newStatus, newStatusName,
                    function (err, result) {
                        if (err) {
                            // res.status(500).json({ status: 500, msg: err.errmsg });
                            callback();
                        } else {
                            // res.status(200).json(result);
                            callback();
                        }
                    });

            } else {
                // then a intra-roster move
                context.intraTeamMove(user, lid, tid, player, fromLevel, toLevel, newStatus, newStatusName,
                    function (err, result) {
                        if (err) {
                            // res.status(500).json({ status: 500, msg: err.errmsg });
                            callback();
                        } else {
                            // res.status(200).json(result);
                            callback();
                        }
                    });
            }
        }

    },

    // ************************************
    //
    // executeTimedActions
    // user, lid, tid, player[], forceAction, index
    // called by admin or chron job.. not owners.
    // execute list of moves, create the transaction, email the interested parties
    // also sends messages about deadlines that will expire in the next 24 hours
    //
    // *****************************************************************************
    executeTimedActions: function (user, lid, tid, player, forceAction, callback) {

        // First, grab all the rosters as we'll need to rifle through them
        // for now, ignore tid and player as we're doing all of them

        var moment = require('moment-timezon');
        var now = moment();

        Roster.find({ LeagueId: lid }, function (err, rosters) {
            if (err) {
                callback(err);
                return;
            } else {
                for (i = 0; i < rosters.length; i++) {
                    team = rosters[i];

                    for (p = 0; p < team.FortyManNL.length; p++) {
                        man = team.FortyManNL[p];
                        status = man.status;
                        trade = man.tradeStatus;
                        if (status > playerStatus.DL && status < playerStatus.TradeWaiversEnd) {
                            if ('DueTime' in man) {

                            }
                        }
                        if (trade == playerStatus.RequestTradingBlock) {
                            if ('TradeDueTime' in man) {

                            }
                        }
                    }
                }
            }
        })
    }

}
