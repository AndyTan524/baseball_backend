var fs = require('fs'); 
var utils = require('../helpers/utils');
var playerHelper = require('../helpers/playerHelper');
var mongoose = require('mongoose');
var Baserunning = mongoose.model('Baserunning');
var League = mongoose.model('League');
var Roster = mongoose.model('Roster');
var SimPlayer = mongoose.model('SimPlayer');

require("../simulator/set-pitchers");  // Step 3: set up the pitching staff
require("../simulator/set-lineup");    // Step 4: set up the batting order and fielders

module.exports = {
    getGamesByDate: function (date, callback) {
        Baserunning.findOne({ GameDate: date }, function (err, stat) {
            if (err) {
                callback(err, []);
            }
            else if (stat) {
                var games = [];
                for (var key in stat.Stats) {
                    var data = stat.Stats[key];
                    if (games.filter(function (key, value) {
                        return key.gameId == data.GameId;
                    }).length == 0 && data.GameId.length > 0) {
                        var visitname = utils.getTeamFromGameString(data.GameString);
                        var homename = utils.getTeamFromGameString(data.GameString, true);
                        var DHTeams = ["TOR", "BAL", "BOS", "NYY"];
                        var isDH = false;
                        if( DHTeams.indexOf(homename) != -1 ) {
                            isDH = true;
                        }
                        games.push({
                            home: { name: homename },
                            visit: { name: visitname },
                            dhGame: isDH,
                            gameId: data.GameId,
                            gameDate: date,
                            matchup: data.GameString
                        });
                    }
                }
                callback(err, games);
            }
            else {
                callback(err, []);
            }
        });
        /*
        var games = [
            {
                _id: "0000-aaaa",
                home: { team: "ARI", _id: "4f88f73b-28b2-4ead-853e-dd20c46d8350" },
                visit: { team: "ATL", _id: "4f88f73b-28b2-4ead-853e-dd20c46d8350" },
                dhGame: false,
                playedOn: "",
                score: "",
                boxScoreId: "",
                gameId: "1",
                gameDate: "20170430"
            }
        ];
        */
    },

    getFortManByLeague: function (lid, team, callback) {
        var query = { $and: [{ _id: (lid) }, { "Teams": { $elemMatch: { "r_name": team } } }] };
        var isNL = this.isTeamNL(team);

        League.findOne(query, { "Teams.$": 1, _id: 1 }, function (err, league) {
            if (err) {
                callback(err, null);
            }
            else if (league) {
                var query = {
                    $and: [
                        { LeagueId: lid },
                        { TeamId: league.Teams[0]._id }
                    ]
                };
                Roster.findOne(query, function (err, roster) {
                    if (err) {
                        callback(err, null);
                    }
                    else if (roster) {
                        if (isNL) {
                            roster.FortyMan = roster.FortyManNL;
                        }
                        else {
                            roster.FortyMan = roster.FortyManAL;
                        }
                        callback(err, roster.FortyMan);
                    }
                    else {
                        callback(err, []);
                    }
                });
            }
            else {
                callback(err, null);
            }
        });
    },

    getPlayerStats: function (players, index, callback) {
        var context = this;
        if (players && players.length > index + 1) {
            SimPlayer.findOne({ MlbId: players[index].MlbId }, function (err, player) {
                players[index] = player;
                context.getPlayerStats(players, ++index, callback);
            });
        }
        else {
            callback(players);
        }
        //playerHelper.getYtdFielding("20170404", date, "20170404", 0, players, function (fielding) {
        //    players = fielding;
        //    playerHelper.getYtdBatting("20170404", date, "20170404", 0, players, function (batting) { 
        //        players = batting;
        //        playerHelper.getYtdBaserunning("20170404", date, "20170404", 0, players, function (baserunning) {
        //            players = baserunning;
        //            playerHelper.getYtdPitching("20170404", date, "20170404", 0, players, function (pitching) {
        //                players = pitching; 
                        /*
                        var speed = 0;
                        var eSpeed = 0;
                        switch (stats.player.Position) {
                            case "P":
                                speed = 0;
                                eSpeed = 0;
                                break;
                            case "C":
                                speed = 1;
                                eSpeed = 1;
                                break;
                            case "1B":
                                speed = 2;
                                eSpeed = 2;
                                break;
                            case "2B":
                                speed = (5 / 4) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                eSpeed = (5 / 4) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                break;
                            case "3B":
                                speed = (4 / 2.65) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                eSpeed = (4 / 2.65) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                break;
                            case "SS":
                                speed = (4.6 / 7) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                eSpeed = (4.6 / 7) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                break;
                            case "DH":
                                speed = 0;
                                eSpeed = 0;
                                break;
                            default:
                                speed = (3) * ((stats.fielding.ytd.PO + stats.fielding.ytd.A) / stats.fielding.ytd.G);
                                eSpeed = (3) * ((stats.fielding.ytd.PO) / stats.fielding.ytd.G);
                                break;
                        }

                        stats.batting.ytd.SPEED = (stats.batting.ytd.SBA + stats.batting.ytd.T + stats.batting.ytd.RS + stats.batting.ytd.GIDP + speed) / 6;
                        stats.batting.ytd.ESPEED = (stats.batting.ytd.SBA + stats.batting.ytd.T + stats.batting.ytd.RS + stats.batting.ytd.GIDP + eSpeed) / 6;

                        ytdFielding = stats.fielding.daily[stats.fielding.daily.length - 1];

                        if (ytdFielding) {
                            stats.fielding.ytd.Pos = ytdFielding.Pos;
                            stats.fielding.ytd.G = ytdFielding.G;
                            stats.fielding.ytd.GS = ytdFielding.GS;
                            stats.fielding.ytd.INN = ytdFielding.INN;
                            stats.fielding.ytd.BallsInZone = ytdFielding.BallsInZone;
                            stats.fielding.ytd.MissedBallsInZone = ytdFielding.MissedBallsInZone;
                            stats.fielding.ytd.OutsOutOfZone = ytdFielding.OutsOutOfZone;
                            stats.fielding.ytd.CBlockingRuns = ytdFielding.CBlockingRuns;
                            stats.fielding.ytd.CFramingRuns = ytdFielding.CFramingRuns;
                            stats.fielding.ytd.Cera = ytdFielding.Cera;
                        }
                        */
        //                callback(players); 
        //            });
        //        });
        //    });
        //});
    }, 

    isTeamNL: function (team) {
        var nlTeams = ["CHC", "LAD", "NYM", "STL", "SF", "ATL", "PIT", "PHI", "WAS", "CIN", "MIL", "MIA", "SD", "COL", "ARI"];

        return nlTeams.indexOf(team) > -1;
    },

    getLineup: function( players ) {
        var lineup = [];

        return( players, lineup);
    }
};