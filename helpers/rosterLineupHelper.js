var utils = require('../helpers/utils');
var request = require('request');
var cheerio = require('cheerio');

var mongoose = require('mongoose');
var Team = mongoose.model('Team');
var Roster = mongoose.model('Roster');
var Player = mongoose.model('Player');
var Log = mongoose.model('Log');
var RosterLineup = mongoose.model('RosterLineup');

var playerStatus = require('../helpers/playerStatus');
var gameHelper = require('../helpers/gameHelper');


var moment = require('moment-timezone');

module.exports = {

    // updates the Roster's depthChart
    // updates the RosterLineup's players list, nonRoster list, batting orders and bench
    // roster == Roster.FortyManNL
    // nonRoster == Roster.NonRoster
    // depthChart == Roster/RosterLineup.DepthChartNL
    // rosterLineup == RosterLineup.findOne()
    // NOTE, does not SAVE anything
    makeLegalDepthChart: function (roster, nonRoster, depthChart, rosterLineup) {

        // first get an array of all the player ids in the roster
        var active = [];
        var activesUsed = [];
        var activesMoved = [];
        var inactive = [];
        var nonrosterIds = [];
        for (var a = 0; a < roster.length; a++) {
            var player = roster[a];
            if (player.Status == playerStatus.ActiveRoster) {
                active.push(player.PlayerId);
            } else {
                inactive.push(player.PlayerId);
            }
        }

        for (var n = 0; n < nonRoster.length; n++) {
            nonrosterIds.push(nonRoster[n].PlayerId);
        }

        for (var d = 0; d < depthChart.length; d++) {
            for (var dp = depthChart[d].Players.length - 1; dp >= 0; dp--) {

                if (depthChart[d].Players[dp] && active.indexOf(depthChart[d].Players[dp].PlayerId) == -1) {
                    // this player is NOT in the active list.
                    // make a deep copy and move him either to the inactive list or to the non-roster list
                    var playerToMove = JSON.parse(JSON.stringify(depthChart[d].Players[dp]));

                    if (activesMoved.indexOf(playerToMove.PlayerId) == - 1) {
                        // then not moved yet
                        if (inactive.indexOf(playerToMove.PlayerId) >= 0) {
                            // then he is inactive.. move him to the inactive list
                            rosterLineup.InactivePlayers.unshift(playerToMove);

                        } else {
                            // then he is non-roster.. move him to the nonroster list
                            if (nonrosterIds.indexOf(playerToMove.PlayerId) >= 0)
                                rosterLineup.NonRosterPlayers.unshift(playerToMove);
                            else {
                                // this player is not on either active or nonroster list        
                            }
                        }

                        // now remove him from the current depth chart
                        depthChart[d].Players.splice(dp, 1);

                        // and remove from the players list
                        for (var p = 0; p < rosterLineup.Players.length; p++) {
                            if (rosterLineup.Players[p].PlayerId == playerToMove.PlayerId) {
                                rosterLineup.Players.splice(p, 1);
                                break;
                            }
                        }

                        activesMoved.push(playerToMove.PlayerId);
                    }
                } else {
                    // this player is active and in the depth chart...
                    if (depthChart[d].Players[dp])
                        activesUsed.push(depthChart[d].Players[dp].PlayerId);
                }
            }
        }

        // add in any active players that aren't yet in the depth chart...
        // if they were inactive, we would have found them, but not if they were on the non-roster list
        for (var p = 0; p < active.length; p++) {
            if (activesUsed.indexOf(active[p]) == -1) {
                // then this active player isn't in the depth chart
                var foundInNonRoster = false;
                for (var nr = 0; nr < rosterLineup.NonRosterPlayers.length; nr++) {
                    if (active[p] == rosterLineup.NonRosterPlayers[nr].PlayerId) {
                        foundInNonRoster = true;
                        var nrIndex = nr;
                        // then this active player is in the nonRosterPlayers list.. will be removed below
                        var playerToMove = JSON.parse(JSON.stringify(rosterLineup.NonRosterPlayers[nrIndex]));
                        if (playerToMove.Primary && playerToMove.Primary[0] == "P") {
                            depthChart[9].Players.push(playerToMove);
                        } else {
                            if (depthChart[12]) {
                                if (depthChart[12].Players)
                                    depthChart[12].Players.unshift(playerToMove);
                                else
                                    depthChart[12].Players = [playerToMove];
                            } else {
                                depthChart[12] = { Position: "Pinch Hitters", Pos: "PH", Players: [playerToMove] };
                            }
                        }
                        // now, move him from the nonroster list ot the players (active players) list
                        rosterLineup.Players.unshift(playerToMove);
                        rosterLineup.NonRosterPlayers.splice(nrIndex, 1);

                        break;
                    }
                }
                if( !foundInNonRoster ) {
                    // still not in depth chart.. see if in inactive list... then drop him in!
                    for (var ia = 0; ia < rosterLineup.InactivePlayers.length; ia++) {
                        if (active[p] == rosterLineup.InactivePlayers[ia].PlayerId) {
                            foundInNonRoster = true;
                            var iaIndex = ia;
                            // then this active player is in the nonRosterPlayers list.. will be removed below
                            var playerToMove = JSON.parse(JSON.stringify(rosterLineup.InactivePlayers[iaIndex]));
                            if (playerToMove.Primary && playerToMove.Primary[0] == "P") {
                                depthChart[9].Players.push(playerToMove);
                            } else {
                                if (depthChart[12]) {
                                    if (depthChart[12].Players)
                                        depthChart[12].Players.unshift(playerToMove);
                                    else
                                        depthChart[12].Players = [playerToMove];
                                } else {
                                    depthChart[12] = { Position: "Pinch Hitters", Pos: "PH", Players: [playerToMove] };
                                }
                            }
                            // now, move him from the inactive list ot the players (active players) list
                            rosterLineup.Players.unshift(playerToMove);
                            rosterLineup.InactivePlayers.splice(nrIndex, 1);
                            break;
                        }
                    }
                }
            }
        }

        // clean up the Players list
        var rlNonRosterIds = [];
        for (var p = rosterLineup.Players.length - 1; p >= 0; p--) {
            if (rosterLineup.Players[p] && active.indexOf(rosterLineup.Players[p].PlayerId) == -1) {
                // then found a player in the players list who's not on the actives (i.e. FortyManNL ) list
                // move him to the nonRoster list
                if (rlNonRosterIds.indexOf(rosterLineup.Players[p].PlayerId) == -1) {
                    // only put the player in once.
                    rosterLineup.NonRosterPlayers.unshift(rosterLineup.Players[p]);
                    rlNonRosterIds.push(rosterLineup.Players[p].PlayerId);
                }
                rosterLineup.Players.splice(p, 1);
            }
        }

        // clean up the NonActivePlayers list
        for (var p = rosterLineup.InactivePlayers.length - 1; p >= 0; p--) {
            if (rosterLineup.InactivePlayers[p] && active.indexOf(rosterLineup.InactivePlayers[p].PlayerId) == -1) {
                // then found a player who's not on the actives (i.e. FortyManNL ) list
                if (inactive.indexOf(rosterLineup.InactivePlayers[p].PlayerId) == -1) {
                    // then found a player who wasn't already on the inactive list
                    rosterLineup.InactivePlayers.splice(p, 1);
                }

            }
        }
        // Players and InactivePlayers lists cleaned up.

        // clean out lineups
        for (var b = 0; b < rosterLineup.BattingOrderNL.length; b++) {
            if (rosterLineup.BattingOrderNL[b] && rosterLineup.BattingOrderNL[b].PlayerId && active.indexOf(rosterLineup.BattingOrderNL[b].PlayerId) == -1) {
                // then remove this player from the batting order
                rosterLineup.BattingOrderNL[b] = [rosterLineup.BattingOrderNL[b].Position, -1];
            }
        }

        for (var b = 0; b < rosterLineup.BattingOrderAL.length; b++) {
            if (rosterLineup.BattingOrderAL[b] && rosterLineup.BattingOrderAL[b].PlayerId && active.indexOf(rosterLineup.BattingOrderAL[b].PlayerId) == -1) {
                // then remove this player from the batting order
                rosterLineup.BattingOrderAL[b] = [rosterLineup.BattingOrderAL[b].Position, -1];
            }
        }

        // clean out the bench
        for (var b = rosterLineup.Bench.length - 1; b >= 0; b--) {
            if (rosterLineup.Bench[b] && active.indexOf(rosterLineup.Bench[b].PlayerId) == -1) {
                rosterLineup.Bench.splice(b, 1);
            }
        }

        // finally, clean up the NonActivePlayers list
        for (var p = rosterLineup.NonRosterPlayers.length - 1; p >= 0; p--) {
            if (rosterLineup.NonRosterPlayers[p] && nonrosterIds.indexOf(rosterLineup.NonRosterPlayers[p].PlayerId) == -1) {
                // then found a player who's not on the nonRoster list
                rosterLineup.NonRosterPlayers.splice(p, 1);
            }
        }
    },

    makeLegalDepthChartAndMarkModified: function (roster, rosterLineup) {
        this.makeLegalDepthChart(roster.FortyManNL, roster.NonRoster, rosterLineup.DepthChartNL, rosterLineup);

        // now have the roster updated... update the rosterLineup to match
        roster.DepthChartNL = rosterLineup.DepthChartNL;
        roster.BattingOrderNL = rosterLineup.BattingOrderNL;
        roster.BattingOrderAL = rosterLineup.BattingOrderAL;
        roster.Bench = rosterLineup.Bench;

        rosterLineup.markModified('DepthChartNL');
        rosterLineup.markModified('BattingOrderNL');
        rosterLineup.markModified('BattingOrderAL');
        rosterLineup.markModified('Bench');
        rosterLineup.markModified('Players');
        rosterLineup.markModified('NonRosterPlayers');

        roster.markModified('DepthChartNL');
        roster.markModified('BattingOrderNL');
        roster.markModified('BattingOrderAL');
        roster.markModified('Bench');
    },


    updateDepthChartsFromRoster(roster, newPlayer, saveRoster, callback) {
        var leagueId = roster.LeagueId;
        var teamId = roster.TeamId;
        var context = this;
        RosterLineup.findOne({ LeagueId: leagueId, TeamId: teamId }, function (err, rosterLineup) {
            if (err) {
                callback(err, null);
                return;
            }
            if( newPlayer ) {
                // then put him in the roster lineup's Players list..
                // note this player is not just a roster player, but has been set up to be a rosterLineup player
                // created with rosterHelper.createRosterLineupPlayer()
                rosterLineup.Players.unshift(newPlayer);
            }
            context.makeLegalDepthChartAndMarkModified(roster, rosterLineup);

            rosterLineup.save(function (err, response) {
                if (saveRoster) {
                    roster.save(function (err, response) {
                        callback(err, response);
                    })
                } else {
                    callback(err, response);
                }
            })
        })
    },

    removePLayerFromRosterLineup(roster, oldPlayer, saveRoster, callback) {
        var leagueId = roster.LeagueId;
        var teamId = roster.TeamId;
        var context = this;
        var playerEStats = null;
        RosterLineup.findOne({ LeagueId: leagueId, TeamId: teamId }, function (err, rosterLineup) {
            if (err) {
                callback(err, null);
                return;
            }
            if( oldPlayer ) {
                var playerId = oldPlayer.PlayerId;

                // remove all versions of this player that you can find...
                for( var p= rosterLineup.Players.length-1; p>=0; p-- ) {
                    if( rosterLineup.Players[p].PlayerId == playerId ) {
                        playerEStats = rosterLineup.Players[p];
                        rosterLineup.Players.splice(p,1);
                    }
                }
                for( var p= rosterLineup.InactivePlayers.length-1; p>=0; p-- ) {
                    if( rosterLineup.InactivePlayers[p].PlayerId == playerId ) {
                        playerEStats = rosterLineup.InactivePlayers[p];
                        rosterLineup.InactivePlayers.splice(p,1);
                    }
                }
                for( var p= rosterLineup.NonRosterPlayers.length-1; p>=0; p-- ) {
                    if( rosterLineup.NonRosterPlayers[p].PlayerId == playerId ) {
                        playerEStats = rosterLineup.NonRosterPlayers[p];
                        rosterLineup.NonRosterPlayers.splice(p,1);
                    }
                }

            }
            context.makeLegalDepthChartAndMarkModified(roster, rosterLineup);

            rosterLineup.save(function (err, playerEStats) {
                if (saveRoster) {
                    roster.save(function (err, playerEStats) {
                        callback(err, response);
                    })
                } else {
                    callback(err, playerEStats);
                }
            })
        })
    },

    addPlayerToRosterLineup(newPlayer, roster, saveRoster, callback) {
        var context = this;

        // to simplify other code... if no new player, just update the depth chart without adding a player here
        if (newPlayer) {
            gameHelper.setEligibilityForOnePlayer(newPlayer, roster.LeagueId, 2018, function (playerWithEStats) {
                if (!playerWithEStats) {
                    callback(err, null);
                } else {
                    context.updateDepthChartsFromRoster(roster, playerWithEStats, saveRoster, function (err, response) {
                        callback(err, response);
                    })
                }
            })
        } else {
            context.updateDepthChartsFromRoster(roster, null, saveRoster, function (err, response) {
                callback(err, response);
            })
        }
    }

}