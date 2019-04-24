var express = require('express');
var router = express.Router();

var utils = require('../helpers/utils'); 
var moment = require('moment');

var mongoose = require('mongoose'); 
var RosterLineup = mongoose.model('RosterLineup');

var playerHelper = require('../helpers/playerHelper');
var playerStatus = require('../helpers/playerStatus');

var rosterHelper = require('../helpers/rosterHelper');

module.exports = {

    // NOTE: This is only called for inactive players from the GameDay Wizard
    setInactiveEligibilty: function(inactiveData, nonRosterData, gameRange, leagueId, season, teamName, teamId, callback) {
        var context = this; 

        context.getData(inactiveData, gameRange, "ML", function(updatedInactive) {
            context.getData(updatedInactive, gameRange, "Minors", function(updatedInactive) {
                context.updateRoster(leagueId, teamId, teamName, updatedInactive, nonRosterData, function(err, result) {
                    callback(err, updatedInactive);
                });
            });
        }); 
    },

    updateRoster: function(leagueId, teamId, teamName, inactive, nonRoster, callback) {
        RosterLineup.update(
            { LeagueId: leagueId, TeamId: teamId },
            {
                LeagueId: leagueId,
                TeamdId: teamId,
                TeamName: teamName,
                InactivePlayers: inactive,
                NonRosterPlayers: nonRoster, 
            },
            { upsert: true }, 
            function (err, response) {
                if (err) {
                    callback(err, null)
                } else {
                    callback(null, response);
                }
            }
        );
    },

    getData: function(data, gameRange, type, callback) {
        var playerHelper = require('../helpers/playerHelper');
        playerHelper.getEligiblePositions(0, data, gameRange, function (output) {
         
            playerHelper.getAvailableBatting(0, output, type, gameRange, function (output) {

                playerHelper.getAvailablePitching(0, output, type, gameRange, function (output) {

                    playerHelper.getAvailableFielding(0, output, type, gameRange, function (output) {

                        playerHelper.getAvailableBaserunning(0, output, type, gameRange, function (output) {
                            callback(output); 
                        });
                    });
                });
            });
        });
    
    },
    
    // NOTE: This is only called for inactive players from the GameDay Wizard
    setNonRosterEligibility: function(inactiveData, nonRosterData, gameRange, leagueId, season, teamName, teamId, callback) {
        var context = this;
        
       
        context.getData(nonRosterData, gameRange, "ML", function(updatedNonRoster) {
            context.getData(updatedNonRoster, gameRange, "Minors", function(updatedNonRoster) {
                context.updateRoster(leagueId, teamId, teamName, inactiveData, updatedNonRoster, function(err, result) {
                    callback(err, result);
                });
            });
        }); 
    },

    setEligibilityForOnePlayer: function(playerData,leagueId, season, callback) {
        var context = this;
        var gameRange = { From: 20180329, To: 20170510 };

        var moment = require('moment-timezone');
        moment.tz.setDefault("America/Los_Angeles");
        var endDate = moment();
        endDate = endDate.subtract(1, "days");
        endDate = endDate.format("YYYYMMDD");
        gameRange.To = endDate;


       var playerArray = [ rosterHelper.createRosterLineupPlayer(leagueId, playerData, season) ];
        context.getData(playerArray, gameRange, "ML", function(updatedPlayerArray) {
            context.getData(updatedPlayerArray, gameRange, "Minors", function(updatedPlayerArray) {
                    callback(updatedPlayerArray[0]);
            });
        }); 
    },

}
