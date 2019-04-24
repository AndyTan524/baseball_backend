//import { O_APPEND } from 'constants';

var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var playerHelper = require('../helpers/playerHelper');
var teamHelper = require('../helpers/teamHelper');
var rosterHelper = require('../helpers/rosterHelper');
var rosterLineupHelper = require('../helpers/rosterLineupHelper');
var sendGridHelper = require('../helpers/sendGridHelper');
var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var Roster = mongoose.model('Roster');
var Team = mongoose.model('Team');
var League = mongoose.model('League');
var Player = mongoose.model('Player');
var User = mongoose.model('User');
var YtdStats = mongoose.model('YtdStats');
var RosterLineup = mongoose.model('RosterLineup');
var Roster = mongoose.model('Roster');
var MasterPlayer = mongoose.model('MasterPlayer');

var playerStatus = require('../helpers/playerStatus');
var teamInfo = require('../helpers/teamInfo');

router.post('/api/teams/owners/contacts', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid && req.body.owners) {
            var owners = req.body.owners;
            var ownerId = [];
            for (i = 0; i < owners.length; i++) {
                nextOwner = owners[i];
                ownerId.push(nextOwner.UserId);
            }
            User.find({ _id: { $in: ownerId } }, function (err, ownerlist) {
                if (err || ownerlist.length == 0) {
                    res.status(401).json({ status: 401, msg: "No owners found", owners: [] });
                } else {
                    res.status(200).json({ status: 200, msg: "Found owners", owners: ownerlist });
                }
            });
        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});


// get the depth chart ONLY, but be sure everyone's accounted for.

router.get('/api/teams/roster/depth-chart', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };

            var qry = { LeagueId: req.query.lid, TeamId: req.query.tid };
            RosterLineup.findOne({ LeagueId: req.query.lid, TeamId: req.query.tid }, function (err, rosterLineup) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg, isValid: false });
                } else {
                    if (!rosterLineup) {
                        rosterLineup = {
                            Players: []
                        }
                        res.status(500).json({ status: 500, msg: "no rosterLineup", isValid: false });
                    } else {
                        
                        Roster.findOne(query, function (err, teamRoster ) {
                            if (err) {
                           res.status(500).json({ status: 500, msg: err.errmsg });
                            } else if (teamRoster && (!req.query.refresh || req.query.refresh != "true")) {
                               // rosterLineupHelper.makeLegalDepthChart(teamRoster.FortyManNL, teamRoster.NonRoster, teamRoster.DepthChartNL, rosterLineup);
                                rosterHelper.putInDisplayStats( rosterLineup.Players );
                                 // insure the depth chart is up to date here...
                                 
                                 var accountedFor = [];

                                for (var d = 0; d < teamRoster.DepthChartNL.length; d++) {
                                    for (var p = teamRoster.DepthChartNL[d].Players.length - 1; p >= 0; p--) {
                                        if (!teamRoster.DepthChartNL[d].Players[p]) {
                                            teamRoster.DepthChartNL[d].Players.splice(p, 1);
                                        } else {
                                            var found = false;
                                            for (var r = 0; r < rosterLineup.Players.length; r++) {
                                                if (rosterLineup.Players[r].PlayerId == teamRoster.DepthChartNL[d].Players[p].PlayerId) {
                                                    // update stats and note the player is accounted for
                                                    teamRoster.DepthChartNL[d].Players[p] = rosterLineup.Players[r];
                                                    if( accountedFor.indexOf(rosterLineup.Players[r].PlayerId) == -1 ) {
                                                        accountedFor.push(rosterLineup.Players[r].PlayerId);
                                                        found = true;
                                                    }
                                                    break;
                                                }
                                            }
                                            if (!found) {
                                                // remove him.
                                                teamRoster.DepthChartNL[d].Players.splice(p, 1);
                                            }
                                        }
                                    }
                                }
                                 if( accountedFor.length > 0 ) {
                                    for( var r=0; r<rosterLineup.Players.length; r++ ) {
                                        if( accountedFor.indexOf(rosterLineup.Players[r].PlayerId == -1) ) {
                                            if( !teamRoster.DepthChartNL[12]) {
                                                teamRoster.DepthChartNL[12] = {Position: "Unassigned", Pos: "UA", Players:[]};
                                            }
                                            teamRoster.DepthChartNL[12].Players.push(rosterLineup.Players[r]);
                                        }
                                    }
                                 }

                                 // finally, double check that the pitchers on the Players list are pitchers.

                                 for( var p=0; p<rosterLineup.Players.length; p++) {
                                     if( !rosterLineup.Players[p].Primary ||  rosterLineup.Players[p].Primary.length==0) {
                                         // then check if this one's a pitcher
                                         if(  rosterLineup.Players[p].highestIP || rosterLineup.Players[p].highestIPMinors ) {
                                            rosterLineup.Players[p]["Primary"] = ["P"];
                                         }
                                     }
                                 }

                                res.status(200).json({ status: 200, msg: "success", depth: teamRoster.DepthChartNL, roster: rosterLineup.Players });
                               
                                // now save everything back to the db
                                rosterLineup.BattingOrderAL = teamRoster.BattingOrderAL;
                                rosterLineup.BattingOrderNL = teamRoster.BattingOrderNL;
                                rosterLineup.markModified("BattingOrderAL");
                                rosterLineup.markModified("BattingOrderNL");
                                rosterLineup.markModified("Players");
                                rosterLineup.markModified("InactivePlayers");
                                rosterLineup.markModified("NonRosterPlayers");
                                rosterLineup.save(function (err, resonse) {

                                    teamRoster.markModified("BattingOrderAL");
                                    teamRoster.markModified("BattingOrderNL");
                                    teamRoster.markModified("DepthChartNL");
                                    teamRoster.save(function (err, resonse) {

                                    })

                                })
   
                            
                            
                            }
                            else {

                                // need to create a roster from the rosterLineup players
                                rosterHelper.getMasterPlayerPosition(0, rosterLineup, function (roster) {
                                    var depth = rosterHelper.getDepthChartTemplate();
                                    depth[0].Players = rosterHelper.getPlayersByPosition("CA", roster.Players);
                                    depth[1].Players = rosterHelper.getPlayersByPosition("1B", roster.Players);
                                    depth[2].Players = rosterHelper.getPlayersByPosition("2B", roster.Players);
                                    depth[3].Players = rosterHelper.getPlayersByPosition("3B", roster.Players);
                                    depth[4].Players = rosterHelper.getPlayersByPosition("SS", roster.Players);
                                    depth[5].Players = rosterHelper.getPlayersByPosition("LF", roster.Players);
                                    depth[6].Players = rosterHelper.getPlayersByPosition("CF", roster.Players);
                                    depth[7].Players = rosterHelper.getPlayersByPosition("RF", roster.Players);
                                    depth[8].Players = rosterHelper.getPlayersByPosition("P", roster.Players);
                                    depth[9].Players = depth[8].Players.length > 5 ? depth[8].Players.splice(4) : [];
                                    depth[10].Players = depth[8].Players.length > 0 ? depth[8].Players.splice(0, 1) : [];
                                    depth[8].Players.length > 5 ? depth[8].Players.splice(0, 4) : depth[8].Players;

                                    depth[11].Players = rosterHelper.getPlayersByPosition("DH", roster.Players);

                                    if (teamRoster.DepthChartNL == null) {
                                        teamRoster.DepthChartNL = depth;
                                        teamRoster.markModified("DepthChartNL");
                                        teamRoster.save(function (err, result) {

                                        });
                                    }
                                    res.status(200).json({ status: 200, msg: "success", depth: depth, roster: rosterLineup.Players });

                                });
                            }
                        });
                        
                    }

                }
            });


        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.get('/api/teams/roster', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (roster) {
                    var output = {}
                    res.status(200).json({ status: 200, msg: "success", roster: roster });
                    /*
                    playerHelper.getPlayersByIds(roster.FortyMan, function (result) {
                        for (var i = 0; i < result.length; i++) {
                            if (!result[i].Salary) {
                                result[i].Salary = {
                                    Salary: "0"
                                };
                            }
                            result[i].Salary.SalaryNumber = Number(result[i].Salary.Salary.replace(/[^0-9\.]+/g, ""));
                        }
                        result.sort(function (a, b) {
                            return b.Salary.SalaryNumber - a.Salary.SalaryNumber;
                        });
                        output.FortyMan = result;
                        playerHelper.getPlayersByIds(roster.Bench, function (result) {
                            for (var i = 0; i < result.length; i++) {
                                if (!result[i].Salary) {
                                    result[i].Salary = {
                                        Salary: "0"
                                    };
                                }
                                result[i].Salary.SalaryNumber = Number(result[i].Salary.Salary.replace(/[^0-9\.]+/g, ""));
                            }
                            result.sort(function (a, b) {
                                return b.Salary.SalaryNumber - a.Salary.SalaryNumber;
                            });
                            output.Bench = result;
                            playerHelper.getPlayersByIds(roster.StartingLineup, function (result) {
                                for (var i = 0; i < result.length; i++) {
                                    if (!result[i].Salary) {
                                        result[i].Salary = {
                                            Salary: "0"
                                        };
                                    }
                                    result[i].Salary.SalaryNumber = Number(result[i].Salary.Salary.replace(/[^0-9\.]+/g, ""));
                                }
                                result.sort(function (a, b) {
                                    return b.Salary.SalaryNumber - a.Salary.SalaryNumber;
                                });
                                output.StartingLineup = result;
                                output.Bench = result;
                                playerHelper.getPlayersByIds(roster.StartingRotation, function (result) {
                                    for (var i = 0; i < result.length; i++) {
                                        if (!result[i].Salary) {
                                            result[i].Salary = {
                                                Salary: "0"
                                            };
                                        }
                                        result[i].Salary.SalaryNumber = Number(result[i].Salary.Salary.replace(/[^0-9\.]+/g, ""));
                                    }
                                    result.sort(function (a, b) {
                                        return b.Salary.SalaryNumber - a.Salary.SalaryNumber;
                                    });
                                    output.StartingRotation = result;
                                    playerHelper.getPlayersByIds(roster.Bullpen, function (result) {
                                        for (var i = 0; i < result.length; i++) {
                                            if (!result[i].Salary) {
                                                result[i].Salary = {
                                                    Salary: "0"
                                                };
                                            }
                                            result[i].Salary.SalaryNumber = Number(result[i].Salary.Salary.replace(/[^0-9\.]+/g, ""));
                                        }
                                        result.sort(function (a, b) {
                                            return b.Salary.SalaryNumber - a.Salary.SalaryNumber;
                                        });
                                        output.Bullpen = result;
                                        playerHelper.getPlayersByIds(roster.WaiverWire, function (result) {
                                            for (var i = 0; i < result.length; i++) {
                                                if (!result[i].Salary) {
                                                    result[i].Salary = {
                                                        Salary: "0"
                                                    };
                                                }
                                                result[i].Salary.SalaryNumber = Number(result[i].Salary.Salary.replace(/[^0-9\.]+/g, ""));
                                            }
                                            result.sort(function (a, b) {
                                                return b.Salary.SalaryNumber - a.Salary.SalaryNumber;
                                            });
                                            output.WaiverWire = result;
                                            res.status(200).json({ status: 200, msg: "success", roster: output });
                                        }); 
                                    }); 
                                }); 
                            }); 
                        }); 
                    });
                    */
                }
                else {
                    res.status(200).json({ status: 200, msg: "success", roster: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ***********************************************************************
//
// GET/ALLOWNED
//    returns the active roster, nonroster, dl, other lists, and waived players
//    this should be EVERY player in the franchise
//    ALSO: does clean up work on missing data such as the rSalaries, status, etc.
//
// ***********************************************************************

/* ******************* NOTES ON ACTIVE ROSTER VS. 40-MAN ROSTER **********
They are not the same during the regular season. 
Active players are your 25 players that are eligible to play in MLB games. 
40-man roster can also include inactive players not on the 25-man. 
For instance, DL players or players on optional assignment.

So, from here on out when I say active, 
I’ll be referring to the 25 eligible to play in games. 
When I refer to 40-man, I’ll refer to all the players 
that also include the up to 15 inactive guys. 
Then, everyone else is just non-roster.

***********************************************************************/
router.get('/api/teams/allowned', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var moment = require('moment-timezone'); // for age calculations
            moment.tz.setDefault("America/Los_Angeles");

            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (roster) {
                    // we'll be checking if there are missing elements or
                    // players in the wrong places.  if so, we'll need to 
                    // update AFTER we send the results back to the caller
                    var updateNeeded = false;
                    var activeCount = 0;
                    var fortyManCount = 0;

                    // create list of roster players for coming compare
                    var fortyman = roster.FortyManNL;
                    var otherRoster = roster.FortyManAL;
                    if (req.query.league && req.query.league == "al") {
                        fortyman = roster.FortyManAL;
                        otherRoster = foster.FortyManNL;
                    }

                    // use these for cleaning up after the results are sent.
                    var nonRosterPlayers = [];
                    var tradeWaiverPlayers = [];
                    var dlIndex = [];
                    var dlPlayers = [];
                    var olIndex = [];
                    var olPlayers = [];
                    var wIndex = [];
                    var wPlayers = [];
                    var mdl = [];       // minor league dl

                    // ************ READ THROUGH THE ACTIVE-ROSTER 
                    // NOTE: these players COULD be on the DL, or OtherLists
                    var rosterIndex = [];

                    for (p = 0; p < fortyman.length; p++) {
                        var rosterPlayer = fortyman[p]
                        rosterIndex[p] = rosterPlayer.PlayerId;
                        if (!rosterPlayer.Image || rosterPlayer.Image == "") {
                            rosterPlayer["Image"] = "/assets/img/avatars/avatar1.jpg";
                        }

                        if (!rosterPlayer.Status) {
                            rosterPlayer["Status"] = playerStatus.ActiveRoster;
                        } else {
                            // let's look at his status and put him in the lists for the front end display
                            var hisStatus = rosterPlayer.Status;
                            if (hisStatus >= playerStatus.DL && hisStatus < playerStatus.DLEnd) {
                                dlPlayers.push(rosterPlayer);
                                dlIndex.push(rosterPlayer.PlayerId);

                            } else if (hisStatus >= playerStatus.LeaveLists && hisStatus < playerStatus.LeaveEnd) {
                                olPlayers.push(rosterPlayer);
                                olIndex.push(rosterPlayer.PlayerId);

                            } else if (hisStatus >= playerStatus.Waivers && hisStatus < playerStatus.WaiversEnd) {
                                wPlayers.push(rosterPlayer);
                                wIndex.push(rosterPlayer.PlayerId);

                            } else if (hisStatus >= playerStatus.TradeWaivers && hisStatus < playerStatus.TradeWaiversEnd) {
                                tradeWaiverPlayers.push(rosterPlayer);
                            }
                        }

                        if (!rosterPlayer.Level) {
                            rosterPlayer["Level"] = "ML";
                        }

                        if (rosterPlayer["TradeStatus"]) {
                            if (rosterPlayer.TradeStatus >= playerStatus.RequestTradingBlock) {
                                tradeWaiverPlayers.push(rosterPlayer);
                            }
                        }
                        // (((((((((((((((  TODO: make this work for everyone )))))))))))))))
                        rosterPlayer["StatusMsg"] = playerHelper.getPlayerStatusMsg(rosterPlayer);
                        if (rosterPlayer.Status == playerStatus.ActiveRoster)
                            activeCount++;
                    }


                    var OwnedPlayers = {
                        roster: fortyman,
                        dl: dlPlayers,
                        otherLists: olPlayers,
                        waivers: wPlayers,
                        nonroster: {
                            "Triple-A": [],
                            "Double-A": [],
                            "High-A": [],
                            "Low-A": [],
                            "ADV-RK": [],
                            "RK": [],
                            "EST": [],
                            "DSL": [],
                            "MDL": [],
                            "Unknown": []
                        },
                        levels: {
                            "Triple-A": { affiliates: [], max: 35, size: 0 },
                            "Double-A": { affiliates: [], max: 35, size: 0 },
                            "High-A": { affiliates: [], max: 35, size: 0 },
                            "Low-A": { affiliates: [], max: 35, size: 0 },
                            "ADV-RK": { affiliates: [], max: 35, size: 0 },
                            "RK": { affiliates: [], max: 35, size: 0 },
                            "EST": { affiliates: [], max: 35, size: 0 },
                            "DSL": { affiliates: [], max: 35, size: 0 },
                            "MDL": { affiliates: [], max: 35, size: 0 },
                            "Unknown": { affiliates: [], max: 1000, size: 0 },
                            "Active": { max: 25, size: activeCount },
                            "40-Man": { max: 40, size: 0 }
                        },
                        affiliates: [],
                        playerStatus: playerStatus,
                        tradewaivers: []
                    };


                    OwnedPlayers.rosterIndex = rosterIndex;

                    // put all the players into one array
                    // makes this code backwards compatible
                    owned = fortyman;
                    owned = owned.concat(roster.NonRoster);
                    // *****************************************************************
                    //
                    // read through all the owned players... put them in their levels
                    //
                    // *****************************************************************
                    for (op = 0; op < owned.length; op++) {

                        var currentStatus = playerStatus.Inactive;
                        if (owned[op].Status) {
                            currentStatus = owned[op].Status;
                        } else {
                            if (owned[op]._doc) {
                                owned[op]._doc["Status"] = currentStatus;
                            } else {
                                owned[op]["Status"] = currentStatus;
                            }
                        }
                        if (owned[op].onFortyMan) {
                            OwnedPlayers.levels["40-Man"].size++;
                        }

                        // see if in a group: ActiveRoster, DL, Other Lists, Waivers

                        // insure they have an updated age.
                        var dob = owned[op].DOB
                        if (dob != "") {
                            var isoDate = new Date(dob).toISOString();
                            var d = moment(isoDate);
                            var age = moment().diff(d, 'years', true);
                            owned[op]['Age'] = Math.floor(age);
                        }

                        var ri = rosterIndex.indexOf(owned[op].PlayerId);
                        if (ri == -1) {
                            // not in the roster.. on the dl?
                            ri = dlIndex.indexOf(owned[op].PlayerId);

                            if (ri == -1) {
                                // not on Dl, on another list?
                                ri = olIndex.indexOf(owned[op].PlayerId);

                                if (ri == -1) {
                                    // not on another list, on waivers?
                                    ri = wIndex.indexOf(owned[op].PlayerId);
                                    if (ri != -1) {
                                        // on waiver lists
                                        if (currentStatus == playerStatus.Inactive)
                                            currentStatus = playerStatus.Waivers;
                                    }
                                } else {
                                    // on another list
                                    if (currentStatus == playerStatus.Inactive)
                                        currentStatus = playerStatus.LeaveLists;
                                }
                            } else {
                                // on DL
                                if (currentStatus == playerStatus.Inactive)
                                    currentStatus = playerStatus.DL;
                            }
                        } else {
                            // player is on active roster
                            if (currentStatus == playerStatus.Inactive)
                                currentStatus = playerStatus.ActiveRoster;
                        }

                        // NOW: for ALL PLAYERS, insure there's a first name
                        var firstname = "";
                        var firstinintial = "";
                        var lastname = "";
                        if (owned[op].FirstName) {
                            firstname = owned[op].FirstName;
                            lastname = owned[op].LastName;
                        } else {
                            // try to create one
                            if (owned[op].FullName) {
                                namearray = owned[op].FullName.split(" ");
                                firstname = namearray[0];
                                if (owned[op]._doc) {
                                    owned[op]._doc["FirstName"] = firstname;
                                } else {
                                    owned[op]["FirstName"] = firstname;
                                }


                                if (namearray.length == 2) {
                                    // easy
                                    lastname = namearray[1];
                                } else {
                                    // more than 2 names! assume space in last name
                                    lastname = namearray[1] + " " + namearray[2];
                                }
                                if (owned[op]._doc) {
                                    owned[op]._doc["LastName"] = lastname;
                                } else {
                                    owned[op]["LastName"] = lastname;
                                }
                            }
                        }

                        // create first initial
                        // create 1st initial
                        if (!owned[op].FirstInitial) {
                            if (firstname.length <= 2) {
                                firstinintial = firstname;
                            } else {
                                firstinintial = firstname.slice(0, 1);
                            }
                            owned[op]["FirstInitial"] = firstinintial;
                        }


                        // check for image
                        if (!owned[op].Image || owned[op].Image == "") {
                            owned[op]._doc["Image"] = "/assets/img/avatars/avatar1.jpg";
                        }

                        // check for active status
                        if (!owned[op].onFortyMan) {
                            owned[op]["onFortyMan"] = false;
                        }

                        if (!owned[op].TradeStatus) {
                            owned[op]["TradeStatus"] = false;
                        }

                        // *******************************************************
                        //
                        // ok.. players ready to put into their correct lists!
                        //
                        // *******************************************************
                        if (ri == -1) {

                            // *********************** NOT ACTIVE ROSTER (ACTIVE) PLAYER ********************
                            // *********************** NOT on DL, OTHER LISTS, or WAIVERS *******************

                            // *********************** FIX AAA error in Crunchbase *********************
                            if (owned[op].Level == "AAA")
                                owned[op].Level = "Triple-A";

                            if (!owned[op].Status) {
                                owned[op]._doc["Status"] = playerStatus.Inactive;
                            }
                            var nextLevel = owned[op].Level;

                            if (!(nextLevel in OwnedPlayers.levels)) {
                                // don't have a legel level, throw into the Rookie league
                                nextLevel = "RK";
                            }
                            OwnedPlayers.levels[nextLevel].size++;
                            hisStatus = owned[op].Status;
                            if (hisStatus == playerStatus.DLMinors) {
                                mdl.push(owned[op]);
                            } else if (hisStatus >= playerStatus.DL && hisStatus < playerStatus.DLEnd) {
                                dlPlayers.push(owned[op]);
                                dlIndex.push(owned[op].PlayerId);
                            } else if (hisStatus >= playerStatus.LeaveLists && hisStatus < playerStatus.LeaveEnd) {
                                olPlayers.push(owned[op]);
                                olIndex.push(owned[op].PlayerId);
                            } else if (hisStatus >= playerStatus.Waivers && hisStatus < playerStatus.WaiversEnd) {
                                wPlayers.push(owned[op]);
                                wIndex.push(owned[op].PlayerId);
                            } 
                            /* no longer keep tradwaivers players off the roster
                              else if (owned[op].TradeStatus && owned[op].TradeStatus >= playerStatus.TradeWaivers && owned[op].TradeStatus < playerStatus.TradeWaiversEnd) {
                                tradeWaiverPlayers.push(owned[op]);
                              }
                                */
                               else {
                                /*
                                if (OwnedPlayers.levels.indexOf(nextLevel) == -1) {

                                    // add the new level to the list
                                    OwnedPlayers.levels.push(nextLevel);
                                    OwnedPlayers.nonroster.push(new Array());

                                    nextLevelIndex = OwnedPlayers.levels.indexOf(nextLevel);

                                    OwnedPlayers.nonroster[nextLevelIndex].push(owned[op]);

                                    // *** drop in the affiliated team name
                                    if (nextLevel == "ML") {
                                        OwnedPlayers.affiliates.push(" ");
                                    } else {
                                        // OwnedPlayers.affiliates.push(owned[op].MilbAffiliate);
                                        OwnedPlayers.affiliates.push(nextLevel);
                                    }

                                } else {
                                    nextLevelIndex = OwnedPlayers.levels.indexOf(nextLevel);

                                    OwnedPlayers.nonroster[nextLevelIndex].push(owned[op]);
                                }
                                */
                                // add the player to the right nonRoster list
                                if (OwnedPlayers.nonroster[nextLevel]) {
                                    OwnedPlayers.nonroster[nextLevel].push(owned[op]);
                                } else {
                                    OwnedPlayers.nonroster["Unknown"].push(owned[op]);
                                }
                            }

                            owned[op]["StatusMsg"] = playerHelper.getPlayerStatusMsg(owned[op]);

                            if (owned[op].TradeStatus) {
                                if (owned[op].TradeStatus >= playerStatus.RequestTradingBlock) {
                                    tradeWaiverPlayers.push(owned[op]);
                                }
                            }

                            // just for clean up, put the player in the nonRosterPlayers list
                            nonRosterPlayers.push(owned[op]);
                        } else {
                            // *********************** SHOULD BE ON ACTIVE ROSTER (ACTIVE) PLAYER ********************
                            // *********************** OR on DL, OTHER LISTS, or WAIVERS *******************

                            var player;
                            if (currentStatus == playerStatus.ActiveRoster) {
                                player = OwnedPlayers.roster[ri];
                                if (player.onFortyMan == false) {
                                    // player.onFortyMan = true;
                                    OwnedPlayers.levels["40-Man"].size++;
                                }
                            } else if (currentStatus >= playerStatus.Waivers && currentStatus < playerStatus.WaiversEnd) {
                                player = OwnedPlayers.waivers[ri];
                            } else if (currentStatus >= playerStatus.LeaveLists && currentStatus < playerStatus.LeaveEnd) {
                                player = OwnedPlayers.otherLists[ri];
                            } else if (currentStatus >= playerStatus.DL && currentStatus < playerStatus.DLEnd) {
                                player = OwnedPlayers.dl[ri];
                            }
                        }
                    }

                    // add in the MDL (minor league dl) into the minor leage spot
                    /*
                    OwnedPlayers.levels.push("MDL");
                    OwnedPlayers.affiliates.push("Minor League DL");
                    */
                    OwnedPlayers.nonroster["MDL"] = mdl;


                    // add in the tradewaiver players
                    OwnedPlayers.tradewaivers = tradeWaiverPlayers;


                    // add in the nonActive players on the 40Man roster
                    OwnedPlayers.fortyManCount += fortyManCount;

                    // **************** finally, attempt to clean up the non-roster ML players ************


                    res.status(200).json({ status: 200, OwnedPlayers });

                    // ********** post response clean up
                    /*
                    var rosterSchema = new mongoose.Schema({
                                    LeagueId: String,
                                     TeamId: String,
                                     TeamAbbr: String,
                                    FortyManAL: Object,
                                    FortyManNL: Object,
                                    NonRoster: Object,
                                    CreatedUTC: String
                    });
                    */

                    // NONLONGER NEED TO SAVE THE FULL ROSTER!
                    /*
                    if (nonRosterPlayers.length > 0) {

                        roster['NonRoster'] = nonRosterPlayers;

                        var league = "NL";
                        if (req.query.league && req.query.league == "al") {
                            league = "AL";
                        }
                        for (i = 0; i < fortyman.length; i++) {

                            var mlbId = fortyman[i].MlbId;
                            var orIndex = -1;
                            for (or = 0; or < otherRoster.length; or++) {
                                if (otherRoster[or].MlbId == mlbId) {
                                    // found the matching player
                                    orIndex = or;
                                    break;
                                }
                            }

                            if (orIndex >= 0) {
                                // grab the stuff that isn't the same!
                                var batting = -1;
                                if (otherRoster.BattingOrder) {
                                    batting = otherROster.Batting;
                                }
                                var bench = -1;
                                if (otherRoster.BenchOrder) {
                                    bench = otherROster.BenchOrder;
                                }
                                var rd = "";
                                if (otherRoster.Rdepth) {
                                    rd = otherROster.Rdepth;
                                }

                                // ok, duplicate from one league to the other!
                                otherRoster[orIndex] = fortyman[i];
                                otherRoster[orIndex]["BattingOrder"] = batting;
                                otherRoster[orIndex]["BenchOrder"] = bench;
                                otherRoster[orIndex]["Rdepth"] = rd;
                            }
                        }
                        // have copied all players from fortyman to otherRoster
                        if (league == "NL") {
                            roster.FortyManAL = otherRoster;
                            roster.FortyManNL = fortyman;
                        } else {
                            // al
                            roster.FortyManNL = otherRoster;
                            roster.FortyManAL = fortyman;
                        }

                        roster.markModified("FortyManNL");
                        roster.markModified("FortyManAL");
                        roster.markModified("NonRoster");

                        // save the updated data to the db!
                        roster.save(function (err, response) {
                            if (err) {
                                console.log(err.message);
                            }
                            console.log("Roster saved ");
                        });
                    }
                    */

                }
                else {
                    res.status(200).json({ status: 200, msg: "no roster" });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.get('/api/teams/roster/getStatsForArb', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            Roster.findOne(query, { FortyManNL: 1 }, function (err, roster) {
                if( err || !roster || !! roster.FortyManNL) {
                    res.status(500).json({ status: 500, msg: "error in arb stats" });
                } else {

                var playerList = [];
                for (i = 0; i < roster.FortyManNL.length; i++) {
                    playerList.push(roster.FortyManNL[i].FullName);
                }
                var query = [
                    { $match: { Name: { $in: playerList } } },
                    { $sort: { _id: 1, "RP.IP": 1, "SP.IP": 1, Name: 1 } },
                    {
                        $group:
                        {
                            _id: "$Name",
                            rG: { $push: "$RP.G" },
                            sG: { $push: "$SP.GS" },
                            rIP: { $push: "$RP.IP" },
                            sIP: { $push: "$SP.IP" },
                            PA: { $push: "$Batter.PA" },
                            position: { $push: "$POS" },
                            playerId: { $push: "$_id" }
                        }
                    }
                ];

                YtdStats.aggregate(query, function (err, stats) {
                    if (err) {
                        res.status(500).json({ status: 500, msg: "can't read ytdstats in arb stats" });
                    } else {
                        res.status(200).json({ status: 200, msg: "success", stats: stats });
                    }
                });
            }
            });
        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.get('/api/teams/roster/forty', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (roster) {
                    res.status(200).json({ status: 200, msg: "success", roster: roster.FortyManNL });
                    /*
                    playerHelper.getPlayersByIds(roster.FortyMan, function (result) {
                        for (var i = 0; i < result.length; i++) {
                            if (result[i]) {
                                if (!result[i].Salary) {
                                    result[i].Salary = {
                                        Salary: "0"
                                    };
                                }
                                result[i].Salary.SalaryNumber = Number(result[i].Salary.Salary.replace(/[^0-9\.]+/g, ""));
                            }
                        }
                        result.sort(function (a, b) {
                            return b.Salary.SalaryNumber - a.Salary.SalaryNumber;
                        });
                        res.status(200).json({ status: 200, msg: "success", roster: result });
                    });
                    */
                }
                else {
                    res.status(200).json({ status: 200, msg: "success", roster: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});


router.get('/api/teams/roster/bench', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (roster) {
                    playerHelper.getPlayersByIds(roster.Bench, function (result) {
                        for (var i = 0; i < result.length; i++) {
                            result[i].Salary.SalaryNumber = Number(result[i].Salary.Salary.replace(/[^0-9\.]+/g, ""));
                        }
                        result.sort(function (a, b) {
                            return b.Salary.SalaryNumber - a.Salary.SalaryNumber;
                        });
                        res.status(200).json({ status: 200, msg: "success", roster: result });
                    });
                }
                else {
                    res.status(200).json({ status: 200, msg: "success", roster: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});


router.get('/api/teams/roster/lineup', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (roster) {
                    playerHelper.getPlayersByIds(roster.StartingLineup, function (result) {
                        res.status(200).json({ status: 200, msg: "success", roster: result });
                    });
                }
                else {
                    res.status(200).json({ status: 200, msg: "success", roster: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});


router.get('/api/teams/roster/starting-rotation', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (roster) {
                    playerHelper.getPlayersByIds(roster.StartingRotation, function (result) {
                        for (var i = 0; i < result.length; i++) {
                            result[i].Salary.SalaryNumber = Number(result[i].Salary.Salary.replace(/[^0-9\.]+/g, ""));
                        }
                        result.sort(function (a, b) {
                            return b.Salary.SalaryNumber - a.Salary.SalaryNumber;
                        });
                        res.status(200).json({ status: 200, msg: "success", roster: result });
                    });
                }
                else {
                    res.status(200).json({ status: 200, msg: "success", roster: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});


router.get('/api/teams/roster/bullpen', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (roster) {
                    playerHelper.getPlayersByIds(roster.Bullpen, function (result) {
                        for (var i = 0; i < result.length; i++) {
                            result[i].Salary.SalaryNumber = Number(result[i].Salary.Salary.replace(/[^0-9\.]+/g, ""));
                        }
                        result.sort(function (a, b) {
                            return b.Salary.SalaryNumber - a.Salary.SalaryNumber;
                        });
                        res.status(200).json({ status: 200, msg: "success", roster: result });
                    });
                }
                else {
                    res.status(200).json({ status: 200, msg: "success", roster: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});


router.get('/api/teams/roster/waiver-wire', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            Roster.findOne(query, function (err, roster) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (roster) {
                    playerHelper.getPlayersByIds(roster.WaiverWire, function (result) {
                        for (var i = 0; i < result.length; i++) {
                            result[i].Salary.SalaryNumber = Number(result[i].Salary.Salary.replace(/[^0-9\.]+/g, ""));
                        }
                        result.sort(function (a, b) {
                            return b.Salary.SalaryNumber - a.Salary.SalaryNumber;
                        });
                        res.status(200).json({ status: 200, msg: "success", roster: result });
                    });
                }
                else {
                    res.status(200).json({ status: 200, msg: "success", roster: [] });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.get('/api/teams/all', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var query = {};
            Team.find(query, function (err, teams) {

                for (t = 0; t < teams.length; t++) {
                    var name = teams[t].abbreviation;

                    // default colors:
                    teams[t]._doc["color1"] = "000000";
                    teams[t]._doc["color2"] = "888888";
                    teams[t]._doc["color3"] = "";
                    teams[t]._doc["colorText"] = "White";
                    teams[t]._doc["affiliates"] = {};

                    // find the MLB colors for this team.
                    for (i = 0; i < teamInfo.Teams.length; i++) {
                        if (name == teamInfo.Teams[i].abbreviation) {
                            var ti = teamInfo.Teams[i];
                            teams[t]._doc["color1"] = ti.colorPrimary;
                            teams[t]._doc["color2"] = ti.colorSecond;
                            teams[t]._doc["color3"] = ti.colorThird;
                            teams[t]._doc["colorText"] = ti.colorText;
                            teams[t]._doc["affiliates"] = ti.affiliates;
                            break;
                        }
                    }
                }
                res.status(200).json({ status: 200, msg: "success", teams: teams });
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.get('/api/teams/all-details', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            res.status(200).json({ status: 200, msg: "success", teamInfo: teamInfo });
        }
    });
});

router.get('/api/teams/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var query = { _id: req.query._id };
            Team.findOne(query, function (err, team) {
                res.status(200).json({ status: 200, msg: "success", team: team });
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.post('/api/teams/roster/generate', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var query = {
                $and: [
                    { _id: req.body.lid },
                    { Teams: { $elemMatch: { _id: req.body.tid } } }
                ]
            };
            League.findOne(query, { 'Teams.$': 1 }, function (err, league) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (league) {
                    playerHelper.generateDefaultPlayersByTeam(league, function () {
                        res.status(200).json({ status: 200, msg: "success" });
                    });
                }
                else {
                    res.status(500).json({ status: 500, msg: "League not found" });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});


router.post('/api/team/roster/intra-team-move', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            if (!req.body.to)
                req.body["to"] = "";
            if (!req.body.status)
                req.body["status"] = "";

            var headersSent = false;
            playerHelper.intraTeamMove(user, req.body.lid, req.body.tid, req.body.player, req.body.from, req.body.to, req.body.status, req.body.newStatusName,
                function (err, result) {
                    if (!headersSent) {
                        if (err) {
                            res.status(500).json({ status: 500, msg: err.errmsg });
                        } else {
                            res.status(200).json(result);
                        }
                    }
                    headersSent = true;
                });
        }
    });
});

router.post('/api/team/roster/set-player-status', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            if (!req.body.newStatus)
                req.body["newStatus"] = null;

            var headersSent = false;
            playerHelper.setPlayerStatus(user, req.body.lid, req.body.tid, req.body.player, req.body.from, req.body.newStatus, req.body.newStatusName,
                function (err, result) {
                    if (!headersSent) {
                        if (err) {
                            res.status(500).json({ status: 500, msg: err.errmsg });
                        } else {
                            res.status(200).json(result);
                        }
                    }
                    headersSent = true;
                });
        }

    });
});

// *****************************************
//
// trigger-next-action
// service that is invoked by the admin, calls the service that
// is invoked by cron jobs, and group triggers
//
//      lid: teamDataService.league._id,
//      tid: teamDataService.selectedTeam._id,
//      player: player,
//      force: true
// *************************************************************************
router.post('/api/team/roster/trigger-next-action', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var player = req.body.player;
            var tid = req.body.tid;
            var lid = req.body.lid;
            var forceAction = false;
            if ("force" in req.body) {
                forceAction = req.body.force
            }
            var headersSent = false;
            playerHelper.executeNextAction(user, lid, tid, player, forceAction, function (response) {
                if (!headersSent) {
                    if (response && ('status' in response)) {
                        res.status(response.status).json(response);
                    } else {
                        res.status(500).json({ status: 500, msg: "Error from ph service" });
                    }
                    headersSent = true;
                }
            });
        }

    });

});

// report mistake: league, team, player, user
router.post('/api/team/roster/report-mistake', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            // find the user.
            var user = req.body.user;
            var customterms = {};
            customterms["playername"] = req.body.player.FullName;
            customterms["teamname"] = req.body.team;
            customterms["leaguename"] = req.body.league;
            customterms["toAddress"] = [];
            customterms["toName"] = [];
            customterms.toAddress.push("eddie@dombrower.com");
            customterms.toName.push("Eddie (Personal)")
            customterms["fromAddress"] = user.Email;
            customterms["Subject"] = "Report Player Mistake: " + req.body.player.FullName;

            for (i = 0; i < 2; i++) {
                if (i == 1) {
                    customterms["toAddress"] = [];
                    customterms["toName"] = [];
                    customterms.toAddress.push("eddie@dombrower.com");
                    customterms.toName.push("Eddie (RSports)");
                    customterms.toAddress.push("tory@rsportsbaseball.com");
                    customterms.toName.push("Admin (RSports)");
                    customterms.toAddress.push("tony@rsportsbaseball.com");
                    customterms.toName.push("Admin (RSports)");
                    customterms.toAddress.push("will@rsportsbaseball.com");
                    customterms.toName.push("Admin (RSports)");
                }
                sendGridHelper.sendEmail("reportplayermistake", user, customterms, function (err, response) {
                    if (err) {
                        //  res.json({ status: 500, msg: "Something went wrong with email", err: err });
                    }
                    else {
                        //  res.status(200).json({ status: 200, msg: "Email sent.", user: result });
                    }
                });
            }
        }
    });
});

// called when updating the pitchers 
router.post('/api/team/roster/depth-chart/update', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.body.lid },
                    { TeamId: req.body.tid } 
                ]
            };
            Roster.findOne(query, function (err, league) {
                console.log(league);
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (league && req.body.positions) {
                    league.DepthChartNL = req.body.positions;

                    var batting_positions = [];
                    for(var i = 0; i < 9; i++) {
                        if(req.body.positions[i].Players.length != 0){
                            batting_positions.push(req.body.positions[i].Players[0]);
                        }
                    }
                    league.BattingOrderNL = batting_positions;
                    // league.BattingOrderNL = [];
                    league.markModified("BattingOrderNL");
                    league.markModified("DepthChartNL");
                    league.save(function (err, result) {
                        res.status(200).json({ status: 200, msg: "success" });

                        // need to save to rosterLineups too...
                        RosterLineup.findOne(query, function (err, rosterLineup) {
                            if (err) {
                                res.status(500).json({ status: 500, msg: "Roster not found" });
                            } else {
                                rosterLineup.DepthChartNL = req.body.positions;
                                rosterLineup.BattingOrderNL = batting_positions;
                                rosterLineup.markModified("DepthChartNL");
                                rosterLineup.markModified("BattingOrderNL");
                                rosterLineup.save(function (err, result) {
                                    
                                })
                            }
                        });
                    })
                }
                else {
                    res.status(500).json({ status: 500, msg: "League not found" });
                }
            });
        }
    });
});


router.get('/api/team/roster/depth-chart/get', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            var query = {
                $and: [
                    { LeagueId: req.query.lid },
                    { TeamId: req.query.tid }
                ]
            };
            Roster.findOne(query, function (err, league) {
                if (err) {
                    res.status(500).json({ status: 500, msg: err.errmsg });
                }
                else if (league) {
                    
                    res.status(200).json({ status: 200, msg: "success", positions: league.DepthChartNL });
                   
                }
                else {
                    res.status(500).json({ status: 500, msg: "League not found" });
                }
            });
        }
    });


    // *******************************************************
    //
    //  CLAIM-PLAYER
    //
    // *******************************************************
    router.post('/api/team/transaction/claim-player', function (req, res) {
        utils.validate(req, function (isValid, user) {
            if (isValid) {

                teamHelper.claimPlayer(req.body.lid, req.body.player, req.body.currentTeamId, req.body.acquiringTeamId, function (err, response) {
                    if (err) {
                        res.json({ status: 500, msg: "Something went wrong with claim", err: err });
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "Claim made.", response });
                    }
                });
            }
        });
    });

    // *******************************************************
    //
    //  EXECUTE-TRANSACTION
    //  ==> actually executes the trade transaction
    //
    // *******************************************************
    router.post('/api/team/transaction/execute', function (req, res) {
        utils.validate(req, function (isValid, user) {
            if (isValid) {

                teamHelper.executeTransaction(req.body.lid, req.body.transaction, function (err, response) {
                    if (err) {
                        res.json({ status: 500, msg: "Something went wrong with trade", err: err });
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "Trade made.", response });
                    }
                });
            }
        });
    });


    // *******************************************************
    //
    //  CANCEL-TRANSACTION
    //  
    //
    // *******************************************************
    router.post('/api/team/transaction/cancel', function (req, res) {
        utils.validate(req, function (isValid, user) {
            if (isValid) {

                teamHelper.executeTransaction(req.body.lid, req.body.transaction, req.body.user, function (err, response) {
                    if (err) {
                        res.json({ status: 500, msg: "Something went wrong with trade", err: err });
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "Trade made.", response });
                    }
                });
            }
        });
    });


    // *******************************************************
    //
    //  PROPOSE-TRANSACTION
    //  any response to a transaction.. create a new one or counter an existing one
    //
    // *******************************************************
    router.post('/api/team/transaction/propose', function (req, res) {
        utils.validate(req, function (isValid, user) {
            if (isValid) {

                teamHelper.proposeTransaction(req.body.lid, req.body.transaction, function (err, response) {
                    if (err) {
                        res.json({ status: 500, msg: "Something went wrong with trade", err: err });
                    }
                    else {
                        res.status(200).json({ status: 200, msg: "Trade made.", response });
                    }
                });
            }
        });
    });
     
});

module.exports = router;