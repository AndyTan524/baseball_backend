var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var utils = require('../helpers/utils');
var rosterHelper = require('../helpers/rosterHelper');
var leagueHelper = require('../helpers/leagueHelper');
var moment = require('moment');

var mongoose = require('mongoose');
var User = mongoose.model('User');
var League = mongoose.model('League');
var Team = mongoose.model('Team');
var Calendar = mongoose.model('Calendar');
var Content = mongoose.model('Content');
var FreeAgent = mongoose.model('FreeAgent');
var Arbitration = mongoose.model('Arbitration');
var Roster = mongoose.model('Roster');

// ***************************************************
//
// leagues/all
// ==> grab all leagues and just return their names
//
// ***************************************************
router.get('/api/leagues/all', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            League.find({}, {_id: 1, Name: 1, "Teams.r_name":1 , "Teams._id":1, "Teams.r_abbreviation":1, "Teams.TeamId": 1, "Settings": 1}, function (err, leagues) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    res.status(200).json({ status: 200, msg: "", leagues: leagues });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ***************************************************
//
// leagues/teams
// ==> grab all basic team info for the requested league
//
// ***************************************************
router.get('/api/leagues/teams', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            League.find({ _id: req.query.lid }, {Teams: 1, Conferences: 1, Affiliates: 1, Commissioners: 1 }, function (err, leagues) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    var userRoles = require( '../helpers/userRoles');
                    res.status(200).json({ status: 200, msg: "", league: leagues[0], roles: userRoles });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ***************************************************
//
// leagues/get
// ==> return everything about one requested league
//
// ***************************************************
router.get('/api/leagues/get', function (req, res) {
    utils.validate(req, function(isValid, user){
        if (isValid) { 
            League.findOne({ _id: req.query._id }, function (err, league) {
                if (err) {
                    res.status(500).json(err);
                }
                else { 
                    var userRoles = require( '../helpers/userRoles');
                    res.status(200).json({ status: 200, msg: "", league: league , ownerroles: userRoles.UserRoles }); 
                }
            });
        }
        else {
            res.status(401).json({status: 401, msg: "unauthorized" });
        }
    });
});

// ***************************************************
//
// leagues/get-settings
// ==> return name and settings one requested league
//
// ***************************************************
router.get('/api/leagues/get-settings', function (req, res) {
    utils.validate(req, function(isValid, user){
        if (isValid) { 
            League.findOne({ _id: req.query._id }, {Name: 1, Settings:1}, function (err, league) {
                if (err) {
                    res.status(500).json(err);
                }
                else { 
                    var userRoles = require( '../helpers/userRoles');
                    res.status(200).json({ status: 200, msg: "", league: league }); 
                }
            });
        }
        else {
            res.status(401).json({status: 401, msg: "unauthorized" });
        }
    });
});

// ***************************************************
//
// leagues/update-settings
// ==> update settings for one requested league
//
// ***************************************************
router.post('/api/leagues/update-settings', function (req, res) {
    utils.validate(req, function(isValid, user){
        if (isValid) { 
            League.updateOne({ _id: req.body._id }, {$set:{Settings:req.body.Settings}}, function (err, results) {
                if (err) {
                    res.status(500).json(err);
                }
                else { 
                    res.status(200).json({ status: 200, msg: "", results: results}); 
                }
            });
        }
        else {
            res.status(401).json({status: 401, msg: "unauthorized" });
        }
    });
});

// ***************************************************
//
// leagues/toggle-freeze
// ==> changes the value of the Settings.rosterEditing / lineupEditing
//        valid values: "Can Edit", "Frozen"
//
// ***************************************************
router.post('/api/leagues/toggle-freeze', function (req, res) {
    utils.validate(req, function(isValid, user){
        if (isValid) { 
         //   League.findOne({ _id : req.body.lid }, {Name: 1, Settings:1}, function (err, league) {
                League.findOne({ _id : req.body.lid }, function (err, league) {
 
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    var rosterEditing = "Can Edit";
                    var lineupEditing = "Can Edit";
                    if (!league.Settings) {
                        league._doc["Settings"] = { rosterEditing: rosterEditing, lineupEditing: lineupEditing };
                    } else {
                        if (!league.Settings.rosterEditing) {
                            league.Settings['rosterEditing'] = "Can Edit";
                        } else {
                            if( league.Settings.rosterEditing == "Frozen") {
                                league.Settings.rosterEditing = "Can Edit";
                            } else {
                                league.Settings.rosterEditing = "Frozen";
                            }
                        }
                        if (!league.Settings.lineupEditing) {
                            league.Settings['lineupEditing'] = "Can Edit";
                        } else {
                            if( league.Settings.lineupEditing == "Frozen") {
                                league.Settings.lineupEditing = "Can Edit";
                            } else {
                                league.Settings.lineupEditing = "Frozen";
                            }                            
                        }
                    }
                    league._doc["TestField"] = "Hello World";
                    league.markModified("Settings");
                    league.save( function( err, updatedLeague){
                        if( err ) {
                            res.status(401).json({ status: 401, msg: "didn't update league" }); 
                        } else {
                            res.status(200).json({ status: 200, msg: "updated", league: updatedLeague }); 
                        }
                    })



                }
            });
        }
        else {
            res.status(401).json({status: 401, msg: "unauthorized" });
        }
    });
});
// ***************************************************
//
// leagues/create
// ==> create a league with a name and possibly making it the admin's default league
// ==> adds in financials, free agents, and assigns rosters to each team
// ==> rosters include the 40 man roster and the non-roster list
//
// ***************************************************
router.post('/api/leagues/create', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            // first get MLB 
            var teamInfo = require('../helpers/teamInfo').Teams;
            var currentUser = user;



            // then get MLB 2017 data for teams
            var mlbleaguedata = "TeamFinancials2017";
            var retaineddata = "retainedBuyouts"
            Content.find({ name:  { $in: [mlbleaguedata, retaineddata ] } }, function (err, content) {
                if (err) {
                    res.status(500).json({ status: 500, msg: "Error", error: err });
                }
                else {
                    var financeInfo;
                    var retainedInfo;
                    if( content[0].name == mlbleaguedata) {
                       financeInfo = content[0].content.dataset;
                       retainedInfo = content[1].content.dataset;
                    } else {
                        financeInfo = content[1].content.dataset;
                        retainedInfo = content[0].content.dataset;
                    }

                    var newleague = {
                        Name: "",
                        PrimaryColor: "003c6d",
                        SecondarColor: "white",
                        Logo: "",
                        Description: "",
                        CreatedUTC: "",
                        Settings: {},
                        Calendar: {},
                        LeagueFinancials: {},
                        Conferences: [
                            {
                                name: "American",
                                color: "red",
                                divisions: [
                                    {
                                        name: "East",
                                        teams: []
                                    },
                                    {
                                        name: "Central",
                                        teams: []
                                    },
                                    {
                                        name: "West",
                                        teams: []
                                    },
                                    {
                                        name: "",
                                        teams: []
                                    }
                                ]
                            },
                            {
                                name: "National",
                                color: "blue",
                                divisions: [
                                    {
                                        name: "East",
                                        teams: []
                                    },
                                    {
                                        name: "Central",
                                        teams: []
                                    },
                                    {
                                        name: "West",
                                        teams: []
                                    },
                                    {
                                        name: "",
                                        teams: []
                                    }
                                ]
                            }
                        ],
                        Teams: [],
                        PlayerData: {
                            CarriedSalaries: []
                        },
                        FreeAgents: []
                    };


                    // name and creation time
                    newleague.Name = req.body.Name;
                    newleague.CreatedUTC = new Date().toISOString();
                    newleague.Settings["Default"] = req.body.Default;


                    // next drop in the teams from the team info array:
                    var teamsIndex = [];
                    for (t = 0; t < teamInfo.length; t++) {
                        newleague.Teams.push(teamInfo[t]);
                        newleague.Teams[t]['financials'] = {};
                        newleague.Teams[t]['retainedPlayers'] = [];

                        // for backwards compatibility
                        newleague.Teams[t]["_id"] = utils.genuuid(),
                            newleague.Teams[t]["Name"] = teamInfo[t].full_name,
                            newleague.Teams[t]["TeamName"] = teamInfo[t].abbreviation,
                            newleague.Teams[t]['Owners'] = [];

                        if (t == 2) {
                            // make this admin's team the Orioles
                            var owner = {
                                CreatedUTC: currentUser.CreatedUTC,
                                Name: currentUser.FirstName + " " + currentUser.LastName,
                                OwnershipPercentage: 1,
                                Role: 0,
                                UserId: currentUser._id.toString()
                            }
                            newleague.Teams[t].Owners.push(owner);
                        }


                        // simplify finding a team in this array
                        teamsIndex.push(teamInfo[t].r_name);
                    }

                    // now put the financials in.
                    for (f = 0; f < financeInfo.length; f++) {
                        var rteamname = financeInfo[f].Team;
                        var rindex = teamsIndex.indexOf(rteamname);
                        newleague.Teams[rindex].financials = financeInfo[f];
                    }

                    // and put the retained values in
                    for (r = 0; r < retainedInfo.length; r++) {
                        var rteamname = retainedInfo[r].Team;
                        if (rteamname != "") {
                            var rindex = teamsIndex.indexOf(rteamname);
                            retainedInfo[r]["Image"] = retainedInfo[r].MlbId ? "http://mlb.mlb.com/mlb/images/players/head_shot/" + retainedInfo[r].MlbId + ".jpg" : "//rsportsbaseball.com/assets/img/avatars/avatar1.jpg";
                            retainedInfo[r]["Source"] = "RetainedBuyouts";
                            newleague.Teams[rindex].retainedPlayers.push(retainedInfo[r]);
                        }
                    }

                    // *************************************
                    //
                    // now put all the MLB free agents in the new league's FreeAgent pool
                    //
                    // ***************************************************************************
                    FreeAgent.find({}, function (err, freeagents) {

                        freeagents = rosterHelper.putFreeAgentsIntoLeague( freeagents );
 
                        newleague.FreeAgents = freeagents;

                        utils.addLog("Begin create new league: " + newleague.Name, 0, "/api/leagues/create", "rosterHelper.buildRosterFromMaster");
                        League.create(newleague, function (err, league) {
                            if (err) {
                                res.status(500).json(err);
                            }
                            else {

                                Content.findOne({ name: "contractInfo" }, function (err, contractInfo) {
                                    var contractExtras = {};

                                    if (err) {
                                    } else {
                                        var dataset = contractInfo.content.dataset;
                                        for (i = 0; i < dataset.length; i++) {
                                            mid = dataset[i].MlbId;
                                            if (contractExtras[mid]) {

                                            } else {
                                                contractExtras[mid] = [];
                                            }
                                            contractExtras[mid].push(dataset[i]);
                                        }
                                    }

                                    // ********************************************************
                                    //
                                    // Finally, build the rosters from the master list
                                    //
                                    // *********************************************************
                                    rosterHelper.buildRosters(league, 0, league.Teams, contractExtras, function () {
                                        utils.addLog("Successfully built rosters for: " + league.Name, 0, "/api/leagues/create", "rosterHelper.buildRosters");

                                        res.status(200).json({ status: 200, msg: "League created." });
                                    });

                                    // if user requested this to be the default league,then update the user
                                    if (req.body.Default && req.body.Default == true) {
                                        if (!user.Settings) {
                                            user._doc["Settings"] = {};
                                        }
                                        user.Settings["DefaultLeague"] = league.Name;
                                        user.Settings["DefaultLeagueId"] = league._id.toString();

                                        user.markModified("Settings");
                                        user.save(function (err, response) {
                                            if (err) {
                                                console.log(err.message);
                                            } else {
                                                console.log("User updated with default league");
                                            }
                                        });

                                    }
                                });
                            }
                        });
                    });
                }
            }
            );
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

// ***************************************************
//
// leagues/reset
// ==> keep the league info
// ==> replace financials
// ==> delete the rosters and rebuild them
// ==> replace the free agents, international draft and first year
//
// ***************************************************
router.post('/api/leagues/resetteams', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            // first get MLB 
            var teamInfo = require('../helpers/teamInfo').Teams;
            var currentUser = user;

            // first delete all the teams that are in this league
            rosterHelper.removeRostersForLeague(req.body._id, function (err, result) {



                // then get MLB 2017 financial data for teams
                var mlbleaguedata = "TeamFinancials2017";
                var retaineddata = "retainedBuyouts"
                Content.find({ name:  { $in: [mlbleaguedata, retaineddata ] } }, function (err, content) {
                    if (err) {
                        res.status(500).json({ status: 500, msg: "Error", error: err });
                    }
                    else {
                        var financeInfo;
                        var retainedInfo;
                        if( content[0].name == mlbleaguedata) {
                           financeInfo = content[0].content.dataset;
                           retainedInfo = content[1].content.dataset;
                        } else {
                            financeInfo = content[1].content.dataset;
                            retainedInfo = content[0].content.dataset;
                        }

                        League.findOne({ "_id": req.body._id }, function (err, newleague) {

                            // next clear out the financials and drop in the teams from the team info array:
                            var teamsIndex = [];
                            for (t = 0; t < teamInfo.length; t++) {
                                 newleague.Teams[t]['financials'] = {};
                                newleague.Teams[t]['retainedPlayers'] = [];

                                newleague.Teams[t]['affiliates'] = teamInfo[t].affiliates;
                                // simplify finding a team in this array
                                teamsIndex.push(teamInfo[t].r_name);
                            }

                            // now put the financials in.
                            for (f = 0; f < financeInfo.length; f++) {
                                var rteamname = financeInfo[f].Team;
                                var rindex = teamsIndex.indexOf(rteamname);
                                newleague.Teams[rindex].financials = financeInfo[f];
                            }

                            // and put the retained values in
                            for (r = 0; r < retainedInfo.length; r++) {
                                var rteamname = retainedInfo[r].Team;
                                if (rteamname != "") {
                                    var rindex = teamsIndex.indexOf(rteamname);
                                    retainedInfo[r]["Image"] = retainedInfo[r].MlbId ? "http://mlb.mlb.com/mlb/images/players/head_shot/" + retainedInfo[r].MlbId + ".jpg" : "//rsportsbaseball.com/assets/img/avatars/avatar1.jpg";
                                    retainedInfo[r]["Source"] = "RetainedBuyouts";
                                    newleague.Teams[rindex].retainedPlayers.push(retainedInfo[r]);
                                }
                            }

                            // now put all the MLB free agents in the new league's FreeAgent pool
                            FreeAgent.find({}, function (err, freeagents) {

                                freeagents = rosterHelper.putFreeAgentsIntoLeague( freeagents );
         
                                // replace the old free agents with the new free agents
                                newleague.FreeAgents = freeagents;

                                //utils.addLog("Begin create new league: " newleague.Name, 0, "/api/leagues/create", "rosterHelper.buildRosterFromMaster");
                                newleague.markModified("Teams");
                                newleague.markModified("FreeAgents");
                                newleague.markModified("InternationalDraft");
                                newleague.markModified("FirstYearDraft");

                                newleague.save(function (err, league) {
                                    if (err) {
                                        res.status(500).json(err);
                                    }
                                    else {

                                        Content.findOne({ name: "contractInfo" }, function (err, contractInfo) {
                                            var contractExtras = {};

                                            if (err) {
                                            } else {
                                                var dataset = contractInfo.content.dataset;
                                                for (i = 0; i < dataset.length; i++) {
                                                    mid = dataset[i].MlbId;
                                                    if (contractExtras[mid]) {

                                                    } else {
                                                        contractExtras[mid] = [];
                                                    }
                                                    contractExtras[mid].push(dataset[i]);
                                                }
                                            }
                                            rosterHelper.buildRosters(league, 0, league.Teams, contractExtras, function () {
                                                //      utils.addLog("Successfully built rosters for: " + league.Name, 0, "/api/leagues/create", "rosterHelper.buildRosters");

                                                console.log("Rosters reset for league");
                                                res.status(200).json({ status: 200, msg: "League teams and financials reset." });
                                            });
                                        });
                                    }
                                });
                            });
                        });
                    }
                });
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});


router.post('/api/leagues/delete', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            League.findOneAndRemove({ _id: req.body._id }, function (err, league) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    rosterHelper.removeRostersForLeague( req.body._id, function(err,result)
                    {
                        res.status(200).json({ status: 200, msg: "deleted"  });
                    });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});
 

router.get('/api/leagues/calendar', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            var date = req.query.date;
            League.findOne({ _id: req.query._id }, function (err, league) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    Calendar.find({ start: moment(date).format("YYYYMMDD")}, function (err, events) {
                        res.status(200).json(events);
                    }); 
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});


router.get('/api/leagues/calendar/daily', function (req, res) {
    utils.validate(req, function (isValid, user) {
        isValid = true;
        if (isValid) {
            League.findOne({ _id: req.query._id }, function (err, league) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    Calendar.find({start: req.query.date}, function (err, events) {
                        res.status(200).json(events);
                    });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

var Standings = require('../helpers/finalStandings');

router.get('/api/leagues/final-standings', function (req, res) {
    utils.validate(req, function (isValid, user) {

        var results = { "Standings": []};
        results.Standings = Standings.Standings2017;
        

        res.status(200).json( results );
    });
});

router.post('/api/leagues/team/updateCap', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var qry = {
                $and:
                    [
                        {
                            _id: req.body.lid
                        },
                        {
                            Teams: { $elemMatch: { _id: req.body.tid } }
                        }
                    ]
            };
            League.findOne( {_id: req.body.lid}, function (err, league) {
                if (err) {
                    res.status(500).json(err);
                }
                else if (league) {
                    var teamindex = -1;
                    for(i=0; i<league.Teams.length; i++) {
                        if( league.Teams[i]._id == req.body.tid) {
                            teamindex = i;
                            break;
                        }
                    }
                    league.Teams[teamindex].financials["Spending Cap"] = req.body.cap;
                    league.markModified("Teams");
                    league.save( function(err, response){
                        if( err ) {
                            res.status(500).json({ status: 500, msg: "couldn't update salary cap" });
                        } else {

                            res.status(200).json({ status: 200, msg: "updated salary cap" });
                        }
                    })

                    
                }
            });
        }
    });
});


// ***************************************
//
// update financials
// if parameters include a team, just do the one team.
// if paramters don't include a team, do them all
// if parameters include save, then write them out to the team(s)
//
// ****************************************
   /*
                    B Salary Cap (Manually entered)
                    B  2018Guarantees        (CurrentPayroll) 	 
                    C 	All Guaranteed Salaries of Players	2018 salaries (bonuses are built in)
 RetainedDollars 	 D 	All Retained Salaries of Players (+/-)	add retained salary for each year for each player
 ContractOption              Buyouts 	 
                    E 	All Buyouts of Options	from the retained salaries view in the financials
 TotalCapHit 	    F 	 F=(C+D+E) 	
 CapSpace 	        G	G=(B-C-D-E)	
 ArbForecasts 	    H	All players from Arbitration Tracker added up by corresponding team (take the mid-point of the 'Arbitration Forecasted Range' to determine player's forecasted salary as per equation)	ok.
 Pre-ArbForecasts 	I	Always $8,175,000 for each team	ok.
 ProjectedPayroll 	J	J=(C+D+E+H+I)	
 EstimatedCapSpace 	K	K=(B-J)	
 AAV 	            L	All players AAV added up	Only for non-zero values and newly signed free agents.
 LuxuryTaxSpace 	M	M=(197000000-L)	197,000,000-L
                    */

router.post('/api/leagues/team/updateFinancials', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            var qryLeague =  { _id: req.body.lid };
            var qryRosterTeams = {};
            var leagueTeamProperty = {Teams: 1};
            var qryArbTeams = {};
            var teamId = req.body.tid;
            var teamList = [];
            if( teamId  == "list") {
                teamList = req.body.teamList;
            }
            leagueHelper.updateFinancials(req.body.lid, teamId, teamList, req.body.save, function(err, message, finArray)
            {
                if( err ) {
                    res.status(401).json({ status: 401, msg: message});  
                } else {
                    res.status(200).json({ status: 200, msg: message, original: finArray, new: finArray});  
                }
            })
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.post('/api/leagues/dbFixes', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            // first,default is:
            //  put teamnames into the rosters.
            // update ALL players with their Rule5 data

            var qry = {
                $and:
                    [
                        {
                            _id: req.body.lid
                        }
                    ]
            }

            // first grab the league so we can update the free agents (rule5 and type)
            League.findOne({ _id: req.body.lid }, function (err, league) {
                if (err) {
                    res.status(500).json({ status: 500, msg: "League not found" });
                } else {

                    // now get all the rosters for the league
                    Rosters.find({ LeagueId: req.body.lid }, function (err, rosters) {
                        if (err) {
                            res.status(500).json({ status: 500, msg: "Rosters not found" });
                        } else {
                            // now get all the free agents
                            FreeAgent.find({}, function (err, freeagents) {
                                if (err) {
                                    res.status(500).json({ status: 500, msg: "Free Agents not found" });
                                } else {
                                    // finally get all players off the master list
                                    MasterPlayers.find({}, function (err, masters) {
                                        if (err) {
                                            res.status(500).json({ status: 500, msg: "Master Players not found" });
                                        } else {

                                        }
                                    });
                                }
                            });
                        }

                        // first go through the rosters and add their r_names in
                        for( i=0; i<league.Teams.length; i++) {
                            var rname = league.Teams[i].r_name;
                            var tid   = league.Teams[i].TeamId;
                            for( r=0; r<rosters.length; r++ ) {
                                if( rosters[r]._id == tid ) {
                                    rosters[r]["TeamName"] = rname;
                                    break;
                                }
                            }
                        }

                        // next go through the free agents and insert their Type
                    });

                }
            });
        } else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

module.exports = router;