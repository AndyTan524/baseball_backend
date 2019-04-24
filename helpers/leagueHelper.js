var utils = require('../helpers/utils');
var request = require('request');
var cheerio = require('cheerio');
var moment = require('moment');
var mongoose = require('mongoose');
var League = mongoose.model('League');
var Roster = mongoose.model('Roster');
var Arbitration = mongoose.model('Arbitration');
var Standings = mongoose.model('Standings');
var Schedule = mongoose.model("Schedule");
var TeamInfo = require('../helpers/teamInfo');



module.exports = {

    getTeam: function (league, callback) {
        var qry = { $and: [{ _id: (league._id) }, { "Teams": { $elemMatch: { "TeamName": league.Teams[0].TeamName } } }] };
        League.findOne(qry, { _id: 1, "Teams.$": 1 }, function (err, league) {
            callback(err, league);
        });

    },

    getTeamById: function (lid, tid, callback) {
        var qry = { $and: [{ _id: lid }, { "Teams": { $elemMatch: { _id: tid } } }] };
        League.findOne(qry, { _id: 1, "Teams.$": 1 }, function (err, league) {
            callback(err, league.Teams[0]);
        });

    },

    getRosterByRName: function (lid, rname, callback) {
        var qry = { $and: [{ _id: lid }, { "Teams": { $elemMatch: { r_name: rname } } }] };
        League.findOne(qry, { _id: 1, "Teams.$": 1 }, function (err, league) {
            if (league && league.Teams && league.Teams.length > 0) {
                Roster.findOne({ TeamId: league.Teams[0]._id }, function (err, roster) {
                    callback(err, roster);
                })
            }
            else {
                callback({ msg: "No league" }, null);
            }
        });

    },

    // if teamId == "All" it goes to everyone
    getOwners: function (leagueId, teamId, callback) {
        if (teamId == "") {
            callback(null, [], "");
            return;
        }
        if (teamId == "Free Agents") {
            callback(null, [], "Free Agents");
            return;
        }
        if (teamId == "All") {
            League.find({ _id: leagueId }, { "Teams.Owners": 1 }, function (err, league) {
                if (err || league == null || !league[0].Teams) {
                    callback(null, [], "");
                } else {
                    var ownerList = [];
                    for (i = 0; i < league[0].Teams.length; i++) {
                        for (j = 0; j < league[0].Teams[i].Owners.length; j++) {
                            ownerList.push(league[0].Teams[i].Owners[j]);
                        }
                    }
                    callback(err, ownerList, "All");
                }
            });
        } else {
            var qry = { $and: [{ _id: (leagueId) }, { "Teams": { $elemMatch: { "_id": teamId } } }] };
            League.findOne(qry, { _id: 1, "Teams.$": 1 }, function (err, league) {
                if (err || league == null) {
                    callback(null, [], "");
                } else {
                    callback(err, league.Teams[0].Owners, league.Teams[0].r_name);
                }
            });
        }
    },

    /*
                B Salary Cap (Manually entered)
            B  2018Guarantees        (CurrentPayroll) 	 
            C 	All Guaranteed Salaries of Players	2018 salaries (bonuses are built in)
  RetainedDollars 	D 	All Retained Salaries of Players (+/-)	add retained salary for each year for each player
  ContractOption Buyouts  E 	All Buyouts of Options	from the retained salaries view in the financials
  TotalCapHit 	 F 	 F=(C+D+E) 	
  CapSpace 	     G	G=(B-C-D-E)	
  ArbForecasts 	H	All players from Arbitration Tracker added up by corresponding team (take the mid-point of the 'Arbitration Forecasted Range' to determine player's forecasted salary as per equation)	ok.
  Pre-ArbForecasts I	Always $8,175,000 for each team	ok.
  ProjectedPayroll J	J=(C+D+E+H+I)	
  EstimatedCapSpace K	K=(B-J)	
  AAV 	          L	All players AAV added up	Only for non-zero values and newly signed free agents.
  LuxuryTaxSpace 	  M	M=(197000000-L)	197,000,000-L
                */
    updateFinancials: function (leagueId, teamId, teamlist, save, callback) {
        if (teamId == "all") {
            var qryRosterTeams = {};
            var qryLeague = { _id: leagueId };
            var leagueTeamProperty = { Teams: 1 };
            var qryArbTeams = {};
        } else if (teamId == "list") {
            var qryRosterTeams = {};
            var qryLeague = { _id: leagueId };
            var leagueTeamProperty = { Teams: 1 };
            var qryArbTeams = {};
        } else {
            // it must be a valid teamId     
            qryRosterTeams = { TeamId: teamId };
            qryLeague = { $and: [{ _id: leagueId }, { Teams: { $elemMatch: { _id: teamId } } }] };
            leagueTeamProperty = { _id: 1, 'Teams.$': 1 };

            teamlist = [];
            teamlist[0] = teamId;
        }

        var qryLeague = { _id: leagueId };
        var leagueTeamProperty = { Teams: 1 };
        var qryArbTeams = {};

        utils.addLog("Loading league", 0, "leagueHelper", "leagueHelper/updateFinancials");

        League.findOne(qryLeague, leagueTeamProperty, function (err, leagueTeams) {
            if (err) {

                utils.addLog("Failed Loading league", 0, "leagueHelper", "leagueHelper/updateFinancials");
                callback(err, err.msg);
            }
            else if (leagueTeams) {



                // ******** get roster(s) first ************
                qry = {
                    $and:
                        [
                            {
                                LeagueId: leagueId
                            },
                            qryRosterTeams
                        ]
                };
                Roster.find(qry, function (err, rosters) {
                    if (err) {
                        utils.addLog("Failed Loading rosters", 0, "leagueHelper", "leagueHelper/updateFinancials");

                        callback(err, "Roster not found");
                        return;
                    } else {

                        // unfortunately, have to  match the league's teams to the roster teams for each pass through the loop

                        // now get the players in arbitration
                        if (teamId == "all") {
                        } else if (teamId == "list") {

                        } else {
                            // one team returned in this case.
                            //   qryArbTeams = {rTeamName: leagueTeams.Teams[0].r_name};
                        }

                        qry = {
                            $and:
                                [
                                    {
                                        LeagueId: leagueId
                                    },
                                    qryArbTeams
                                ]
                        };
                        Arbitration.find(qry, function (err, arbPlayers) {
                            if (err) {
                                utils.addLog("Failed Loading arbitration players", 0, "leagueHelper", "leagueHelper/updateFinancials");
                                callback(err, "Arbitration players not found");
                            }
                            // ok, ready to do each team
                            // unfortunately, have to match the team names, abbreviations, etc.
                            for (li = 0; li < leagueTeams.Teams.length; li++) {

                                if (teamId == "all" || teamlist.indexOf(leagueTeams.Teams[li]._id) >= 0) {
                                    var team = leagueTeams.Teams[li];
                                    var financials = team.financials;
                                    var retainedPlayers = team.retainedPlayers;
                                    var teamname = team.r_name;
                                    var teamabbr = team.r_abbreviation;

                                    var nextRoster;
                                    for (ni = 0; ni < rosters.length; ni++) {
                                        if (rosters[ni].TeamAbbr == teamabbr) {
                                            nextRoster = rosters[ni];
                                            break;
                                        }
                                    }


                                    var payroll = 0; // add up all the roster/non-roster players current year payroll
                                    var AAV = 0;     // add up all roster/non-roster AAV's if they appear
                                    var year = "2018";

                                    for (i = 0; i < nextRoster.FortyManNL.length; i++) {
                                        var sal = nextRoster.FortyManNL[i].rSalary[year].Salary;
                                        if (sal && sal != "" && sal[0] != "A" && sal != "FA") {
                                            payroll += Number(sal.replace(/[^0-9.]/g, ""));
                                        }
                                        var aav = nextRoster.FortyManNL[i].rSalary.AAV;
                                        if (aav && aav != "") {
                                            AAV += Number(aav.replace(/[^0-9.]/g, ""));
                                        }
                                    }
                                    // only add in non-roster players who are on the 40 man roster!
                                    // OR in off-season mode.

                                    for (i = 0; i < nextRoster.NonRoster.length; i++) {
                                        var nrPlayer = nextRoster.NonRoster[i];
                                        var guaranteed = false;
                                        if (("Guaranteed" in nrPlayer.rSalary) && nrPlayer.rSalary.Guaranteed == true) {
                                            guaranteed = true;
                                        }
                                        if (nrPlayer.onFortyMan || guaranteed) {
                                            var sal = nextRoster.NonRoster[i].rSalary[year].Salary;
                                            if (sal && sal != "" && sal[0] != "A" && sal != "FA") {
                                                payroll += Number(sal.replace(/[^0-9.]/g, ""));
                                            }
                                            var aav = nextRoster.NonRoster[i].rSalary.AAV;
                                            if (aav && aav != "") {
                                                AAV += Number(aav.replace(/[^0-9.]/g, ""));
                                            }
                                        }
                                    }

                                    var retained = 0; // total all retained salaries and buyouts of options
                                    var buyouts = 0;
                                    var retainedAAV = 0;
                                    var negative = new RegExp("[(]");

                                    for (i = 0; i < retainedPlayers.length; i++) {
                                        var rp = retainedPlayers[i];

                                        var sal = rp.AAV;
                                        if (sal && sal != "") {
                                            if (negative.test(sal)) {
                                                // negative number
                                                retainedAAV -= Number(sal.replace(/[^0-9.]/g, ""));
                                            } else {
                                                // positive number
                                                retainedAAV += Number(sal.replace(/[^0-9.]/g, ""));
                                            }

                                        }

                                        for (y = 2018; y < 2019; y++) {
                                            var sal = rp["Salary" + y];
                                            var bot = rp["Buyout" + y];
                                            if (sal && sal != "") {
                                                if (negative.test(sal)) {
                                                    // negative number
                                                    retained -= Number(sal.replace(/[^0-9.]/g, ""));
                                                } else {
                                                    // positive number
                                                    retained += Number(sal.replace(/[^0-9.]/g, ""));
                                                }

                                            }
                                            if (bot && bot != "") {
                                                if (negative.test(bot)) {
                                                    // negative number
                                                    buyouts -= Number(bot.replace(/[^0-9.]/g, ""));
                                                } else {
                                                    // positive number
                                                    buyouts += Number(bot.replace(/[^0-9.]/g, ""));
                                                }
                                            }
                                        }

                                        // ******
                                        // finally, if player doesn't have carried date, add it in
                                        // when the team is saved, the dates will get saved
                                        if (!rp.CarriedDate) {
                                            // set date to 12/31/2017
                                            d = new Date("12/31/2017").toISOString();
                                            rp.CarriedDate = d;
                                        }
                                    }

                                    var arbforecast = 0; // average of high/low next year forecasts
                                    var countagreed = 0;


                                    if (arbPlayers) {
                                        for (i = 0; i < arbPlayers.length; i++) {

                                            // only for this team...
                                            if (arbPlayers[i].rTeamName == teamname) {
                                                var ap = arbPlayers[i];
                                                var agreed = ap.AgreedTerms;
                                                var nt = ap.NonTender;
                                                if (agreed == "" && nt == "") {
                                                    var high = Number(ap.NextYearForecastedHigh.replace(/[^0-9.]/g, ""));
                                                    var low = Number(ap.NextYearForecastedLow.replace(/[^0-9.]/g, ""));
                                                    arbforecast += (high + low) / 2000000; // put in 0.000M format for others
                                                } else {
                                                    countagreed++;
                                                }
                                            }
                                        }
                                    }

                                    // ok, create the return object
                                    var totalCapHit = payroll + retained + buyouts;
                                    var spendingCap = Number(financials["Spending Cap"].replace(/[^0-9.]/g, ""));
                                    var capSpace = (spendingCap / 1000000) - (totalCapHit);
                                    var preArb = Number(financials["Pre-Arb Forecasts"].replace(/[^0-9.]/g, ""));
                                    var projectedPayroll = totalCapHit + arbforecast + (preArb / 1000000);
                                    var estCapSpace = (spendingCap / 1000000) - projectedPayroll;
                                    var luxurySpace = 197 - (AAV + retainedAAV);

                                    // ((((((((((((((( TODO: Make this a league variable )))))))))))))))
                                    var needToIncludePreArb = false;
                                    if (!needToIncludePreArb) {
                                        preArb = 0;
                                    }

                                    // example for formatting
                                    // var luxuryString = "$" + (luxury - avg);
                                    // $scope.newLuxury =  luxuryString.replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");
                                    //
                                    totalCapHit = ("$" + (totalCapHit * 1000000).toFixed(0)).replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");
                                    spendingCap = ("$" + (spendingCap).toFixed(0)).replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");
                                    capSpace = ("$" + (capSpace * 1000000).toFixed(0)).replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");
                                    preArb = ("$" + (preArb).toFixed(0)).replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");
                                    arbforecast = ("$" + (arbforecast * 1000000).toFixed(0)).replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");

                                    projectedPayroll = ("$" + (projectedPayroll * 1000000).toFixed(0)).replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");
                                    estCapSpace = ("$" + (estCapSpace * 1000000).toFixed(0)).replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");
                                    luxurySpace = ("$" + (luxurySpace * 1000000).toFixed(0)).replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");

                                    payroll = ("$" + (payroll * 1000000).toFixed(0)).replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");
                                    retained = ("$" + (retained * 1000000).toFixed(0)).replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");
                                    buyouts = ("$" + (buyouts * 1000000).toFixed(0)).replace(/(\d)(?=(\d{3})+(?!\d))/g, "$1,");


                                    if (save && save == true) {


                                        leagueTeams.Teams[li].financials = {
                                            "Luxury Tax Space": luxurySpace,
                                            "Estimated Cap Space": estCapSpace,
                                            "Projected Payroll": projectedPayroll,
                                            "Pre-Arb Forecasts": preArb,
                                            "Arb Forecasts": arbforecast,
                                            "Cap Space": capSpace,
                                            "Total Cap Hit": totalCapHit,
                                            "Contract Option Buyouts": buyouts,
                                            "Retained Dollars": retained,
                                            "2018 Guarantees (Current Payroll)": payroll,
                                            "Spending Cap": spendingCap,
                                            "Team": teamname
                                        }

                                        var financials = leagueTeams.Teams[li].financials;
                                        var test = 1;

                                    } else {

                                        var finArray = [
                                            { name: "Spending Cap", amount: spendingCap, new: spendingCap },
                                            { name: "2018 Guarantees (Current Payroll)", amount: financials["2018 Guarantees (Current Payroll)"], new: payroll },
                                            { name: "Retained Dollars", amount: financials["Retained Dollars"], new: retained },
                                            { name: "Contract Option Buyouts", amount: financials["Contract Option Buyouts"], new: buyouts },
                                            { name: "Total Cap Hit", amount: financials["Total Cap Hit"], new: totalCapHit },
                                            { name: "Cap Space", amount: financials["Cap Space"], new: capSpace },
                                            { name: "Arb Forecasts", amount: financials["Arb Forecasts"], new: arbforecast },
                                            { name: "Pre-Arb Forecasts", amount: preArb, new: preArb },
                                            { name: "Projected Payroll", amount: financials["Projected Payroll"], new: projectedPayroll },
                                            { name: "Estimated Cap Space", amount: financials["Estimated Cap Space"], new: estCapSpace },
                                            { name: "Luxury Tax Space", amount: financials["Luxury Tax Space"], new: luxurySpace }
                                        ]
                                    }
                                }
                            }

                            if (save && save == true) {
                                leagueTeams.markModified("Teams");
                                leagueTeams.save(function (err, response) {
                                    if (err) {
                                        callback(err, "Failed to update DB.");

                                    } else {
                                        callback(err, "success", []);
                                    }
                                });
                            } else {
                                // just send back most recent array;

                                callback(false, "succes", finArray);
                            }

                        });

                    }
                })
            } else {
                utils.addLog("Team note found", 0, "leagueHelper", "leagueHelper/updateFinancials");

                callback(err, "Team not found");
            }
        });
    },

    // ************************************** LEAGUE STANDINGS ***************************** //

    calcuatedStandingsOrder: function (dbStandings) {
        var leagues = dbStandings.Conferences;

        for (l = 0; l < leagues.length; l++) {
            for (d = 0; d < leagues[l].divisions.length; d++) {

                if (leagues[l].divisions[d].name != "") {
                    var oldTeams = leagues[l].divisions[d].teams;
                    var pctArray = new Array(oldTeams.length);

                    // calculate the pct w/l and sort in order by highest winning %
                    for (t = 0; t < oldTeams.length; t++) {
                        var wins = oldTeams[t].W;
                        var total = oldTeams[t].L + wins;
                        var winningPct = 0;
                        if (total > 0) {
                            winningPct = wins / total;
                        }
                        oldTeams[t].Pct = winningPct;
                        oldTeams[t].PctDisplay = winningPct.toFixed(3);
                        pctArray[t] = { index: t, pct: winningPct };
                    }
                    pctArray.sort(function (a, b) { return b.pct - a.pct });

                    var bestTeam = oldTeams[pctArray[0].index];
                    bestTeam.GB = "-";
                    for (t = 1; t < oldTeams.length; t++) {
                        var nextTeam = oldTeams[pctArray[t].index];
                        var gb = (bestTeam.W - nextTeam.W) + (nextTeam.L - bestTeam.L);
                        if (gb == 0) {
                            gb = "-"
                        } else if (gb % 2 == 0) {
                            // even
                            gb = (gb / 2).toFixed(0);
                        } else {
                            gb = ((gb - 1) / 2).toFixed(0);
                            if (gb == 0)
                                gb = "";
                            gb = gb + '1/2';
                        }
                        oldTeams[pctArray[t].index].GB = gb;
                    }

                    var newTeams = [];
                    for (t = 0; t < oldTeams.length; t++) {
                        newTeams.push(oldTeams[pctArray[t].index]);
                    }

                    leagues[l].divisions[d].teams = newTeams;
                }

            }
        }
        return (leagues);
    },

    createLeagueStandings: function (lid, season, callback) {

        League.findOne({ _id: lid }, { Name: 1, Teams: 1, Conferences: 1 }, function (err, league) {
            if (err) {
                callback(null);
            } else {
                // create league structure...
                conf = league.Conferences;
                teams = league.Teams;


                standings = [];
                var leagueIndex = new Array(conf.length);
                var divisionIndex = new Array(conf.length);

                for (l = 0; l < conf.length; l++) {
                    leagueIndex[l] = conf[l].name;
                    nextLeague = {
                        name: conf[l].name,
                        style: {
                            "color": "white",
                            "text-transform": "uppercase",
                            "background-color": conf[l].color,
                        },
                        divisions: []
                    };


                    for (d = 0; d < conf[l].divisions.length; d++) {
                        // empty the teams from the division.
                        if (d == 0) {
                            divisionIndex[l] = new Array(conf[l].divisions.length);
                        }
                        nextDivision = {
                            teams: [],
                            name: conf[l].divisions[d].name
                        }
                        divisionIndex[l][d] = conf[l].divisions[d].name;
                        nextLeague.divisions.push(nextDivision);
                    }
                    standings.push(nextLeague);
                }


                for (t = 0; t < teams.length; t++) {
                    nextTeam = teams[t];
                    delete nextTeam.affiliates;
                    delete nextTeam.financials;
                    delete nextTeam.retainedPlayers;
                    delete nextTeam.Owners;
                    nextTeam["W"] = 0;
                    nextTeam["L"] = 0;
                    nextTeam["Pct"] = 0;
                    nextTeam["PctDisplay"] = "0.000";
                    nextTeam["GB"] = "";
                    nextTeam["History"] = [];
                    nextTeam["MagicNumber"] = "";
                    nextTeam["Elim"] = "";
                    nextTeam["Status"] = "";

                    nextTeam.useDH = false;
                    if (TeamInfo.ALTeams.indexOf(nextTeam.r_name) >= 0) {
                        nextTeam.useDH = true;
                    }

                    if (nextTeam.conference) {
                        li = leagueIndex.indexOf(nextTeam.conference);
                    } else {
                        li = Math.round(t / 15);
                    }

                    if (nextTeam.division) {
                        div = divisionIndex[li].indexOf(nextTeam.division);
                    } else {
                        div = Math.round(t / 6);
                    }
                    standings[li].divisions[div].teams.push(nextTeam);
                }

                var standingsObj = {
                    LeagueId: lid,
                    LeagueName: league.Name,
                    Season: season,
                    LastUpdate: new Date().toISOString(),
                    Conferences: standings
                }
                Standings.create(standingsObj, function (err, newStandings) {
                    if (err) {
                        callback("error", null);
                    } else {
                        callback(null, standings);

                    }
                })
            }
        });
    },

    insertWLRecordsInStandings: function (Conferences, summary) {
        for (c = 0; c < Conferences.length; c++) {
            for (d = 0; d < Conferences[c].divisions.length; d++) {
                for (t = 0; t < Conferences[c].divisions[d].teams.length; t++) {

                    // is home team?
                    if (Conferences[c].divisions[d].teams[t].r_name == summary.Home.name) {
                        var result = "W";
                        if (summary.Home.R > summary.Visit.R) {
                            // home team wins!
                            Conferences[c].divisions[d].teams[t].W++;
                        } else {
                            Conferences[c].divisions[d].teams[t].L++;
                            result = "L";
                        }
                        if (!Conferences[c].divisions[d].teams[t].History) {
                            Conferences[c].divisions[d].teams[t].History = [];
                        }
                        // put this game's result at the [0] index of the array.
                        Conferences[c].divisions[d].teams[t].History.unshift(result);
                    }

                    // is visitor team?
                    if (Conferences[c].divisions[d].teams[t].r_name == summary.Visit.name) {
                        var result = "W";
                        if (summary.Home.R < summary.Visit.R) {
                            // visit team wins!
                            Conferences[c].divisions[d].teams[t].W++;
                        } else {
                            Conferences[c].divisions[d].teams[t].L++;
                            result = "L";
                        }
                        if (!Conferences[c].divisions[d].teams[t].History) {
                            Conferences[c].divisions[d].teams[t].History = [];
                        }
                        // put this game's result at the [0] index of the array.
                        Conferences[c].divisions[d].teams[t].History.unshift(result);
                    }
                }
            }
        }

        return Conferences;
    },

    adjustWLRecordsInStandings: function (Conferences, summary) {

        // if a win, then subtract a loss
        // if a loss, subtract a win
        for (c = 0; c < Conferences.length; c++) {
            for (d = 0; d < Conferences[c].divisions.length; d++) {
                for (t = 0; t < Conferences[c].divisions[d].teams.length; t++) {

                    // is home team?
                    if (Conferences[c].divisions[d].teams[t].r_name == summary.Home.name) {
                        var result = "W";
                        if (summary.Home.R > summary.Visit.R) {
                            // home team wins!
                            Conferences[c].divisions[d].teams[t].W++;
                            Conferences[c].divisions[d].teams[t].L--;
                        } else {
                            Conferences[c].divisions[d].teams[t].L++;
                            Conferences[c].divisions[d].teams[t].W--;
                            result = "L";
                        }
                        if (!Conferences[c].divisions[d].teams[t].History) {
                            Conferences[c].divisions[d].teams[t].History = [];
                        }
                        // put this game's result at the [0] index of the array.
                        Conferences[c].divisions[d].teams[t].History.unshift(result);
                    }

                    // is visitor team?
                    if (Conferences[c].divisions[d].teams[t].r_name == summary.Visit.name) {
                        var result = "W";
                        if (summary.Home.R < summary.Visit.R) {
                            // visit team wins!
                            Conferences[c].divisions[d].teams[t].W++;
                            Conferences[c].divisions[d].teams[t].L--;
                        } else {
                            Conferences[c].divisions[d].teams[t].L++;
                            Conferences[c].divisions[d].teams[t].W--;
                            result = "L";
                        }
                        if (!Conferences[c].divisions[d].teams[t].History) {
                            Conferences[c].divisions[d].teams[t].History = [];
                        }
                        // put this game's result at the [0] index of the array.
                        Conferences[c].divisions[d].teams[t].History.unshift(result);
                    }
                }
            }
        }

        return Conferences;
    },

    updateWonLossRecords: function (lid, season, finalscore, callback) {

        var context = this;

        // 1) update the game
        Schedule.findOne({ LeagueId: lid, Season: season }, function (err, gameday) {

            for (g = 0; g < gameday.Games.length; g++) {
                if (gameday.Games[g].gameId == finalscore.Game.gameId) {
                    gameday.Games[g].extra.played = true;
                    gameday.Games[g].extra.homeScore = finalscore.Boxscore.summary.Home.R;
                    gameday.Games[g].extra.visitScore = finalscore.Boxscore.summary.Visit.R;
                    break;
                }
            }
            gameday.markModified("Games");
            gameday.save(function (err, response) {


                // 2) update the standings
                // update standings, then update the game
                Standings.findOne({ LeagueId: lid, Season: season }, function (err, leaguestandings) {
                    if (err) {
                        callback(null);
                    } else {
                        if (leaguestandings == null) {
                            // create a new file here!
                            context.createLeagueStandings(lid, season, function (err, newstandings) {
                                context.insertWLRecordsInStandings(newstandings, finalscore.Boxscore.summary);

                                var standingsObj = {
                                    LeagueId: lid,
                                    LeagueName: league.Name,
                                    Season: season,
                                    LastUpdate: new Date().toISOString(),
                                    Conferences: newstandings
                                }
                                Standings.update(
                                    { LeagueId: lid, Season: season },
                                    {
                                        Conferences: newStandings
                                    }, function (err, response) {
                                        callback(err);
                                    });
                            })
                        } else {
                            // update and save the current w/l records
                            context.insertWLRecordsInStandings(leaguestandings.Conferences, finalscore.Boxscore.summary);
                            leaguestandings.markModified("Conferences");
                            leaguestandings.LastUpdate = new Date().toISOString();
                            leaguestandings.save(function (err, response) {
                                callback(err);
                            })
                        }
                    }
                });
            });
        });
    },

    // this updates the score (in the schedule) no matter what
    // and the w/l records (in the standings) if the winner is different
    overwriteWonLossRecords: function (lid, season, finalscore, gameId, override, callback) {

        var context = this;

        // 1) update the game score in the schedule
        Schedule.findOne({ LeagueId: lid, Season: season }, function (err, gameday) {

            // find the game by id.
            for (g = 0; g < gameday.Games.length; g++) {
                if (gameday.Games[g].gameId == gameId) {

                    // found the game and need to update the score
                    gameday.Games[g].extra.played = true;
                    gameday.Games[g].extra.homeScore = finalscore.summary.Home.R;
                    gameday.Games[g].extra.visitScore = finalscore.summary.Visit.R;
                    break;
                }
            }
            gameday.markModified("Games");
            gameday.save(function (err, response) {


                // 2) update the standings if there's a change in outcome
                if (override && override.change && override.change == true) {
                    // update standings, then update the game
                    Standings.findOne({ LeagueId: lid, Season: season }, function (err, leaguestandings) {
                        if (err) {
                            callback(null);
                        } else {
                            if (leaguestandings == null) {
                                // create a new file here!
                                context.createLeagueStandings(lid, season, function (err, newstandings) {
                                    context.insertWLRecordsInStandings(newstandings, finalscore.summary);

                                    var standingsObj = {
                                        LeagueId: lid,
                                        LeagueName: league.Name,
                                        Season: season,
                                        LastUpdate: new Date().toISOString(),
                                        Conferences: newstandings
                                    }
                                    Standings.update(
                                        { LeagueId: lid, Season: season },
                                        {
                                            Conferences: newStandings
                                        }, function (err, response) {
                                            callback(err);
                                        });
                                })
                            } else {
                                // update and save the current w/l records
                                // by adding a win and subtracting a loss for the winner
                                // by adding a loss and subtracting a win for the loser
                                context.adjustWLRecordsInStandings(leaguestandings.Conferences, finalscore.summary);
                                leaguestandings.markModified("Conferences");
                                leaguestandings.LastUpdate = new Date().toISOString();
                                leaguestandings.save(function (err, response) {
                                    callback(err);
                                })
                            }
                        }
                    });
                } else {
                    // done as no change in the w/l record
                    callback(null);
                }
            });
        });
    }
}

