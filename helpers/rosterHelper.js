var utils = require('../helpers/utils');
var request = require('request');
var cheerio = require('cheerio');

var mongoose = require('mongoose');
var Team = mongoose.model('Team');
var Roster = mongoose.model('Roster');
var Player = mongoose.model('Player');
var Log = mongoose.model('Log');
var MasterPlayer = mongoose.model('MasterPlayer');


var playerHelper = require('../helpers/playerHelper');
var playerStatus = require('../helpers/playerStatus');
var startingStatsHelper = require('../helpers/getBattingStats');

var moment = require('moment-timezone');

module.exports = {
    getLineup: function (url, callback) {


        utils.scrapeUrl(url, function (error, response, html) {
            if (!error) {
                var $ = cheerio.load(html);

                var tables = $(".data.roster_table");
                var output = [];
                for (var index = 0; index < tables.length; index++) {

                    var data = tables[index];
                    var rows = $(data).find("tr");

                    for (var i = 1; i < rows.length; i++) {
                        output.push({
                            Number: $(rows[i]).find(".dg-jersey_number").text(),
                            Image: $(rows[i]).find(".dg-player_headshot img").attr("src"),
                            Name: $(rows[i]).find(".dg-name_display_first_last a").text(),
                            MlbId: $(rows[i]).find(".dg-name_display_first_last a").attr("href").split('/')[2],
                            Inactive: $(rows[i]).find(".dg-name_display_first_last a").hasClass("player-inactive"),
                            NotOnRoster: $(rows[i]).find(".dg-name_display_first_last a").hasClass("player-noton40man")
                        });
                    }
                }

                callback(output);
            }
        });
    },

    removeRostersForLeague: function (leagueId, callback) {
        Roster.remove({ 'LeagueId': leagueId }, function (err, result) {
            callback(err, result);
        });
    },

    putFreeAgentsIntoLeague: function (freeagents) {
        // drop in images and
        // clean up after inconsistant data entry
        for (i = 0; i < freeagents.length; i++) {
            var mid;

            // clean up sloppy data entry...
            if (freeagents[i]._doc.MLDId) {
                mid = freeagents[i]._doc.MLDId;
                freeagents[i]._doc["MlbId"] = mid;
            } else if (freeagents[i]._doc.MlbId) {
                mid = freeagents[i]._doc.MlbId;
                freeagents[i]._doc["MLBId"] = mid;
            }

            if (!freeagents[i]._doc.Image)
                freeagents[i]._doc["Image"] = freeagents[i]._doc.MlbId ? "http://mlb.mlb.com/mlb/images/players/head_shot/" + freeagents[i]._doc.MlbId + ".jpg" : "//rsportsbaseball.com/assets/img/avatars/avatar1.jpg";
            if (freeagents[i].Level == "MLB")
                freeagents[i].Level = "ML";

            freeagents[i]._doc["PlayerId"] = freeagents[i]._doc._id.toString();
            freeagents[i]._doc["Source"] = "FreeAgents";
            freeagents[i]._doc["Status"] = playerStatus.FreeAgent;
            freeagents[i]._doc["onFortyMan"] = false;


            // make sure they have salary structure and options
            freeagents[i]._doc["rSalary"] = //require('../helpers/playerSalaryObject').rSalary;
                {
                    AAV: "",
                    SigningBonus: "",
                    2018: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2019: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2020: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2021: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2022: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2023: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2024: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2025: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2026: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2027: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2028: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2029: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    "Details": "",
                    "Contract": "",
                    "Notes": "",
                    "FreeAgent": "",
                };
            freeagents[i]._doc["Options"] = {
                Options: freeagents[i]._doc["Options"],
                PriorOR: freeagents[i]._doc["PriorOR"],
                DraftExcluded: freeagents[i]._doc["DraftExcluded"],
                No40: freeagents[i]._doc["FortyMan"],
                Rule5: freeagents[i]._doc["RuleFiveEligibleYear"]
            };

        };
        return freeagents;
    },

    buildOneFreeAgent: function (rawP, contractExtras) {
        var newP = {
            "PlayerId": "",
            "Source": "MasterList",
            "MlbId": "",
            "Type" : "",
            "FullName": "",
            "FirstInitial": "",
            "FirstName": "",
            "LastName": "",
            "Age": "",
            "DOB": "",
            "MLS": "",
            "SigningYear": "",


            "Bats": "",
            "Throws": "",

            "Depth": "",
            "Position": "",

            "Image": "/assets/img/avatars/avatar1.jpg",

            "rSalary": // require('../helpers/playerSalaryObject').rSalary,
                {
                    AAV: "",
                    SigningBonus: "",
                    2018: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2019: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2020: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2021: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2022: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2023: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2024: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2025: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2026: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2027: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2028: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    2029: {
                        Salary: "",
                        Contract: "",
                        Buyout: ""
                    },
                    "Details": "",
                    "Contract": "",
                    "Notes": "",
                    "FreeAgent": "",
                },

            Options: {
                Options: "",
                PriorOR: "",
                DraftExcluded: "",
                No40: "",
                Rule5: ""
            },

            "Level": "R",
            "onFortyMan": false,
            "Status": 2, // inactive
            "TradeStatus": false,
            "StatusMsg": "",

            "BattingOrder9": -22,
            "StartinPosition9": -33,
            "BenchOrder9": -44,
            "BattingOrderDH": -1,
            "StartingPositionDH": -1,
            "BenchOrderDH": -1
        };


        if (rawP._id)
            newP.PlayerId = rawP._id.toString();
        if (rawP.MlbId)
            newP.MlbId = rawP.MlbId;
        if (rawP.MLBId)
            newP.MlbId = rawP.MLBId;

        if (newP.MlbId == "") {
            msg = "No MlbId";
        }

        if( rawP.NEW && rawP.NEW != "New" && rawP.New != "NEW ") {
            newP.Type = rawP.NEW;
        }

        newP.FullName = rawP.FullName;
        newP.FirstName = rawP.FirstName;
        newP.LastName = rawP.LastName;
        newP.FirstInitial = rawP.FirstName.slice(0, 1);
        newP.Age = rawP.Age;
        newP.DOB = rawP.DOB;
        newP.MLS = rawP.MLS;
        newP.SigningYear = rawP.SigningYear;

        newP.Position = rawP.Position;
        newP.Throws = rawP.Throws;
        newP.Bats = rawP.Bats;
        if (rawP.Image) {
            newP.Image = rawP.Image;
        } else {
            newP.Image = "//www.rsportsbaseball.com/assets/img/avatars/avatar1.jpg"
        }

        var level = rawP.Level;
        if (level == "MLB" || level == "ML") {
            newP.Level = "ML";
            newP.onFortyMan = true;
            newP.Status = 1; // active roster
        } else {
            // be sure to use the correct levels
            nonRosterLevels = {
                MLB: "ML",
                AAA: "Triple-A",
                AA: "Double-A",
                "A+": "High-A",
                "A-": "Low-A",
                RA: "ADV-RK",
                R: "RK",
                EST: "EST",
                DSL: "DSL"
            };
            if (nonRosterLevels[level]) {
                newP.Level = nonRosterLevels[level];
            }
        }

        newP.rSalary.AAV = rawP.AAV;
        newP.rSalary.SigningBonus = rawP.SigningBonus;

        for (y = 2018; y <= 2029; y++) {
            var si = "Salary" + y;
            var ci = "Contract" + y;
            var bi = "Buyout" + y;
            if (rawP[si])
                newP.rSalary[y].Salary = rawP[si];
            if (rawP[ci])
                newP.rSalary[y].Contract = rawP[ci];
            if (rawP[bi])
                newP.rSalary[y].Buyout = rawP[bi];
        }

        // add the contract extras if they exist
        // note it is an array!!!
        // grab the contract extras for rSports

        // var contractExtras = require('../helpers/contractExtras').rSports;

        if (contractExtras[newP.MlbId]) {
            if (newP.MlbId != "")
                newP.rSalary.Contract = contractExtras[newP.MlbId];
        }


        // add in the options/eligibility
        newP.Options.Options = rawP.Options;
        newP.Options.PriorOR = rawP.PriorOR;
        newP.Options.DraftExcluded = newP.DraftExcluded;
        newP.Options.No40 = newP.FortyMan;
        newP.Options.Rule5 = newP.RuleFiveEligibleYear;


        return newP;
    },

    buildRosters: function (league, index, teams, contractExtras, callback) {
        var context = this;
        if (index < teams.length) {
            var team = teams[index];
            var rTeam = team.r_name;
            MasterPlayer.find({ MlbTeam: rTeam }, function (err, players) {
                if (err) {
                    // utils.addLog("Error finding mlbteam: " + rTeam + ". Error: " + err.errmsg, 1, "rosterHelper", "buildRoster");
                    context.buildRosters(league, ++index, teams, contractExtras, callback);

                } else {
                    // create roster!
                    var newRoster = {
                        LeagueId: league._id,
                        TeamId: team._id,
                        TeamAbbr: team.r_abbreviation,
                        FortyManAL: [],
                        FortyManNL: [],
                        NonRoster: [],
                        CreatedUTC: new Date().toISOString()
                    };

                    nonRosterLevels = {
                        MLB: "ML",
                        AAA: "Triple-A",
                        AA: "Double-A",
                        "A+": "High-A",
                        "A-": "Low-A",
                        RA: "ADV-RK",
                        R: "RK",
                        EST: "EST",
                        DSL: "DSL"
                    };


                    for (i = 0; i < players.length; i++) {
                        var newP = {
                            "PlayerId": "",
                            "Source": "MasterList",
                            "MlbId": "",
                            "FullName": "",
                            "FirstInitial": "",
                            "FirstName": "",
                            "LastName": "",
                            "Age": "",
                            "DOB": "",
                            "MLS": "",
                            "SigningYear": "",


                            "Bats": "",
                            "Throws": "",

                            "Depth": "",
                            "Position": "",

                            "Image": "/assets/img/avatars/avatar1.jpg",

                            "rSalary": // require('../helpers/playerSalaryObject').rSalary,
                                {
                                    AAV: "",
                                    SigningBonus: "",
                                    2018: {
                                        Salary: "",
                                        Contract: "",
                                        Buyout: ""
                                    },
                                    2019: {
                                        Salary: "",
                                        Contract: "",
                                        Buyout: ""
                                    },
                                    2020: {
                                        Salary: "",
                                        Contract: "",
                                        Buyout: ""
                                    },
                                    2021: {
                                        Salary: "",
                                        Contract: "",
                                        Buyout: ""
                                    },
                                    2022: {
                                        Salary: "",
                                        Contract: "",
                                        Buyout: ""
                                    },
                                    2023: {
                                        Salary: "",
                                        Contract: "",
                                        Buyout: ""
                                    },
                                    2024: {
                                        Salary: "",
                                        Contract: "",
                                        Buyout: ""
                                    },
                                    2025: {
                                        Salary: "",
                                        Contract: "",
                                        Buyout: ""
                                    },
                                    2026: {
                                        Salary: "",
                                        Contract: "",
                                        Buyout: ""
                                    },
                                    2027: {
                                        Salary: "",
                                        Contract: "",
                                        Buyout: ""
                                    },
                                    2028: {
                                        Salary: "",
                                        Contract: "",
                                        Buyout: ""
                                    },
                                    2029: {
                                        Salary: "",
                                        Contract: "",
                                        Buyout: ""
                                    },
                                    "Details": "",
                                    "Contract": "",
                                    "Notes": "",
                                    "FreeAgent": "",
                                },

                            Options: {
                                Options: "",
                                PriorOR: "",
                                DraftExcluded: "",
                                No40: "",
                                Rule5: ""
                            },

                            "Level": "R",
                            "onFortyMan": false,
                            "Status": 2, // inactive
                            "TradeStatus": false,
                            "StatusMsg": "",

                            "BattingOrder9": -22,
                            "StartinPosition9": -33,
                            "BenchOrder9": -44,
                            "BattingOrderDH": -1,
                            "StartingPositionDH": -1,
                            "BenchOrderDH": -1
                        };



                        var rawP = players[i]._doc;

                        newP.PlayerId = rawP._id.toString();
                        if (rawP.MlbId)
                            newP.MlbId = rawP.MlbId;
                        if (rawP.MLBId)
                            newP.MlbId = rawP.MLBId;

                        if (newP.MlbId == "") {
                            msg = "No MlbId";
                        }
                        newP.FullName = rawP.FullName;
                        newP.FirstName = rawP.FirstName;
                        newP.LastName = rawP.LastName;
                        newP.FirstInitial = rawP.FirstName.slice(0, 1);
                        newP.Age = rawP.Age;
                        newP.DOB = rawP.DOB;
                        newP.MLS = rawP.MLS;
                        newP.SigningYear = rawP.SigningYear;

                        newP.Position = rawP.Position;
                        newP.Throws = rawP.Throws;
                        newP.Bats = rawP.Bats;
                        if (rawP.Image) {
                            newP.Image = rawP.Image;
                        } else {
                            newP.Image = "//www.rsportsbaseball.com/assets/img/avatars/avatar1.jpg"
                        }

                        var level = rawP.Level;


                        if (level == "MLB" || level == "ML") {
                            newP.Level = "ML";
                            newP.onFortyMan = true;
                            newP.Status = 1; // active roster
                        } else {
                            // be sure to use the correct levels

                            if (level in nonRosterLevels) {
                                newP.Level = nonRosterLevels[level];
                            } else {
                                newP.Level = "RK";
                            }
                        }

                        newP.rSalary.AAV = rawP.AAV;
                        newP.rSalary.SigningBonus = rawP.SigningBonus;

                        for (y = 2018; y <= 2029; y++) {
                            var si = "Salary" + y;
                            var ci = "Contract" + y;
                            var bi = "Buyout" + y;
                            if (rawP[si])
                                newP.rSalary[y].Salary = rawP[si];
                            if (rawP[ci])
                                newP.rSalary[y].Contract = rawP[ci];
                            if (rawP[bi])
                                newP.rSalary[y].Buyout = rawP[bi];
                        }

                        // add the contract extras if they exist
                        // note it is an array!!!
                        // grab the contract extras for rSports

                        // var contractExtras = require('../helpers/contractExtras').rSports;

                        if (contractExtras[newP.MlbId]) {
                            if (newP.MlbId != "")
                                newP.rSalary.Contract = contractExtras[newP.MlbId];
                        }


                        // add in the options/eligibility
                        newP.Options.Options = rawP.Options;
                        newP.Options.PriorOR = rawP.PriorOR;
                        newP.Options.DraftExcluded = newP.DraftExcluded;
                        newP.Options.No40 = newP.FortyMan;
                        newP.Options.Rule5 = newP.RuleFiveEligibleYear;

                        // have the player.. push them onto the array that matters!
                        if (newP.Level == "ML") {
                            newRoster.FortyManAL.push(newP);
                            newRoster.FortyManNL.push(newP);
                        } else {
                            newRoster.NonRoster.push(newP);
                        }


                    }
                    Roster.create(newRoster, function (err, roster) {
                        if (err) {
                            //callback(err, null);
                            // utils.addLog("Error creating a roster for: " + newRoster.LeagueId + " - " + newRoster.TeamId + ". Error: " + err.errmsg, 1, "rosterHelper", "buildRoster");

                        }
                        context.buildRosters(league, ++index, teams, contractExtras, callback);

                    });

                }
            });
        }
        else {
            callback();
        }
    },

    buildRosterFromMaster: function (league, team, callback) {
        var rTeam = team.r_name;
        MasterPlayer.find({ MlbTeam: rTeam }, function (err, players) {
            if (err) {
                callback(err, "no team");
            } else {
                // create roster!
                var newRoster = {
                    LeagueId: league._id,
                    TeamId: team._id,
                    TeamAbbr: team.r_abbreviation,
                    FortyManAL: [],
                    FortyManNL: [],
                    NonRoster: [],
                    CreatedUTC: new Date().toISOString()
                };

                nonRosterLevels = {
                    MLB: "ML",
                    AAA: "Triple-A",
                    AA: "Double-A",
                    "A+": "High-A",
                    "A-": "Low-A",
                    RA: "ADV-RK",
                    R: "RK",
                    EST: "EST",
                    DSL: "DSL"
                };


                for (i = 0; i < players.length; i++) {
                    var newP = {
                        "PlayerId": "",
                        "MlbId": "",
                        "FullName": "",
                        "FirstInitial": "",
                        "FirstName": "",
                        "LastName": "",
                        "Age": "",
                        "MLS": "",


                        "Bats": "",
                        "Throws": "",

                        "Depth": "",
                        "Position": "",

                        "Image": "/assets/img/avatars/avatar1.jpg",

                        "rSalary": {
                            AAV: "",
                            SigningBonus: "",
                            2018: {
                                Salary: "",
                                Contract: "",
                                Buyout: ""
                            },
                            2019: {
                                Salary: "",
                                Contract: "",
                                Buyout: ""
                            },
                            2020: {
                                Salary: "",
                                Contract: "",
                                Buyout: ""
                            },
                            2021: {
                                Salary: "",
                                Contract: "",
                                Buyout: ""
                            },
                            2022: {
                                Salary: "",
                                Contract: "",
                                Buyout: ""
                            },
                            2023: {
                                Salary: "",
                                Contract: "",
                                Buyout: ""
                            },
                            2024: {
                                Salary: "",
                                Contract: "",
                                Buyout: ""
                            },
                            2025: {
                                Salary: "",
                                Contract: "",
                                Buyout: ""
                            },
                            2026: {
                                Salary: "",
                                Contract: "",
                                Buyout: ""
                            },
                            2027: {
                                Salary: "",
                                Contract: "",
                                Buyout: ""
                            },
                            2028: {
                                Salary: "",
                                Contract: "",
                                Buyout: ""
                            },
                            2029: {
                                Salary: "",
                                Contract: "",
                                Buyout: ""
                            },
                            "Details": "",
                            "Contract": "",
                            "Notes": "",
                            "FreeAgent": ""
                        },

                        Options: {
                            Options: "",
                            PriorOR: "",
                            DraftExcluded: "",
                            No40: "",
                            Rule5: ""
                        },

                        "Level": "R",
                        "onFortyMan": false,
                        "Status": 2, // inactive
                        "TradeStatus": false,
                        "StatusMsg": "",

                        "BattingOrder9": -99,
                        "StartinPosition9": -99,
                        "BenchOrder9": -99,
                        "BattingOrderDH": -99,
                        "StartingPositionDH": -99,
                        "BenchOrderDH": -99
                    };


                    // grab the contract extras for rSports
                    var contractExtras = require('../helpers/contractExtras').rSports;

                    var rawP = players[i]._doc;


                    newP.PlayerId = rawP._id.toString();
                    if (rawP.MlbId)
                        newP.MlbId = rawP.MlbId;
                    if (rawP.MLBId)
                        newP.MlbId = rawP.MLBId;
                    newP.FullName = rawP.FullName;
                    newP.FirstName = rawP.FirstName;
                    newP.LastName = rawP.LastName;
                    newP.FirstInitial = rawP.FirstName.slice(0, 1);
                    newP.Age = rawP.Age;
                    newP.MLS = rawP.MLS;

                    newP.Position = rawP.Position;
                    newP.Throws = rawP.Throws;
                    newP.Bats = rawP.Bats;
                    if (rawP.Image) {
                        newP.Image = rawP.Image;
                    } else {
                        newP.Image = "/assets/img/avatars/avatar1.jpg"
                    }

                    var level = rawP.Level;
                    if (level == "MLB" || level == "ML") {
                        newP.Level = "ML";
                        newP.onFortyMan = true;
                        newP.Status = 1; // active roster
                    } else {
                        // be sure to use the correct levels

                        if (level in nonRosterLevels) {
                            newP.Level = nonRosterLevels[level];
                        } else {
                            newP.Level = "RK";
                        }
                    }

                    newP.rSalary.AAV = rawP.AAV;
                    newP.rSalary.SigningBonus = rawP.SigningBonus;

                    for (y = 2018; y <= 2029; y++) {
                        var si = "Salary" + y;
                        var ci = "Contract" + y;
                        var bi = "Buyout" + y;
                        if (rawP[si])
                            newP.rSalary[y].Salary = rawP[si];
                        if (rawP[ci])
                            newP.rSalary[y].Contract = rawP[ci];
                        if (rawP[bi])
                            newP.rSalary[y].Buyout = rawP[bi];
                    }

                    // add the contract extras if they exist
                    // note it is an array!!!
                    if (contractExtras[newP.MlbId]) {
                        if (newP.MlbId != "")
                            newP.rSalary.Contract = contractExtras[newP.MlbId];
                    }

                    // add in the options/eligibility
                    newP.Options.Options = rawP.Options;
                    newP.Options.PriorOR = rawP.PriorOR;
                    newP.Options.DraftExcluded = newP.DraftExcluded;
                    newP.Options.No40 = newP.FortyMan;
                    newP.Options.Rule5 = newP.RuleFiveEligibleYear;

                    // have the player.. push them onto the array that matters!
                    if (newP.Level == "ML") {
                        newRoster.FortyManAL.push(newP);
                        newRoster.FortyManNL.push(newP);
                    } else {
                        newRoster.NonRoster.push(newP);
                    }


                }
                Roster.create(newRoster, function (err, roster) {
                    if (err) {
                        callback(err, null);
                        utils.addLog("Error creating a roster for: " + newRoster.LeagueId + " - " + newRoster.TeamId + ". Error: " + err.errmsg, 1, "rosterHelper", "buildRosterFromMaster");

                    } else {
                        callback(null, team.Name);
                        utils.addLog("Created a roster for: " + newRoster.LeagueId + " - " + newRoster.TeamId, 0, "rosterHelper", "buildRosterFromMaster");

                    }

                });

            }
        });

    },

    // ******************
    // for scraping version
    // ******************************
    saveFortyManLineup: function (league, team, callback) {
        var url = "http://m.angels.mlb.com/" + team.abbreviation.toLowerCase() + "/roster/40-man/";
        this.getLineup(url, function (lineup) {
            var bench = lineup.filter(function (value) {
                return value.Inactive == false;
            });
            var dlTen = lineup.filter(function (value) {
                return value.Inactive == true;
            });
            var dlSixty = lineup.filter(function (value) {
                return value.NotOnRoster == true;
            });
            var roster = {
                LeagueId: league._id,
                TeamId: team._id,
                FortyMan: lineup,
                Bench: bench,
                DLTen: dlTen,
                DLSixty: dlSixty,
                Bullpen: [],
                Active: [],
                StartingRotation: [],
                WaiverWire: [],
                PlayerData: [],
                Positions: []
            };
            Roster.create(roster, function (err, lineup) {
                callback(err, lineup);
            })
        });
    },

    getRosterPositions: function (league, team, callback) {

        var query = {
            $and: [
                { LeagueId: league._id },
                { TeamId: team._id }
            ]
        };
        Roster.findOne(query, function (err, roster) {
            if (err) {
                console.log(err.errmsg);
            }
            else if (roster) {
                /// use same code from scraper!
                var output = [];

                players = roster.FortyManNL;
                var positionList = ["SP", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "DH", "RP", "CL"];
                for (var pos = 0; pos < positionList.length; pos++) {
                    var position = positionList[pos];
                    var data = {
                        Position: position,
                        Players: []
                    }
                    var rows = new Array(10);

                    for (i = 0; i < players.length; i++) {
                        var nextPlayer = players[i];

                        // ((((((((((((( TODO fix this or abandon it)))))))))))))
                        var depth = "";
                        if (nextPlayer.RDepth) {
                            depth = nextPlayer.RDepth;
                        }
                        if (!depth || depth.length < 2) {
                            if (nextPlayer.MlbDepth)
                                depth = nextPlayer.MlbDepth;
                        }

                        var dpos = depth.substring(0, depth.length - 1);
                        var ddepth = depth.substring(depth.length - 1) - 1;

                        if (dpos == position) {
                            if (rows[ddepth]) {
                                // then occupied, find the first empty row
                                for (dd = 0; dd < rows.length; dd++) {
                                    if (rows[dd] == null) {
                                        ddepth = dd;
                                        break;
                                    }
                                }
                            }
                            rows[ddepth] = nextPlayer;
                        }

                    }

                    data.Players = rows;
                    output.push(data);
                }
                callback(output);

            }
            else {
                callback([]);
            }
        });
    },

    updateRosterPositions: function (league, team, positions, callback) {

        var query = {
            $and: [
                { LeagueId: league._id },
                { TeamId: team._id }
            ]
        };
        Roster.findOne(query, function (err, roster) {
            if (err) {
                console.log(err.errmsg);
            }
            else if (roster) {
                roster.Positions = positions;
                roster.markModified("Positions");
                roster.save(function (err, response) {
                    if (err) {
                        console.log(err.message);
                    }
                    else {
                        callback();
                    }
                });
            }
            else {
                callback();
            }
        });
    },

    savePlayer: function (index, leagueId, teamId) {
        var context = this;
        if (this.playerData.length > index) {
            var player = this.playerData[index].MlbId;
            Player.findOne({ MlbId: player }, function (err, player) {
                if (err) {

                }
                else if (player) {
                    var query = {
                        $and: [
                            { LeagueId: leagueId },
                            { TeamId: teamId }
                        ]
                    };
                    Roster.findOne(query, function (err, roster) {
                        if (err) {
                            console.log(err.errmsg);
                        }
                        else if (roster) {
                            roster.PlayerData.push(player);
                            roster.markModified("PlayerData");
                            roster.save(function (err, response) {
                                if (err) {
                                    console.log(err.message);
                                }
                                context.savePlayer(++index, leagueId, teamId);
                            });
                        }
                    });
                }
            });
        }
    },

    playerData: [],

    getPlayerData: function (lid, tid, players) {
        var context = this;
        this.playerData = players;
        this.savePlayer(0, lid, tid);
    },

    getActiveLineup: function (team, callback) {
        var url = "/roster/";
        return this.getLineup(team.abbreviation, url);
    },

    salaries: [],
    output: [],
    baseData: [],

    getSalaryHistory: function (output, callback) {
        this.salaries = output.Active;
        this.baseData = output;

        this.getSalaryData(0, callback);
    },

    getSalaryData(index, callback) {
        if (index < this.salaries.length) {
            var url = this.salaries[index].Url;
            var context = this;
            utils.scrapeUrl(url, function (error, response, html) {
                if (!error) {
                    var $ = cheerio.load(html);

                    var contractCells = $(".cd-tabs-content .playercontracttable .salaryRow");
                    var contractHistory = [];

                    for (var i = 1; i < contractCells.length; i++) {
                        var row = contractCells[i];
                        if ($($(row).find("td")[0]).text().trim().length > 0) {
                            contractHistory.push({
                                Year: $($(row).find("td")[0]).text().trim(),
                                Age: $($(row).find("td")[2]).text().trim(),
                                Base: $($(row).find("td")[3]).text().trim(),
                                Bonus: $($(row).find("td")[4]).text().trim(),
                                Total: $($(row).find("td")[5]).text().trim(),
                                Adjusted: $($(row).find("td")[6]).text().trim()
                            });
                        }
                    }

                    if (context.salaries[index]) {
                        context.output.push({
                            Name: context.salaries[index].Name,
                            Salary: context.salaries[index].Salary,
                            Url: context.salaries[index].Url,
                            PlayerType: context.salaries[index].PlayerType,
                            Team: context.baseData.Team,
                            Abbreviation: context.baseData.Abbreviation,
                            Description: $(".cd-tabs-content .currentinfo").text(),
                            SalaryInfo: {
                                Contract: $($(".cd-tabs-content .salaryTable td")[0]).text(),
                                Bonus: $($(".cd-tabs-content .salaryTable td")[1]).text(),
                                Average: $($(".cd-tabs-content .salaryTable td")[2]).text(),
                                FreeAgent: $($(".cd-tabs-content .salaryTable td")[3]).text()
                            },
                            ContractHistory: contractHistory,
                        });
                    }

                    context.getSalaryData(++index, callback);
                }
                else {
                    context.getSalaryData(++index, callback);
                }
            });
        }
        else {
            var isReset = false;
            if (this.baseData.Active) {
                if (!this.baseData.Active[0].Description) {
                    this.baseData.Active = this.output;
                    this.salaries = this.baseData.Injured;
                    isReset = true;
                }
                else if (!this.baseData.Injured[0].Description) {
                    this.baseData.Injured = this.output;
                    this.salaries = this.baseData.Retained;
                    isReset = true;
                }
                else if (!this.baseData.Retained[0].Description) {
                    this.baseData.Retained = this.output;
                    this.salaries = this.baseData.Minors;
                    isReset = true;
                }

                if (!isReset) {
                    callback(this.output);
                }
                else {
                    this.getSalaryData(0, callback);
                }
            }
            else {
                callback(this.output);
            }
        }
    },

    updatePlayerSalary: function (data, callback) {
        this.playerData = data;
        this.updatePlayerData(0, callback);
    },

    updatePlayerData: function (index, callback) {
        var context = this;
        if (this.playerData[index]) {
            Player.findOne({ FullName: this.playerData[index].Name, MlbTeam: this.playerData[index].Abbreviation }, function (err, player) {
                if (err) {
                    context.updatePlayerData(++index, callback);
                }
                else if (player) {
                    player.Salary = context.playerData[index];
                    player.markModified("Salary");
                    player.save(function (err, response) {
                        context.updatePlayerData(++index, callback);
                    });
                }
                else if (!player && context.playerData.length > index) {
                    Log.create({
                        Message: "player not found when matching salary: " + context.playerData[index].Name + " MlbTeam: " + context.playerData[index].Abbreviation,
                        Level: "Error",
                        Route: "Srape",
                        Method: "updatePlayerData",
                        CreatedUTC: new Date().toISOString()
                    });

                    context.updatePlayerData(++index, callback);
                }
                else {
                    callback();
                }
            });
        }
        else {
            //context.updatePlayerData(++index, callback);
            callback();
        }
    },

    updateTeamRoster: function (output, callback) {
        this.playerData = output;
        this.updatePlayerTeam(0, callback);
    },

    updatePlayerTeam: function (index, callback) {
        var context = this;
        if (this.playerData[index]) {
            Player.findOne({ MlbId: this.playerData[index].MlbId }, function (err, player) {
                if (err) {
                    context.updatePlayerTeam(++index, callback);
                }
                else if (player) {
                    player.Number = context.playerData[index].Number;
                    player.Level = "ML";
                    player.Image = context.playerData[index].Image;
                    player.MlbTeam = context.playerData[index].Abbreviation;
                    player.save(function (err, response) {
                        context.updatePlayerTeam(++index, callback);
                    });
                }
                else {
                    context.updatePlayerTeam(++index, callback);
                }
            });
        }
        else {
            callback();
        }
    },

    getDepthChartTemplate: function () {
        return [
            { Position: "CA", Pos: "C", Players: [] },
            { Position: "1B", Pos: "1B", Players: [] },
            { Position: "2B", Pos: "2B", Players: [] },
            { Position: "3B", Pos: "3B", Players: [] },
            { Position: "SS", Pos: "SS", Players: [] },
            { Position: "LF", Pos: "LF", Players: [] },
            { Position: "CF", Pos: "CF", Players: [] },
            { Position: "RF", Pos: "RF", Players: [] },
            { Position: "SP", Pos: "SP", Players: [] },
            { Position: "RP", Pos: "RP", Players: [] },
            { Position: "CL", Pos: "CL", Players: [] },
            { Position: "DH", Pos: "DH", Players: [] },
        ];
    },

    getPlayersByPosition: function (position, players) {
        return !players ? [] : players.filter(function (value) {
            var isMatch = false;
            if (value.Primary && value.Primary.length > 0) {
                isMatch = !isMatch ? value.Primary[0] == position : isMatch;
            }
            if (value.Secondary && value.Secondary.length > 0) {
                isMatch = !isMatch ? value.Secondary[0] == position : isMatch;
            }
            if (value.Tertiary && value.Tertiary.length > 0) {
                isMatch = !isMatch ? value.Tertiary[0] == position : isMatch;
            }
            return isMatch;
        });
    },

    getMasterPlayerPosition: function (index, roster, callback) {
        var context = this;
        if (!roster || !roster.Players || !roster.Players[index]) {
            callback(roster);
        }
        else {
            var qry = { MlbId: roster.Players[index].MlbId };
            Player.findOne(qry, function (err, item) {
                if (item) {
                    roster.Players[index].Position = item.Position;
                }
                context.getMasterPlayerPosition(++index, roster, callback);
            });
        }
    },

    createRosterLineupPlayer: function (leagueId, player, season) {
        var newRLP = {
            LeagueId: leagueId,
            PlayerId: player.PlayerId,
            MlbId: player.MlbId,
            Status: player.Status,
            Level: player.Level,
            FullName: player.FullName,
            Season: season,
            /* examples...
            FullGames: ["20170404", "20170406"],
            PartialGames: {"20170405": {id:"1234", paTotal: 6, paUsed:3, ipTotal: 4, ipUsed:0} , "20170409": {id:"1234", paTotal: 9, paUsed:3, ipUsed:0} },
            */
            FullGames: [],
            PartialGames: {},

            CanCatch: false, // note this only means the player isn't prohibited by the 10 day rule
            CanStart: false,  // only means pitcher isn't prohibitied by the 4 days rest rule
            CanRelieve: false,   // only means pitcher isn't prohibited from relieving by rules
            FirstFullOffensiveGame: "",
            FirstFullPitchingGame: ""
        }
        return (newRLP);
    },

    separateRosterIntoAffiliates: function (roster) {
        var OwnedPlayers = null;
        if (roster) {
            // we'll be checking if there are missing elements or
            // players in the wrong places.  if so, we'll need to 
            // update AFTER we send the results back to the caller
            var updateNeeded = false;
            var activeCount = 0;
            var fortyManCount = 0;

            // create list of roster players for coming compare
            var fortyman = roster.FortyManNL;
            var otherRoster = roster.FortyManAL;
            /*
            if (req.query.league && req.query.league == "al") {
                fortyman = roster.FortyManAL;
                otherRoster = foster.FortyManNL;
            }
            */

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
                // rosterPlayer["StatusMsg"] = playerHelper.getPlayerStatusMsg(rosterPlayer);
                if (rosterPlayer.Status == playerStatus.ActiveRoster)
                    activeCount++;
            }


            OwnedPlayers = {
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
                    } else if (owned[op].TradeStatus && owned[op].TradeStatus >= playerStatus.TradeWaivers && owned[op].TradeStatus < playerStatus.TradeWaiversEnd) {
                        tradeWaiverPlayers.push(owned[op]);
                    } else {
                        // add the player to the right nonRoster list
                        if (OwnedPlayers.nonroster[nextLevel]) {
                            OwnedPlayers.nonroster[nextLevel].push(owned[op]);
                        } else {
                            OwnedPlayers.nonroster["Unknown"].push(owned[op]);
                        }
                    }

                   // owned[op]["StatusMsg"] = playerHelper.getPlayerStatusMsg(owned[op]);

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
            OwnedPlayers.nonroster["MDL"] = mdl;


            // add in the tradewaiver players
            OwnedPlayers.tradewaivers = tradeWaiverPlayers;


            // add in the nonActive players on the 40Man roster
            OwnedPlayers.fortyManCount += fortyManCount;

            // **************** finally, attempt to clean up the non-roster ML players ************

        }
        return (OwnedPlayers);

    },

    nonRosterLevels: [
        "Triple-A", "Double-A", "High-A", "Low-A",
        "ADV-RK", "RK", "EST", "DSL", "MDL"],

    affiliateKeys: [
        "AAA", "AA", "A-High", "A-Low",
        "Rookie-Advanced", "Rookie", "EST", "DSL", "MDL"],
    shortKeys: [
        "AAA", "AA", "A+", "A-",
        "RA", "R", "EST", "DSL", "MDL"],

    nonRosterLevelNames: [
        "(Triple-A)",
        "(Double-A)",
        "(High Single-A)",
        "(Low Single-A)",
        "(Advanced Rookie Ball)",
        "(Rookie Ball)",
        "(Extended Spring Training)",
        "(Dominican Summer League)",
        "Minor League Disabled List"
        /*   , "Major League - Unassaigned - For testing validation. Should have no players."
        */
    ],

    checkRosterSizes: function (index, league, settings, messageArray, callback) {
        var context = this;
        if (index >= league.Teams.length) {
            callback(null, messageArray);
        } else {
            var team = league.Teams[index];
            var teamId = team._id;
            var teamRName = team.r_name;

            Roster.findOne({ LeagueId: league._id, TeamId: teamId }, function (err, roster) {
                if (err || !roster) {
                    context.checkRosterSizes(++index, league, messageArray, callback);
                } else {
                    roster["TeamName"] = teamRName;
                    var OP = context.separateRosterIntoAffiliates(roster);

                    // cycle through the teams and generate messages as needed...
                    var leagueList = OP.levels;

                    // ****** create error message for problems with MY TEAM's roster
                    // only 25/40 on ML, 25 on minor leage, 35 on rookie league.. don't count DL in minors/rookies

                    var activeSize = 0;
                    for (i = 0; i < OP.roster.length; i++) {
                        var status = OP.roster[i].Status;
                        if (status == playerStatus.ActiveRoster) {
                            activeSize++;
                        }
                    }
                    selectedActiveRosterSize = activeSize;


                    var msg = "";
                    var newMessages = [];
                    var teamName = teamRName;

                    // set up limits.. 
                    var maxMLRoster = 25;
                    if (settings["ActiveRosterSize"])
                        maxMLRoster = settings["ActiveRosterSize"];
                    var maxMinorRoster = 25;
                    if (settings["MinorRosterSize"])
                        maxMinorRoster = settings["MinorRosterSize"];
                    var maxRookieRoster = 25;
                    if (settings["MinorRookieSize"])
                        maxRookieRoster = settings["MinorRookieSize"];


                    myActiveRosterSize = activeSize;
                    if (maxMLRoster != "unlimited" && activeSize > maxMLRoster) {
                        msg = "Too many players on Major League roster. You can only have " + maxMLRoster + ". You have " + activeSize + '.\n';
                        newMessages.push({ type: "message", msg: msg, shortMsg: "Active: " + activeSize + "/" + maxMLRoster });
                    } else {
                        if( activeSize < maxMLRoster ) {
                            msg = "Too few players on Major League roster. You must have " + maxMLRoster + ". You have " + activeSize + '.\n';
                            newMessages.push({ type: "message", msg: msg, shortMsg: "Active: " + activeSize + "/" + maxMLRoster });
                 
                        }
                    }

                    if (leagueList["40-Man"].size > 40) {
                        msg = "Too many players on your 40 Man roster. You have " + leagueList["40-Man"].size + '.\n';
                        newMessages.push({ type: "message", msg: msg, shortMsg: "40-Man: " + leagueList["40-Man"].size + "/40" });
                    }

                    // now loop through the minors
                    for (i = 0; i < 4; i++) {
                        var levelname = context.nonRosterLevels[i];
                        var levelteams = team.affiliates[context.affiliateKeys[i]];
                        if (levelteams && levelteams != "") {
                            var numTeams = levelteams.split(",").length;
                        } else {
                            numTeams = 0;
                        }
                        if (maxMinorRoster != "unlimited" && OP.nonroster[levelname] && OP.nonroster[levelname].length > (numTeams * maxMinorRoster)) {
                            msg = "Too many players on " + levelname + "  roster. You can only have " + numTeams * maxMinorRoster + ". You have " + OP.nonroster[levelname].length + '.\n';
                            newMessages.push({ type: "message", msg: msg, shortMsg: context.shortKeys[i] + ": " + OP.nonroster[levelname].length + "/" + (numTeams * maxMinorRoster) });
                        }
                    }

                    // now loop through the rookies
                    for (i = 4; i < 8; i++) {
                        var levelname = context.nonRosterLevels[i];
                        var levelteams = team.affiliates[context.affiliateKeys[i]];
                        if (levelteams && levelteams != "") {
                            var numTeams = levelteams.split(",").length;
                        } else {
                            numTeams = 0;
                        }
                        if (maxRookieRoster != "unlimited" && OP.nonroster[levelname] && OP.nonroster[levelname].length > (numTeams * maxRookieRoster)) {
                            msg = "Too many players on " + levelname + "  roster. You can only have " + numTeams * maxRookieRoster + ". You have " + OP.nonroster[levelname].length + '.\n';
                            newMessages.push({ type: "message", msg: msg, shortMsg: context.shortKeys[i] + ": " + OP.nonroster[levelname].length + "/" + (numTeams * maxRookieRoster) });
                        }
                    }
                    if (newMessages.length > 0) {
                        messageArray.push({ leagueId: league._id, teamName: teamRName, teamId: teamId, messages: newMessages });
                    }
                    context.checkRosterSizes(++index, league, settings, messageArray, callback);
                }
            });
        }
    },

    calculateBattingLW: function (stats) {
        lw = 0;

        if (typeof (stats) == "undefined" || !stats) {
            lw = 0;
        } else {
            /*
                "LW" : {
                    "name": "linear weight",
                    "value" : [0.46,0.8,1.02,1.4,0.146,0.2,0.33,0.33, -0.073, 0.3, -0.6, -0.46,-0.25],
                    "notes" : "Calculated x 1B, 2B, 3B, HR, SAC, SF, HBP, BB, K, SB, CS, GIDP, OUTS"
                    */
            // shortcut...
            alw = algorithms.Batting.LW;
            columns = alw.columns;
            value = alw.value;
            lw = 0;
            var outs = 0;
            if (stats.AB && stats.H)
                outs = stats.AB - stats.H;
            stats.OUTS = outs;
            for (var c = 0; c < columns.length; c++) {

                // raw stats
                var s = stats[columns[c]];
                if (s && !isNaN(s))
                    lw += s * value[c];
            }


            // **************************** MLE CALCULATION ******************************************** //
            var useMLE = false;
            if (useMLE) {
                if (lw > 0) {
                    lw = lw / 4;
                } else {
                    lw = lw * 2;
                }

                var hasHittingCoorindator = false;
                if (hasHittingCoorindator) {
                    // 15% boost
                    lw = lw * 1.15;
                }
            }

            // **************************** Store Final Calculation ******************************************** //

        }
        return lw;

    },

    calculateFieldingLW: function (stats, defensivePlayer) {
        if (!defensivePlayer.Primary && !defensivePlayer.Secondary && !defensivePlayer.Tertiary) {
            var position = "DH";
        } else {
            var position = defensivePlayer.Primary.length > 0 ? defensivePlayer.Primary[0] : defensivePlayer.Secondary > 0 ? defensivePlayer.Secondary[0] : defensivePlayer.Tertiary.length > 0 ? defensivePlayer.Tertiary[0] : "P";
        }
        if (position == "DH")
            return null;

        var positionIndex = algorithms.Positions[position];

        lw = 0;

        if (typeof (stats) == "undefined" || !stats) {
            lw = 0;
        } else {

            // shortcut...
            var useMLE = false;

            if (positionIndex == 1) {
                alw = algorithms.Fielding.LWCatcher;
                if (useMLE)
                    alw = algorithms.Fielding.LWCatcherMLE;
                value = alw.value;
            } else {
                alw = algorithms.Fielding.LW;
                value = alw.value[positionIndex];
            }
            columns = alw.columns;

            lw = 0;
            for (var c = 0; c < columns.length; c++) {

                // raw stats
                var s = stats[columns[c]];
                lw += s * value[c];
            }

            if (useMLE) {

                if (lw > 0) {
                    lw = lw / 4;
                } else {
                    lw = lw * 2;
                }

                var hasInfieldCoordinator = false;
                if (positionIndex < 6 && hasInfieldCoordinator) {
                    lw = lw * 1.15;
                }
                var hasOutfieldCoordinator = false;
                if (positionIndex > 5 && hasOutfieldCoordinator) {
                    lw = lw * 1.15;
                }

            }

        }
        return lw;
    },


    calculateDefensiveStats: function (fstats, defensivePlayer, fieldFI) {
        if (!defensivePlayer.Primary && !defensivePlayer.Secondary && !defensivePlayer.Tertiary) {
            var position = "P";
        } else {
            var position = defensivePlayer.Primary.length > 0 ? defensivePlayer.Primary[0] : defensivePlayer.Secondary > 0 ? defensivePlayer.Secondary[0] : defensivePlayer.Tertiary.length > 0 ? defensivePlayer.Tertiary[0] : "P";
        }
        var positionIndex = algorithms.Positions[position];

        var returnStats = { cDef: "", cERA: 0, FieldFI: fieldFI, Block: 0, Frame: 0, FZ: 0 };
        var alw = algorithms.Fielding;
        if (fstats) {

            var useMLE = false;

            // catchers are treated differently.
            if ((position && position == "CA") || (!position && fstats.Pos == 2)) {

                if (!useMLE) {
                    var innings = fstats.INN;
                    var inningFraction = innings % 1 == 0.2 ? 0.666 : innings % 1 == 0.1 ? 0.333 : 0;
                    innings = Math.floor(innings) + inningFraction;

                    if (innings > 0) {
                        var cBlockingRuns = defensivePlayer.YTDcBlockingRuns ? defensivePlayer.YTDcBlockingRuns : 0;
                        var cFramingRuns = defensivePlayer.YTDcFramingRuns ? defensivePlayer.YTDcFramingRuns : 0;
                        var daysOfSeason = alw.avgCatcherInnings.value;
                        returnStats.Block = cBlockingRuns / ((daysOfSeason * 9) / innings);
                        returnStats.Frame = cFramingRuns / ((daysOfSeason * 9) / innings);

                    } else {
                        returnStats.Block = 0;
                        returnStats.Frame = 0;

                    }
                    // cERA
                    // from Tory   CERA =((leaguePitchingAverage-cERA)*(10))*(INN/typicalInningsPerCatcherPerYear)
                    var avgERA = algorithms.Pitching.ERA.value;
                    var cera = (avgERA - fstats.Cera) * 10;
                    var typicalInnings = algorithms.Fielding.avgCatcherInnings.value;

                    returnStats.cERA = cera * (innings / typicalInnings);


                    // catchers don't get the zone value
                } else {
                    // using the MLE
                    returnStats.Block = -0.25;
                    returnStats.Frame = -0.25;
                    returnStats.cERA = -0.25;

                    var hasCatchingCoordinator = false;
                    if (hasCatchingCoordinator) {
                        returnStats.Block = 0;
                        returnStats.Frame = 0;
                        returnStats.cERA = 0;
                    }
                }
                returnStats.Zone = 0;

            } else {
                // all other fielders

                // fielders don't get these stats
                returnStats.Block = "";
                returnStats.Frame = "";
                returnStats.cERA = "";

                var zone = 0;

                if (fstats.OutsOutOfZone) {

                    if (fstats.OutsOutOfZone > 0) {

                        alw = algorithms.Fielding.OutOfZone;
                        columns = alw.columns;
                        value = alw.value[positionIndex];
                        zone = 0;
                        for (var c = 0; c < columns.length; c++) {

                            // raw stats
                            var s = fstats[columns[c]];
                            zone += s * value[c];
                        }

                    } else {

                        alw = algorithms.Fielding.NoOutOfZone;
                        columns = alw.columns;
                        value = alw.value[positionIndex];
                        zone = 0;
                        for (var c = 0; c < columns.length; c++) {

                            // raw stats
                            var s = fstats[columns[c]];
                            zone += s * value[c];
                        }
                    }

                }
                returnStats.Zone = zone;

            }
        }
        returnStats.FZ = returnStats.FieldFI + returnStats.Zone;
        if (position == "CA") {
            returnStats.cDef = returnStats.Block + returnStats.Frame + returnStats.cERA;
        }
        return returnStats;
    },



    calculateBases: function (bDayStats, player) {

        var bases = 0;

        if (!bDayStats) {

        } else {
            // LW of baserunning.. calculated in class player in set-lineups-helper

            /*
     bases =(StolenBases2B*0.15)+(StolenBases3B*0.225)
     +(StolenBasesHP*0.3)
     +(CaughtStealing2B*-0.7)
     +(CaughtStealing3B*-1)
     +(CaughtStealingHP*-1.3)
     +(Pickoffs*-0.7)
     +(OutsonBase*-0.7)
     +(AdvancedOnFlyBall*0.15)
     +(FirstToThird1B*0.225)
     +(FirstToHome2B*0.375)
     +(SecondToHome1B*0.3)
             */
            /*
        // From Tony 3/26/18, the bases formula should be 
        for stealing 2nd, is a .3, 
        stealing 3rd is a .395, 
        stealing home is a .585, 
        
        going 1st to 3rd on a single is a .075, 
        going 1st to home on a double is .09875, 
        going 2nd to home on a single is a .19375, 
        
        
        
        caught stealing of 2nd is a -.6, 
        caught stealing of 3rd is a-.79, 
        caught stealing of home is a -1.77
        , 
        pickoff is a -.76, 
         an out on base is a -.96.
        a bases taken (on fly) is a .1225, 
        */
            // shortcut...
            var alw = algorithms.Baserunning.Bases;
            var columns = alw.columns;
            var value = alw.value;
            for (var c = 0; c < columns.length; c++) {

                // raw stats
                var s = bDayStats[columns[c]];
                if (typeof (s) == "undefined")
                    s = 0;
                bases += s * value[c];
            }

            var useMLE = false;

            if (useMLE) {
                if (bases > 0) {
                    bases = bases / 4;
                } else {
                    bases = bases * 2;
                }

                var hasBaserunningCoordinator = false;
                if (hasBaserunningCoordinator) {
                    bases = 1.15 * bases;
                }
            }

            // bDayStats.Bases = bases;
        }
        return (bases);
    },


    calculatePitchingLW: function (stats) {
        lw = 0;

        if (typeof (stats) == "undefined") {
            lw = 0;
        } else {
            /*
          "LW" : {
                "name": "linear weight",
                "value" : [0.25, -0.46, -0.8, -1.2, -1.4, -0.33, -0.33, 0.073, -0.15, 0.3, -0.395, -0.15, 0.15],
                "columns" : ["OUTS", "1B", "2B", "3B", "HR", "HBP", "BB", "K", "SB", "CS", "WP", "BLK", "PKO"],
                "notes" : "Calculated x OUTS, 1B, 2B, 3B, HR, HBP, BB, K, SB, CS, WP, BLK, PKO",
    
                "fromTory" : "Pitchers’ LW = =(OUT*0.25)+(1B*-0.46)+(2B*-0.8)+(3B*1.2)+(HR*-1.4)+(HBP*-0.33)+(BB*-0.33)+(K*0.073)+(SB*-0.15)+(CS*0.3)+(WP*-0.395)+(BLK*-0.15)+(PKO*0.15)"
    
                    */

            // shortcut...
            alw = algorithms.Pitching.LW;
            columns = alw.columns;
            value = alw.value;
            var lw = 0;
            var IP = stats.IP;
            var outs = 0;
            if (IP) {
                var intOuts = Math.floor(IP * 3);
                var outs = IP % 1 == 0 ? intOuts : IP % 1 >= 0.2 ? intOuts + 2 : intOuts + 1;
            }

            stats.OUT = outs;
            for (var c = 0; c < columns.length; c++) {

                // raw stats
                var s = stats[columns[c]];
                var cval = s * value[c];
                if (cval && !isNaN(cval))
                    lw += cval;
            }

        }

        var useMLE = false;
        if (stats.Level && stats.Level == "Minors")
            useMLE = true;

        if (useMLE) {
            if (lw > 0)
                lw = lw / 4;
            else
                lw = 2 * lw;
            var hasPitchingCoordinator = false;
            if (hasPitchingCoordinator)
                lw = lw * 1.15;
        }
        return lw;
    },

    putInDisplayStats: function (players) {
        for (var p = 0; p < players.length; p++) {
            var player = players[p];

            // do batting stats first
            var startingBattingStats = startingStatsHelper.getStartingBatterStats(player);
            var useStartingStats = true;
            if (useStartingStats) {
                if( startingBattingStats.Level == "Majors") {
                    player["firstLW"] = startingBattingStats.Batting ? startingBattingStats.Batting.LW : "-";
                    player["startingLevel"] = "ML";
                } else {
                    player["startingLevel"] = "Minors";
                    player["firstLWMinors"] = startingBattingStats.Batting ? startingBattingStats.Batting.LW : "-";
                }
                player["firstBases"] = startingBattingStats.Baserunning ? startingBattingStats.Baserunning.Bases : "-";
                player["firstFI"] = startingBattingStats.Fielding ? startingBattingStats.Fielding.FieldLW : "-";
                player["firstDef"] = this.calculateDefensiveStats(startingBattingStats.Fielding, player, player["firstFI"]);

            } else {
                // old way to do it...
                if (player.eStats && (player.FirstFullOffensiveGame || (player.PA2 && player.PA2.length > 0) || (player.PA1 && player.PA1 > 0))) {
                    var gameDay = player.FirstFullOffensiveGame ? player.FirstFullOffensiveGame : player.PA2.length > 0 ? player.PA2[0] : player.PA1[0];
                    if (gameDay) {
                        var stats = player.eStats[gameDay]

                        // ** do offensive stats first
                        var batting = stats.Batting;
                        var fielding = stats.Fielding;
                        var running = stats.Baserunning;

                        if (batting) {
                            if (batting.Level != "Minors") {
                                player["firstLW"] = this.calculateBattingLW(batting);
                            } else {
                                player["firstLWMinors"] = this.calculateBattingLW(batting);
                            }
                        }
                        if (fielding) {
                            player["firstFI"] = this.calculateFieldingLW(fielding, player);
                            player["firstDef"] = this.calculateDefensiveStats(fielding, player, player["firstFI"]);
                        }
                        if (running) {
                            player["firstBases"] = this.calculateBases(running, player);
                        }

                    }
                }
            }

            // do pitching...
            if (player.eStats && player.eStats.TotalPitching && player.eStats.TotalPitching.G > 0) {
                var gameDay = player.FirstFullPitchingGame;
                if (gameDay) {
                    stats = player.eStats[gameDay];
                    var pitching = stats.Pitching;
                    if (pitching) {
                        player["firstPitchingLW"] = this.calculatePitchingLW(pitching);
                    }
                }

                pitching = player.eStats.TotalPitching;
                player["totalPitchingLW"] = this.calculatePitchingLW(pitching);
            }
            if (player.eStats && player.eStats.TotalPitchingMinors && player.eStats.TotalPitchingMinors.G > 0) {
                var gameDay = player.highestIPMinors;
                if (gameDay) {
                    stats = player.eStats[gameDay];
                    var pitching = stats.Pitching;
                    if (pitching) {
                        player["firstPitchingMinorsLW"] = this.calculatePitchingLW(pitching);
                        if( player.Primary.indexOf("P") == -1)
                            player.Primary.push("P");
                    }
                }
                player.eStats.TotalPitchingMinors["Level"] = "Minors";
                pitching = player.eStats.TotalPitchingMinors;
                player["totalPitchingMinorsLW"] = this.calculatePitchingLW(pitching);
            }
        }
    }
}