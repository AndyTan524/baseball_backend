var express = require('express');
var router = express.Router();
var jwt = require('jsonwebtoken');
var accessHelper = require('../helpers/accessHelper');
var utils = require('../helpers/utils');

var mongoose = require('mongoose');
var League = mongoose.model('League');


router.post('/api/owners/create', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {

            req.body.CreatedUTC = new Date().toISOString();

            var qry = {
                $and:
                    [
                        {
                            _id: req.body._id
                        },
                        {
                            Teams: { $elemMatch: { _id: req.body.Teams[0]._id } }
                        }
                    ]
            };


            req.body.Teams[0].Owners.CreatedUTC = new Date().toUTCString();

            var update = {
                $addToSet: {
                    "Teams.$.Owners": req.body.Teams[0].Owners
                }
            };

            League.update(qry, update, function (err, league) {
                if (err) {
                    res.status(500).json(err);
                }
                else if (league) {
                    res.status(200).json({ status: 200, msg: "Owner added" });

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

router.post('/api/owners/setUserRole', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            League.findOne({ _id: req.body._id }, function (err, league) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    var userRoles = require('../helpers/userRoles').UserRoles;

                    // find the team for this owner
                    var teams = league.Teams;
                    var teamIndex = 0;
                    for (t = 0; t < teams.length; t++) {
                        if (teams[t]._id == req.body.tid) {
                            teamIndex = t;
                            break;
                        }
                    }

                    // see if the owner is already part of this team
                    var ownerIndex = -1;
                    for (i = 0; i < teams[teamIndex].Owners.length; i++) {
                        if (teams[teamIndex].Owners[i].UserId == req.body.owner.UserId) {
                            // then update this owner
                            ownerIndex = i;
                            teams[teamIndex].Owners[i].Role = req.body.owner.Role;
                            teams[teamIndex].Owners[i].OwnershipPercentage = req.body.owner.OwnershipPercentage;
                            break;
                        }
                    }

                    // if owner not in yet, put them in now
                    if (ownerIndex == -1) {
                        req.body.owner.CreatedUTC = new Date().toISOString();
                        teams[teamIndex].Owners.push(
                            req.body.owner
                        );
                    }

                    // put the league back.
                    // put the league back.
                    league.markModified("Teams");
                    league.save(function (err, response) {
                        if (err) {
                            res.status(500).json({ status: 500, msg: "Could not update user (league)" });

                        } else {


                            // now update the user.. if anything but fan, make the user a shareholder/owner
                            var rolename = userRoles[req.body.owner.Role].role;
                            if (user.Roles.indexOf("Owner") > -1) {
                                // user is an owner... 
                                if (req.body.roleName == rolename) {
                                    user.Roles.splice(user.Roles.indexOf("Owner"), 1);
                                }
                            } else {
                                if (rolename == "Fan") {
                                    if (user.Roles.indexOf("Fan") == -1) {
                                        // add fan to the roles list
                                        user.Roles.push("Fan");
                                    }
                                } else {
                                    // add owner to the roles list
                                    user.Roles.push("Owner");
                                }
                            }

                            // now put the default team into the user
                            if (!user.Settings)
                                user._doc['Settings'] = {};
                            user.Settings["DefaultLeague"] = league.Name;
                            user.Settings["DefaultLeagueId"] = league._id;
                            user.Settings["DefaultTeam"] = {
                                FullName: teams[teamIndex].Name,
                                Abbreviation: teams[teamIndex].abbreviation,
                                Role: rolename,
                                RoleId: req.body.owner.Role
                            };
                            // save user
                            user.markModified("Roles");
                            user.markModified("Settings");
                            user.save(function (err, response) {
                                if (err) {
                                    res.status(500).json({ status: 500, msg: "Could not update user" });
                                } else {
                                    res.status(200).json({ status: 200, msg: "", league: league, team: league.Teams[teamIndex] });
                                }
                            });
                        }
                    });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});

router.post('/api/owners/delete', function (req, res) {
    utils.validate(req, function (isValid, user) {
        if (isValid) {
            League.findOne({ _id: req.body._id }, function (err, league) {
                if (err) {
                    res.status(500).json(err);
                }
                else {
                    var userRoles = require('../helpers/userRoles').UserRoles;

                    // find the team for this owner
                    var teams = league.Teams;
                    var teamIndex = 0;
                    for (t = 0; t < teams.length; t++) {
                        if (teams[t]._id == req.body.tid) {
                            teamIndex = t;
                            break;
                        }
                    }

                    // assume the owner is already part of this team
                    var ownerIndex = -1;
                    for (i = 0; i < teams[teamIndex].Owners.length; i++) {
                        if (teams[teamIndex].Owners[i].UserId == req.body.owner.UserId) {
                            // then found this owner
                            ownerIndex = i;
                            league.Teams[teamIndex].Owners.splice(i, 1);
                            break;
                        }
                    }
                    // put the league back.
                    league.markModified("Teams");
                    league.save(function (err, response) {
                        if (err) {
                            res.status(500).json({ status: 500, msg: "Could not update user (league)" });

                        } else {


                            // now update the user.. if anything but fan, remove the "owner".

                            if (user.Roles.indexOf("Owner") > -1) {
                                // user was an owner... 
                                user.Roles.splice(user.Roles.indexOf("Owner"), 1);
                            }

                            // now remove the default team from the user
                            if (!user.Settings)
                                user._doc['Settings'] = {};
                            user.Settings["DefaultLeague"] = null;
                            user.Settings["DefaultLeagueId"] = null;
                            user.Settings["DefaultTeam"] = {};

                            // save user
                            user.markModified("Roles");
                            user.markModified("Settings");
                            user.save(function (err, response) {
                                if (err) {
                                    res.status(500).json({ status: 500, msg: "Could not update user" });
                                } else {
                                    res.status(200).json({ status: 200, msg: "", league: league, team: league.Teams[teamIndex] });
                                }
                            });
                        }
                    });
                }
            });
        }
        else {
            res.status(401).json({ status: 401, msg: "unauthorized" });
        }
    });
});
module.exports = router;