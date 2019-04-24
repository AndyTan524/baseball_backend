"use strict";
// **********************************************************
//
// File: set-pitchers.js 
// RFB Simulator: functions for setting the starting pitcher, relievers, closer, and bullpen
// By: Eddie Dombrower and Adam Feldman
//
// **********************************************************

function calculateOuts(stats) {
    var ip = stats.IP;
    var innings = Math.floor(ip);
    var outs = Math.floor((ip - innings) * 10) + (innings * 3);
    stats.OUT = outs;

}

function calculatePitchingLW(stats) {
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
        lw = 0;
        for (var c = 0; c < columns.length; c++) {

            // raw stats
            var s = stats[columns[c]];
            lw += s * value[c];
        }

    }
    stats.LW = lw;
}

class pitcher {
    constructor(stats, rosterIndex, availableSubIndex, type) {
        this.FullName = stats.PlayerName;
        this.MLBId = stats.MLBId;
        this.RosterIndex =  [rosterIndex, availableSubIndex];
        this.Pos = type;
        this.OUT = stats.OUT;


        // ((((((((((((((((((((((( TODO )))))))))))))))))))))))
        // - get most recent unused game(s)
        // - if not there, calculate # of outs per pitcher
        // - look to see if starter has CG.. if so, no relievers needed! (add to pitchingStaff object)
        // - if starter goes 8 inninges, setupPitcher = false (see below)
        // - if starter has less than 3 days rest, reduce to 75% of lw. ie. LW = .75 * LW if LW >0, 
        //                                          else LW = 1.25*LW if LW < 0
        // - research "Conditioning" for relieve pitchers
        // - tag players when they have played already
    }
}

function setPitchers(game, team, roster) {
    var gr = getGameRequest();
    var minstatvalue = algorithms.Pitching.MinStarterStat.value;
    var minstattype  = algorithms.Pitching.MinStarterStat.stat;


    var pitchingStaff = {
        pitchers: [],
        pitchersUsed: [],
        starter: null,
        closer: null,
        relievers: [],
        completeGame: false,
        setupPitcher: true
    }

    var players = roster;

    if (gr.useFakeData) {

        // **** data for sorting the best starter:
        var spIndex = null;
        var spSubIndex = null;
        var spOuts = 0;
        var spStats = null;

        for (var i = 0; i < players.length; i++) {

            var availabledays = players[i].Pitching ? players[i].Pitching.daily.length : 0;
            // ***** for each player who's pitched, check for their best day
            for (var av = 0; av < availabledays; av++) {
                
                var stats = players[i].Pitching && players[i].Pitching.daily.length > 0 ? players[i].Pitching.daily[av] : false;

 
                if (stats) {

                    calculateOuts(stats);
      //              calculatePitchingLW(stats);
                    if (stats.GS == "1") {
                        // ((((((((((((((((((((((((  TODO ))))))))))))))))))))))))
                        // 1) pick the USER'S highest pitcher order (i.e. in order from 1 to 6)
                        // 2) insure that pitcher has enough rSports rest days (4 days from last rSports appearance)
                        // 3) for now, starters only need GS == "1" to qualify

                        // ****** temporary ((((((((( todo: remove )))))))))
                        // pick starter with highest number of outs
                        // don't bother to break ties
                        if (spIndex == null) {
                            // first option available.. grab him
                            spIndex = i;
                            spSubIndex = av;
                            spStats = stats;
                            spOuts = stats["OUT"];
                        } else {
                            // have another potential starter.. see who's better
                            // unless we already have someone going at least 5 innings.
                            // for now, who's lasted LONGER
                            if ( (spOuts < (5*3)) && stats.OUT > spOuts) {
                                // *** this one has pitched longer, use him
                                spIndex = i;
                                spSubIndex = av;
                                spStats = stats;
                                spOuts = stats.OUT;
                            }
                        }

                    } else {
                        // not a starter.. throw him in the pen
                        // look for potential closers
                        if (pitchingStaff.closer == null && stats.SVO == "1") {
                            pitchingStaff.closer = new Array( [i, av] );
                            pitchingStaff.pitchers.push(new pitcher(stats, i, av, "CL"));
                        } else {
                            // just in the bullpen
                            pitchingStaff.pitchers.push(new pitcher(stats, i, av, "BP"));
                        }

                    }
                }
            }
        }

        // **** assume we have a starter.. put him in the lineup
        if (spIndex != null) {
            calculatePitchingLW(spStats);
            pitchingStaff.pitchers.push(new pitcher(spStats, spIndex, spSubIndex, "SP"));
            pitchingStaff.pitchersUsed.push(new pitcher(spStats, spIndex, spSubIndex, "SP"));
            pitchingStaff.starter = [spIndex, spSubIndex];
            pitchingStaff.starterStats = spStats;
        }

        // (((((((((((((((((((((((((( TODO REMOVE UNNEEDED PITCHERS FROM THE LIST))))))))))))))))))))))))))

    } else {

        // **************************************
        // 
        // using real data
        //
        // **************************************

        // **** data for sorting the best starter:
        var spIndex = null;
        var spSubIndex = null;
        var spOuts = 0;
        var spStats = null;

        for (var i = 0; i < players.length; i++) {

            var p = players[i];
            if (p.Primary && (p.Primary.length > 0) && (p.Primary[0] == "P")) {

                // have a pitcher... is he set by the owner?
                if (p.LineupDH.Position == "SP") {

                } else {
                    // not set by owner, is he a starter
                    if (p.CanStart && p.FirstFullPitchingGame != "") {
                        // found a starter...
                        stats = p.eStats.Pitching[p.FirstFullPitchingGame];
                        if (stats) {

                            calculateOuts(stats);
                            // ((((((((((((((((((((((((  TODO ))))))))))))))))))))))))
                            // 1) pick the USER'S highest pitcher order (i.e. in order from 1 to 6)
                            // 2) insure that pitcher has enough rSports rest days (4 days from last rSports appearance)
                            // 3) for now, starters only need GS == "1" to qualify

                            // ****** temporary ((((((((( todo: remove )))))))))
                            // pick starter with highest number of outs
                            // don't bother to break ties
                            if (spIndex == null) {
                                // first option available.. grab him
                                spIndex = i;
                                spSubIndex = av;
                                spStats = stats;
                                spOuts = stats["OUT"];
                            } else {
                                // have another potential starter.. see who's better
                                // unless we already have someone going at least 5 innings.
                                // for now, who's lasted LONGER
                                if ((spOuts < (5 * 3)) && stats.OUT > spOuts) {
                                    // *** this one has pitched longer, use him
                                    spIndex = i;
                                    spSubIndex = av;
                                    spStats = stats;
                                    spOuts = stats.OUT;
                                }
                            }
                        }
                    } else {
                        // not a starter.. throw him in the pen
                        // look for potential closers

                        // see if he has any games....
                        if (p.CanRelieve && p.gamesList && p.gamesList.length > 0) {
                            var gameday = p.gamesList[0];
                            stats = p.eStats.Pitching[gameday];
                            if (pitchingStaff.closer == null && stats.SVO == "1") {
                                pitchingStaff.closer = new Array([i, av]);
                                pitchingStaff.pitchers.push(new pitcher(stats, i, av, "CL"));
                            } else {
                                // just in the bullpen
                                pitchingStaff.pitchers.push(new pitcher(stats, i, av, "BP"));
                            }
                        }
                    }
                }
            }

            // **** assume we have a starter.. put him in the lineup
            if (spIndex != null) {
                calculatePitchingLW(spStats);
                pitchingStaff.pitchers.push(new pitcher(spStats, spIndex, spSubIndex, "SP"));
                pitchingStaff.pitchersUsed.push(new pitcher(spStats, spIndex, spSubIndex, "SP"));
                pitchingStaff.starter = [spIndex, spSubIndex];
                pitchingStaff.starterStats = spStats;
            }
        }
        return( pitchingStaff );
    }

    if (team)
        team.pitchingStaff = pitchingStaff;

}
