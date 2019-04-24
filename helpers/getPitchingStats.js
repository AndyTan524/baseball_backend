
module.exports = {

    calculateOuts: function (stats) {
        var ip = stats.IP;
        var innings = Math.floor(ip);
        var outs = Math.floor(((ip - innings) + 0.0001) * 10) + (innings * 3);
        stats["OUT"] = outs;

    },

    getOutsFromIP: function (ip) {
        var ip = Number(ip).toFixed(1);
        var innings = ip.charAt(0);
        var fraction = ip.charAt(2);
        var outs = innings * 3;
        if (fraction == "2") {
            outs += 2;
        } else if (fraction == "1") {
            outs += 1;
        }
        return (outs);
    },

    getIPFromOuts: function (outs) {
        var fractOuts = outs / 3;
        var whole = Math.floor(fractOuts);
        var ip = whole + (fractOuts % 1 == 0 ? 0 : (fractOuts % 1 > 0.5 ? 0.2 : 0.1));
        return (ip);
    },

    calculatePitchingLW: function (stats, isPitchingOn3DaysRest, coaches) {
        lw = 0;

        if (typeof (stats) == "undefined") {
            lw = 0;
        } else {
            var hasPitchingCoordinator = false;
            if( coaches && coaches.Pitching && coaches.Pitching==true) {
                hasPitchingCoordinator = true;
            }
            
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
            for (var c = 0; c < columns.length; c++) {

                // raw stats
                var s = stats[columns[c]];
                var cval = s * value[c];
                lw += cval;
            }

        }
        if( isPitchingOn3DaysRest && isPitchingOn3DaysRest == true ) {
            var penalty = Math.abs(lw) * 0.25;
            lw = lw - penalty;
        }

        var useMLE = false;
        if( stats.Level && stats.Level == "Minors")
            useMLE = true;

        if( useMLE) {
            if( lw > 0 )
                lw = lw/4;
            else
                lw = 2* lw;

            if( hasPitchingCoordinator )
                lw = lw * 1.15;
        }
        stats["LW"] = lw;
    },


    getBestPitchingStatsForReliever: function (nextPitcher, targetOuts, coaches) {
        var gameDates = [];

        if (targetOuts == 0)
            targetOuts = 1;

        // first look for a day with the closest to that many outs without going over..

        var foundOuts = 0;

        // ********((((((((((( TODO INCLUDE USED GAMES/PARITAL GAMES)))))))))))

        // grab array of games that have enough outs are available
        var totalOuts = nextPitcher.eStats.TotalPitching ? nextPitcher.eStats.TotalPitching.OutsAvailable : 0;
        var totalOutsMinors = nextPitcher.eStats.TotalPitchingMinors ? nextPitcher.eStats.TotalPitchingMinors.OutsAvailable : 0;

        var outsArray = [];
        var isMajors = true;
        if (totalOuts >= targetOuts) {
            outsArray = nextPitcher.outsArray;
        } else if (totalOutsMinors >= targetOuts) {
            outsArray = nextPitcher.outsArrayMinor;
            isMajors = false;
        }


        // look for a game or games that add up to, or over the target.
        // assumes this was tested before the function was called
        var foundOuts = 0;
        if ( outsArray[targetOuts] && outsArray[targetOuts].length > 0) {
            gameDates[0] = outsArray[targetOuts][0];
            foundOuts = targetOuts;
            // found the exact number of outs.
            // put in the first gameData... and done.
        } else {
            // need to build a game that has that many outs...from more than one game

            // first look to add up outs from lesser outs than the target outs....
            for (var fo = targetOuts - 1; fo > 0; fo--) {
                if (outsArray[fo] && outsArray[fo].length > 0) {
                    for (go = 0; go < outsArray[fo].length; go++) {
                        foundOuts += fo;
                        gameDates.push(outsArray[fo][go]);
                        if (foundOuts >= targetOuts)
                            break;
                    }
                    if (foundOuts >= targetOuts)
                        break;
                }
            }

            // if didn't find enough outs, grab some more...
            // start at target outs and work upwards... should create too many outs..
            if (foundOuts < targetOuts) {
                for (var fo = targetOuts; fo < 28; fo++) {
                    if (outsArray[fo] && outsArray[fo].length > 0) {
                        foundOuts += fo;
                        // because fo > targetouts, we know we have more than enough outs...
                        // so, this will be a partial!
                        gameDates.push(outsArray[fo][0]);
                        break;
                    }
                }
            }
        }

        // default, assume we found the exact outs in one game
        var eStats = nextPitcher.eStats[gameDates[0]];
        var stats = eStats.Pitching;
        stats.Level = "ML";
        if ( !isMajors)
            stats.Level = "Minors";

        var fstats = {};
        if (eStats && eStats.Fielding) {
            fstats = eStats.Fielding;
        }

        if (foundOuts > targetOuts || gameDates.length > 1) {

            // **********************************
            // 
            // did not find exact outs
            // add up outs from multiple and/or partial games
            //
            // *******************************************************************
            eStats = { Pitching: null, Fielding: null };
            stats = null;
            fstats = null;

            var multiPitchGame = [];
            var outsUsed = 0;
            for (var g = 0; g < gameDates.length; g++) {

                var availOuts = this.getOutsFromIP(nextPitcher.eStats[gameDates[g]].Pitching.IP);

                // assume more outs than needed...
                var remainingTargetOuts = targetOuts - outsUsed;
                var ratio = remainingTargetOuts / availOuts;

                var outsToUse = availOuts;
                if ( ratio >= 1) {
                    // found less outs than needed
                    ratio = 1.0;    //use  all available outs

                } else {
                    // only use those that we need
                    outsToUse = remainingTargetOuts;
                }

                outsUsed += outsToUse;
                multiPitchGame.push({
                    GameDate: gameDates[g],
                    OutsAvailable: availOuts,
                    OutsUsed: outsToUse,
                    Ratio: ratio
                });

                var roundup = true;
                /*
                            if (nextBatter.useStats[g].UsingPartial == "new") {
                                ratio = nextBatter.useStats[g].Ratio;
                            } else if (nextBatter.useStats[g].UsingPartial == "partialExists") {
                                ratio = nextBatter.useStats[g].Ratio;
                                roundup = false;
                            }
                */
                gameDate = gameDates[g];

                // add and prorate as needed the fielding stats...
                if (nextPitcher.eStats[gameDate] && nextPitcher.eStats[gameDate].Fielding) {
                    // add to previous stats....
                    var ns = nextPitcher.eStats[gameDate].Fielding;
                    for (var stat in ns) {
                        var value = Number(ns[stat]);
                        if (isNaN(value)) {
                            //   value = 0;

                        } else {
                            // do the partials!
                            if (ratio < 1.0) {
                                if (roundup) {
                                    // it's new, just do the ratio
                                    value = Math.ceil(value * ratio);
                                } else {
                                    // need to round down
                                    value = Math.floor(value * ratio);
                                }
                            }
                        }
                        if (!fstats) {
                            fstats = {};
                            fstats[stat] = value;
                        } else {
                            if (fstats[stat] && isNaN(fstats[stat]) == false) {
                                // add 'em
                                fstats[stat] += value;
                            } else {
                                fstats[stat] = value;
                            }
                        }
                    }
                }
                eStats.Fielding = fstats;

                // add and if needed prorate the pitching stats...
                if (nextPitcher.eStats[gameDate] && nextPitcher.eStats[gameDate].Pitching) {

                    // add to previous stats....
                    var ns = nextPitcher.eStats[gameDate].Pitching;
                    for (var stat in ns) {
                        var value = Number(ns[stat]);
                        if (isNaN(value)) {
                            // value = 0;
                        } else {
                            // do the partials!
                            if (ratio < 1.0) {
                                if (roundup) {
                                    // it's new, just do the ratio
                                    value = Math.ceil(value * ratio);
                                } else {
                                    // need to round down
                                    value = Math.floor(value * ratio);
                                }
                            }
                            if (!stats) {
                                stats = {};
                                stats[stat] = value;
                            } else {
                                if (stats[stat] && isNaN(stats[stat]) == false) {
                                    // add 'em
                                    stats[stat] += value;
                                } else {
                                    stats[stat] = value;
                                }
                            }

                        }
                    }

                }

                stats.multiPitchGame = multiPitchGame;
                // and last, but not least, need to stuff in the actual IP....
                stats.IP = this.getIPFromOuts(targetOuts);

            }
            eStats.Pitching = stats;
        }

        // make the pitcher here...
        if (stats) {
            this.calculateOuts(stats);
            this.calculatePitchingLW(stats, null, coaches);   // just a best guess until game time
        }

        nextPitcher.fStats = eStats.Fielding;
        nextPitcher.pStats.Pitching = stats;
        nextPitcher.pStats.Fielding = eStats.Fielding;
    }
};