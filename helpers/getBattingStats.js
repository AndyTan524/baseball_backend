var setLineupHelper = require('../simulator/set-lineup-helper');

module.exports = {

            // adds outs to batting stats based on AB-H
    insertOuts: function (batstats) {
                if (batstats) {
                    batstats.OUTS = batstats.AB - batstats.H;
                }
            },

    calculateBattingStats: function (nextBatter, partialStats, coordinators, statsObject) {

        // do this for all...
        var position = nextBatter.Primary[0];


        var gameDate = null;

        // we need to add up the stats from the partial games!
        var fstats = null;
        var batstats = null;
        var brstats = null;
        for (var g = 0; g < partialStats.length; g++) {

            // there are empty array elements, skip them.
            if (partialStats[g]) {
                // figure the partials!
                var ratio = 1.0;
                var roundup = true;
                if (partialStats[g].UsingPartial == "new") {
                    ratio = partialStats[g].Ratio;
                } else if (partialStats[g].UsingPartial == "partialExists") {
                    ratio = partialStats[g].Ratio;
                    roundup = false;
                }
                gameDate = partialStats[g].GameDay;
                if (nextBatter.eStats[gameDate] && nextBatter.eStats[gameDate].Fielding) {


                    // add to previous stats....
                    var ns = nextBatter.eStats[gameDate].Fielding;
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
                            fstats = { stat: value };
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
                if (nextBatter.eStats[gameDate] && nextBatter.eStats[gameDate].Batting) {

                    // add to previous stats....
                    var ns = nextBatter.eStats[gameDate].Batting;
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
                            if (!batstats) {
                                batstats = { stat: value };
                            } else {
                                if (batstats[stat] && isNaN(batstats[stat]) == false) {
                                    // add 'em
                                    batstats[stat] += value;
                                } else {
                                    batstats[stat] = value;
                                }
                            }

                        }
                    }
                    if (ratio < 1.0) {
                        // then need to clean up the stats :(
                        var padiff = (batstats.AB + batstats.BB + batstats.HBP) - batstats.PA
                        while (padiff > 0) {
                            // too many plate appearances
                            if (batstats.BB > 0) {
                                batstats.BB--;
                                padiff--;
                            }
                            if (padiff > 0 && batstats.HBP > 0) {
                                padiff--;
                                batstats.HBP--;
                            }
                        }

                        var hitdiff = (batstats["1B"] + batstats["2B"] + batstats["3B"] + batstats.HR) - batstats.H
                        while (hitdiff > 0) {
                            // too many hits
                            if (batstats["1B"] > 0) {
                                batstats["1B"]--;
                                hitdiff--;
                            }
                            if (hitdiff > 0 && batstats["2B"] > 0) {
                                batstats["2B"]--;
                                hitdiff--;
                            }
                            if (hitdiff > 0 && batstats["3B"] > 0) {
                                batstats["3B"]--;
                                hitdiff--;
                            }
                            if (hitdiff > 0 && batstats["HR"] > 0) {
                                batstats["HR"]--;
                                hitdiff--;
                            }

                        }
                    }
                    batstats.Level = nextBatter.eStats[gameDate].Batting.Level;
                }
                if (nextBatter.eStats[gameDate] && nextBatter.eStats[gameDate].Baserunning) {

                    // add to previous stats....
                    var ns = nextBatter.eStats[gameDate].Baserunning;
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
                        }
                        if (!brstats) {
                            brstats = { stat: value };
                        } else {
                            if (brstats[stat] && isNaN(brstats[stat]) == false) {
                                // add 'em
                                brstats[stat] += value;
                            } else {
                                brstats[stat] = value;
                            }
                        }
                    }
                } else {
                    if (nextBatter.eStats[gameDate]) {
                        nextBatter.eStats[gameDate].Baserunning = { Bases: 0 };
                        brstats = nextBatter.eStats[gameDate].Baserunning;
                    }
                }
            }
        }

    
        this.insertOuts(batstats);
        if( fstats ) {
            nextBatter.StartingPosition = nextBatter.Primary[0];
            setLineupHelper.calculateDefensiveStats(fstats, nextBatter, coordinators);
        }
        if( brstats )
            setLineupHelper.calculateBases(brstats, nextBatter, coordinators);

        setLineupHelper.calculateBattingLW(batstats);
        setLineupHelper.calculateFieldingLW(fstats, nextBatter, coordinators);
        
        statsObject.Batting = batstats;
        statsObject.Fielding = fstats;
        statsObject.Baserunning = brstats;
        return( statsObject );
    },

    getBestStats: function (batter, minPA, exclude, exact) {
        var useStats = null;

        var excludeList = [];
        if (exclude) {

            // note it's always 3 elements long, but can have empty elements.
            for (var e = 0; e < exclude.length; e++) {
                if (exclude[e])
                    excludeList.push(exclude[e].GameDay);
            }
        }
        if (batter.eStats) {

            var mostRecent = 0;
            for (var gameDay in batter.eStats) {
                if (gameDay.charAt(0) == "2" && (!exclude || excludeList.indexOf(gameDay) == -1)) {

                    if (!gameDay.includes("Minors")) {
                        // then it's a date, not an excluded date, and not a YTD or TOTAL object
                        var nextStats = batter.eStats[gameDay];

                        if ((!exact && nextStats.Batting && nextStats.Batting.PA >= minPA)
                            || (exact && nextStats.Batting && nextStats.Batting.PA == minPA)) {
                            // found the minimum stats on this day
                            // check the partials here.

                            var remainingPA = nextStats.Batting.PA;
                            var usingPartial = false;
                            var usedPA = 0;
                            var ratio = 1.0;
                            if (batter.PartialGames[gameDay]) {
                                usedPA = batter.PartialGames[gameDay].paUsed;
                                if (usedPA > 0) {
                                    remainingPA = remainingPA - usedPA;
                                    usingPartial = "partialExists";
                                }

                            }
                            if (remainingPA > minPA) {

                                // then recalculate remaining to try to be == minPA

                                // for full game at bat.. split in "half"
                                if (minPA >= 3) {
                                    // full games only require 5...
                                    if (remainingPA > 5) {
                                        remainingPA = Math.ceil(remainingPA / 2);       // rounds up.
                                    }
                                    if (!usingPartial) {
                                        usingPartial = "new"
                                    } else {
                                        usingPartial = "partialExists";
                                    }
                                } else {
                                    // just take the number we wanted!
                                    remainingPA = minPA;
                                    if (!usingPartial) {
                                        usingPartial = "new"
                                    } else {
                                        usingPartial = "partialExists";
                                    }
                                }
                            }

                            if ((!exact && remainingPA >= minPA)
                                || (exact && remainingPA == minPA)) {
                                {
                                    var dateOnly = gameDay.substr(0, 8);
                                    if (Number(dateOnly) > mostRecent) {
                                        // then found a more recent date (more recent == higher number)
                                        mostRecent = Number(dateOnly);
                                        var statsSet = {
                                            GameDay: gameDay,
                                            Batting: nextStats.Batting,
                                            Baserunning: nextStats.Baserunning ? nextStats.Baserunning : null,
                                            Fielding: nextStats.Fielding ? nextStats.Fielding : null,
                                            UsingPartial: usingPartial,
                                            TotalPA: nextStats.Batting.PA,  // original available
                                            PA: remainingPA,     // this is the number for this game's play
                                            UsedPA: usedPA,     // this is the number of pre-used stats...
                                            Ratio: remainingPA / nextStats.Batting.PA
                                        }

                                        useStats = statsSet;
                                    }
                                }

                            }

                        }
                    }
                }
            }
        }
        return useStats;
    },

    getBestMinorStats: function (batter, minPA, exclude, exact) {
        var useStats = null;

        var excludeList = [];
        if (exclude) {

            // note it's always 3 elements long, but can have empty elements.
            for (var e = 0; e < exclude.length; e++) {
                if (exclude[e])
                    excludeList.push(exclude[e].GameDay);
            }
        }
        if (batter.eStats) {

            var mostRecent = 0;
            for (var gameDay in batter.eStats) {
                if (gameDay.charAt(0) == "2" && (!exclude || excludeList.indexOf(gameDay) == -1)) {

                    if (gameDay.includes("Minors")) {
                        // then it's a date, not an excluded date, and not a YTD or TOTAL object
                        var nextStats = batter.eStats[gameDay];

                        if ((!exact && nextStats.Batting && nextStats.Batting.PA >= minPA)
                            || (exact && nextStats.Batting && nextStats.Batting.PA == minPA)) {
                            // found the minimum stats on this day
                            // check the partials here.

                            var remainingPA = nextStats.Batting.PA;
                            var usingPartial = false;
                            var usedPA = 0;
                            var ratio = 1.0;
                            if (batter.PartialGames[gameDay]) {
                                usedPA = batter.PartialGames[gameDay].paUsed;
                                if (usedPA > 0) {
                                    remainingPA = remainingPA - usedPA;
                                    usingPartial = "partialExists";
                                }

                            }
                            if (remainingPA > minPA) {

                                // then recalculate remaining to try to be == minPA

                                // for full game at bat.. split in "half"
                                if (minPA >= 3) {
                                    // full games only require 5...
                                    if (remainingPA > 5) {
                                        remainingPA = Math.ceil(remainingPA / 2);       // rounds up.
                                    }
                                    if (!usingPartial) {
                                        usingPartial = "new"
                                    } else {
                                        usingPartial = "partialExists";
                                    }
                                } else {
                                    // just take the number we wanted!
                                    remainingPA = minPA;
                                    if (!usingPartial) {
                                        usingPartial = "new"
                                    } else {
                                        usingPartial = "partialExists";
                                    }
                                }
                            }

                            if ((!exact && remainingPA >= minPA)
                                || (exact && remainingPA == minPA)) {
                                {
                                    var dateOnly = gameDay.substr(0, 8);
                                    if (Number(dateOnly) > mostRecent) {
                                        // then found a more recent date (more recent == higher number)
                                        mostRecent = Number(dateOnly);
                                        var statsSet = {
                                            GameDay: gameDay,
                                            Batting: nextStats.Batting,
                                            Baserunning: nextStats.Baserunning ? nextStats.Baserunning : null,
                                            Fielding: nextStats.Fielding ? nextStats.Fielding : null,
                                            UsingPartial: usingPartial,
                                            TotalPA: nextStats.Batting.PA,  // original available
                                            PA: remainingPA,     // this is the number for this game's play
                                            UsedPA: usedPA,     // this is the number of pre-used stats...
                                            Ratio: remainingPA / nextStats.Batting.PA
                                        }

                                        useStats = statsSet;
                                    }
                                }

                            }

                        }
                    }
                }
            }
        }
        return useStats;
    },

    getBestStarterStats: function (batter, minPA, exclude, exact) {
        var useStats = null;

        var excludeList = [];
        if (exclude) {

            // note it's always 3 elements long, but can have empty elements.
            for (var e = 0; e < exclude.length; e++) {
                if (exclude[e])
                    excludeList.push(exclude[e].GameDay);
            }
        }
        if (batter.eStats) {

            var PAAvailable = 0;
            if (batter.PA2 && batter.PA2.length > 0)
                PAAvailable = batter.PA2.length * 2;
            if (batter.PA1 && batter.PA1.length > 0)
                PAAvailable += batter.PA1.length;

            var mostRecent = 0;
            for (var gameDay in batter.eStats) {
                if (gameDay.charAt(0) == "2" && (!exclude || excludeList.indexOf(gameDay) == -1)) {

                    // then it's a date, not an excluded date, and not a YTD or TOTAL object
                    var nextStats = batter.eStats[gameDay];
                    if ((!exact && nextStats.Batting && nextStats.Batting.PA >= minPA)
                        || (exact && nextStats.Batting && nextStats.Batting.PA == minPA)) {
                        // found the minimum stats on this day
                        // check the partials here.

                        if (gameDay.includes("Minors") && PAAvailable >= minPA) {
                            // try to skip using minors
                        }

                        var remainingPA = nextStats.Batting.PA;
                        var usingPartial = false;
                        var usedPA = 0;
                        var ratio = 1.0;
                        if (batter.PartialGames[gameDay]) {
                            usedPA = batter.PartialGames[gameDay].paUsed;
                            if (usedPA > 0) {
                                remainingPA = remainingPA - usedPA;
                                usingPartial = "partialExists";
                            }

                        }
                        if (remainingPA > minPA) {

                            // then recalculate remaining to try to be == minPA

                            // for full game at bat.. split in "half"
                            if (minPA >= 3) {
                                // full games only require 5...
                                if (remainingPA > 5) {
                                    remainingPA = Math.ceil(remainingPA / 2);       // rounds up.
                                }
                                if (!usingPartial) {
                                    usingPartial = "new"
                                } else {
                                    usingPartial = "partialExists";
                                }
                            } else {
                                // just take the number we wanted!
                                remainingPA = minPA;
                                if (!usingPartial) {
                                    usingPartial = "new"
                                } else {
                                    usingPartial = "partialExists";
                                }
                            }
                        }

                        if ((!exact && remainingPA >= minPA)
                            || (exact && remainingPA == minPA)) {
                            {
                                var dateOnly = gameDay.substr(0, 8);
                                if (Number(dateOnly) > mostRecent) {
                                    // then found a more recent date (more recent == higher number)
                                    mostRecent = Number(dateOnly);
                                    var statsSet = {
                                        GameDay: gameDay,
                                        Batting: nextStats.Batting,
                                        Baserunning: nextStats.Baserunning ? nextStats.Baserunning : null,
                                        Fielding: nextStats.Fielding ? nextStats.Fielding : null,
                                        UsingPartial: usingPartial,
                                        TotalPA: nextStats.Batting.PA,  // original available
                                        PA: remainingPA,     // this is the number for this game's play
                                        UsedPA: usedPA,     // this is the number of pre-used stats...
                                        Ratio: remainingPA / nextStats.Batting.PA
                                    }

                                    useStats = statsSet;
                                }
                            }

                        }

                    }
                }
            }
        }
        return useStats;
    },

    // ***************************************************
    //
    // getStartingBatterStats
    //   ==> returns object with the actual stats for this player's first available game where he can start
    //       
    // ***********************************************************************************************************
    getStartingBatterStats: function (batter, coordinators = null) {
        var startingStats = { PA: 0, PAMinors: 0, Level: "Majors" };

        if (batter.eStats) {
            var availablePA = batter.eStats.TotalBatting ? batter.eStats.TotalBatting.PAAvailable : 0;
            var availablePAMinors = batter.eStats.TotalBattingMinors ? batter.eStats.TotalBattingMinors.PAAvailable : 0;
            var partialStats = new Array(3);

            if (availablePA >= 2) {
                // *****************************
                // then this player can start with at least 2 Major league PA
                // ***************************************************************************

                partialStats[0] = this.getBestStats(batter, 3, null, false); // get a minimum of 3 PAs 
                if (!partialStats[0]) {

                    // didn't find 3 in one game...
                    partialStats[0] = this.getBestStats(batter, 2, null, true); // get exactly 2 PAs from 1 game

                    if (partialStats[0]) {
                        // then has a game with exactly 2. try for one more! try for 1 from a game with 1 exact PA (last param=true)
                        partialStats[1] = this.getBestStats(batter, 1, partialStats, true); // get an exact count of 1 PAs
                        if (partialStats[1]) {
                            // yes, exactly 1 PA!
                            // we're done
                        } else {
                            // try one more time to get at least 1
                            partialStats[1] = this.getBestStats(batter, 1, partialStats, false); // get a minimum of 1 PAs
                            // if it fails, we still have 2 PA for this batter, so we're ok for now.

                        }
                    } else {

                        // looking for at least 3! and we know there aren't any 3+ or 2 PA games, get all 1s
                        partialStats[0] = this.getBestStats(batter, 1, null, false); // get a minimum of 1 PAs
                        if (partialStats[0]) {
                            // then still need 2 more
                            partialStats[1] = this.getBestStats(batter, 1, partialStats, false); // get a minimum of 1 PAs
                            if (partialStats[1]) {
                                partialStats[2] = this.getBestStats(batter, 1, partialStats, false); // get a minimum of 1 PAs
                                // if it fails, we still have 2 PA for this batter, so we're ok for now.
                            }
                        }
                    }
                }

            } else if (availablePAMinors >= 2) {

                // ********************************   
                // then this player can start with at least 2 Minor league PA
                // ******************************************************************
                startingStats.Level = "Minors";
                partialStats[0] = this.getBestMinorStats(batter, 3, null, false); // get a minimum of 3 PAs 
                if (!partialStats[0]) {

                    // didn't find 3 in one game...
                    partialStats[0] = this.getBestMinorStats(batter, 2, null, true); // get exactly 2 PAs from 1 game

                    if (partialStats[0]) {
                        // then has a game with exactly 2. try for one more! try for 1 from a game with 1 exact PA (last param=true)
                        partialStats[1] = this.getBestMinorStats(batter, 1, partialStats, true); // get an exact count of 1 PAs
                        if (partialStats[1]) {
                            // yes, exactly 1 PA!
                            // we're done
                        } else {
                            // try one more time to get at least 1
                            partialStats[1] = this.getBestMinorStats(batter, 1, partialStats, false); // get a minimum of 1 PAs
                            // if it fails, we still have 2 PA for this batter, so we're ok for now.

                        }
                    } else {

                        // looking for at least 3! and we know there aren't any 3+ or 2 PA games, get all 1s
                        partialStats[0] = this.getBestMinorStats(batter, 1, null, false); // get a minimum of 1 PAs
                        if (partialStats[0]) {
                            // then still need 2 more
                            partialStats[1] = this.getBestMinorStats(batter, 1, partialStats, false); // get a minimum of 1 PAs
                            if (partialStats[1]) {
                                partialStats[2] = this.getBestMinorStats(batter, 1, partialStats, false); // get a minimum of 1 PAs
                                // if it fails, we still have 2 PA for this batter, so we're ok for now.
                            }
                        }
                    }
                }

            } else if (availablePA == 1) {
                // then this player cannot start, but can pinch hit with 1 Major league PA
                partialStats[0] = this.getBestStats(batter, 1, null, false); // get a minimum of 1 PAs

            } else if (availablePAMinors == 1) {
                // then this player cannot start, but can pinch hit with 1 Minor league PA
                startingStats.Level = "Minors";
                partialStats[0] = this.getBestMinorStats(batter, 1, null, false); // get a minimum of 1 PAs
            } else {
                // nothing to do, this player has no PA's available in either Majors or Minors;
            }


            // add up the starting stats if necessary....
            if (availablePA || availablePAMinors) {
                this.calculateBattingStats(batter, partialStats, coordinators, startingStats);
                if (startingStats.Level == "Majors") {
                    startingStats.PA = startingStats.Batting.PA;
                } else {
                    startingStats.PAMinors = startingStats.Batting.PA;
                }
            }
        }
        return startingStats;
    }
};