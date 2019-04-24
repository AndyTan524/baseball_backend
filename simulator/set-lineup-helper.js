
// **********************************************************
//
// File: set-lineup-helper.js 
// RFB Simulator: functions for setting the batting order, fielding positions, and pinch hitters
// By: Eddie Dombrower and Adam Feldman
//
// **********************************************************

module.exports = {

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
            for (var c = 0; c < columns.length; c++) {

                // raw stats
                var s = stats[columns[c]];
                lw += s * value[c];
            }


            // **************************** MLE CALCULATION ******************************************** //
            var useMLE = false;
            if (stats.Level && stats.Level == "Minors")
                useMLE = true;

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
            stats.LW = lw;
        }

    },

    calculateFieldingLW: function (stats, defensivePlayer, coaches) {
        var position = defensivePlayer.StartingPosition;
        if (position == "DH" || position == "PH" || position == "PR")
            return;


        lw = 0;

        if (typeof (stats) == "undefined" || !stats) {
            lw = 0;
        } else {
            var hasInfieldCoordinator = false;
            if( coaches && coaches.Infield && coaches.Infield==true) {
                hasInfieldCoordinator = true;
            }

            var hasOutfieldCoordinator = false;
            if( coaches && coaches.Outfield && coaches.Outfield==true) {
                hasOutfieldCoordinator = true;
            }


            var positionIndex = algorithms.Positions[position];
            var mlbPos = stats.Pos - 1;     // when done, both positionIndex and mlbPos are: 0=P, 1=CA, etc.
            if (position == "P") {
                mlbPos = 0;
            }
            if (mlbPos && mlbPos >= 0 && mlbPos < 9)
                positionIndex = mlbPos;

            // shortcut...
            var useMLE = false;
            if (stats.Level && stats.Level == "Minors")
                useMLE = true;

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


                if (positionIndex < 6 && hasInfieldCoordinator) {
                    lw = lw * 1.15;
                }

                if (positionIndex > 5 && hasOutfieldCoordinator) {
                    lw = lw * 1.15;
                }

            }
            stats.FieldLW = lw;
        }
    },


    calculateDefensiveStats: function (fstats, defensivePlayer, coaches) {
        var position = defensivePlayer.StartingPosition;
        if (position == "PH" || position == "PR" || position == "DH") {
            return;
        }

        var positionIndex = algorithms.Positions[position];

        var alw = algorithms.Fielding;
        if (fstats) {

            var useMLE = false;
            if (fstats.Level && fstats.Level == "Minors")
                useMLE = true;

            // catchers are treated differently.
            if ((position && position == "CA") || (!position && fstats.Pos == 2)) {

                var hasCatchingCoordinator = false;
                if( coaches && coaches.Catching && coaches.Catching==true) {
                    hasCatchingCoordinator = true;
                }              

                if (!useMLE) {
                    var innings = fstats.INN;
                    var inningFraction = innings % 1 == 0.2 ? 0.666 : innings % 1 == 0.1 ? 0.333 : 0;
                    innings = Math.floor(innings) + inningFraction;

                    if (innings > 0) {
                        if (defensivePlayer.eStats && defensivePlayer.eStats.YTDTotalFielding) {
                            var cBlockingRuns = defensivePlayer.eStats.YTDTotalFielding.cBlockingRuns ? defensivePlayer.eStats.YTDTotalFielding.cBlockingRuns : 0;
                            var cFramingRuns = defensivePlayer.eStats.YTDTotalFielding.cFramingRuns ? defensivePlayer.eStats.YTDTotalFielding.cFramingRuns : 0;
                        } else {
                            var cBlockingRuns = 0;
                            var cFramingRuns = 0;

                        }
                        var daysOfSeasonFactor = alw.avgCatcherInnings.value;
                        /* / this was the original requirement:
                        fstats.Block = cBlockingRuns / ((daysOfSeason * 9) / innings);
                        fstats.Frame = cFramingRuns / ((daysOfSeason * 9) / innings);
                        */
                        // this is the updated requirement:
                        fstats.Block = cBlockingRuns * (innings / daysOfSeasonFactor);
                        fstats.Frame = cFramingRuns * (innings / daysOfSeasonFactor);

                    } else {
                        fstats.Block = 0;
                        fstats.Frame = 0;

                    }
                    // cERA
                    // from Tory   CERA =((leaguePitchingAverage-cERA)*(10))*(INN/typicalInningsPerCatcherPerYear)
                    var avgERA = algorithms.Pitching.ERA.value;
                    var cera = (avgERA - fstats.Cera) * 10;
                    var typicalInnings = algorithms.Fielding.avgCatcherInnings.value;

                    fstats.cERA = cera * (innings / typicalInnings);


                    // catchers don't get the zone value
                } else {
                    // using the MLE
                    fstats.Block = -0.25;
                    fstats.Frame = -0.25;
                    fstats.cERA = -0.25;


                    if (hasCatchingCoordinator) {
                        fstats.Block = 0;
                        fstats.Frame = 0;
                        fstats.cERA = 0;
                    }
                }
                fstats.Zone = 0;

            } else {
                // all other fielders

                // fielders don't get these stats
                fstats.Block = "";
                fstats.Frame = "";
                fstats.cERA = "";

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
                fstats.Zone = zone;

            }
        }
    },



    calculateBases: function (bDayStats, player, coaches) {

        var bases = 0;

        if (!bDayStats) {

        } else {
            var hasBaserunningCoordinator = false;
            if( coaches && coaches.Baserunning && coaches.Baserunning==true) {
                hasBaserunningCoordinator = true;
            }   
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
            if (bDayStats.Level && bDayStats.Level == "Minors")
                useMLE = true;

            if (useMLE) {
                if (bases > 0) {
                    bases = bases / 4;
                } else {
                    bases = bases * 2;
                }


                if (hasBaserunningCoordinator) {
                    bases = 1.15 * bases;
                }
            }

            bDayStats.Bases = bases;
        }
        return (bases);
    },



    setLineup: function (game, team, roster, realdata, bench) {
        // *****************************************************
        // includes fake batting orders for testing
        // *****************************************************


        // **********************************************************************************
        //
        // SETLINEUP() HELPER FUNCTIONS
        //
        // **********************************************************************************


        // adds outs to batting stats based on AB-H
        function insertOuts(batstats) {
            if (batstats) {
                batstats.OUTS = batstats.AB - batstats.H;
            }
        }

        function findSub(forPositionIndex, forPosition, bench) {
            // look through bench for best possible player

            // assume first player is best player
            var bestPlayer = 0;
            var bestPA = bench[0].PA;
            var bestPos = bench[0].Pos;

            // get the array of positions
            var canPlay = algorithms.Fielding.AdjustPosition.value;

            for (var bi = 1; bi < bench.length; bi++) {
                var benchplayer = bench[bi];
                if (benchplayer.Pos == forPosition) {
                    // then this bench player plays this position

                    if (bestPos == forPosition) {
                        // and so does the best player!  who has more at bats?
                        if (benchplayer.PA > bestPA) {
                            bestPlayer = bi;
                            bestPA = benchplayer.PA;
                            // best Pos already set
                        } else { // ** have the best option
                        }
                    } else {
                        // this player is better as he plays the right position
                        bestPlayer = bi;
                        bestPA = benchplayer.PA;
                        bestPos = forPosition;
                    }
                } else {
                    // this player does NOT play the desired position
                    // if the best player doesn't either, see if this guy does better
                    if (bestPos != forPosition) {
                        // ok.. current best player doesn't play the desired position
                        var pi = utils.getPositionNumber(benchplayer.Pos);
                        var adjust = canPlay[pi - 1] ? canPlay[pi - 1][forPositionIndex] : "-";
                        if (adjust != "CP") {
                            // then he can play
                        } else {
                            // he cannot play this position
                            var a = adjust;
                        }

                    }
                }
            }

        }

        function getBattingOrderIndex(order, position) {
            for (let i = 0; i < order.length; i++) {
                if (order[i][0] == position)
                    return i;
            }
            return false;
        }

        function getPlayerFromDepth(depth, playersUsed, targetPosition, rule, minPA) {
            var newPlayerIndex = null;

            if (rule == "match") {
                var depthIndex = depthChartIndex.indexOf(targetPosition);

                for (d = 0; d < depth[depthIndex].Players.length; d++) {
                    var tryPlayer = depth[depthIndex].Players[d];
                    if (playersUsed.indexOf(tryPlayer.PlayerId) == -1) {
                        if (tryPlayer.highestPA && tryPlayer.eStats[tryPlayer.highestPA].Batting.PA >= minPA) {
                            newPlayerIndex = { position: depthIndex, depth: d };
                            break;
                        }
                    }
                    if (newPlayerIndex)
                        break;
                }
            } else {
                // go through the depth

                for (let i = 0; i < depth.length; i++) {
                    if (rule == "any" || (rule == "no pitchers" && (i < 8 || i > 10))) {
                        for (d = 0; d < depth[i].Players.length; d++) {
                            var tryPlayer = depth[i].Players[d];
                            if (playersUsed.indexOf(tryPlayer.PlayerId) == -1) {
                                if (tryPlayer.highestPA && tryPlayer.eStats[tryPlayer.highestPA].Batting.PA >= minPA) {
                                    newPlayerIndex = { position: i, depth: d };
                                    break;
                                }
                            }

                        }
                    }
                    if (newPlayerIndex)
                        break;
                }
            }

            if (!newPlayerIndex && minPA == 0) {
                // grab the first non-used player...
                for (let i = 0; i < depth.length; i++) {

                    for (d = 0; d < depth[i].Players.length; d++) {
                        var tryPlayer = depth[i].Players[d];
                        if (playersUsed.indexOf(tryPlayer.PlayerId) == -1) {
                            newPlayerIndex = { position: i, depth: d };
                            if (!tryPlayer.eStats) {
                                tryPlayer.eStats = { ZeroStats: true };
                            }
                            break;
                        }
                    }

                    if (newPlayerIndex)
                        break;
                }
            }
            return (newPlayerIndex);
        }

        function getPinchHitterFromBench(depthChart, bench, playersUsed, minPA) {
            var newPlayer = null;

            if (depthChart && depthChart[12]) {
                // try to find the first pinch hitter
                for (let d = 0; d < depthChart[12].Players.length; d++) {
                    var tryPlayer = depthChart[12].Players[d];
                    if (playersUsed.indexOf(tryPlayer.PlayerId) == -1) {
                        if (!tryPlayer.Primary || tryPlayer.Primary.indexOf("P") == -1 || tryPlayer.Primary.indexOf("DH") >= 0) {
                            if (tryPlayer.highestPA && tryPlayer.eStats[tryPlayer.highestPA].Batting.PA >= minPA) {
                                return (tryPlayer);
                                break;
                            }
                        }
                    }
                }
            }

            for (let b = 0; b < bench.length; b++) {
                var tryPlayer = bench[b];
                if (playersUsed.indexOf(tryPlayer.PlayerId) == -1) {
                    if (!tryPlayer.Primary || tryPlayer.Primary[0] != "P") {
                        if (tryPlayer.highestPA && tryPlayer.eStats[tryPlayer.highestPA].Batting.PA >= minPA) {
                            newPlayer = bench[b];
                            break;
                        }
                    }
                }
                if (newPlayer)
                    break;
            }

            return (newPlayer);
        }

        function putPlayerInBattingOrder(context, team, depth, depthPositionIndex, depthIndex, order, orderIndex, positionsFilled, battingOrderMembers, position) {
            // cheating here.. if depth is null, then depthPositionIndex is actually the next batter!
            var nextBatter = depthPositionIndex;

            // if NOT already a starter
            if (depth) {
                // depth is from the depth array of players grouped by position

                nextBatter = depth[depthPositionIndex].Players[depthIndex];
                if (team.missingslots && team.missingslots.length > 0) {
                    for (let m = 0; m < team.missingslots.length; m++) {
                        if (team.missingslots[m] == orderIndex) {
                            team.missingslots.splice(m, 1);
                            break;
                        }
                    }
                }

                if (team.missingpositions && team.missingpositions.indexOf(position) >= 0)
                    team.missingpositions.splice(team.missingpositions.indexOf(position), 1);

                if (team.missingslots.length == 0)
                    team.illegallineup = false;
            } else {

            }

            // do this for all...
            nextBatter.StartingPosition = position;
            order[orderIndex] = nextBatter;
            positionsFilled[positionsIndex.indexOf(position)] = true;
            battingOrderMembers.push(nextBatter.PlayerId);

            var gameDate = null;

            // check on weirdness with pitcher
            if (!nextBatter.FirstFullOffensiveGame && position == "P" && nextBatter.FirstFullPitchingGame) {
                nextBatter.FirstFullOffensiveGame = nextBatter.FirstFullPitchingGame;
            }

            if ((!nextBatter.FirstFullOffensiveGame && !nextBatter.useStats) || position == "PH") {
                if (nextBatter.PA1 && nextBatter.PA1.length > 0) {
                    // this doesn't mean he has only 1 PA here.
                    if (position != "PH") {
                        nextBatter.FirstFullOffensiveGame = nextBatter.PA1[0];
                    } else {

                        // only 1 pa for pinch hitters.
                        var partialStats = new Array(3);
                        partialStats[0] = getBestStats(nextBatter, 1, null, false); // get 1 Major league PA...
                        if (!partialStats[0]) {
                            partialStats[0] = getBestMinorStats(nextBatter, 1, null, false); // find a Minor league PA
                        }
                        nextBatter.useStats = partialStats;
                        nextBatter.FirstFullOffensiveGame = null;

                    }

                } else {
                    var partialStats = new Array(3);
                    partialStats[0] = getBestStats(nextBatter, 1, null, true); // get exactly 1 Major league PA
                    if (!partialStats[0]) {
                        partialStats[0] = getBestStats(nextBatter, 1, null, false); // find a Major league PA
                    }
                    if (!partialStats[0]) {
                        partialStats[0] = getBestMinorStats(nextBatter, 1, null, true); // find exactly 1 Minor league PA
                    }
                    if (!partialStats[0]) {
                        partialStats[0] = getBestMinorStats(nextBatter, 1, null, false); // find a Minor league PA
                    }
                    nextBatter.useStats = partialStats;
                    nextBatter.FirstFullOffensiveGame = null;
                }
            } else {
                // did have a full game of stats, but these were too much.
                if (nextBatter.useStats) {
                    nextBatter.FirstFullOffensiveGame = null;
                }
            }

            if (nextBatter.FirstFullOffensiveGame) {
                gameDate = nextBatter.FirstFullOffensiveGame

                var fstats = null;
                var batstats = null;
                var brstats = null;
                if (gameDate) {
                    if (nextBatter.eStats[gameDate].Fielding) {
                        fstats = nextBatter.eStats[gameDate].Fielding;
                    } else {

                        nextBatter.eStats[gameDate].Fielding = {};
                        fstats = nextBatter.eStats[gameDate].Fielding;
                    }


                    if (nextBatter.eStats[gameDate].Batting)
                        batstats = nextBatter.eStats[gameDate].Batting;


                    if (nextBatter.eStats[gameDate].Baserunning) {
                        brstats = nextBatter.eStats[gameDate].Baserunning;
                    } else {
                        nextBatter.eStats[gameDate].Baserunning = { Bases: 0 };
                        brstats = nextBatter.eStats[gameDate].Baserunning;
                    }
                }
            } else {
                // we need to add up the stats from the previous games!
                var fstats = null;
                var batstats = null;
                var brstats = null;
                if (nextBatter.useStats) {
                    for (var g = 0; g < nextBatter.useStats.length; g++) {

                        // there are empty array elements, skip them.
                        if (nextBatter.useStats[g]) {
                            // figure the partials!
                            var ratio = 1.0;
                            var roundup = true;
                            if (nextBatter.useStats[g].UsingPartial == "new") {
                                ratio = nextBatter.useStats[g].Ratio;
                            } else if (nextBatter.useStats[g].UsingPartial == "partialExists") {
                                ratio = nextBatter.useStats[g].Ratio;
                                roundup = false;
                            }
                            gameDate = nextBatter.useStats[g].GameDay;
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
                }
            }

            insertOuts(batstats);
            context.calculateDefensiveStats(fstats, nextBatter, team.extra.Coordinators);
            context.calculateBases(brstats, nextBatter, team.extra.Coordinators);

            context.calculateBattingLW(batstats);
            context.calculateFieldingLW(fstats, nextBatter, team.extra.Coordinators);
            batters.battingOrder[orderIndex] = new player(nextBatter, fstats, batstats, brstats, i, gameDate, calendarDate, nextBatter.StartingPosition, orderIndex);

            /*
             var nextPlayer = depth[depthPositionIndex].Players[depthIndex];
             nextPlayer.StartingPosition = position;
             order[orderIndex] = nextPlayer;
             positionsFilled[positionsIndex.indexOf(position)] = true;
             battingOrderMembers.push(nextPlayer.PlayerId);
 
             var gameDate = null;
             if (nextPlayer.FirstFullOffensiveGame) {
                 gameDate = nextPlayer.FirstFullOffensiveGame
             } else if (nextPlayer.highestIP) {
                 gameDate = nextPlayer.highestIP;
             } else {
                 // find the right date?
                 for (PA = 2; PA >= 0; PA--) {
                     for (day in player.eStats) {
                         if (day.charAt(0) == "2" && nextPlayer.eStats[day].Batting && nextPlayer.eStats[day].Batting.PA > PA) {
                             gameDate = day;
                         }
                     }
                 }
             }
             var fstats = null;
             if (nextPlayer.eStats[gameDate].Fielding) {
                 fstats = nextPlayer.eStats[gameDate].Fielding;
             } else {
                 nextPlayer.eStats[gameDate].Fielding = {};
                 fstats = nextPlayer.eStats[gameDate].Fielding;
             }
 
             var batstats = null;
             if (nextPlayer.eStats[gameDate].Batting) {
                 batstats = nextPlayer.eStats[gameDate].Batting;
                 batstats.OUTS = batstats.AB - batstats.H;
             }
 
             var brstats = null;
             if (nextPlayer.eStats[gameDate].Baserunning) {
                 brstats = nextPlayer.eStats[gameDate].Baserunning;
             } else {
                 nextPlayer.eStats[gameDate].Baserunning = { Bases: 0 };
                 brstats = nextPlayer.eStats[gameDate].Baserunning;
             }
 
             context.calculateDefensiveStats(fstats, nextPlayer);
             context.calculateBases(brstats, nextPlayer);
 
             context.calculateBattingLW(batstats);
             context.calculateFieldingLW(fstats, nextPlayer);
 
 
             batters.battingOrder[orderIndex] = new player(nextPlayer, fstats, batstats, brstats, i, gameDate, calendarDate, nextPlayer.StartingPosition, orderIndex);
 
             if (team.missingslots)
                 team.missingslots.slice(orderIndex, 1);
 
             if (team.missingpositions && team.missingpositions.indexOf(position) >= 0)
                 team.missingpositions.slice(team.missingpositions.indexOf(position), 1);
 
             if (team.missingslots.length = 0)
                 team.illegallineup = false;
                 */

        }

        // this is for putting in a player to pinch hit, or pinch run....
        function putSubstituteInBattingOrder(context, team, depth, depthPositionIndex, depthIndex, order, orderIndex, positionsFilled, battingOrderMembers, position) {

            // get the player...
            var nextPlayer = depth[depthPositionIndex].Players[depthIndex];
            nextPlayer.StartingPosition = position;

            var subSlot = -1;
            var subIndex = 0;
            var originalSlot = -1;
            for (var b = 0; b < order.length; b++) {
                if (order[b].Slot == orderIndex) {
                    subSlot = b;
                    subIndex++;     // moves to 1, 2 so we can add to the base slot #.. can do this multiple times.
                }
                if (originalSlot == -1) {
                    originalSlot = b;
                }
            }
            nextPlayer.SubSlot = subIndex;

            // add sub slot!
            //  WAS: order[orderIndex] = nextPlayer;
            // but, need to add a slot in the batting order, so insert him after all the others in the same slot.
            order.splice(subSlot, 0, nextPlayer)

            battingOrderMembers.push(nextPlayer.PlayerId);

            var gameDate = null;
            if (nextPlayer.FirstFullOffensiveGame) {
                gameDate = nextPlayer.FirstFullOffensiveGame
            } else if (nextPlayer.highestIP) {
                gameDate = nextPlayer.highestIP;
            } else {
                // find the right date?
                for (PA = 2; PA >= 0; PA--) {
                    for (day in player.eStats) {
                        if (day.charAt(0) == "2" && nextPlayer.eStats[day].Batting && nextPlayer.eStats[day].Batting.PA > PA) {
                            gameDate = day;
                        }
                    }
                }
            }
            var fstats = null;
            if (nextPlayer.eStats[gameDate].Fielding) {
                fstats = nextPlayer.eStats[gameDate].Fielding;
            } else {
                nextPlayer.eStats[gameDate].Fielding = {};
                fstats = nextPlayer.eStats[gameDate].Fielding;
            }

            var batstats = null;
            if (nextPlayer.eStats[gameDate].Batting) {
                batstats = nextPlayer.eStats[gameDate].Batting;
                batstats.OUTS = batstats.AB - batstats.H;
            }

            var brstats = null;
            if (nextPlayer.eStats[gameDate].Baserunning) {
                brstats = nextPlayer.eStats[gameDate].Baserunning;
            } else {
                nextPlayer.eStats[gameDate].Baserunning = { Bases: 0 };
                brstats = nextPlayer.eStats[gameDate].Baserunning;
            }

            context.calculateDefensiveStats(fstats, nextPlayer, team.extra.Coordinators);
            context.calculateBases(brstats, nextPlayer, team.extra.Coordinators);

            context.calculateBattingLW(batstats);
            context.calculateFieldingLW(fstats, nextPlayer, team.extra.Coordinators);

            // WAS just a drop in, now have to splice it in...
            // batters.battingOrder[orderIndex] = new player(nextPlayer, fstats, batstats, brstats, i, gameDate, calendarDate, nextPlayer.StartingPosition, orderIndex);
            batters.battingOrder.splice(subSlot, 0, new player(nextPlayer, fstats, batstats, brstats, i, gameDate, calendarDate, nextPlayer.StartingPosition, orderIndex));

            /*
            if (team.missingslots )
                  team.missingslots.slice(orderIndex, 1);
    
            if( team.missingpositions && team.missingpositions.indexOf(position) >= 0 )
                team.missingpositions.slice( team.missingpositions.indexOf(position), 1);
            
            if( team.missingslots.length = 0) 
                team.illegallineup = false;
            */

        }

        function canPlayPosition(testPlayer, position) {
            if ((testPlayer.Primary.indexOf(position) >= 0)
                || (testPlayer.Secondary.indexOf(position) >= 0)
                || (testPlayer.Tertiary.indexOf(position) >= 0))
                return true;

            // else need to work a little harder
            var alp = algorithms.Fielding.AdjustPosition;
            var targetIndex = alp.positions.indexOf(position) + 1;    // the 0th spot is the player position for reference

            if (testPlayer.Primary) {

                for (var p = 0; p < testPlayer.Primary.length; p++) {
                    // for each primary position, see if he can play the target position
                    // if ANY of them work out, then return true
                    var primeIndex = alp.positions.indexOf(testPlayer.Primary[p]);
                    if (primeIndex >= 0 && alp.value[primeIndex][targetIndex] != "CP") {
                        // then CAN play this position..
                        return true;
                    }

                }
            }

            return false;
        }

        class player {
            constructor(player, fstats, bstats, brstats, rosterIndex, gameDate, calendarDate, position, slot) {

                this.FullName = player.FullName;
                this.PlayerId = player.PlayerId;
                this.MlbId = player.MlbId;
                this.Status = player.Status;
                this.useStats = null;
                this.sourceLevel = "Majors";
                if (player.useStats)
                    this.useStats = player.useStats;

                if (bstats) {
                    bstats.MlbId = this.MlbId;
                }

                this.Slot = slot;
                this.SubSlot = 0;
                if (player.SubSlot) {
                    this.SubSlot = player.SubSlot;
                }
                this.Primary = player.Primary;
                this.Secondary = player.Secondary;
                this.Tertiary = player.Tertiary;

                this.RosterIndex = [rosterIndex, gameDate];

                this.Pos = position;
                this.PA = 0;
                this.bStats = [];
                this.gameDates = [];
                this.fStats = [];
                this.brStats = [];

                this.YTDSpeed = 0;
                this.YTDISO = 0;
                if (player.eStats) {
                    if (player.eStats.YTDTotalBatting) {
                        this.YTDSpeed = player.eStats.YTDTotalBatting.Speed;
                        this.YTDISO = player.eStats.YTDTotalBatting.ISO;
                    } else if (player.eStats.YTDTotalFielding) {
                        this.YTDSpeed = player.eStats.YTDTotalFielding.Speed;
                    }
                }

                this.YTDcFramingRuns = 0;
                this.YTDcBlockingRuns = 0;
                if (player.eStats) {
                    if (player.eStats.YTDTotalFielding) {
                        this.YTDcBlockingRuns = player.eStats.YTDTotalFielding.cBlockingRuns;
                        this.YTDcFramingRuns = player.eStats.YTDTotalFielding.cFramingRuns;
                    }
                }

                if (bstats) {
                    this.PA = bstats.PA;
                    this.bStats.push(bstats);
                    this.gameDates.push(gameDate);
                    this.CalendarDay = calendarDate;
                    this.sourceLevel = bstats.Level;
                    if (fstats) {
                        this.fStats.push(fstats);
                    }
                    if (brstats) {
                        this.brStats.push(brstats);
                        // calculate the baserunningLW here...
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
                        var bases = (brstats.StolenBases2B * 0.15) + (brstats.StolenBases3B * 0.225);
                        bases += (brstats.StolenBasesHP * 0.3);
                        bases += (brstats.CaughtStealing2B * -0.7);
                        bases += (brstats.CaughtStealing3B * -1);
                        bases += (brstats.CaughtStealingHP * -1.3);
                        bases += (brstats.Pickoffs * -0.7);
                        bases += (brstats.OutsOnBase * -0.7);
                        bases += (brstats.AdvancedOnFlyBall * 0.15);
                        bases += (brstats.FirstToThird1B * 0.225);
                        bases += (brstats.FirstToHome2B * 0.375);
                        bases += (brstats.SecondToHome1B * 0.3);
                        this.brStats.Bases = bases;

                    }


                    /*
                    // not enough at bats for full game?
                    if (this.PA < 3) {
                        // add some more batting stats if possible
                        var target = 3 - this.PA;  // either 1 or 2
                        var gameDate2 = -1;
                        for (let i = 0; i < player.gamesList.length; i++) {
                            if (player.gamesList[i] != gameDate && player.eStats[player.gamesList[i]].Batting) {
                                if (player.eStats[player.gamesList[i]].Batting.PA == target) {
                                    this.bStats.push(player.eStats[player.gamesList[i]].Batting);
                                    this.gameDates.push(player.gamesList[i]);
 
                                    if (player.eStats[player.gamesList[i]].Fielding) {
                                        this.fStats.push( player.eStats[player.gamesList[i]].Fielding );
                                    }
 
                                    if (player.eStats[player.gamesList[i]].Baserunning) {
                                        this.fStats.push( player.eStats[player.gamesList[i]].Baserunning );
                                    }
                        
                                    gameDate2 = player.gamesList[i];
                                    this.PA += player.eStats[player.gamesList[i]].Batting.PA;
                                }
                            }
                        }
                      
                   
                        // make sure it's 3!
                
                        if (this.PA < 3) {
                            // add some more batting stats if possible
                            var target = 3 - this.PA;  // either 1 or 2
                            for (var i = 0; i < player.gamesList.length; i++) {
                                if (player.gamesList[i] != gameDate && player.gamesList[i] != gameDate2 && player.eStats[player.gamesList[i]].Batting) {
                                    if (player.eStats[player.gamesList[i]].Batting.PA == target) {
                                        this.bStats.push(player.eStats[player.gamesList[i]].Batting);
                                        this.gameDates.push(player.gamesList[i]);
 
                                        if (player.eStats[player.gamesList[i]].Fielding) {
                                            this.fStats.push( player.eStats[player.gamesList[i]].Fielding );
                                        }
    
                                        if (player.eStats[player.gamesList[i]].Baserunning) {
                                            this.fStats.push( player.eStats[player.gamesList[i]].Baserunning );
                                        }
 
                                        this.PA += player.eStats[player.gamesList[i]].Batting.PA;
                                    }
                                }
                            }
 
                        }
                      
 
                    }
                    */

                } else {
                    this.bStats.push({ MlbId: this.MlbId });
                    if (!fstats)
                        this.fStats.push({ MlbId: this.MlbId });
                    if (!brstats)
                        this.brStats.push({ MlbId: this.MlbId });
                }



                this.baserunning = {};
            }
        }


        getBestStats = function (batter, minPA, exclude, exact) {
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
                                if( !batter.PartialGames ) {
                                    var mustbepitcher = true;
                                }
                                if (batter.ParticalGames && batter.PartialGames[gameDay]) {
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
        }

        getBestMinorStats = function (batter, minPA, exclude, exact) {
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
                                if (batter.PartialGames && batter.PartialGames[gameDay]) {
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
        }

        getBestStarterStats = function (batter, minPA, exclude, exact) {
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
                if( batter.PA2 && batter.PA2.length>0)
                    PAAvailable = batter.PA2.length * 2;
                    if( batter.PA1 && batter.PA1.length>0)
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

                            if( gameDay.includes("Minors") && PAAvailable >= minPA ) {
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
        }

        getStartingStatsToUse = function (player, pos, order, isPitcher, orderIndex, justTesting) {

            var partialStats = new Array(3);

            var useStats = getBestStats(order[orderIndex], 3, null, false); // get a minimum of 3 PAs
            if (useStats) {

                // ok, can use this player as is!!! done with this slot.

                if (!justTesting) {
                    if (useStats.Ratio < 1) {
                        order[orderIndex].useStats = [useStats];   // it's a list of game days and amounts of PA per game day!
                        order[orderIndex].FirstFullOffensiveGame = null; //useStats.GameDay
                    } else {
                        order[orderIndex].FirstFullOffensiveGame = useStats.GameDay;
                    }

                    order[orderIndex].StartingPosition = pos;
                    order[orderIndex].Position = pos;

                    // positionsFilled[positionsIndex.indexOf(pos)] = true;
                    // battingOrderMembers.push(order[i].PlayerId);
                } else {
                    return (useStats)
                }

            } else {

                // doesn't have any single game with 3+ PAs, try to construct a full game from 1 and 2 PA games
                var partialStats = new Array(3);
                partialStats[0] = getBestStats(order[orderIndex], 2, null, true); // get exactly 2 PAs
                if (partialStats[0]) {
                    // then just need one more! try for exact (last param=true)
                    partialStats[1] = getBestStats(order[orderIndex], 1, partialStats, true); // get an exact count of 1 PAs
                    if (partialStats[1]) {

                        // yes, exactly 1 PA!
                        useStats = partialStats; // this will indicate that we're done! 
                    } else {
                        // try one more time to get at least 1
                        partialStats[1] = getBestStats(order[orderIndex], 1, partialStats, false); // get a minimum of 1 PAs
                        if (partialStats[1] || isPitcher) {
                            // yes, at least 1 PA!
                            useStats = partialStats; // this will indicate that we're done! 
                        }
                    }
                } else {

                    // need at least 3! and we know there aren't any 3+ or 2 PA games, get all 1s
                    partialStats[0] = getBestStats(order[orderIndex], 1, null, false); // get a minimum of 1 PAs
                    if (partialStats[0]) {
                        // then still need 2 more
                        partialStats[1] = getBestStats(order[orderIndex], 1, partialStats, false); // get a minimum of 1 PAs
                        if (partialStats[1] || isPitcher) {
                            partialStats[2] = getBestStats(order[orderIndex], 1, partialStats, false); // get a minimum of 1 PAs
                            if (partialStats[2] || isPitcher) {
                                useStats = partialStats; // this will indicate that we're done!
                            }
                        }
                    } else if (isPitcher) {
                        if( !justTesting) {
                        // it's a pitcher with no PAs.
                        /*
                        If the SP pitched less than two innings, keep the PAs at zero.
                        If the SP pitched between 2 innings and 4 ⅔ innings, assign 1 PA.
                        If the SP pitched between 5 innings and 6 ⅔ innings, assign 2 PAs.
                        If the SP pitched 7 innings, or more, assign 3 PAs
                        */
                        var ip = 0.1;
                        var pitcherGame = "";
                        if (player.FirstFullPitchingGame) {
                            ip = player.eStats[player.FirstFullPitchingGame].Pitching.IP;
                            pitcherGame = player.FirstFullPitchingGame;
                        } else if (player.highestIP) {
                            ip = player.eStats[player.highestIP].Pitching.IP;
                            pitcherGame = player.highestIP;

                        } else if (player.highestIPMinors) {
                            player.eStats[player.highestIPMinors].Pitching.IP;
                            pitcherGame = player.highestIPMinors;
                            player.FirstFullPitchingGame = pitcherGame;
                        }
                            var pitchingPA = 0;
                            if (ip >= 7) {
                                pitchingPA = 3;
                            } else if (ip >= 5) {
                                pitchingPA = 2;
                            } else if (ip >= 2) {
                                pitchingPA = 1;
                            }

                            if (order[orderIndex] && pitcherGame.length > 0 && order[orderIndex].eStats && order[orderIndex].eStats[pitcherGame]) {
                                if (!order[orderIndex].eStats[pitcherGame].Batting) {
                                    order[orderIndex].eStats[pitcherGame].Batting = {};
                                }
                                order[orderIndex].eStats[pitcherGame].Batting.PA = pitchingPA;
                                order[orderIndex].eStats[pitcherGame].Batting.AB = pitchingPA;
                                order[orderIndex].eStats[pitcherGame].Batting.OUT = pitchingPA;
                                order[orderIndex].eStats[pitcherGame].Batting.H = 0;
                                order[orderIndex].eStats[pitcherGame].Batting.G = 1;
                            }
                        } else {
                            return (getBestStats(order[orderIndex], 3, null, false)); // get a minimum of 3 PAs               
                        }
                    }
                }
                if (useStats) {
                    if (!justTesting) {
                        // ok.. can still use the guy
                        // ok, can use this player as is!!! done with this slot.

                        order[orderIndex].StartingPosition = pos;
                        order[orderIndex].Position = pos;
                        order[i].useStats = useStats;   // it's a list of game days and amounts of PA per game day!
                        order[i].FirstFullOffensiveGame = null;
                    }
                } else {
                    return (useStats)
                }
            }
        }

        // **********************************************************************************
        //
        // SETLINEUP() MAIN CODE
        //
        // **********************************************************************************


        var useDH = game.dhGame; // true if dh game.
        var proposedDHSLot = null;
        var calendarDate = game.CalendarDay;

        var batters = {
            battingOrder: new Array(9),  // start with 9, but may be more!
            bench: []
        }

        // ************************************
        //
        // step 1: fill the lineups with defensive players
        //         NOTE: if DH league, do NOT put in pitcher
        // ***************************************************

        var positionsFilled = new Array(11);
        positionsFilled.fill(false);
        var positionsIndex = ["CA", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "P", "DH", "PH", "CL", "RP"];
        var depthChartIndex = ["CA", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "SP", "RP", "CL", "DH", "PH", "CL", "RP"];

        var battingOrderMembers = new Array();
        var benchMembers = new Array();

        //
        // *****************************  get from real data ***************************
        //


        var players = roster;
        var order;

        // batting order array
        if (!roster.battingOrder) {
            // take a traditional order...
            if (useDH) {
                order = [
                    ["CF", -1],
                    ["2B", -1],
                    ["DH", -1],
                    ["LF", -1],
                    ["3B", -1],
                    ["RF", -1],
                    ["1B", -1],
                    ["SS", -1],
                    ["CA", -1]];
            } else {
                order = [
                    ["CF", -1],
                    ["2B", -1],
                    ["LF", -1],
                    ["3B", -1],
                    ["RF", -1],
                    ["1B", -1],
                    ["SS", -1],
                    ["CA", -1],
                    ["P", -1]];

            }
        } else {
            order = roster.battingOrder;
        }

        var dhSlot = -1;
        var pitcherSlot = -1;

        // ******************************************************************
        //
        //  TRY TO USE THE USER'S BATTING ORDER
        //       FIRST, IF THERE'S A PLAYER, WHO'S ELIGIBLE, USE HIM
        //       SECOND, IF THERE ISN'T THEN USE THE DEPTH CHART TO POPULATE THE SPOT
        //       OTHERWISE, LEAVE EMPTY FOR NOW.
        //
        // *******************************************************************************************
        var translatePositions = {
            "C": "CA",
            "CA": "CA",
            "CATCHER": "CA",
            "1B": "1B",
            "FIRSTBASE": "1B",
            "FIRST BASE": "1B",
            "SECOND BASE": "2B",
            "SECONDBASE": "2B",
            "2B": "2B",
            "3B": "3B",
            "THIRD BASE": "3B",
            "THIRDBASE": "3B",
            "SS": "SS",
            "SHORTSTOP": "SS",
            "SHORT STOP": "SS",
            "LF": "LF",
            "LEFTFIELD": "LF",
            "LEFT FIELD": "LF",

            "CF": "CF",
            "CENTERFIELD": "CF",
            "CENTER FIELD": "CF",

            "RF": "RF",
            "RIGHTFIELD": "RF",
            "RIGHT FIELD": "RF",
            "DH": "DH",
            "DESIGNATED HITTER": "DH",
            "DESIGNTATEDHITTER": "DH",

            "P": "P",
            "RP": "P",
            "SP": "P",
            "CL": "P",
            "PITCHER": "P"


        }

        // ******************************************
        //
        // try out best to use the user's batting order!!!
        // 
        // ******************************************************
        var useAdmin = false;


        // if it's not a DH game, insure the STARTING pitcher is in the lineup
        if( !useDH) {
            for( var i=0; i< order.length; i++) {
                if( order[i]) {
                    if( order[i].Position == "P") {
                        if( team.pitchingStaff.starterStats) {
                            order[i] = team.pitchingStaff.starterStats;
                        }
                    }
                }
            }
        }
        // next, before doing ANYTHING else, remove illegal players from the lineup...
        var proposedStarters = new Array(10);
        var startersList = new Array(10);
        for (var i = 0; i < 9; i++) {
            if (order[i] && (order[i].PlayerId)) {
                var pos = order[i].Position;
                if (!pos) {
                    // remove this guy
                    order[i] = null;
                } else {
                    posIndex = positionsIndex.indexOf(pos);
                    if (posIndex == -1 || positionsFilled[posIndex] != false) {
                        // remove this guy
                        order[i] = null;
                    } else {
                        // he has a position, he's the ONLY one with it.
                        positionsFilled[posIndex] = true;
                        order[i].StartingPosition = pos;
                        proposedStarters[i] = order[i].PlayerId;
                        startersList[i] = order[i];    // save the starters in case they have to start with only 2PA
                    }
                }
            }
        }


        // then make sure there's a slot for every position
        for (var i = 0; i < 9; i++) {
            if (!order[i] || !order[i].PlayerId) {
                for (var p = 0; p < 10; p++) {
                    if (positionsFilled[p] == false && ((useDH && p != 8) || (!useDH && p != 9))) {
                        order[i] = [positionsIndex[p]];
                        positionsFilled[p] = true;
                        break;
                    }
                }
            }
        }

        // used again below
        positionsFilled.fill(false);

        var starterHas2 = new Array(9);
        var starterHas1 = new Array(9);
        starterHas2.fill(false);
        starterHas1.fill(false);

        var subHas2 = new Array(9);
        subHas2.fill(false);

        // ********************** CYCLE THROUGH THE PROPOSED ORDER HERE ************************************ //

        for (var i = 0; i < 9; i++) {
            if (order[i]) {

                var pos = null;
                if (order[i].StartingPosition) {
                    pos = order[i].StartingPosition;
                } else if (order[i].Position) {
                    pos = order[i].Position;
                }
                if (pos && translatePositions[pos.toUpperCase()]) {
                    // ****************************** then have a player here , TRY TO USE HIM!

                    pos = translatePositions[pos.toUpperCase()]
                    if (positionsFilled[positionsIndex.indexOf(pos)] === false) {
                        // then haven't filled this spot yet!

                        var useStats = getBestStats(order[i], 3, null, false); // get a minimum of 3 PAs     
                        //var useStast = getStartingStatsToUse (order[i], pos, i, pos=="P", i, true);                    
                        if (!useStats && pos == "P") {

                            // special rules for pitchers who don't have at least 3PA...
                            order[i].StartingPosition = pos;
                            order[i].Position = pos;        // unfortunately, they both get used.
                            positionsFilled[positionsIndex.indexOf(pos)] = true;
                            getStartingStatsToUse(order[i], "P", order, true, i);
                            battingOrderMembers.push(order[i].PlayerId);

                        } else if (useStats) {

                            // ok, can use this player as is!!! done with this slot.

                            // only put in useStats if the ratio < 1
                            if (useStats.Ratio < 1) {
                                // create the array of useStats.
                                order[i].useStats = [useStats];   // it's a list of game days and amounts of PA per game day!
                                order[i].FirstFullOffensiveGame = null;
                            } else {
                                order[i].FirstFullOffensiveGame = useStats.GameDay
                            }
                            order[i].StartingPosition = pos;
                            order[i].Position = pos;

                            positionsFilled[positionsIndex.indexOf(pos)] = true;
                            battingOrderMembers.push(order[i].PlayerId);

                        } else {
                            // (((((((((((((( TODO: check if it's the pitcher! ))))))))))))))))))))))

                            // doesn't have any single game with 3+ PAs, try to construct a full game from 1 and 2 PA games
                            var partialStats = new Array(3);
                            partialStats[0] = getBestStats(order[i], 2, null, true); // get exactly 2 PAs
                            if (partialStats[0]) {
                                starterHas2[i] = partialStats;

                                // then just need one more! try for exact (last param=true)
                                partialStats[1] = getBestStats(order[i], 1, partialStats, true); // get an exact count of 1 PAs
                                if (partialStats[1]) {

                                    // yes, exactly 1 PA!
                                    useStats = partialStats; // this will indicate that we're done! 
                                } else {
                                    // try one more time to get at least 1
                                    partialStats[1] = getBestStats(order[i], 1, partialStats, false); // get a minimum of 1 PAs
                                    if (partialStats[1]) {
                                        // yes, at least 1 PA!
                                        useStats = partialStats; // this will indicate that we're done! 
                                    }
                                }
                            } else {

                                // need at least 3! and we know there aren't any 3+ or 2 PA games, get all 1s
                                partialStats[0] = getBestStats(order[i], 1, null, false); // get a minimum of 1 PAs
                                if (partialStats[0]) {
                                    // then still need 2 more
                                    starterHas1[i] = partialStats;
                                    partialStats[1] = getBestStats(order[i], 1, partialStats, false); // get a minimum of 1 PAs
                                    if (partialStats[1]) {

                                        // has 2!
                                        starterHas2[i] = partialStats;

                                        partialStats[2] = getBestStats(order[i], 1, partialStats, false); // get a minimum of 1 PAs
                                        if (partialStats[2]) {
                                            useStats = partialStats; // this will indicate that we're done!
                                        }
                                    }
                                }
                            }

                            // note, when you get here, useStats is an array already
                            if (useStats) {
                                // ok.. can still use the guy
                                // ok, can use this player as is!!! done with this slot.

                                order[i].useStats = useStats;   // it's a list of game days and amounts of PA per game day!
                                order[i].StartingPosition = pos;
                                order[i].Position = pos;
                                order[i].FirstFullOffensiveGame = null; // useStats.GameDay
                                positionsFilled[positionsIndex.indexOf(pos)] = true;
                                battingOrderMembers.push(order[i].PlayerId);

                            } else {

                                /**************************************************************************************/
                                /*************** If no Major stats were found, then look up Minor stats ***************/
                                /**************************************************************************************/

                                useStats = getBestMinorStats(order[i], 3, null, false); // get a minimum of 3 PAs

                                if (!useStats && pos == "P") {

                                    // special rules for pitchers who don't have at least 3PA...
                                    order[i].StartingPosition = pos;
                                    order[i].Position = pos;        // unfortunately, they both get used.
                                    positionsFilled[positionsIndex.indexOf(pos)] = true;
                                    getStartingStatsToUse(order[i], "P", order, true, i);
                                    battingOrderMembers.push(order[i].PlayerId);
        
                                } else if (useStats) {
        
                                    // ok, can use this player as is!!! done with this slot.
        
                                    // only put in useStats if the ratio < 1
                                    if (useStats.Ratio < 1) {
                                        // create the array of useStats.
                                        order[i].useStats = [useStats];   // it's a list of game days and amounts of PA per game day!
                                        order[i].FirstFullOffensiveGame = null;
                                    } else {
                                        order[i].FirstFullOffensiveGame = useStats.GameDay
                                    }
                                    order[i].StartingPosition = pos;
                                    order[i].Position = pos;
        
                                    positionsFilled[positionsIndex.indexOf(pos)] = true;
                                    battingOrderMembers.push(order[i].PlayerId);
        
                                } else {
                                    // (((((((((((((( TODO: check if it's the pitcher! ))))))))))))))))))))))
        
                                    // doesn't have any single game with 3+ PAs, try to construct a full game from 1 and 2 PA games
                                    var partialStats = new Array(3);
                                    partialStats[0] = getBestMinorStats(order[i], 2, null, true); // get exactly 2 PAs
                                    if (partialStats[0]) {
                                        starterHas2[i] = partialStats;
        
                                        // then just need one more! try for exact (last param=true)
                                        partialStats[1] = getBestMinorStats(order[i], 1, partialStats, true); // get an exact count of 1 PAs
                                        if (partialStats[1]) {
        
                                            // yes, exactly 1 PA!
                                            useStats = partialStats; // this will indicate that we're done! 
                                        } else {
                                            // try one more time to get at least 1
                                            partialStats[1] = getBestMinorStats(order[i], 1, partialStats, false); // get a minimum of 1 PAs
                                            if (partialStats[1]) {
                                                // yes, at least 1 PA!
                                                useStats = partialStats; // this will indicate that we're done! 
                                            }
                                        }
                                    } else {
        
                                        // need at least 3! and we know there aren't any 3+ or 2 PA games, get all 1s
                                        partialStats[0] = getBestMinorStats(order[i], 1, null, false); // get a minimum of 1 PAs
                                        if (partialStats[0]) {
                                            // then still need 2 more
                                            starterHas1[i] = partialStats;
                                            partialStats[1] = getBestMinorStats(order[i], 1, partialStats, false); // get a minimum of 1 PAs
                                            if (partialStats[1]) {
        
                                                // has 2!
                                                starterHas2[i] = partialStats;
        
                                                partialStats[2] = getBestMinorStats(order[i], 1, partialStats, false); // get a minimum of 1 PAs
                                                if (partialStats[2]) {
                                                    useStats = partialStats; // this will indicate that we're done!
                                                }
                                            }
                                        }
                                    }

                                    // note, when you get here, useStats is an array already
                                    if (useStats) {
                                        // ok.. can still use the guy
                                        // ok, can use this player as is!!! done with this slot.

                                        order[i].useStats = useStats;   // it's a list of game days and amounts of PA per game day!
                                        order[i].StartingPosition = pos;
                                        order[i].Position = pos;
                                        order[i].FirstFullOffensiveGame = null; // useStats.GameDay
                                        positionsFilled[positionsIndex.indexOf(pos)] = true;
                                        battingOrderMembers.push(order[i].PlayerId);

                                    } else {

                                        // can't use him.. .try to replace him...
                                        // first remove him from the batting order
                                        // dropping in an array of the position tells us where we'd LIKE that position to be filled.
                                        order[i] = [pos];
                                        var posIndex = positionsIndex.indexOf(pos);
                                        var depthIndex = depthChartIndex.indexOf(pos);
                                        if (pos == "DH")
                                            dhSlot = i;

                                        // next work through the depth chart...
                                        var foundsub = false;
                                        if (posIndex >= 0 && positionsFilled[posIndex] === false) {
                                            // not filled, then find the right person in the depth chart
                                            var depth = roster[depthIndex].Players;

                                            // remember 9 = pitcher, 10=cl, 11=rp, 12=dh, 13=extra
                                            for (var d = 0; d < depth.length; d++) {

                                                if (depth[d]) {

                                                    // see if he has minimum PAs to start
                                                    var minPAs = 0;
                                                    if (depth[d].eStats) {
                                                        if (depth[d].FirstFullOffensiveGame)
                                                            minPAs = 3;
                                                        if (depth[d].PA2) {
                                                            minPAs += 2 * depth[d].PA2.length;
                                                            minPAs += depth[d].PA1.length;
                                                        }
                                                    }

                                                    if (proposedStarters.indexOf(depth[d].PlayerId) == -1 && minPAs >= 3
                                                        && canPlayPosition(depth[d], pos)) {

                                                        // this player not yet used! and has an available full game,  put him in!
                                                        depth[d].StartingPosition = pos;
                                                        depth[d].Position = pos;        // unfortunately, they both get used.
                                                        order[i] = depth[d];
                                                        getStartingStatsToUse(depth[d], pos, order, false, i);
                                                        positionsFilled[positionsIndex.indexOf(pos)] = true;
                                                        battingOrderMembers.push(depth[d].PlayerId);
                                                        proposedStarters.splice(i, 1);  // remove the original!
                                                        proposedStarters.push(depth[d].PlayerId);

                                                        foundsub = true;
                                                        break;
                                                    }
                                                }
                                            }
                                        }

                                        if (!foundsub) {
                                            /*
                                            // then just grab the first reasonable batter...
                                            for (var pi = 0; pi < roster.length; pi++) {
                                                if (pi < 8 || pi > 10) {
                                                    var depth = roster[pi].Players;

                                                    // remember 9 = pitcher, 10=cl, 11=rp, 12=dh, 13=ph
                                                    for (var d = 0; d < depth.length; d++) {

                                                        if (depth[d]) {

                                                            var minPAs = 0;
                                                            var minPAs = 0;
                                                            if (depth[d].eStats) {
                                                                if (depth[d].FirstFullOffensiveGame)
                                                                    minPAs = 3;
                                                                if (depth[d].PA2) {
                                                                    minPAs += 2 * depth[d].PA2.length;
                                                                    minPAs += depth[d].PA1.length;
                                                                }
                                                            }

                                                            if (proposedStarters.indexOf(depth[d].PlayerId) == -1 && minPAs >= 3
                                                                && canPlayPosition(depth[d], pos)) {

                                                                // this player not yet used! plays the position, and has an available full game,  put him in!
                                                                depth[d].StartingPosition = pos;
                                                                depth[d].Position = pos;        // unfortunately, they both get used.
                                                                order[i] = depth[d];
                                                                getStartingStatsToUse(depth[d], pos, order, false, i);
                                                                positionsFilled[positionsIndex.indexOf(pos)] = true;
                                                                battingOrderMembers.push(depth[d].PlayerId);
                                                                proposedStarters.splice(i, 1);  // remove the original!
                                                                proposedStarters.push(depth[d].PlayerId);

                                                                foundsub = true;
                                                                break;
                                                            }
                                                        }
                                                    }
                                                }
                                                if (foundsub) {
                                                    break;
                                                }
                                            }
                                            */
                                        }

                                        if (!foundsub) {
                                            //abandon hope here!
                                            // will pick it up later...
                                        }

                                    }

                                }        

                            }
                        }
                    } else {
                        // trying to put two guys in the same slot, not allowed
                        // should be taken care of above.
                        //  order[i] = null;
                        var pst = order[i];
                    }
                } else {

                    // DON'T HAVE A PLAYER, BUT HAVE THE POSITION FOR THIS SPOT IN THE BATTING ORDER
                    // find the position in the array
                    pos = order[i][0];
                    if (pos == 'C')
                        pos = "CA";
                    if (pos == 'DH')
                        dhSlot = i;
                    if (pos == "P") {
                        pitcherSlot = i;
                        order[i] = team.pitchingStaff.starterStats;
                        order[i]["Pos"] = "P";
                        order[i]["Position"] = "P";
                        order[i]["StartingPosition"] = "P";
                        positionsFilled[positionsIndex.indexOf("P")] = true;
                        getStartingStatsToUse(order[i], "P", order, true, i);
                        battingOrderMembers.push(team.pitchingStaff.starterStats.PlayerId);
                        break;
                    }

                    var posIndex = positionsIndex.indexOf(pos);
                    var depthIndex = depthChartIndex.indexOf(pos);

                    if (posIndex >= 0 && positionsFilled[posIndex] === false) {
                        // have an acutal position, it's not filled, then find the right person in the depth chart
                        var foundsub = false;
                        var depth = roster[depthIndex].Players;

                        // remember 9 = pitcher, 10=cl, 11=rp, 12=dh, 13=ph
                        for (var d = 0; d < depth.length; d++) {
                            // see if he has minimum PAs to start
                            var minPAs = 0;
                            if (depth[d].eStats) {
                                if (depth[d].FirstFullOffensiveGame)
                                    minPAs = 3;
                                if (depth[d].PA2) {
                                    minPAs += 2 * depth[d].PA2.length;
                                    minPAs += depth[d].PA1.length;
                                }
                            }

                            if (proposedStarters.indexOf(depth[d].PlayerId) == -1 && minPAs >= 3
                                && canPlayPosition(depth[d], pos)) {

                                // this player not yet used! and has an available full game,  put him in!
                                depth[d].StartingPosition = pos;
                                depth[d].Position = pos;        // unfortunately, they both get used.
                                order[i] = depth[d];
                                getStartingStatsToUse(depth[d], pos, order, false, i);
                                positionsFilled[positionsIndex.indexOf(pos)] = true;
                                battingOrderMembers.push(depth[d].PlayerId);
                                proposedStarters.push(depth[d].PlayerId);

                                foundsub = true;
                                break;
                            } else {
                                if (minPAs > 1 && subHas2[i] == false) {
                                    subHas2[i] = depth[d];  // player data
                                }
                            }
                        }
                        if (!foundsub) {
                            // then just grab the first reasonable batter...
                            for (var pi = 0; pi < roster.length; pi++) {
                                if (pi < 8 || pi > 10) {
                                    var depth = roster[pi].Players;

                                    // remember 9 = pitcher, 10=cl, 11=rp, 12=dh, 13=extra
                                    for (var d = 0; d < depth.length; d++) {
                                        // see if he has minimum PAs to start
                                        var minPAs = 0;
                                        if (depth[d].eStats) {
                                            if (depth[d].FirstFullOffensiveGame)
                                                minPAs = 3;
                                            if (depth[d].PA2) {
                                                minPAs += 2 * depth[d].PA2.length;
                                                minPAs += depth[d].PA1.length;
                                            }
                                        }

                                        if (proposedStarters.indexOf(depth[d].PlayerId) == -1 && minPAs >= 3
                                            && canPlayPosition(depth[d], pos)) {

                                            // this player not yet used! and has an available full game,  put him in!
                                            depth[d].StartingPosition = pos;
                                            depth[d].Position = pos;        // unfortunately, they both get used.
                                            order[i] = depth[d];
                                            getStartingStatsToUse(depth[d], pos, order, false, i);
                                            positionsFilled[positionsIndex.indexOf(pos)] = true;
                                            battingOrderMembers.push(depth[d].PlayerId);
                                            proposedStarters.push(depth[d].PlayerId);

                                            foundsub = true;
                                            break;
                                        } else {
                                            if (minPAs > 1 && subHas2[i] == false) {
                                                subHas2[i] = depth[d];  // player data
                                            }
                                        }
                                    }
                                }
                                if (foundsub) {
                                    break;
                                }
                            }
                        }

                        if (!foundsub) {
                            //abandon hope here!
                            // will pick it up later...
                        }
                    }
                }
            }
        }



        // so, should be filled with starters... based on depth if not by user selection.
        // look for a pitcher in the nonDH league
        if (!useDH && positionsFilled[positionsIndex.indexOf("P")] === false) {
            if (team.pitchingStaff.starter) {
                // put him in the batting order....
                // find an available slot in the batting order!
                var pslot = 8;
                for (let bo = 0; bo < 9; bo++) {
                    if (!order[bo] || order[bo][0] || (order[bo] && (!order[bo].PlayerId))) {
                        pslot = bo;
                        break;
                    }
                }

                order[pslot] = team.pitchingStaff.starterStats;
                var useStats = getBestStats(order[pslot], 3, null, false); // get a minimum of 3 PAs                        
                if (!useStats && pos == "P") {

                    // special rules for pitchers who don't have at least 3PA...
                    order[pslot].StartingPosition = pos;
                    order[pslot].Position = pos;        // unfortunately, they both get used.
                    positionsFilled[positionsIndex.indexOf("P")] = true;
                    getStartingStatsToUse(order[pslot], "P", order, true, i);
                    battingOrderMembers.push(order[pslot].PlayerId);

                } else if (useStats) {

                    // ok, can use this player as is!!! done with this slot.

                    // only put in useStats if the ratio < 1
                    if (useStats.Ratio < 1) {
                        // create the array of useStats.
                        order[pslot].useStats = [useStats];   // it's a list of game days and amounts of PA per game day!
                        order[pslot].FirstFullOffensiveGame = null;
                    } else {
                        order[pslot].FirstFullOffensiveGame = useStats.GameDay
                    }
                    order[pslot].StartingPosition = "P";
                    order[pslot].Position = "P";

                    positionsFilled[positionsIndex.indexOf(pos)] = true;
                    battingOrderMembers.push(order[pslot].PlayerId);

                } else {
                    // doesn't have any single game with 3+ PAs, try to construct a full game from 1 and 2 PA games
                    var partialStats = new Array(3);
                    partialStats[0] = getBestStats(order[pslot], 2, null, true); // get exactly 2 PAs
                    if (partialStats[0]) {
                        starterHas2[pslot] = partialStats;

                        // then just need one more! try for exact (last param=true)
                        partialStats[1] = getBestStats(order[pslot], 1, partialStats, true); // get an exact count of 1 PAs
                        if (partialStats[1]) {

                            // yes, exactly 1 PA!
                            useStats = partialStats; // this will indicate that we're done! 
                        } else {
                            // try one more time to get at least 1
                            partialStats[1] = getBestStats(order[pslot], 1, partialStats, false); // get a minimum of 1 PAs
                            if (partialStats[1]) {
                                // yes, at least 1 PA!
                                useStats = partialStats; // this will indicate that we're done! 
                            }
                        }
                    } else {

                        // need at least 3! and we know there aren't any 3+ or 2 PA games, get all 1s
                        partialStats[0] = getBestStats(order[pslot], 1, null, false); // get a minimum of 1 PAs
                        if (partialStats[0]) {
                            // then still need 2 more
                            starterHas1[pslot] = partialStats;
                            partialStats[pslot] = getBestStats(order[pslot], 1, partialStats, false); // get a minimum of 1 PAs
                            if (partialStats[1]) {

                                // has 2!
                                starterHas2[pslot] = partialStats;

                                partialStats[2] = getBestStats(order[pslot], 1, partialStats, false); // get a minimum of 1 PAs
                                if (partialStats[2]) {
                                    useStats = partialStats; // this will indicate that we're done!
                                }
                            }
                        }
                    }

                    // note, when you get here, useStats is an array already
                    if (useStats) {
                        // ok.. can still use the guy
                        // ok, can use this player as is!!! done with this slot.

                        order[pslot].useStats = useStats;   // it's a list of game days and amounts of PA per game day!
                        order[pslot].StartingPosition = pos;
                        order[pslot].Position = pos;
                        order[pslot].FirstFullOffensiveGame = null; // useStats.GameDay
                        positionsFilled[positionsIndex.indexOf(pos)] = true;
                        battingOrderMembers.push(order[i].PlayerId);

                    }
                }
            }
        }
        // now fill in empty slots...start with dh if needed

        if (useDH && positionsFilled[positionsIndex.indexOf("DH")] === false && dhSlot >= 0 && !starterHas2[dhSlot]) {
            // then loop through the roster, looking for players who aren't in the batting order
            // and players who have a starting FirstFullOffensiveGame and have the highest OPS
            var highOPS = 0;
            var dhSelected = null;
            var dhBackup = null;
            var DHuseStats;

            if (dhSlot == -1) {
                // find an available slot in the batting order!
                for (let bo = 0; bo < 9; bo++) {
                    if (!order[bo] || order[bo][0] || (order[bo] && (!order[bo].PlayerId))) {
                        dhSlot = bo;
                        break;
                    }
                }
            }

            // there's an issue if dhSlot is still -1.
            if (dhSlot == -1)
                dhSlot = 9; // force it to after the lineup.

            for (var i = 0; i < roster.length; i++) {
                if (roster[i] && ((i < 8) || (i >= 11))) {
                    // got the depth spot, now look through it
                    for (d = 0; d < roster[i].Players.length; d++) {
                        if (battingOrderMembers.indexOf(roster[i].Players[d].PlayerId) == -1) {

                            var minPAs = 0;
                            var bestStats = null;
                            if (roster[i].Players[d].eStats) {
                                if (roster[i].FirstFullOffensiveGame) {
                                    minPAs = 3;
                                    bestStats = getBestStats(roster[i].Players[d], 3, null); // get a minimum of 3 PAs
                                } else {
                                    if (roster[i].Players[d].PA2 && roster[i].Players[d].PA2.length > 0) {
                                        minPAs += 2 * roster[i].Players[d].PA2.length;
                                        if (subHas2[dhSlot] == false)
                                            subHas2[dhSlot] = roster[i].Players[d];
                                    }
                                    if (roster[i].Players[d].PA1)
                                        minPAs += roster[i].Players[d].PA1.length;
                                }
                            }


                            if (minPAs >= 3 && bestStats && bestStats.Batting.SLG > highOPS) {
                                // a candidate
                                highOPS = bestStats.Batting.SLG;
                                dhSelected = roster[i].Players[d];
                            } else {
                                if (!dhSelected) {
                                    // see if this can be a backup...
                                    if (roster[i].Players[d].highestIP) {
                                        dhBackup = roster[i].Players[d];
                                    } else {
                                        if (roster[i].Players[d].eStats) {
                                            for (day in roster[i].Players[d].eStats) {
                                                if (day.charAt(0) == "2" && roster[i].Players[d].eStats[day].Batting && roster[i].Players[d].eStats[day].Batting.IP > 0) {
                                                    dhBackup = roster[i].Players[d];
                                                }
                                            }
                                        }
                                    }

                                }
                            }
                        }
                    }

                }
            }

            if (!dhSelected && dhBackup) {
                dhSelected = dhBackup
            };

            if (dhSelected) {
                // drop in the batting order
                dhSelected.StartingPosition = "DH";
                order[dhSlot] = dhSelected;
                order[dhSlot].Position = "DH";
                getStartingStatsToUse(order[dhSlot], "DH", order, false, dhSlot);
                positionsFilled[positionsIndex.indexOf("DH")] = true;
                battingOrderMembers.push(dhSelected.PlayerId);

            } else {
                // wow, couldn't find ANY batters?
                console.log("no batters left for DH");
            }
        } else {
            // drop the pitcher in the lineup!
            if (positionsFilled[positionsIndex.indexOf("P")] === false) {
                console.log("pitchers slot empty!");
            }
        }

        // next, find players to fill empty slots or bring enough PAs to the game!
        team.missingslots = [];
        team.missingpositions = [];
        team.partialslots = [];

        function getBatterPA(batter) {
            if (batter.FirstFullOffensiveGame) {
                var PA = batter.eStats[batter.FirstFullOffensiveGame].Batting.PA;
                return PA;
            }

            var PA = 0;
            if (batter.useStats) {
                for (var b = 0; b < batter.useStats.length; b++) {
                    if (batter.useStats[b]) {
                        PA += Number(batter.useStats[b].PA);
                    }
                }
            }
            return PA;
        }

        // *********************************** see if any starters can return to the lineup *****************************//
        for (var b = 0; b < 9; b++) {
            // if he's the nextBatter is in the batting order...
            var nextBatter = order[b];
            if (nextBatter && (battingOrderMembers.indexOf(nextBatter.PlayerId) >= 0)) {
            } else {
                // this is still an empty slot...
                // see if we can drop in a partial now...
                if (starterHas2[b] && battingOrderMembers.indexOf(nextBatter.PlayerId) == -1) {

                    // pitcher has 2 PA, so put him in and grab 1 PH...
                    order[b] = startersList[b];  // put the guy back!
                    var useStats = getBestStats(order[b], 2, null, true); // get a minimum of 2 PAs    
                    order[b].useStats = starterHas2[b];   // it's a list of game days and amounts of PA per game day!  
                    battingOrderMembers.push(order[b].PlayerId);
                }
            }
        }


        // **************** Desparation time A ****************************** //
        // if there are still empty slots, look for a starter with enough Minor league PA
        for (var b = 0; b < 9; b++) {
            // if he's the nextBatter is in the batting order...
            var nextBatter = order[b];
            if (nextBatter && (battingOrderMembers.indexOf(nextBatter.PlayerId) >= 0)) {
            } else {
                // this is still an empty slot...
                // see if we can drop in a minor league game
                if (startersList[b] && battingOrderMembers.indexOf(startersList[b].PlayerId) == -1
                    && startersList[b].highestPA && startersList[b].highestPA.includes("Minors")
                ) {

                    order[b] = startersList[b];  // put the guy back!
                    //   var useStats = getBestStats(order[b], 2, null, true); // get a minimum of 2 PAs   
                    var nextStats = startersList[b].eStats[startersList[b].highestPA];
                    var usingPA = nextStats.Batting.PA;

                    var statsSet = {
                        GameDay: startersList[b].highestPA,
                        Batting: nextStats.Batting,
                        Baserunning: nextStats.Baserunning ? nextStats.Baserunning : null,
                        Fielding: nextStats.Fielding ? nextStats.Fielding : null,
                        UsingPartial: false,
                        TotalPA: usingPA,  // original available
                        PA: usingPA,     // this is the number for this game's play
                        UsedPA: 0,     // this is the number of pre-used stats...
                        Ratio: 1.0
                    }
                    order[b].useStats = [statsSet];   // it's a list of game days and amounts of PA per game day!  
                    battingOrderMembers.push(order[b].PlayerId);
                }
            }
        }

        // **************** Desparation time B ****************************** //
        // if there are still empty slots, look for a starter with 1PA
        for (var b = 0; b < 9; b++) {
            // if he's the nextBatter is in the batting order...
            var nextBatter = order[b];
            if (nextBatter && (battingOrderMembers.indexOf(nextBatter.PlayerId) >= 0)) {
            } else {
                // this is still an empty slot...
                // see if we can drop in a partial now...
                if (starterHas1[b] && battingOrderMembers.indexOf(nextBatter.PlayerId) == -1) {

                    // pitcher has 1 PA, so put him in and wait for 2 PH...
                    order[b] = startersList[b];  // put the guy back!
                    var useStats = getBestStats(order[b], 2, null, true); // get a minimum of 2 PAs    
                    order[b].useStats = starterHas1[b];   // it's a list of game days and amounts of PA per game day!  
                    battingOrderMembers.push(order[b].PlayerId);
                }
            }
        }
        // *********************************** transfer the order into the box score's order **************************** //

        for (var b = 8; b >= 0; b--) {
            var nextBatter = order[b];

            // if he's the nextBatter is in the batting order...
            if (nextBatter && (battingOrderMembers.indexOf(nextBatter.PlayerId) >= 0)) {

                // then have a full out starter here...
                putPlayerInBattingOrder(this, team, null, nextBatter, null, order, b, positionsFilled, battingOrderMembers, nextBatter.StartingPosition);

                if (getBatterPA(nextBatter) < 3) {
                    // need a pinch hitter
                    var benchPlayer = getPinchHitterFromBench(roster, bench, battingOrderMembers, 1);
                    if (benchPlayer) {
                        order.splice(b + 1, 0, benchPlayer);
                        // then have a pinch hitter
                        // order[b + 1].Position = "PH";
                        batters.battingOrder.splice(b + 1, 0, {});
                        putPlayerInBattingOrder(this, team, null, benchPlayer, null, order, b + 1, positionsFilled, battingOrderMembers, "PH");
                        batters.battingOrder[b + 1].Slot = batters.battingOrder[b].Slot;
                        batters.battingOrder[b + 1].SubSlot = 1;

                        if (getBatterPA(nextBatter) < 2) {
                            // need a 2nd pinch hitter
                            var benchPlayer = getPinchHitterFromBench(roster, bench, battingOrderMembers, 1);
                            if (benchPlayer) {
                                order.splice(b + 2, 0, benchPlayer);
                                // then have a pinch hitter
                                //order[b + 2].Position = "PH";
                                batters.battingOrder.splice(b + 2, 0, {});
                                putPlayerInBattingOrder(this, team, null, benchPlayer, null, order, b + 2, positionsFilled, battingOrderMembers, "PH");
                                batters.battingOrder[b + 2].Slot = batters.battingOrder[b].Slot;
                                batters.battingOrder[b + 2].SubSlot = 2;
                            }
                        }
                        if (getBatterPA(nextBatter) == 0) {
                            // need a 3rd pinch hitter
                            var benchPlayer = getPinchHitterFromBench(roster, bench, battingOrderMembers, 1);
                            if (benchPlayer) {
                                order.splice(b + 2, 0, benchPlayer);
                                // then have a pinch hitter
                                //order[b + 2].Position = "PH";
                                batters.battingOrder.splice(b + 2, 0, {});
                                putPlayerInBattingOrder(this, team, null, benchPlayer, null, order, b + 2, positionsFilled, battingOrderMembers, "PH");
                                batters.battingOrder[b + 2].Slot = batters.battingOrder[b].Slot;
                                batters.battingOrder[b + 2].SubSlot = 2;
                            }
                        }
                    }
                }
            } else {

                // this is still an empty slot... warn the game play that this is an illegal lineup
                // see if we can drop in a partial now...
                if (starterHas2[b]) {

                    // pitcher has 2 PA, so put him in and grab 1 PH...
                    nextBatter = order[b] = startersList[b];  // put the guy back!

                    // then have a 2PA out starter here...

                    var us = getBestStats(nextBatter, 2, null, true); // get exactly 2;
                    if (us) {
                        nextBatter.useStats = [us]; // be sure it's an array
                    } else {
                        var us = [];
                        us[0] = getBestStats(nextBatter, 1, null, true); // get exactly 1;
                        us[1] = getBestStats(nextBatter, 1, us, false); // get 1 more
                        nextBatter.useStats = us;   // it's already an array
                    }

                    putPlayerInBattingOrder(this, team, null, nextBatter, null, order, b, positionsFilled, battingOrderMembers, nextBatter.Position);

                    // need a pinch hitter
                    var benchPlayer = getPinchHitterFromBench(roster, bench, battingOrderMembers, 1);
                    if (benchPlayer) {
                        order.splice(b + 1, 0, benchPlayer);
                        // then have a pinch hitter
                        // order[b + 1].Position = "PH";
                        batters.battingOrder.splice(b + 1, 0, {});
                        putPlayerInBattingOrder(this, team, null, benchPlayer, null, order, b + 1, positionsFilled, battingOrderMembers, "PH");
                        batters.battingOrder[b + 1].Slot = batters.battingOrder[b].Slot;
                        batters.battingOrder[b + 1].SubSlot = 1;

                        if (getBatterPA(nextBatter) == 1) {
                            // need a 2nd pinch hitter
                            var benchPlayer = getPinchHitterFromBench(roster, bench, battingOrderMembers, 1);
                            if (benchPlayer) {
                                order.splice(b + 2, 0, benchPlayer);
                                // then have a pinch hitter
                                //order[b + 2].Position = "PH";
                                batters.battingOrder.splice(b + 2, 0, {});
                                putPlayerInBattingOrder(this, team, null, benchPlayer, null, order, b + 2, positionsFilled, battingOrderMembers, "PH");
                                batters.battingOrder[b + 2].Slot = batters.battingOrder[b].Slot;
                                batters.battingOrder[b + 2].SubSlot = 2;
                            }
                        }
                    }
                    team.partialslots[b] = true;

                } else if (subHas2[b] && battingOrderMembers.indexOf(subHas2[b].PlayerId) == -1) {

                    nextBatter = order[b] = subHas2[b];  // put the guy back!
                    // then have a 2PA sub for the starter...
                    if (startersList[b]) {
                        nextBatter.Position = nextBatter.StartingPosition = startersList[b].Position;
                    } else {
                        if (order[b].Position) {
                            nextBatter.Position = nextBatter.StartingPosition = order[b].Position;
                        } else {
                            nextBatter.Position = nextBatter.StartingPosition = order[b][0];
                        }
                    }

                    putPlayerInBattingOrder(this, team, null, nextBatter, null, order, b, positionsFilled, battingOrderMembers, nextBatter.StartingPosition);

                    team.partialslots[b] = true;
                } else {

                    // no one is even got 2pas for this slot.
                    team.illegallineup = true;
                    team.missingslots.push(b);

                    if (!nextBatter || nextBatter.FullName) {

                    } else if (nextBatter[0]) {
                        team.missingpositions.push(nextBatter[0]);
                    }
                }

            }
        }


        // ************** fill in empty slots with players that have full games..
        if (team.illegallineup == true) {
            var fixOrder = ["CA", "SS", "2B", "3B", "LF", "CF", "RF", "1B", "P", "DH"];

            for (let i = team.missingslots.length - 1; i >= 0; i--) {
                if (order[team.missingslots[i]] && order[team.missingslots[i]].PlayerId) {
                    team.missingslots.splice(i, 1);
                }
            }
            if (team.missingslots == 0) {
                team.illegallineup = false;
            } else {
                team.missingpositions = [];

                for (let i = 0; i < team.missingslots.length; i++) {
                    for (let pos = 0; pos < fixOrder.length; pos++) {
                        var pf = positionsFilled[positionsIndex.indexOf(fixOrder[pos])];
                        if ((team.missingpositions.indexOf(fixOrder[pos]) == -1) && pf == false && ((useDH && fixOrder[pos] != "P") || (!useDH && fixOrder[pos] != "DH"))) {
                            team.missingpositions[i] = fixOrder[pos];
                            break;
                        }
                    }
                }

                var foundsub = false;
                for (let i = 0; i < fixOrder.length; i++) {
                    var nextPosition = fixOrder[i];
                    if (team.missingpositions.indexOf(nextPosition) >= 0) {

                        // find someone to play this spot who has at least 3 PA if possible.
                        // look for an exact match! based on the owner's depth chart first!
                        var pi = getPlayerFromDepth(roster, battingOrderMembers, nextPosition, "match", 3);
                        if (pi) {

                            foundsub = true;
                            var orderslot = team.missingslots[team.missingpositions.indexOf(nextPosition)];
                            useStats = getBestStats(roster[pi.position].Players[pi.depth], 3, null); // get a minimum of 3 PAs
                            if( !useStats ) {
                                useStats = getBestMinorStats(roster[pi.position].Players[pi.depth], 3, null); // get a minimum of 3 PAs   
                            }
                            if (useStats.Ratio < 1) {
                                roster[pi.position].Players[pi.depth].FirstFullOffensiveGame = null // useStats.GameDay;
                                roster[pi.position].Players[pi.depth].useStats = [useStats];
                            } else {
                                roster[pi.position].Players[pi.depth].FirstFullOffensiveGame = useStats.GameDay;
                            }

                            putPlayerInBattingOrder(this, team, roster, pi.position, pi.depth, order, orderslot,
                                positionsFilled, battingOrderMembers, nextPosition);


                            // order[orderslot].FirstFullOffensiveGame = useStats.GameDay
                        } else {

                            // find someone who CAN play this position with 3+ PA
                            pi = getPlayerFromDepth(roster, battingOrderMembers, nextPosition, "can play", 1);
                            if (pi) {
                                foundsub = true;
                                var orderslot = team.missingslots[team.missingpositions.indexOf(nextPosition)];
                                useStats = getBestStats(roster[pi.position].Players[pi.depth], 3, null); // get a minimum of 3 PAs
                                if( !useStats ) {
                                    useStats = getBestMinorStats(roster[pi.position].Players[pi.depth], 3, null); // get a minimum of 3 PAs   
                                }
                                if (useStats.Ratio < 1) {
                                    roster[pi.position].Players[pi.depth].FirstFullOffensiveGame = null // useStats.GameDay;
                                    roster[pi.position].Players[pi.depth].useStats = [useStats];
                                } else {
                                    roster[pi.position].Players[pi.depth].FirstFullOffensiveGame = useStats.GameDay;
                                }
                                putPlayerInBattingOrder(this, team, roster, pi.position, pi.depth, order, orderslot,
                                    positionsFilled, battingOrderMembers, nextPosition);


                                // order[orderslot].FirstFullOffensiveGame = useStats.GameDay
                            } else {

                                // now the hard part, build players out of 1 and 2 PA games.
                                pi = getPlayerFromDepth(roster, battingOrderMembers, nextPosition, "no pitchers", 0);
                                if (pi) {
                                    foundsub = true;
                                    putPlayerInBattingOrder(this, team, roster, pi.position, pi.depth, order,
                                        team.missingslots[team.missingpositions.indexOf(nextPosition)], positionsFilled, battingOrderMembers, nextPosition);
                                } else {
                                    // no subs left.. put in a 0 stats players
                                    pi = getPlayerFromDepth(roster, battingOrderMembers, nextPosition, "no pitchers", 0);
                                    if (pi) {
                                        foundsub = true;
                                        putPlayerInBattingOrder(this, team, roster, pi.position, pi.depth, order,
                                            team.missingslots[team.missingpositions.indexOf(nextPosition)], positionsFilled, battingOrderMembers, nextPosition);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        /*
                    for (var i = 0; i < players.length; i++) {
        
                        // (((((((((((( TODO: REPLACE WITH ACTUAL USER LINEUP ORDER ))))))))))))
        
        
                        // ***** for each player who's had at least 3PA, check for their first available day
                        // ***** NOTE: DH is dealt with LATER
                        p = players[i];
                        if (p.FirstFullOffensiveGame != "") {
        
                            // players here have at least 3 PA
                            // see which positions are this player's primary and if it's an empty slot in the lineup
                            var startingPosition = false;
                            for (x = 0; x < p.Primary.length; x++) {
        
                                if (p.Primary[x] != "P" && positionsFilled[positionsIndex.indexOf(p.Primary[x])] === false) {
                                    startingPosition = p.Primary[x];
                                }
                            }
        
                            var gameDate = p.FirstFullOffensiveGame
                            var stats = p.eStats[gameDate].Fielding;
                            var batstats = p.eStats[gameDate].Batting;
                            var brstats = p.eStats[gameDate].Baserunning;
                            insertOuts(batstats);
                            this.calculateDefensiveStats(stats, startingPosition);
                        //    this.calculateBases(brstats);
                            this.calculateBattingLW(batstats);
                            this.calculateFieldingLW(stats);
        
                            // add to starting lineup if a primary position player
                            if (startingPosition) {
        
        
                                // **************************************
                                //
                                // add in stats that are calculated
                                //
                                // **************************************
        
        
                                var position = startingPosition;
        
                                // ok.. see where this position is in the batting order
                                var orderIndex = getBattingOrderIndex(userOrder, position);
        
                                if (orderIndex !== false) {
                                    batters.battingOrder[orderIndex] = new player(p, stats, batstats, brstats, i, gameDate,calendarDate,  position);
                                    battingOrderMembers.push(p.MlbId);
                                    positionsFilled[positionsIndex.indexOf(startingPosition)] = i;
                                } else {
                                    // drop him on the bench!
                                    batters.bench.push(new player(p, stats, batstats, i, gameDate, calendarDate, position));
                                }
        
                            } else {
        
                                // has a full game available
                                // not a starting batter.. but played somewhere;
                                // just on the bench (could be DH Pitcher though)
                                // default position is pinch hitter
        
                                var position = "PH";
                                if (stats) {
                                    // then played in the field (including pitchers)
                                    if (p.Primary.length > 0) {
                                        position = p.Primary[0];
                                    } else if (p.Secondary.length > 0) {
                                        position = p.Secondary[0];
                                    } else if (p.Tertiary.length > 0) {
                                        position = p.Tertiary[0];
                                    }
                                    benchMembers.push(p.MlbId);
                                } else {
                                    // didn't field, only batted... assume PH for now
                                    stats = players[i].Batting;
                                }
                                batters.bench.push(new player(p, stats, batstats, brstats, i, gameDate, calendarDate, position));
                            }
                        } else {
                            // doesn't have a full offensive game.. drop him on the bench.
                            if (p.eStats) {
                                for (g = 0; g < p.gamesList.length; g++) {
                                    if (p.eStats[p.gamesList[g]].Batting && p.eStats[p.gamesList[g]].Batting.PA > 0) {
                                        var gameDate = p.gamesList[g];
                                        var stats = p.eStats[gameDate].Fielding;
                                        var batstats = p.eStats[gameDate].Batting;
                                        var brstats = p.eStats[gameDate].Baserunning;
                                        insertOuts(batstats);
                                        this.calculateDefensiveStats(stats);
                                    //    this.calculateBases(brstats);
                                        this.calculateBattingLW(batstats);
                                        this.calculateFieldingLW(stats);
                                        batters.bench.push(new player(p, stats, batstats, brstats, i, p.gamesList[g], calendarDate, "PH"));
                                        break;
                                    }
                                }
                            }
                        }
                    }
        
                    // ************************************
                    // 
                    // step 2: fill in position players
                    //
                    // ***********************************
        
                    // skip the pitcher and DH
                    for (var pi = 2; pi <= 9; pi++) {
        
                        if (positionsFilled[pi] === false) {
                            var foundsub = false;
                            var maybesub = false;
        
                            // then look for someone who can play this position
                            for (let i = 0; i < players.length; i++) {
        
                                p = players[i];
        
                                // don't bother with players without stats
                                // or that are already filling a position
                                if (p.eStats && positionsFilled.indexOf(i) == -1) {
        
                                    for (g = 0; g < p.gamesList.length; g++) {
                                        var gameDate = p.gamesList[g];
                                        var stats = p.eStats[gameDate].Fielding;
                                        var batstats = p.eStats[gameDate].Batting;
                                        var brstats = p.eStats[gameDate].Baserunning;
        
                                        if (stats && batstats && batstats.PA > 2) {
        
                                            var nextPosition = positionsIndex[pi]
                                            if (p.Primary.indexOf(nextPosition) >= 0) {
                                                foundsub = true;
                                            } else if (p.Secondary.indexOf(nextPosition) >= 0) {
                                                foundsub = true;
                                            }
        
                                            if (foundsub) {
        
                                                var orderIndex = getBattingOrderIndex(userOrder, nextPosition);
                                                // batters.battingOrder.push(new player(p, stats, batstats, i, av, position));
                                                if (orderIndex !== false) {
                                                    batters.battingOrder[orderIndex] = new player(p, stats, batstats, brstats, i, gameDate, calendarDate, nextPosition);
                                                    battingOrderMembers.push(stats.MLBId);
                                                    positionsFilled[pi] = i;
                                                }
                                                break;
                                            }
                                        }
                                    }
                                    if (foundsub) {
                                        break;
                                    }
                                }
                            }
                        }
                    }
        
                    // step 2a: desperately fill in position players
                    // skip the pitcher and DH
                    for (var pi = 2; pi <= 9; pi++) {
        
                        if (positionsFilled[pi] === false) {
                            var foundsub = false;
                            var maybesub = false;
        
                            // then look for someone who can play this position
                            for (let i = 0; i < players.length; i++) {
        
                                p = players[i];
        
                                // don't bother with players without stats
                                // or that are already filling a position
                                if (p.eStats && positionsFilled.indexOf(i) == -1) {
        
                                    for (g = 0; g < p.gamesList.length; g++) {
                                        var gameDate = p.gamesList[g];
                                        var stats = p.eStats[gameDate].Fielding;
                                        var batstats = p.eStats[gameDate].Batting;
                                        var brstats = p.eStats[gameDate].Baserunning;
        
        
                                        if (stats && batstats && batstats.PA > 0) {
        
                                            var nextPosition = positionsIndex[pi]
                                            if (p.Primary.indexOf(nextPosition) >= 0) {
                                                foundsub = true;
                                            } else if (p.Secondary.indexOf(nextPosition) >= 0) {
                                                foundsub = true;
                                            } else if (p.Tertiary.indexOf(nextPosition) >= 0) {
                                                foundsub = true;
                                            } else if( p.Primary.indexOf("P") == -1 ) {
                                                foundsub = true;
                                            }
        
                                            if (foundsub) {
        
                                                var orderIndex = getBattingOrderIndex(userOrder, nextPosition);
                                                // batters.battingOrder.push(new player(stats, batstats, i, av, position));
                                                if (orderIndex !== false) {
                                                    batters.battingOrder[orderIndex] = new player(p, stats, batstats, brstats, i, gameDate, calendarDate, nextPosition);
                                                    battingOrderMembers.push(stats.MLBId);
                                                    positionsFilled[pi] = i;
                                                }
                                                break;
                                            }
                                        }
                                    }
                                    if (foundsub) {
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    // ************************************
                    //
                    // step 3: if dh Game, add in the DH.. otherwise, add in the pitcher to the batting order
                    // ***************************************************
                    if (useDH) {
                        // simply look for the batter with the most PA's
                        if (positionsFilled[0] === false) {
                            // then don't have a DH already
        
                            var mostIndex = 0;
                            var mostPA = 0;
                            var mostId = 0;
                            var mostGame = null;
        
        
                            for (let i = 0; i < players.length; i++) {
        
                                p = players[i];
        
                                // don't bother with players without stats
                                // or that are already filling a position
                                if (p.eStats && positionsFilled.indexOf(i) == -1) {
        
                                    for (g = 0; g < p.gamesList.length; g++) {
                                        var gameDate = p.gamesList[g];
                                        var batstats = p.eStats[gameDate].Batting;
        
                                        if (batstats && batstats.PA > 2) {
        
                                            if (batstats.PA > mostPA || (batstats.PA == mostPA && p.Primary.indexOf("DH") >= 0)) {
                                                // this player has more PAs or has the same and is a primary DH
                                                mostIndex = benchMembers.indexOf(p.MlbId);
                                                mostId = i;
                                                mostPA = batstats.PA;
                                                mostGame = gameDate;
                                                break;
                                            }
                                        }
                                    }
                                }
                            }
        
                            // have the DH player - roster[mostIndex]... move into batting order
                            var orderIndex = getBattingOrderIndex(userOrder, "DH");
                            if (mostIndex > -1 && batters.bench[mostIndex]) {
                                batters.bench[mostIndex].Pos = "DH";
        
        
                                batters.battingOrder[orderIndex] = batters.bench[mostIndex];
                                positionsFilled[0] = mostIndex;
                                batters.battingOrder[batters.battingOrder.length - 1].position = "DH";
                                batters.bench.splice(mostIndex, 1);
                            } else {
                                for( bo=0; bo<9; bo++) {
                                    if( !batters.battingOrder[bo]) {
                                        batters.battingOrder[bo] = new player(players[mostId], mostId, calendarDate, "DH");
                                        break;
                                    }
                                }
                               
                            }
                        }
        
                    } else {
                        // drop the pitcher into the #9 slot
                        var starter = team.pitchingStaff.starter;
         
                        var pitcherIndex = team.pitchingStaff.starter.RosterIndex[0];
                        var pitcherGameDate = team.pitchingStaff.starter.RosterIndex[1];
         
        
                        var stats = team.pitchingStaff.starter.fStats;
        
                        var batstats = null;
                        if( players[pitcherIndex].eStats[pitcherGameDate].Batting)
                            batstats = players[pitcherIndex].eStats[pitcherGameDate].Batting;
                        var brstats = null; 
                        if( players[pitcherIndex].eStats[pitcherGameDate].Baserunning )
                            brstats = players[pitcherIndex].eStats[pitcherGameDate].Baserunning;
        
                        var position = "P";
                        var orderIndex = getBattingOrderIndex(userOrder, position);
                        // batters.battingOrder.push(new player(stats, batstats, i, av, position));
                        if (orderIndex !== false) {
                            batters.battingOrder[orderIndex] = new player(players[pitcherIndex], stats, batstats, brstats, pitcherIndex, pitcherGameDate, calendarDate, position);
                            battingOrderMembers.push(stats.MLBId);
                            positionsFilled[1] = pitcherIndex;
                        }
        
                    }
        
                    // 0-0-0-0-0-0-0-0-0-0-0-0-0-0-0
                    //
                    // TODO:
                    // 0-0-0-0-0-0-0-0-0-0-0-0-0-0-0
                    // ************************************
                    //
                    // step 5: complete the lineup
                    // ***************************************************
        
                    if (batters.battingOrder.length < 9) {
                        // oops, not enough players!  ASSUME pitchers are set by now
                        for (var bi = 2; bi < 10; bi++) {
                            if (positionsFilled[bi] === false) {
                                findSub(bi, positionsIndex[bi], batters.bench);
                            }
                        }
                    }
        
                    // ************* insure there are at least 3 PA for each of the 9 batting slots
                    // add in substitutions (position players who play defense and bat... often 2 PA players)
                    // then, add in pinch hitters (non-pitchers who bat, but don't play in the field)
                    var paPerSlot = [];
        
        
                    for (var b = 0; b < batters.battingOrder.length; b++) {
                        if (batters.battingOrder[b]) {
                            var nextBatterPA = batters.battingOrder[b].PA;
                            paPerSlot[b] = nextBatterPA;
                        }
                    }
        
                    // 0-0-0-0-0-0-0-0-0-0-0-0-0-0-0
                    //
                    // TODO:
                    // 0-0-0-0-0-0-0-0-0-0-0-0-0-0-0
                    // ************************************
                    //
                    // step 4: create the batting order itself from the starters
                    // ***************************************************
        
                    // (((((((((((((((((((((( TODO ))))))))))))))))))))))
                    // order the pinch hitters from most to least and best to worst.
                    // NOTE: DO NOT INCLUDE PITCHERS! Well, maybe after they pitch.
        
                    // (((((((((((((((((((((( TODO ))))))))))))))))))))))
                    // and what if there are NO pinch hitters? Do something!
        */

        team.batters = batters;
    }

}