"use strict";
// **********************************************************
//
// File: set-lineup.js 
// RFB Simulator: functions for setting the batting order, fielding positions, and pinch hitters
// By: Eddie Dombrower and Adam Feldman
//
// **********************************************************



// *****************************************************
// fake batting orders for testing
// *****************************************************


    // **********************************************************************************
    //
    // SETLINEUP() HELPER FUNCTIONS
    //
    // **********************************************************************************


// adds outs to batting stats based on AB-H
function insertOuts( batstats) {
    if (batstats ) {
        batstats.OUTS = batstats.AB - batstats.H;
    }
}

function calculateBattingLW(stats) {
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
        stats.LW = lw;
    }

}

function calculateFieldingLW(stats) {
    lw = 0;

    if (typeof (stats) == "undefined" || !stats) {
        lw = 0;
    } else {
        /*
                   "LW" : {
                    "name": "linear weight",
                    "value" : [ 0.125161, 0.085843, -0.508],
                    "columns" : ["PO", "A", "E"],
                */
        // shortcut...
        alw = algorithms.Fielding.LW;
        columns = alw.columns;
        value = alw.value;
        lw = 0;
        for (var c = 0; c < columns.length; c++) {

            // raw stats
            var s = stats[columns[c]];
            lw += s * value[c];
        }
        stats.FieldLW = lw;
    }
}


function calculateDefensiveStats(fstats) {

    // (((((((((((((( TODO: MAKE REAL:  calculate Zone, Frame, Block, cERA ))))))))))))))
    // will come from YTD stats
  //  var fstats = stats.Fielding && stats.Fielding.daily.length > 0 ? stats.Fielding.daily[0] : null;

                  // shortcut...
    var  alw = algorithms.Fielding;
    if (fstats) {

        // catchers are treated differently.
         if (fstats.Pos == 2) {
            var fiAlg = alw.LWCatcher;
            /*
            Block =(CBlockingRuns)/(INN/9)
            Frame =(CFramingRuns)/(INN/9)
            cERA (ERA LW)   =((4.35-cERA)*(10))*(INN/1260)
            */

            // ((((((((((((((((( TODO: PULL FROM THIS PLAYER'S STATS )))))))))))))))))


            var cBlockingRuns = fStats.YTDcBlockingRuns;
            var cFramingRuns = fStats.YTDcFramingRuns;
            var cEra = fStats.Cera;
            var innings = 1;
            var avgCatcherInnings = alw.avgCatcherInnings.value;

            var innPerGame = innings/9;

            fstats.Block = cBlockingRuns / innPerGame;
            fstats.Frame = cFramingRuns / innPerGame;

            // cERA
            // from Tory   CERA =((4.48-cERA)*(10))*(INN/1260)
            var avgERA = algorithms.Pitching.ERA.value;
            var totalInn = algorithms.Fielding.avgCatcherInnings.value;

            var value = fstats.Cera;
            if (value != "") {
                value = ((avgERA - value) * 10) * (fstats.INN / totalInn);
            }
            fstats.cERA = value;

            // catchers don't get the zone value
            fstats.Zone = 0;

        } else {
            // all other fielders
            var zone = 0;

            // ((((((((((((((((( TODO: NEED THIS STAT IN THE DB )))))))))))))))))
            var OutsOutOfZone = fstats.OutsOutOfZone;
            var zvals;

            // (((((((((((((((((( TODO: allow real values to stand ))))))))))))))))))
            var r = utils.getRandom(0, 10);
            if (r < 5) {
               r = 0;
            } else {
                if( r<8) {
                    r = 1;
                } else {
                    r = 2;
                }
            }
            r = 0;
            OutsOutOfZone = r;

            var MissedBalls = fstats.MissedBallsInZone;
            r = utils.getRandom(0, 10);
            if (r < 5) {
               r = 0;
            } else {
                if( r<8) {
                    r = 1;
                } else {
                    r = 2;
                }
            }
            r = 0;
            MissedBalls = r;

            if( OutsOutOfZone > 0 ) {
                // then rate based on these zvals
                zvals = algorithms.Fielding.OutOfZone;               
            } else {
                // no outs out of zone.. look just at misses
                zvals = algorithms.Fielding.NoOutOfZone;
            }
            var value1 = zvals.value[fstats.Pos-1][0];
            var value2 = zvals.value[fstats.Pos-1][1];
            zone = (value1 * OutsOutOfZone) + (value2 * MissedBalls);

            // replace old zone with new zone
            fstats.Zone = zone;

            // fielders don't get these stats
            fstats.Block = "";
            fstats.Frame = "";
            fstats.cERA = "";

        }

    }
}


function calculateBases(bstats ) {

     var bases = 0;

    if (!bstats) {

    } else {

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
// From Tony 3/26/18, the bases formula should be for stealing 2nd, is a .3, 
//stealing 3rd is a .395, 
//stealing home is a .585, 
//going 1st to 3rd on a single is a .075, 
going 1st to home on a double is .09875, 
going 2nd to home on a single is a .19375, 
a bases taken is a .1225, 
caught stealing of 2nd is a -.6, 
caught stealing of 3rd is a-.79, 
caught stealing of home is a -1.77, 
pickoff is a -.76, 
and an out on base is a -.96.
*/

        // shortcut...
        alw = algorithms.Baserunning.Bases;
        var columns = alw.columns;
        var value = alw.value;
        for (var c = 0; c < columns.length; c++) {

            // raw stats
            var s = bstats[columns[c]];
            if( typeof(s) == "undefined")
                s = 0;
            bases += s * value[c];
        }
        bstats.Bases = bases;
    }
}

function findSub( forPositionIndex, forPosition, bench ) {
    // look through bench for best possible player

    // assume first player is best player
    var bestPlayer = 0;
    var bestPA = bench[0].PA;
    var bestPos = bench[0].Pos;

    // get the array of positions
     var canPlay = algorithms.Fielding.AdjustPosition.value;

    for( var bi=1; bi<bench.length; bi++ ) {
        var benchplayer = bench[bi];
        if( benchplayer.Pos == forPosition ) {
            // then this bench player plays this position

            if( bestPos == forPosition ) {
                // and so does the best player!  who has more at bats?
                if( benchplayer.PA > bestPA) {
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
            if( bestPos != forPosition ) {
                // ok.. current best player doesn't play the desired position
                var pi = utils.getPositionNumber( benchplayer.Pos ); 
                var adjust = canPlay[pi - 1] ? canPlay[pi-1][forPositionIndex] : "-";
                if( adjust != "CP") {
                    // then he can play
                } else {
                    // he cannot play this position
                    var a = adjust;
                }

            }
        }
    }

}

function getBattingOrderIndex(order, position ) {
    for( i=0; i<order.length; i++ ) {
        if( order[i][0] == position)
            return i;
    }
    return false;
}

class player {
    constructor(fstats, bstats, rosterIndex, subIndex, position) {

        this.name = fstats.FullName;
        if( fstats.MLBId )
            this.MLBId = fstats.MLBId;
        if( fstats.MlbId ) {
            this.MLBId = fstats.MlbId;
            stats.MLBId = fstats.MlbId;
        }

        this.RosterIndex = [rosterIndex, subIndex];

        this.Pos = position;
        this.Pos2 = "";
        this.Pos3 = "";
        this.orderType = "leadoff";
        this.hasPlayed = false;
        this.PA = 0;
        if(  bstats) {
            this.PA = bstats.PA;
        }



        this.baserunning = {};

        // 0-0-0-0-0-0-0-0-0-0-0-0-0-0-0
        //
        // TODO: get most recent unused game(s)
        // TODO: calculate "bonuses" here - RBI and RUN bonuses
        // TODO: position similarities
        // TODO: see if need MLE.. If so, calculate it
        // TODO: include primary, secondary, tertiary positions as part of similarties?
        // TODO: Lineup "groupings" e.g. #1 hitter scores if #2,3,4 have rbi potential events
        // TODO: tag players when they enter the game

        // 0-0-0-0-0-0-0-0-0-0-0-0-0-0-0
    }
}



    // **********************************************************************************
    //
    // SETLINEUP() MAIN CODE
    //
    // **********************************************************************************
function setLineup(game, team, roster) {
    var gr = getGameRequest();
    var useDH = game.dhGame; // true if dh game.

    var batters = {
        battingOrder: new Array(9),  // start with 9, but may be more!
        bench: []
    }

     // ************************************
    //
    // step 1: fill the lineups with defensive players
    //         NOTE: if DH league, do NOT put in pitcher
    // ***************************************************

    var positionsFilled = [false, false, false, false,false,false,false,false,false, false, false];
    var battingOrderMembers = new Array();
    var benchMembers = new Array();

    var userOrder = roster.battingOrder;

    if (gr.useFakeData) {


        var players = roster;

        for (var i = 0; i < players.length; i++) {

            var availabledays = players[i].Fielding ? players[i].Fielding.daily.length : 0;

            // (((((((((((( TODO: REPLACE WITH ACTUAL USER LINEUP ORDER ))))))))))))
            var depth = players[i].MlbDepth;

            // ***** for each player who's fielded, check for their best day
            // ***** NOTE: DH is dealt with LATER
            for (var av = 0; av < availabledays; av++) {
                
                var stats = players[i].Fielding && players[i].Fielding.daily.length > 0 ? players[i].Fielding.daily[av] : false;
                var batstats = players[i].Batting && players[i].Batting.daily.length > 0 ? players[i].Batting.daily[av] : false;
                var brstats =  players[i].Baserunning && players[i].Baserunning.daily.length > 0 ? players[i].Baserunning.daily[av] : false; 
                
            // **************************************
            //
            // add in stats that are calculated
            //
            // **************************************
            insertOuts(batstats);
            calculateDefensiveStats(stats);
            calculateBases(brstats);
            calculateBattingLW( batstats );
            calculateFieldingLW( stats );



            // ((((((((((((((((((((((( TODO )))))))))))))))))))))))
            // consider availability.. must be able to start to be a starter
            //                         must be available to play to be included at all

            // add to starting lineup if a starting player (GS==1) and either a 9man lineup or not a pitcher
            if (stats && stats.GS == "1" && positionsFilled[stats.Pos] == false && (stats.Pos != "1") && batstats && batstats.PA >=3 ) {
                var position = utils.positions[stats.Pos];

                // ok.. see where this position is in the batting order
                var orderIndex = getBattingOrderIndex(userOrder, position);
                // batters.battingOrder.push(new player(stats, batstats, i, av, position));
                if (orderIndex !== false) {
                    batters.battingOrder[orderIndex] = new player(stats, batstats, i, av, position);
                    battingOrderMembers.push(stats.MLBId);
                    positionsFilled[stats.Pos] = i;
                }

                orderIndex++;
                break; // no further processing of this player.. skip the rest of the av's
            } else {

                // not a starting batter.. but played somewhere;
                // just on the bench (could be DH Pitcher though)
                // default position is pinch hitter

                    var position = "PH";
                    if (stats) {
                        // then played in the field (including pitchers)
                        var positionIndex = stats.Pos;
                        if (positionIndex == "") {
                            positionIndex = 0;
                        }
                        position = utils.positions[positionIndex];
                        benchMembers.push(stats.MLBId);
                    } else {
                        // didn't field, only batted... assume PH for now
                        stats = players[i].Batting;
                    }
                    batters.bench.push(new player(stats, batstats, i, av, position));
            }
        }
    }

    // ************************************
    // 
    // step 2: fill in position players
    //
    // ***********************************
    for( var p=2; p<=9; p++) {
        if( positionsFilled[p] == false ) {
            var foundsub = false;
            var maybesub = false;

            // then look for someone who can play this position
            for( i=0; i<players.length; i++ ) {

                var availabledays = players[i].Fielding ? players[i].Fielding.daily.length : 0;

                // don't bother with players already filling a position
                if( positionsFilled.indexOf(i) != -1 )
                    availabledays = 0;
                
                for( av=0; av<availabledays; av++ ) {
                    var stats = players[i].Fielding && players[i].Fielding.daily.length > 0 ? players[i].Fielding.daily[av] : false;
                    var batstats = players[i].Batting && players[i].Batting.daily.length > 0 ? players[i].Batting.daily[av] : false;
                    
                    if( stats && batstats && batstats.PA > 2 ) {

                        var pos = stats.Pos;
                        if( p==2 ) {
                            // then looking for a catcher: use 3B and 1B only for now
                            if( pos==3 || pos==5) {
                                foundsub = true;                     
                            }
                        } 
                        if( p==3 ) {
                            // then looking for a 1b
                            if( pos==5 || pos==2 || pos > 6) {
                                foundsub = true;
                            }
                        }
                        if( p==4 || p==6 ) {
                            // then looking for a 2B or SS
                            if( pos==4 || pos==6 ) {
                                foundsub = true;
                            }
                        }
                        if( p==5 ) {
                            // then looking for a 3B
                            if( pos==6 || pos==4  ) {
                                foundsub = true;
                            }
                        }
                        if( p>6 ) {
                            // then looking for a 3B
                            if( pos>6 || pos==4  ) {
                                foundsub = true;
                            }
                        }                       
                        if( foundsub ) {
                                 var brstats =  players[i].Baserunning && players[i].Baserunning.daily.length > 0 ? players[i].Baserunning.daily[av] : false; 
        
                            var position = utils.positions[p];
                            var orderIndex = getBattingOrderIndex(userOrder, position);
                            // batters.battingOrder.push(new player(stats, batstats, i, av, position));
                            if (orderIndex !== false) {
                                batters.battingOrder[orderIndex] = new player(stats, batstats, i, av, position);
                                battingOrderMembers.push(stats.MLBId);
                                positionsFilled[stats.Pos] = i;
                            }
                            break;   
                        }
                     }
                }   
                if( foundsub ) {
                    break;
                }                 
            }
        }
    }

   // step 2a: desperately fill in position players
   for( var p=2; p<=9; p++) {
    if( positionsFilled[p] == false ) {
        var foundsub = false;
        var maybesub = false;

        // then look for ANYONE who can play this position
        for( i=0; i<players.length; i++ ) {

            var availabledays = players[i].Fielding ? players[i].Fielding.daily.length : 0;

            // don't bother with players already filling a position
            if( positionsFilled.indexOf(i) != -1 )
                availabledays = 0;
            
            for( av=0; av<availabledays; av++ ) {
                var stats = players[i].Fielding && players[i].Fielding.daily.length > 0 ? players[i].Fielding.daily[av] : false;
                var batstats = players[i].Batting && players[i].Batting.daily.length > 0 ? players[i].Batting.daily[av] : false;
                
                if( stats && batstats && batstats.PA > 1 ) {

                    foundsub = true;
   
                    if( foundsub ) {
                        var brstats =  players[i].Baserunning && players[i].Baserunning.daily.length > 0 ? players[i].Baserunning.daily[av] : false; 
    
                        var position = utils.positions[p];
                        var orderIndex = getBattingOrderIndex(userOrder, position);
                        if (orderIndex !== false) {
                            batters.battingOrder[orderIndex] = new player(stats, batstats, i, av, position);
                            battingOrderMembers.push(stats.MLBId);
                            positionsFilled[stats.Pos] = i;
                        }
                        break;   
                    }
                 }
            }   
            if( foundsub ) {
                break;
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
            var mostIndex = 0;
            var mostPA = 0;
            var mostId = 0;


            for (i = 0; i < players.length; i++) {
                var bstats = players[i].Batting;

                // see if this player already in lineup
                if (bstats && battingOrderMembers.indexOf(bstats.MLBId) == -1) {
                    if (bstats.PA > mostPA) {
                        // this player has more PAs
                        mostIndex = benchMembers.indexOf(bstats.MLBId);
                        mostId = i;
                        mostPA = bstats.PA;
                    }
                }

            }

            // have the DH player - roster[mostIndex]... move into batting order
            if (mostIndex > -1) {
                batters.battingOrder.push(batters.bench[mostIndex]);
                positionsFilled[0] = mostIndex;
                batters.battingOrder[batters.battingOrder.length - 1].position = "DH";
                batters.bench.splice(mostIndex, 1);
            } else {
                batters.battingOrder.push(new player(players[mostId], mostId, "DH"));
            }

        } else {
            // drop the pitcher into the #9 slot
            pitcherIndex = team.pitchingStaff.starter [0];
            pitcherAV    = team.pitchingStaff.starter[1];

            var stats = players[pitcherIndex].Fielding && players[pitcherIndex].Fielding.daily.length > 0 ? players[pitcherIndex].Fielding.daily[pitcherAV] : false;
            var batstats = players[pitcherIndex].Batting && players[pitcherIndex].Batting.daily.length > 0 ? players[pitcherIndex].Batting.daily[pitcherAV] : false;
 
            var position = utils.positions[1];
            var orderIndex = getBattingOrderIndex(userOrder, position);
            // batters.battingOrder.push(new player(stats, batstats, i, av, position));
            if (orderIndex !== false) {
                batters.battingOrder[orderIndex] = new player(stats, batstats, pitcherIndex, pitcherAV, position);
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
            for( var bi=2; bi<10; bi++) {
                if( positionsFilled[bi] == null ) {
                    findSub( bi, utils.positions[bi], batters.bench );
                }
            }
        }

        // ************* insure there are 4-5 PA for each of the 9 batting slots
        // add in substitutions (position players who play defense and bat... often 2 PA players)
        // then, add in pinch hitters (non-pitchers who bat, but don't play in the field)
        var paPerSlot = [];

 
        for( var b=0; b<batters.battingOrder.length; b++ ) {
            if (batters.battingOrder[b]) {
                var nextBatter = batters.battingOrder[b];
                var nextBatterStats = players[nextBatter.RosterIndex[0]];
                var nextBatterPA = nextBatterStats.Batting.daily[nextBatter.RosterIndex[1]].PA;

                paPerSlot[b] = nextBatterPA;
            }
        }

        for( b=0; b<9; b++ ) {
            if( paPerSlot[b] < 3 ) {
                var nextBatter = batters.battingOrder[b];
                
                if( paPerSlot[b] < 2 && nextBatter.Pos != "P") {
                    // then get a sub player (note, never sub for pitchers, always PH for pitchers)

                    // can only get subs who aren't already in the lineup/
                    // battingOrderMembers is array of MLBId's of players in the lineup somewhere
                }
            }
        }

        // finally, fill in with PH
        for( b=0; b<9; b++ ) {
            if( paPerSlot[b] <= 3  ) {
                var nextBatter = batters.battingOrder[b];
                
                if( paPerSlot[b] == 2 || nextBatter.Pos == "P") {
                    // then get a PH


                    //(((((((( recommend that pitchers with 3 PA but less than 8 innings get a PH ))))))))

                    // can only get PH who aren't already in the lineup
                }
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

    } else {

        // get from real data

    }

    team.batters = batters;

}
