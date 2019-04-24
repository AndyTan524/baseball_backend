// **********************************************************
//
// File: playGame.js 
// RFB Simulator: functions for playing one game and creating the box score
// By: Eddie Dombrower and Adam Feldman
//
// **********************************************************
var algorithms = require("../simulator/algorithms.json");
var boxscoreTemplate = require("../simulator/boxscore-template.json");
var leverageIndex =  require("../helpers/leverageIndex");
var getPitchingStats = require("../helpers/getPitchingStats");


// **********************************************************
//
// score summary object
//
// **********************************************************
class scoreSummary {
    constructor() {
        this.Visit = {
            name: "",
            abbreviation: "",
            R: 0,
            H: 0,
            E: 0,
            Off: 0.00,
            BR: 0,
            Pit: 0.00,
            Def: 0.00
        };
        this.Home = {
            name: "",
            abbreviation: "",
            R: 0,
            H: 0,
            E: 0,
            Off: 0.00,
            BR: 0,
            Pit: 0.00,
            Def: 0.00
        };
        this.Decision =  {Win: "", WinId : "", Loss: "", LossId:"", Save:"", SaveId:"", Blown: "", BlownId:"", Hold: [], HoldId: []};
    }
}

// **********************************************************
//
// score details object
//
// **********************************************************
class scoreDetails {
    constructor() {
        this.summary = new scoreSummary();
        this.Visit = {
            Batters: [],
            BattingTotals: {},
            Pitchers: [],
            PitchingTotals: {}
        };
        this.Home = {
            Batters: [],
            BattingTotals: {},
            Pitchers: [],
            PitchingTotals: {}
        };
    }
}

// ************************************************************
//
//     GENERIC HELPER FUNCTIONS
//
// ************************************************************

function isNumber(n) {
    return !isNaN(parseFloat(n)) && isFinite(n);
}

function formatNumber(n, decimalplaces, round) {
    if (!isNumber(n))
        return (n);

    // is a number...
    var value = "--";
    if (typeof (n) != "undefined") {
        if (decimalplaces == 0) {
            if (round)
                n += 0.5;
            value = Math.floor(n);
        } else {
            value = n.toFixed(decimalplaces);
        }
    }
    return value;
}

function numberNaN(n) {
    if (isNumber(n))
        return n;
    return (0);
}

// ************************************************************
//
//     BOXSCORE HELPER FUNCTIONS
//
// ************************************************************
function getIPFromOuts( outs ) {

    var newIP = outs / 3;
    var newIPInteger = Math.floor(newIP);
    newIP = (newIP - newIPInteger == 0) ? newIPInteger : ((newIP - newIPInteger < 0.5) ? newIPInteger + 0.1 : newIPInteger + 0.2);
    return (newIP );

}

function insertPitcherIntoBoxscore(nextPitcher, index, pitchingTotals, score, team, isDH) {
    bt = boxscoreTemplate.JSON.Pitchers;

    // get the actual stats from the pitcher object
    var playerStats = nextPitcher.pStats;

    // note, we only use one day for pitchers
    var dayToUse = 0;

    // create new player object
    var boxPlayer = {};

    // fill in the object
    for (c = 0; c < bt.length; c++) {
        var column = bt[c];
        var type = column.charAt(0);
        column = column.substring(2);
        var nextStat = 0;

        if (type == "L") {
            // find pitcher in the lineup, then grab their stats. in this case, just the type of pitcher
            if (column == "Decision") {
                nextStat = "-";
            } else {
                nextStat = nextPitcher.Pos;
                if (index == 0) {
                    nextStat = "SP";
                }
            }
        }

        if (type == "P") {
            if (playerStats && playerStats.Pitching) {
                nextStat = playerStats.Pitching[column];
            }
        }

        if (type == "F") {
            if (nextPitcher.fStats) {
                nextStat = nextPitcher.fStats[column];
            }
        }

        if (type == "S") {
            nextStat = nextPitcher[column];
        }
        if (type == "C") {
            if( column == "IP") {
                nextStat = getIPFromOuts( nextPitcher.OUT);
                // calculate innings pitched from outs...
            } else {
            nextStat = nextPitcher[column];
            }
        }

        boxPlayer[column] = nextStat;
        if (isNumber(nextStat)) {
            if( isDH || index != 0 || column != "E") {
                pitchingTotals[column] += parseFloat(nextStat);
            } else {
                var e = parseFloat(nextStat);
            }
        }
    }

    // put the players game stats into the box score
    if (team == 0) {
        score.Visit.Pitchers[index] = boxPlayer;
    } else {
        score.Home.Pitchers[index] = boxPlayer
    }
}

// ************************************************************
//
//     CALCULATE GAME RESULTS HELPER FUNCTIONS: PITCHING
//
// ************************************************************
function selectPitchers(myPitchers, pitchingStaff, status, relieftype, targetOutsToGet, startingOut, side, diff) {

    // relief types: long, mid, final
    // status: winning, leading, tied, trailing, losing
    // side: home, visit
    /*
    If SP goes less than six innings, find a RP with at least two innings to pitch.  (THIS IS THE RELIEFTYPE == LONG)
        Do not allow this pitcher to take the total team innings past eight. START WITH 4TH RANKED PITCHER.

    ED INTERPETATION OF BELOW:  After 5th inning starts, but the 7th innings isn't over:
        If total innings is not a whole number, find a RP that enables the total outs to be a whole inning.

    If the team is winning after five innings, bring in the fourth highest ranked pitcher. (THIS IS THE RELIEFTYPE == MID ONLY) 
        If the fourth ranked pitcher is not available, starting with the 5th ranked pitcher, look for the next lowest ranked pitcher who is available.
    
    If the team is winning after six innings, bring in the third highest ranked pitcher.  (THIS IS ALSO RELIEFTYPE == MID)
        If the third ranked pitcher is not available, bring in the lowest ranked pitcher who is available.
    
    If the team is winning after seven innings, bring in the second highest ranked pitcher.  (THIS IS RELIEFTYPE == FINAL)
        If the second ranked pitcher is not available, bring in the lowest ranked pitcher who is available.

    If the team is winning after eight innings, bring in the highest ranked pitcher. 
        (THIS WOULD BE THE CLOSER) 
    
    If the highest ranked pitcher is not available, bring in the lowest ranked pitcher who is available.
        ⇒ example.. in (b) If #3 ranking pitcher does not have 1 IP, combo him with a reliever below him in the rankings to get to at least 1 IP.
    */

    // note: pitchingStaff.pitchersUsed is in order from closer [0], to highest rank [1], going down the list... 
    // note: null array elements may exist when a pitcher has been removed from the list.
       // for now, skip the starter who's already in the the pitchersUsed array
       var allPitchers = pitchingStaff.relievers;
   
    var partialInningOuts = (3 * Math.ceil(startingOut / 3)) - startingOut;
    if (startingOut == 0)  // in case no outs in first inning!
        partialInningOuts = 3;

    var minOutsTarget = 6;
    var targetRanking = 4;
    var reverseSearch = false;

    var LI = 0;
    var levArray = null;
        if( startingOut > (8*3)) {
            // 9th inning
            levArray = leverageIndex.inn9;

        } else if (startingOut > (7*3)) {
            // 8th inning
            levArray = leverageIndex.inn8;

        } else if (startingOut > (6*3)) {
            // 7th inning
            levArray = leverageIndex.inn7;

        } else if (startingOut > (5*3)) {
            // 6th inning
            levArray = leverageIndex.inn6;
        }

        if( levArray ) {

            for( var l=0; l<levArray.length; l++ ) {
                if( levArray[l].Diff == diff) {
                    LI = levArray[l].LI;
                    break;
                }
            }
        }


    if (status == "winning" || status == "losing") {
        targetRanking = allPitchers.length >= 12 ? 12: allPitchers.length;

        minOutsTarget = partialInningOuts == 0 ? 6 : (partialInningOuts + 3);
        reverseSearch = true; // start with the highest number (e.g. RP11) and get better and better pitchers

    } else if (relieftype == "long") {
        minOutsTarget = 6;
        minOutsTarget = partialInningOuts == 0 ? 6 : (partialInningOuts + 3);
        targetRanking = allPitchers.length >= 10 ? 10: allPitchers.length;
        reverseSearch = true; // start with the highest number (e.g. RP11) and get better and better pitchers

    } else if (relieftype == "mid") {
        minOutsTarget = partialInningOuts == 0 ? 3 : partialInningOuts;
        targetRanking = 4;
        if (status == "leading")
            reverseSearch = true;   // get better and better pitchers

    } else if (relieftype == "short") {
        targetRanking = 3;
        if (status == "leading")
            reverseSearch = true;   // get better and better pitchers

        // if at the beginning of an inning, try for 3 outs, otherwise finish the inning
        minOutsTarget = partialInningOuts == 0 ? 3 : partialInningOuts;


        // if after 7 innings completed
        if (startingOut > (7 * 3)) {
            targetRanking = 2;
        }

    } else if (relieftype == "final") {
        if (status == "leading" || (side == "home" && status == "tied")) {
            targetRanking = 1; // note 1 is closer
        } else {
            if (status == "tied" || status == "trailing") {
                targetRanking = 2;
            } else {
                targetRanking = 3;
            }
        }

    }

    // ok, here we go.. first try to find the right pitcher!
    // generally looking for a target ranking and, if he's not available or need more innings, to go the next lowest ranking (which is the next highest number)

    var pitchersUsedIds = [];
    for (pu = 1; pu < pitchingStaff.pitchersUsed.length; pu++) {
        pitchersUsedIds.push(pitchingStaff.pitchersUsed[pu].PlayerId);
    }
  
    var reliefList = [];
    var reliefOutsUsed = [];
    var reliefOuts = 0;
    var matchRanking = true;
    var originalTargetRanking = targetRanking;


    for (loop = 0; loop < 5; loop++) {
        // loop 0 - normal rules... 
        // loop 1 - reverse order starting at the original target ranking
        // loop 2 - original order starting at the original target ranking, but taking all pitchers
        // loop 3 - reverse order starting at the original target ranking, but taking all pitchers
        // loop 4 - first come, first served.
        if (loop == 1) {
            reverseSearch = !reverseSearch
            targetRanking = originalTargetRanking;
        }
        if (loop == 2 || loop == 3) {
            status = "short";
            reverseSearch = !reverseSearch
            targetRanking = originalTargetRanking;
            minOutsTarget = 0;
        }
        if (loop == 4) {
            matchRanking = false;

            // if winning or tied..
            reverseSearch = false;
            targetRanking = 1;
            minOutsTarget = 0;
            if (status == "trailing" || status == "losing") {
                reverseSearch = true;
                targetRanking = 20;
            }
        }

        for (p = 1; p <= allPitchers.length; p++) {

            // revlievers start at 0, not 1...
            var nextIndex = p;
            if (reverseSearch) {
                nextIndex = (allPitchers.length) - (p-1);
            }

            var nextPitcher = allPitchers[nextIndex-1];
            var pitchersLeft = allPitchers.length - pitchersUsedIds.length;
            // as we can loop through this a few times.
            // must be a pitcher who hasn't been used, and either there are no relievers yet or grabbing first "correct" pitcher
            if (nextIndex == targetRanking && (!nextPitcher || (pitchersUsedIds.indexOf(nextPitcher.PlayerId) >= 0))) {
                // then can't get pitcher at this target.. move to the next correct pitching target...
                if (reverseSearch !== false) {
                    // searching from the back... 
                    if (targetRanking == allPitchers.length) {
                        // already targeted the worst pitcher... 
                        // get the next worst pitcher...
                        if (nextIndex == targetRanking)
                            targetRanking--;
                    } else {
                        targetRanking--;
                    }
                } else {
                    targetRanking++;
                }
            }

            if (nextPitcher && ((pitchersUsedIds.indexOf(nextPitcher.PlayerId) == -1))) {
                // it's a pitcher, he matches the target ranking, and he's not already used!

                if (nextIndex == targetRanking && (nextIndex != 1 || relieftype == "final" || matchRanking === false)) {

                    // don't look at closer unless or until it's the final innings or we're desparate.
                    // found a pitcher at the right ranking.. see if he has enough outs to qualify here!
                    var outs = nextPitcher.OUT;
                    var totalOuts = nextPitcher.eStats.TotalPitching ? nextPitcher.eStats.TotalPitching.OutsAvailable : 0;
                    var totalOutsMinors = nextPitcher.eStats.TotalPitchingMinors ? nextPitcher.eStats.TotalPitchingMinors.OutsAvailable : 0;     
                    
                    // get the most available outs...
                    outs = totalOuts >= totalOutsMinors ? totalOuts : totalOutsMinors;

                    outs = totalOuts >= totalOutsMinors ? totalOuts : totalOutsMinors;

                    if (outs > 0 || matchRanking === false) {

                        // has some outs...or we're using him anyway.
                      //  if (outs >= targetOutsToGet) {
                          if( (totalOuts >= targetOutsToGet) || (totalOutsMinors >= targetOutsToGet)) {

                            // then  at least enough outs, HAVE OUR MAN TO FINISH THIS REQUEST
                            // get his outs into his pitcher data
                            //outs = totalOuts >= targetOutsToGet ? totalOuts : totalOutsMinors;
                            // just get the outs we need
                            outs = targetOutsToGet;

                            getPitchingStats.getBestPitchingStatsForReliever(nextPitcher, targetOutsToGet, null);

                            reliefList.push(nextIndex-1);
                            pitchersUsedIds.push(nextPitcher.PlayerId);

                            var usedouts = (outs - targetOutsToGet) == 0 ? outs : targetOutsToGet;
                            reliefOutsUsed.push(usedouts);
                            reliefOuts += usedouts;
                            targetOutsToGet -= usedouts; // should make this 0
                            break;

                        } else {

                            // NOT ENOUGH OUTS! I.e. outs < targetOutsToGet

                            // grab his data, but will need to FIND THE NEXT PITCHER TO HELP OUT....
                            if (relieftype == "final") {
                                // then see if we have to use the closer, etc.

                                // a bit tricky, but here it is...
                                // looking for the closer ( targetRanking of 1)
                                if( targetRanking != 1 ) {

                                    // ************ end of game, not the closer....

                                    // then just proceed like any other matched ranking and go to worse pitchers...
                                    if (reliefList.length == 0 && (outs >= minOutsTarget || outs >= partialInningOuts)) {
                                        // first reliever found for very short relief that gets us to the end of the inning
                                       
                                        getPitchingStats.getBestPitchingStatsForReliever(nextPitcher, outs, null);

                                        reliefList.push(nextIndex-1);
                                        pitchersUsedIds.push(nextPitcher.PlayerId);
    
                                        reliefOutsUsed.push(outs);
                                        reliefOuts += outs;
                                        targetOutsToGet -= outs; // KNOW this is > 0
                                        if (targetOutsToGet < minOutsTarget)
                                            minOutsTarget = targetOutsToGet;
                                     // return now with however many outs were created...just this one pitcher...
                                    return ({ newOuts: reliefOuts, outsUsed: reliefOutsUsed, newPitchers: reliefList, runsAllowed: 0 });
                                    }
   
                                } else {
                                    // 
                                    // *************************   try to get the closer in!  
                                    //
                                    // see how many outs the setup man needs... 
                                    // get the next best pitcher!
                                    for (p1 = 1; p1 < allPitchers.length; p1++) {

                                        var p1Outs = 0;
                                        var p1OutsMinors = 0;
                                        if( reliefList.indexOf(p1) == -1 ) {
                                            p1Outs = allPitchers[p1].eStats.TotalPitching ? allPitchers[p1].eStats.TotalPitching.OutsAvailable : 0;
                                            p1OutsMinors = allPitchers[p1].eStats.TotalPitchingMinors ? allPitchers[p1].eStats.TotalPitchingMinors.OutsAvailable : 0;     
                        
                                        } 
                                        if( ( p1Outs + outs < targetOutsToGet) && (p1OutsMinors + outs >= targetOutsToGet ) ) {
                                            p1Outs = p1OutsMinors;
                                        } 

                                        // finish this loop trying to find a pitcher with ANY innings at this point.
                                        if (p1Outs >= 1) {
                                            var p1Pitcher = allPitchers[p1];
                                            
                                            if ((p1Outs + outs) >= targetOutsToGet) {

                                                // HAVE OUR NEXT SHORT RELIEVER.. put the p1 pitcher in until the p pitcher can relieve him.
                                                // and return now.
                                                p1Outs = targetOutsToGet - outs;  // this is how many outs we need from mr. p1
                                                
                                                getPitchingStats.getBestPitchingStatsForReliever(p1Pitcher, p1Outs, null);
                                                reliefList.push(p1);
                                                pitchersUsedIds.push(p1Pitcher.PlayerId);
                                                reliefOutsUsed.push(p1Outs);
                                                reliefOuts += p1Outs;
                                                return ({ newOuts: reliefOuts, outsUsed: reliefOutsUsed, newPitchers: reliefList, runsAllowed: 0 });

                                            }
                                        }
                                    }
                                }



                            } else if (relieftype == "mid" || relieftype == "short") {

                                if (reliefList.length == 0 && outs >= minOutsTarget) {
                                    // first reliever found for middle relief that gets us to the end of the inning
                                    getPitchingStats.getBestPitchingStatsForReliever(nextPitcher, outs, null);
                                    reliefList.push(nextIndex-1);
                                    pitchersUsedIds.push(nextPitcher.PlayerId);

                                    reliefOutsUsed.push(outs);
                                    reliefOuts += outs;
                                    targetOutsToGet -= outs; // KNOW this is > 0
                                    if (targetOutsToGet < minOutsTarget)
                                        minOutsTarget = targetOutsToGet;
                                }

                                // STILL HAVE OUTS TO GO... so, regardless, 
                                // find the next appropirate pitcher to complete this assignment
                                if (reverseSearch === true) {
                                    if (targetRanking > 2 || matchRanking == false) {
                                        targetRanking--;        // find the next BEST pitcher to follow this pitcher!
                                    }
                                } else {
                                    // searching from the back... 
                                    if (targetRanking == allPitchers.length) {
                                        // already targeted the worst pitcher... 
                                        // get the next worst pitcher...
                                        if (nextIndex == targetRanking)
                                            targetRanking--;
                                    } else {
                                        targetRanking++;
                                    }
                                }

                            } else if (nextIndex != 1 && (relieftype == "long")) {

                                // **********  LONG RELIEF
                                if (reliefList.length > 0 || outs >= minOutsTarget) {
                                    // first reliever found for long relief that gets us to the minimum
                                    getPitchingStats.getBestPitchingStatsForReliever(nextPitcher, outs, null);
                                    reliefList.push(nextIndex-1);
                                    pitchersUsedIds.push(nextPitcher.PlayerId);

                                    reliefOutsUsed.push(outs);
                                    reliefOuts += outs;
                                    targetOutsToGet -= outs; // KNOW this is > 0
                                    if (targetOutsToGet < minOutsTarget)
                                        minOutsTarget = targetOutsToGet;


                                    // STILL HAVE OUTS TO GO... so, regardless, 
                                    // find the next appropirate pitcher to complete this assignment
                                    if (reverseSearch === true) {
                                        if (targetRanking > 2 || matchRanking == false) {
                                            targetRanking--;        // find the next BEST pitcher to follow this pitcher!
                                        }
                                    } else {
                                        // searching from the back... 
                                        if (targetRanking == allPitchers.length) {
                                            // already targeted the worst pitcher... 
                                            // get the next worst pitcher...
                                            if (nextIndex == targetRanking)
                                                targetRanking--;
                                        } else {
                                            targetRanking++;
                                        }
                                    }
                                } else {


                                    // a long relief stretch...targeted pitcher doesn't have what it takes to come right in...
                                    // ok.. the tricky bit if we have a need for a longer  first reliever!
                                    // ingnore reverse and look for next worst pitchers...
                                    // start at the very back...
                                    for (var p1 = allPitchers.length-1; p1>=0;p1--) {

                                        // finish this loop trying to find a pitcher with ANY innings at this point.
                                        var p1Outs = 0;
                                        var p1OutsMinors = 0;
                                        if( reliefList.indexOf(p1) == -1 ) {
                                            p1Outs = allPitchers[p1].eStats.TotalPitching ? allPitchers[p1].eStats.TotalPitching.OutsAvailable : 0;
                                            p1OutsMinors = allPitchers[p1].eStats.TotalPitchingMinors ? allPitchers[p1].eStats.TotalPitchingMinors.OutsAvailable : 0;     
                        
                                        }
                                        if ((p1+1) != nextIndex && allPitchers[p1] && (p1Outs >= minOutsTarget || p1OutsMinors >=minOutsTarget || matchRanking === false)) {
                                            var p1Pitcher = allPitchers[p1];
                                            //var p1Outs = allPitchers[p1].OUT;
                                            if (p1Outs < minOutsTarget) {
                                                if (p1OutsMinors >= minOutsTarget) {
                                                    p1Outs = p1OutsMinors;
                                                }
                                            }

                                            if ((p1Outs + outs) >= targetOutsToGet) {

                                                // HAVE OUR SOLUTION.. put the p1 pitcher in until the p pitcher can relieve him.
                                                if (p1Outs >= targetOutsToGet) {
                                                    // just let him finish the required work.
                                                    p1Outs = targetOutsToGet;  // this is how many outs we need from mr. p1
                                                    getPitchingStats.getBestPitchingStatsForReliever(p1Pitcher, targetOutsToGet, null);
                                                    reliefList.push(p1);
                                                    pitchersUsedIds.push(p1Pitcher.PlayerId);
                                                    reliefOutsUsed.push(p1Outs);
                                                    reliefOuts += p1Outs;
                                                    outs = 0;   // stops from adding the original reliever
                                                } else {
                                                    p1Outs = targetOutsToGet - outs;  // this is how many outs we need from mr. p1
                                                    getPitchingStats.getBestPitchingStatsForReliever(p1Pitcher, p1Outs, null);
                                                    reliefList.push(p1);
                                                    pitchersUsedIds.push(p1Pitcher.PlayerId);
                                                    reliefOutsUsed.push(p1Outs);
                                                    reliefOuts += p1Outs;
                                                }


                                                targetOutsToGet -= p1Outs;    // this is how many outs are left to get!
                                                if (targetOutsToGet < minOutsTarget)
                                                    minOutsTarget = targetOutsToGet;

                                                // now add in the other, target reliever
                                                if (outs > 0) {

                                                    // see how many outs we'll use...
                                                    var usedouts = (outs - targetOutsToGet) <= 0 ? outs : targetOutsToGet;
                                                    getPitchingStats.getBestPitchingStatsForReliever(nextPitcher, usedouts, null);
                                                    reliefList.push(nextIndex-1);
                                                    pitchersUsedIds.push(nextPitcher.PlayerId);

                                                    reliefOutsUsed.push(usedouts);
                                                    reliefOuts += usedouts;
                                                    if (targetOutsToGet < minOutsTarget)
                                                        minOutsTarget = targetOutsToGet;

                                                    // STILL HAVE OUTS TO GO... so, regardless, 
                                                    // find the next appropirate pitcher to complete this assignment
                                                    if (reverseSearch === true) {
                                                        if (targetRanking > 2 || matchRanking == false) {
                                                            targetRanking--;        // find the next BEST pitcher to follow this pitcher!
                                                        }
                                                    } else {
                                                        // searching from the back... 
                                                        if (targetRanking == allPitchers.length) {
                                                            // already targeted the worst pitcher... 
                                                            // get the next worst pitcher...
                                                            if (nextIndex == targetRanking)
                                                                targetRanking--;
                                                        } else {
                                                            targetRanking++;
                                                        }
                                                    }
                                                }
                                                outs = 0;

                                            } else {

                                                // NEXT PITCHER + TARGET CAN'T GET US THERE... 
                                                if ((p1Outs + outs) >= minOutsTarget) {

                                                    // then we have some solution.. put the p1 pitcher in until the p pitcher can relieve him.
                                                    // take all the outs we need from mr. p1
                                                    getPitchingStats.getBestPitchingStatsForReliever(p1Pitcher, p1Outs, null);
                                                    reliefList.push(p1);
                                                    pitchersUsedIds.push(p1Pitcher.PlayerId);

                                                    reliefOutsUsed.push(p1Outs);
                                                    reliefOuts += p1Outs;


                                                    targetOutsToGet -= p1Outs;    // this is how many outs are left to get!
                                                    if (targetOutsToGet < minOutsTarget)
                                                        minOutsTarget = targetOutsToGet;

                                                    // now add in the other, target reliever if we haven't already
                                                    if (outs > 0) {
                                                        var usedouts = (outs - targetOutsToGet) <= 0 ? outs : targetOutsToGet;

                                                        getPitchingStats.getBestPitchingStatsForReliever(nextPitcher, usedouts, null);
                                                        reliefList.push(nextIndex-1);
                                                        pitchersUsedIds.push(nextPitcher.PlayerId);

                                                        reliefOutsUsed.push(usedouts);
                                                        reliefOuts += usedouts;
                                                        targetOutsToGet -= usedouts;
                                                        if (targetOutsToGet < minOutsTarget)
                                                            minOutsTarget = targetOutsToGet;

                                                        // STILL HAVE OUTS TO GO... so, regardless, 
                                                        // find the next appropirate pitcher to complete this assignment
                                                        if (reverseSearch === true) {
                                                            if (targetRanking > 2 || matchRanking == false) {
                                                                targetRanking--;        // find the next BEST pitcher to follow this pitcher!
                                                            }
                                                        } else {
                                                            // searching from the back... 
                                                            if (targetRanking == allPitchers.length) {
                                                                // already targeted the worst pitcher... 
                                                                // get the next worst pitcher...
                                                                if (nextIndex == targetRanking)
                                                                    targetRanking--;
                                                            } else {
                                                                targetRanking++;
                                                            }
                                                        }
                                                    }
                                                    outs = 0;


                                                }
                                            }
                                        }
                                        if (targetOutsToGet <= 0)
                                            break;
                                    }
                                }

                                // LOOKED AT ALL TYPE OF RELIEF... FALL THROUGH HERE...


                                if (targetOutsToGet > 0 && nextIndex != 1 && (relieftype != "long" || outs >= partialInningOuts || reliefList.length > 0 || p == (allPitchers.length - 1))) {
                                    // FALL THROUGH AND GRAB OUTS TO GET TO END OF THIS INNING...  
                                    // OR, IF IT'S LONG RELIEF AND SOMEONE'S COME IN ALREADY, ADD SOME MORE OUTS.

                                    // don't allow the first long reliever to be a less than the full amount
                                    // in this case, either we already have a long reliever or he has enough outs or it's not a long reliever
                                    if (reliefList.indexOf(nextIndex - 1) == -1) {
                                        // only do this if this pitcher is NOT already in the relief list...
                                        var usedouts = (outs - targetOutsToGet) <= 0 ? outs : targetOutsToGet;
                                        
                                        getPitchingStats.getBestPitchingStatsForReliever(nextPitcher, usedouts, null);
                                        reliefList.push(nextIndex - 1);
                                        pitchersUsedIds.push(nextPitcher.PlayerId);


                                        reliefOutsUsed.push(usedouts);
                                        reliefOuts += usedouts;
                                    }
                                    if (targetOutsToGet < minOutsTarget)
                                        minOutsTarget = targetOutsToGet;
                                    partialInningOuts = 3;

                                    // STILL HAVE OUTS TO GO... so, regardless, 
                                    // find the next appropirate pitcher to complete this assignment
                                    if (reverseSearch === true) {
                                        if (targetRanking > 2 || matchRanking == false) {
                                            targetRanking--;        // find the next BEST pitcher to follow this pitcher!
                                        }
                                    } else {
                                        // searching from the back... 
                                        if (targetRanking == allPitchers.length) {
                                            // already targeted the worst pitcher... 
                                            // get the next worst pitcher...
                                            if (nextIndex == targetRanking)
                                                targetRanking--;
                                        } else {
                                            targetRanking++;
                                        }
                                    }

                                    // let the loop continue!

                                }
                            }

                        }

                    }

                }

            }
            if (targetOutsToGet <= 0)
                break;
            // otherwise, let the loop continue!
        }
        // see if we can break out the the outer loop
        if (targetOutsToGet <= 0)
            break;
    }

    return ({ newOuts: reliefOuts, outsUsed: reliefOutsUsed, newPitchers: reliefList, runsAllowed: 0, LI: LI });
}

function setPitchersAndEstimateScore(score, homeTeam, visitTeam, isDHGame) {

    // set up scoring
    var homeTotalAllowed = 0;
    var homeScores = new Array(27);
    homeScores.fill(0);


    var visitTotalAllowed = 0;
    var visitScores = new Array(27);
    visitScores.fill(0);

    // 1) set the starters in the game
    var homePitchers = [homeTeam.pitchingStaff.starter];
    var homeOutsSoFar = homeTeam.pitchingStaff.starter ? homeTeam.pitchingStaff.starter.OUT : 0;
    if( homeOutsSoFar > 27)
        homeOutsSoFar = 27;
    var visitPitchers = [visitTeam.pitchingStaff.starter];
    var visitOutsSoFar = visitTeam.pitchingStaff.starter ? visitTeam.pitchingStaff.starter.OUT : 0;
    if( visitOutsSoFar > 27)
        visitOutsSoFar = 27;

    // home team pitches first.. visiting scores first...
    var decimalRuns = 0;
    if( homePitchers[0] && homePitchers[0].pStats )
        decimalRuns = calculateRunsAllowed(score.Visit.BattingTotals, score.Home.BattingTotals, homePitchers[0].pStats.Pitching, score.summary.Visit.Off, score.summary.Home.Def);
    homeTotalAllowed += Math.floor(decimalRuns + 0.5);

    // visit team pitches second.. home scores second...    
    var decimalRuns = 0;
    if( visitPitchers[0] && visitPitchers[0].pStats)
        decimalRuns = calculateRunsAllowed(score.Home.BattingTotals, score.Home.BattingTotals, visitPitchers[0].pStats.Pitching, score.summary.Home.Off, score.summary.Visit.Def);
    visitTotalAllowed += Math.floor(decimalRuns + 0.5);


    // go through each out....
    for (var gameOuts = 0; gameOuts <= 27; gameOuts++) {

        if (gameOuts >= homeOutsSoFar || gameOuts >= visitOutsSoFar) {

            // then need to catch up with at least one team...
            if (homeOutsSoFar < 27 && (homeOutsSoFar <= visitOutsSoFar || ((visitOutsSoFar < 25) && (homeOutsSoFar>24) && homeOutsSoFar<27))) {

                    // home team has pitched for less outs than visitor...
                    // OR home team is in the 9th innning and hasn't finished yet.
                var status;
                var diff = visitTotalAllowed - homeTotalAllowed; // home score - visit score
                if (diff == 0) {
                    status = "tied";
                } else if (diff > 3) {
                    status = "winning";
                } else if (diff > 0) {
                    status = "leading";
                } else if (diff > -4) {
                    status = "trailing";
                } else if (diff <= -4) {
                    status = "losing";
                }

                // home team pitcher didn't last longer than visiting pitcher
                var result;
                if (homeOutsSoFar < 5 * 3) {
                    // lasted less than 5 full innings
                    // get pitcher(s) to get through end of 5th innning.. long relievers if possible
                    outsToGet = (6 * 3) - homeOutsSoFar;
                    result = selectPitchers(homePitchers, homeTeam.pitchingStaff, status, "long", outsToGet, homeOutsSoFar, "home", diff);

                } else if (homeOutsSoFar < (6 * 3)) {
                    // lasted less than 6 full innings
                    // get pitcher(s) to get through end of 7th/middle of 8th innning.. mid relievers if possible
                    outsToGet = (7 * 3) - homeOutsSoFar;
                    if( status=="losing" || status=="winning") 
                        outsToGet += 3; // get through the 8th.
                    result = selectPitchers(homePitchers, homeTeam.pitchingStaff, status, "mid", outsToGet, homeOutsSoFar, "home", diff);

                } else if (homeOutsSoFar < (3 * 7)) {
                    // between 5 and 7  full innings
                    // get pitchers to get us through end of 7th inning.. mid and short relievers
                    // get complete innings required to catch up to other pitcher.. turn into outs.
                    var visitEndOfInning = Math.ceil(visitOutsSoFar / 3) * 3;
                    var homeEndOfInning = Math.ceil(homeOutsSoFar / 3) * 3;
                    var outsToGet = visitEndOfInning - homeEndOfInning;
                    if (outsToGet <= 0) // pitched same number of innings
                        outsToGet = 3;


                    result = selectPitchers(homePitchers, homeTeam.pitchingStaff, status, "short", outsToGet, homeOutsSoFar, "home", diff);

                } else {
                    // at least 7 complete innings and we're into the 8th... 
                    // try to bring in the closer if a) there is a slim lead and b) he has enough outs to make it to the end of the game
                    // note if it's the 9th and the game is tied, the home closer will enter like he has a lead.
                    // starter (or pitching staff) made it into the 8th inning
                    // get pitchers to finish game, possibly closer
                    outsToGet = (9 * 3) - homeOutsSoFar;
                    result = selectPitchers(homePitchers, homeTeam.pitchingStaff, status, "final", outsToGet, homeOutsSoFar, "home", diff);

                }

                // put in the new results
                if (result) {
                    homeOutsSoFar += result.newOuts;
                    visitTotalAllowed += result.runsAllowed;
                    for (let np = 0; np < result.newPitchers.length; np++) {
                        var reliever = homeTeam.pitchingStaff.relievers[result.newPitchers[np]];
                        reliever.AvailableOuts = reliever.OUT;
                        reliever.OUT = result.outsUsed[np];
                        reliever.LI = result.LI;
                        reliever.pStats.Pitching.LI = result.LI;

                        calculatePitchingLW( reliever, homeTeam.extra.Coordinators );
                        homeTeam.pitchingStaff.pitchersUsed.push(reliever);

                        // home team pitches first.. visiting scores first...
                        var decimalRuns = calculateRunsAllowed(score.Visit.BattingTotals, score.Home.BattingTotals, reliever.pStats.Pitching , score.summary.Visit.Off, score.summary.Home.Def, reliever.OUT);
                        homeTotalAllowed += Math.floor(decimalRuns + 0.5);

                    }
                }

            } else {

                if( visitOutsSoFar < 27 || (homeOutsSoFar==27 && visitOutsSoFar>=24)) {
                // visiting pitcher has less outs than home team.
                // see if game is really over

                var status = "tied";
                var diff = homeTotalAllowed - visitTotalAllowed; // visit score - home score
                if( diff== 0 ) {
                    status = "tied";
                } else if (diff > 4) {
                    status = "winning";
                } else if (diff > 0) {
                    status = "leading";
                } else if (diff > -4) {
                    status = "trailing";
                } else if (diff <= -4) {
                    status = "losing";
                }

                if(homeOutsSoFar >= 27 && visitOutsSoFar >= 24) {
                    // then completed 8 1/2 innings
                    if( diff < 0 ) {
                        // then we're in the bottom of the 9th, but the home team is winning, so game over!
                        if( visitOutsSoFar > 24) {
                            // then need to reduce pitchers.. from the back of the rotation...
                            var pitchers = visitTeam.pitchingStaff.pitchersUsed;
                            for( var pu=pitchers.length-1; pu>=0; pu--) {
                                var puOuts = pitchers[pu].OUT;
                                if( visitOutsSoFar-puOuts >= 24 ) {
                                    // then found a pitcher to remove
                                    pitchers.splice(pu,1);
                                    visitOutsSoFar -= puOuts;
                                } else {
                                    // this pitcher has more than enough outs.. back him up to the 24th out
                                    var outsToSubtract = visitOutsSoFar - 24;
                                    visitOutsSoFar = 24;
                                    pitchers[pu].OUT = puOuts - outsToSubtract;
                                    pitchers[pu].AvailableOuts = puOuts;
                                    if (pu == 0) {
                                        // need to remove and reinsert starter!
                                        score.Visit.PitchingTotals = {};
                                        // fill in the pitching total object with 0 stats
                                        for (c = 0; c < bt.length; c++) {

                                            var column = bt[c].substring(2);
                                            score.Visit.PitchingTotals[column] = 0;

                                        }

                                        var coaches = visitTeam.extra.Coordinators;
                                        calculatePitchingLW(pitchers[pu], coaches);                         
                                        insertPitcherIntoBoxscore(pitchers[pu], 0, score.Visit.PitchingTotals, score, 0, isDHGame);

                                    }

                                }
                                if( visitOutsSoFar == 24) 
                                    break;
                            }
                        }
                        
                        
                        break;
                    }
                }

                // home team pitcher didn't last longer than visiting pitcher
                var result;
                if (visitOutsSoFar < 5 * 3) {
                    // lasted less than 5 full innings
                    // get pitcher(s) to get through end of 6th innning.. long relievers if possible
                    outsToGet = (6 * 3) - visitOutsSoFar;
                    if( status=="losing" || status=="winning") 
                        outsToGet += 2; // get through the mid 7th.
                    result = selectPitchers(visitPitchers, visitTeam.pitchingStaff, status, "long", outsToGet, visitOutsSoFar, "visit", diff);

                } else if (visitOutsSoFar < (6 * 3)) {
                    // lasted less than 6 full innings
                    // get pitcher(s) to get through end of 7th innning.. long relievers if possible
                    outsToGet = (7 * 3) - visitOutsSoFar;
                    if( status=="losing" || status=="winning") 
                    outsToGet += 3; // get through the 8th.
                    result = selectPitchers(visitPitchers, visitTeam.pitchingStaff, status, "mid", outsToGet, visitOutsSoFar, "visit", diff);

                } else if (visitOutsSoFar < (3 * 7)) {
                    // between 5 and 7  full innings
                    // get pitchers to get us through end of 7th inning.. mid and short relievers
                    // get complete innings required to catch up to other pitcher.. turn into outs.
                    var visitEndOfInning = Math.ceil(visitOutsSoFar / 3) * 3;
                    var homeEndOfInning = Math.ceil(homeOutsSoFar / 3) * 3;
                    var outsToGet = homeEndOfInning - visitEndOfInning;
                    if (outsToGet <= 0) // pitched same number of innings
                        outsToGet = 3;


                    result = selectPitchers(visitPitchers, visitTeam.pitchingStaff, status, "short", outsToGet, visitOutsSoFar, "visit", diff);

                } else {
                    // starter made it into the 8th inning
                    // get pitchers to finish game, possibly closer
                    outsToGet = (9 * 3) - visitOutsSoFar;

                    // if we're losing in the 8th inning, only go to the end of the 8th
                    if( visitOutsSoFar < (8*3) & diff <= 0 ) {
                        outsToGet = (8*3) - visitOutsSoFar;
                    }
                    result = selectPitchers(visitPitchers, visitTeam.pitchingStaff, status, "final", outsToGet, visitOutsSoFar, "visit", diff);

                }
                // put in the new results
                if (result) {
                    visitOutsSoFar += result.newOuts;
                    homeTotalAllowed += result.runsAllowed;
                    for (let np = 0; np < result.newPitchers.length; np++) {
                        var reliever = visitTeam.pitchingStaff.relievers[result.newPitchers[np]];
                        reliever.AvailableOuts = reliever.OUT;
                        reliever.OUT = result.outsUsed[np];
                        reliever.LI = result.LI;
                        reliever.pStats.Pitching.LI = result.LI;

                 
                        calculatePitchingLW( reliever, visitTeam.extra.Coordinators );
                        visitTeam.pitchingStaff.pitchersUsed.push( reliever );


                        // and calculate updated score...
                        // visit team pitches now.. home scores now...    
                        var decimalRuns = calculateRunsAllowed(score.Home.BattingTotals, score.Home.BattingTotals, reliever.pStats.Pitching, score.summary.Home.Off, score.summary.Visit.Def, reliever.OUT);
                        visitTotalAllowed += Math.floor(decimalRuns + 0.5);

                    }
                }
            }
            }

            /* reference...
            // visiting pitchers...
            var runningV = new Array(27);
            runningV.fill(0);
            changesV = [];
            var nextChange = -1;
            var vruns = 0;  // visiting pitchers runs allowed, the home teams runs
            for (var p = 0; p < score.Visit.Pitchers.length; p++) {
 
                // round up
                var decimalRuns = calculateRunsAllowed(score.Home.BattingTotals, score.Home.BattingTotals, score.Visit.Pitchers[p], score.summary.Home.Off, score.summary.Visit.Def);
                vruns += Math.floor(decimalRuns + 0.5);
                nextChange = score.Visit.Pitchers[p].OUT;
                if (nextChange < 0)
                    nextChange = 0;
                runningV[nextChange] += vruns;
                changesV.push(nextChange);
            }
            score.Visit.PitchingTotals.R = vruns;
 
            // home pitchers...
            var runningH = new Array(27);
            runningH.fill(0);
            changesH = [];
            var nextChange = -1;
            var hruns = 0;
            for (var p = 0; p < score.Home.Pitchers.length; p++) {
                var decimalRuns = calculateRunsAllowed(score.Visit.BattingTotals, score.Home.BattingTotals, score.Home.Pitchers[p], score.summary.Visit.Off, score.summary.Home.Def);
                hruns += Math.floor(decimalRuns + 0.5);
                nextChange = score.Home.Pitchers[p].OUT;
                if (nextChange < 0)
                    nextChange = 0;
                runningH[nextChange] += hruns;
                changesH.push(nextChange);
 
            }
            score.Home.PitchingTotals.R = hruns;
        }
        */
        }
    }

}

function testPitching(score, homeTeam, visitTeam, homeRoster, visitRoster) {
    setPitchersAndEstimateScore(score, homeTeam, visitTeam);
}
function calculateTotalDefense(fieldstats, DH, pitchingstats, startingpitcher) {
    var defense = numberNaN(fieldstats.FieldLW) + numberNaN(fieldstats.Zone) + numberNaN(fieldstats.cERA) + numberNaN(fieldstats.Block) + numberNaN(fieldstats.Frame);

    if (DH || startingpitcher) {
        // dh game, so add in the pitchers
        var add = numberNaN(pitchingstats.FieldLW) + numberNaN(pitchingstats.Zone) + numberNaN(pitchingstats.cERA) + numberNaN(pitchingstats.Block) + numberNaN(pitchingstats.Frame);
        defense += add;

        if (!DH) {
            // not DH so SP is part of the defense already.. take him back out.. was added twice when added pitchers
            if (startingpitcher.pStats.Fielding) {
                var sp = (startingpitcher.pStats.Fielding.FieldLW) + numberNaN(startingpitcher.pStats.Fielding.Zone);
                // subtract his LW.
                defense = defense - sp;
            }
        }
    }

  

    
    if (isNumber(defense))
        defense = defense.toFixed(2);

    return defense;
}

function calculateTotalOffense(batstats) {
    var offense = batstats.LW + batstats.ArW + batstats.ArbW + batstats.Bases;

    if (isNumber(offense))
        offense = offense.toFixed(2);

    return offense;
}

function calculateRunsAllowed(offenseStats, defenseStats, pitcher, oppOff, myDef, usedOuts) {
    /*
        from Tory
 
        Runs=IF((Opponent Overall Offense*X)-(Pitcher’s Team Overall Defense*X)-(Pitcher’s LW)+(4.48*X)<0,
              0,
             (Opponent Overall Offense*X)-(Pitcher’s Team Overall Defense*X)-(Pitcher’s LW)+(4.48*X))
 
 
            Where X = Pitcher’s IP/9
 
            translates to:
 
            ipPerGame = pitcher.stats.ip/9;
             avgERA = algorithms.ERA.value;
 
            Runs = if((Opponent Overall Offense * ipPerGame)
                   -(Pitcher’s Team Overall Defense * ipPerGame)
                   -(Pitcher.stats.LW)+( avgERA * ipPerGame)<0,
                   0,
                   else: (Opponent Overall Offense * ipPerGame)-(Pitcher’s Team Overall Defense * ipPerGame)-( pitcher.stats.LW)+(avgERA * ipPerGame))
 
 
        Where X = Pitcher’s IP/9

        x factor = avg ERA * IP/9
    */

    var avgERA = algorithms.Pitching.ERA.value;

    // var ipPerGame = pitcher.IP / 9;
    var ipPerGame = pitcher.OUT / 27;        // easier, more accurate calculation.
    var outsToUse = pitcher.OUT;
    if( usedOuts ) {
       ipPerGame = usedOuts / 27; 
       outsToUse = usedOuts;
    }

    var offense = offenseStats.LW;
    var defense = defenseStats.FieldLW;

    var pitchLW = 0
    if (pitcher.LW)
        pitchLW = pitcher.LW;

    // updated 
    offense = oppOff;
    defense = myDef;

    /* test values.. should result in Runs = 3... and it does
    pitchLW = 1.65;
    defense = 0.82;
    ipPerGame = 0.67;
    offense = 4.05;
    */

    var Runs;
    if (outsToUse == 0) {
        // Offense (no pro rata) minus defense (no pro rata) plus pitchers LW + x-factor (4.65). 

        Runs = (offense - defense - pitchLW + avgERA) / avgERA;

    } else {
        // normal pitcher who's gotten at least 1 out
        var v1 =  (offense * ipPerGame);
        var v2 = -(defense * ipPerGame);
        var v3 = - pitchLW;
        var v4 = avgERA * ipPerGame;

        Runs = (offense * ipPerGame) - (defense * ipPerGame) - (pitchLW) + (avgERA * ipPerGame);
    }

    // finally, if they have a Leverage Index (LI) subtract it here...
    if( pitcher.LI ) {
        Runs = Runs - pitcher.LI;
    }

    if (Runs < 0)
        Runs = 0;

    pitcher.Runs = Math.floor(Runs + 0.5); // Don't round
    pitcher["RawRuns"] = Runs;
    pitcher.IPPG = ipPerGame;

    return (Runs);

}


// ********************************************
//
// adjust and calculate Fielding penalties
/*
First, the code should look for a) the position he played in MLB for the game he's pulling. If he is playing that same position in RSports, then no reduction. 

Then, if a) above doesn't apply, assess whether he's playing his primary position. If so, no reduction.

If not playing primary, check and see if he's playing his secondary or tertiary position. If so, assign reduction based on secondary/tertiary criteria.

If not playing secondary or tertiary, apply penalty.
*/
//
// *********************************************
function adjustFielding(boxStats, rosterStats, playingPosition) {
    var canplay = true;
    var penalty = 0;
    var reduction = 0;

 
    if (typeof (boxStats) == "undefined " || !boxStats) {
        // then he's WAY out of position with no fielding stats!
    } else {

        var alg = algorithms.Fielding.AdjustPosition;
        var reallyPlayed = rosterStats.fStats[0] ? rosterStats.fStats[0].Pos: -1; // where 1=P, 2=CA, etc.


        // ************ first, see if he's playing his primary position
        if (playingPosition == "DH" 
            || rosterStats.Primary.indexOf(playingPosition)  >= 0
            || rosterStats.Primary.indexOf(playingPosition) == (reallyPlayed-1) )
            {
            // then nothing to do!
            return;

        } 
    
        // index of the position he's really playing....
        var playingPosIndex = algorithms.Fielding.AdjustPosition.positions.indexOf(playingPosition) + 1; // note the 0th position is the reference position for the table

        if( playingPosIndex == reallyPlayed) {
            // nothing to do.
            return;
        }

        // see if, based on his primary position, he has no penalty.. if so, done
        if (rosterStats.Primary) {
            for (var i = 0; i < rosterStats.Primary.length; i++) {
                var primeIndex = alg.positions.indexOf(rosterStats.Primary[i]);
                if (primeIndex == -1) {
                    // treat like a catcher
                    primeIndex = 1;
                }
                if (alg.value[primeIndex][playingPosIndex] == 0) {
                    // then can play this position with NO PENALTY.  DONE
                    return;
                }
            }
        }

        // only get here if it's a position that requires a penalty

        // now file through the secondary positions
        var realLetterPosition = algorithms.Fielding.AdjustPosition.positions[reallyPlayed - 1];
        if (rosterStats.Secondary) {
            for (var i = 0; i < rosterStats.Secondary.length; i++) {
                if (rosterStats.Secondary[i] == playingPosition) {
                    reduction = -0.25;
                    break;
                }
            }
        }


        if (penalty == 0 && reduction == 0 && rosterStats.Tertiary) {
            // then not playing a secondary position.. see if tertiary
            // now file through the tertiary positions
            for (var i = 0; i < rosterStats.Tertiary.length; i++) {
                if( rosterStats.Tertiary[i] == playingPosition ) {
                    reduction = -0.50;
                    break;
                }
            }
        }

        if( penalty == 0 && reduction == 0 && rosterStats.Primary ) {
            // then no playing secondary NOR tertiary position.. get the penalty by position
            // look for the lowest penalty based on his starting positions
            for( var prime=0; prime< rosterStats.Primary.length; prime++) {

                var primeIndex = alg.positions.indexOf( rosterStats.Primary[prime] );
            
                if( primeIndex >=0 && alg.value[primeIndex][playingPosIndex] != "CP") {
                    // then can play this position with WITH PENALTY.  
                    if( penalty == 0 || alg.value[primeIndex][playingPosIndex] > penalty) {

                        // then there hasn't been a penalty assessed, or this is a less harmful penalty
                        penalty = alg.value[primeIndex][playingPosIndex];
                    }

                }              
            }

            if( penalty == 0 ) {
                // then must not have a primary position... really penalize this guy
                penalty = -1.75;
            }


        }

        if( reduction != 0 ) {
            rosterStats.fStats[0].FieldLW = rosterStats.fStats[0].FieldLW * (1-Math.abs(reduction));
           // don't need as this is a pointer
           //  boxStats.FieldLW = rosterStats.fStats[0].FieldLW - reduction;
            rosterStats.fStats[0].Zone = rosterStats.fStats[0].Zone * (1- Math.abs(reduction));
            // same: boxStats.Zone = rosterStats.fStats[0].Zone - reduction;
        } else if( penalty != 0) {
            rosterStats.fStats[0].FieldLW = penalty;
            // don't need as this is a pointer
            ///boxStats.FieldLW = penalty;
            rosterStats.fStats[0].Zone = 0;
            // boxStats.Zone = 0;
        }

    }

    return;
}

// *********************************************
//
// calculate pitchers LW based on outs used/outs available
//
//
// *********************************************
function calculatePitchingLW(stats, coaches ) {
    lw = 0;

    if (!stats || !stats.pStats || !stats.pStats.Pitching ) {
        lw = 0;
    } else {

        var outs = stats.pStats.Pitching.OUT;
        var percentStats = 1;
        if( stats.AvailableOuts ) {
            outs = stats.OUT;
            percentStats = stats.OUT / stats.pStats.Pitching.OUT;
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
        lw = 0;
        for (var c = 0; c < columns.length; c++) {

            // raw stats
            var s = stats.pStats.Pitching[columns[c]];
            if( columns[c] == "OUT" )
                s = outs;
            lw += percentStats *s * value[c];
        }

        var useMLE = false;
        if( stats.Level && stats.Level == "Minors")
            useMLE = true;

        if( useMLE) {
            if( lw > 0 )
                lw = lw/4;
            else
                lw = 2* lw;
            var hasPitchingCoordinator = false;
            if( coaches && coaches.Pitching && coaches.Pitching==true) {
                hasPitchingCoordinator = true;
            }
            if( hasPitchingCoordinator )
                lw = lw * 1.15;
        }

    }
    if( stats.IsPitchingOn3DaysRest && stats.IsPitchingOn3DaysRest == true ) {
        var penalty = Math.abs(lw) * 0.25;
        lw = lw - penalty;
    }
    stats.pStats.Pitching["LW"] = lw;
}

// *********************************************
// 
// calculate ArW and ArbW
//
//
// ********************************************

function calculateArW(player, dayToUse, coaches) {

    var ArW = 0;
    var ArbW = 0;

    /*
    (Year to Date Speed Score/100) = Added Runs Weighted (ArW)
 
    A player who hits drives in a run (as determined by above criteria) gets a value as follows for each RBI.
 
    ISO = YTD Slugging % -  YTD Batting Avg.
(Year to Date ISO*3) /(10) = Added Runs Batted In Weighted (ArbW)
 
    */
    var stats = player.bStats;
    var speed = player.YTDSpeed ? player.YTDSpeed : 0;
    var ISO = player.YTDISO ? player.YTDISO : 0;

    if (stats) {

        var battingstats = stats[dayToUse];

        if (battingstats) {
            var hasBaserunningCoordinator = false;
            if( coaches && coaches.Baserunning && coaches.Baserunning==true) {
                hasBaserunningCoordinator = true;
            }   

            var ArWDenominator = 100;
            var ArbWDenominator = 10;
            var useMLE = false;
            if( stats.Level && stats.Level == "Minors")
            useMLE = true;
            
            if( useMLE ) {
                ArWDenominator = 144;
                ArbWDenominator = 14;
            }

            var runs = battingstats.RSRuns;
            var ArW = runs * (speed / ArWDenominator);


            if( useMLE && hasBaserunningCoordinator ) {
                ArW = ArW * 1.15;
            }

            var rbi = battingstats.RSRBI;
            var ArbW = rbi * ((ISO * 3) / ArbWDenominator);



            battingstats.ArW = ArW;
            battingstats.ArbW = ArbW;


        }
    }
}

// *********************************************
//
// GAME PLAYER MAIN FUNCTION:
// playGame() entry
//
// *********************************************
playGame = function (game, visitTeam, visitRoster, homeTeam, homeRoster) {


    // (((((((((((((((((((((((((((((( TODO ))))))))))))))))))))))))))))))
    // create working stats array for each team:
    //          LW, Bonus, Bases_Taken, Outs_on_Base, Field, E, Zone, Block, Frame 

    // ((((((((((((((((((((((((((((((( TODO )))))))))))))))))))))))))))))))
    // create output box score object:
    //  batting_col <- c("POS","GameDate","FirstName","LastName","LW","Run.Bonus","RBI.Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
    //          "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
    //          "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")
    // see blank_home data in R to see what else goes in this object

    // grab team names to save time/space
    var teamNames = [game.visit.name, game.home.name];

    // JSON version of full score
    var score = new scoreDetails();



    // *************************************************
    //
    // calculate offense and defense values
    // and create the box score objects
    //
    // **************************************************

    var bt = boxscoreTemplate.JSON.Batters;

    var battingOrder;
    var roster;
    // create batting total object
    var battingTotals;



    // for each batter that makes an appearance in our game
    // put their stats in the boxScore stats format
    // and add their totals to the totals
    for (var team = 0; team < 2; team++) {
        // visitors first (whem team == 0)

        // ***** point at correct batting order
        battingOrder = visitTeam.batters.battingOrder;
        roster = visitRoster;
        var coaches = visitTeam.extra.Coordinators;

        if (team == 1) {
            // home team pointers
            battingOrder = homeTeam.batters.battingOrder;
            roster = homeRoster;
            coaches = homeTeam.extra.Coordinators;
        }


        // ******* create empty batting totals
        battingTotals = {};

        // fill in the batting total object with 0 stats
        for (c = 0; c < bt.length; c++) {

            var column = bt[c].substring(2);
            battingTotals[column] = 0;

        }

        // ********* for each batter in the batting order (including pinch hitters)
        // ********* create a line in the box score
        for (var batter = 0; batter < battingOrder.length; batter++) {

            if (battingOrder[batter]) {
                // get the index of the actual player in the actual roster
                var indexArray = battingOrder[batter].RosterIndex;

                // get the actual stats from the roster
                var player = battingOrder[batter];
                var playerStats = player.bStats;
                var dayToUse = 0;

                // calcluate the ArW and AbrW for this player
                calculateArW(player, dayToUse, coaches);

                var playerPosition = player.Pos;
                adjustFielding(player.fStats[dayToUse], player, playerPosition);

                // create new player object
                var boxPlayer = {};


                // fill in the object
                for (c = 0; c < bt.length; c++) {
                    var skipthis = false;
                    var addthis = true;
                    var column = bt[c];
                    var type = column.charAt(0);
                    column = column.substring(2);
                    var nextStat = "";

                    if (type == "L") {
                        // find player in the lineup, then grab their stats. in this case, just the position
                        nextStat = playerPosition;
                    }

                    if (type == "F") {
                        if (player.Pos == "DH" || player.Pos == "PH" || player.Pos == "PR") {
                            skipthis = true;
                        } else {
                            if (player.fStats && player.fStats[dayToUse]) {
                                nextStat = player.fStats[dayToUse][column];
                                if (column == "Zone") {
                                    ytdzone = nextStat;
                                }
                            }
                        }
                    }

                    if (type == "B") {
                        if (player.bStats && player.bStats[dayToUse]) {
                            nextStat = player.bStats[dayToUse][column];
                        }
                    }

                    if (type == "R") {
                        if (player.brStats) {
                            nextStat = player.brStats.length > 0 && player.brStats.length > dayToUse ? player.brStats[dayToUse][column] : "";
                        }
                    }

                    if (type == "S") {
                        // nextStat = player[column];
                        nextStat = player[column];
                        addthis = false;
                    }

                    if (skipthis == false) {
                        boxPlayer[column] = nextStat;
                        if (addthis && isNumber(nextStat)) {
                            battingTotals[column] += parseFloat(nextStat);
                        }
                    }
                }

                // put the players game stats into the box score
                if (team == 0) {
                    score.Visit.Batters[batter] = boxPlayer;
                } else {
                    score.Home.Batters[batter] = boxPlayer
                }
            }
        }

        // put the total game stats into the box score
        if (team == 0) {
            score.Visit.BattingTotals = battingTotals;
        } else {
            score.Home.BattingTotals = battingTotals
        }
    }

    // **********************************************
    //
    // calculate the offensive totals
    //
    // **********************************************
    score.summary.Visit.Off = calculateTotalOffense(score.Visit.BattingTotals);;
    score.summary.Home.Off = calculateTotalOffense(score.Home.BattingTotals);;


    // ************************************************
    // 
    // next get pitchers into the box score objects
    //
    // ************************************************
    bt = boxscoreTemplate.JSON.Pitchers;

    var pitchingList;
    roster;
    // create batting total object
    var pitchingTotals;



    // for each pitcher that makes an appearance in our game SO FAR, JUST THE STARTERS!
    // put their stats in the boxScore stats format
    // and add their totals to the totals
    for (var team = 0; team < 2; team++) {
        // visitors first (whem team == 0)

        // ***** point at correct pitching order
        pitchingList = visitTeam.pitchingStaff.pitchersUsed;
        roster = visitRoster;

        if (team == 1) {
            // home team pointers
            pitchingList = homeTeam.pitchingStaff.pitchersUsed;
            roster = homeRoster;
        }


        // ******* create empty pitching totals
        pitchingTotals = {};

        // fill in the pitching total object with 0 stats
        for (c = 0; c < bt.length; c++) {

            var column = bt[c].substring(2);
            pitchingTotals[column] = 0;

        }

        // ********* for each pitcher in the pitching staff
        // ********* create a line in the box score
        for (var pitcher = 0; pitcher < pitchingList.length; pitcher++) {
            var nextPitcher = pitchingList[pitcher];
            insertPitcherIntoBoxscore(nextPitcher, pitcher, pitchingTotals, score, team,game.dhGame);
        }

      

        // put the total game stats into the box score
        // fix up IP based on outs...
        newIP = getIPFromOuts( pitchingTotals.OUT );
        /*
        var newIP = pitchingTotals.OUT / 3;
        var newIPInteger = Math.floor(newIP);
        newIP = (newIP - newIPInteger == 0) ? newIPInteger : ((newIP - newIPInteger < 0.5) ? newIPInteger + 0.1 : newIPInteger + 0.2);
        */
        pitchingTotals.IP = newIP;
        if (team == 0) {
            score.Visit.PitchingTotals = pitchingTotals;
        } else {
            score.Home.PitchingTotals = pitchingTotals
        }

    }

      // (((((((((((((((((((((((( todo.. add relief pitchers FieldLW into defense calculation ))))))))))))))))))))))))

    // **********************************************
    //
    // esimate!  calculate the  defensive totals
    // if dhGame, will need to add in the fielding totals from the pitchers
    //
    // **********************************************
    score.summary.Visit.Def = calculateTotalDefense(score.Visit.BattingTotals, game.dhGame, score.Visit.PitchingTotals, null);;
    score.summary.Home.Def = calculateTotalDefense(score.Home.BattingTotals, game.dhGame, score.Home.PitchingTotals, null);




    // *************************************************
    //
    // go inning by inning to get to the entire pitching staff
    // estimate the runs for each pitcher
    // get the score through 8 innings... keeping track of the scores as we go!
    // puts the pitchers into the pitchersUsed array
    //
    // ************************************************* 

    if (!homeTeam.pitchingStaff.override) {
        setPitchersAndEstimateScore(score, homeTeam, visitTeam, game.dhGame);

        // put the rest of the pitchers into the box score, SKIP the starters.
        // for each pitcher that makes an appearance in our game
        // put their stats in the boxScore stats format
        // and add their totals to the totals
        for (var team = 0; team < 2; team++) {

            // visitors first (when team == 0)

            // ***** point at correct pitching order
            // *******  and grab the starters pitching totals
            if (team == 0) {
                pitchingList = visitTeam.pitchingStaff.pitchersUsed;
                roster = visitRoster;
                pitchingTotals = score.Visit.PitchingTotals;
                coaches = visitTeam.extra.Coordinators;
            } else {
                // home team pointers
                pitchingList = homeTeam.pitchingStaff.pitchersUsed;
                roster = homeRoster;
                pitchingTotals = score.Home.PitchingTotals;
                coaches = homeTeam.extra.Coordinators;
            }

            // ********* for each pitcher in the pitching staff AFTER THE STARTER!
            // ********* create a line in the box score
            for (var pitcher = 1; pitcher < pitchingList.length; pitcher++) {
                var nextPitcher = pitchingList[pitcher];
                calculatePitchingLW(nextPitcher, coaches);
                insertPitcherIntoBoxscore(nextPitcher, pitcher, pitchingTotals, score, team, game.dhGame);
            }


            // put the total game stats into the box score
            // fix up IP based on outs...
            var newIP = getIPFromOuts(pitchingTotals.OUT);
            pitchingTotals.IP = newIP;
            if (team == 0) {
                score.Visit.PitchingTotals = pitchingTotals;
            } else {
                score.Home.PitchingTotals = pitchingTotals
            }

        }

          // recalculated the defense with the new pitchers
    score.summary.Visit.Def = calculateTotalDefense(score.Visit.BattingTotals, game.dhGame, score.Visit.PitchingTotals, visitTeam.pitchingStaff.pitchersUsed[0]);
    score.summary.Home.Def = calculateTotalDefense(score.Home.BattingTotals, game.dhGame, score.Home.PitchingTotals, homeTeam.pitchingStaff.pitchersUsed[0]);

  

    }
          // add in errors from pitchers....
          score.Visit.BattingTotals.E += score.Visit.PitchingTotals.E;
          score.Home.BattingTotals.E  += score.Home.PitchingTotals.E;

    // *************************************************
    //
    // now have all the lines in the line score
    // calculate runs for each pitcher
    // get the score through 8 innings... keeping track of the scores as we go!
    //
    // *************************************************

  

    // visiting pitchers...
    var runningV = new Array(27);
    runningV.fill(0);
    changesV = [];
    var nextChange = -1;
    var vruns = 0;  // visiting pitchers runs allowed, the home teams runs
    for (var p = 0; p < score.Visit.Pitchers.length; p++) {

        // round up
        var decimalRuns = calculateRunsAllowed(score.Home.BattingTotals, score.Home.BattingTotals, score.Visit.Pitchers[p], score.summary.Home.Off, score.summary.Visit.Def);
        vruns += Math.floor(decimalRuns + 0.5);
        nextChange = score.Visit.Pitchers[p].OUT;
        if (nextChange < 0)
            nextChange = 0;
        runningV[nextChange] += vruns;
        changesV.push(nextChange);
    }
    score.Visit.PitchingTotals.R = vruns;

    // home pitchers...
    var runningH = new Array(27);
    runningH.fill(0);
    changesH = [];
    var nextChange = -1;
    var hruns = 0;
    for (var p = 0; p < score.Home.Pitchers.length; p++) {
        var decimalRuns = calculateRunsAllowed(score.Visit.BattingTotals, score.Home.BattingTotals, score.Home.Pitchers[p], score.summary.Visit.Off, score.summary.Home.Def);
        hruns += Math.floor(decimalRuns + 0.5);
        nextChange = score.Home.Pitchers[p].OUT;
        if (nextChange < 0)
            nextChange = 0;
        runningH[nextChange] += hruns;
        changesH.push(nextChange);

    }
    score.Home.PitchingTotals.R = hruns;



    
    // ****************************************************
    // 
    // deal with the 8th inning.
    //
    // ***************************************************
    

    var doCloserLogic = false;
    if (doCloserLogic) {

        var homeScore = vruns;
        var visitScore = hruns; // for readability.
        var homeCloserOuts = 0;
        var visitCloserOuts = 0;
        var homeSetupPitchers = [];
        var visitSetupPitchers = [];

        // top of the 8th.. if HOME TEAM WINNING by less than 4 runs, look for the final pitchers...

        var outsleft = 6;
        if (homeTeam.pitchingStaff.starter.OUT && homeTeam.pitchingStaff.starter.OUT > 21) {
            outsleft = 27 - homeTeam.pitchingStaff.starter.OUT;
        }

        if (outsleft > 3) {
            // do we need to see if there are 3 more outs from the current reliever

        }

        if (outsleft > 3 && homeTeam.pitchingStaff.closer && (homeScore - visitScore < 4) && (visitScore < homeScore)) {  // visitor winning by less than 4 runs

            homeCloserOuts = homeTeam.pitchingStaff.closer.OUT;
            if (homeCloserOuts < outsleft) {
                // need another pitcher or two :(
                var outsleft = outsleft - homeCloserOuts;
                for (maxouts = 3; maxouts < 15; maxouts++) {
                    for (p = 0; p < homeTeam.pitchingStaff.relievers.length; p++) {

                        if (!homeTeam.pitchingStaff.relievers[p].G || homeTeam.pitchingStaff.relievers[p].G == 0) {
                            if (homeTeam.pitchingStaff.relievers[p].OUT > 0 && homeTeam.pitchingStaff.relievers[p].OUT < maxouts) {
                                // put in the rotation, etc....
                                homeSetupPitchers.push(homeTeam.pitchingStaff.relievers[p]);
                                outsleft -= homeTeam.pitchingStaff.relievers[p].OUT;
                                if (outsleft <= 0)
                                    break;
                            }
                        }
                    }
                    if (outsleft <= 0)
                        break;
                }
            }
            homeSetupPitchers.push(homeTeam.pitchingStaff.closer);
            /*
                score.Home.Pitchers.push( homeTeam.pitchingStaff.closer );
                var decimalRuns = calculateRunsAllowed(score.Visit.BattingTotals, score.Home.BattingTotals, score.Home.Pitchers[p], score.summary.Visit.Off, score.summary.Home.Def );
                hruns += Math.floor( decimalRuns + 0.5);
                running[26] += hruns; // final score for visiting team...
                */
        } else {
            // top of 8th and not a save situation.. see if we need any outs for this inning
            if (outsleft > 3) {
                outsleft = outsleft - 3;
                for (maxouts = 3; maxouts < 15; maxouts++) {
                    for (p = 0; p < homeTeam.pitchingStaff.relievers.length; p++) {

                        if (!homeTeam.pitchingStaff.relievers[p].G || homeTeam.pitchingStaff.relievers[p].G == 0) {
                            if (homeTeam.pitchingStaff.relievers[p].OUT > 0 && homeTeam.pitchingStaff.relievers[p].OUT < maxouts) {
                                // put in the rotation, etc....
                                homeSetupPitchers.push(homeTeam.pitchingStaff.relievers[p]);
                                outsleft -= homeTeam.pitchingStaff.relievers[p].OUT;
                                if (outsleft <= 0) {
                                    if (outsleft < 0) {
                                        homeTeam.pitchingStaff.relievers[p].OUT = homeTeam.pitchingStaff.relievers[p].OUT + outsleft;
                                        homeTeam.pitchingStaff.relievers[p]["UnusedOuts"] = -outsleft;
                                        homeTeam.pitchingStaff.relievers[p]["pStats"].OUT = homeTeam.pitchingStaff.relievers[p].OUT
                                        homeTeam.pitchingStaff.relievers[p]["pStats"]["UnusedOuts"] = -outsleft;

                                        var IP = getIPFromOuts(homeTeam.pitchingStaff.relievers[p]["pStats"].OUT);
                                        /*
                                        var newIP = homeTeam.pitchingStaff.relievers[p]["pStats"].IP / 3;
                                        var newIPInteger = Math.floor(newIP);
                                        newIP = (newIP - newIPInteger == 0) ? newIPInteger : ((newIP - newIPInteger < 0.5) ? newIPInteger + 0.1 : newIPInteger + 0.2);
                                        */
                                        homeTeam.pitchingStaff.relievers[p]["pStats"].IP = newIP;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                    if (outsleft <= 0)
                        break;
                }
            }
        }

        // add in the score for the top of the 8th to the visiting team...
        for (var p = 0; p < homeSetupPitchers.length; p++) {
            insertPitcherIntoBoxscore(homeSetupPitchers[p], score.Home.Pitchers.length, score.Home.PitchingTotals, score, 1, game.dhGame);
            var pindex = score.Home.Pitchers.length - 1;
            var decimalRuns = calculateRunsAllowed(score.Visit.BattingTotals, score.Home.BattingTotals, score.Home.Pitchers[pindex], score.summary.Visit.Off, score.summary.Home.Def);
            hruns += Math.floor(decimalRuns + 0.5);
            nextChange = homeSetupPitchers[p].OUT;
            if (nextChange < 0)
                nextChange = 0;
            runningH[nextChange] += hruns;
            changesH.push(nextChange);

        }
        var newIP = getIPFromOuts(score.Home.PitchingTotals.OUT);
        /*
        var newIP = score.Home.PitchingTotals.OUT / 3;
        var newIPInteger = Math.floor(newIP);
        newIP = (newIP - newIPInteger == 0) ? newIPInteger : ((newIP - newIPInteger < 0.5) ? newIPInteger + 0.1 : newIPInteger + 0.2);
        */
        score.Home.PitchingTotals.IP = newIP;
        score.Home.PitchingTotals.R = hruns;

        //                      ****************
        //
        // bottom of the 8th.. if VISITING TEAM WINNING by less than 4 runs, look for the final pitchers...
        //
        //                      ****************

        var outsleft = 6;
        if (visitTeam.pitchingStaff.starter.OUT > 21) {
            outsleft = 27 - visitTeam.pitchingStaff.starter.OUT;
        }

        if (outsleft > 3) {
            // do we need to see if there are 3 more outs from the current reliever

        }

        if (outsleft > 3 && visitTeam.pitchingStaff.closer && (visitScore - visitScore < 4) && (visitScore < visitScore)) {  // visitor winning by less than 4 runs

            visitCloserOuts = visitTeam.pitchingStaff.closer.OUT;
            if (visitCloserOuts < outsleft) {
                // need another pitcher or two :(
                var outsleft = outsleft - visitCloserOuts;
                for (maxouts = 3; maxouts < 15; maxouts++) {
                    for (p = 0; p < visitTeam.pitchingStaff.relievers.length; p++) {

                        if (!visitTeam.pitchingStaff.relievers[p].G || visitTeam.pitchingStaff.relievers[p].G == 0) {
                            if (visitTeam.pitchingStaff.relievers[p].OUT > 0 && visitTeam.pitchingStaff.relievers[p].OUT < maxouts) {
                                // put in the rotation, etc....
                                visitSetupPitchers.push(visitTeam.pitchingStaff.relievers[p]);
                                outsleft -= visitTeam.pitchingStaff.relievers[p].OUT;

                                if (outsleft <= 0)
                                    break;
                            }
                        }
                    }
                    if (outsleft <= 0)
                        break;
                }
            }
            visitSetupPitchers.push(visitTeam.pitchingStaff.closer);
            /*
                score.visit.Pitchers.push( visitTeam.pitchingStaff.closer );
                var decimalRuns = calculateRunsAllowed(score.Visit.BattingTotals, score.visit.BattingTotals, score.visit.Pitchers[p], score.summary.Visit.Off, score.summary.visit.Def );
                hruns += Math.floor( decimalRuns + 0.5);
                running[26] += hruns; // final score for visiting team...
                */
        } else {
            // top of 8th and not a save situation.. see if we need any outs for this inning
            if (outsleft > 3) {
                outsleft = outsleft - 3;
                for (maxouts = 3; maxouts < 15; maxouts++) {
                    for (p = 0; p < visitTeam.pitchingStaff.relievers.length; p++) {

                        if (!visitTeam.pitchingStaff.relievers[p].G || visitTeam.pitchingStaff.relievers[p].G == 0) {
                            if (visitTeam.pitchingStaff.relievers[p].OUT > 0 && visitTeam.pitchingStaff.relievers[p].OUT < maxouts) {
                                // put in the rotation, etc....
                                visitSetupPitchers.push(visitTeam.pitchingStaff.relievers[p]);
                                outsleft -= visitTeam.pitchingStaff.relievers[p].OUT;
                                if (outsleft <= 0) {
                                    if (outsleft < 0) {
                                        visitTeam.pitchingStaff.relievers[p].OUT = visitTeam.pitchingStaff.relievers[p].OUT + outsleft;
                                        visitTeam.pitchingStaff.relievers[p]["UnusedOuts"] = -outsleft;
                                        visitTeam.pitchingStaff.relievers[p]["pStats"].OUT = visitTeam.pitchingStaff.relievers[p].OUT
                                        visitTeam.pitchingStaff.relievers[p]["pStats"]["UnusedOuts"] = -outsleft;

                                        var newIP = getIPFromOuts(visitTeam.pitchingStaff.relievers[p]["pStats"].OUT);
                                        /*
                                        var newIP = visitTeam.pitchingStaff.relievers[p]["pStats"].IP / 3;
                                        var newIPInteger = Math.floor(newIP);
                                        newIP = (newIP - newIPInteger == 0) ? newIPInteger : ((newIP - newIPInteger < 0.5) ? newIPInteger + 0.1 : newIPInteger + 0.2);
                                        */
                                        visitTeam.pitchingStaff.relievers[p]["pStats"].IP = newIP;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                    if (outsleft <= 0)
                        break;
                }
            }
        }

        // add in the score for the top of the 8th to the visiting team...
        for (var p = 0; p < visitSetupPitchers.length; p++) {
            insertPitcherIntoBoxscore(visitSetupPitchers[p], score.Visit.Pitchers.length, score.Visit.PitchingTotals, score, 0, game.dhGame);
            var pindex = score.Visit.Pitchers.length - 1;
            var decimalRuns = calculateRunsAllowed(score.Home.BattingTotals, score.Visit.BattingTotals, score.Visit.Pitchers[pindex], score.summary.Home.Off, score.summary.Visit.Def);
            vruns += Math.floor(decimalRuns + 0.5);
            nextChange = visitSetupPitchers[p].OUT;
            if (nextChange < 0)
                nextChange = 0;
            runningV[nextChange] += hruns;
            changesV.push(nextChange);

        }
        var newIP = getIPFromOuts(score.Visit.PitchingTotals.OUT);
        score.Visit.PitchingTotals.IP = newIP;
        score.Visit.PitchingTotals.R = vruns;

    }
    // ****************************************************
    // 
    // deal with tie games...
    //
    // ***************************************************
    if (score.Visit.PitchingTotals.R == score.Home.PitchingTotals.R) {
        /*
        Calculate each team’s total LW’s for the three main statistical categories
        (pitching, defense, batting).  Whichever team has the higher LW total, will win the game.
        Should the two teams have the same LW total, then the home team is assigned the win.
 
        The winning team is given one more run to add to their score.  
        To conclude this step, assign one earned run to a pitcher on the losing team.  
        To determine who is assigned the run, it is given to the pitcher with 
        the lowest LW total for that day.  
        If two or more pitchers have the same LW total and it’s the lowest for the day, 
        then assign the run to the pitcher who pitched closest to the end of the game.
        */
        var homeLW = score.Home.BattingTotals.LW + score.Home.BattingTotals.FieldLW + score.Home.PitchingTotals.LW;
        var visitLW = score.Visit.BattingTotals.LW + score.Visit.BattingTotals.FieldLW + score.Visit.PitchingTotals.LW;

        var winner = "home"; // by default if LWs are equal
        if (homeLW != visitLW) {
            // then have a winner!
            winner = homeLW > visitLW ? "home" : "visit";
        }

        var losingStaff = score.Visit.Pitchers;  // defalult losing pitching staff if home wins
        if (winner == "home") {
            // home team wins, so visitors allow another run..
            score.Visit.PitchingTotals.R++;
        } else {
            // visitor wins, so home team allow another run
            score.Home.PitchingTotals.R++;
            losingStaff = score.Home.Pitchers;      // losing pitching staff
        }

        // last step, who gets the extra earned run?
        // the lowest LW total for that day.  
        // If two or more pitchers have the same LW total and it’s the lowest for the day, 
        // then assign the run to the pitcher who pitched closest to the end of the game.
        var lowestIndex = 0;
        var lowestLW = 0;
        if( losingStaff[0] && losingStaff[0].LW )
            lowestLW = losingStaff[0].LW;
        var tiedIndex = [0];
        for (i = 1; i < losingStaff.length; i++) {
            if (losingStaff[i] && losingStaff[i].LW == lowestLW) {
                tiedIndex.push(i);
            } else {
                if (losingStaff[i] && losingStaff[i].LW < lowestLW) {
                    var lowestLW = losingStaff[i].LW;
                    var tiedIndex = [i];            // start tied count over                       
                }
            }
        }

        // have the lowest LW.. see if there are more than one pitcher here
        if (tiedIndex.length > 1) {
            // grab the last pitcher in the list
            lowestIndex = tiedIndex[tiedIndex.lenght - 1];
        }

        // have the losing pitcher's index!  Give him the run
        if( losingStaff[lowestIndex])
            losingStaff[lowestIndex].Runs++;


    }

    if( score.Visit.PitchingTotals.R > score.Home.PitchingTotals.R ) {
        // find the last 3 outs from the visitor's pitching and see if they gave up runs in that last inning...

    }
    // ****************************************************
    // 
    // finally, assign pitcher decisions
    //
    // ***************************************************

    var homeScore = new Array(27);
    homeScore.fill(0);
    lastHomeScore = 0;
    var nextHomeOut = 0;
    var visitScore = new Array();
    visitScore.fill(0);
    var nextVisitOut = 0;
    lastVisitScore = 0;

    for (var i = 0; i < score.Visit.Pitchers.length; i++) {
        var runs = Number(score.Visit.Pitchers[i].Runs);
        var outs = Number(score.Visit.Pitchers[i].OUT);
        for (var o = 0; o < outs; o++) {
            visitScore[nextVisitOut] = lastVisitScore + runs;
            nextVisitOut++;
        }
        lastVisitScore += runs;
    }
    for (var i = 0; i < score.Home.Pitchers.length; i++) {
        var runs = Number(score.Home.Pitchers[i].Runs);
        var outs = Number(score.Home.Pitchers[i].OUT);
        for (var o = 0; o < outs; o++) {
            homeScore[nextHomeOut] = lastHomeScore + runs;
            nextHomeOut++;
        }
        lastHomeScore += runs;
    }

    // set up decision object
    var Decision = {
        Win: null,
        Loss: null,
        Save: null,
        Blown: null,
        Hold: [],
        HoldId: []
    }

    // see who won
    var homeTeamWins = true;
    if (score.Visit.PitchingTotals.R < score.Home.PitchingTotals.R) {
        homeTeamWins = false;
    }

    if( homeTeamWins ){

        // find the winning, saving and hold pitchers for the home team
        var firstOut = 0;
        var lastScore = 0;
        var earliestWinner = 99;
        var mostOuts = 0;
        var mostOutsIndex = 0;
        var leastRuns = 99; // forces the starter to have the least
        var leastRunsIndex = 0;
        var winIndex = -1;
        var winRuns = 99;
        var winOuts = 0;

        for (var i = 0; i < score.Home.Pitchers.length; i++) {

            var nextPitcher = score.Home.Pitchers[i];
            var runs = Number(nextPitcher.Runs);
            var outs = Number(nextPitcher.OUT);

            if( outs > mostOuts) {
                mostOutsIndex = i;
            }
            var prevLeastRunsIndex = leastRunsIndex;
            var prevLeastRuns = leastRuns;
            if( runs < leastRuns) {
                leastRunsIndex = i;
            }

            // note "score" is actually runs allowed by each team
            var scoreWhenEntered = visitScore[firstOut] -homeScore[firstOut];
            if( i==0 ) {
                scoreWhenEntered = 0;   // always a tie when the pitchers start the game
            }
            var lastOut = firstOut+outs;
            if( lastOut > 26)
                lastOut = 26;
            var scoreNow = visitScore[lastOut] - homeScore[lastOut];
            firstOut += outs;   // for next round...

            // score was losing/tied but now winning....can win
            if( scoreWhenEntered <=0 && scoreNow > 0) {
                //then can win!
                if( i<earliestWinner) {
                    earliestWinner = i; // earliest potential winner
                }
                if( outs >= (5*3) && i==0) {
                    // then the starter when at least 5 innings
                    Decision.Win = nextPitcher.FullName;
                    winIndex = 0;
                } else if( winIndex != 0) {
                    // then either the starter didn'g go 5 for the win or it's a relief pitcher
                    if( Decision.Win == null) {
                        // no one is a winner yet.
                        Decision.Win = nextPitcher.FullName;
                        winIndex = i;
                        winOuts = outs;
                        winRuns = runs;
                    } else {
                        // there is a winner.. this pitcher is LATER, so only swap in if less runs or pitched longer
                        // this pitcher allowed the least amount of runs in the entire game so far
                        // see if tied with an earlier pitcher
                        if (runs < winRuns || (runs == winRuns && outs > winOuts)) {
                            // fewer runs as previous winner, or same runs as previous pitcher but more innings.. 
                            Decision.Win = nextPitcher.FullName;
                            winIndex = i;
                            winOuts = outs;
                            winRuns = runs;
                        } else {
                            // leave the previous pitcher as he pitched earlier
                        }
                    }
                }
            } else {
                if (scoreNow <= 0) {
                    Decision.Win = null;    // any previous potential winner can't win now.
                    winIndex = -1;
                    winOuts = 0;
                    winRuns = 99;
                }

                // can hold or save
                if( i == (score.Home.Pitchers.length-1)) {
                    // last pitcher in, potential save
                    if( runs<4 && scoreWhenEntered < 3) {
                        Decision.Save = nextPitcher.FullName;
                    }
                } else {
                    // not, the first or last pitcher...potential hold
                    if( i>0 && runs<4) {
                        Decision.Hold.push( nextPitcher.FullName);
                    }
                }
            }
        }

        // now find the visiting losing pitcher, and possibly a blown save (pitcher can have both loss and blown save)
            // ****************************************************
            // 
            // also, deal with games where the home team wins after 8 1/2 innings
            // might need to remove the last 3 outs of the visiting pitchers box score
             //
            // ***************************************************
        firstOut = 0;
        for (var i = 0; i < score.Visit.Pitchers.length; i++) {

            var nextPitcher = score.Visit.Pitchers[i];
            var runs = Number(nextPitcher.Runs);
            var outs = Number(nextPitcher.OUT);
            var scoreWhenEntered = homeScore[firstOut] -visitScore[firstOut];
            if( i==0 ) {
                scoreWhenEntered = 0;   // always a tie when the pitchers start the game
            }
            var lastOut = firstOut+outs;
            if( lastOut > 26)
                lastOut = 26;
            var scoreNow = homeScore[lastOut] - visitScore[lastOut];

            if( scoreWhenEntered >=0 && scoreNow <0 ) {
                // then was tied or winning and now losing...make him the loser.. last one to do this loses by MLB rules
                Decision.Loss = nextPitcher.FullName;
            } 
            if( i == (score.Visit.Pitchers.length-1)) {
                // see if a blown save...
                if( scoreWhenEntered >0 && scoreNow<0) {
                    // it is!
                    // note losing pitcher can also have a blown save
                    Decision.Blown = nextPitcher.FullName;
                }
            }
            if( firstOut >= 24 && scoreWhenEntered < 0) {
                // then need to remove this pitcher, as the home team already won.
            }
            firstOut += outs;   // for next round...
        }

    } else {
        // visiting team wins
                // find the winning, saving and hold pitchers for the visiting team
                var firstOut = 0;
                var lastScore = 0;
                var earliestWinner = 99;
                var mostOuts = 0;
                var mostOutsIndex = 0;
                var leastRuns = 99; // forces the starter to have the least
                var leastRunsIndex = 0;
                var winIndex = -1;
                var winRuns = 99;
                var winOuts = 0;
        
                for (var i = 0; i < score.Visit.Pitchers.length; i++) {
        
                    var nextPitcher = score.Visit.Pitchers[i];
                    var runs = Number(nextPitcher.Runs);
                    var outs = Number(nextPitcher.OUT);
        
                    if( outs > mostOuts) {
                        mostOutsIndex = i;
                    }
                    var prevLeastRunsIndex = leastRunsIndex;
                    var prevLeastRuns = leastRuns;
                    if( runs < leastRuns) {
                        leastRunsIndex = i;
                    }
        
                    // note "scores" are actually runs allowed by the team
                    var scoreWhenEntered = homeScore[firstOut] -visitScore[firstOut];
                    if( i==0 ) {
                        scoreWhenEntered = 0;   // always a tie when the pitchers start the game
                    }
                    var lastOut = firstOut+outs;
                    if( lastOut > 26)
                        lastOut = 26;
                    var scoreNow = homeScore[lastOut] - visitScore[lastOut];
                    firstOut += outs;   // for next round...
        
                    // score was losing/tied but now winning....can win
                    if( scoreWhenEntered <=0 && scoreNow > 0) {
                        //then can win!
                        if( i<earliestWinner) {
                            earliestWinner = i; // earliest potential winner
                        }
                        if( outs >= (5*3) && i==0) {
                            // then the starter when at least 5 innings
                            Decision.Win = nextPitcher.FullName;
                            winIndex = 0;
                        } else if( winIndex != 0) {
                            // then either the starter didn'g go 5 for the win or it's a relief pitcher
                            if( Decision.Win == null) {
                                // no one is a winner yet.
                                Decision.Win = nextPitcher.FullName;
                                winIndex = i;
                                winOuts = outs;
                                winRuns = runs;
                            } else {
                                // there is a winner.. this pitcher is LATER, so only swap in if less runs or pitched longer
                                // this pitcher allowed the least amount of runs in the entire game so far
                                // see if tied with an earlier pitcher
                                if (runs < winRuns || (runs == winRuns && outs > winOuts)) {
                                    // fewer runs as previous winner, or same runs as previous pitcher but more innings.. 
                                    Decision.Win = nextPitcher.FullName;
                                    winIndex = i;
                                    winOuts = outs;
                                    winRuns = runs;
                                } else {
                                    // leave the previous pitcher as he pitched earlier
                                }
                            }
                        }
                    } else {
                        if (scoreNow <= 0) {
                            Decision.Win = null;    // any previous potential winner can't win now.
                            winIndex = -1;
                            winOuts = 0;
                            winRuns = 99;
                        }
        
                        // can hold or save
                        if( i == (score.Visit.Pitchers.length-1)) {
                            // last pitcher in, potential save
                            if( runs<4 && scoreWhenEntered < 3) {
                                Decision.Save = nextPitcher.FullName;
                            }
                        } else {
                            // not, the first or last pitcher...potential hold
                            if( i>0 && runs<4) {
                                Decision.Hold.push( nextPitcher.FullName);
                            }
                        }
                    }
                }
        
                // now find the home losing pitcher, and possibly a blown save (pitcher can have both loss and blown save)
                firstOut = 0;
                for (var i = 0; i < score.Home.Pitchers.length; i++) {
        
                    var nextPitcher = score.Home.Pitchers[i];
                    var runs = Number(nextPitcher.Runs);
                    var outs = Number(nextPitcher.OUT);
                    var scoreWhenEntered = visitScore[firstOut] -homeScore[firstOut];
                    if( i==0 ) {
                        scoreWhenEntered = 0;   // always a tie when the pitchers start the game
                    }
                    var lastOut = firstOut+outs;
                    if( lastOut > 26)
                        lastOut = 26;
                    var scoreNow = visitScore[lastOut] - homeScore[lastOut];
                    firstOut += outs;   // for next round...

                    if( scoreWhenEntered >=0 && scoreNow <0 ) {
                        // then was tied or winning and now losing...make him the loser.. last one to do this loses by MLB rules
                        Decision.Loss = nextPitcher.FullName;
                    } 
                    if( i == (score.Home.Pitchers.length-1)) {
                        // see if a blown save...
                        if( scoreWhenEntered >0 && scoreNow<0) {
                            // it is!
                            // note losing pitcher can also have a blown save
                            Decision.Blown = nextPitcher.FullName;
                        }
                    }
                }
    }

    // ****************************************************
    // 
    // transfer totals to summary
    //
    // ***************************************************
    score.summary.Visit.name = game.visit;
    score.summary.Home.name = game.home;
    score.summary.Decision = Decision;

    score.summary.Visit.R = Math.floor(0.5 + score.Home.PitchingTotals['R']); // the opposing pitchers' runs
    var hits = (score.Visit.BattingTotals['H'] + score.Home.PitchingTotals['H']) / 2;
    score.summary.Visit.H = hits.toFixed(0);
    score.summary.Visit.E = score.Visit.BattingTotals['E'].toFixed(0);

    score.summary.Visit.Pit = score.Visit.PitchingTotals['LW'].toFixed(2);
    score.summary.Visit.BR = score.Visit.BattingTotals['Bases'].toFixed(2);
    score.Visit.BattingTotals["Pos"] = "";
    score.Visit.BattingTotals["GameDate"] = "";
    score.Visit.BattingTotals["Name"] = "TOTALS";
    score.Visit.PitchingTotals["Runs"] = score.Visit.PitchingTotals["R"];
    score.Visit.PitchingTotals["Pos"] = "";
    score.Visit.PitchingTotals["GameDate"] = "";
    score.Visit.PitchingTotals["Name"] = "TOTALS";

    score.summary.Home.R = Math.floor(0.5 + score.Visit.PitchingTotals['R']); // the opposing pitchers' runs
    hits = (score.Home.BattingTotals['H'] + score.Visit.PitchingTotals['H']) / 2;
    score.summary.Home.H = hits.toFixed(0);
    score.summary.Home.E = score.Home.BattingTotals['E'].toFixed(0);
    score.summary.Home.Pit = score.Home.PitchingTotals['LW'].toFixed(2);
    score.summary.Home.BR = score.Home.BattingTotals['Bases'].toFixed(2);
    score.Home.BattingTotals["Pos"] = "";
    score.Home.BattingTotals["GameDate"] = "";
    score.Home.BattingTotals["Name"] = "TOTALS";
    score.Home.PitchingTotals["Runs"] = score.Home.PitchingTotals["R"];
    score.Home.PitchingTotals["Pos"] = "";
    score.Home.PitchingTotals["GameDate"] = "";
    score.Home.PitchingTotals["Name"] = "TOTALS";
   
    // put decisions into the box score
    function putInDecision(boxscore, decision, letter) {

        if (Decision[decision] && Decision[decision] != "") {
            var found = false;
            for (var p = 0; p < boxscore.Home.Pitchers.length; p++) {
                if (boxscore.Home.Pitchers[p].FullName == Decision[decision]) {
                    found = true;
                    boxscore.summary.Decision[decision] = Decision[decision];
                    boxscore.summary.Decision[decision + "Id"] = boxscore.Home.Pitchers[p].PlayerId;
                    if( boxscore.Home.Pitchers[p].Decision == "BS" && letter=="L" ) {
                        letter = "BS, L"
                    }
                    boxscore.Home.Pitchers[p].Decision = letter;
                    break;
                }
            }
            if (!found) {
                // look on visitor's team;
                for (var p = 0; p < boxscore.Visit.Pitchers.length; p++) {
                    if (boxscore.Visit.Pitchers[p].FullName == Decision[decision]) {
                        found = true;
                        boxscore.summary.Decision[decision] = Decision[decision];
                        boxscore.summary.Decision[decision + "Id"] = boxscore.Visit.Pitchers[p].PlayerId;
                        if( boxscore.Visit.Pitchers[p].Decision == "BS" && letter=="L" ) {
                            letter = "BS, L"
                        }
                        boxscore.Visit.Pitchers[p].Decision = letter;
                        break;
                    }
                }
            }
        }
    }

    function putInHold(boxscore, holds) {

        if (Decision.Hold && Decision.Hold.length > 0) {

            var holdIndex = 0;
            for (var h = 0; h < Decision.Hold.length; h++) {
                var nextHold = Decision.Hold[h];

                if (nextHold && nextHold != "") {
                    var found = false;
                    for (var p = 0; p < boxscore.Home.Pitchers.length; p++) {
                        if (boxscore.Home.Pitchers[p].FullName == nextHold) {
                            found = true;
                            boxscore.summary.Decision.Hold[holdIndex] = nextHold;
                            boxscore.summary.Decision.HoldId[holdIndex] = boxscore.Home.Pitchers[p].PlayerId;
                            boxscore.Home.Pitchers[p].Decision = "HD";
                            holdIndex++
                            break;
                        }
                    }
                    if (!found) {
                        // look on visitor's team;
                        for (var p = 0; p < boxscore.Visit.Pitchers.length; p++) {
                            if (boxscore.Visit.Pitchers[p].FullName == nextHold) {
                                found = true;
                                boxscore.summary.Decision.Hold[holdIndex] = nextHold;
                                boxscore.summary.Decision.HoldId[holdIndex] = boxscore.Visit.Pitchers[p].PlayerId;
                                boxscore.Visit.Pitchers[p].Decision = "HD";
                                holdIndex++
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    // stuff the data into the boxscore...
    putInDecision(score, "Win", "W");
    putInDecision(score, "Blown", "BS"); // do this first in case he also lost.
    putInDecision(score, "Loss", "L");
    putInDecision(score, "Save", "SV");
    putInHold(score, Decision.Hold);

    // *************************************************
    //
    // render CSV Version of full score
    //
    // **************************************************

    if (game.saveCSVTestFile === true) {
        // CSV version of full score
        renderCSV(game, score);
    }

    return score;
}
