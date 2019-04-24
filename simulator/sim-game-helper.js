// **********************************************************
//
// File: sim-game.js 
// RFB Simulator: simOffense() function for calculating the ArW and AbrW
// by "simulating" who scores runs and who gets rbis
//
//    Added Runs Weighted (ArW)
//    ArW = Runs * ((Year to Date Speed Score/100)) = Added Runs Weighted (ArW)
//    A player who hits drives in a run (as determined by simulator)
//    gets a value as follows for each RBI.
//    ArbW = RBI * ((Year to Date ISO*3) /(10)) = Added Runs Batted In Weighted (ArbW)
//
// By: Eddie Dombrower 
//
// **********************************************************

module.exports = {

    simulateOffense: function (lineup, roster) {


// helper
// stuff offensive stats
var PAEvents;
var PSUsed;
var PAAvailable;
var OtherEvents;
var RunBonus;
var RBIBonus;

// score keeping
var Inning;
var RunsScored;
var RBIScored;
var Outs;
var OnFirst;
var OnSecond;
var OnThird;
var RunnersOnBase;
var RunnersInScoringPosition;

var PAPlays = [
    "1B",
    "2B",
    "3B",
    "HR",
    "SAC",
    "SF",
    "HBP",
    "BB",
    "OUTS"
]

var SpecialPlays = [
    ["IFH", "1B"],  // infield hit.. changes RBI opportunity
    ["BH", "1B"],  // bunt hit... changes RBI opportunity
    ["GIDP", "OUTS"], // two outs.. only used if less than 2 outs or only PA left.. erases next logical player
    ["HFC", "OUTS"]  // batter goes to first, some batter ahead is out... only if a force?
]

var OutPlays = ["OUTS", "GIDP", "HFC", "SF", "SH", "K"];
var OneBasePlays = ["1B", "BB", "IFH", "BH"];

insertPlays = function (playerIndex, play, count) {
    for (var p = 0; p < count; p++) {
        PAEvents[playerIndex].push(play);
        PAUsed[playerIndex].push(false);
    }
}

insertSpecialPlays = function (playerIndex, play, count) {
    for (var p = 0; p < count; p++) {
        OtherEvents[playerIndex].push(play);
    }
}

substituteSpecialPlays = function (playerIndex, newPlay, oldPlay, count) {

    while (count > 0) {
        for (var p = 0; p < PAAvailable[playerIndex]; p++) {
            if (PAEvents[playerIndex][p] == oldPlay) {
                PAEvents[playerIndex][p] = newPlay;
                break;
            }
        }
        count--;
    }
}

// *********************************************************
//
// baseball plays
//
// *********************************************************
evaluateBases = function () {
    RunnersOnBase = 0;
    RunnersInScoringPosition = false;

    if (OnFirst !== false) RunnersOnBase = 1;
    if (OnSecond !== false) {
        RunnersOnBase++;
        RunnersInScoringPosition = true;
    }
    if (OnThird !== false) {
        RunnersOnBase++;
        RunnersInScoringPosition = true;
    }
}

advanceAllOneBase = function (batter, play) {

    var scores = [false, false, false, false];

    if (play == "BB") {
        // force forward only
        if( OnFirst  !== false) {
            force2nd = OnSecond;
            OnSecond = OnFirst;
            if( force2nd !== false ) {
                // then runner on 2nd moving to 3rd
                force3rd = OnThird;
                onThird = force2nd;
                scores[0] = force3rd; // either false or the lineup index
            }
        }
    } else {

        // a hit ball to advance runners
        if (OnThird !== false) {
            scores[0] = OnThird;
            OnThird = false;
        }
        if (OnSecond !== false) {
            if (play == "1B") {
                scores[1] = OnSecond;
            } else {
                OnThird = OnSecond;
            }
            OnSecond = false;
        }
        if (OnFirst !== false) {
            OnSecond = OnFirst;
            // batter goes to first
        }
    }
    OnFirst = batter;

    evaluateBases();
    return scores;

}

advanceAllTwoBases = function (batter) {
    var scores = [OnThird, OnSecond, false, false];

    OnThird = false;
    OnSecond = batter;
    if (OnFirst !== false) {
        OnThird = OnFirst;
        OnFirst = false;
    }
    evaluateBases();
    return scores;

}

advanceAllThreeBases = function (batter) {
    var scores = [OnThird, OnSecond, OnFirst, false];

    OnThird = batter;
    OnSecond = false;
    OnFirst = false;
    evaluateBases();
    return scores;

}

scoreHR = function (batter) {
    var scores = [OnThird, OnSecond, OnFirst, batter];
    OnThird = false;
    OnSecond = false;
    OnFirst = false;
    evaluateBases();
    return scores;
}

calledOut = function (batter, play) {
    var scores = [false, false, false, false];
    if (play != "OUTS ") {
        // then deal with special cases
        // GIDP, HFC, SF, SH
        if (play == "GIDP") {

            // ***************** DOUBLE PLAY **************************
            if (Outs < 2 && RunnersOnBase > 0) {
                // less than 2 outs and runners on base.
                // the first of two outs recorded here...
                Outs++;

                // get the right guy out...
                if (OnFirst !== false) {
                    // force out at 2nd, other runners advance (and possibly score)
                    OnFirst = false;
                    scores[0] = OnThird;
                    OnThird = OnSecond;
                    OnSecond = false;
                } else {
                    // no force at 2nd, get the lead runner
                    if (OnThird !== false) {
                        // no runner on first, but runner on 2nd advances
                        OnThird = OnSecond; // out going home.. runner on 2nd advances
                    } else {
                        if (OnSecond !== false) {
                            // should ONLY get here if only runner on 2nd base
                            OnSecond = false;
                        }
                    }
                }
            } else {
                // no runners or already 2 outs, just the batter is out
                play = "OUTS";
            }
        }

        if (play == "HFC" && Outs < 2 && RunnersOnBase > 0) {

            // ******************** FIELDER'S CHOICE (only matters if less than 2 out ) **************
            if (OnFirst !== false) {
                // go for easist play, runners advance. Batter safe at first
                scores[0] = OnThird;
                OnThird = OnSecond;
                OnSecond = false;
            } else {
                if (OnThird !== false) {
                    // get runner out at home, advance runner on 2nd (no runner on 1st) 
                    OnThird = OnSecond;
                } else {
                    if (OnSecond !== false) {
                        // get here if only runner is on 2nd
                        OnSecond = false;
                    }
                }
            }
            // regardless, batter gets to first
            OnFirst = batter;
        }

        if ((play == "SF" || play == "SH") && Outs < 2) {
            // advance all runners
            advanceAllOneBase(false, "SF");
        }
    }
    Outs++;
    evaluateBases();
    return scores;
}

executePlay = function (batter, play) {
    // first look for outs
    if (OutPlays.indexOf(play) >= 0) {
        // an out
        scores = calledOut(batter, play);
    } else {
        // gets on base
        // count bases
        if (OneBasePlays.indexOf(play) >= 0) {
            scores = advanceAllOneBase(batter, play);
        }

        if (play == "2B") {
            scores = advanceAllTwoBases(batter);
        }

        if (play == "3B") {
            scores = advanceAllThreeBases(batter);
        }

        if (play == "HR") {
            scores = scoreHR(batter);
        }
    }

    return scores;
}

// ********************************************************
//
// scoring functions
//
//
// *********************************************************
calculateRBI = function () {

}

// *********************************************************
// *********************************************************
//
// simulateOffense() main entry point
//
// *********************************************************
// *********************************************************


    // step one, unpack all the offensive events
    PAEvents = [];        // array of arrays for each batter: outcome of each PA: Out, 1b, 2b, 3b, etc.
    PAUsed = [];
    PAAvailable = [];
    OtherEvents = [];    // array of arrays for each batter: baserunning events: SB, CS, etc.
    RunBonus = [];      // array for each run scored in synch with a PA. 0 means unused, 1 means used with Run
    RBIBonus = [];      // array for each RBI in synch with a PA. 0 means unused, 1 means used with RBI



    for (var b = 0; b < lineup.length; b++) {
        var player = lineup[b];
       // var stats = roster[player.RosterIndex[0]].Batting.daily[0];

       if( player ) {
        var indexArray = player.RosterIndex;
        
                    // get the actual stats from the roster
        var fullstats = roster[indexArray[0]];
        var dayToUse = indexArray[1];
        var stats = fullstats.Batting.daily[dayToUse];

        var pas = player.PA;
        var paIndex;

        // create arrays for each array for this player
        PAEvents[b] = [];
        PAUsed[b] = [];
        PAAvailable[b] = pas;
        OtherEvents[b] = [];

        // make arrays for PA events and Run/RBI bonuses
        for (var playtype = 0; playtype < PAPlays.length; playtype++) {
            var pt = PAPlays[playtype];
            insertPlays(b, pt, stats[pt]);
        }

        // make array for special events: right now, just sb and cs
        var sb = stats["SB"];
        if (sb > 0) insertSpecialPlays(b, "SB", sb);
        var cs = stats["CS"];
        if (cs > 0) insertSpecialPlays(b, "CS", cs);

        // now, substitute special types of hits and outs!

        var length = PAEvents[b].length;
        for (var playtype = 0; playtype < SpecialPlays.length; playtype++) {
            var newPlay = SpecialPlays[playtype][0];
            var oldPlay = SpecialPlays[playtype][1];
            if (stats[newPlay] > 0)
                substituteSpecialPlays(b, newPlay, oldPlay, stats[newPlay]);
        }

        // lengthen array for tracking rbis and runs
        RunBonus.push(0);
        RBIBonus.push(0);
    }
    }

    // ********
    // step 2: run the inning-by-inning simulator
    // *************************************************
    // for game play tracking
    RunsScored = RBIScored = 0;
    var nextBatter = 0;

    for (var Inning = 1; Inning < 10; Inning++) {

        console.log(">>>>> Inning " + Inning );
        // start the inning.. no one on, no one out
        Outs = 0;
        OnFirst = OnSecond = OnThird = RunnersOnBase = RunnersInScoringPosition = false;
        var playCount = 0;

        while (Outs < 3) {
            var scores;
            if (PAAvailable[nextBatter] == 0) {
                // this batter is out
                scores = calledOut(nextBatter, "OUTS");
            } else {
                var nextPlay = false;
                var paIndex = 0;
                if (PAUsed[nextBatter]) {
                    for (var i = 0; i < PAUsed[nextBatter].length; i++) {

                        if (PAUsed[nextBatter][i] == false) {
                            nextPlay = PAEvents[nextBatter][i];
                            paIndex = i;
                            break;
                        }
                    } 

                    if (nextPlay == false)
                        nextPlay = "OUTS";

                    // have the play, move him forward
                    scores = executePlay(nextBatter, nextPlay);    

                    calculateRBI( nextBatter, nextPlay, scores);


                    // record the PA
                    PAUsed[nextBatter][paIndex] = true;
                    PAAvailable[nextBatter]--;
                }
            }

            playCount++;

            runsscored = 0;
            output = "";
            for (s = 0; s < scores.length; s++) {
                if (scores[s] !== false) {
                    if (output == "") output = " scoring: ";

                    runsscored++;
                    RunsScored++;
                    lineupIndex = scores[s];
                    output += lineup[lineupIndex].name + ", ";
                    if (s != 3) {
                        // then not a Homerun scoring the batter
                        RunBonus[lineupIndex]++;
                    }
                    RBIBonus[nextBatter]++;
                }
            }

            // ******(((((((((((( TODO: fix this))))))))))))
           // console.log("    Play: " + playCount + " - " + roster[lineup[nextBatter].RosterIndex[0]].FullName + ": " +nextPlay  + output);
            
            // finally advance to the next batter
            nextBatter++;
            if( nextBatter >= lineup.length )
                nextBatter = 0;
 
        }
    }
    console.log("TOTAL RUNS SCORED: " + RunsScored );
    
    // ****** step 3: record all the run and rbi values
    for ( b = 0; b < lineup.length; b++) {
        var player = lineup[b];
        if( player ) {
        var avgame = player.RosterIndex[1];
        var stats = roster[player.RosterIndex[0]].Batting.daily[avgame];
        stats.RSRuns = RunBonus[b];
        stats.RSRBI = RBIBonus[b];
        }
    }

  }
}