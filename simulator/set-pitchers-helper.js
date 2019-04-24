// **********************************************************
//
// File: set-pitchers-helper.js 
// RFB Simulator: functions for setting the starting pitcher, relievers, closer, and bullpen
// By: Eddie Dombrower and Adam Feldman
//
// **********************************************************

module.exports = {

    setPitchers: function (game, team, roster, realdata, isHome, Override) {

        function calculateOuts(stats) {
            var ip = stats.IP;
            var innings = Math.floor(ip);
            var outs = Math.floor(((ip - innings) + 0.0001) * 10) + (innings * 3);
            stats["OUT"] = outs;

        }

        function getOutsFromIP(ip) {
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
        }

        function getIPFromOuts(outs) {
            var fractOuts = outs / 3;
            var whole = Math.floor(fractOuts);
            var ip = whole + (fractOuts % 1 == 0 ? 0 : (fractOuts % 1 > 0.5 ? 0.2 : 0.1));
            return (ip);
        }

        function calculatePitchingLW(stats, isPitchingOn3DaysRest, coaches) {
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
        }

        function calculateFieldingLW(stats) {
            var lw = 0;
            if (typeof (stats) == "undefined" || !stats || !stats.Pos) {
                lw = 0;
                if (stats) {
                    stats.FieldLW = lw;
                }
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
                value = alw.value[0];
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

            if (fstats && fstats.Pos) {

                // catchers are treated differently.
                if (fstats.Pos == 2) {
                    /*
                    Block =(CBlockingRuns)/(INN/9)
                    Frame =(CFramingRuns)/(INN/9)
                    */

                    // ((((((((((((((((( TODO: PULL FROM THIS PLAYER'S STATS )))))))))))))))))
                    var innings = 1069; // buster posey in oct 2016
                    var innPerGame = innings / 9;
                    var cBlockingRuns = 5;
                    var cFramingRuns = 11;

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
                        if (r < 8) {
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
                        if (r < 8) {
                            r = 1;
                        } else {
                            r = 2;
                        }
                    }
                    r = 0;
                    MissedBalls = r;

                    if (OutsOutOfZone > 0) {
                        // then rate based on these zvals
                        zvals = algorithms.Fielding.OutOfZone;
                    } else {
                        // no outs out of zone.. look just at misses
                        zvals = algorithms.Fielding.NoOutOfZone;
                    }
                    var cardinalPosition = 0;
                    if (fstats.Pos)
                        cardinalPosition = fstats.Pos - 1;
                    var value1 = zvals.value[cardinalPosition][0];
                    var value2 = zvals.value[cardinalPosition][1];
                    zone = (value1 * OutsOutOfZone) + (value2 * MissedBalls);

                    // replace old zone with new zone
                    fstats.Zone = zone;

                    // fielders don't get these stats
                    fstats.Block = "";
                    fstats.Frame = "";
                    fstats.cERA = "";

                }

            } else {
                if( fstats ) {
                    fstats.Zone = 0;

                    // fielders don't get these stats
                    fstats.Block = "";
                    fstats.Frame = "";
                    fstats.cERA = "";
                   
                }
            }
        }

        function getOutsArrayMajor(nextPitcher) {
            // create an array of Major league games that have whatever outs are available
            var outsArray = new Array(27);
            for (var gd in nextPitcher.eStats) {
                if (gd.charAt(0) == "2" && !gd.includes("Minors")) {
                    if (nextPitcher.eStats[gd].Pitching) {
                        ip = nextPitcher.eStats[gd].Pitching.IP;
                        outs = getOutsFromIP(ip);
                        if (outsArray[outs])
                            outsArray[outs].push(gd);
                        else
                            outsArray[outs] = [gd];
                    }
                }
            }
            // now sort each sub-array, newest gamedate to oldest gamedate
            for (var i = 0; i <= 27; i++) {
                if (outsArray[i]) {
                    outsArray[i].sort();
                    outsArray[i].reverse();
                }
            }
            return outsArray;
        }

        function getOutsArrayMinor(nextPitcher) {
            // create an array of Minor league games that have whatever outs are available
            var outsArrayMinor = new Array(27);
            for (var gd in nextPitcher.eStats) {
                if (gd.charAt(0) == "2" && gd.includes("Minors")) {
                    if (nextPitcher.eStats[gd].Pitching) {
                        ip = nextPitcher.eStats[gd].Pitching.IP;
                        outs = getOutsFromIP(ip);
                        if (outsArrayMinor[outs])
                            outsArrayMinor[outs].push(gd);
                        else
                            outsArrayMinor[outs] = [gd];
                    }
                }
            }
            // now sort each sub-array, newest gamedate to oldest gamedate
            for (var i = 0; i <= 27; i++) {
                if (outsArrayMinor[i]) {
                    outsArrayMinor[i].sort();
                    outsArrayMinor[i].reverse();
                }
            }
            return outsArrayMinor;
        }

        class pitcher {
            constructor(player, stats, fstats, pitcherListIndex, firstAvailableGameDay, calendarDate, type) {
                this.FullName = player.FullName;
                this.MlbId = player.MlbId;
                this.PlayerId = player.PlayerId;
                this.RosterIndex = [pitcherListIndex, firstAvailableGameDay];
                this.IsPitchingOn3DaysRest = false;
                if( player.IsPitchingOn3DaysRest && player.IsPitchingOn3DaysRest == true)
                    this.IsPitchingOn3DaysRest = true;
                this.Pos = type;
                this.CanStart = false;
                this.CanRelieve = false;

                this.W = 0;
                this.L = 0;
                this.SV = 0;
                this.Hold = 0;
                this.CalendarDay = calendarDate;
                if (type == "SP" || type == "P") {
                    this.CanStart = player.CanStart;
                }
                if (type == "RP" || type == "CL") {
                    this.CanRelieve = player.CanRelieve;
                }
                this.OUT = stats.Pitching.OUT;
                this.pStats = stats;

                this.eStats = {};
                if( type != "SP" && player.eStats ) {
                    this.eStats = player.eStats;
                    if( player.PartialGames) {
                        this.PartialGames = player.PartialGames;
                    }
                    if( player.OutsPerGame) {
                        this.OutsPerGame = player.OutsPerGame;
                    }
                }
                this.sourceLevel = "Majors";
                if( stats.Level )
                    this.sourceLevel = stats.Level;

                if (fstats) {
                    calculateDefensiveStats(fstats);
                    calculateFieldingLW(fstats);
                    this.fStats = fstats;
                }
                this.outsArray = [];
                if (player.eStats)
                    this.outsArray = getOutsArrayMajor(player);
                this.outsArrayMinor = [];
                if (player.eStats)
                    this.outsArrayMinor = getOutsArrayMinor(player);
            }
        }


        var minstatvalue = algorithms.Pitching.MinStarterStat.value;
        var minstattype = algorithms.Pitching.MinStarterStat.stat;


        var pitchingStaff = {
            pitchers: [],
            pitchersUsed: [],
            unusablePitchers: [],
            starter: null,
            closer: null,
            relievers: [],
            completeGame: false,
            override: false
        }

        var players = roster;
        var calendarDate = game.CalendarDay;

        if (realdata == false) {

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
                                if ((spOuts < (5 * 3)) && stats.OUT > spOuts) {
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
                                pitchingStaff.closer = new Array([i, av]);

                                // don't put the closer in unless it's a save situation.
                                //  pitchingStaff.pitchers.push(new pitcher(stats, i, av, "CL"));
                            } else {
                                // just in the bullpen
                                pitchingStaff.pitchers.push(new pitcher(stats, i, av, "RP"));
                            }

                        }
                    }
                }
            }

            // **** assume we have a starter.. put him in the lineup
            if (spIndex != null) {
                calculatePitchingLW(spStats, null, team.extra.Coordinators);
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

            // Override?
            if (Override) {
                /*
                overrideQuery = {
                    vPitchers : $scope.vPitching,
                    vPitchersIP : $scope.vPitchSubIP,
                    hPitchers : $scope.hPitching,
                    hPitchersIP : $scope.hPitchSubIP
                }
                */
                var oPitchers = Override.vPitchers;
                var oIP = Override.vPitchersIP;
                var boxIP = Override.vBoxIP;
                if (isHome) {
                    oPitchers = Override.hPitchers;
                    oIP = Override.hPitchersIP;
                    boxIP = Override.hBoxIP;
                }
                if (oIP && oPitchers) {
                    // then we're working on the selected team and there are pitchers in the array.

                    // double check..
                    /*
                    var doOverride = false;
                    for (var i = 0; i < oIP.length; i++) {
                        if (oIP[i] && oIP[i] != "") {
                            doOverride = true;
                            break;              // found at least one.
                        }
                    }
                    */
                    var doOverride = true;
                    pitchingStaff.override = true;
                    if (doOverride) {
                        // ok.. cycle through and put the pitchers into the pitching staff...

                        for (var i = 0; i < oPitchers.length; i++) {
                            if (oPitchers[i]) {
                                var nextPitcher = oPitchers[i];
                                // is he alread in a pitcher class?
                                var outsToGet = -1;
                                if (oIP[i]) {
                                    var ip = Number(oIP[i]);
                                    var outsToGet = (Math.floor(ip) * 3) + Math.floor((ip % 1) * 10);
                                }
                                if ( (nextPitcher.OUT && !oIP[i]) || (nextPitcher.OUT && nextPitcher.OUT==outsToGet )){
                                    // yes, just put him in the pitching staff
                                    pitchingStaff.pitchersUsed.push(nextPitcher);

                                    // however, if he's the starter, we need to actually throw him in the batting order.

                                    if (i == 0) {

                                        // find his real starter stats
                                        for (var p = 0; p < players[8].Players.length; p++) {
                                            if (players[8].Players[p] && nextPitcher.PlayerId == players[8].Players[p].PlayerId) {
                                                nextPitcher = players[8].Players[p];
                                                break;
                                            }
                                        }

                                        if( !nextPitcher.FirstFullPitchingGame && nextPitcher.highestIP ) {
                                            nextPitcher.FirstFullPitchingGame = nextPitcher.highestIP;
                                        } else if( !nextPitcher.FirstFullPitchingGame && nextPitcher.highestIPMinors) {
                                            nextPitcher.FirstFullPitchingGame = nextPitcher.highestIPMinors;
                                        } else {
                                            if( nextPitcher.RosterIndex) {
                                                nextPitcher.FirstFullPitchingGame = nextPitcher.RosterIndex[1];
                                            }
                                        }

                                        if (nextPitcher.FirstFullPitchingGame && nextPitcher.FirstFullPitchingGame != "") {
                                            // just assume so and rifle through the batters.
                                            for (var bo = 0; bo < roster.battingOrder.length; bo++) {
                                                if (!roster.battingOrder[bo] || (roster.battingOrder[bo] && roster.battingOrder[bo].Position == "P")) {
                                                    // then found a slot!
                                                    roster.battingOrder[bo] = nextPitcher;
                                                    roster.battingOrder[bo].Position = "P";
                                                    break;
                                                }
                                            }
                                        }

                                        function isEmpty(obj) {
                                            for(var key in obj) {
                                                if(obj.hasOwnProperty(key))
                                                    return false;
                                            }
                                            return true;
                                        }
                                    
                                        if( isEmpty(nextPitcher.eStats)) {
                                            for( var ps=0; ps<players[8].Players.length; ps++) {
                                                  if( players[8].Players[ps].PlayerId == nextPitcher.PlayerId) {
                                                      nextPitcher.eStats = players[8].Players[ps].eStats;
                                                      nextPitcher.Primary = players[8].Players[ps].Primary;
                                                      nextPitcher.Secondary = players[8].Players[ps].Secondary;
                                                      nextPitcher.Tertiary = players[8].Players[ps].Tertiary;
                                                      break;
                                                  }
                                              }
                                          }
                                          if( isEmpty(nextPitcher.eStats)) {
                                            for( var ps=0; ps<players[9].Players.length; ps++) {
                                                  if( players[9].Players[ps].PlayerId == nextPitcher.PlayerId) {
                                                      nextPitcher.eStats = players[9].Players[ps].eStats;
                                                      nextPitcher.Primary = players[9].Players[ps].Primary;
                                                      nextPitcher.Secondary = players[9].Players[ps].Secondary;
                                                      nextPitcher.Tertiary = players[9].Players[ps].Tertiary;
                                                      break;
                                                  }
                                              }
                                          }
                                          if( isEmpty(nextPitcher.eStats)) {
                                            for( var ps=0; ps<players[10].Players.length; ps++) {
                                                  if( players[10].Players[ps].PlayerId == nextPitcher.PlayerId) {
                                                      nextPitcher.eStats = players[10].Players[ps].eStats;
                                                      nextPitcher.Primary = players[10].Players[ps].Primary;
                                                      nextPitcher.Secondary = players[10].Players[ps].Secondary;
                                                      nextPitcher.Tertiary = players[10].Players[ps].Tertiary;
                                                      break;
                                                  }
                                              }
                                          }                                        
                                        if( isEmpty(nextPitcher.eStats)) {
                                          for( var ps=0; ps<players[12].Players.length; ps++) {
                                                if( players[12].Players[ps].PlayerId == nextPitcher.PlayerId) {
                                                    nextPitcher.eStats = players[12].Players[ps].eStats;
                                                    nextPitcher.Primary = players[12].Players[ps].Primary;
                                                    nextPitcher.Secondary = players[12].Players[ps].Secondary;
                                                    nextPitcher.Tertiary = players[12].Players[ps].Tertiary;
                                                    break;
                                                }
                                            }
                                        }
                                        var stats = nextPitcher.eStats[nextPitcher.FirstFullPitchingGame].Pitching;
                                        calculatePitchingLW(stats, null, team.extra.Coordinators);   // just a best guess until game time
                                        pitchingStaff.starterStats = nextPitcher;
                                    }
                                } else {

                                    // either real pitcher data (a new pitcher) and/or he has a request for new outs to pitch

                                    var gameDates = [];

                                    if (i == 0 && nextPitcher.FirstFullPitchingGame && nextPitcher.FirstFullPitchingGame != "") {
                                        // it's the pitcher, drop him in totally...
                                        gameDates[0] = nextPitcher.FirstFullPitchingGame;

                                        // AND, if it's a DH game, we need to put into the batting order...
                                        // just assume so and rifle through the batters.
                                        for (var bo = 0; bo < roster.battingOrder.length; bo++) {
                                            if (roster.battingOrder[bo] && roster.battingOrder[bo].Position == "P") {
                                               // then found a slot!
                                                roster.battingOrder[bo] = nextPitcher;
                                                roster.battingOrder[bo].Position = "P";
                                                break;
                                            }
                                        }

                                    } else {

                                        // need to create this pitcher's data and put it into the pitcher class first....
                                        var ip = Number(oIP[i]);
                                        if( isNaN( ip )) {
                                            var targetOuts = 3; // default
                                            if( boxIP[i] )
                                                targetOuts = boxIP[i];

                                        } else {
                                            var targetOuts = (Math.floor(ip) * 3) + Math.floor((ip % 1) * 10);
                                            targetOuts = getOutsFromIP(ip);
                                        }

                                        if (targetOuts == 0)
                                            targetOuts = 1;

                                        if (!nextPitcher.eStats) {
                                            // need to replace with the real pitcher stats...
                                            var foundNext = false;
                                            for (p = 0; p < players[8].Players.length; p++) {
                                                if (players[8].Players[p] && nextPitcher.PlayerId == players[8].Players[p].PlayerId) {
                                                    nextPitcher = players[8].Players[p];
                                                    foundNext = true;
                                                    break;
                                                }
                                            }
                                            if (!foundNext) {
                                                for (p = 0; p < players[9].Players.length; p++) {
                                                    if (players[9].Players[p] && nextPitcher.PlayerId == players[9].Players[p].PlayerId) {
                                                        nextPitcher = players[9].Players[p];
                                                        foundNext = true;
                                                        break;
                                                    }
                                                }
                                            }
                                            if (!foundNext) {
                                                for (p = 0; p < players[10].Players.length; p++) {
                                                    if (players[10].Players[p] && nextPitcher.PlayerId == players[10].Players[p].PlayerId) {
                                                        nextPitcher = players[10].Players[p];
                                                        foundNext = true;
                                                        break;
                                                    }
                                                }
                                            }
                                        }

                                        // first look for a day with the closest to that many outs without going over..

                                        var foundOuts = 0;
                                        /*
                                        for (var outs = targetOuts; outs > 0; outs--) {
                                            if (nextPitcher.OutsPerGame[outs]) {
                                                gameDates.push(nextPitcher.OutsPerGame[outs]);
                                                foundOuts += outs;
    
                                                if (foundOuts >= targetOuts) {
                                                    break;
                                                }
                                            }
                                        }
                                        if (foundOuts < targetOuts) {
                                            for (var outs = targetOuts + 1; outs < nextPitcher.OutsPerGame.length; outs++) {
                                                if (nextPitcher.OutsPerGame[outs]) {
    
                                                    for (var go = 0; go < nextPitcher.OutsPerGame[outs].length; go++) {
                                                        gameDates.push(nextPitcher.OutsPerGame[outs]);
                                                        foundOuts += outs;
                                                        if (foundOuts >= targetOuts) {
                                                            break;
                                                        }
                                                    }
                                                    if (foundOuts >= targetOuts) {
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                        */

                                        // ********((((((((((( TODO INCLUDE USED GAMES/PARITAL GAMES)))))))))))

                                        // create an array of Major league games that have whatever outs are available
                                        var outsArray = new Array(27);
                                        for (var gd in nextPitcher.eStats) {
                                            if (gd.charAt(0) == "2" && !gd.includes("Minors")) {
                                                if (nextPitcher.eStats[gd].Pitching) {
                                                    ip = nextPitcher.eStats[gd].Pitching.IP;
                                                    outs = getOutsFromIP(ip);
                                                    if (outsArray[outs])
                                                        outsArray[outs].push(gd);
                                                    else
                                                        outsArray[outs] = [gd];
                                                }
                                            }
                                        }

                                        // // create an array of Minor league games that have whatever outs are available
                                        // var outsArrayMinor = new Array(27);
                                        // for (var gd in nextPitcher.eStats) {
                                        //     if (gd.charAt(0) == "2" && gd.includes("Minors")) {
                                        //         if (nextPitcher.eStats[gd].Pitching) {
                                        //             ip = nextPitcher.eStats[gd].Pitching.IP;
                                        //             outs = getOutsFromIP(ip);
                                        //             if (outsArrayMinor[outs])
                                        //                 outsArrayMinor[outs].push(gd);
                                        //             else
                                        //                 outsArrayMinor[outs] = [gd];
                                        //         }
                                        //     }
                                        // }
                                        
                                        // look for a game or games that add up to, or over the target.
                                        // assumes the admin knows what he/she is doing.
                                        var foundOuts = 0;
                                        if (outsArray[targetOuts] && outsArray[targetOuts].length > 0) {
                                            gameDates[0] = outsArray[targetOuts][0];
                                            foundOuts = targetOuts;
                                            // and done.
                                        } else {
                                            // need to build a game that has that many outs...

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

                                            if (foundOuts < targetOuts) {
                                                for (var fo = targetOuts; fo < 28; fo++) {
                                                    if (outsArray[fo] && outsArray[fo].length > 0) {
                                                        foundOuts += fo;
                                                        // this will be a partial!
                                                        gameDates.push(outsArray[fo][0]);
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    // Store outs values, for use in Pitcher class
                                    // nextPitcher.outsArray = outsArray;
                                    // nextPitcher.outsArrayMinor = outsArrayMinor;

                                    // default, assume we found the exact outs in one game
                                    var eStats = nextPitcher.eStats[gameDates[0]];
                                    var stats = eStats.Pitching;
                                    if( eStats.Pitching.Level )
                                        stats.Level = eStats.Pitching.Level;
                                        
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

                                            var availOuts = getOutsFromIP(nextPitcher.eStats[gameDates[g]].Pitching.IP);

                                            // assume more outs than needed...
                                            var partialOuts = targetOuts - outsUsed;
                                            var ratio = partialOuts / availOuts;

                                            if (availOuts < partialOuts) {
                                                // found less outs than needed
                                                partialOuts = availOuts;
                                                ratio = 1.0;    //use them all.

                                            }
                                            outsUsed = partialOuts;
                                            multiPitchGame.push({
                                                GameDate: gameDates[g],
                                                OutsAvailable: availOuts,
                                                OutsUsed: partialOuts,
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
                                            stats.IP = getIPFromOuts(targetOuts);

                                        }
                                        eStats.Pitching = stats;
                                    }

                                    // make the pitcher here...
                                    if (stats) {
                                        calculateOuts(stats);
                                        var position = "RP";
                                        if (i == 0)
                                            position = "SP";

                                        calculatePitchingLW(stats, null, team.extra.Coordinators);   // just a best guess until game time
                                        pitchingStaff.pitchersUsed.push(new pitcher(nextPitcher, eStats, fstats, i, gameDates[0], calendarDate, position));

                                        if (i == 0) {
                                            pitchingStaff.starter = new pitcher(nextPitcher, eStats, fstats, i, gameDates[0], calendarDate, position);
                                            pitchingStaff.starterStats = nextPitcher;
                                        }
                                    }



                                }
                            }
                        }
                    }
                    team.pitchingStaff = pitchingStaff;
                    return;
                }
            } // end of override

            // no override, pick the pitchers based on the user's wishes here...

            /// data arrives in the DepthChart as players
            //  players[8] are ranked starters
            // players[9] are ranked relievers
            // players[10] if it has a player, is THE closer.. any beyond that, and we stick them in with the relief pitchers!

            // start with the starters....
            // **** data for sorting the best starter:
            var spIndex = null;
            var spSubIndex = null;
            var spOuts = 0;
            var spFStats = null;
            var spStats = null;
            var spPlayer = null;
            var unusablePitchers = [];
            var relieverToStart = null;

            // make sure all the pitchers are in the depth array somewhere!
            isProd = false;         // *** asked to temporarily remove this from production.
            var pitcherIds = [];
            var player12Index = [];
            if (!isProd && players[12]) {
                for (var i = 0; i < players[12].Players.length; i++) {
                    if (players[12].Players[i].Primary && players[12].Players[i].Primary.indexOf("P") >= 0) {
                        // then a pitcher.

                        if (pitcherIds.indexOf(players[12].Players[i].PlayerId) == -1) {
                            pitcherIds.push(players[12].Players[i].PlayerId);
                            player12Index.push(i);
                        }

                    }
                }
            }

            // now see if all the pitchers are accounted for in the depth chart.
            // starters..
            for( var i=0; i<players[8].Players.length; i++ ) {
                var isPitcher = pitcherIds.indexOf( players[8].Players[i].PlayerId);
                if( isPitcher >= 0 ) {
                    pitcherIds[isPitcher] = "na";
                    player12Index[isPitcher] = -1;
                }
            }
            // relievers..
            for (var i = 0; i < players[9].Players.length; i++) {
                var isPitcher = pitcherIds.indexOf(players[9].Players[i].PlayerId);
                if (isPitcher >= 0) {
                    pitcherIds[isPitcher] = "na";
                    player12Index[isPitcher] = -1;
                }
            }
            // closers..
            for (var i = 0; i < players[10].Players.length; i++) {
                var isPitcher = pitcherIds.indexOf(players[10].Players[i].PlayerId);
                if (isPitcher >= 0) {
                    pitcherIds[isPitcher] = "na";
                    player12Index[isPitcher] = -1;
                }
            }

            // now move all unassigned pitchers into the relief depth area
            for( var i=0; i<pitcherIds.length; i++ ) {
                if( pitcherIds[i] != "na") {
                    if( players[12].Players[player12Index[i]].FirstFullPitchingGame == "" ) {
                        players[9].Players.push( players[12].Players[player12Index[i]])
                    } else {
                        players[8].Players.push( players[12].Players[player12Index[i]])                     
                    }
                }
            }

            // well, not quite ready.. see if the 3 day rest rule needs to be invoked....
            if( !team.extra || !team.extra.Allow3DaysRest) {
                team.extra = {Allow3DaysRest: true};
            }
            var legalStarter = false;
            var foundLegalStarter = null;
            for (let p = 0; p < players[8].Players.length; p++) {
                var nextPitcher = players[8].Players[p];
                if( nextPitcher.CanStart && (nextPitcher.FirstFullPitchingGame != "" || nextPitcher.highestIP)) {
                    // don't bother, we have a starter
                    legalStarter = true;
                    foundLegalStarter = nextPitcher;
                    break;
                }
                if( (nextPitcher.highestIPMinors && nextPitcher.highestIPMinors != "")) {
                    // see if this pitcher has enough rest... 4 days!
                    var daysRested = game.CalendarDay;
                    if (nextPitcher.LastDayPlayed) {
                        daysRested = game.CalendarDay - Math.floor(nextPitcher.LastDayPlayed); // in case of double header
                    }
                    if (daysRested >= 4) {
                        legalStarter = true;
                        foundLegalStarter = nextPitcher;
                        break;
                    }
                }
            }

            if( legalStarter==false  && team.extra && team.extra.Allow3DaysRest && team.extra.Allow3DaysRest == true) {
                var legalStarter = false;
                for (let p = 0; p < players[8].Players.length; p++) {
                    var nextPitcher = players[8].Players[p];
                    if( nextPitcher.CanStart && (nextPitcher.FirstFullPitchingGame != "" || nextPitcher.highestIP)) {
                        // don't bother, we have a starter
                        legalStarter = true;
                        break;
                    }
                }
                if( !legalStarter ) {
                    for (let p = 0; p < players[8].Players.length; p++) {
                        var nextPitcher = players[8].Players[p];
                        var daysRested = game.CalendarDay;
                        if (nextPitcher.LastDayPlayed) {
                            daysRested = game.CalendarDay - Math.floor(nextPitcher.LastDayPlayed); // in case of double header
                        }
                        // var daysRested = game.CalendarDay - Math.floor(nextPitcher.LastDayPlayed); // in case of double header
                        if( daysRested >= 4) {
                            nextPitcher.CanStart = true;
                            nextPitcher.IsPitchingOn3DaysRest = true;
                            if( ! nextPitcher.FirstFullPitchingGame) {
                                if( nextPitcher.highestIP ) {
                                    nextPitcher.FirstFullPitchingGame = nextPitcher.highestIP;
                                } else if( nextPitcher.highestIPMinors ) {
                                    nextPitcher.FirstFullPitchingGame = nextPitcher.highestIPMinors;
                                }
                            }
                            break;
                        }
                    }
                }
            }

            // and even MORE rule adjustments for minor leaguer starters and relievers...
            for (let p = 0; p < players[8].Players.length; p++) {
                var nextPitcher = players[8].Players[p];
                if (nextPitcher.CanStart == false && nextPitcher.highestIPMinors && nextPitcher.highestIPMinors != "") {
                    nextPitcher.CanStart = true;
                    nextPitcher.FirstFullPitchingGame = nextPitcher.highestIPMinors;
                }
            }
            for (let p = 0; p < players[9].Players.length; p++) {
                var nextPitcher = players[9].Players[p];
                if (nextPitcher.CanRelieve == false && nextPitcher.highestIPMinors && nextPitcher.highestIPMinors != "") {
                    nextPitcher.CanRelieve = true;
                    
                }
            }
            for (let p = 0; p < players[10].Players.length; p++) {
                var nextPitcher = players[10].Players[p];
                if (nextPitcher.Relieve == false && nextPitcher.highestIPMinors && nextPitcher.highestIPMinors != "") {
                    nextPitcher.Relieve = true;
                }
            }

            // ok.. find a starter now.. start with the starters ptype=8, then the relievers, then the closer
            for (let ptype = 8; ptype <= 10; ptype++) {
                for (let p = 0; p < players[ptype].Players.length; p++) {
                    var nextPitcher = players[ptype].Players[p];

                    if (!spPlayer && ((foundLegalStarter && nextPitcher.PlayerId == foundLegalStarter.PlayerId) || !foundLegalStarter) && nextPitcher.CanStart && (nextPitcher.FirstFullPitchingGame != "" || nextPitcher.highestIP)) {
                        // found a starter...
                        if (spPlayer == null && (nextPitcher.FirstFullPitchingGame && nextPitcher.eStats[nextPitcher.FirstFullPitchingGame] && nextPitcher.eStats[nextPitcher.FirstFullPitchingGame].Pitching)
                            || (nextPitcher.highestIP && nextPitcher.eStats[nextPitcher.highestIP] && nextPitcher.eStats[nextPitcher.highestIP].Pitching)) {

                            var eStats;
                            if (nextPitcher.FirstFullPitchingGame /* && nextPitcher.eStats[nextPitcher.FirstFullPitchingGame].Pitching.GS != "0" */) {
                                eStats = nextPitcher.eStats[nextPitcher.FirstFullPitchingGame];
                            } else if (nextPitcher.highestIP && nextPitcher.eStats[nextPitcher.highestIP].Pitching.GS != "0") {
                                eStats = nextPitcher.eStats[nextPitcher.highestIP];
                            }


                            if (eStats && eStats.Pitching) {

                                var stats = eStats.Pitching;
                                var fstats = {};
                                if (eStats.Fielding) {
                                    fstats = eStats.Fielding;
                                }
                                calculateOuts(stats);
                                // first option available.. grab him
                                spIndex = p;
                                spGameDay = nextPitcher.FirstFullPitchingGame;
                                spStats = eStats;
                                spDayStats = stats;
                                spFStats = fstats;
                                spOuts = stats["OUT"]; // not used.
                                spPlayer = nextPitcher;
                            } else {
                                unusablePitchers.push(nextPitcher);
                            }
                        } else {
                            unusablePitchers.push(nextPitcher);
                        }
                    } else {
                        // there isn't a reliever already as a starter
                        if (!relieverToStart) {
                            if (!spPlayer && nextPitcher.CanRelieve && (nextPitcher.FirstFullPitchingGame != "" || nextPitcher.highestIP)) {
                                relieverToStart = nextPitcher;
                            } else {
                                unusablePitchers.push(nextPitcher);
                            }
                        }
                    }
                }
                if (spPlayer || relieverToStart)
                    break;

            }


            // 
            //
            //  A BIT OF DESPARATION.. if there is not a proper starting pitcher. see if there's a reliever in the rotation...
            //
            //
            if (!spPlayer && relieverToStart) {
                // drop him in now!
                var nextPitcher = relieverToStart;
                spPlayer = relieverToStart;
                /*
                spGameDay = nextPitcher.FirstFullPitchingGame;
                spStats = eStats;
                spDayStats = stats;
                spFStats = fstats;
                spOuts = stats["OUT"]; // not used.
                */

                // need to create this pitcher's data and put it into the pitcher class first....
                var ip = 9;
                var targetOuts = (Math.floor(ip) * 3) + Math.floor((ip % 1) * 10);
                targetOuts = getOutsFromIP(ip);
                if (targetOuts == 0)
                    targetOuts = 1;

                // ********((((((((((( TODO INCLUDE USED GAMES/PARITAL GAMES)))))))))))

                // create an array of games that have whatever outs are available
                var outsArray = new Array(27);
                for (var gd in nextPitcher.eStats) {
                    if (nextPitcher.eStats[gd].Pitching) {
                        ip = nextPitcher.eStats[gd].Pitching.IP;
                        outs = getOutsFromIP(ip);
                        if (outsArray[outs])
                            outsArray[outs].push(gd);
                        else
                            outsArray[outs] = [gd];
                    }
                }

                // look for the highest number of innings in the majors (if there are majors)
                // if not majors, find the highest in the minors...
                // assumes the admin knows what he/she is doing.
                var foundOuts = 0;
                if (outsArray[targetOuts] && outsArray[targetOuts].length > 0) {
                    gameDates[0] = outsArray[targetOuts][0];
                    foundOuts = targetOuts;
                    // and done.
                } else {
                    // need to pick a game with the most number of outs.
                    var gameDates = [];

                    var firstMajorsGame = null;
                    var firstMinorsGame = null;
                    for (var fo = 27; fo > 0; fo--) {
                        if (outsArray[fo] && outsArray[fo].length > 0) {
                            for (go = 0; go < outsArray[fo].length; go++) {
                                if (!outsArray[fo][go].includes("Minors")) {
                                    // then found the first major league game he can play.
                                    firstMajorsGame = outsArray[fo][go];
                                    gameDates[0] = firstMajorsGame;
                                    break;
                                } else {
                                    if( firstMinorsGame == null) {
                                        firstMinorsGame = outsArray[fo][go];
                                    }
                                }
                            }
                            if( firstMajorsGame )
                                break;
                        }
                        if( firstMajorsGame )
                            break;
                    }
                }

                if( !firstMajorsGame ) {
                    gameDates[0] = firstMinorsGame;
                }
                // default, assume we found some outs!
                var eStats = nextPitcher.eStats[gameDates[0]];
                var stats = eStats.Pitching;
                var fstats = {};
                if (eStats && eStats.Fielding) {
                    fstats = eStats.Fielding;
                }

                if (gameDates.length > 1) {


                    eStats = { Pitching: null, Fielding: null };
                    stats = null;
                    fstats = null;

                    var multiPitchGame = [];
                    var outsUsed = 0;
                    for (var g = 0; g < gameDates.length; g++) {

                        var availOuts = getOutsFromIP(nextPitcher.eStats[gameDates[g]].Pitching.IP);

                        // assume more outs than needed...
                        var partialOuts = targetOuts - outsUsed;
                        var ratio = partialOuts / availOuts;

                        if (availOuts < partialOuts) {
                            // found less outs than needed
                            partialOuts = availOuts;
                            ratio = 1.0;    //use them all.

                        }
                        outsUsed = partialOuts;
                        multiPitchGame.push({
                            GameDate: gameDates[g],
                            OutsAvailable: availOuts,
                            OutsUsed: partialOuts,
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
                        var gameDate = gameDates[g];

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
                            ns.OutsAvailable = availOuts;
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
                        stats.IP = getIPFromOuts(stats.OutsAvailable);

                    }
                    eStats.Pitching = stats;
                }

                stats.Level = "ML";
                if( eStats.Batting && eStats.Batting.Level ) {
                    stats.Level = eStats.Batting.Level;
                } else if( gameDate && nextPitcher.eStats[gameDate] && nextPitcher.eStats[gameDate].Batting && nextPitcher.eStats[gameDate].Batting.Level ) {
                    stats.Level = nextPitcher.eStats[gameDate].Batting.Level;
                }
                // make the pitcher here...
                if (stats) {
                    calculateOuts(stats);
                    var position = "SP";

                    calculatePitchingLW(stats, null, team.extra.Coordinators);   // just a best guess until game time
                    pitchingStaff.pitchersUsed.push(new pitcher(nextPitcher, eStats, fstats, i, gameDates[0], calendarDate, position));
                    pitchingStaff.starter = new pitcher(nextPitcher, eStats, fstats, i, gameDates[0], calendarDate, position);
                    pitchingStaff.starterStats = nextPitcher;
                    pitchingStaff.starter["G"] = 1;
                    pitchingStaff.starter["GS"] = 1;
                    pitchingStaff.starter["Pos"] = "P";
                }
            }


            // ok.. dones with the starter, grab the closer
            var clPlayer = null;
            for (p = 0; p < players[10].Players.length; p++) {
                var nextPitcher = players[10].Players[p];

                if (nextPitcher.CanRelieve) {
                    // found a closer...
                    if (clPlayer == null && nextPitcher.eStats.TotalPitching && nextPitcher.eStats.TotalPitching.G > 0) {
                        var eStats = nextPitcher.eStats[nextPitcher.highestIP];
                        var stats = eStats.Pitching;
                        var fstats = {};
                        if (eStats.Fielding) {
                            fstats = eStats.Fielding;
                        }

                        if (stats) {

                            calculateOuts(stats);
                            // first option available.. grab him
                            clIndex = p;
                            clGameDay = nextPitcher.FirstFullPitchingGame;
                            clStats = eStats;
                            clDayStats = stats;
                            clFStats = fstats;
                            clOuts = stats["OUT"]; // not used
                            clPlayer = nextPitcher;

                        }
                    } else {
                        unusablePitchers.push(nextPitcher);
                    }
                } else {
                    unusablePitchers.push(nextPitcher);
                }

            }


            // put into the game's format
            if (clPlayer) {
                calculatePitchingLW(clDayStats, null, team.extra.Coordinators);   // just a best guess until game time
                closer = new pitcher(clPlayer, clStats, clFStats, 1, clGameDay, calendarDate, "CL");
                closer["G"] = 1;
                pitchingStaff.relievers.push(closer);
            }


            // if any closers left, put them first in the pen...
            for (p = 0; p < players[10].Players.length; p++) {
                var nextPitcher = players[10].Players[p];

                if (clPlayer != nextPitcher && nextPitcher.eStats && nextPitcher.CanRelieve && (nextPitcher.highestIP && nextPitcher.highestIP != "")) {
                    // found a pitcher for the pullpen...
                    var eStats = nextPitcher.eStats[nextPitcher.highestIP];

                    /*
                    for( statDay in nextPitcher.eStats) {
                        var day = statDay;
                        if( statDay.Pitching && statDay.charAt(0)=="2") {
                            eStats = nextPitcher.eStats[statDay];
                            break;
                        }
                    }
                    */
                    var stats = eStats.Pitching;

                    var fstats = {};
                    if (eStats && eStats.Fielding) {
                        fstats = eStats.Fielding;
                    }

                    if (stats) {

                        if (clPlayer == null) {
                            // make him the closer
                            clIndex = p;
                            clSubIndex = nextPitcher.FirstFullPitchingGame;
                            clStats = eStats;
                            clDayStats = stats;
                            clFStats = fstats;
                            clOuts = stats["OUT"];
                            clPlayer = nextPitcher;
                        } else {

                            bpIndex = pitchingStaff.pitchers.length; // that's where he'll appear in the pitchers list
                            pitchingStaff.relievers.push(new pitcher(nextPitcher, eStats, fstats, bpIndex, nextPitcher.highestIP, calendarDate, "RP"));
                        }

                    }
                } else {
                    if (clPlayer != nextPitcher)
                        unusablePitchers.push(nextPitcher);
                }

            }

            // and now, grab the bullpen!  they are in order.
            // if there is no starter, grab the first guy in the order
            // if there is no closer, grab the first guy in the order (unless there's no starter, in which case grab #2) {

            for (p = 0; p < players[9].Players.length; p++) {
                var nextPitcher = players[9].Players[p];

                if (nextPitcher.eStats && nextPitcher.CanRelieve && ((nextPitcher.highestIP && nextPitcher.highestIP != "") || ((nextPitcher.highestIPMinors && nextPitcher.highestIPMinors != "")))  ) {
                    // found a pitcher for the pullpen...

                    
                    if( nextPitcher.highestIP) {
                        var highestGame = nextPitcher.highestIP ;
                    } else {
                        var highestGame = nextPitcher.highestIPMinors ;
                    }
                    var eStats = nextPitcher.eStats[highestGame] ;

                    /*
                    for( statDay in nextPitcher.eStats) {
                        var day = statDay;
                        if( statDay.Pitching && statDay.charAt(0)=="2") {
                            eStats = nextPitcher.eStats[statDay];
                            break;
                        }
                    }
                    */
                    var stats = eStats.Pitching;

                    var fstats = {};
                    if (eStats && eStats.Fielding) {
                        fstats = eStats.Fielding;
                    }

                    if (stats) {

                        calculateOuts(stats);
                        if (spPlayer == null && eStats.Pitching.OUT > 3) {
                            // make him the starter
                            spIndex = p;
                            spGameDay = nextPitcher.FirstFullPitchingGame;
                            spStats = eStats;
                            spDayStats = stats;
                            spFStats = fstats;
                            spOuts = stats["OUT"];
                            spPlayer = nextPitcher;
                            pitchingStaff.starter = new pitcher(spPlayer, spStats, spFStats, 0, spGameDay, calendarDate, "P");
                            pitchingStaff.starterStats = spPlayer;
                       
                        } else if (clPlayer == null) {
                            // make him the closer
                            clIndex = p;
                            clSubIndex = nextPitcher.FirstFullPitchingGame;
                            clStats = eStats;
                            clDayStats = stats;
                            clFStats = fstats;
                            clOuts = stats["OUT"];
                            clPlayer = nextPitcher;

                            bpIndex = pitchingStaff.pitchers.length; // that's where he'll appear in the pitchers list
                            pitchingStaff.relievers.push(new pitcher(nextPitcher, eStats, fstats, bpIndex, highestGame, calendarDate, "RP"));

                        } else {

                            bpIndex = pitchingStaff.pitchers.length; // that's where he'll appear in the pitchers list
                            pitchingStaff.relievers.push(new pitcher(nextPitcher, eStats, fstats, bpIndex, highestGame, calendarDate, "RP"));
                        }

                    } else {
                        unusablePitchers.push(nextPitcher);
                    }
                } else {
                    unusablePitchers.push(nextPitcher);
                }

            }

            // have the entire staff in place now!
            // put into the game's format
            if (spPlayer && !pitchingStaff.starter) {
                calculatePitchingLW(spDayStats, spPlayer.IsPitchingOn3DaysRest, team.extra.Coordinators);
                starter = new pitcher(spPlayer, spStats, spFStats, 0, spGameDay, calendarDate, "SP");
                starter["G"] = 1;
                starter["GS"] = 1;
                pitchingStaff.pitchersUsed.push(starter);
                starter["Pos"] = "P";
                pitchingStaff.starter = new pitcher(spPlayer, spStats, spFStats, 0, spGameDay, calendarDate, "P");
                pitchingStaff.starterStats = spPlayer;
            } else {
                // oops, don't have a starter!!!
            }

            /*
 
            for( b=0; b<bullPen.length; b++ ) {
                var nextPitcher = bullPen[b];
 
                // first option available.. grab him
 
 
                var eStats = nextPitcher.eStats[nextPitcher.FirstFullPitchingGame];
                var stats = eStats.Pitching;
                var fstats = {};
                if( eStats.Fielding ) {
                    fstats = eStats.Fielding;
                }
 
                bpIndex = pitchingStaff.pitchers.length; // that's where he'll appear in the pitchers list
                bpGameDay = nextPitcher.FirstFullPitchingGame;
                calculateOuts(stats);
                bpOuts = stats["OUT"];
 
                pitchingStaff.pitchers.push(new pitcher(nextPitcher, eStats, fStats, bpIndex, bpGameDay, calendarDate,  "RP"));
 
            }
            */

            /*
            
                        for (var i = 0; i < players.length; i++) {
            
                            var p = players[i];
                            if (p.Primary.length > 0 && p.Primary[0] == "P") {
            
                                // have a pitcher... is he set by the owner?
                                if (p.LineupDH.Position == "SP") {
            
                                } else {
                                    // not set by owner, is he a starter
                                    if (p.CanStart && p.FirstFullPitchingGame != "") {
                                        // found a starter...
                                        if (p.eStats[p.FirstFullPitchingGame] && p.eStats[p.FirstFullPitchingGame].Pitching) {
                                            var stats = p.eStats[p.FirstFullPitchingGame].Pitching;
                                            var fstats = {};
                                            if( p.eStats[p.FirstFullPitchingGame].Fielding )
                                                fstats = p.eStats[p.FirstFullPitchingGame].Fielding;
            
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
                                                    spSubIndex = p.FirstFullPitchingGame;
                                                    spStats = stats;
                                                    spFStats = fstats;
                                                    spOuts = stats["OUT"];
                                                    spPlayer = p;
                                                } else {
                                                    // have another potential starter.. see who's better
                                                    // unless we already have someone going at least 5 innings.
                                                    // for now, who's lasted LONGER
                                                    if ((spOuts < (5 * 3)) && stats.OUT > spOuts) {
                                                        // *** this one has pitched longer, use him
            
                                                        // possibly move last starter into the bullpen...
            
                                                        spIndex = i;
                                                        spSubIndex = p.FirstFullPitchingGame;
                                                        spStats = stats;
                                                        spFStats = fstats;
                                                        spOuts = stats.OUT;
                                                        spPlayer = p;
                                                    }
                                                }
                                            }
                                        }
                                    } else {
                                        // not a starter.. throw him in the pen
                                        // look for potential closers
            
                                        // see if he has any games....
                                        if (p.CanRelieve && p.gamesList && p.gamesList.length > 0) {
                                            var gameday = -1;
                                            for( g=0; g<p.gamesList.length; g++ ) {
                                                if ( p.eStats[p.gamesList[g]].Pitching) {
                                                    gameday = p.gamesList[g];
                                                    break;
                                                }
                                            }
                                            if (gameday != -1 ) {
            
                                                // actually pitched that day...
                                                stats = p.eStats[gameday].Pitching;
                                                calculateOuts(stats);
                                                calculatePitchingLW(stats);
                                                if (pitchingStaff.closer == null && stats.SVO == "1") {
                                                    pitchingStaff.closer = new pitcher(p, stats, fstats, i, gameday, calendarDate,  "CL");
                                                    // don't put the closer in yet!
                                                   //  pitchingStaff.pitchers.push(new pitcher(p, stats, fstats, i, gameday, "CL"));
                                                } else {
                                                    // just in the bullpen
                                                    pitchingStaff.pitchers.push(new pitcher(p, stats, fstats, i, gameday, calendarDate,  "RP"));
                                                }
                                            }
                                        }
                                    }
                                }
                            }
            
                        }
                        // **** assume we have a starter.. put him in the lineup
                        if (spIndex != null) {
                            calculatePitchingLW(spStats);
                            starter = new pitcher(spPlayer, spStats, spFStats, spIndex, spSubIndex, calendarDate,  "SP");
                            starter["G"] = 1;
                            starter["GS"] = 1;
                            pitchingStaff.pitchers.push( starter );
                            pitchingStaff.pitchersUsed.push( starter );
                          //  pitchingStaff.starter = [spIndex, spSubIndex];
                            starter["Pos"] = "P";
                            pitchingStaff.starter = new pitcher(spPlayer, spStats, spFStats, spIndex, spSubIndex, calendarDate,  "P")
            
                        } else {
                            console.log( "Need to pick a RP for a starter");
                            if( pitchingStaff.pitchers.length == 0 ) {
                                console.log( "No relief pitchers."); 
                            } else {
                                pitchingStaff.starter = pitchingStaff.pitchers[0];
                            }
                        }
                    }
            
            
                            // ((((((((((((((((((((((( TODO )))))))))))))))))))))))
                            // - get most recent unused game(s)
                            // - if not there, calculate # of outs per pitcher
                            // - look to see if starter has CG.. if so, no relievers needed! (add to pitchingStaff object)
                            // - if starter goes 8 inninges, setupPitcher = false (see below)
                            // - if starter has less than 3 days rest, reduce to 75% of lw. ie. LW = .75 * LW if LW >0, 
                            //                                          else LW = 1.25*LW if LW < 0
                            // - research "Conditioning" for relieve pitchers
                            // - tag players when they have played already
            
                            // get the pitchers up until the end of the 7th inning
                            var OutsLeft = 21;
                            if( pitchingStaff.starter )
                            OutsLeft -= pitchingStaff.starter.pStats.OUT;
                            var closerPlayerId = -1;
                            if( pitchingStaff.closer ) {
                                closerPlayerId = pitchingStaff.closer.PlayerId;
                            }
                            if( OutsLeft > 0 ) {
                                // find the remaining outs... assume to NOT use the closer
                                for( let i=0; i<pitchingStaff.pitchers.length; i++) {
                                    var nextPitcher = pitchingStaff.pitchers[i];
                                    if( nextPitcher.CanRelieve === true && pitchingStaff.starter.PlayerId != nextPitcher.PlayerId && closerPlayerId != nextPitcher.PlayerId) {
                                        // put this pitcher in used pitchers...
                                        nextPitcher["G"] = 1;
                                        pitchingStaff.pitchersUsed.push( nextPitcher );
                                        OutsLeft -= nextPitcher.pStats.OUT;
                                        
                                        if( OutsLeft <= 0) {
                                            if (OutsLeft < 0) {
                                                // then a partial outing for this pitcher
                                                nextPitcher.pStats.OUT = nextPitcher.pStats.OUT + OutsLeft;
                                                nextPitcher.pStats["UnusedOuts"] = -OutsLeft; // these are outs he didn't use out of his original outs
                                                var newIP = nextPitcher.pStats.OUT / 3;
                                                var newIPInteger = Math.floor( newIP);
                                                newIP = (newIP-newIPInteger == 0) ? newIPInteger : ((newIP-newIPInteger < 0.5) ? newIPInteger+0.1 : newIPInteger+0.2);
                                                nextPitcher.pStats.IP = newIP;
                                            }
                                            break;
                                        }
                                    }
                                }
                                */
        } // end of real data section

        // this "passes" back the data in the team object
        pitchingStaff.unusablePitchers = unusablePitchers;
        team.pitchingStaff = pitchingStaff;

    }
};