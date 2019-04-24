// ********************************************
//
//  render CSV file of results
//
// ********************************************

// **********************************************************
//
// temporarily write out box score to csv file
//
// **********************************************************
function writeTestCSV(boxscore, game) {

    var fs = require('fs');

    filename = "rfb-zzz-test-" + game.home.abbreviation + "vs" + game.visit.abbreviation;
    fs.writeFile("./simulator/games/" + filename + ".csv", boxscore,
        function (err) {
            if (err) {
                return console.log(err);
            }

            console.log("The file was saved!");
        });
}

// ************************************************************
//
// number formatting functions
//
// ************************************************************
function isNumber(n) {
    return !isNaN(parseFloat(n)) && isFinite(n);
}

function formatValue(column, n) {
    if (typeof (n) == "undefined")
        return ("--");

    if (!isNumber(n))
        return (n);


    formatDecimals = false;
    decimalplaces = 0;
    if (decimalColumns.indexOf(column) > -1) {
        formatDecimals = true;
        decimalplaces = 2;
    }
    if( column == "IP") {
        formatDecimals = true;
        decimalplaces = 1;                   
    }

    rounding = false;
  if (column == "R" || column == "Runs") {
    rounding = true;
  }

    // is a number...
    var value = 0;

    if (decimalplaces == 0) {
        if (rounding)
            n += 0.5;
        value = Math.floor(n);
    } else {
        value = Number(n).toFixed(decimalplaces);
    }

    return value;
}

// *****************************************************
//
// columns that are 2 decimal places
//
//
var decimalColumns = ["LW", "ArW", "ArbW", "Bases", "FieldLW", "Zone", "Block", "Frame", "cERA", "IPPG"];

// *****************************************************
//
//  renderCSV() main code
//
// *****************************************************

function renderCSV(game, score) {

    // grab team names to save time/space
    var teamNames = [game.visit.name, game.home.name];

    var bt = boxscoreTemplate;

    var boxCSV = "";

    // ***********  create summary ***************
    var summary = bt.Summary.Rows[0];
    for (var line = 0; line < 3; line++) {

        var results = score.summary.Visit;
        if (line == 2)
            results = score.summary.Home;

        for (var c = 0; c < summary.length; c++) {
            var column = summary[c];

            // remove the "empty" as it is just for readablity of the code
            if (column == "empty")
                column = "";
            if (column == "FINAL SCORE") {
                if (line > 0) {
                    column = results.name;
                    if (column == "")
                        column = "team name";
                }
            } else {
                // grab the value
                if (line > 0) {

                    column = results[column];

                }
            }

            boxCSV += column;

            if ((c + 1) == summary.length) {
                // end of line
                boxCSV += '\r\n';
            } else {

                // not end, add comma
                boxCSV += ", ";
            }
        }
    }

    // ***********  create details ***************

    // start by adding two blank rows
    boxCSV += '\r\n\r\n';

    // twice through.. one for each team
    // each team does:
    // title
    // column headers for batters
    // data for batters
    // totals for batters
    // same for pitchers

    var details = bt.Details.Rows;
    for (team = 0; team < 2; team++) {


        // ******* section header
        boxCSV += ">>> " + teamNames[team] + '\r\n';
        boxCSV += ">>> BATTING " + '\r\n';

        // ******* batters
        var batters = details['Batters'];

        var linestats = score.Visit.Batters;
        if (team == 1)
            linestats = score.Home.Batters;

        var numberlines = linestats.length;

        for (var lines = -1; lines < numberlines; lines++) {
            if( linestats[lines]) {
            for (var c = 0; c < batters.length; c++) {
                var column = batters[c];

                // remove the "empty" as it is just for readablity of the code
                if (column == "empty") {
                    column = "";
                } else {
                    if (lines >= 0) {
                        if (column == "Name")
                            column = "FullName";
                            if( column == "DEC")
                            column = "Decision";


                        // column = linestats[lines][column];
                        column = formatValue(column, linestats[lines][column]);
                    }
                }
                boxCSV += column;

                if ((c + 1) == batters.length) {
                    boxCSV += '\r\n';
                } else {
                    boxCSV += ", ";
                }
            }

        }

        }

        // ******* batters total
        batters = details['Batters'];
        linestats = score.Visit.BattingTotals;
        if (team == 1)
            linestats = score.Home.BattingTotals;
        for (var c = 0; c < batters.length; c++) {
            var column = batters[c];

            // remove the "empty" as it is just for readablity of the code
            if (column == "empty") {
                column = "";
            } else {
                // column = linestats[column];

                column = formatValue(column, linestats[column]);
            }
            boxCSV += column;

            if ((c + 1) == batters.length) {
                boxCSV += '\r\n';
            } else {
                boxCSV += ", ";
            }
        }

         results = score.summary.Visit;
        if (team == 1)
            results = score.summary.Home;
        // ******* offense total
        batters = details['OverallOffense'];
        for (var c = 0; c < batters.length; c++) {
            var column = batters[c];

            // remove the "empty" as it is just for readablity of the code
            if (column == "empty")
                column = "";
            boxCSV += column;

            if ((c + 1) == batters.length) {
                // add in the actual value
              boxCSV += ", " + results.Off;
                boxCSV += '\r\n';
            } else {
                boxCSV += ", ";
            }
        }


        // ******* defense total
        batters = details['OverallDefense'];
        for (var c = 0; c < batters.length; c++) {
            var column = batters[c];

            // remove the "empty" as it is just for readablity of the code
            if (column == "empty")
                column = "";
            boxCSV += column;

            if ((c + 1) == batters.length) {
                boxCSV += ", " + results.Def;
                boxCSV += '\r\n';
            } else {
                boxCSV += ", ";
            }
        }

        // blank line

        boxCSV += '\r\n';

        // ******* pitchers
        boxCSV += ">>> PITCHING " + '\r\n';
        var pitchers = details['Pitchers'];


        var linestats = score.Visit.Pitchers;
        if (team == 1)
            linestats = score.Home.Pitchers;

        var numberlines = linestats.length;

        for (var lines = -1; lines < numberlines; lines++) {
            for (var c = 0; c < pitchers.length; c++) {
                var column = pitchers[c];

                // remove the "empty" as it is just for readablity of the code
                if (column == "empty") {
                    column = "";
                } else {
                    if (lines >= 0) {
                        if (column == "Name")
                            column = "FullName";
                            if( column == "DEC")
                            column = "Decision";

                        //  column = linestats[lines][column];
                        column = formatValue(column, linestats[lines][column]);
                    }
                }
                boxCSV += column;

                if ((c + 1) == pitchers.length) {
                    boxCSV += '\r\n';
                } else {
                    boxCSV += ", ";
                }
            }
        }


        // ******* pitchers total
        pitchers = details['Pitchers'];
        linestats = score.Visit.PitchingTotals;
        if (team == 1)
            linestats = score.Home.PitchingTotals;
        for (var c = 0; c < pitchers.length; c++) {
            var column = pitchers[c];

            // remove the "empty" as it is just for readablity of the code
            if (column == "empty") {
                column = "";
            } else {
                // column = linestats[column];
                column = formatValue(column, linestats[column]);
            }
            boxCSV += column;

            if ((c + 1) == pitchers.length) {
                boxCSV += '\r\n';
            } else {
                boxCSV += ", ";
            }
        }

        // blank line
        boxCSV += '\r\n';

        // end of one team
    }

    writeTestCSV(boxCSV, game);
}