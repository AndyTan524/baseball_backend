var mongoose = require('mongoose');
 

var rosterLineupSchema = new mongoose.Schema({
    LeagueId: String,
    TeamId: String,
    TeamName: String,
    Players: [],
    InactivePlayers:[],
    NonRosterPlayers:[],
    DepthChartNL: [],
    BattingOrderNL: [],
    BattingOrderAL: [],
    Bench: [],
    CreatedUTC: String
});

mongoose.model('RosterLineup', rosterLineupSchema);