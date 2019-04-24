var mongoose = require('mongoose');
 

var rosterSchema = new mongoose.Schema({
    LeagueId: String,
    TeamId: String,
    TeamName: String,
    TeamAbbr: String,
    FortyManAL: Object,
    FortyManNL: Object,
    DepthChartNL: Object,
    DepthChartAL: Object,
    BattingOrderNL: Object,
    BattingOrderAL: Object,
    Bench: [],
    NonRoster: Object,
    Extra: Object,
    CreatedUTC: String
});

mongoose.model('Roster', rosterSchema);