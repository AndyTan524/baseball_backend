var mongoose = require('mongoose'); 

var leagueSchema = new mongoose.Schema({
    Name: String,
    PrimaryColor: String,
    SecondarColor: String,
    Logo: String,
    Description: String,
    CreatedUTC: String,
    Settings: Object,
    Calendar: Object,
    Commissioners: [],
    Conferences: [],
    LeagueFinancials: Object,
    Teams: Object,
    PlayerData: Object,
    FreeAgents: Object,
    InternationalDraft: Object,
    FirstYearDraft: Object,
    RetainedSalaries: Object
});

mongoose.model('League', leagueSchema);