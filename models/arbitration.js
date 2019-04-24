var mongoose = require('mongoose'); 

var arbitrationSchema = new mongoose.Schema({
    LeagueId: String,
    TeamId: String,
    FullName: String,
    Position: String,
    Category: String,
    LastUpdateUTC: String,
    MLS: String,
    rTeamName: String,
    LastYearSalary: String,
    ProjectedRaise: String,
    NextYearForecastedHigh: String,
    NextYearForecastedLow: String,
    NonTender: String,
    ClubSubmission: String,
    PlayerSubmission: String,
    Note: String,
    AgreedTerms: String,
    MultiYear: String,
    MlbId: String    
});


mongoose.model('Arbitration', arbitrationSchema);