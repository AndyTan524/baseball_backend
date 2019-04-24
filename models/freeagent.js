var mongoose = require('mongoose');

// http://mongoosejs.com/docs/schematypes.html

var freeAgentSchema = new mongoose.Schema({
    MlbId: String,
    LeagueId: String,
    FirstName: String,
    LastName: String,
    FullName: String,
    Image: String,
    Position: String, 
    Bats: String, 
    Throws: String,
    DOB: String,
    Type: String,
    MlbTeam: String,
    MilbAffiliate: String,
    Level: String,
    MLS: Number,
    FortyMan: String,
    rSalary: {},
    Options: Number,
    PriorOR: String,
    DraftExcluded: String,
    SigningYear: String,
    RuleFiveEligibleYear: String,
    Image: String
});

mongoose.model('FreeAgent', freeAgentSchema);