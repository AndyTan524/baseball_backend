var mongoose = require('mongoose');


var dealSchema = new mongoose.Schema({
    Type: String,
    Status: String,
    LeagueId: String,
    OriginalDealId: String,
    ParentDealId: String,
    ChildDealId: String,
    DateCreatedUTC: String,
    DateUpdatedUTC: String,
    DateCompletedUTC: String,
    SubmittedBy: String,
    SubmittedById: String,
    SubmitterTeam: String,
    LeagueRepresentative: {},
    Teams: [],
    Players: []
});

mongoose.model('Deal', dealSchema);