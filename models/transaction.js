var mongoose = require('mongoose');


var transactionSchema = new mongoose.Schema({
    Type: String,
    Status: String,
    Archived: Boolean,
    LeagueId: String,
    DateUTC: String,
    Teams: [],
    Player: {},
    Headline: String,
    DealId: String
});

mongoose.model('Transaction', transactionSchema);