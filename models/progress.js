var mongoose = require('mongoose'); 

var progressSchema = new mongoose.Schema({
    LeagueId: String,
    ItemId: String,
    ItemName: String,
    Status: String,
    PercentComplete: Number,
    StartTime: String,
    EndTime: String
});


mongoose.model('Progress', progressSchema, 'progress');