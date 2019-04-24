var mongoose = require('mongoose');

var battingMinorsSchema = new mongoose.Schema({
    Season: String,
    GameDate: String,
    CreatedUTC: String,
    LastModifiedUTC: String,
    Stats: Object
});

mongoose.model('BattingMinors', battingMinorsSchema, 'battingMinors');