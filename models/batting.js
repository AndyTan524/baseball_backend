var mongoose = require('mongoose');

var battingSchema = new mongoose.Schema({
    Season: String,
    GameDate: String,
    CreatedUTC: String,
    LastModifiedUTC: String,
    Stats: Object
});

mongoose.model('Batting', battingSchema, 'batting');