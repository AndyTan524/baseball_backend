var mongoose = require('mongoose');

var fieldingSchema = new mongoose.Schema({
    Season: String,
    GameDate: String,
    CreatedUTC: String,
    LastModifiedUTC: String,
    Stats: Object
});

mongoose.model('Fielding', fieldingSchema, 'fielding');