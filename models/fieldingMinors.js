var mongoose = require('mongoose');

var fieldingMinorsSchema = new mongoose.Schema({
    Season: String,
    GameDate: String,
    CreatedUTC: String,
    LastModifiedUTC: String,
    Stats: Object
});

mongoose.model('FieldingMinors', fieldingMinorsSchema, 'fieldingMinors');