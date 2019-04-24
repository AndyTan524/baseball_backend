var mongoose = require('mongoose');

var baserunningMinorsSchema = new mongoose.Schema({
    Season: String,
    GameDate: String,
    CreatedUTC: String,
    LastModifiedUTC: String,
    Stats: Object
});

mongoose.model('BaserunningMinors', baserunningMinorsSchema, 'baserunningMinors');