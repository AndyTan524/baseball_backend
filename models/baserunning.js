var mongoose = require('mongoose');

var baserunningSchema = new mongoose.Schema({
    Season: String,
    GameDate: String,
    CreatedUTC: String,
    LastModifiedUTC: String,
    Stats: Object
});

mongoose.model('Baserunning', baserunningSchema, 'baserunning');