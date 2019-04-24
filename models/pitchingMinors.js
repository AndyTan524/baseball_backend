var mongoose = require('mongoose');

// http://mongoosejs.com/docs/schematypes.html

var pitchingMinorsSchema = new mongoose.Schema({
    Season: String,
    GameDate: String,
    CreatedUTC: String,
    LastModifiedUTC: String,
    Stats: Object
});

mongoose.model('PitchingMinors', pitchingMinorsSchema, 'pitchingMinors');