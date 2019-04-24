var mongoose = require('mongoose');
 

var ytdFieldingMinorsSchema = new mongoose.Schema({
    Season: String,
    GameDate: String,
    CreatedUTC: String,
    LastModifiedUTC: String,
    Stats: Object
}, { collection: 'ytdfieldingMinors' });

mongoose.model('YtdFieldingMinors', ytdFieldingMinorsSchema);