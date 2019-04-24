var mongoose = require('mongoose');
 

var ytdFieldingSchema = new mongoose.Schema({
    Season: String,
    GameDate: String,
    CreatedUTC: String,
    LastModifiedUTC: String,
    Stats: Object
}, { collection: 'ytdfielding' });

mongoose.model('YtdFielding', ytdFieldingSchema);