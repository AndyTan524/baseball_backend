var mongoose = require('mongoose'); 

var logSchema = new mongoose.Schema({
    Message: String,
    Level: String,
    Route: String,
    Method: String,
    CreatedUTC: String 
});

mongoose.model('Log', logSchema);