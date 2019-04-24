var mongoose = require('mongoose');

// http://mongoosejs.com/docs/schematypes.html

var apiSchema = new mongoose.Schema({
    Name: String,
    Route: String,
    Method: String,
    Description: String,
    Params: Object,
    AdminOnly: Boolean,
    Category: String
});

mongoose.model('Api', apiSchema);