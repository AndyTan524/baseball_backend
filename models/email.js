var mongoose = require('mongoose');

// http://mongoosejs.com/docs/schematypes.html

var emailSchema = new mongoose.Schema({
    Name: String,
    Template: String,
    CreatedUTC: String,
    Body: String,
    Subject: String,
    FromAddress: String
});

mongoose.model('Email', emailSchema);