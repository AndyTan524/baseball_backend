var mongoose = require('mongoose');

var stateSchema = new mongoose.Schema({
    name: String,
    abbreviation: String
});

mongoose.model('State', stateSchema);