var mongoose = require('mongoose');

var contentSchema = new mongoose.Schema({
    name: String,
    version: String,
    content: {}
});

mongoose.model('Content', contentSchema);