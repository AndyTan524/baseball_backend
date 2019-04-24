var mongoose = require('mongoose');

var playerBaserunningMinorsSchema = new mongoose.Schema({
    MLBId: String,
    PlayerName: String,
    Games: Array
});

mongoose.model('PlayerBaserunningMinors', playerBaserunningMinorsSchema, 'playerBaserunningMinors');

