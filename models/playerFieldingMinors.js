var mongoose = require('mongoose');

var playerFieldingMinorsSchema = new mongoose.Schema({
    MLBId: String,
    PlayerName: String,
    Games: Array
});

mongoose.model('PlayerFieldingMinors', playerFieldingMinorsSchema, 'playerFieldingMinors');

