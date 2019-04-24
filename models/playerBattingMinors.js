var mongoose = require('mongoose');

var playerBattingMinorsSchema = new mongoose.Schema({
    MLBId: String,
    PlayerName: String,
    Games: Array
});

mongoose.model('PlayerBattingMinors', playerBattingMinorsSchema, 'playerBattingMinors');

