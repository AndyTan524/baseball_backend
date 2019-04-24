var mongoose = require('mongoose');

var playerBattingSchema = new mongoose.Schema({
    MLBId: String,
    PlayerName: String,
    Games: Array
});

mongoose.model('PlayerBatting', playerBattingSchema, 'playerBatting');

