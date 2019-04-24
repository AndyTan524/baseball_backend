var mongoose = require('mongoose');

var playerPitchingMinorsSchema = new mongoose.Schema({
    MLBId: String,
    PlayerName: String,
    Games: Array
});

mongoose.model('PlayerPitchingMinors', playerPitchingMinorsSchema, 'playerPitchingMinors');

