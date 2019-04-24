var mongoose = require('mongoose');

var playerPitchingSchema = new mongoose.Schema({
    MLBId: String,
    PlayerName: String,
    Games: Array
});

mongoose.model('PlayerPitching', playerPitchingSchema, 'playerPitching');

