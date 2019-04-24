var mongoose = require('mongoose');

var playerFieldingSchema = new mongoose.Schema({
    MLBId: String,
    PlayerName: String,
    Games: Array
});

mongoose.model('PlayerFielding', playerFieldingSchema, 'playerFielding');

