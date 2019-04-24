var mongoose = require('mongoose');

var playerBaserunningSchema = new mongoose.Schema({
    MLBId: String,
    PlayerName: String,
    Games: Array
});

mongoose.model('PlayerBaserunning', playerBaserunningSchema, 'playerBaserunning');

