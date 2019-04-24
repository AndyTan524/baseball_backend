var mongoose = require('mongoose');

var boxscoreSchema = new mongoose.Schema({
    Season: String,
    LeagueId: String,
    GameDate: String,
    GameNumber: String,
    CreatedUTC: String,
    Game: Object,
    Boxscore: Object,
    TeamData: Object,
    Status: String,
    URL: String,
    Override: Object,

});

mongoose.model('Boxscore', boxscoreSchema, 'boxscores');