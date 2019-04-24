var mongoose = require('mongoose');


var usedGameSchema = new mongoose.Schema({
    LeagueId: String,
    PlayerId: String,
    MlbId: String,
    FullName: String,
    Season: String,
    FullGames: [],
    FullGameIds: [],
    PartialGames: [],
    LastGameDateUTC: String,
    ConsecutiveGamesCaught: 0
});

mongoose.model('UsedGame', usedGameSchema);