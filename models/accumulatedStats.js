var mongoose = require('mongoose');

var accumulatedStatsSchema = new mongoose.Schema({
    LeagueId: String,
    PlayerId: String,
    MlbId: String,
    FullName: String,
    LastDayPlayed: String,
    NextAvilableGame: String,
    GamesPlayedInARow: 0,
    DaysList: [],
    OutsPitchedList: [],
    GamesList: [],
    PositionsList: [],
    Stats: [],
    PitchingStats: [],
    FullyUsedGames: [],
    PartialGames: [],
    TeamName: String,
    TeamId: String,
    TeamConference: String
});

mongoose.model('AccumulatedStats', accumulatedStatsSchema, 'accumulatedStats');