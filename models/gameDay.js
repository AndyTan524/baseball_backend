var mongoose = require('mongoose');

var gameDaySchema = new mongoose.Schema({
    LeagueId: String,
    GameDate: String,
    CalendarGameDay: Number,
    Completed: Boolean,
    Status: String,
    ImportStatus: String,
    GamesPlayed: {},
    GamesPlayedNC: {},
    DoubleHeader: Boolean,
    ImportStats: {},
    EligibilityActive: {},
    EligibilityActive2: {},
    EligibilityActivePostGame: {},
    EligibilityInactive: {},
    Transactions: {},
    Games: {},
    AccumulatedStats: {},
    LastUpdate: String
});

mongoose.model('GameDay', gameDaySchema, 'gameDay');