var mongoose = require('mongoose');

var ytdStatsSchema = new mongoose.Schema({
    Name: String,
    Team: String,
    PlayerId: String,
    MlbId: String,
    POS: String,
    Batter: Object,
    SP: Object,
    RP: Object,
    POverall: Object,
    Defense: Object,
    Baserunning: Object,
    Catcher: Object,
    RValue: Object,
    Zone: Object,
    FieldingIndex: Object,
    BlockFrameCEra: Object
});

mongoose.model('YtdStats', ytdStatsSchema);