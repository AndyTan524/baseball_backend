var mongoose = require('mongoose');

// http://mongoosejs.com/docs/schematypes.html

var scheduleSchema = new mongoose.Schema({
    LeagueName: String,
    LeagueId: String,
    Season: String,
    LastUpdate: String,
    Games: [
        {
            gameId: String,
            title: String,
            start: String,
            end: String,
            color: String,
            description: String,
            url: String,
            simpleDate: String,
            isDoubleHeaderNC: Boolean,
            home: String,
            homeId: String,
            visit: String,
            visitId: String,
            CalendarDay: Number,
            dhGame: false,
            extra: {}
        }
    ]
});

mongoose.model('Schedule', scheduleSchema);