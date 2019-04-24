var mongoose = require('mongoose'); 

var calendarSchema = new mongoose.Schema({
    title: String,
    start: String,
    allDay: Boolean,
    home: String,
    away: String
});

mongoose.model('Calendar', calendarSchema);