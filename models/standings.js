var mongoose = require('mongoose');

// http://mongoosejs.com/docs/schematypes.html

var standingsSchema = new mongoose.Schema({
    LeagueName: String,
    LeagueId: String,
    Season: String,
    LastUpdate: String,
    Conferences: Object
});

mongoose.model('Standings', standingsSchema, 'standings');