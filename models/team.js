var mongoose = require('mongoose');

var teamSchema = new mongoose.Schema({
    team_id: String,
    abbreviation: String,
    active: String,
    first_name: String,
    last_name: String,
    conference: String,
    division: String,
    site_name: String,
    city: String,
    state: String,
    full_name: String
});

mongoose.model('Team', teamSchema);