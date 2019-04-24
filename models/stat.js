var mongoose = require('mongoose');

// http://mongoosejs.com/docs/schematypes.html

var statSchema = new mongoose.Schema({
    GameDate: String,
    CreatedUTC: String,
    LastModifiedUTC: String,
    Pitching: Object,
    Fielding: Object,
    Batting: Object,
    Baserunning: Object,
    DRS: Object
});

mongoose.model('Stat', statSchema);