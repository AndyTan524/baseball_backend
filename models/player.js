var mongoose = require('mongoose');

// http://mongoosejs.com/docs/schematypes.html

var playerSchema = new mongoose.Schema({
    MlbId: String,
    EspnId: String,
    CbsId: String,
    MlbDepth: String,
    FirstName: String,
    LastName: String,
    FullName: String,
    Position: String,
    Bats: String,
    Throws: String,
    Dob: String,
    Age: String,
    YearSigned: String,
    MLBTeam: String, 
    MilbAffiliate: String,
    Level: String,
    Salary: Object,
    SecondaryPostiion: String,
    TertiaryPosition: String,
    Number: String,
    Image: String
});

mongoose.model('Player', playerSchema);