var mongoose = require('mongoose'); 

var simPlayerSchema = new mongoose.Schema({
    MlbId: String, 
    MlbDepth: String,
    PlayerId: String,
    Fielding: Object,
    FullName: String,
    Position: String,  
    Batting: Object,   
    Image: String,
    Baserunning: Object,   
    Pitching: Object
});

mongoose.model('SimPlayer', simPlayerSchema);