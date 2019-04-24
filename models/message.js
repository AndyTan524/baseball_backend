var mongoose = require('mongoose');

var messageSchema = new mongoose.Schema({
    Type: String,
    ToUserId: String,
    ToTeamName: String,
    FromUserId: String,
    UserLeagueId: String,
    UserTeamId: String,
    MessageBodyId: String,
    CreatedUTC: String,
    ReadUTC: String,
    ThreadId: String,
    ThreadCount: Number,
    ParentId: String,
    ChildId: String
});

mongoose.model('Message', messageSchema);