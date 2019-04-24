var mongoose = require('mongoose');

var tempMasterPlayersSchema = new mongoose.Schema({
    MasterPlayers:[]
});

mongoose.model('TempMasterPlayers', tempMasterPlayersSchema, 'tempMasterPlayers');