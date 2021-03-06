﻿var mongoose = require('mongoose'); 

var playerDailySchema = new mongoose.Schema({
    GameDate: String,
    mlb_id: String,
    mlb_name: String,
    mlb_pos: String,
    mlb_team: String,
    mlb_team_long: String,
    bats: String,
    throws: String, 
    birth_year: String,
    bp_id: String,
    bref_id: String,
    bref_name: String,
    cbs_id: String,
    cbs_name: String,
    cbs_pos: String, 
    espn_id: String,
    espn_name: String,
    espn_pos: String,
    fg_id: String,
    fg_name: String,
    fg_pos: String,
    lahman_id: String, 
    nfbc_id: String,
    nfbc_name: String,
    nfbc_pos: String,
    retro_id: String,
    retro_name: String,
    debut: String,
    yahoo_id: String, 
    yahoo_name: String,
    yahoo_pos: String,
    mlb_depth: String,
    ottoneu_id: String,
    ottoneu_name: String,
    ottoneu_pos: String 
});

mongoose.model('PlayerDaily', playerDailySchema);