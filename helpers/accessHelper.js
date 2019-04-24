
var mongoose = require('mongoose'); 

var utils = require('../helpers/utils');

module.exports = {
    isAccessAllowed: function (req, callback) { 
        var token = utils.getToken(req);
        callback(null, { user: null, isAllowed: true });
    } 
};