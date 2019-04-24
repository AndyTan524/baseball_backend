var helper = require('sendgrid').mail;

var mongoose = require('mongoose');
var User = mongoose.model('User');
var utils = require('./utils');
var twillio = require("../data/twilio")();

module.exports = {
    sendSmsByJwt: function (jwt, callback) { 
        var qry = { jwt: jwt };

        this.executeQuery(qry, callback);
    },

    sendSmsByPhone: function (phone, callback) {
        var qry = { phone: phone };

        this.executeQuery(qry, callback);
    },

    executeQuery: function (qry, callback) {
        var context = this;

        User.findOne(qry, function (err, user) {
            if (err) {
                callback(err);
            }
            else if (user) {
                context.sendSmsByUser(user, callback);
            }
            else {
                callback({ errmsg: "No user" });
            }
        });
    },

    sendSmsByUser: function (user, callback) {

        if (user) {
            var code = utils.generateRandomInteger(1000, 9999).toString();
            user._doc.code = code;

            twillio.create({ body: "Your code is " + code, to: "+1" + user.Phone }, function (err, response) {
                if (err) {
                    callback(err);
                }
                else {
                    user.save();
                    callback(err, response);
                }
            });
        }
        else {
            callback({ errmsg: "No user" });
        }
    }
     
}