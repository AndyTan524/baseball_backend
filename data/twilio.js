var twilio = require("twilio");

var accountSid = 'ACee08e7229811435e3358c0fb5e05926b'; // Your Account SID from www.twilio.com/console
var authToken = 'c07ea1a8c00ba476b0ee1eef84070981';   // Your Auth Token from www.twilio.com/console
var fromNumber = "+14159360401";
var client = new twilio(accountSid, authToken);

module.exports = function (app) {
    return {
        create: function (message, callback) {
            client.messages.create({
                body: message.body,
                to: message.to,
                from: fromNumber
            }, function (err, message) {
                callback(err, message);
            });
        }
    }
};