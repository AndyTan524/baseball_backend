var mongoose = require('mongoose'); 

// http://mongoosejs.com/docs/schematypes.html

var userSchema = new mongoose.Schema({
    Username: {
        type: String, 
        required: true
    },
    Email: {
        type: String,
        required: true
    },
    Password: {
        type: String,
        required: true
    },
    Salt: String,
    FirstName: String,
    LastName: String,
    Enabled: Boolean,
    LastLogonTimeUtc: String,
    CreatedUTC: String,
    token: String,
    Roles: Object,
    PasswordReset: String,
    PasswordResetExpiration: String,
    Phone: String,
    Twitter: String,
    Facebook: String,
    Website: String,
    Settings: {},
    jwt: String,
    fbtoken: String,
    lastIPAddress: String
});

mongoose.model('User', userSchema);