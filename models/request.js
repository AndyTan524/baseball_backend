var mongoose = require('mongoose');

var requestSchema = new mongoose.Schema({
    Type: String,
    FromFirstName: String,
    FromLastName: String,
    FromEmail: String,
    FromPage: String,
    CreatedUTC: String,
    Details: Object
});

mongoose.model('Request', requestSchema);