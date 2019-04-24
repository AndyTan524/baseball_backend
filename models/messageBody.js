var mongoose = require('mongoose');

var messageBodySchema = new mongoose.Schema({
    Type: String,
    FromUserId: String,
    CreatedUTC: String,
    Subject: String,
    Message: String,
    ReferenceId: String,
    ReferenceType: String,
    EmailTemplate: String
});

mongoose.model('MessageBody', messageBodySchema);