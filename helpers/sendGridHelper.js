var helper = require('sendgrid').mail;

var mongoose = require('mongoose');
var User = mongoose.model('User');
var Email = mongoose.model('Email');

// SendGrid account key for RFBaseball.IT.Ops account
// var API_KEY = "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";

// SendGrid account key for rsportsbaseball account
var API_KEY = "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";

module.exports = {
    sendEmail: function (template, user, customterms, callback) {
        var qry = { Template: template };

        var context = this;

        Email.findOne(qry, function (err, email) {
            if (err) {
                callback(err);
            }
            else if (email) {
                context.generateEmail(user, email, customterms, function(){
                    if( callback ) {
                        callback();
                    }
                })
            }
            else {
                callback();
            }
        });
    },

    generateEmail: function (user, email, customterms, callback) {

        var isProd = false;
        var skipDevTest = false;

        var testName = "";
        if (!isProd) {
            testName = "DEV TEST - ";
            skipDevTest = true;
        }
        var fromEmailAddress = 'doNotReply@rsportsbaseball.com';
        if (email.FromAddress)
            fromEmailAddress = email.FromAddress
        if (customterms && customterms.fromAddress)
            fromEmailAddress = customterms.fromAddress;
        var fromEmail = new helper.Email(fromEmailAddress);


        var toEmailAddress = [];
        var toEmailName = [];

        if (user && user.Email) {
            // single user
            var name = "";
            if (user.FirstName && user.LastName)
                name = user.FirstName + " " + user.LastName;
            toEmailAddress.push(user.Email);
            toEmailName.push(name);
        }
        if (customterms && customterms.toAddress) {
            // it's a list
            toEmailAddress = [];
            toEmailName = [];

            for (i = 0; i < customterms.toAddress.length; i++) {
                name = "";
                if (customterms.toName[i])
                    name = customterms.toName[i];

                var skip = customterms.toAddress[i].includes("SKIP-");
                if (!skip && !isProd) {
                    skip = !customterms.toAddress[i].includes("dombrower");
                    if (skip) {
                        if( !skipDevTest)
                            skip = !customterms.toAddress[i].includes("rsportsbaseball");
                    }
                }
                if (!skip) {
                    toEmailAddress.push(customterms.toAddress[i]);
                    toEmailName.push(customterms.toName[i]);
                } else {
                    console.log("email skipped for: " + customterms.toName[i])
                }
            }
        }

        var subject = email.Subject;
        if (customterms && customterms.Subject)
            subject = customterms.Subject;

        subject = testName + subject;
        var body = this.addCustomContent(user, email, customterms);

        var content = new helper.Content('text/html', body);

        // ok send 'em
        var count = 0;
        
        if( toEmailAddress.length==0) {
            callback();
            return;
        }
        for (i = 0; i < toEmailAddress.length; i++) {

            var toEmail = new helper.Email(toEmailAddress[i], toEmailName[i]);

            this.execute(fromEmail, subject, toEmail, content, function () {
                count++;
                if (count >= toEmailAddress.length) {
                    if (callback) {
                        callback();
                    }
                    return;
                }

            });
            console.log("email sent to: " + toEmail.email + " for " + toEmail.name);
        }
    },


    execute: function (fromEmail, subject, toEmail, content, callback) {
        var mail = new helper.Mail(fromEmail, subject, toEmail, content);

        var sg = require('sendgrid')(API_KEY);
        var request = sg.emptyRequest({
            method: 'POST',
            path: '/v3/mail/send',
            body: mail.toJSON()
        });

        sg.API(request, function (error, response) {
            if (error) {
                console.log('Error response received');
            }

            callback(error, response);
        });
    },

    addCustomContent: function (user, email, customterms) {
        var body = email.Body;

        if (email.Template == "forgotpass") {
            // body = email.Body.replace("{{reset-hash}}", user.PasswordReset);
            body = email.Body.replace("{{newpw}}", customterms.newpw);
        } else {

            if (user && user.FirstName && user.LastName) {
                body = body.replace(new RegExp("{{username}}", 'g'), user.FirstName + " " + user.LastName);
            }

            if (customterms) {
                Object.keys(customterms).forEach(function (key, index) {
                    // key: the name of the object key
                    // index: the ordinal position of the key within the object 
                    var term = customterms[key];
                    body = body.replace(new RegExp("{{" + key + "}}", 'g'), term);
                });
            }
        }

        return body;
    }
}