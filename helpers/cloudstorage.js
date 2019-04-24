var fs = require('fs');

var gcs = require('@google-cloud/storage')({
    projectId: 'theta-maker-171414',
    keyFilename: '../cfg/keyfile.json',
    credentials: require('../cfg/keyfile.json'),
    promise: require('bluebird')
});

var utils = require('../helpers/utils');

module.exports = {
    createBucket: function (bucket, callback) {
        gcs.createBucket(bucket, function (err, bucket) {
            callback(err, bucket);
        });
    },
    uploadFile: function (filePath, bucket, callback) { 
        var bkt = gcs.bucket(bucket);
         
        bkt.upload(filePath, function (err, file) {
            callback(err, file);
        });
    },
    downloadFile: function (filename, filePath, bucket, callback) {
        var bkt = gcs.bucket(bucket);

        bkt.file(filename).download({ destination: filePath + filename }, function (err) {
            callback(err);
        });
    },
};