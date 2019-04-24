var PromiseFtp = require('promise-ftp');
var fs = require('fs');
var ftp = new PromiseFtp();

var utils = require('../helpers/utils');
var csvHelper = require('../helpers/csvHelper');

module.exports = {
    syncAllFiles: function (callback) {
        client.connect(function () { 
            client.download('/', 'i:/tmp/rfb', { overwrite: 'none' }, function (result) {
                console.log(result);
            });

        }); 
    },

    syncFile: function (filename, callback) {

        /*
        getConnectionStatus(): Returns a string describing the current connection state. Possible strings are enumerated in PromiseFtp.STATUSES, as well as below:

not yet connected

connecting

connected

logging out

disconnecting

disconnected

reconnecting
        */
        var status = ftp.getConnectionStatus();
        if (status != "disconnected" && status != "not yet connected") {
            // not good to go
            console.log( filename +  " not ready to connect to :" + status);
          //  ftp.end();
           callback([]);
           return false;
             /*
            // try again...
            if( this.syncTryCount == 0 )
                this.syncTryCount = 20;
            else {
                this.syncTryCount--;
                if( this.syncTryCount > 0  )
                 this.syncFile( filename, callback);
                 else {
                    callback([]);
                 }
            }
            */

        } // else {
        {
            try {
                ftp.connect({
                    host: 'ftp2.baseballinfosolutions.com',
                    port: 21,
                    user: 'RFBB',
                    password: 'c.h8KE:r76'
                })
                    .then(function (serverMessage, more) {

                        return ftp.list()

                    }).then(function (data, something) {

                        var found = data.filter(function (key, value) {
                            return key.name == filename;
                        });

                        if (found.length > 0) {
                            var result = ftp.get(filename);
                            return result;
                        }
                        else {
                            return false;
                        }

                    }).then(function (stream) {

                        if (stream.once) {

                            return new Promise(function (resolve, reject) {
                              stream.once('close', resolve);
                              stream.once('error', reject);
                                //stream.pipe(fs.createWriteStream('' + filename));
                                csvHelper.parse(stream, callback);
                            });
                        }
                        else {
                            return new Promise(function (resolve, reject) {
                                callback([]);
                            });
                        }
                    }).then(function () {

                        return ftp.end();
                     return true;

                    }).error( function(err) {
                        console.log( "ftp error from try.." + filename);
                        ftp.end();
                        callback([]);
                    })
  
            }
            catch (err) {
                if (err) {
                    console.log( "ftp error from try..");

                    return ftp.end();
                }
            }
        }
    }
  
};