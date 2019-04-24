var firebase = require("firebase"); 
var admin = require("firebase-admin");

var PROJECT_ID = "rfb-demo";
var API_KEY = "AIzaSyAm8MHR1zNafZBBSKYKHiFntcaGVBJi1lI";

var config = {
    apiKey: "AIzaSyAm8MHR1zNafZBBSKYKHiFntcaGVBJi1lI",
    authDomain: "rfb-demo.firebaseapp.com",
    databaseURL: "https://rfb-demo.firebaseio.com",
    projectId: "rfb-demo",
    storageBucket: "rfb-demo.appspot.com",
    messagingSenderId: "437193484154"
};
firebase.initializeApp(config);


var serviceAccount = require("../cfg/rfb-demo-firebase.json");

admin.initializeApp({
    credential: admin.credential.cert(serviceAccount),
    databaseURL: "https://" + PROJECT_ID + ".firebaseio.com"
});
 

module.exports = function (app) {
    return {
        sendToDevices: function (tokens, payload, options, callback) {
            admin.messaging().sendToDevice(tokens, payload, options)
                .then(function (response) {
                    callback(null, response);
                })
                .catch(function (error) { 
                    callback(error, null);
                });
        },

        sendToDeviceGroup: function (notificationKey, payload, options, callback) {
            admin.messaging().sendToDeviceGroup(notificationKey, payload, options)
                .then(function (response) {
                    callback(null, response);
                })
                .catch(function (error) { 
                    callback(error, null);
                });
        },

        sendToTopic: function (topic, payload, options, callback) {
            admin.messaging().sendToTopic(topic, payload, options)
                .then(function (response) {
                    callback(null, response);
                })
                .catch(function (error) { 
                    callback(error, null);
                });
        },

        sendToCondition: function (condition, payload, options, callback) {
            admin.messaging().sendToCondition(condition, payload, options)
                .then(function (response) {
                    callback(null, response);
                })
                .catch(function (error) { 
                    callback(error, null);
                });
        },

        getToken: function (callback) {
            callback(); 
        } 
    }
};