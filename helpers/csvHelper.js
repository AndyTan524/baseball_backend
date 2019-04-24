var fs = require('fs');
var parse = require('csv-parse');

var csvjson = require('csvjson');
const csv = require('csvtojson')

module.exports = {
    parse: function (stream, callback) {
        const csv = require('csvtojson')
        var json = [];
        csv()
            .fromStream(stream) 
            .on('json', (jsonObj, rowIndex) => { 
                json.push(jsonObj);
            })
            .on('done', (error) => {
                callback(json)
            })
        .on('error', (error) => {
            callback([]);
        })
        
    },
    parseFlat: function (stream, callback) {
        const csv = require('csvtojson')
        var json = [];
        csv({noheader:true})
            .fromStream(stream) 
            .on('csv', (row, rowIndex) => { 
                json.push(row);
            })
            .on('done', (error) => {
                callback(json)
            })
        .on('error', (error) => {
            callback([]);
        })
        
    }

}