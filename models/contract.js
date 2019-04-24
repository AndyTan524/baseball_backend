var mongoose = require('mongoose');

var contractSchema = new mongoose.Schema({
              "MlbId" : String,
              "FirstName" : String,
              "LastName" : String,
              "FullName" :String,
              "PBType" : String,
              "PBThreshholds" : String,
              "PBDollars" : String,
              "AwardType" : String,
              "AwardDollars" : String,
              "EscalatorType" :String,
              "EscalatorThreshholds" : String,
              "EscalatorPlatformYear" : String,
              "EscalatorYear" : String,
              "EscalatorDollarsIncrease" :String,
              "EscalatorDollarsNewSalary" : String,
              "TradeBonus" : String,
              "OptionYear" : String,
              "OptionPlatformYear" : String,
              "OptionIncentives" : String,
              "OptionIncentiveThreshholds" : String,
              "OptionNotes" : String,
              "Arbitration Opt In" : String,
              "Other" : String,
              "ContractNotes" : String
});

mongoose.model('Contract', contractSchema);