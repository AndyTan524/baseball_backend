module.exports = {
    "Unknown": 0,
    "UnavailableForTrade": 0,
    "ActiveRoster": 1,
    "Inactive": 2,
    "FortyManInactive": 3,
    "OptionedToAAA": 4,
    "OptionedToAA" :5,


    "DL": 10,
    "DL10": 11,
    "DL60": 12,
    "DLConcussion": 13,
    "DLRehab": 14,
    "DLMinors": 15,
    "DLEnd": 19,


    "LeaveLists": 50,
    "Bereavement": 51,
    "Paternity": 52,
    "Restricted": 53,
    "Suspended": 54,
    "DesignateForAssignment": 55,
    "LeaveEnd": 59,

    "FreeAgent": 100,
    "FreeAgentOfferedContract": 101,
    "FreeAgentEnd": 110,

    "Waivers": 200,
    "OutrightWaivers": 201,
    "OutrightWaiversAA": 202,
    "Release": 203,
    "WaiversEnd": 210,


    "TradeWaivers": 500,
    "TradeWaiversPending": 501,
    "TradeWaiversCleared": 502,
    "TradeWaiversClaimed": 503,
    "TradeWaiversEnd": 510,

    "TradingBlock": 520,
    "RequestTradingBlock": 521,
    "OnTradingBlock": 522,
    "CancelTrading": 523,
    "TradingBlockEnd": 529,

    "Claimed": 600,

    "RemoveWaivers": 700,





    "Details": {
        "Unknown": { "Text": "Unknown", "Start": "", "Min": "", "Max": "", msg: ""  },
        "ActiveRoster": { "Text": "Active Roster", "Start": "", "Min": "", "Max": "", msg: "moved {{name}} to the active roster."  },
        "Inactive": { "Text": "Inactive", "Start": "", "Min": "", "Max": "" , msg: "optioned {{name}} to the {{level}} {{minorteamname}}."},
        "FortyManInactive": { "Text": "Forty-Man Inactive", "Start": "", "Min": "", "Max": "", msg: "removed {{name}} from the 40-man roster." },
        "OptionedToAAA": { "Text": "On optional assignment to Triple-A", "Start": "", "Min": "", "Max": "", msg: "placed {{name}} on optional assignment to Triple-A." },
        "OptionedToAA": { "Text": "On optional assignment to Double-A", "Start": "", "Min": "", "Max": "", msg: "placed {{name}} on optional assignment to Double-A." },

        /*
        pre-season dl's
        7-day DL:	April 5th
10-day DL:	April 8th
60-day DL:	May 28th
paternity and bereavement: 4/3/18
        format for 9am: 2018-02-16T16:00:00Z
        format for 2018 season: 2018-04-08T16:00:00Z
        */

        "DL": { "Text": "DL", "Start": "", "Min": "", "Max": "", "NextStatus": "DLRehab" },
        "DL10": { "Text": "10 Day DL", "Preseason": "", "Start": "-3 days", "Min": "days:10", "Max": "days:10", "NextStatus": "DLRehab", msg: "placed {{name}} on the 10-day disabled list."  },
        "DL60": { "Text": "60 Day DL", "Preseason": "","Start": "-3 days minus DL", "Min": "days:60", "Max": "days:60", "NextStatus": "DLRehab", msg: "placed {{name}} on the 60-day disabled list."  },
        "DLConcussion": { "Text": "Concussion List", "Preseason": "","Start": "tomorrow", "Min": "days:7", "Max": "days:7", "NextStatus": "DL10" , msg: "placed {{name}} on the 7-day concussion list." },
        "DLRehab": { "Text": "Rehab", "Start": "now", "Min": "days:20:30", "Max": "days:20:30", "NextStatus": "DLRehab", msg: "moved {{name}} to rehab." },
        "DLMinors": { "Text": "Minor League DL", "Start": "", "Min": "", "Max": "", msg: "placed {{name}} on the minor league disabled list." },
        "DLEnd": { "Text": "DLEnd", "Start": "", "Min": "", "Max": "" },


        "LeaveLists": { "Text": "LeaveLists", "Start": "", "Min": "", "Max": "", "NextStatus": "" },
        "Bereavement": { "Text": "On bereavement leave", "Preseason": "", "Start": "tomorrow", "Min": "days:3", "Max": "days:7", "NextStatus": "Return",  msg: "placed {{name}} on the bereavement list." },
        "Paternity": { "Text": "On paternity leave","Preseason": "", "Start": "tomorrow", "Min": "days:3", "Max": "days:7", "NextStatus": "Return", msg: "placed {{name}} on the paternity leave." },
        "Restricted": { "Text": "On restricted list", "Start": "tomorrow", "Min": "commisioner", "Max": "commisioner", "NextStatus": "Return", msg: "placed {{name}} on the restricted list." },
        "Suspended": { "Text": "On suspended list", "Start": "tomorrow", "Min": "commisioner", "Max": "commisioner", "NextStatus": "Return",  msg: "placed {{name}} on the suspended list." },
        "DesignateForAssignment": { "Text": "Designated for assignment", "Start": "tomorrow", "Min": "days:5", "Max": "days:5", "NextStatus": "OutrightWaivers", msg: "designated {{name}} for assignment." },
        "LeaveEnd": { "Text": "LeaveEnd", "Start": "", "Min": "", "Max": "" },


        "Waivers": { "Text": "Waivers", "Start": "tomorrow", "Min": "", "Max": "" },
        "OutrightWaivers": { "Text": "On Outright Waivers (AAA)", "Start": "tomorrow", "Min": "days:2", "Max": "days:2", "NextStatus": "Inactive", msg: "placed {{name}} on Outright Waivers (AAA)."  },
        "OutrightWaiversAA": { "Text": "On Outright Waivers (AA)", "Start": "tomorrow", "Min": "days:2", "Max": "days:2", "NextStatus": "Inactive", msg: "placed {{name}} on Outright Waivers (AA)."  },
        "Release": { "Text": "Released", "Start": "tomorrow", "Min": "days:2", "Max": "days:2", "NextStatus": "FreeAgent", msg: "placed {{name}} on Unconditional Release Waivers." },

        "FreeAgent": { "Text": "Free Agent", "Start": "", "Min": "", "Max": "", msg: "{{name}} released." },
        "FreeAgentOfferedContract": { "Text": "Contract(s) offered", "Start": "now", "Min": "", "Max": "", "NextStatus": "Inactive" },

        "UnavailableForTrade": { "Text": "UnavailableForTrade", "Start": "", "Min": "", "Max": "" },
        /*
         "RequestTradingBlock" : {"Text": "RequestTradingBlock", "Start": "nexthour", "Min": "hours:48", "Max": "hours:48", "NextStatus": "OnTradingBlock"},
 
         "OnTradingBlock" : {"Text": "OnTradingBlock", "Start": "now", "Min": "", "Max": "", "NextStatus": ""},
     
         "TradeWaivers" : {"Text": "TradeWaivers", "Start": "", "Min": "", "Max": ""},
         "TradeWaiversPending" : {"Text": "TradeWaiversPending", "Start": "now", "Min": "hours:48", "Max": "hours:48", "NextStatus": "TradeWaiversCleared"},
         "TradeWaiversCleared" : {"Text": "TradeWaiversCleared", "Start": "now", "Min": "hours:48", "Max": "hours:48", "NextStatus": "TradeWaiversClaimed"},
         "TradeWaiversClaimed" : {"Text": "TradeWaiversClaimed", "Start": "now", "Min": "hours:48", "Max": "hours:48", "NextStatus": "Claimed"},
        */

        "RequestTradingBlock": { "Text": "Requested to be on Trading Block", "Start": "tomorrow", "Min": "days:2", "Max": "days:2", "NextStatus": "OnTradingBlock", msg: "made {{name}} available for trading (pending two-day waiting period). "   },

        "OnTradingBlock": { "Text": "OnTradingBlock", "Start": "", "Min": "", "Max": "", "NextStatus": "" , msg: "{{name}} is available for trades."  },
        "TradeWaivers": { "Text": "Trade waivers", "Start": "", "Min": "", "Max": "" },
        "TradeWaiversPending": { "Text": "Trade waivers pending", "Start": "tomorrow", "Min": "days:2", "Max": "days:2", "NextStatus": "TradeWaiversCleared" , msg: "placed {{name}} on trade waivers."  },
        "TradeWaiversCleared": { "Text": "Trade waivers cleared", "Start": "tomorrow", "Min": "days:2", "Max": "days:2", "NextStatus": "TradeWaiversClaimed", msg: "{{name}} cleared trade waivers."   },
        "TradeWaiversClaimed": { "Text": "TradeWaiversClaimed", "Start": "tomorrow", "Min": "days:2", "Max": "days:2", "NextStatus": "Claimed" },

        "CancelTrading": { "Text": "Trade waivers claimed", "Start": "", "Min": "", "Max": "", "NextStatus": "" },

        "Claimed": { "Text": "Claimed from waivers", "Start": "match", "Min": "", "Max": "", "NextStatus": "" },
        "RemoveWaivers": { "Text": "Removed from waivers", "Start": "", "Min": "", "Max": "", "NextStatus": "" },
    }
}