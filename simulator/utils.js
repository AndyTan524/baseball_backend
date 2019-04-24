utils = {};
    utils.getRandom = function(low, hi) {
        n = Math.floor((Math.random() * hi) + low);
        return (n);
    }

    utils.positions = ["PH", "P", "CA", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "DH"];
    
    utils.getPositionNumber =function ( pos ) {
        var p = utils.positions.indexOf( pos );
        if( p < 0 )
            p = 0;
        return p;
    }
    
    // list of positions.. note there is no 0 position, so just added a blank placeholder.
    

