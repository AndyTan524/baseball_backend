var date = new Date();
var dateString = date.getFullYear() + "/" + ("0" + (date.getMonth() + 1)).slice(-2) + "/" + ("0" + (date.getDate() - 1)).slice(-2);

console.log( date + " " +  date.toLocaleTimeString());
// process.exit();

var count = 0;
setInterval(()=>{
    console.log( count++ );
    if ( count == 30) {
        console.log("exiting inner loop.");
        process.exit();
    }
}, 2000);