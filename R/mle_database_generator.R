library(openxlsx)

depth_chart <- read.xlsx(xlsxFile="WSFB Depth Charts.xlsx", sheet = "Transaction Log")
crunch <- read.csv("crunch.csv")
fangraph <- read.csv("fangraphs.csv")

fangraph <- fangraph[!fangraph$Team == "Mexican (AAA)",]

colnames(depth_chart) <- c("Date","MLBId","Player","POS","Club","Transaction","Acquiring Club","Notes","")

depth_chart <- depth_chart[!depth_chart$Date %in% c(NA,"Date"),]

depth_chart$Date <- as.double(depth_chart$Date)

depth_chart$Date <- as.Date(depth_chart$Date,origin="1899-12-30")

depth_chart$MLBId <- as.double(depth_chart$MLBId)

crunch$MLE_Eligibility <- ""

for(i in 1:nrow(crunch))
{
  if(crunch$fg_id[i] %in% fangraph$playerid)
  {
    crunch$MLE_Eligibility[i] <- "YES"
  }
  
  if(!crunch$fg_id[i] %in% fangraph$playerid)
  {
    crunch$MLE_Eligibility[i] <- "NO"
  }
}

write.csv(crunch,"crunchbaseball.csv",row.names = FALSE)