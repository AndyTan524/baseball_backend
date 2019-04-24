library(gsheet)
play_pos <- read.csv("playerid_with_pos.csv")

#Read MLe from Google Doc

start_MLe <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1Icz21vlg7trhNy_UZJXtHsp1bGkwksAalLmHQ2bjWn4/edit?usp=sharing')

if(nrow(start_MLe) > 0){

start_MLe <- as.data.frame(start_MLe)

start_MLe <- start_MLe[!start_MLe$LastName == "",]

start_MLe[is.na(start_MLe)] <- 0

start_MLe$GameDate <- ""
start_MLe$DEC <- ""
start_MLe$X1 <- ""
start_MLe$X2 <- ""
start_MLe$X3 <- ""
start_MLe$X4 <- ""
start_MLe$X5 <- ""
start_MLe$X6 <- ""
start_MLe$X7 <- ""
start_MLe$PlayerName <- ""
start_MLe$GameString <- ""
start_MLe$GameId <- ""
start_MLe$used <- ""
start_MLe$uniqueId <- ""

start_MLe$GameDate <- as.Date(start_MLe$GameDate,format="%Y-%m-%d",origin="1970-01-01")

start_MLe <- start_MLe[,c("GameDate","FirstName","LastName","LW","DEC","X1","X2","X3","X4","X5","X6","X7","IP","BFP","H","X1B","X2B",
                          "X3B","HR","ER","SH","SF","HBP","BB","K","WP","BLK","IR","IRS","GB","FB","LD","POPU","SB","CS","PKO","OUT",
                          "MLBId","PlayerName","GameString","GameId","used","uniqueId")]

start_MLe$PlayerName <- as.character(paste(start_MLe$FirstName,start_MLe$LastName,sep=" "))

for(u in 1:nrow(start_MLe))
{
  start_MLe$MLBId[u] <- as.character(play_pos$MLBId[which(play_pos$PlayerName %in% start_MLe$PlayerName[u])])
  
}

start_MLe$GameString <- "MLE"
start_MLe$GameId <- "MLE"
start_MLe$uniqueId <- as.character(paste(start_MLe$MLBId,start_MLe$GameString,start_MLe$GameId,sep=" "))

pitching_SP <- read.csv("Pitching/Pitching_Master_Starts2.csv")

start_MLe_new <- start_MLe[!start_MLe$uniqueId %in% pitching_SP$uniqueId,]

pitching_SP <- rbind(pitching_SP,start_MLe_new)
}

if(nrow(start_MLe_new) > 0)
{
  write.csv(pitching_SP,"Pitching/Pitching_Master_Starts2.csv",row.names = FALSE)
}

relief_MLe <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1A99etLUNjFlxKDn3FfNoghPs1ZdV6X6QZap_ORd0IHo/edit?usp=sharing')

if(nrow(relief_MLe) > 0){
relief_MLe <- as.data.frame(relief_MLe)

relief_MLe <- relief_MLe[!relief_MLe$LastName == "",]

relief_MLe[is.na(relief_MLe)] <- 0

relief_MLe$GameDate <- ""
relief_MLe$DEC <- ""
relief_MLe$X1 <- ""
relief_MLe$X2 <- ""
relief_MLe$X3 <- ""
relief_MLe$X4 <- ""
relief_MLe$X5 <- ""
relief_MLe$X6 <- ""
relief_MLe$X7 <- ""
relief_MLe$PlayerName <- ""
relief_MLe$GameString <- ""
relief_MLe$GameId <- ""
relief_MLe$used <- ""
relief_MLe$uniqueId <- ""

relief_MLe$GameDate <- as.Date(relief_MLe$GameDate,format="%Y-%m-%d")

relief_MLe <- relief_MLe[,c("GameDate","FirstName","LastName","LW","DEC","X1","X2","X3","X4","X5","X6","X7","IP","BFP","H","X1B","X2B",
                            "X3B","HR","ER","SH","SF","HBP","BB","K","WP","BLK","IR","IRS","GB","FB","LD","POPU","SB","CS","PKO","OUT",
                            "MLBId","PlayerName","GameString","GameId","used","uniqueId")]

relief_MLe$PlayerName <- as.character(paste(relief_MLe$FirstName,relief_MLe$LastName,sep=" "))

for(u in 1:nrow(relief_MLe))
{
  relief_MLe$MLBId[u] <- play_pos$MLBId[which(play_pos$PlayerName %in% relief_MLe$PlayerName[u])]
  
  relief_MLe$MLBId[u] <- ifelse(relief_MLe$PlayerName[u] == "Jose Ramirez",relief_MLe$MLBId[u] <- 542432,relief_MLe$MLBId[u] <- play_pos$MLBId[which(play_pos$PlayerName %in% relief_MLe$PlayerName[u])])
}

relief_MLe$GameString <- "MLE"
relief_MLe$GameId <- "MLE"
relief_MLe$uniqueId <- as.character(paste(relief_MLe$MLBId,relief_MLe$GameString,relief_MLe$GameId,sep=" "))

pitching_RP <- read.csv("Pitching/Pitching_Master_RP2.csv")

relief_MLe_new <- relief_MLe[!relief_MLe$uniqueId %in% pitching_RP$uniqueId,]

pitching_RP <- rbind(pitching_RP,relief_MLe_new)
}

if(nrow(relief_MLe_new) > 0)
{
  write.csv(pitching_RP,"Pitching/Pitching_Master_RP2.csv",row.names = FALSE)
}