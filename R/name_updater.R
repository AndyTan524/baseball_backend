# Set up something to update this on a daily basis by loading new daily data and check for new player.

master <- read.csv("BIS/2016_player_list.csv")

today <- today() - 1

today <- format(today,"%Y%m%d")
batting <- read.csv(paste("BIS/Batting_",today,".csv",sep=""))
pitching <- read.csv(paste("BIS/Pitching_",today,".csv",sep=""))

batting <- batting[!(batting$GameId %in% NA),]
pitching <- pitching[!(pitching$GameId %in% NA),]

batting <- select(batting, LastName, FirstName, MLBId, PlayerName, Team)
pitching <- select(pitching, LastName, FirstName, MLBId, PlayerName, Team)

master2 <- rbind(batting,pitching)

master2 <- unique(master2)

master2 <- master2[!(master2$MLBId %in% master$MLBId),]

for(j in 1:5)
{
  master2[,j] <- as.character(master2[,j])
}

for(k in 1:nrow(master2))
{
  master2$FirstName[k] <- sub(paste(master2$LastName[k]," ",sep=""),"",master2$PlayerName[k])
}

master2$PlayerName <- paste(master2$FirstName," ",master2$LastName,sep="")


master <- rbind(master,master2[!(master2$MLBId %in% master$MLBId),])

write.csv(master,"BIS/2016_player_list.csv",row.names = FALSE)