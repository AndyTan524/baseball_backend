col <- c("LastName", "FirstName", "MLBId", "PlayerName", "Team")
col2 <- c("LastName", "FirstName", "MLBId", "PlayerName", "Team","Pos")
master_bat <- data.frame(matrix(NA,nrow=1,ncol=length(col)))
master_pit <- data.frame(matrix(NA,nrow=1,ncol=length(col)))
master_field <- data.frame(matrix(NA,nrow=1,ncol=length(col2)))

colnames(master_bat) <- col
colnames(master_pit) <- col
colnames(master_field) <- col2
file <- list.files("BIS/")
batting_file <- file[grepl("Batting",file)]
pitch_file <- file[grepl("Pitching",file)]
field_file <- file[grepl("Fielding",file)]
field_file <- field_file[!grepl("YTD",field_file)]

for(i in 1:length(batting_file)){
    
  batting <- read.csv(paste("BIS/",batting_file[i],sep=""))
  batting <- batting[!(batting$GameId %in% NA),]
  batting <- select(batting, LastName, FirstName, MLBId, PlayerName, Team)
  master_bat <- rbind(master_bat, batting)
  
  pitching <- read.csv(paste("BIS/",pitch_file[i],sep=""))
  pitching <- pitching[!(pitching$GameId %in% NA),]
  pitching <- select(pitching, LastName, FirstName, MLBId, PlayerName, Team)
  master_pit <- rbind(master_pit, pitching)
  
  field <- read.csv(paste("BIS/",field_file[i],sep=""))
  field <- field[!(field$GameId %in% NA),]
  field <- select(field, LastName, FirstName, MLBId, PlayerName, Team, Pos)
  master_field <- rbind(master_field, field)
}

master_bat <- unique(master_bat)
master_pit <- unique(master_pit)
master_field <- unique(master_field)

master <- rbind(master_bat, master_pit)

master <- unique(master)

master <- master[!(master$MLBId %in% NA),]

master_field <- master_field[!(master_field$MLBId %in% NA),]

for(j in 1:nrow(master_field))
{
  master_field$FirstName[j] <- sub(paste(master_field$LastName[j]," ",sep=""), "", master_field$PlayerName[j])
}

master_field$PlayerName <- paste(master_field$FirstName," ", master_field$LastName,sep="")


write.csv(master,"fakeid.csv", row.names = FALSE)
write.csv(master_field, "playerid_with_pos.csv", row.names = FALSE)

