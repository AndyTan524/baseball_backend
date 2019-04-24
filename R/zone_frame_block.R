library(dplyr)

all_files <- list.files(path = paste("BIS/"))

all_YTD <- all_files[grep("^YTDFielding_",all_files)]

master_YTD <- data.frame(matrix(NA,nrow=1,ncol=12))

colnames(master_YTD) <- c("MLBId","PlayerName","BallsInZone","MissedBallsInZone","Block","Frame","Team","INN","Pos","Position","Date","TeamNbr")

for(i in 2:183)
{
  YTD2 <- read.csv(paste("BIS/",all_YTD[i],sep=""))
  YTD <- read.csv(paste("BIS/",all_YTD[i-1],sep=""))
  
  ytd_col <- c("LastName","FirstName","MLBId","PlayerName")
  
  for(m in 1:length(ytd_col))
  {
    YTD[,ytd_col[m]] <- as.character(YTD[,ytd_col[m]])
    YTD2[,ytd_col[m]] <- as.character(YTD2[,ytd_col[m]])
    
  }
  
  for(j in 1:nrow(YTD))
  {
    YTD$FirstName[j] <- sub(paste(YTD$LastName[j]," ",sep=""), "", YTD$PlayerName[j])
  }
  
  for(k in 1:nrow(YTD2))
  {
    YTD2$FirstName[k] <- sub(paste(YTD2$LastName[k]," ",sep=""), "", YTD2$PlayerName[k])
  }
  
  
  YTD$PlayerName <- paste(YTD$FirstName," ",YTD$LastName,sep="")
  
  YTD2$PlayerName <- paste(YTD2$FirstName," ",YTD2$LastName,sep="")
  
  # Create four new columns
  
  YTD$previous_missed <- ""
  YTD$available_missed <- ""
  YTD$previous_outs <- ""
  YTD$available_outs <- ""
  YTD$Position <- ""
  
  YTD2$previous_missed <- ""
  YTD2$available_missed <- ""
  YTD2$previous_outs <- ""
  YTD2$available_outs <- ""
  YTD2$Zone <- ""
  YTD2$Block <- ""
  YTD2$Frame <- ""
  YTD2$Position <- ""
  
  
  YTD2 <- select(YTD2, PlayerId, LastName, FirstName, MLBId, Zone, Block, Frame,INN,PlayerName, Team, TeamNbr, Pos, Position, G, GS, INN, BallsInZone, MissedBallsInZone, previous_missed, 
                 available_missed, OutsOutOfZone, previous_outs, available_outs, CBlockingRuns, CFramingRuns)
  
  YTD <- select(YTD,LastName, FirstName, MLBId, PlayerName, PlayerId, Team, INN ,TeamNbr, Pos, G, GS, INN, BallsInZone, MissedBallsInZone, OutsOutOfZone, CBlockingRuns, CFramingRuns)
  
  
  position <- c("P","CA","1B","2B","3B","SS","LF","CF","RF")
  
  for(nn in 1:length(position))
  {
    YTD2$Position[which(YTD2$Pos %in% nn)] <- position[nn]
  }
  
  # Replace NA with 0 in last five columns
  
  YTD$BallsInZone[which(YTD$BallsInZone %in% NA)] <- 0
  YTD$MissedBallsInZone[which(YTD$MissedBallsInZone %in% NA)] <- 0
  YTD$OutsOutOfZone[which(YTD$OutsOutOfZone %in% NA)] <- 0
  YTD$CBlockingRuns[which(YTD$CBlockingRuns %in% NA)] <- 0
  YTD$CFramingRuns[which(YTD$CFramingRuns %in% NA)] <- 0
  
  
  YTD2$BallsInZone[which(YTD2$BallsInZone %in% NA)] <- 0
  YTD2$MissedBallsInZone[which(YTD2$MissedBallsInZone %in% NA)] <- 0
  YTD2$OutsOutOfZone[which(YTD2$OutsOutOfZone %in% NA)] <- 0
  YTD2$CBlockingRuns[which(YTD2$CBlockingRuns %in% NA)] <- 0
  YTD2$CFramingRuns[which(YTD2$CFramingRuns %in% NA)] <- 0
  
  # Calculate previous_missed
  
  for(l in 1:nrow(YTD2))
  {
    
    if(nrow(YTD[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l]) & (YTD$TeamNbr %in% YTD2$TeamNbr[l])),]) == 1)
    {
      YTD2$previous_missed[l] <- YTD$MissedBallsInZone[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l]) & (YTD$TeamNbr %in% YTD2$TeamNbr[l]))]
    }
    
    if(!(nrow(YTD[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l]) & (YTD$TeamNbr %in% YTD2$TeamNbr[l])),]) == 1))
    {
      YTD2$previous_missed[l] <- 0
    }
  }
  
  # Calculate previous_missed
  
  for(l in 1:nrow(YTD2))
  {
    
    if(nrow(YTD[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l]) & (YTD$TeamNbr %in% YTD2$TeamNbr[l])),]) == 1)
    {
      YTD2$previous_missed[l] <- YTD$MissedBallsInZone[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l]) & (YTD$TeamNbr %in% YTD2$TeamNbr[l]))]
    }
    
    if(!(nrow(YTD[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l]) & (YTD$TeamNbr %in% YTD2$TeamNbr[l])),]) == 1))
    {
      YTD2$previous_missed[l] <- 0
    }
  }
  
  
  YTD2$previous_missed <- as.double(YTD2$previous_missed)
  
  # Calculate available_missed
  
  YTD2$available_missed <- YTD2$MissedBallsInZone - YTD2$previous_missed
  
  
  # Calculate previous_outs
  
  for(l in 1:nrow(YTD2))
  {
    
    if(nrow(YTD[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l]) & (YTD$TeamNbr %in% YTD2$TeamNbr[l])),]) == 1)
    {
      YTD2$previous_outs[l] <- YTD$OutsOutOfZone[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l]) & (YTD$TeamNbr %in% YTD2$TeamNbr[l]))]
    }
    
    if(!(nrow(YTD[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l]) & (YTD$TeamNbr %in% YTD2$TeamNbr[l])),]) == 1))
    {
      YTD2$previous_outs[l] <- 0
    }
  }
  
  
  YTD2$previous_outs<- as.double(YTD2$previous_outs)
  
  
  # Calculate available_outs
  
  YTD2$available_outs <- YTD2$OutsOutOfZone - YTD2$previous_outs
  
  
  # Calculate Zone
  
  # Pitcher
  
  ifelse((YTD2$available_outs > 0) & (YTD2$Pos == 1), YTD2$Zone[which((YTD2$available_outs > 0) & (YTD2$Pos == 1))] <- ((YTD2$available_missed[which((YTD2$available_outs > 0) & (YTD2$Pos == 1))] * -0.39) + (YTD2$available_outs[which((YTD2$available_outs > 0) & (YTD2$Pos == 1))] * 0.2) - 0.09), YTD2$Zone[which(((YTD2$available_outs == 0) & (YTD2$Pos == 1)))] <- (YTD2$available_missed[which(((YTD2$available_outs == 0) & (YTD2$Pos == 1)))] * -0.39))
  
  #1B
  
  ifelse((YTD2$available_outs > 0) & (YTD2$Pos == 3), YTD2$Zone[which((YTD2$available_outs > 0) & (YTD2$Pos == 3))] <- ((YTD2$available_missed[which((YTD2$available_outs > 0) & (YTD2$Pos == 3))] * -0.55) + (YTD2$available_outs[which((YTD2$available_outs > 0) & (YTD2$Pos == 3))] * 0.28) - 0.13), YTD2$Zone[which(((YTD2$available_outs == 0) & (YTD2$Pos == 3)))] <- (YTD2$available_missed[which(((YTD2$available_outs == 0) & (YTD2$Pos == 3)))] * -0.55))
  
  
  #2B
  
  ifelse((YTD2$available_outs > 0) & (YTD2$Pos == 4), YTD2$Zone[which((YTD2$available_outs > 0) & (YTD2$Pos == 4))] <- ((YTD2$available_missed[which((YTD2$available_outs > 0) & (YTD2$Pos == 4))] * -0.39) + (YTD2$available_outs[which((YTD2$available_outs > 0) & (YTD2$Pos == 4))] * 0.2) - 0.09), YTD2$Zone[which(((YTD2$available_outs == 0) & (YTD2$Pos == 4)))] <- (YTD2$available_missed[which(((YTD2$available_outs == 0) & (YTD2$Pos == 4)))] * -0.39))
  
  #3B
  
  ifelse((YTD2$available_outs > 0) & (YTD2$Pos == 5), YTD2$Zone[which((YTD2$available_outs > 0) & (YTD2$Pos == 5))] <- ((YTD2$available_missed[which((YTD2$available_outs > 0) & (YTD2$Pos == 5))] * -0.35) + (YTD2$available_outs[which((YTD2$available_outs > 0) & (YTD2$Pos == 5))] * 0.18) - 0.12), YTD2$Zone[which(((YTD2$available_outs == 0) & (YTD2$Pos == 5)))] <- (YTD2$available_missed[which(((YTD2$available_outs == 0) & (YTD2$Pos == 5)))] * -0.35))
  
  
  #SS
  
  ifelse((YTD2$available_outs > 0) & (YTD2$Pos == 6), YTD2$Zone[which((YTD2$available_outs > 0) & (YTD2$Pos == 6))] <- ((YTD2$available_missed[which((YTD2$available_outs > 0) & (YTD2$Pos == 6))] * -0.39) + (YTD2$available_outs[which((YTD2$available_outs > 0) & (YTD2$Pos == 6))] * 0.2) - 0.09), YTD2$Zone[which(((YTD2$available_outs == 0) & (YTD2$Pos == 6)))] <- (YTD2$available_missed[which(((YTD2$available_outs == 0) & (YTD2$Pos == 6)))] * -0.39))
  
  
  #OF
  
  ifelse((YTD2$available_outs > 0) & (YTD2$Pos %in% c(7,8,9)), YTD2$Zone[which((YTD2$available_outs > 0) & (YTD2$Pos %in% c(7,8,9)))] <- ((YTD2$available_missed[which((YTD2$available_outs > 0) & (YTD2$Pos %in% c(7,8,9)))] * -0.55) + (YTD2$available_outs[which((YTD2$available_outs > 0) & (YTD2$Pos %in% c(7,8,9)))] * 0.28) - 0.13), YTD2$Zone[which(((YTD2$available_outs == 0) & (YTD2$Pos %in% c(7,8,9))))] <- (YTD2$available_missed[which(((YTD2$available_outs == 0) & (YTD2$Pos %in% c(7,8,9))))] * -0.55))
  
  
  # Block
  
  YTD2$Block[which(YTD2$Pos == 2)] <- round((YTD2$CBlockingRuns[which(YTD2$Pos == 2)]) / (YTD2$INN[which(YTD2$Pos == 2)] / 9),digits=2)
  
  # Frame
  
  YTD2$Frame[which(YTD2$Pos == 2)] <- round((YTD2$CFramingRuns[which(YTD2$Pos == 2)]) / (YTD2$INN[which(YTD2$Pos == 2)] / 9),digits=2)
  
  YTD2$Date <- substr(all_YTD[i],13,20)
  
  YTD2 <- YTD2[,c("MLBId","PlayerName","BallsInZone","MissedBallsInZone","Block","Frame","Team","INN","Pos","Position","Date","TeamNbr")]

  master_YTD <- rbind(master_YTD, YTD2)
  
  print(paste0(i," of 183"))
}

master_YTD <- master_YTD[!master_YTD$MLBId %in% c(NA,"NA"),]

for(v in 3:5){master_YTD[,v] <- as.double(master_YTD[,v])}
for(w in 5:7){master_YTD[,w] <- as.double(master_YTD[,w])}

master_YTD$OUT <- ((master_YTD$INN %/% 1) * 3) + ((master_YTD$INN %% 1) * 10)

YTD_STAT <- master_YTD %>% group_by(MLBId,PlayerName,Position,Team) %>% summarize(Zone=sum(Zone,na.rm=TRUE),Block=sum(Block,na.rm=TRUE),Frame=sum(Frame,na.rm=TRUE))

YTD_STAT2 <- master_YTD %>% group_by(MLBId,PlayerName,Position,Team) %>% summarize(Block=sum(Block,na.rm=TRUE),Frame=sum(Frame,na.rm=TRUE))

YTD_STAT2 <- YTD_STAT2[YTD_STAT2$Position %in% "CA",]

YTD_STAT <- YTD_STAT[order(YTD_STAT$Zone,decreasing=TRUE),]

write.csv(YTD_STAT2, "block_frame_only.csv",row.names=FALSE)

write.csv(YTD_STAT,"zone_block_frame.csv",row.names = FALSE)