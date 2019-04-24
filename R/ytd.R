YTD <- function(){

# Convert date to the format that is required for loading BIS data

date2 <- as.Date(formatted_date, format="%Y-%m-%d", origin = "1970-01-01") + 1
date2 <- strftime(strptime(date2, "%Y-%m-%d"), "%Y%m%d")

# read two YTD files

YTD2 <- read.csv(paste("BIS/YTDFielding_",date2,".csv",sep=""))
YTD <- read.csv(paste("BIS/YTDFielding_",date,".csv",sep=""))

# Characterize columns

ytd_col <- c("LastName","FirstName","MLBId","PlayerName")

for(i in 1:length(ytd_col))
{
  YTD[,ytd_col[i]] <- as.character(YTD[,ytd_col[i]])
  YTD2[,ytd_col[i]] <- as.character(YTD2[,ytd_col[i]])
  
}

# Fix name issues


for(j in 1:nrow(YTD))
{
  YTD$FirstName[j] <- sub(paste(YTD$LastName[j]," ",sep=""), "", YTD$PlayerName[j])
}

for(k in 1:nrow(YTD2))
{
  YTD2$FirstName[k] <- sub(paste(YTD2$LastName[k]," ",sep=""), "", YTD2$PlayerName[k])
}

# Rearrange from 'lastname firstname' to 'firstname lastname'

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

# Sort just like master version of YTD

YTD2 <- select(YTD2, PlayerId, LastName, FirstName, MLBId, Zone, Block, Frame, PlayerName, Team, TeamNbr, Pos, Position, G, GS, INN, BallsInZone, MissedBallsInZone, previous_missed, 
       available_missed, OutsOutOfZone, previous_outs, available_outs, CBlockingRuns, CFramingRuns)

YTD <- select(YTD,LastName, FirstName, MLBId, PlayerName, PlayerId, Team, TeamNbr, Pos, G, GS, INN, BallsInZone, MissedBallsInZone, OutsOutOfZone, CBlockingRuns, CFramingRuns)

# Fill in position column in YTD2

position <- c("P","CA","1B","2B","3B","SS","LF","CF","RF")

for(m in 1:length(position))
{
  YTD2$Position[which(YTD2$Pos %in% m)] <- position[m]
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
  
  if(nrow(YTD[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l])),]) == 1)
  {
    YTD2$previous_missed[l] <- YTD$MissedBallsInZone[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l]))]
  }
  
  if(!(nrow(YTD[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l])),]) == 1))
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
  
  if(nrow(YTD[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l])),]) == 1)
  {
    YTD2$previous_outs[l] <- YTD$OutsOutOfZone[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l]))]
  }
  
  if(!(nrow(YTD[which((YTD$MLBId %in% YTD2$MLBId[l]) & (YTD$Pos %in% YTD2$Pos[l])),]) == 1))
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

}