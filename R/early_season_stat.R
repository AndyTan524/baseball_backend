library(dplyr)

boxscores <- list.files("oldbox/")

boxname <- c("GameDate","FirstName","LastName","POS","LW","Bonus","Bases.Taken","Outs.on.Base","Field","E","Zone","Block","Frame",
             "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB","LD","POPU",
             "BH","IFH","OUTS")

master_box <- data.frame(matrix(NA,nrow=1,ncol=length(boxname)))

colnames(master_box) <- boxname

for(i in 1:length(boxscores))
{
  box <- read.csv(paste(getwd(),"/oldbox/",boxscores[i],sep=""))
  
  colnames(box) <- boxname
  
  box <- box[,c("GameDate","FirstName","LastName","POS","LW","Bonus","Bases.Taken","Outs.on.Base","Field","E","Zone","Block","Frame",
                "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB","LD","POPU",
                "BH","IFH","OUTS")]
  master_box <- rbind(master_box,box)
}

master_box$PlayerName <- paste(master_box$FirstName,master_box$LastName,sep=" ")

master_box <- master_box[!master_box$PlayerName %in% c("NA NA","FirstName LastName"),]

master_box <- master_box[!master_box$GameDate == "",]

master_box$MLBId <- ""

play_pos <- read.csv("playerid_with_pos.csv")

player <- unique(play_pos$PlayerName)

for(j in 1:length(player))
{
  mlbid <- play_pos$MLBId[which(play_pos$PlayerName %in% player[j])[1]]
  
  master_box$MLBId[which(master_box$PlayerName %in% play_pos$PlayerName[which(play_pos$PlayerName %in% player[j])[1]])] <- mlbid
}

player_name <- unique(master_box$PlayerName[which(master_box$MLBId == "")])

for(i in 1:length(player_name))
{
  master_box$MLBId[which(master_box$PlayerName %in% player_name[i])] <- readline(prompt=paste("Give MLBId for ",player_name[i],":",sep=""))
}

master_box$GameDate <- as.Date(master_box$GameDate,format = "%m/%d/%y")

# Baserunning

which_NA <- which(master_box$Bases.Taken %in% c("#N/A","#REF!"))

for(k in 1:length(which_NA))
{

  baserunning <- read.csv(paste("BIS/","Baserunning_",strftime(strptime(master_box$GameDate[which_NA[k]],"%Y-%m-%d"),"%Y%m%d"),".csv",sep=""))
  
  baserunning <- baserunning[baserunning$MLBId %in% master_box$MLBId[which_NA[k]],]
  
  master_box$Bases.Taken[which_NA[k]] <-baserunning$BasesTaken[1] * 0.3
}

# Outs on base

which_NA <- which(master_box$Outs.on.Base %in% c("#N/A","#REF!"))

for(k in 1:length(which_NA))
{
  
  baserunning <- read.csv(paste("BIS/","Baserunning_",strftime(strptime(master_box$GameDate[which_NA[k]],"%Y-%m-%d"),"%Y%m%d"),".csv",sep=""))
  
  baserunning <- baserunning[baserunning$MLBId %in% master_box$MLBId[which_NA[k]],]
  
  master_box$Outs.on.Base[which_NA[k]] <- baserunning$OutsOnBase * -0.7
}

#Field

which_NA <- which(master_box$Field %in% c("#N/A","#REF!"))

for(l in 1:length(which_NA))
{
  fielding <- read.csv(paste("BIS/","Fielding_",strftime(strptime(master_box$GameDate[which_NA[l]],"%Y-%m-%d"),"%Y%m%d"),".csv",sep=""))
  
  fielding <- fielding[fielding$MLBId %in% master_box$MLBId[which_NA[l]],]
  
  pos_num <- c(1,2,3,4,5,6,7,8,9)
  pos_let <- c("P","CA","1B","2B","3B","SS","LF","CF","RF")
  
  if((nrow(fielding) == 0))
  {
    next;
  }
  
  if((fielding$GS %in% c(NA,"NA")))
  {
    next;
  }
  
  if(nrow(fielding) > 1)
  {
    fielding$INN <- as.double(fielding$INN)
    
    fielding <- fielding[fielding$INN == max(fielding$INN),]
    
    fielding <- fielding[1,]

  }
  
  
  fielding$LW[which(fielding$Pos %in% pos_num[1])] <- round(((fielding$PO[which(fielding$Pos %in% pos_num[1])] * 0.13455) + (fielding$A[which(fielding$Pos %in% pos_num[1])] * 0.13754) - (fielding$E[which(fielding$Pos %in% pos_num[1])] * 0.508)),digits=2)
  
  #Catchers
  #=(U17*0.13754)+(AC17*0.234)+(AB17*0.234)-(V17*0.508)-(Y17*0.269)-(AJ17*0.392)-(AA17*0.088)
  
  fielding$LW[which(fielding$Pos %in% pos_num[2])] <- round(((fielding$A[which(fielding$Pos %in% pos_num[2])] * 0.13754) + (fielding$PKOF[which(fielding$Pos %in% pos_num[2])] * 0.234) + (fielding$CS[which(fielding$Pos %in% pos_num[2])] * 0.234) - (fielding$E[which(fielding$Pos %in% pos_num[2])] * 0.508) - (fielding$PB[which(fielding$Pos %in% pos_num[2])] * 0.269) - (fielding$Cint[which(fielding$Pos %in% pos_num[2])] * 0.392) - (fielding$SB[which(fielding$Pos %in% pos_num[2])] * 0.088)),digits=2)
  
  #1B
  #=(T41*0.00897)+(U41*0.13754)-(V41*0.508)
  
  fielding$LW[which(fielding$Pos %in% pos_num[3])] <- round(((fielding$PO[which(fielding$Pos %in% pos_num[3])] * 0.00897) + (fielding$A[which(fielding$Pos %in% pos_num[3])] * 0.13754) - (fielding$E[which(fielding$Pos %in% pos_num[3])] * 0.508)),digits=2)
  
  
  #2B
  #=(T45*0.06122)+(U45*0.085843)-(V45*0.508)
  
  fielding$LW[which(fielding$Pos %in% pos_num[4])] <- round(((fielding$PO[which(fielding$Pos %in% pos_num[4])] * 0.06122) + (fielding$A[which(fielding$Pos %in% pos_num[4])] * 0.085843) - (fielding$E[which(fielding$Pos %in% pos_num[4])] * 0.508)),digits=2)
  
  
  #3B
  #=(T43*0.14352)+(U43*0.115115)-(V43*0.508)
  
  fielding$LW[which(fielding$Pos %in% pos_num[5])] <- round(((fielding$PO[which(fielding$Pos %in% pos_num[5])] * 0.14352) + (fielding$A[which(fielding$Pos %in% pos_num[5])] * 0.115115) - (fielding$E[which(fielding$Pos %in% pos_num[5])] * 0.508)),digits=2)
  
  #SS
  #=(T63*0.125161)+(U63*0.085843)-(V63*0.508)
  
  fielding$LW[which(fielding$Pos %in% pos_num[6])] <- round(((fielding$PO[which(fielding$Pos %in% pos_num[6])] * 0.125161) + (fielding$A[which(fielding$Pos %in% pos_num[6])] * 0.085843) - (fielding$E[which(fielding$Pos %in% pos_num[6])] * 0.508)),digits=2)
  
  
  #OF
  #=(T64*0.130065)+(U64*0.148005)-(V64*0.508)
  
  
  fielding$LW[which(fielding$Pos %in% pos_num[c(7,8,9)])] <- round(((fielding$PO[which(fielding$Pos %in% pos_num[c(7,8,9)])] * 0.130065) + (fielding$A[which(fielding$Pos %in% pos_num[c(7,8,9)])] * 0.148005) - (fielding$E[which(fielding$Pos %in% pos_num[c(7,8,9)])] * 0.508)),digits=2)
  
  
  master_box$Field[which_NA[l]] <- fielding$LW[1]
  
  
}

master_box$Field[which_NA] <- ""

# ERROR FIX

which_NA <- which(master_box$E %in% c("#N/A","#REF!"))

for(l in 1:length(which_NA))
{
  fielding <- read.csv(paste("BIS/","Fielding_",strftime(strptime(master_box$GameDate[which_NA[l]],"%Y-%m-%d"),"%Y%m%d"),".csv",sep=""))
  
  fielding <- fielding[fielding$MLBId %in% master_box$MLBId[which_NA[l]],]
  
  pos_num <- c(1,2,3,4,5,6,7,8,9)
  pos_let <- c("P","CA","1B","2B","3B","SS","LF","CF","RF")
  
  if((nrow(fielding) == 0))
  {
    next;
  }
  
  if((fielding$GS %in% c(NA,"NA")))
  {
    next;
  }
  
  if(nrow(fielding) > 1)
  {
    fielding$INN <- as.double(fielding$INN)
    
    fielding <- fielding[fielding$INN == max(fielding$INN),]
    
    fielding <- fielding[1,]
    
  }
  
  master_box$E[which_NA[l]] <- fielding$E[1]
  
  
}

# Zone

which_NA <- which(master_box$Zone %in% c("#N/A","#REF!"))

for(n in 1:length(which_NA))
{
  date2 <- as.Date(master_box$GameDate[which_NA[n]])
  date1 <- date2 - 1

  YTD2 <- read.csv(paste("BIS/","YTDFielding_",strftime(strptime(date2,"%Y-%m-%d"),"%Y%m%d"),".csv",sep=""))
  YTD <- read.csv(paste("BIS/","YTDFielding_",strftime(strptime(date1,"%Y-%m-%d"),"%Y%m%d"),".csv",sep=""))
  
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
  
  
  YTD2 <- select(YTD2, PlayerId, LastName, FirstName, MLBId, Zone, Block, Frame, PlayerName, Team, TeamNbr, Pos, Position, G, GS, INN, BallsInZone, MissedBallsInZone, previous_missed, 
                 available_missed, OutsOutOfZone, previous_outs, available_outs, CBlockingRuns, CFramingRuns)
  
  YTD <- select(YTD,LastName, FirstName, MLBId, PlayerName, PlayerId, Team, TeamNbr, Pos, G, GS, INN, BallsInZone, MissedBallsInZone, OutsOutOfZone, CBlockingRuns, CFramingRuns)
  
  
  position <- c("P","CA","1B","2B","3B","SS","LF","CF","RF")
  
  for(m in 1:length(position))
  {
    YTD2$Position[which(YTD2$Pos %in% m)] <- position[m]
  }
  
  
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
  
  YTD2 <- YTD2[YTD2$MLBId == master_box$MLBId[which_NA[n]],]
  
  if(nrow(YTD2) == 0)
  {
    next;
  }
  
  if((YTD2$GS %in% c(NA,"NA")))
  {
    next;
  }
  
  if(nrow(YTD2) > 1)
  {
    YTD2$INN <- as.double(YTD2$INN)
    
    YTD2 <- YTD2[YTD2$INN == max(YTD2$INN),]
    
    YTD2 <- YTD2[1,]
    
  }
  
  master_box$Zone[which_NA[n]] <- YTD2$Zone[1]
}

master_box$Zone[which(master_box$Zone %in% c("#N/A","#REF!"))] <- ""

# Block

which_NA <- which(master_box$Block %in% c("#N/A","#REF!"))

for(n in 1:length(which_NA))
{
  date2 <- as.Date(master_box$GameDate[which_NA[n]])
  date1 <- date2 - 1
  
  YTD2 <- read.csv(paste("BIS/","YTDFielding_",strftime(strptime(date2,"%Y-%m-%d"),"%Y%m%d"),".csv",sep=""))
  YTD <- read.csv(paste("BIS/","YTDFielding_",strftime(strptime(date1,"%Y-%m-%d"),"%Y%m%d"),".csv",sep=""))
  
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
  
  
  YTD2 <- select(YTD2, PlayerId, LastName, FirstName, MLBId, Zone, Block, Frame, PlayerName, Team, TeamNbr, Pos, Position, G, GS, INN, BallsInZone, MissedBallsInZone, previous_missed, 
                 available_missed, OutsOutOfZone, previous_outs, available_outs, CBlockingRuns, CFramingRuns)
  
  YTD <- select(YTD,LastName, FirstName, MLBId, PlayerName, PlayerId, Team, TeamNbr, Pos, G, GS, INN, BallsInZone, MissedBallsInZone, OutsOutOfZone, CBlockingRuns, CFramingRuns)
  
  
  position <- c("P","CA","1B","2B","3B","SS","LF","CF","RF")
  
  for(m in 1:length(position))
  {
    YTD2$Position[which(YTD2$Pos %in% m)] <- position[m]
  }
  
  
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
  
  YTD2 <- YTD2[YTD2$MLBId == master_box$MLBId[which_NA[n]],]
  
  if(nrow(YTD2) == 0)
  {
    next;
  }
  
  if((YTD2$GS %in% c(NA,"NA")))
  {
    next;
  }
  
  if(nrow(YTD2) > 1)
  {
    YTD2$INN <- as.double(YTD2$INN)
    
    YTD2 <- YTD2[YTD2$INN == max(YTD2$INN),]
    
    YTD2 <- YTD2[1,]
    
  }
  
  master_box$Block[which_NA[n]] <- YTD2$Block[1]
}

# Framing

which_NA <- which(master_box$Frame %in% c("#N/A","#REF!"))

for(o in 1:length(which_NA))
{
  date2 <- as.Date(master_box$GameDate[which_NA[o]])
  date1 <- date2 - 1
  
  YTD2 <- read.csv(paste("BIS/","YTDFielding_",strftime(strptime(date2,"%Y-%m-%d"),"%Y%m%d"),".csv",sep=""))
  YTD <- read.csv(paste("BIS/","YTDFielding_",strftime(strptime(date1,"%Y-%m-%d"),"%Y%m%d"),".csv",sep=""))
  
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
  
  
  YTD2 <- select(YTD2, PlayerId, LastName, FirstName, MLBId, Zone, Block, Frame, PlayerName, Team, TeamNbr, Pos, Position, G, GS, INN, BallsInZone, MissedBallsInZone, previous_missed, 
                 available_missed, OutsOutOfZone, previous_outs, available_outs, CBlockingRuns, CFramingRuns)
  
  YTD <- select(YTD,LastName, FirstName, MLBId, PlayerName, PlayerId, Team, TeamNbr, Pos, G, GS, INN, BallsInZone, MissedBallsInZone, OutsOutOfZone, CBlockingRuns, CFramingRuns)
  
  
  position <- c("P","CA","1B","2B","3B","SS","LF","CF","RF")
  
  for(m in 1:length(position))
  {
    YTD2$Position[which(YTD2$Pos %in% m)] <- position[m]
  }
  
  
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
  
  YTD2 <- YTD2[YTD2$MLBId == master_box$MLBId[which_NA[o]],]
  
  if(nrow(YTD2) == 0)
  {
    next;
  }
  
  if((YTD2$GS %in% c(NA,"NA")))
  {
    next;
  }
  
  if(nrow(YTD2) > 1)
  {
    YTD2$INN <- as.double(YTD2$INN)
    
    YTD2 <- YTD2[YTD2$INN == max(YTD2$INN),]
    
    YTD2 <- YTD2[1,]
    
  }
  
  master_box$Frame[which_NA[o]] <- YTD2$Frame[1]
}

master_box$Frame[which(master_box$Frame %in% c("#N/A","#REF!"))] <- ""

write.csv(master_box,"early_season_master_stat.csv",row.names=FALSE)


