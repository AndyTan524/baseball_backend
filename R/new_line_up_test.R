for(i in 1:9){
  
  batting_bench2 <- batting_bench
  batting_pinch2 <- batting_pinch
  batting_start2 <- batting_start
  fielding_available2 <- fielding_available
  base_available2 <- base_available
  
  only_active_players2 <- only_active_players
  
  if(box_visit$POS[i] %in% c(2,3,4,5,6,7,8,9,"DH")){
  if((box_visit$MLBId[i] %in% batting_start$MLBId) | (box_visit$MLBId[i] %in% batting_bench$MLBId)){
    
    # Match starter ID from batting_start to ID from box_visit in ith row. This pulls data
    if(box_visit$MLBId[i] %in% batting_start$MLBId){
      
    stat <- batting_start[which(batting_start$MLBId %in% box_visit$MLBId[i]),]
    
    # Add POS column
    
    stat$POS <- ""
    
    # Rearrange the stat to have POS column up front
    
    stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                    "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                    "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
    
    # Take the stat of most recent.
    
    stat <- stat[which(stat$GameDate %in% max(stat$GameDate)),]
    
    # stat could be in multiple rows. Choose the top one.
    
    stat <- stat[1,]
    
    # Fill POS column with position of player that he is assigned to play
    
    stat$POS <- box_visit$POS[i]
    
    # If picked player in the stat is a pitcher, assign "1". (As in pitcher)
    
    if(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x]) & lineup$POS %in% c("SP","RP")])
    {
      stat$POS <- "1"
    }
    
    # If picked player in the stat is not a pitcher, assign according to ith row of box_visit
    
    if(!(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x]) & lineup$POS %in% c("SP","RP")]))
    {
      stat$POS <- box_visit$POS[i]
    }
    
    box_stat_visit <- rbind(box_stat_visit, stat)
    
    }
    
    if((box_visit$MLBId[i] %in% batting_bench$MLBId) & !(box_visit$MLBId[i] %in% batting_start$MLBId))
    {
      
      
      stat <- batting_bench[which(batting_bench$MLBId %in% box_visit$MLBId[i]),]
      
      # Add POS column
      
      stat$POS <- ""
      
      # Rearrange the stat to have POS column up front
      
      stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                      "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                      "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
      
      # Take the stat of most recent.
      
      stat <- stat[which(stat$GameDate %in% max(stat$GameDate)),]
      
      # stat could be in multiple rows. Choose the top one.
      
      stat <- stat[1,]
      
      # Fill POS column with position of player that he is assigned to play
      
      stat$POS <- box_visit$POS[i]
      
      # If picked player in the stat is a pitcher, assign "1". (As in pitcher)
      
      if(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x]) & lineup$POS %in% c("SP","RP")])
      {
        stat$POS <- "1"
      }
      
      # If picked player in the stat is not a pitcher, assign according to ith row of box_visit
      
      if(!(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x]) & lineup$POS %in% c("SP","RP")]))
      {
        stat$POS <- box_visit$POS[i]
      }
      
      box_stat_visit <- rbind(box_stat_visit, stat)
    }
  }
  }
  
  if(box_visit$POS[i] %in% c(1))
  {

      # Match starter ID from batting_start to ID from box_visit in ith row. This pulls data
      if(box_visit$MLBId[i] %in% batting_start$MLBId){
        
        stat <- batting_start[which(batting_start$MLBId %in% box_visit$MLBId[i]),]
        
        # Add POS column
        
        stat$POS <- ""
        
        # Rearrange the stat to have POS column up front
        
        stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                        "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                        "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
        
        # Take the stat of most recent.
        
        stat <- stat[which(stat$GameDate %in% max(stat$GameDate)),]
        
        # stat could be in multiple rows. Choose the top one.
        
        stat <- stat[1,]
        
        # Fill POS column with position of player that he is assigned to play
        
        stat$POS <- box_visit$POS[i]
        
        # If picked player in the stat is a pitcher, assign "1". (As in pitcher)
        
        if(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x]) & lineup$POS %in% c("SP","RP")])
        {
          stat$POS <- "1"
        }
        
        # If picked player in the stat is not a pitcher, assign according to ith row of box_visit
        
        if(!(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x]) & lineup$POS %in% c("SP","RP")]))
        {
          stat$POS <- box_visit$POS[i]
        }
        
        box_stat_visit <- rbind(box_stat_visit, stat)
        
      }
      
      if((box_visit$MLBId[i] %in% batting_bench$MLBId) & !(box_visit$MLBId[i] %in% batting_start$MLBId))
      {
        stat <- batting_bench[which(batting_bench$MLBId %in% box_visit$MLBId[i]),]
        
        # Add POS column
        
        stat$POS <- ""
        
        # Rearrange the stat to have POS column up front
        
        stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                        "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                        "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
        
        # Take the stat of most recent.
        
        stat <- stat[which(stat$GameDate %in% max(stat$GameDate)),]
        
        # stat could be in multiple rows. Choose the top one.
        
        stat <- stat[1,]
        
        # Fill POS column with position of player that he is assigned to play
        
        stat$POS <- box_visit$POS[i]
        
        # If picked player in the stat is a pitcher, assign "1". (As in pitcher)
        
        if(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x]) & lineup$POS %in% c("SP","RP")])
        {
          stat$POS <- "1"
        }
        
        # If picked player in the stat is not a pitcher, assign according to ith row of box_visit
        
        if(!(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x]) & lineup$POS %in% c("SP","RP")]))
        {
          stat$POS <- box_visit$POS[i]
        }
        
        box_stat_visit <- rbind(box_stat_visit, stat)
      }
    
    if(box_visit$MLBId[i] %in% batting_pinch$MLBId)
    {
      stat <- batting_pinch[which(batting_pinch$MLBId %in% box_visit$MLBId[i]),]
      
      # Add POS column
      
      stat$POS <- ""
      
      # Rearrange the stat to have POS column up front
      
      stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                      "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                      "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
      
      # Take the stat of most recent.
      
      stat <- stat[which(stat$GameDate %in% max(stat$GameDate)),]
      
      # stat could be in multiple rows. Choose the top one.
      
      stat <- stat[1,]
      
      # Fill POS column with position of player that he is assigned to play
      
      stat$POS <- box_visit$POS[i]
      
      # If picked player in the stat is a pitcher, assign "1". (As in pitcher)
      
      if(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x]) & lineup$POS %in% c("SP","RP")])
      {
        stat$POS <- "1"
      }
      
      # If picked player in the stat is not a pitcher, assign according to ith row of box_visit
      
      if(!(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x]) & lineup$POS %in% c("SP","RP")]))
      {
        stat$POS <- box_visit$POS[i]
      }
      
      box_stat_visit <- rbind(box_stat_visit, stat)
    }
    
    if((!(box_visit$MLBId[i] %in% batting_start$MLBId) & !(box_visit$MLBId[i] %in% batting_bench$MLBId) & !(box_visit$MLBId[i] %in% batting_pinch$MLBId)))
    {
      print(paste("Giving MLE for ",box_visit$fullname[i],sep=""))
      
      if(away_sp_stat$IP > 5)
      {
        MLE_col <- c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                     "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                     "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")
        
        MLE <- data.frame(matrix(NA,nrow=1,ncol=length(MLE_col)))
        
        colnames(MLE) <- MLE_col
        
        for(v in 1:ncol(MLE))
        {
          if(v %in% c(5:38))
          {
            MLE[1,v] <- as.integer(MLE[1,v])
          }
          
          if(v %in% c(1,3:4,39:44))
          {
            MLE[1,v] <- as.character(MLE[1,v])
          }
          
          if(v %in% c(2))
          {
            MLE[,v] <- as.Date(MLE[1,v],format="%Y-%m-%d")
          }
        }
        MLE_fill <- c("P",format(away_sp_stat$GameDate, "%Y-%m-%d"), as.character(away_sp_stat$FirstName), as.character(away_sp_stat$LastName), -0.75, 0, 0, 0 , 0, 0, 0 , "", "",
                         3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,3,as.character(away_sp_stat$MLBId),as.character(away_sp_stat$PlayerName),as.character(away_sp_stat$GameString),as.character(away_sp_stat$GameId),as.character(away_sp_stat$uniqueId), "")
        
        for(n in 1:length(MLE_fill))
        {
          MLE[1,n] <- MLE_fill[n]
        }
        
        box_stat_visit <- rbind(box_stat_visit, MLE)        
      }
      
      if((away_sp_stat$IP >= 3.01) & (away_sp_stat$IP < 5.01))
      {
        MLE_col <- c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                     "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                     "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")
        
        MLE <- data.frame(matrix(NA,nrow=1,ncol=length(MLE_col)))
        
        colnames(MLE) <- MLE_col
        
        for(v in 1:ncol(MLE))
        {
          if(v %in% c(5:38))
          {
            MLE[1,v] <- as.integer(MLE[1,v])
          }
          
          if(v %in% c(1,3:4,39:44))
          {
            MLE[1,v] <- as.character(MLE[1,v])
          }
          
          if(v %in% c(2))
          {
            MLE[,v] <- as.Date(MLE[1,v],format="%Y-%m-%d")
          }
        }
        MLE_fill <- c("P",format(away_sp_stat$GameDate, "%Y-%m-%d"), as.character(away_sp_stat$FirstName), as.character(away_sp_stat$LastName), -0.75, 0, 0, 0 , 0, 0, 0 , "", "",
                      2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,3,as.character(away_sp_stat$MLBId),as.character(away_sp_stat$PlayerName),as.character(away_sp_stat$GameString),as.character(away_sp_stat$GameId),as.character(away_sp_stat$uniqueId), "")
        
        for(n in 1:length(MLE_fill))
        {
          MLE[1,n] <- MLE_fill[n]
        }
        
        box_stat_visit <- rbind(box_stat_visit, MLE)        
      }
      
      if(away_sp_stat$IP < 3.01)
      {
        MLE_col <- c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                     "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                     "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")
        
        MLE <- data.frame(matrix(NA,nrow=1,ncol=length(MLE_col)))
        
        colnames(MLE) <- MLE_col
        
        for(v in 1:ncol(MLE))
        {
          if(v %in% c(5:38))
          {
            MLE[1,v] <- as.integer(MLE[1,v])
          }
          
          if(v %in% c(1,3:4,39:44))
          {
            MLE[1,v] <- as.character(MLE[1,v])
          }
          
          if(v %in% c(2))
          {
            MLE[,v] <- as.Date(MLE[1,v],format="%Y-%m-%d")
          }
        }
        MLE_fill <- c("P",format(away_sp_stat$GameDate, "%Y-%m-%d"), as.character(away_sp_stat$FirstName), as.character(away_sp_stat$LastName), -0.75, 0, 0, 0 , 0, 0, 0 , "", "",
                      1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,3,as.character(away_sp_stat$MLBId),as.character(away_sp_stat$PlayerName),as.character(away_sp_stat$GameString),as.character(away_sp_stat$GameId),as.character(away_sp_stat$uniqueId), "")
        
        for(n in 1:length(MLE_fill))
        {
          MLE[1,n] <- MLE_fill[n]
        }
        
        box_stat_visit <- rbind(box_stat_visit, MLE)        
      }
    }
  }
  
  
  
}