
bat_report <- read.csv(paste("report/bat/",date,"/",final_schedule$Home[x],date,"_batting_report.csv",sep=""))

bat_report$MLBId <- as.character(bat_report$MLBId)

batting_col <- c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                 "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                 "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")

box_stat_home <- data.frame(matrix(NA, nrow = 1, ncol = length(batting_col)))

colnames(box_stat_home) <- batting_col

box_stat_home$LW <- as.numeric(as.character(box_stat_home$LW))

for(v in 1:ncol(box_stat_home))
{
  if(v %in% c(1,3:4,39:44))
  {
    box_stat_home[,v] <- as.character(box_stat_home[,v])
    
  }
  
  if(v %in% c(6:38))
  {
    box_stat_home[,v] <- as.numeric(box_stat_home[,v])
    
    
  }
  
  if(v %in% c(2))
  {
    box_stat_home[,v] <- as.Date(box_stat_home[,v],format="%Y-%m-%d")
    
    
  }
  
}


pos_sim <- read.csv("pos_sim.csv",header = TRUE)

for(i in 1:9){
  
  batting_bench2 <- batting_bench
  batting_pinch2 <- batting_pinch
  batting_start2 <- batting_start
  fielding_available2 <- fielding_available
  base_available2 <- base_available
  
  only_active_players2 <- only_active_players
  
  
  if((box_home$MLBId[i] %in% batting_start$MLBId) | (box_home$MLBId[i] %in% batting_bench$MLBId)){
    
    if(box_home$MLBId[i] %in% batting_start$MLBId){
    # Match starter ID from batting_start to ID from box_home in ith row. This pulls data
    
    stat <- batting_start[which(batting_start$MLBId %in% box_home$MLBId[i]),]
    
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
    
    stat$POS <- box_home$POS[i]
    
    # If picked player in the stat is a pitcher, assign "1". (As in pitcher)
    
    if(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Home[x]) & lineup$POS %in% c("SP","RP")])
    {
      stat$POS <- "1"
    }
    
    # If picked player in the stat is not a pitcher, assign according to ith row of box_home
    
    if(!(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Home[x]) & lineup$POS %in% c("SP","RP")]))
    {
      stat$POS <- box_home$POS[i]
    }
    
    box_stat_home <- rbind(box_stat_home, stat)
    }
    
    if((box_home$MLBId[i] %in% batting_bench$MLBId) & (!(box_home$MLBId[i] %in% batting_start$MLBId))){
      # Match starter ID from batting_start to ID from box_home in ith row. This pulls data
      
      stat <- batting_bench[which(batting_bench$MLBId %in% box_home$MLBId[i]),]
      
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
      
      stat$POS <- box_home$POS[i]
      
      # If picked player in the stat is a pitcher, assign "1". (As in pitcher)
      
      if(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Home[x]) & lineup$POS %in% c("SP","RP")])
      {
        stat$POS <- "1"
      }
      
      # If picked player in the stat is not a pitcher, assign according to ith row of box_home
      
      if(!(stat$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Home[x]) & lineup$POS %in% c("SP","RP")]))
      {
        stat$POS <- box_home$POS[i]
      }
      
      box_stat_home <- rbind(box_stat_home, stat)
    }
    
  }
  
  if(!((box_home$MLBId[i] %in% batting_start$MLBId) | (box_home$MLBId[i] %in% batting_bench$MLBId))){
    
    if(box_home$POS[i] == "DH")
    {
      
      bat_report2 <- bat_report
      
      bat_report2 <- bat_report2[(bat_report2$Start >= 1),]
      
      bat_report2 <- bat_report2[(bat_report2$MLBId %in% box_home$MLBId[i]),]
      
      if(nrow(bat_report2) > 0)
      {
        batting_start2 <- batting_start
        
        batting_start2 <- batting_start2[batting_start2$MLBId %in% bat_report2$MLBId,]
        
        batting_start2$POS <- box_home$POS[i]
        batting_start2 <- batting_start2[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
        
        stat <- batting_start2
        
        stat <- stat[1,]
        
        box_stat_home <- rbind(box_stat_home, stat)
      }
      
      if((nrow(bat_report2) == 0))
      {
        bat_report2 <- bat_report
        
        bat_report2 <- bat_report2[(bat_report2$Bench >= 1),]
        
        bat_report2 <- bat_report2[(bat_report2$MLBId %in% box_home$MLBId[i]),]
        
        if(nrow(bat_report2) > 0)
        {
          batting_bench2 <- batting_bench
          
          batting_bench2 <- batting_bench2[batting_bench2$MLBId %in% bat_report2$MLBId,]
          
          batting_bench2$POS <- box_home$POS[i]
          batting_bench2 <- batting_bench2[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                              "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                              "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
          
          stat <- batting_bench2
          
          stat <- stat[1,]
          
          box_stat_home <- rbind(box_stat_home, stat)
        }
        
        if(nrow(bat_report2) == 0)
        {
          bat_report3 <- bat_report
          bat_report3 <- bat_report3[!(bat_report3$PlayerName %in% box_stat_home$PlayerName) & (bat_report3$PlayerName %in% lineup$fullname[(!lineup$POS %in% c("SP","RP")) & (lineup$Team %in% final_schedule$Home[x])]) & !(bat_report3$PlayerName %in% box_home$fullname),]
        
          bat_report3 <- bat_report3[1,]
          
            batting_start2 <- batting_start
            
            batting_start2 <- batting_start2[batting_start2$MLBId %in% bat_report3$MLBId,]
            
            stat <- batting_start2
            
            stat <- stat[order(stat$GameDate, decreasing = TRUE),]
            
            stat$POS <- "DH"
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            stat <- stat[1,]
            
            box_stat_home <- rbind(box_stat_home, stat)
            
            
          
          
          
        }
        
      }
      
    }
    
    if(box_home$POS[i] == "1"){
      
      # Team name of player in line up
      
      team_name <- final_schedule$Home[x]
      
      # All players in the team with same position
      
      only_active_players3 <- only_active_players2[(only_active_players2$Team_RFB %in% final_schedule$Home[x]) & (only_active_players2$Pos %in% box_home$POS[i]),]
      
      fielding_available3 <- fielding_available2[(fielding_available2$Pos %in% only_active_players3$Pos) & (fielding_available$Team %in% final_schedule$Home[x]) & (fielding_available2$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Home[x])]),]
      
      
      stat <- batting_bench2[batting_bench2$uniqueId %in% fielding_available3$uniqueId[which(fielding_available3$MLBId %in% box_home$MLBId[box_home$POS == "1"])],]
      
      if(nrow(stat) > 0)
      {
        
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
        
        stat$POS <- box_home$POS[i]
        
        box_stat_home <- rbind(box_stat_home, stat)
      }
      
      if(nrow(stat) == 0)
      {
        team_name <- final_schedule$Home[x]
        
        # All players in the team with same position
        
        only_active_players3 <- only_active_players2[(only_active_players2$Team_RFB %in% final_schedule$Home[x]) & (only_active_players2$Pos %in% box_home$POS[i]),]
        
        fielding_available3 <- fielding_available2[(fielding_available2$Pos %in% only_active_players3$Pos) & (fielding_available$Team %in% final_schedule$Home[x]) & (fielding_available2$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Home[x])]),]
        
        
        stat <- batting_pinch2[batting_pinch2$uniqueId %in% fielding_available3$uniqueId[which(fielding_available3$MLBId %in% box_home$MLBId[box_home$POS == "1"])],]
        
        if(nrow(stat) > 0)
        {
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
          
          stat$POS <- box_home$POS[i]
          
          box_stat_home <- rbind(box_stat_home, stat)
          
        }
        
        if(nrow(stat) == 0)
        {
          bench_player_home <- (batting_pinch2$MLBId[(batting_pinch2$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Home[x])])])
          
          bench_player_home <- bench_player_home[!(bench_player_home %in% box_stat_home$MLBId)]
          
          bench_player_home <- bench_player_home[!(bench_player_home %in% box_home$MLBId)]
          
          batting_pinch2 <- batting_pinch2[batting_pinch2$MLBId %in% bench_player_home,]
          
          stat <- batting_pinch2
          
          if(nrow(stat) > 0)
          {
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
            
            stat$POS <- "PH"
            
            
            box_stat_home <- rbind(box_stat_home, stat)
          }
          
          if(nrow(stat) == 0)
          {
            batting_start2 <- batting_start
            batting_start2 <- batting_start2[batting_start2$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Home[x]) & !(lineup$POS %in% c("SP","RP"))],]
            batting_start2 <- batting_start2[!batting_start2$MLBId %in% box_home$MLBId,]
            batting_start2 <- batting_start2[!batting_start2$MLBId %in% box_stat_home$MLBId,]
            
            batting_start2 <- batting_start2[order(batting_start2$GameDate, decreasing = TRUE),]
            
            if(nrow(batting_start2) > 0)
            {
              batting_start2$POS <- "DH"
              battting_start2 <- batting_start2[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                                   "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                                   "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
              
              stat <- batting_start2
              
              stat <- stat[1,]
              
              box_stat_home <- rbind(box_stat_home, stat)
            }
            
            if(nrow(batting_start2) == 0)
            {
              stop("No batters to replace pitching slot")
            }
          }
          
          
          
        }
        
        
      }
      
    }
    
    if(box_home$POS[i] %in% c(2,3,4,5,6,7,8,9)){
      
      bat_report2 <- bat_report
      
      bat_report2 <- bat_report2[(bat_report2$Start >= 1),]
      
      bat_report2 <- bat_report2[(bat_report2$MLBId %in% box_home$MLBId[i]),]
      
      if(nrow(bat_report2) > 0)
      {
        batting_start2 <- batting_start
        
        batting_start2 <- batting_start2[batting_start2$MLBId %in% bat_report2$MLBId,]
        
        batting_start2$POS <- box_home$POS[i]
        batting_start2 <- batting_start2[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                             "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                             "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
        
        stat <- batting_start2
        
        stat <- stat[1,]
        
        box_stat_home <- rbind(box_stat_home, stat)
      }
      
      if((nrow(bat_report2) == 0))
      {
        bat_report2 <- bat_report
        
        bat_report2 <- bat_report2[(bat_report2$Bench >= 1),]
        
        bat_report2 <- bat_report2[(bat_report2$MLBId %in% box_home$MLBId[i]),]
        
        if(nrow(bat_report2) > 0)
        {
          batting_bench2 <- batting_bench
          
          batting_bench2 <- batting_bench2[batting_bench2$MLBId %in% bat_report2$MLBId,]
          
          batting_bench2$POS <- box_home$POS[i]
          batting_bench2 <- batting_bench2[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                               "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                               "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
          
          stat <- batting_bench2
          
          stat <- stat[1,]
          
          box_stat_home <- rbind(box_stat_home, stat)
        }
        
        if(nrow(bat_report2) == 0)
        {
          bat_report3 <- bat_report
          bat_report3 <- bat_report3[!(bat_report3$PlayerName %in% box_stat_home$PlayerName) & (bat_report3$PlayerName %in% lineup$fullname[(!lineup$POS %in% c("SP","RP")) & (lineup$Team %in% final_schedule$Home[x])]) & !(bat_report3$PlayerName %in% box_home$fullname),]
          
          if(!(box_home$POS[i] %in% lineup$POS2[lineup$fullname %in% bat_report3$PlayerName]))
          {
            
            mle_blank <- data.frame(matrix(NA,nrow=1,ncol=length(batting_start)))
            colnames(mle_blank) <- colnames(batting_start)
            
            for(t in 1:ncol(mle_blank))
            {
              mle_blank[1,t] <- readline(paste("Type in value for column named ",colnames(mle_blank)[t],". This is for creating MLE for ",final_schedule$Home[x]," ",box_home$fullname[i],sep=""))
            }
            
            mle_blank$GameDate <- as.Date(mle_blank$GameDate,format="%Y-%m-%d")
            
            for(u in c(2:5,40:43))
            {
              mle_blank[,u] <- as.character(mle_blank[,u])
            }
            
            for(v in c(6:39))
            {
              mle_blank[,v] <- as.double(mle_blank[,v])
            }
            
            stat <- mle_blank
            
            if(nrow(stat) > 0)
            {
              stat <- stat[order(stat$GameDate, decreasing = TRUE),]
              
              stat$POS <- "DH"
              
              stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                              "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                              "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
              
              stat <- stat[1,]
              
              box_stat_home <- rbind(box_stat_home, stat)
            }
          }
          
          if(box_home$POS[i] %in% lineup$POS2[lineup$fullname %in% bat_report3$PlayerName]){
            
            
            id <- lineup$MLBId[(lineup$fullname %in% bat_report3$PlayerName) & (lineup$Team %in% final_schedule$Home[x])]
            
            batting_start2 <- batting_start
            
            batting_start2 <- batting_start2[batting_start2$MLBId %in% id,]
            
            stat <- batting_start2
            
            stat <- stat[order(stat$GameDate, decreasing = TRUE),]
            
            stat$POS <- "DH"
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            stat <- stat[1,]
            
            box_stat_home <- rbind(box_stat_home, stat)
            
            
          }
          
          
        }
      
      }
      
    }  
  }
  
}

box_stat_home <- box_stat_home[!(box_stat_home$PlayerName %in% c("",NA)),]