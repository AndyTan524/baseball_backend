for(i in 1:length(count_2))
{
  POS <- box_visit$POS[box_visit$MLBId %in% box_visit$MLBId[i]]
  
  team_name <- box_visit$Team[i]
  
  if(POS %in% "1"){
   
    box_stat_visit$MLBId <- as.character(box_stat_visit$MLBId)
    
    unplayed <- play_pos$MLBId[(play_pos$Team_RFB %in% final_schedule$Away[x]) & !(play_pos$MLBId %in% box_stat_visit$MLBId)]
    
    unplayed <- unique(unplayed)
  
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% unplayed),]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      box_stat_visit <- rbind(box_stat_visit,stat)
    
  }
  
  if(POS %in% "2"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("2"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_visit$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_visit$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
    
    if(nrow(fielder_available) > 0)
    {
      bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      box_stat_visit <- rbind(box_stat_visit,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_visit$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      fielding_available$GameDate <- as.Date(fielding_available$GameDate)
      
      fielding_available$MLBId <- as.character(fielding_available$MLBId)
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch$MLBId <- as.character(bat_pinch$MLBId)
      
      delete <- vector()
      
      for(i in 1:nrow(bat_pinch))
      {
        mlbid <- bat_pinch$MLBId[i]
        gamedate <- bat_pinch$GameDate[i]
        
        if(nrow(fielding_available[(fielding_available$MLBId %in% mlbid) & (fielding_available$GameDate %in% gamedate),]) > 0)
        {
          delete[i] <- i
        }
      }
      
      delete <- delete[!is.na(delete)]
      
      num <- c(1:nrow(bat_pinch))
      
      bat_pinch <- bat_pinch[!(num %in% delete),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[1,] 
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      box_stat_visit$GameDate <- as.character(box_stat_visit$GameDate)
      
      box_stat_visit <- rbind(box_stat_visit, stat)
      
    }
    
  }
  
  if(POS %in% "3"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("3"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_visit$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_visit$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
    
    if(nrow(fielder_available) > 0)
    {
      bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      box_stat_visit <- rbind(box_stat_visit,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_visit$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      fielding_available$GameDate <- as.Date(fielding_available$GameDate)
      
      fielding_available$MLBId <- as.character(fielding_available$MLBId)
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch$MLBId <- as.character(bat_pinch$MLBId)
      
      delete <- vector()
      
      for(i in 1:nrow(bat_pinch))
      {
        mlbid <- bat_pinch$MLBId[i]
        gamedate <- bat_pinch$GameDate[i]
        
        if(nrow(fielding_available[(fielding_available$MLBId %in% mlbid) & (fielding_available$GameDate %in% gamedate),]) > 0)
        {
          delete[i] <- i
        }
      }
      
      delete <- delete[!is.na(delete)]
      
      num <- c(1:nrow(bat_pinch))
      
      bat_pinch <- bat_pinch[!(num %in% delete),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[1,] 
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      box_stat_visit$GameDate <- as.character(box_stat_visit$GameDate)
      
      box_stat_visit <- rbind(box_stat_visit, stat)
      
    }
    
  }
  
  if(POS %in% "4"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("4","5","6"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_visit$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_visit$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
    
    if(nrow(fielder_available) > 0)
    {
      bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      box_stat_visit <- rbind(box_stat_visit,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_visit$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      fielding_available$GameDate <- as.Date(fielding_available$GameDate)
      
      fielding_available$MLBId <- as.character(fielding_available$MLBId)
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch$MLBId <- as.character(bat_pinch$MLBId)
      
      delete <- vector()
      
      for(i in 1:nrow(bat_pinch))
      {
        mlbid <- bat_pinch$MLBId[i]
        gamedate <- bat_pinch$GameDate[i]
        
        if(nrow(fielding_available[(fielding_available$MLBId %in% mlbid) & (fielding_available$GameDate %in% gamedate),]) > 0)
        {
          delete[i] <- i
        }
      }
      
      delete <- delete[!is.na(delete)]
      
      num <- c(1:nrow(bat_pinch))
      
      bat_pinch <- bat_pinch[!(num %in% delete),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[1,] 
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      box_stat_visit$GameDate <- as.character(box_stat_visit$GameDate)
      
      box_stat_visit <- rbind(box_stat_visit, stat)
      
    }
    
  }
  
  if(POS %in% "5"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("4","5","6"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_visit$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_visit$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
    
    if(nrow(fielder_available) > 0)
    {
      bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      box_stat_visit <- rbind(box_stat_visit,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_visit$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      fielding_available$GameDate <- as.Date(fielding_available$GameDate)
      
      fielding_available$MLBId <- as.character(fielding_available$MLBId)
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch$MLBId <- as.character(bat_pinch$MLBId)
      
      delete <- vector()
      
      for(i in 1:nrow(bat_pinch))
      {
        mlbid <- bat_pinch$MLBId[i]
        gamedate <- bat_pinch$GameDate[i]
        
        if(nrow(fielding_available[(fielding_available$MLBId %in% mlbid) & (fielding_available$GameDate %in% gamedate),]) > 0)
        {
          delete[i] <- i
        }
      }
      
      delete <- delete[!is.na(delete)]
      
      num <- c(1:nrow(bat_pinch))
      
      bat_pinch <- bat_pinch[!(num %in% delete),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[1,] 
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      box_stat_visit$GameDate <- as.character(box_stat_visit$GameDate)
      
      box_stat_visit <- rbind(box_stat_visit, stat)
      
    }
    
  }
  
  if(POS %in% "6"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("4","5","6"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_visit$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_visit$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
    
    if(nrow(fielder_available) > 0)
    {
      bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      box_stat_visit <- rbind(box_stat_visit,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_visit$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      fielding_available$GameDate <- as.Date(fielding_available$GameDate)
      
      fielding_available$MLBId <- as.character(fielding_available$MLBId)
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch$MLBId <- as.character(bat_pinch$MLBId)
    
      delete <- vector()
      
      for(i in 1:nrow(bat_pinch))
      {
        mlbid <- bat_pinch$MLBId[i]
        gamedate <- bat_pinch$GameDate[i]

        if(nrow(fielding_available[(fielding_available$MLBId %in% mlbid) & (fielding_available$GameDate %in% gamedate),]) > 0)
        {
          delete[i] <- i
        }
      }
      
      delete <- delete[!is.na(delete)]
      
      num <- c(1:nrow(bat_pinch))
      
      bat_pinch <- bat_pinch[!(num %in% delete),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[1,] 
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      box_stat_visit$GameDate <- as.character(box_stat_visit$GameDate)
      
      box_stat_visit <- rbind(box_stat_visit, stat)
      
    }
    
  }
  

}