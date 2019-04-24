batting_col <- c("GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                 "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                 "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")

box_stat_home <- data.frame(matrix("", nrow = 1, ncol = length(batting_col)))

colnames(box_stat_home) <- batting_col


for(i in 1:9)
{
  if(box_home$MLBId[i] %in% batting_start$MLBId){
    
    stat <- batting_start[which(batting_start$MLBId %in% box_home$MLBId[i]),]
    
    stat <- stat[which(stat$GameDate %in% max(stat$GameDate)),]
    
    box_stat_home <- rbind(box_stat_home, stat)
    
  }
  
  if(!(box_home$MLBId[i] %in% batting_start$MLBId))
  {
    if(box_home$POS[i] == "1"){
      team_name <- play_pos$Team_RFB[which((play_pos$MLBId %in% box_home$MLBId[i]) & (play_pos$Pos %in% box_home$POS[i]))]
      bench_player <- batting_bench$MLBId[which(batting_bench$MLBId %in% box_home$MLBId[i])]
      bench_player <- unique(bench_player)
      bench_player <- bench_player[!(bench_player %in% box_home$MLBId)]
      available_bench <- batting_bench[which(batting_start$MLBId %in% bench_player),]
      
      if(nrow(available_bench) > 0){
        
        stat <- available_bench[which(available_bench$GameDate %in% max(available_bench$GameDate)),]
        
        box_stat_home <- rbind(box_stat_home, stat)
      }
      
      if(nrow(available_bench) == 0){
        
        team_name <- play_pos$Team_RFB[which((play_pos$MLBId %in% box_home$MLBId[i]) & (play_pos$Pos %in% box_home$POS[i]))]
        bench_player <- batting_pinch$MLBId[which(batting_pinch$MLBId %in% box_home$MLBId[i])]
        bench_player <- unique(bench_player)
        bench_player <- bench_player[!(bench_player %in% box_home$MLBId[i])]
        available_pinch <- batting_pinch[which(batting_pinch$MLBId %in% bench_player),]
        
        if(nrow(available_pinch) > 0){
          stat <- available_pinch[which(available_pinch$GameDate %in% max(available_pinch$GameDate)),]
          
          box_stat_home <- rbind(box_stat_home, available_pinch)
        }
        
        if(nrow(available_pinch) == 0){
          
          bench_player <- bench_player[!(bench_player %in% box_home$MLBId)]
          stat <- batting_pinch[which(batting_pinch$MLBId %in% bench_player),]
          box_stat_home <- rbind(box_stat_home, stat)
        }
        
      }
      
    }
    
    if(box_home$POS[i] == "2"){
      
      # Team name of player in line up
      
      team_name <- play_pos$Team_RFB[which((play_pos$MLBId %in% box_home$MLBId[i]) & (play_pos$Pos %in% box_home$POS[i]))]
      
      # All players in the team with same position
      
      bench_player <- play_pos$MLBId[which((play_pos$Team_RFB %in% team_name) & (play_pos$Pos %in% c("2","3","4","5","6","7","8","9")))]
      
      # Make the list containing only unique players
      
      bench_player <- unique(bench_player)
      
      # Exclude players who are in starting line up, they have no starting stats and they can't be playing the game anyway
      
      bench_player <- bench_player[!(bench_player %in% box_home$MLBId)]
      
      # From Batting master starting stats, retrieve players from bench who has starting lineup stats from the past
      
      available_starter <- batting_start[which(batting_start$MLBId %in% bench_player),]
      
      available_starter$GameDate <- as.Date(available_starter$GameDate)
      
      # Filter out who may already have been filled for today's game
      
      available_starter <- available_starter[which(!(available_starter$MLBId %in% box_stat_home$MLBId)),]
      
      # Looking to see how many players in available_starter could be eligible to start
      
      fielder_available <- fielding_available[which((fielding_available$Pos %in% c("2")) & (fielding_available$Team == team_name) & (fielding_available$INN > 4)),]
      
      # Again, exclude someone already in lineup
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      
      fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
      
      # Filter out players who can't play the position
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      fielder_available$GameDate <- as.Date(fielder_available$GameDate)
      
      available_starter <- available_starter[which((available_starter$MLBId %in% fielder_available$MLBId) & (available_starter$GameDate %in% fielder_available$GameDate)),]
      
      if(nrow(available_starter) > 0){ # If you find someone with stats from bench, you will take this course of code.
        
        # Isolate bench player's starter stats and save it to 'stat'. You will take the oldest stats available.
        
        stat <- available_starter[which(available_starter$GameDate %in% max(available_starter$GameDate)),]
        
        # Make sure 'stat' does not contain what is already implemented in box_stat_home
        
        box_stat_home$GameDate <- as.Date(box_stat_home$GameDate)
        
        box_stat_home$PlayerName <- as.character(box_stat_home$PlayerName)
        
        box_stat_home$MLBId <- as.character(box_stat_home$MLBId)
        
        stat <- stat[!((stat$GameDate %in% box_stat_home$GameDate) & (stat$PlayerName %in% box_stat_home$PlayerName) & (stat$MLBId %in% box_stat_home$MLBId)),]
        
        # Paste to box_stat_home
        
        box_stat_home <- rbind(box_stat_home, stat)
      }
      
      if(nrow(available_starter) == 0){ # If you could not get any stats from your team, including from bench player, take this course of action
        
        # See if you can find something else based on fielding available master file, where it may have more players with 
        # starting stats in particular.
        
        fielder_available <- fielding_available[which((fielding_available$Pos %in% c("2")) & (fielding_available$Team == team_name) & (fielding_available$INN > 5)),]
        
        # Again, exclude someone already in lineup
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
        
        # Take the oldest stats available
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        fielder_available <- fielder_available[which(fielder_available$GameDate %in% max(fielder_available$GameDate)),]
        
        # Remove somebody who may be already on the line up for today's game
        
        fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
        
        # Look for somebody from bench master file, where they may have player with 2PA. If, so I can call PH to ensure
        # we have 3PA for this game
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        
        batting_bench$GameDate <- as.Date(batting_bench$GameDate)
        
        batting_bench <- batting_bench[which((batting_bench$MLBId %in% fielder_available$MLBId) & (batting_bench$GameDate %in% fielder_available$GameDate)),]
        
        available_bench <- batting_bench
        
        if(nrow(available_bench) > 0){ # If you found someone off the bench, take this course of action
          available_bench <- available_bench[which(available_bench$GameDate %in% max(available_bench$GameDate)),]
          
          available_bench <- available_bench[!((available_bench$GameDate %in% box_stat_home$GameDate) & (available_bench$PlayerName %in% box_stat_home$PlayerName) & (available_bench$MLBId %in% box_stat_home$MLBId)),]
          
          
          box_stat_home <- rbind(box_stat_home, available_bench)
        }
        
        if(nrow(available_bench) == 0){ # If you still haven't found anyone, take this action.
          
          bench_player <- bench_player[!(bench_player %in% box_home$MLBId[i])]
          available_pinch <- batting_pinch[which(batting_pinch$MLBId %in% bench_player),]
          available_pinch <- available_pinch[available_pinch$GameDate %in% max(available_pinch$GameDate),]
          available_pinch <- available_pinch[!((available_pinch$MLBId %in% box_stat_home$MLBId) & (available_pinch$GameDate %in% box_stat_home$GameDate)),]
          box_stat_home <- rbind(box_stat_home, available_pinch)
        }
        
        
        
      }
      
      
    }
    
    if(box_home$POS[i] == "3"){
      
      # Team name of player in line up
      
      team_name <- play_pos$Team_RFB[which((play_pos$MLBId %in% box_home$MLBId[i]) & (play_pos$Pos %in% box_home$POS[i]))]
      
      # All players in the team with same position
      
      bench_player <- play_pos$MLBId[which((play_pos$Team_RFB %in% team_name) & (play_pos$Pos %in% c("2","3","4","5","6","7","8","9")))]
      
      # Make the list containing only unique players
      
      bench_player <- unique(bench_player)
      
      # Exclude players who are in starting line up, they have no starting stats and they can't be playing the game anyway
      
      bench_player <- bench_player[!(bench_player %in% box_home$MLBId)]
      
      # From Batting master starting stats, retrieve players from bench who has starting lineup stats from the past
      
      available_starter <- batting_start[which(batting_start$MLBId %in% bench_player),]
      
      available_starter$GameDate <- as.Date(available_starter$GameDate)
      
      # Filter out who may already have been filled for today's game
      
      available_starter <- available_starter[which(!(available_starter$MLBId %in% box_stat_home$MLBId)),]
      
      # Looking to see how many players in available_starter could be eligible to start
      
      fielder_available <- fielding_available[which((fielding_available$Pos %in% c("3")) & (fielding_available$Team == team_name) & (fielding_available$INN > 4)),]
      
      # Again, exclude someone already in lineup
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      
      fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
      
      # Filter out players who can't play the position
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      fielder_available$GameDate <- as.Date(fielder_available$GameDate)
      
      available_starter <- available_starter[which((available_starter$MLBId %in% fielder_available$MLBId) & (available_starter$GameDate %in% fielder_available$GameDate)),]
      
      if(nrow(available_starter) > 0){ # If you find someone with stats from bench, you will take this course of code.
        
        # Isolate bench player's starter stats and save it to 'stat'. You will take the oldest stats available.
        
        stat <- available_starter[which(available_starter$GameDate %in% max(available_starter$GameDate)),]
        
        # Make sure 'stat' does not contain what is already implemented in box_stat_home
        
        box_stat_home$GameDate <- as.Date(box_stat_home$GameDate)
        
        box_stat_home$PlayerName <- as.character(box_stat_home$PlayerName)
        
        box_stat_home$MLBId <- as.character(box_stat_home$MLBId)
        
        stat <- stat[!((stat$GameDate %in% box_stat_home$GameDate) & (stat$PlayerName %in% box_stat_home$PlayerName) & (stat$MLBId %in% box_stat_home$MLBId)),]
        
        # Paste to box_stat_home
        
        box_stat_home <- rbind(box_stat_home, stat)
      }
      
      if(nrow(available_starter) == 0){ # If you could not get any stats from your team, including from bench player, take this course of action
        
        # See if you can find something else based on fielding available master file, where it may have more players with 
        # starting stats in particular.
        
        fielder_available <- fielding_available[which((fielding_available$Pos %in% c("3")) & (fielding_available$Team == team_name) & (fielding_available$INN > 5)),]
        
        # Again, exclude someone already in lineup
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
        
        # Take the oldest stats available
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        fielder_available <- fielder_available[which(fielder_available$GameDate %in% max(fielder_available$GameDate)),]
        
        # Remove somebody who may be already on the line up for today's game
        
        fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
        
        # Look for somebody from bench master file, where they may have player with 2PA. If, so I can call PH to ensure
        # we have 3PA for this game
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        
        batting_bench$GameDate <- as.Date(batting_bench$GameDate)
        
        batting_bench <- batting_bench[which((batting_bench$MLBId %in% fielder_available$MLBId) & (batting_bench$GameDate %in% fielder_available$GameDate)),]
        
        available_bench <- batting_bench
        
        if(nrow(available_bench) > 0){ # If you found someone off the bench, take this course of action
          available_bench <- available_bench[which(available_bench$GameDate %in% max(available_bench$GameDate)),]
          
          available_bench <- available_bench[!((available_bench$GameDate %in% box_stat_home$GameDate) & (available_bench$PlayerName %in% box_stat_home$PlayerName) & (available_bench$MLBId %in% box_stat_home$MLBId)),]
          
          
          box_stat_home <- rbind(box_stat_home, available_bench)
        }
        
        if(nrow(available_bench) == 0){ # If you still haven't found anyone, take this action.
          
          bench_player <- bench_player[!(bench_player %in% box_home$MLBId[i])]
          available_pinch <- batting_pinch[which(batting_pinch$MLBId %in% bench_player),]
          available_pinch <- available_pinch[available_pinch$GameDate %in% max(available_pinch$GameDate),]
          available_pinch <- available_pinch[!((available_pinch$MLBId %in% box_stat_home$MLBId) & (available_pinch$GameDate %in% box_stat_home$GameDate)),]
          box_stat_home <- rbind(box_stat_home, available_pinch)
        }
        
        
        
      }
      
      
    }
    
    if(box_home$POS[i] == "4"){
      
      # Team name of player in line up
      
      team_name <- play_pos$Team_RFB[which((play_pos$MLBId %in% box_home$MLBId[i]) & (play_pos$Pos %in% box_home$POS[i]))]
      
      # All players in the team with same position
      
      bench_player <- play_pos$MLBId[which((play_pos$Team_RFB %in% team_name) & (play_pos$Pos %in% c("2","3","4","5","6","7","8","9")))]
      
      # Make the list containing only unique players
      
      bench_player <- unique(bench_player)
      
      # Exclude players who are in starting line up, they have no starting stats and they can't be playing the game anyway
      
      bench_player <- bench_player[!(bench_player %in% box_home$MLBId)]
      
      # From Batting master starting stats, retrieve players from bench who has starting lineup stats from the past
      
      available_starter <- batting_start[which(batting_start$MLBId %in% bench_player),]
      
      available_starter$GameDate <- as.Date(available_starter$GameDate)
      
      # Filter out who may already have been filled for today's game
      
      available_starter <- available_starter[which(!(available_starter$MLBId %in% box_stat_home$MLBId)),]
      
      # Looking to see how many players in available_starter could be eligible to start
      
      fielder_available <- fielding_available[which((fielding_available$Pos %in% c("4","5","6")) & (fielding_available$Team == team_name) & (fielding_available$INN > 4)),]
      
      # Again, exclude someone already in lineup
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      
      fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
      
      # Filter out players who can't play the position
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      fielder_available$GameDate <- as.Date(fielder_available$GameDate)
      
      available_starter <- available_starter[which((available_starter$MLBId %in% fielder_available$MLBId) & (available_starter$GameDate %in% fielder_available$GameDate)),]
      
      if(nrow(available_starter) > 0){ # If you find someone with stats from bench, you will take this course of code.
        
        # Isolate bench player's starter stats and save it to 'stat'. You will take the oldest stats available.
        
        stat <- available_starter[which(available_starter$GameDate %in% max(available_starter$GameDate)),]
        
        # Make sure 'stat' does not contain what is already implemented in box_stat_home
        
        box_stat_home$GameDate <- as.Date(box_stat_home$GameDate)
        
        box_stat_home$PlayerName <- as.character(box_stat_home$PlayerName)
        
        box_stat_home$MLBId <- as.character(box_stat_home$MLBId)
        
        stat <- stat[!((stat$GameDate %in% box_stat_home$GameDate) & (stat$PlayerName %in% box_stat_home$PlayerName) & (stat$MLBId %in% box_stat_home$MLBId)),]
        
        # Paste to box_stat_home
        
        box_stat_home <- rbind(box_stat_home, stat)
      }
      
      if(nrow(available_starter) == 0){ # If you could not get any stats from your team, including from bench player, take this course of action
        
        # See if you can find something else based on fielding available master file, where it may have more players with 
        # starting stats in particular.
        
        fielder_available <- fielding_available[which((fielding_available$Pos %in% c("4","5","6")) & (fielding_available$Team == team_name) & (fielding_available$INN > 5)),]
        
        # Again, exclude someone already in lineup
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
        
        # Take the oldest stats available
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        fielder_available <- fielder_available[which(fielder_available$GameDate %in% max(fielder_available$GameDate)),]
        
        # Remove somebody who may be already on the line up for today's game
        
        fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
        
        # Look for somebody from bench master file, where they may have player with 2PA. If, so I can call PH to ensure
        # we have 3PA for this game
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        
        batting_bench$GameDate <- as.Date(batting_bench$GameDate)
        
        batting_bench <- batting_bench[which((batting_bench$MLBId %in% fielder_available$MLBId) & (batting_bench$GameDate %in% fielder_available$GameDate)),]
        
        available_bench <- batting_bench
        
        if(nrow(available_bench) > 0){ # If you found someone off the bench, take this course of action
          available_bench <- available_bench[which(available_bench$GameDate %in% max(available_bench$GameDate)),]
          
          available_bench <- available_bench[!((available_bench$GameDate %in% box_stat_home$GameDate) & (available_bench$PlayerName %in% box_stat_home$PlayerName) & (available_bench$MLBId %in% box_stat_home$MLBId)),]
          
          
          box_stat_home <- rbind(box_stat_home, available_bench)
        }
        
        if(nrow(available_bench) == 0){ # If you still haven't found anyone, take this action.
          
          bench_player <- bench_player[!(bench_player %in% box_home$MLBId[i])]
          available_pinch <- batting_pinch[which(batting_pinch$MLBId %in% bench_player),]
          available_pinch <- available_pinch[available_pinch$GameDate %in% max(available_pinch$GameDate),]
          available_pinch <- available_pinch[!((available_pinch$MLBId %in% box_stat_home$MLBId) & (available_pinch$GameDate %in% box_stat_home$GameDate)),]
          box_stat_home <- rbind(box_stat_home, available_pinch)
        }
        
        
        
      }
      
      
    }
    
    if(box_home$POS[i] == "5"){
      
      # Team name of player in line up
      
      team_name <- play_pos$Team_RFB[which((play_pos$MLBId %in% box_home$MLBId[i]) & (play_pos$Pos %in% box_home$POS[i]))]
      
      # All players in the team with same position
      
      bench_player <- play_pos$MLBId[which((play_pos$Team_RFB %in% team_name) & (play_pos$Pos %in% c("2","3","4","5","6","7","8","9")))]
      
      # Make the list containing only unique players
      
      bench_player <- unique(bench_player)
      
      # Exclude players who are in starting line up, they have no starting stats and they can't be playing the game anyway
      
      bench_player <- bench_player[!(bench_player %in% box_home$MLBId)]
      
      # From Batting master starting stats, retrieve players from bench who has starting lineup stats from the past
      
      available_starter <- batting_start[which(batting_start$MLBId %in% bench_player),]
      
      available_starter$GameDate <- as.Date(available_starter$GameDate)
      
      # Filter out who may already have been filled for today's game
      
      available_starter <- available_starter[which(!(available_starter$MLBId %in% box_stat_home$MLBId)),]
      
      # Looking to see how many players in available_starter could be eligible to start
      
      fielder_available <- fielding_available[which((fielding_available$Pos %in% c("4","5","6")) & (fielding_available$Team == team_name) & (fielding_available$INN > 4)),]
      
      # Again, exclude someone already in lineup
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      
      fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
      
      # Filter out players who can't play the position
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      fielder_available$GameDate <- as.Date(fielder_available$GameDate)
      
      available_starter <- available_starter[which((available_starter$MLBId %in% fielder_available$MLBId) & (available_starter$GameDate %in% fielder_available$GameDate)),]
      
      if(nrow(available_starter) > 0){ # If you find someone with stats from bench, you will take this course of code.
        
        # Isolate bench player's starter stats and save it to 'stat'. You will take the oldest stats available.
        
        stat <- available_starter[which(available_starter$GameDate %in% max(available_starter$GameDate)),]
        
        # Make sure 'stat' does not contain what is already implemented in box_stat_home
        
        box_stat_home$GameDate <- as.Date(box_stat_home$GameDate)
        
        box_stat_home$PlayerName <- as.character(box_stat_home$PlayerName)
        
        box_stat_home$MLBId <- as.character(box_stat_home$MLBId)
        
        stat <- stat[!((stat$GameDate %in% box_stat_home$GameDate) & (stat$PlayerName %in% box_stat_home$PlayerName) & (stat$MLBId %in% box_stat_home$MLBId)),]
        
        # Paste to box_stat_home
        
        box_stat_home <- rbind(box_stat_home, stat)
      }
      
      if(nrow(available_starter) == 0){ # If you could not get any stats from your team, including from bench player, take this course of action
        
        # See if you can find something else based on fielding available master file, where it may have more players with 
        # starting stats in particular.
        
        fielder_available <- fielding_available[which((fielding_available$Pos %in% c("4","5","6")) & (fielding_available$Team == team_name) & (fielding_available$INN > 5)),]
        
        # Again, exclude someone already in lineup
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
        
        # Take the oldest stats available
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        fielder_available <- fielder_available[which(fielder_available$GameDate %in% max(fielder_available$GameDate)),]
        
        # Remove somebody who may be already on the line up for today's game
        
        fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
        
        # Look for somebody from bench master file, where they may have player with 2PA. If, so I can call PH to ensure
        # we have 3PA for this game
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        
        batting_bench$GameDate <- as.Date(batting_bench$GameDate)
        
        batting_bench <- batting_bench[which((batting_bench$MLBId %in% fielder_available$MLBId) & (batting_bench$GameDate %in% fielder_available$GameDate)),]
        
        available_bench <- batting_bench
        
        if(nrow(available_bench) > 0){ # If you found someone off the bench, take this course of action
          available_bench <- available_bench[which(available_bench$GameDate %in% max(available_bench$GameDate)),]
          
          available_bench <- available_bench[!((available_bench$GameDate %in% box_stat_home$GameDate) & (available_bench$PlayerName %in% box_stat_home$PlayerName) & (available_bench$MLBId %in% box_stat_home$MLBId)),]
          
          
          box_stat_home <- rbind(box_stat_home, available_bench)
        }
        
        if(nrow(available_bench) == 0){ # If you still haven't found anyone, take this action.
          
          bench_player <- bench_player[!(bench_player %in% box_home$MLBId[i])]
          available_pinch <- batting_pinch[which(batting_pinch$MLBId %in% bench_player),]
          available_pinch <- available_pinch[available_pinch$GameDate %in% max(available_pinch$GameDate),]
          available_pinch <- available_pinch[!((available_pinch$MLBId %in% box_stat_home$MLBId) & (available_pinch$GameDate %in% box_stat_home$GameDate)),]
          box_stat_home <- rbind(box_stat_home, available_pinch)
        }
        
        
        
      }
      
      
    }
    
    if(box_home$POS[i] == "6"){
      
      # Team name of player in line up
      
      team_name <- play_pos$Team_RFB[which((play_pos$MLBId %in% box_home$MLBId[i]) & (play_pos$Pos %in% box_home$POS[i]))]
      
      # All players in the team with same position
      
      bench_player <- play_pos$MLBId[which((play_pos$Team_RFB %in% team_name) & (play_pos$Pos %in% c("2","3","4","5","6","7","8","9")))]
      
      # Make the list containing only unique players
      
      bench_player <- unique(bench_player)
      
      # Exclude players who are in starting line up, they have no starting stats and they can't be playing the game anyway
      
      bench_player <- bench_player[!(bench_player %in% box_home$MLBId)]
      
      # From Batting master starting stats, retrieve players from bench who has starting lineup stats from the past
      
      available_starter <- batting_start[which(batting_start$MLBId %in% bench_player),]
      
      available_starter$GameDate <- as.Date(available_starter$GameDate)
      
      # Filter out who may already have been filled for today's game
      
      available_starter <- available_starter[which(!(available_starter$MLBId %in% box_stat_home$MLBId)),]
      
      # Looking to see how many players in available_starter could be eligible to start
      
      fielder_available <- fielding_available[which((fielding_available$Pos %in% c("4","5","6")) & (fielding_available$Team == team_name) & (fielding_available$INN > 4)),]
      
      # Again, exclude someone already in lineup
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      
      fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
      
      # Filter out players who can't play the position
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      fielder_available$GameDate <- as.Date(fielder_available$GameDate)
      
      available_starter <- available_starter[which((available_starter$MLBId %in% fielder_available$MLBId) & (available_starter$GameDate %in% fielder_available$GameDate)),]
      
      if(nrow(available_starter) > 0){ # If you find someone with stats from bench, you will take this course of code.
        
        # Isolate bench player's starter stats and save it to 'stat'. You will take the oldest stats available.
        
        stat <- available_starter[which(available_starter$GameDate %in% max(available_starter$GameDate)),]
        
        # Make sure 'stat' does not contain what is already implemented in box_stat_home
        
        box_stat_home$GameDate <- as.Date(box_stat_home$GameDate)
        
        box_stat_home$PlayerName <- as.character(box_stat_home$PlayerName)
        
        box_stat_home$MLBId <- as.character(box_stat_home$MLBId)
        
        stat <- stat[!((stat$GameDate %in% box_stat_home$GameDate) & (stat$PlayerName %in% box_stat_home$PlayerName) & (stat$MLBId %in% box_stat_home$MLBId)),]
        
        # Paste to box_stat_home
        
        box_stat_home <- rbind(box_stat_home, stat)
      }
      
      if(nrow(available_starter) == 0){ # If you could not get any stats from your team, including from bench player, take this course of action
        
        # See if you can find something else based on fielding available master file, where it may have more players with 
        # starting stats in particular.
        
        fielder_available <- fielding_available[which((fielding_available$Pos %in% c("6")) & (fielding_available$Team == team_name) & (fielding_available$INN > 5)),]
        
        # Again, exclude someone already in lineup
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
        
        # Take the oldest stats available
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        fielder_available <- fielder_available[which(fielder_available$GameDate %in% max(fielder_available$GameDate)),]
        
        # Remove somebody who may be already on the line up for today's game
        
        fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
        
        # Look for somebody from bench master file, where they may have player with 2PA. If, so I can call PH to ensure
        # we have 3PA for this game
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        
        batting_bench$GameDate <- as.Date(batting_bench$GameDate)
        
        batting_bench <- batting_bench[which((batting_bench$MLBId %in% fielder_available$MLBId) & (batting_bench$GameDate %in% fielder_available$GameDate)),]
        
        available_bench <- batting_bench
        
        if(nrow(available_bench) > 0){ # If you found someone off the bench, take this course of action
          available_bench <- available_bench[which(available_bench$GameDate %in% max(available_bench$GameDate)),]
          
          available_bench <- available_bench[!((available_bench$GameDate %in% box_stat_home$GameDate) & (available_bench$PlayerName %in% box_stat_home$PlayerName) & (available_bench$MLBId %in% box_stat_home$MLBId)),]
          
          
          box_stat_home <- rbind(box_stat_home, available_bench)
        }
        
        if(nrow(available_bench) == 0){ # If you still haven't found anyone, take this action.
          
          bench_player <- bench_player[!(bench_player %in% box_home$MLBId[i])]
          available_pinch <- batting_pinch[which(batting_pinch$MLBId %in% bench_player),]
          available_pinch <- available_pinch[available_pinch$GameDate %in% max(available_pinch$GameDate),]
          available_pinch <- available_pinch[!((available_pinch$MLBId %in% box_stat_home$MLBId) & (available_pinch$GameDate %in% box_stat_home$GameDate)),]
          box_stat_home <- rbind(box_stat_home, available_pinch)
        }
        
        
        
      }
      
      
    }
    
    if(box_home$POS[i] == "7"){
      
      # Team name of player in line up
      
      team_name <- play_pos$Team_RFB[which((play_pos$MLBId %in% box_home$MLBId[i]) & (play_pos$Pos %in% box_home$POS[i]))]
      
      # All players in the team with same position
      
      bench_player <- play_pos$MLBId[which((play_pos$Team_RFB %in% team_name) & (play_pos$Pos %in% c("2","3","4","5","6","7","8","9")))]
      
      # Make the list containing only unique players
      
      bench_player <- unique(bench_player)
      
      # Exclude players who are in starting line up, they have no starting stats and they can't be playing the game anyway
      
      bench_player <- bench_player[!(bench_player %in% box_home$MLBId)]
      
      # From Batting master starting stats, retrieve players from bench who has starting lineup stats from the past
      
      available_starter <- batting_start[which(batting_start$MLBId %in% bench_player),]
      
      available_starter$GameDate <- as.Date(available_starter$GameDate)
      
      # Filter out who may already have been filled for today's game
      
      available_starter <- available_starter[which(!(available_starter$MLBId %in% box_stat_home$MLBId)),]
      
      # Looking to see how many players in available_starter could be eligible to start
      
      fielder_available <- fielding_available[which((fielding_available$Pos %in% c("7","8","9")) & (fielding_available$Team == team_name) & (fielding_available$INN > 4)),]
      
      # Again, exclude someone already in lineup
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      
      fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
      
      # Filter out players who can't play the position
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      fielder_available$GameDate <- as.Date(fielder_available$GameDate)
      
      available_starter <- available_starter[which((available_starter$MLBId %in% fielder_available$MLBId) & (available_starter$GameDate %in% fielder_available$GameDate)),]
      
      if(nrow(available_starter) > 0){ # If you find someone with stats from bench, you will take this course of code.
        
        # Isolate bench player's starter stats and save it to 'stat'. You will take the oldest stats available.
        
        stat <- available_starter[which(available_starter$GameDate %in% max(available_starter$GameDate)),]
        
        # Make sure 'stat' does not contain what is already implemented in box_stat_home
        
        box_stat_home$GameDate <- as.Date(box_stat_home$GameDate)
        
        box_stat_home$PlayerName <- as.character(box_stat_home$PlayerName)
        
        box_stat_home$MLBId <- as.character(box_stat_home$MLBId)
        
        stat <- stat[!((stat$GameDate %in% box_stat_home$GameDate) & (stat$PlayerName %in% box_stat_home$PlayerName) & (stat$MLBId %in% box_stat_home$MLBId)),]
        
        # Paste to box_stat_home
        
        box_stat_home <- rbind(box_stat_home, stat)
      }
      
      if(nrow(available_starter) == 0){ # If you could not get any stats from your team, including from bench player, take this course of action
        
        # See if you can find something else based on fielding available master file, where it may have more players with 
        # starting stats in particular.
        
        fielder_available <- fielding_available[which((fielding_available$Pos %in% c("7","8","9")) & (fielding_available$Team == team_name) & (fielding_available$INN > 5)),]
        
        # Again, exclude someone already in lineup
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
        
        # Take the oldest stats available
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        fielder_available <- fielder_available[which(fielder_available$GameDate %in% max(fielder_available$GameDate)),]
        
        # Remove somebody who may be already on the line up for today's game
        
        fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
        
        # Look for somebody from bench master file, where they may have player with 2PA. If, so I can call PH to ensure
        # we have 3PA for this game
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        
        batting_bench$GameDate <- as.Date(batting_bench$GameDate)
        
        batting_bench <- batting_bench[which((batting_bench$MLBId %in% fielder_available$MLBId) & (batting_bench$GameDate %in% fielder_available$GameDate)),]
        
        available_bench <- batting_bench
        
        if(nrow(available_bench) > 0){ # If you found someone off the bench, take this course of action
          available_bench <- available_bench[which(available_bench$GameDate %in% max(available_bench$GameDate)),]
          
          available_bench <- available_bench[!((available_bench$GameDate %in% box_stat_home$GameDate) & (available_bench$PlayerName %in% box_stat_home$PlayerName) & (available_bench$MLBId %in% box_stat_home$MLBId)),]
          
          
          box_stat_home <- rbind(box_stat_home, available_bench)
        }
        
        if(nrow(available_bench) == 0){ # If you still haven't found anyone, take this action.
          
          bench_player <- bench_player[!(bench_player %in% box_home$MLBId[i])]
          available_pinch <- batting_pinch[which(batting_pinch$MLBId %in% bench_player),]
          available_pinch <- available_pinch[available_pinch$GameDate %in% max(available_pinch$GameDate),]
          available_pinch <- available_pinch[!((available_pinch$MLBId %in% box_stat_home$MLBId) & (available_pinch$GameDate %in% box_stat_home$GameDate)),]
          box_stat_home <- rbind(box_stat_home, available_pinch)
        }
        
        
        
      }
      
      
    }
    
    if(box_home$POS[i] == "8"){
      
      # Team name of player in line up
      
      team_name <- play_pos$Team_RFB[which((play_pos$MLBId %in% box_home$MLBId[i]) & (play_pos$Pos %in% box_home$POS[i]))]
      
      # All players in the team with same position
      
      bench_player <- play_pos$MLBId[which((play_pos$Team_RFB %in% team_name) & (play_pos$Pos %in% c("2","3","4","5","6","7","8","9")))]
      
      # Make the list containing only unique players
      
      bench_player <- unique(bench_player)
      
      # Exclude players who are in starting line up, they have no starting stats and they can't be playing the game anyway
      
      bench_player <- bench_player[!(bench_player %in% box_home$MLBId)]
      
      # From Batting master starting stats, retrieve players from bench who has starting lineup stats from the past
      
      available_starter <- batting_start[which(batting_start$MLBId %in% bench_player),]
      
      available_starter$GameDate <- as.Date(available_starter$GameDate)
      
      # Filter out who may already have been filled for today's game
      
      available_starter <- available_starter[which(!(available_starter$MLBId %in% box_stat_home$MLBId)),]
      
      # Looking to see how many players in available_starter could be eligible to start
      
      fielder_available <- fielding_available[which((fielding_available$Pos %in% c("7","8","9")) & (fielding_available$Team == team_name) & (fielding_available$INN > 4)),]
      
      # Again, exclude someone already in lineup
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      
      fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
      
      # Filter out players who can't play the position
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      fielder_available$GameDate <- as.Date(fielder_available$GameDate)
      
      available_starter <- available_starter[which((available_starter$MLBId %in% fielder_available$MLBId) & (available_starter$GameDate %in% fielder_available$GameDate)),]
      
      if(nrow(available_starter) > 0){ # If you find someone with stats from bench, you will take this course of code.
        
        # Isolate bench player's starter stats and save it to 'stat'. You will take the oldest stats available.
        
        stat <- available_starter[which(available_starter$GameDate %in% max(available_starter$GameDate)),]
        
        # Make sure 'stat' does not contain what is already implemented in box_stat_home
        
        box_stat_home$GameDate <- as.Date(box_stat_home$GameDate)
        
        box_stat_home$PlayerName <- as.character(box_stat_home$PlayerName)
        
        box_stat_home$MLBId <- as.character(box_stat_home$MLBId)
        
        stat <- stat[!((stat$GameDate %in% box_stat_home$GameDate) & (stat$PlayerName %in% box_stat_home$PlayerName) & (stat$MLBId %in% box_stat_home$MLBId)),]
        
        # Paste to box_stat_home
        
        box_stat_home <- rbind(box_stat_home, stat)
      }
      
      if(nrow(available_starter) == 0){ # If you could not get any stats from your team, including from bench player, take this course of action
        
        # See if you can find something else based on fielding available master file, where it may have more players with 
        # starting stats in particular.
        
        fielder_available <- fielding_available[which((fielding_available$Pos %in% c("7","8","9")) & (fielding_available$Team == team_name) & (fielding_available$INN > 5)),]
        
        # Again, exclude someone already in lineup
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
        
        # Take the oldest stats available
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        fielder_available <- fielder_available[which(fielder_available$GameDate %in% max(fielder_available$GameDate)),]
        
        # Remove somebody who may be already on the line up for today's game
        
        fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
        
        # Look for somebody from bench master file, where they may have player with 2PA. If, so I can call PH to ensure
        # we have 3PA for this game
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        
        batting_bench$GameDate <- as.Date(batting_bench$GameDate)
        
        batting_bench <- batting_bench[which((batting_bench$MLBId %in% fielder_available$MLBId) & (batting_bench$GameDate %in% fielder_available$GameDate)),]
        
        available_bench <- batting_bench
        
        if(nrow(available_bench) > 0){ # If you found someone off the bench, take this course of action
          available_bench <- available_bench[which(available_bench$GameDate %in% max(available_bench$GameDate)),]
          
          available_bench <- available_bench[!((available_bench$GameDate %in% box_stat_home$GameDate) & (available_bench$PlayerName %in% box_stat_home$PlayerName) & (available_bench$MLBId %in% box_stat_home$MLBId)),]
          
          
          box_stat_home <- rbind(box_stat_home, available_bench)
        }
        
        if(nrow(available_bench) == 0){ # If you still haven't found anyone, take this action.
          
          bench_player <- bench_player[!(bench_player %in% box_home$MLBId[i])]
          available_pinch <- batting_pinch[which(batting_pinch$MLBId %in% bench_player),]
          available_pinch <- available_pinch[available_pinch$GameDate %in% max(available_pinch$GameDate),]
          available_pinch <- available_pinch[!((available_pinch$MLBId %in% box_stat_home$MLBId) & (available_pinch$GameDate %in% box_stat_home$GameDate)),]
          box_stat_home <- rbind(box_stat_home, available_pinch)
        }
        
        
        
      }
      
      
    }
    
    if(box_home$POS[i] == "9"){
      
      # Team name of player in line up
      
      team_name <- play_pos$Team_RFB[which((play_pos$MLBId %in% box_home$MLBId[i]) & (play_pos$Pos %in% box_home$POS[i]))]
      
      # All players in the team with same position
      
      bench_player <- play_pos$MLBId[which((play_pos$Team_RFB %in% team_name) & (play_pos$Pos %in% c("2","3","4","5","6","7","8","9")))]
      
      # Make the list containing only unique players
      
      bench_player <- unique(bench_player)
      
      # Exclude players who are in starting line up, they have no starting stats and they can't be playing the game anyway
      
      bench_player <- bench_player[!(bench_player %in% box_home$MLBId)]
      
      # From Batting master starting stats, retrieve players from bench who has starting lineup stats from the past
      
      available_starter <- batting_start[which(batting_start$MLBId %in% bench_player),]
      
      available_starter$GameDate <- as.Date(available_starter$GameDate)
      
      # Filter out who may already have been filled for today's game
      
      available_starter <- available_starter[which(!(available_starter$MLBId %in% box_stat_home$MLBId)),]
      
      # Looking to see how many players in available_starter could be eligible to start
      
      fielder_available <- fielding_available[which((fielding_available$Pos %in% c("7","8","9")) & (fielding_available$Team == team_name) & (fielding_available$INN > 4)),]
      
      # Again, exclude someone already in lineup
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      
      fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
      
      # Filter out players who can't play the position
      
      fielder_available$MLBId <- as.character(fielder_available$MLBId)
      fielder_available$GameDate <- as.Date(fielder_available$GameDate)
      
      available_starter <- available_starter[which((available_starter$MLBId %in% fielder_available$MLBId) & (available_starter$GameDate %in% fielder_available$GameDate)),]
      
      if(nrow(available_starter) > 0){ # If you find someone with stats from bench, you will take this course of code.
        
        # Isolate bench player's starter stats and save it to 'stat'. You will take the oldest stats available.
        
        stat <- available_starter[which(available_starter$GameDate %in% max(available_starter$GameDate)),]
        
        # Make sure 'stat' does not contain what is already implemented in box_stat_home
        
        box_stat_home$GameDate <- as.Date(box_stat_home$GameDate)
        
        box_stat_home$PlayerName <- as.character(box_stat_home$PlayerName)
        
        box_stat_home$MLBId <- as.character(box_stat_home$MLBId)
        
        stat <- stat[!((stat$GameDate %in% box_stat_home$GameDate) & (stat$PlayerName %in% box_stat_home$PlayerName) & (stat$MLBId %in% box_stat_home$MLBId)),]
        
        # Paste to box_stat_home
        
        box_stat_home <- rbind(box_stat_home, stat)
      }
      
      if(nrow(available_starter) == 0){ # If you could not get any stats from your team, including from bench player, take this course of action
        
        # See if you can find something else based on fielding available master file, where it may have more players with 
        # starting stats in particular.
        
        fielder_available <- fielding_available[which((fielding_available$Pos %in% c("7","8","9")) & (fielding_available$Team == team_name) & (fielding_available$INN > 5)),]
        
        # Again, exclude someone already in lineup
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
        
        # Take the oldest stats available
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        fielder_available <- fielder_available[which(fielder_available$GameDate %in% max(fielder_available$GameDate)),]
        
        # Remove somebody who may be already on the line up for today's game
        
        fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
        
        # Look for somebody from bench master file, where they may have player with 2PA. If, so I can call PH to ensure
        # we have 3PA for this game
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        
        batting_bench$GameDate <- as.Date(batting_bench$GameDate)
        
        batting_bench <- batting_bench[which((batting_bench$MLBId %in% fielder_available$MLBId) & (batting_bench$GameDate %in% fielder_available$GameDate)),]
        
        available_bench <- batting_bench
        
        if(nrow(available_bench) > 0){ # If you found someone off the bench, take this course of action
          available_bench <- available_bench[which(available_bench$GameDate %in% max(available_bench$GameDate)),]
          
          available_bench <- available_bench[!((available_bench$GameDate %in% box_stat_home$GameDate) & (available_bench$PlayerName %in% box_stat_home$PlayerName) & (available_bench$MLBId %in% box_stat_home$MLBId)),]
          
          
          box_stat_home <- rbind(box_stat_home, available_bench)
        }
        
        if(nrow(available_bench) == 0){ # If you still haven't found anyone, take this action.
          
          bench_player <- bench_player[!(bench_player %in% box_home$MLBId[i])]
          available_pinch <- batting_pinch[which(batting_pinch$MLBId %in% bench_player),]
          available_pinch <- available_pinch[available_pinch$GameDate %in% max(available_pinch$GameDate),]
          available_pinch <- available_pinch[!((available_pinch$MLBId %in% box_stat_home$MLBId) & (available_pinch$GameDate %in% box_stat_home$GameDate)),]
          box_stat_home <- rbind(box_stat_home, available_pinch)
        }
        
        
        
      }
      
      
    }
    
    if(i == "10")
    {
      blank_home <- data.frame(matrix("",nrow=1,ncol=ncol(box_stat_home)))
      
      colnames(blank_home) <- colnames(box_stat_home)  
      
      box_stat_home <- rbind(box_stat_home, blank_home)
    }
    
  }
}


# Count numbers of players in line up with 2PA and 1PA. If you see one player with 2PA, get one pH. If you see two players
# with 2PA, get two PHs. If you get player with 1PA, get one bench player or two PHs

# Play one bench player or two PHs for the batter with 1 PA

box_stat_home <- box_stat_home[!(box_stat_home$PlayerName == ""),]

box_stat_home$PA <- c("4","4","4","4","4","4","3","1","2")

box_stat_home$PA <- as.integer(box_stat_home$PA)


count_2 <- which(box_stat_home$PA %in% 2)
count_1 <- which(box_stat_home$PA %in% 1)

# subbing in bench player/pinch hitter in event of starter with only one PA

for(i in 1:length(count_1))
{
  POS <- box_home$POS[box_home$MLBId %in% box_home$MLBId[count_1[i]]]
  
  team_name <- box_home$Team[count_1[i]]
  
  
    
  if(POS %in% "1"){
    
    box_stat_home$MLBId <- as.character(box_stat_home$MLBId)
    
    unplayed <- play_pos$MLBId[(play_pos$Team_RFB %in% final_schedule$Home[x]) & !(play_pos$MLBId %in% box_stat_home$MLBId)]
    
    unplayed <- unique(unplayed)
    
    bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% unplayed),]
    
    bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
    
    stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
    
    box_stat_home <- rbind(box_stat_home,stat)
    
    
    unplayed <- play_pos$MLBId[(play_pos$Team_RFB %in% final_schedule$Home[x]) & !(play_pos$MLBId %in% box_stat_home$MLBId)]
    
    unplayed <- unique(unplayed)
    
    bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% unplayed),]
    
    bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
    
    stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
    
    box_stat_home <- rbind(box_stat_home,stat)
    
  }
  
  if(POS %in% "2"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("2"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$MLBId <- as.character(fielder_available$MLBId)
    
    
    if(nrow(fielder_available) > 0)
    {
      bat_bench <- batting_bench[batting_bench$MLBId %in% fielder_available$MLBId,]
      
      bat_bench$GameDate <- as.character(bat_bench$GameDate)
      
      stat <- bat_bench[bat_bench$GameDate %in% max(bat_bench$GameDate),]
      
      if(nrow(stat) == 0)
      {
        potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
        
        potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
        
        potential_pinch_hitter <- unique(potential_pinch_hitter)
        
        bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
        
        bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
        
        stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
        
        bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
        
        stat <- rbind(stat,bat_pinch[1,])
        
        stat$GameDate <- as.character(stat$GameDate)
        
        box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
        
      }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
  
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[1,] 
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      stat <- rbind(stat,bat_pinch[1,])
      
      stat$GameDate <- as.character(stat$GameDate)
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }
  
  if(POS %in% "3"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("3","4","5","6","7","8","9"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$MLBId <- as.character(fielder_available$MLBId)
    
    
    if(nrow(fielder_available) > 0)
    {
      bat_bench <- batting_bench[batting_bench$MLBId %in% fielder_available$MLBId,]
      
      bat_bench$GameDate <- as.character(bat_bench$GameDate)
      
      stat <- bat_bench[bat_bench$GameDate %in% max(bat_bench$GameDate),]
      
      if(nrow(stat) == 0)
      {
        potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
        
        potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
        
        potential_pinch_hitter <- unique(potential_pinch_hitter)
        
        bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
        
        bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
        
        stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
        
        bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
        
        stat <- rbind(stat,bat_pinch[1,])
        
        stat$GameDate <- as.character(stat$GameDate)
        
        box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
        
      }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
  
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[1,] 
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      stat <- rbind(stat,bat_pinch[1,])
      
      stat$GameDate <- as.character(stat$GameDate)
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }
  
  if(POS %in% "4"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("4","5","6"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$MLBId <- as.character(fielder_available$MLBId)
      
      if(nrow(fielder_available) > 0)
      {
        bat_bench <- batting_bench[batting_bench$MLBId %in% fielder_available$MLBId,]
        
        bat_bench$GameDate <- as.character(bat_bench$GameDate)
        
        stat <- bat_bench[bat_bench$GameDate %in% max(bat_bench$GameDate),]
        
        if(nrow(stat) == 0)
        {
          potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
          
          potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
          
          potential_pinch_hitter <- unique(potential_pinch_hitter)
          
          bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
          
          bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
          
          bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
          
          stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
          
          stat <- rbind(stat,bat_pinch[1,])
          
          stat$GameDate <- as.character(stat$GameDate)
          
          box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
          
        }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[1,] 
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      stat <- rbind(stat,bat_pinch[1,])
      
      stat$GameDate <- as.character(stat$GameDate)
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
    }
  
  
  if(POS %in% "5"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("4","5","6"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$MLBId <- as.character(fielder_available$MLBId)
    
      
      if(nrow(fielder_available) > 0)
      {
        bat_bench <- batting_bench[batting_bench$MLBId %in% fielder_available$MLBId,]
        
        bat_bench$GameDate <- as.character(bat_bench$GameDate)
        
        stat <- bat_bench[bat_bench$GameDate %in% max(bat_bench$GameDate),]
        
        if(nrow(stat) == 0)
        {
          potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
          
          potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
          
          potential_pinch_hitter <- unique(potential_pinch_hitter)
          
          bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
          
          bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
          
          bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
          
          stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
          
          stat <- rbind(stat,bat_pinch[1,])
          
          stat$GameDate <- as.character(stat$GameDate)
          
          box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
          
        }
      
      box_stat_home <- rbind(box_stat_home,stat)
      }
  
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[1,] 
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      stat <- rbind(stat,bat_pinch[1,])
      
      stat$GameDate <- as.character(stat$GameDate)
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
  }
  
  
  if(POS %in% "6"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("4","5","6"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$MLBId <- as.character(fielder_available$MLBId)
      
      if(nrow(fielder_available) > 0)
      {
        bat_bench <- batting_bench[batting_bench$MLBId %in% fielder_available$MLBId,]
        
        bat_bench$GameDate <- as.character(bat_bench$GameDate)
        
        stat <- bat_bench[bat_bench$GameDate %in% max(bat_bench$GameDate),]
        
        if(nrow(stat) == 0)
        {
          potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
          
          potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
          
          potential_pinch_hitter <- unique(potential_pinch_hitter)
          
          bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
          
          bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
          
          bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
          
          stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
          
          stat <- rbind(stat,bat_pinch[1,])
          
          stat$GameDate <- as.character(stat$GameDate)
          
          box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
          
        }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[1,] 
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      stat <- rbind(stat,bat_pinch[1,])
      
      stat$GameDate <- as.character(stat$GameDate)
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }
  
  
  if(POS %in% "7"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("7","8","9"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$MLBId <- as.character(fielder_available$MLBId)
      
      if(nrow(fielder_available) > 0)
      {
        bat_bench <- batting_bench[batting_bench$MLBId %in% fielder_available$MLBId,]
        
        bat_bench$GameDate <- as.character(bat_bench$GameDate)
        
        stat <- bat_bench[bat_bench$GameDate %in% max(bat_bench$GameDate),]
        
        if(nrow(stat) == 0)
        {
          potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
          
          potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
          
          potential_pinch_hitter <- unique(potential_pinch_hitter)
          
          bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
          
          bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
          
          bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
          
          stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
          
          stat <- rbind(stat,bat_pinch[1,])
          
          stat$GameDate <- as.character(stat$GameDate)
          
          box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
          
        }
      
      box_stat_home <- rbind(box_stat_home,stat)
      }
    
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[1,] 
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      stat <- rbind(stat,bat_pinch[1,])
      
      stat$GameDate <- as.character(stat$GameDate)
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
  }
  
  
  if(POS %in% "8"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("7","8","9"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$MLBId <- as.character(fielder_available$MLBId)
    
    
    if(nrow(fielder_available) > 0)
    {
      bat_bench <- batting_bench[batting_bench$MLBId %in% fielder_available$MLBId,]
      
      bat_bench$GameDate <- as.character(bat_bench$GameDate)
      
      stat <- bat_bench[bat_bench$GameDate %in% max(bat_bench$GameDate),]
      
      if(nrow(stat) == 0)
      {
        potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
        
        potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
        
        potential_pinch_hitter <- unique(potential_pinch_hitter)
        
        bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
        
        bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
        
        stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
        
        bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
        
        stat <- rbind(stat,bat_pinch[1,])
        
        stat$GameDate <- as.character(stat$GameDate)
        
        box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
        
      }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[1,] 
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      stat <- rbind(stat,bat_pinch[1,])
      
      stat$GameDate <- as.character(stat$GameDate)
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }
  
  if(POS %in% "9"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("7","8","9"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$MLBId <- as.character(fielder_available$MLBId)
      
      if(nrow(fielder_available) > 0)
      {
        bat_bench <- batting_bench[batting_bench$MLBId %in% fielder_available$MLBId,]
        
        bat_bench$GameDate <- as.character(bat_bench$GameDate)
        
        stat <- bat_bench[bat_bench$GameDate %in% max(bat_bench$GameDate),]
        
        if(nrow(stat) == 0)
        {
          potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
          
          potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
          
          potential_pinch_hitter <- unique(potential_pinch_hitter)
          
          bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
          
          bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
          
          bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
          
          stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
          
          stat <- rbind(stat,bat_pinch[1,])
          
          stat$GameDate <- as.character(stat$GameDate)
          
          box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
          
        }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[1,] 
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      stat <- rbind(stat,bat_pinch[1,])
      
      stat$GameDate <- as.character(stat$GameDate)
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
    }
  
  
  if(POS %in% "DH"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("1","2","3","4","5","6","7","8","9"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$MLBId <- as.character(fielder_available$MLBId)
    
    
    if(nrow(fielder_available) > 0)
    {
      bat_bench <- batting_bench[batting_bench$MLBId %in% fielder_available$MLBId,]
      
      bat_bench$GameDate <- as.character(bat_bench$GameDate)
      
      stat <- bat_bench[bat_bench$GameDate %in% max(bat_bench$GameDate),]
      
      if(nrow(stat) == 0)
      {
        potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
        
        potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
        
        potential_pinch_hitter <- unique(potential_pinch_hitter)
        
        bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
        
        bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
        
        stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
        
        bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
        
        stat <- rbind(stat,bat_pinch[1,])
        
        stat$GameDate <- as.character(stat$GameDate)
        
        box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
        
      }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
      potential_pinch_hitter <- unique(potential_pinch_hitter)
      
      bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
      
      bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
      
      bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
      
      stat <- rbind(stat,bat_pinch[1,])
      
      stat$GameDate <- as.character(stat$GameDate)
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }
}
  

# Subbing in PH in the event of starter with only 2 PA

for(i in 1:length(count_2))
{
  POS <- box_home$POS[box_home$MLBId %in% box_home$MLBId[count_2[i]]]
  
  team_name <- box_home$Team[i]
  
  if(POS %in% "1"){
    
    box_stat_home$MLBId <- as.character(box_stat_home$MLBId)
    
    unplayed <- play_pos$MLBId[(play_pos$Team_RFB %in% final_schedule$Home[x]) & !(play_pos$MLBId %in% box_stat_home$MLBId)]
    
    unplayed <- unique(unplayed)
    
    bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% unplayed),]
    
    bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
    
    stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
    
    if(nrow(fielder_available) > 0)
    {
      bat_bench <- batting_bench[batting_bench$MLBId %in% fielder_available$MLBId,]
      
      bat_bench$GameDate <- as.character(bat_bench$GameDate)
      
      stat <- bat_bench[bat_bench$GameDate %in% max(bat_bench$GameDate),]
      
      if(nrow(stat) == 0)
      {
        potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
        
        potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
        
        potential_pinch_hitter <- unique(potential_pinch_hitter)
        
        bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
        
        bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
        
        stat <- bat_pinch[1,] 
        
        bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
        
        stat <- rbind(stat,bat_pinch[1,])
        
        stat$GameDate <- as.character(stat$GameDate)
        
        box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
        
      }
    
    box_stat_home <- rbind(box_stat_home,stat)
    
    }
  }
  
  if(POS %in% "2"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("2"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
    
    if(nrow(fielder_available) > 0)
    {
      bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      if(nrow(stat) == 0)
      {
        potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
        
        potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
        
        potential_pinch_hitter <- unique(potential_pinch_hitter)
        
        bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
        
        bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
        
        stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
        
        stat$GameDate <- as.character(stat$GameDate)
        
        box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
        
      }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
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
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }

  if(POS %in% "3"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("3"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
    
    if(nrow(fielder_available) > 0)
    {
      bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      if(nrow(stat) == 0)
      {
        potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
        
        potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
        
        potential_pinch_hitter <- unique(potential_pinch_hitter)
        
        bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
        
        bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
        
        stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
        
        stat$GameDate <- as.character(stat$GameDate)
        
        box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
        
      }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
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
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }
  
  if(POS %in% "4"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("4","5","6"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
    
    if(nrow(fielder_available) > 0)
    {
      bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      if(nrow(stat) == 0)
      {
        potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
        
        potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
        
        potential_pinch_hitter <- unique(potential_pinch_hitter)
        
        bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
        
        bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
        
        stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
        
        stat$GameDate <- as.character(stat$GameDate)
        
        box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
        
      }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
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
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }
  
  if(POS %in% "5"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("4","5","6"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
    
    if(nrow(fielder_available) > 0)
    {
      bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      if(nrow(stat) == 0)
      {
        potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
        
        potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
        
        potential_pinch_hitter <- unique(potential_pinch_hitter)
        
        bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
        
        bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
        
        stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
        
        stat$GameDate <- as.character(stat$GameDate)
        
        box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
        
      }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
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
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }
  
  if(POS %in% "6"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("4","5","6"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
      
      if(nrow(fielder_available) > 0)
      {
        bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
        
        bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
        
        stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
        
        if(nrow(stat) == 0)
        {
          potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
          
          potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
          
          potential_pinch_hitter <- unique(potential_pinch_hitter)
          
          bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
          
          bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
          
          bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
          
          stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
          
          stat$GameDate <- as.character(stat$GameDate)
          
          box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
          
        }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
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
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }

  if(POS %in% "7"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("7","8","9"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
    
    if(nrow(fielder_available) > 0)
    {
      bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      if(nrow(stat) == 0)
      {
        potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
        
        potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
        
        potential_pinch_hitter <- unique(potential_pinch_hitter)
        
        bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
        
        bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
        
        stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
        
        stat$GameDate <- as.character(stat$GameDate)
        
        box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
        
      }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
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
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }
    
  if(POS %in% "8"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("7","8","9"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
    
    if(nrow(fielder_available) > 0)
    {
      bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      if(nrow(stat) == 0)
      {
        potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
        
        potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
        
        potential_pinch_hitter <- unique(potential_pinch_hitter)
        
        bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
        
        bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
        
        stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
        
        stat$GameDate <- as.character(stat$GameDate)
        
        box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
        
      }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
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
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }
  
  if(POS %in% "9"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("7","8","9"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
    
    if(nrow(fielder_available) > 0)
    {
      bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      if(nrow(stat) == 0)
      {
        potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
        
        potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
        
        potential_pinch_hitter <- unique(potential_pinch_hitter)
        
        bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
        
        bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
        
        stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
        
        stat$GameDate <- as.character(stat$GameDate)
        
        box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
        
      }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
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
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }
  
  if(POS %in% "DH"){
    fielder_available <- fielding_available[which((fielding_available$Team %in% team_name) & (fielding_available$Pos %in% c("2","3","4","5","6","7","8","9"))),]
    
    fielder_available <- fielder_available[!(fielder_available$MLBId %in% box_home$MLBId),]
    
    fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
    
    fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
    
    fielder_available$GameDate <- as.Date(fielder_available$GameDate)
    
    if(nrow(fielder_available) > 0)
    {
      bat_pinch <- batting_pinch[batting_pinch$MLBId %in% fielder_available$MLBId,]
      
      bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
      
      stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
      
      if(nrow(stat) == 0)
      {
        potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
        
        potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
        
        potential_pinch_hitter <- unique(potential_pinch_hitter)
        
        bat_pinch <- batting_pinch[(batting_pinch$MLBId %in% potential_pinch_hitter),]
        
        bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
        
        stat <- bat_pinch[bat_pinch$GameDate %in% max(bat_pinch$GameDate),]
        
        stat$GameDate <- as.character(stat$GameDate)
        
        box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
        
      }
      
      box_stat_home <- rbind(box_stat_home,stat)
    }
    
    if(nrow(fielder_available) == 0)
    {
      potential_pinch_hitter <- play_pos$MLBId[play_pos$Team_RFB %in% team_name]
      
      potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
      
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
      
      box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
      
      box_stat_home <- rbind(box_stat_home, stat)
      
    }
    
  }
}
# Sum LW, Bonus, Bases Taken, Outs On base, Field, E, Zone, Block, Frame columns.

box_stat_home$Bonus <- as.numeric(as.character(box_stat_home$Bonus))

box_stat_home$Bonus[1] <- as.numeric(as.character((as.numeric(as.character(box_stat_home$R[1])) * 0.25) + (as.numeric(as.character(box_stat_home$RBI[1])) * 0.15)))
box_stat_home$Bonus[2] <- as.numeric(as.character((as.numeric(as.character(box_stat_home$R[2])) * 0.22) + (as.numeric(as.character(box_stat_home$RBI[2])) * 0.17)))
box_stat_home$Bonus[3] <- as.numeric(as.character((as.numeric(as.character(box_stat_home$R[3])) * 0.20) + (as.numeric(as.character(box_stat_home$RBI[3])) * 0.25)))
box_stat_home$Bonus[4] <- as.numeric(as.character((as.numeric(as.character(box_stat_home$R[4])) * 0.17) + (as.numeric(as.character(box_stat_home$RBI[4])) * 0.22)))
box_stat_home$Bonus[5] <- as.numeric(as.character((as.numeric(as.character(box_stat_home$R[5])) * 0.12) + (as.numeric(as.character(box_stat_home$RBI[5])) * 0.20)))
box_stat_home$Bonus[6] <- as.numeric(as.character((as.numeric(as.character(box_stat_home$R[6])) * 0.12) + (as.numeric(as.character(box_stat_home$RBI[6])) * 0.20)))
box_stat_home$Bonus[7] <- as.numeric(as.character((as.numeric(as.character(box_stat_home$R[7])) * 0.07) + (as.numeric(as.character(box_stat_home$RBI[7])) * 0.10)))
box_stat_home$Bonus[8] <- as.numeric(as.character((as.numeric(as.character(box_stat_home$R[8])) * 0.05) + (as.numeric(as.character(box_stat_home$RBI[8])) * 0.07)))
box_stat_home$Bonus[9] <- as.numeric(as.character((as.numeric(as.character(box_stat_home$R[9])) * 0.15) + (as.numeric(as.character(box_stat_home$RBI[9])) * 0.05)))

box_stat_home <- box_stat_home[!(box_stat_home$GameDate == ""),]

for(i in 1:nrow(box_stat_home))
{
  base_available$GameDate <- as.Date(base_available$GameDate)
  box_stat_home$GameDate <- as.Date(box_stat_home$GameDate)
    
    bs_stat <- base_available[(base_available$GameDate %in% box_stat_home$GameDate[i]) & (base_available$FirstName %in% box_stat_home$FirstName[i]) & (base_available$LastName %in% box_stat_home$LastName[i]),]
    bs_stat <- unique(bs_stat)
    
    box_stat_home$Bases_Taken <- as.numeric(as.character(box_stat_home$Bases_Taken))
    box_stat_home$Outs_on_Base <- as.numeric(as.character(box_stat_home$Outs_on_Base))
    
    if(nrow(bs_stat) == 0)
    {
      box_stat_home$Bases_Taken[i] <- 0
      box_stat_home$Outs_on_Base[i] <- 0
    }
    
    if(nrow(bs_stat) > 0)
    {
    box_stat_home$Bases_Taken[i] <- as.numeric(as.character(bs_stat$BT[which(bs_stat$MLBId %in% box_stat_home$MLBId[i])]))
    box_stat_home$Outs_on_Base[i] <- as.numeric(as.character(bs_stat$BO[which(bs_stat$MLBId %in% box_stat_home$MLBId[i])]))
    }

  
}

for(i in 1:nrow(box_stat_home))
{
  fielding_available$GameDate <- as.Date(fielding_available$GameDate)

  field_stat <- fielding_available[(fielding_available$GameDate %in% box_stat_home$GameDate[i]) & (fielding_available$FirstName %in% box_stat_home$FirstName[i]) & (fielding_available$LastName %in% box_stat_home$LastName[i]),]
  field_stat <- unique(field_stat)
  
  POS <-box_home$POS[(box_home$MLBId %in% box_stat_home$MLBId[i])]
  
  if(nrow(field_stat) > 0){
    
    box_stat_home$Field <- as.numeric(as.character(box_stat_home$Field))
    box_stat_home$E <- as.numeric(as.character(box_stat_home$E))
    
    box_stat_home$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
    box_stat_home$E[i] <- as.numeric(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
  }
  
  if((nrow(field_stat) == 0) || (POS %in% "DH") || (length(POS) == 0))
  {
    box_stat_home$Field <- as.numeric(as.character(box_stat_home$Field))
    box_stat_home$E <- as.numeric(as.character(box_stat_home$E))
    
    box_stat_home$Field[i] <- ""
    box_stat_home$E[i] <- ""
  }
}

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
  
  write.csv(YTD2, "YTD_Fielding/YTD_Fielding2.csv", row.names = FALSE)
  
}

YTD()

YTD <- read.csv("YTD_Fielding/YTD_Fielding2.csv")


for(i in 1:nrow(box_stat_home))
{
  YTD$MLBId <- as.character(YTD$MLBId)
  
  ytd_stat <- YTD[(YTD$MLBId %in% box_stat_home$MLBId[i]),]
  ytd_stat <- unique(ytd_stat)
  
  ytd_stat$Pos <- as.character(ytd_stat$Pos)
  
  POS <- box_home$POS[(box_home$FirstName %in% box_stat_home$FirstName[i]) & (box_home$FirstName %in% box_stat_home$FirstName[i]) & (box_home$MLBId %in% box_stat_home$MLBId[i])]
  
  ytd_stat <- ytd_stat[ytd_stat$Pos %in% POS,]
  
  ytd_stat$Zone[1]
  
  box_stat_home$Zone <- as.numeric(as.character(box_stat_home$Zone))
  box_stat_home$Block <- as.numeric(as.character(box_stat_home$Block))
  box_stat_home$Frame <- as.numeric(as.character(box_stat_home$Block))
  
  
  box_stat_home$Zone[i] <- as.numeric(as.character(ytd_stat$Zone[1]))
  box_stat_home$Block[i] <- as.numeric(as.character(ytd_stat$Block[1]))
  box_stat_home$Frame[i] <- as.numeric(as.character(ytd_stat$Frame[1]))
  
}

box_stat_home$Block[which(box_stat_home$Block %in% NA)] <- ""
box_stat_home$Frame[which(box_stat_home$Frame %in% NA)] <- ""
box_stat_home$Zone[which(box_stat_home$Zone %in% NA)] <- ""
box_stat_home$Field[which(box_stat_home$Field %in% NA)] <- ""
box_stat_home$E[which(box_stat_home$E %in% NA)] <- ""


blank_home <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_home)))

colnames(blank_home) <- colnames(box_stat_home)

box_stat_home <- rbind(box_stat_home, blank_home)

# Everything beyond this must be pushed behind, to the part when pitching is all taken care of

blank_home <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_home)))

colnames(blank_home) <- colnames(box_stat_home)

blank_home$LastName <- "Total"


blank_home$Bonus <- sum(as.numeric(as.character(box_stat_home$Bonus)),na.rm = TRUE)
blank_home$Bases_Taken <- sum(as.numeric(as.character(box_stat_home$Bases_Taken)),na.rm = TRUE)
blank_home$Outs_on_Base <- sum(as.numeric(as.character(box_stat_home$Outs_on_Base)),na.rm = TRUE)
blank_home$Field <- sum(as.numeric(as.character(box_stat_home$Field)),na.rm = TRUE)
blank_home$E <- sum(as.numeric(as.character(box_stat_home$E)),na.rm = TRUE)
blank_home$Zone <- sum(as.numeric(as.character(box_stat_home$Zone)),na.rm = TRUE)
blank_home$Block <- sum(as.numeric(as.character(box_stat_home$Block)),na.rm = TRUE)
blank_home$Frame <- sum(as.numeric(as.character(box_stat_home$Frame)),na.rm = TRUE)

box_stat_home$LW <- as.numeric(as.character(box_stat_home$LW))

blank_home$LW <- as.numeric(as.character(blank_home$LW))
blank_home$LW[1] <- sum(box_stat_home$LW,na.rm = TRUE)

box_stat_home$H <- as.numeric(as.character(box_stat_home$H))

blank_home$H <- as.numeric(as.character(blank_home$H))
blank_home$H[1] <- sum(box_stat_home$H,na.rm = TRUE)

# Calculate 'Overall Offense'

blank_home2 <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_home)))

colnames(blank_home2) <- colnames(box_stat_home)

blank_home2$LastName <- as.character(blank_home2$LastName)

blank_home2$LastName[1] <- "Overall Offense"

blank_home2$LW <- as.numeric(blank_home$LW[1]) + as.numeric(blank_home$Bonus[1]) + as.numeric(blank_home$Bases_Taken[1]) + as.numeric(blank_home$Outs_on_Base[1])

blank_home <- rbind(blank_home, blank_home2)

blank_home3 <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_home)))

colnames(blank_home3) <- colnames(box_stat_home)

blank_home3$LastName <- "Overall Defense"

blank_home3$LW <- as.numeric(blank_home$Field[1]) + as.numeric(blank_home$Zone[1]) + as.numeric(blank_home$Block[1]) + as.numeric(blank_home$Frame[1])

blank_home <- rbind(blank_home,blank_home3)
