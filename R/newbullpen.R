# Score

score_col <- c("Out","Side","Out_count","Inning","Visit","Home","V-Score","H-Score","Pit","Add_score","Winning_Team","Lead_By","Winning_Pit","Losing_Pit")

score <- data.frame(matrix("",nrow=54,ncol=length(score_col)))

colnames(score) <- score_col

score$Out <- as.numeric(as.character(score$Out))
score$Side <- as.character(score$Side)
score$Out_count <- as.numeric(as.character(score$Out_count))
score$Inning <- as.numeric(as.character(score$Inning))
score$Visit <- as.character(score$Visit)
score$Home <- as.character(score$Home)
score$`V-Score` <- as.numeric(as.character(score$`V-Score`))
score$`H-Score` <- as.numeric(as.character(score$`H-Score`))
score$Pit <- as.character(score$Pit)
score$Add_score <- as.numeric(as.character(score$Add_score))
score$Winning_Team <- as.character(score$Winning_Team)
score$Lead_By <- as.numeric(as.character(score$Lead_By))
score$Winning_Pit <- as.character(score$Winning_Pit)
score$Losing_Pit <- as.character(score$Losing_Pit)

no_setup_visit <- "NO"

no_setup_home <- "NO"

score$Out_count <- c(0,1,2)

score$Out <- c(1:54)

score$Side <- c("Top","Top","Top","Bottom","Bottom","Bottom")

score$Inning <- c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9)

score$Visit <- final_schedule$Away[x]
score$Home <- final_schedule$Home[x]

away_sp <- visit_starter

away_sp_stat <- pitching_SP[pitching_SP$PlayerName %in% away_sp,]

away_sp_stat <- away_sp_stat[order(away_sp_stat$GameDate, decreasing= TRUE),]

away_sp_stat <- away_sp_stat[1,]

away_sp_stat$OUT <- ((away_sp_stat$IP %/% 1) * 3) + ((away_sp_stat$IP %% 1) * 10)

if(away_sp_stat$OUT == 27)
{
  no_reliever_away <- "YES"
}

if(away_sp_stat$OUT < 27)
{
  no_reliever_away <- "NO"
}

visit_team <- which(score$Side == "Bottom")

out_sp <- ((away_sp_stat$IP %/% 1) * 3) + ((away_sp_stat$IP %% 1) * 10)

out_sp <- as.integer(as.character(out_sp))

score$Pit[visit_team[1:out_sp]] <- away_sp

visit_team <- visit_team[which(!(visit_team %in% which(score$Pit == away_sp)))]

visit_team <- visit_team[visit_team < 52]

home_sp <-  home_starter

home_sp_stat <- pitching_SP[pitching_SP$PlayerName %in% home_sp,]

home_sp_stat <- home_sp_stat[order(home_sp_stat$GameDate, decreasing= TRUE),]

home_sp_stat <- home_sp_stat[1,]

home_sp_stat$OUT <- ((home_sp_stat$IP %/% 1) * 3) + ((home_sp_stat$IP %% 1) * 10)

if(home_sp_stat$OUT == 27)
{
  no_reliever_home <- "YES"
}

if(home_sp_stat$OUT < 27)
{
  no_reliever_home <- "NO"
}

home_team <- which(score$Side == "Top")

out_sp <- ((home_sp_stat$IP %/% 1) * 3) + ((home_sp_stat$IP %% 1) * 10)

out_sp <- as.integer(as.character(out_sp))

score$Pit[home_team[1:out_sp]] <- home_sp

home_team <- home_team[which(!(home_team %in% which(score$Pit == home_sp)))]

visiting_pitcher <- only_active_players$MLBId[(only_active_players$Team_RFB %in% final_schedule$Away[x]) & (only_active_players$Pos %in% "1")]

pitching_RP_visit <- pitching_RP[pitching_RP$MLBId %in% visiting_pitcher,]

away_rp_report <- read.csv(paste("report/RP/",date,"/",final_schedule$Away[x],date,"_RP_report.csv",sep=""))

cannot_use_away <- away_rp_report$MLBId[away_rp_report$Cannot.Use == "TRUE"]

home_pitcher <- only_active_players$MLBId[(only_active_players$Team_RFB %in% final_schedule$Home[x]) & (only_active_players$Pos %in% "1")]

pitching_RP_home <- pitching_RP[pitching_RP$MLBId %in% home_pitcher,]

home_rp_report <- read.csv(paste("report/RP/",date,"/",final_schedule$Home[x],date,"_RP_report.csv",sep=""))

cannot_use_home <- home_rp_report$MLBId[home_rp_report$Cannot.Use == "TRUE"]

pitching_RP_home <- pitching_RP_home[!(pitching_RP_home$MLBId %in% cannot_use_home),]

for(i in 1:nrow(pitching_RP_home))
{
  pitching_RP_home$OUT[i] <- (pitching_RP_home$IP[i] %/% 1) * 3 + ((pitching_RP_home$IP[i] %% 1) * 10)
}


if(length(home_team) == 0)
{
  print("Pitcher went 8 innings")
  no_setup_home <- "YES"
}

if(length(visit_team) == 0)
{
  print("Pitcher went at least 8th inning")
  no_setup_visit <- "YES"
  
}

###

if(!no_setup_home == "YES")
{
  #home_team

  success <- "NO"
  
  pitching_RP_home <- pitching_RP_home[!pitching_RP_home$IP == 0,]
  
  for(f in 1:nrow(pitching_RP_home))
  {

    if(success == "YES")
    {
      break;
    }
    counter <- 0
    out_counting <- 0
    out_counting <- pitching_RP_home$OUT[f]
    counter <- counter + 1
    which_row <- vector()
    which_row[counter] <- f
    which_id <- vector()
    which_id[counter] <- pitching_RP_home$MLBId[f]
    
    #nrow(pitching_RP_home)
    
    for(ff in f:nrow(pitching_RP_home))
    {
      if(f == ff)
      {
        next;
      }
      
      if((f != ff) & (!pitching_RP_home$MLBId[ff] %in% which_id))
      {
        out_counting <- out_counting + pitching_RP_home$OUT[ff]
        
        if(out_counting > length(home_team))
        {
          out_counting <- out_counting - pitching_RP_home$OUT[ff]
          next;
        }
        
        counter <- counter + 1
        which_row[counter] <- ff
        which_id[counter] <- pitching_RP_home$MLBId[ff]
        
        if(out_counting == length(home_team))
        {
          success <- "YES"
          break;
        }
        
        if(out_counting < length(home_team))
        {
          next;
        }
        
       
      }
      
      if((f != ff) & (!pitching_RP_home$MLBId[ff] %in% which_id) & !(ff=nrow(pitching_RP_home)))
      {
        next;
      }
      
      
      
      if(ff == nrow(pitching_RP_home))
      {
        pitching_RP_home2 <- pitching_RP_home[1:f,]
        
        for(t in 1:nrow(pitching_RP_home2))
        {
          if(!pitching_RP_home2$MLBId[t] %in% which_id)
          {
            counter <- counter + 1
            which_id[counter] <- pitching_RP_home2$MLBId[t]
            which_row[counter] <- which(pitching_RP_home$uniqueId %in% pitching_RP_home2$uniqueId[t])
          }
        }
      }
    }
  }
  
  pitching_RP_home3 <- pitching_RP_home[which_row,]
  
  bullpen_outs_needed <- 27 - home_sp_stat$OUT
  
  bullpen_outs_needed <- as.integer(bullpen_outs_needed)
  
  total_outs_generated <- sum(pitching_RP_home3$OUT)
  
  total_outs_generated <- as.integer(total_outs_generated)
  
  if((total_outs_generated - bullpen_outs_needed) == 0)
  {
    pitching_RP_home <- pitching_RP_home3
  }
  
  if((total_outs_generated - bullpen_outs_needed) > 0)
  {
    difference <- (total_outs_generated - bullpen_outs_needed)
    
    done <- "NO"
    
    for(w in 1:nrow(pitching_RP_home3))
    {
      if(done == "YES")
      {
        break;
      }
      
      out_count <- 0
      
      out_count <- as.integer(out_count)
      
      counting <- 0
      
      what_row <- vector()
      
      counting <- counting + 1
      
      out_count <- out_count + as.integer(pitching_RP_home3$OUT[w])
      
      what_row[counting] <- w
      
      for(r in 1:nrow(pitching_RP_home3))
      {
        
      if(out_count < difference)
      {
        counting <- counting + 1
        out_count <- out_count + as.integer(pitching_RP_home3$OUT[r])

        if(out_count < difference)
        {
          what_row[counting] <- r
          
          if(out_count != difference)
          {
            next;
          }
        }
      
      }
      
      if(out_count > difference)
      {
        counting <- counting - 1
        
        out_count <- out_count - as.integer(pitching_RP_home3$OUT[w])
      
      }
      
      if(out_count == difference)
      {
        pitching_RP_home3 <- pitching_RP_home3[!c(1:nrow(pitching_RP_home3)) %in% what_row,]
        
        done <- "YES"
        
        break;
      }
      
      }
    }
    

    which_id <- which_id[which_id %in% pitching_RP_home3$MLBId]
    
    which_row <- which(pitching_RP_home$uniqueId %in% pitching_RP_home3$uniqueId)
    

    if(sum(pitching_RP_home3$OUT) == bullpen_outs_needed)
    {
      pitching_RP_home <- pitching_RP_home3
      
    }
    
    if(sum(pitching_RP_home3$OUT) != bullpen_outs_needed)
    {
      inning_needed <- bullpen_outs_needed - sum(pitching_RP_home3$OUT)
      
      add <- pitching_RP_home[which((pitching_RP_home$OUT == inning_needed) & !(pitching_RP_home$MLBId %in% which_id)),]
      
      pitching_RP_home <- rbind(pitching_RP_home3,add[1,])
    }
  }
  
  pitching_RP_home <- rbind(home_sp_stat,pitching_RP_home)
  
  inning <- 9
  
  Away_Run <- ifelse((blank_visit$LW[2] * inning/9) - (blank_home$LW[3] * inning/9) - (sum(pitching_RP_home$LW,na.rm=TRUE)) + (4.25 * inning/9) < 0, 0, ER <- round((blank_visit$LW[2] * inning/9) - (blank_home$LW[3] * inning/9) - (sum(pitching_RP_home$LW,na.rm=TRUE)) + (4.25 * inning/9),digits=0))
  
  
}

pitching_RP_visit <- pitching_RP_visit[!(pitching_RP_visit$MLBId %in% cannot_use_away),]

for(i in 1:nrow(pitching_RP_visit))
{
  pitching_RP_visit$OUT[i] <- (pitching_RP_visit$IP[i] %/% 1) * 3 + ((pitching_RP_visit$IP[i] %% 1) * 10)
}


if(!no_setup_visit == "YES")
{
  
  #home_team
  
  success <- "NO"
  
  pitching_RP_visit <- pitching_RP_visit[!pitching_RP_visit$IP == 0,]
  
  for(f in 1:nrow(pitching_RP_visit))
  {
    
    if(success == "YES")
    {
      break;
    }
    counter <- 0
    out_counting <- 0
    out_counting <- pitching_RP_visit$OUT[f]
    counter <- counter + 1
    which_row <- vector()
    which_row[counter] <- f
    which_id <- vector()
    which_id[counter] <- pitching_RP_visit$MLBId[f]
    
    for(ff in f:nrow(pitching_RP_visit))
    {
      if(f == ff)
      {
        next;
      }
      
      if((f != ff) & (!pitching_RP_visit$MLBId[ff] %in% which_id))
      {
        out_counting <- out_counting + pitching_RP_visit$OUT[ff]
        
        if(out_counting > length(visit_team))
        {
          out_counting <- out_counting - pitching_RP_visit$OUT[ff]
          next;
        }
        
        counter <- counter + 1
        which_row[counter] <- ff
        which_id[counter] <- pitching_RP_visit$MLBId[ff]
        
        if(out_counting == length(visit_team))
        {
          success <- "YES"
          break;
        }
        
        if(out_counting < length(visit_team))
        {
          next;
        }
        
        
      }
      
      if((f != ff) & (!pitching_RP_visit$MLBId[ff] %in% which_id))
      {
        next;
      }
    }
  }
  
  pitching_RP_visit <- pitching_RP_visit[which_row,]
  
  pitching_RP_visit <- rbind(away_sp_stat,pitching_RP_visit)
  
  inning <- 8
  
  Home_Run <- ifelse((blank_home$LW[2] * inning/9) - (blank_visit$LW[3] * inning/9) - (sum(pitching_RP_visit$LW,na.rm=TRUE)) + (4.25 * inning/9) < 0, 0, ER <- round((blank_home$LW[2] * inning/9) - (blank_visit$LW[3] * inning/9) - (sum(pitching_RP_visit$LW,na.rm=TRUE)) + (4.25 * inning/9),digits=0))
  
  if(Home_Run < Away_Run)
  {
    
    visiting_pitcher <- only_active_players$MLBId[(only_active_players$Team_RFB %in% final_schedule$Away[x]) & (only_active_players$Pos %in% "1")]
    
    pitching_RP_visit <- pitching_RP[pitching_RP$MLBId %in% visiting_pitcher,]
    
    away_rp_report <- read.csv(paste("report/RP/",date,"/",final_schedule$Away[x],date,"_RP_report.csv",sep=""))
    
    cannot_use_away <- away_rp_report$MLBId[away_rp_report$Cannot.Use == "TRUE"]
    
    success <- "NO"
    
    pitching_RP_visit <- pitching_RP_visit[!pitching_RP_visit$IP == 0,]
    
    visit_team <- which(score$Side == "Bottom")
    
    out_sp <- ((away_sp_stat$IP %/% 1) * 3) + ((away_sp_stat$IP %% 1) * 10)
    
    out_sp <- as.integer(as.character(out_sp))
    
    visit_team <- visit_team[which(!(visit_team %in% which(score$Pit == away_sp)))]
    
    pitching_RP_visit <- pitching_RP_visit[!(pitching_RP_visit$MLBId %in% cannot_use_away),]
    
    for(i in 1:nrow(pitching_RP_visit))
    {
      pitching_RP_visit$OUT[i] <- (pitching_RP_visit$IP[i] %/% 1) * 3 + ((pitching_RP_visit$IP[i] %% 1) * 10)
    }
    
    
    for(f in 1:nrow(pitching_RP_visit))
    {
      
      if(success == "YES")
      {
        break;
      }
      
      counter <- 0
      out_counting <- 0
      out_counting <- pitching_RP_visit$OUT[f]
      counter <- counter + 1
      which_row <- vector()
      which_row[counter] <- f
      which_id <- vector()
      which_id[counter] <- pitching_RP_visit$MLBId[f]
      
      for(ff in f:nrow(pitching_RP_visit))
      {
        if(f == ff)
        {
          next;
        }
        
        if((f != ff) & (!pitching_RP_visit$MLBId[ff] %in% which_id))
        {
          out_counting <- out_counting + pitching_RP_visit$OUT[ff]
          
          if(out_counting > length(visit_team))
          {
            out_counting <- out_counting - pitching_RP_visit$OUT[ff]
            next;
          }
          
          counter <- counter + 1
          which_row[counter] <- ff
          which_id[counter] <- pitching_RP_visit$MLBId[ff]
          
          if(out_counting == length(visit_team))
          {
            success <- "YES"
            break;
          }
          
          if(out_counting < length(visit_team))
          {
            next;
          }
          
          
        }
        
        if((f != ff) & (!pitching_RP_visit$MLBId[ff] %in% which_id))
        {
          next;
        }
      }
    }
    
    pitching_RP_visit <- pitching_RP_visit[which_row,]
    
    pitching_RP_visit <- rbind(away_sp_stat,pitching_RP_visit)
    
    inning <- 9
    
    Home_Run <- ifelse((blank_home$LW[2] * inning/9) - (blank_visit$LW[3] * inning/9) - (sum(pitching_RP_visit$LW,na.rm=TRUE)) + (4.25 * inning/9) < 0, 0, ER <- round((blank_home$LW[2] * inning/9) - (blank_visit$LW[3] * inning/9) - (sum(pitching_RP_visit$LW,na.rm=TRUE)) + (4.25 * inning/9),digits=0))
    
  }
  
}

###



if(length(home_team) > 0)
{
  
  continue <- TRUE
  
  attempt <- 0
  
  
  while(continue == TRUE)
  {
    attempt <- attempt + 1
    print(attempt)
    
    sample_home <- pitching_RP_home[sample((1:nrow(pitching_RP_home)), size = sample(c(1:nrow(pitching_RP_home)), size = 1, replace = FALSE), replace = FALSE),]
    
    sample_home <- sample_home[!(sample_home$MLBId %in% lineup$MLBId[(lineup$Role == "CLOSER") & (lineup$Team %in% final_schedule$Home[x])]),]
    
    number <- sum(sample_home$OUT)
    
    number <- as.integer(number)
    
    if((number == length(home_team)) & (length(unique(sample_home$PlayerName)) == nrow(sample_home)) & ((home_sp %in% sample_home$PlayerName) == FALSE))
    {
      continue <- FALSE
    }
    
    if(attempt == 5000)
    {
      
      attempt2 <- 0
      continue2 <- TRUE
      while(continue2 == TRUE)
      {
        attempt2 <- attempt2 + 1
        print(attempt2)
        
        sample_home <- pitching_RP_home[sample((1:nrow(pitching_RP_home)), size = sample(c(1:nrow(pitching_RP_home)), size = 1, replace = FALSE), replace = FALSE),]
        
        sample_home <- sample_home[!(sample_home$MLBId %in% lineup$MLBId[(lineup$Role == "CLOSER") & (lineup$Team %in% final_schedule$Home[x])]),]
        
        number <- sum(sample_home$OUT)
        
        if(((number - length(home_team)) == c(1)) & (home_sp %in% sample_home$PlayerName == FALSE) & (length(unique(sample_home$PlayerName)) == nrow(sample_home)))
        {
          sample_home <- sample_home
          continue <- FALSE
          continue2 <- FALSE
        }
        
      }
    }
  }
  
  sample_home <- sample_home[!(sample_home$IP == 0),]
  
  
  for(i in 1:nrow(sample_home))
  {
    inning <- ((sample_home$IP[i] %/% 1 + (((sample_home$IP[i] %% 1) * (10/3)))) / 9)
    
    ER <- ifelse((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (sample_home$LW[i]) + (4.25 * inning) < 0, 0, ER <- round((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (sample_home$LW[i]) + (4.25 * inning),digits=0))
    
    sample_home$ER[i] <- ER
  }
  
  name <- as.character(sample_home$PlayerName)
  
  for(i in 1:length(name))
  {
    num <- sample_home$OUT[sample_home$PlayerName %in% name[i]]
    score$Pit[home_team[1:num]] <- name[i]
    home_team <- home_team[(num+1):length(home_team)]
  }
  
}

if(length(visit_team) > 0)
{
  
  continue <- TRUE
  
  attempt <- 0
  
  while(continue == TRUE)
  {
    attempt <- attempt + 1
    
    sample_visit <- pitching_RP_visit[sample((1:nrow(pitching_RP_visit)), size = sample(nrow(pitching_RP_visit), size = 1, replace = FALSE), replace = FALSE),]
    
    sample_visit <- sample_visit[!(sample_visit$MLBId %in% lineup$MLBId[(lineup$Role == "CLOSER") & (lineup$Team %in% final_schedule$Away[x])]),]
    
    number <- sum(sample_visit$OUT)
    
    if((number == length(visit_team)) & (length(unique(sample_visit$PlayerName)) == nrow(sample_visit)) & ((away_sp %in% sample_visit$PlayerName) == FALSE))
    {
      continue <- FALSE
    }
    
    if(attempt == 5000)
    {
      
      attempt2 <- 0
      continue2 <- TRUE
      while(continue2 == TRUE)
      {
        attempt2 <- attempt2 + 1
        print(attempt2)
        
        sample_visit <- pitching_RP_visit[sample((1:nrow(pitching_RP_visit)), size = sample(c(1:nrow(pitching_RP_visit)), size = 1, replace = FALSE), replace = FALSE),]
        
        sample_visit <- sample_visit[!(sample_visit$MLBId %in% lineup$MLBId[(lineup$Role == "CLOSER") & (lineup$Team %in% final_schedule$Away[x])]),]
        
        number <- sum(sample_visit$OUT)
        
        if(((number - length(visit_team)) == c(1)) & (away_sp %in% sample_visit$PlayerName == FALSE) & (length(unique(sample_visit$PlayerName)) == nrow(sample_visit)))
        {
          sample_visit <- sample_visit
          continue <- FALSE
          continue2 <- FALSE
        }
        
      }
    }
  }
  sample_visit <- sample_visit[!(sample_visit$IP == 0),]
  
  
  for(i in 1:nrow(sample_visit))
  {
    inning <- ((sample_visit$IP[i] %/% 1 + (((sample_visit$IP[i] %% 1) * (10/3)))) / 9)
    
    ER <- ifelse((blank_home$LW[2] * inning) - (blank_visit$LW[3] * inning) - (sample_visit$LW[i]) + (4.25 * inning) < 0, 0, ER <- round((blank_home$LW[2] * inning) - (blank_visit$LW[3] * inning) - (sample_visit$LW[i]) + (4.25 * inning),digits=0))
    
    sample_visit$ER[i] <- ER
  }
  
  name <- as.character(sample_visit$PlayerName)
  
  for(i in 1:length(name))
  {
    num <- sample_visit$OUT[sample_visit$PlayerName %in% name[i]]
    score$Pit[visit_team[1:num]] <- name[i]
    visit_team <- visit_team[(num+1):length(visit_team)]
  }
  
}



# Calculate overall offense, overall defense, and overall pitching of visiting and home team. Then, calculate the score of each time by the end of 8th.

visit_overall_offense_8th <- blank_visit$LW[which(blank_visit$LastName %in% "Overall Offense")] * (8/9)

visit_overall_defense_8th <- blank_visit$LW[which(blank_visit$LastName %in% "Overall Defense")] * (8/9)

if(length(visit_team) == 0)
{
  pitching_line_visit <- away_sp_stat
}

if(length(visit_team) > 0)
{
  pitching_line_visit <- rbind(away_sp_stat, sample_visit)
  
}


visit_overall_pitch_8th <- sum(as.numeric(pitching_line_visit$LW), na.rm= TRUE)



home_overall_offense_8th <- blank_home$LW[which(blank_home$LastName %in% "Overall Offense")] * (8/9)

home_overall_defense_8th <- blank_home$LW[which(blank_home$LastName %in% "Overall Defense")] * (8/9)

if(length(home_team) == 0)
{
  pitching_line_home <- home_sp_stat
}

if(length(home_team) > 0)
{
  pitching_line_home <- rbind(home_sp_stat, sample_home)
}

home_overall_pitch_8th <- sum(as.numeric(pitching_line_home$LW))


visiting_score_end8th <- ifelse(round((visit_overall_offense_8th * (8/9)) - (home_overall_defense_8th * (8/9)) - (home_overall_pitch_8th * (8/9)) + (4.25 * (8/9)), digits = 0) < 0, 0, visiting_score_end8th <- round((visit_overall_offense_8th * (8/9)) - (home_overall_defense_8th * (8/9)) - (home_overall_pitch_8th * (8/9)) + (4.25 * (8/9)), digits = 0))

home_score_end8th <- ifelse(round((home_overall_offense_8th * (8/9)) - (visit_overall_defense_8th * (8/9)) - (visit_overall_pitch_8th * (8/9)) + (4.25 * (8/9)), digits = 0) < 0, 0, home_score_end8th <- round((home_overall_offense_8th * (8/9)) - (visit_overall_defense_8th * (8/9)) - (visit_overall_pitch_8th * (8/9)) + (4.25 * (8/9)), digits = 0))



for(i in 1:nrow(pitching_line_home))
{
  inning <- ((pitching_line_home$IP[i] %/% 1 + (((pitching_line_home$IP[i] %% 1) * (10/3)))) / 9)
  
  ER <- ifelse(((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (pitching_line_home$LW[i]) + (4.25 * inning)) < 0, 0, ER <- round(((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (pitching_line_home$LW[i]) + (4.25 * inning)),digits=0))
  
  pitching_line_home$ER[i] <- ER
}

for(i in 1:nrow(pitching_line_visit))
{
  inning <- ((pitching_line_visit$IP[i] %/% 1 + (((pitching_line_visit$IP[i] %% 1) * (10/3)))) / 9)
  
  ER <- ifelse((blank_home$LW[2] * inning) - (blank_visit$LW[3] * inning) - (pitching_line_visit$LW[i]) + (4.25 * inning) < 0, 0, ER <- round((blank_home$LW[2] * inning) - (blank_visit$LW[3] * inning) - (pitching_line_visit$LW[i]) + (4.25 * inning),digits=0))
  
  pitching_line_visit$ER[i] <- ER
}

if((no_reliever_home == "YES") & (no_setup_home == "YES")){
  
  print("Skip the IP alternation process as home starter had complete game")
  
}

if((no_reliever_home == "NO") & (no_setup_home == "NO"))
{
  pitching_line_home$OUT[1] <- 24 - sum(sample_home$OUT, na.rm = TRUE)
  
  pitching_line_home$IP[1] <- (pitching_line_home$OUT[1] %/% 3) + ((pitching_line_home$OUT[1] %% 3) / 10)
  
}

if((no_reliever_away == "YES") & (no_setup_visit == "YES")){
  print("Skip the IP alternation process as away starter had complete game")
  
}

if((no_reliever_away == "NO") & (no_setup_visit == "NO"))
{
  pitching_line_visit$OUT[1] <- 24 - sum(sample_visit$OUT, na.rm = TRUE)
  
  pitching_line_visit$IP[1] <- (pitching_line_visit$OUT[1] %/% 3) + ((pitching_line_visit$OUT[1] %% 3) / 10)
}



for(i in 1:nrow(pitching_line_visit))
{
  runs <- pitching_line_visit$ER[i]
  ifelse(runs > 0, runs <- runs, runs <- 0)
  slot <- which(score$Pit %in% pitching_line_visit$PlayerName[i])
  slot <- as.numeric(slot)
  
  
  if(runs > 0)
  {
    if(length(slot) == 1)
    {
      run_slot <- slot
      score$Add_score[run_slot] <- runs
    }
    
    if(length(slot) > 1)
    {
      run_slot <- sample(slot, size = 1, replace = FALSE)
      score$Add_score[run_slot] <- runs
    }
  }
  
  if(runs == 0)
  {
    print("run is zero")
    score$Add_score[slot] <- 0
    
  }
}

for(i in 1:nrow(pitching_line_home))
{
  runs <- pitching_line_home$ER[i]
  ifelse(runs > 0, runs <- runs, runs <- 0)
  slot <- which(score$Pit %in% pitching_line_home$PlayerName[i])
  slot <- as.numeric(slot)
  
  if(runs > 0)
  {
    if(length(slot) == 1)
    {
      run_slot <- slot
      score$Add_score[run_slot] <- runs
    }
    
    if(length(slot) > 1)
    {
      run_slot <- sample(slot, size = 1, replace = FALSE)
      score$Add_score[run_slot] <- runs
    }
  }
  
  if(runs == 0)
  {
    print("Run is zero")
    score$Add_score[slot] <- 0
  }
}

score$`V-Score` <- ""

score$`H-Score` <- ""

score$`V-Score`[1] <- 0

score$`H-Score`[1] <- 0

score$Add_score[which(score$Add_score %in% NA)] <- 0

score$Add_score[which(score$Pit %in% c("",NA))] <- ""

streak <- which(!(score$Pit %in% ""))

for(i in 2:length(streak))
{
  if(score$Side[i] == "Top")
  {
    score$`V-Score`[i] <- as.numeric(score$`V-Score`[i-1]) + as.numeric(score$Add_score[i])
    score$`H-Score`[i] <- as.numeric(score$`H-Score`[i-1])
  }
  
  if(score$Side[i] == "Bottom")
  {
    score$`H-Score`[i] <- as.numeric(score$`H-Score`[i-1]) + as.numeric(score$Add_score[i])
    score$`V-Score`[i] <- as.numeric(score$`V-Score`[i-1])
  }
}

score$`V-Score` <- as.numeric(as.character(score$`V-Score`))

score$`H-Score` <- as.numeric(as.character(score$`H-Score`))

score$Add_score <- as.numeric(as.character(score$Add_score))

## TOP 9TH - HOME TEAM PITCHING
# Run this on one of these conditions: 1) Home team is winning by more than 3. 2) Visiting team is winning.

if(!((score$`H-Score`[which((score$Side == "Bottom") & (score$Inning %in% 8) & (score$Out_count %in% 2))] - score$`V-Score`[which((score$Side == "Bottom") & (score$Inning %in% 8) & (score$Out_count %in% 2))]) %in% c(0,1,2,3)))
{
  if(no_reliever_home == "YES"){
    print("No reliever required")
  }
  
  if(!(no_reliever_home == "YES")){
    pitching_line_home$MLBId <- as.character(pitching_line_home$MLBId)
    
    reliever_9th_home <- lineup[!(lineup$Team %in% final_schedule$Home[x]),]
    reliever_9th_home <- reliever_9th_home[reliever_9th_home$POS %in% "RP",]
    reliever_9th_home <- reliever_9th_home$MLBId[!(reliever_9th_home$MLBId %in% pitching_line_home$MLBId)]
    
    continue <- TRUE
    
    counter <- 0
    
    while(continue == TRUE)
    {
      
      counter <- counter + 1
      
      print(counter)
      
      sample_home <- pitching_RP_home[sample((1:nrow(pitching_RP_home)), size = 1, replace = FALSE),]
      
      sample_home <- sample_home[!(sample_home$MLBId %in% pitching_line_home$MLBId),]
      
      number <- sum(sample_home$OUT)
      
      if((number == 3))
      {
        continue <- FALSE
      }
      
      if(counter == 500)
      {
        continue2 <- TRUE
        while(continue2 == TRUE)
        {
          
          sample_home <- pitching_RP_home[sample((1:nrow(pitching_RP_home)), size = 1, replace = FALSE),]
          
          sample_home <- sample_home[!(sample_home$MLBId %in% pitching_line_home$MLBId),]
          
          number <- sum(sample_home$OUT)
          
          if(number %in% c(1,2,4,5))
          {
            sample_home$IP[1] <- 1
            
            sample_home$OUT[1] <- 3
            
            sample_home$LW <- round((sample_home$X1B *-0.46) + (sample_home$X2B * -0.8) + (sample_home$X3B * -1.02) + (sample_home$HR * -1.4) + (sample_home$HBP * -0.33) + (sample_home$BB * -0.33) + 
                                      (sample_home$K * 0.1) + (sample_home$WP * -0.395) + (sample_home$BLK * -0.15) + (sample_home$CS * 0.3) + (sample_home$PKO * 0.145) + (sample_home$OUT * 0.25) + (sample_home$SB * -0.15) + (sample_home$SH * -0.146) + (sample_home$SF * -0.2),digits = 3)
            
            
            continue <- FALSE
            continue2 <- FALSE
          }
          
          
        }
      }
    }
    
    sample_home <- sample_home[!(sample_home$IP == 0),]
    
    for(i in 1:nrow(sample_home))
    {
      inning <- ((sample_home$IP[i] %/% 1 + (((sample_home$IP[i] %% 1) * (10/3)))) / 9)
      
      ER <- ifelse((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (sample_home$LW[i]) + (4.25 * inning) < 0, 0, ER <- round((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (sample_home$LW[i]) + (4.25 * inning),digits=0))
      
      sample_home$ER[i] <- ER
    }
    
    
    slot <- score$Out[(score$Side == "Top") & (score$Inning == 9)]
    
    for(i in 1:nrow(sample_home))
    {
      if(sample_home$IP[i] == 0)
      {
        next;
      }
      
      num_outs <- (sample_home$IP[i] %/% 1) * 3 + ((sample_home$IP[i] %% 1) * 10)
      
      score$Pit[slot[1:num_outs]] <- as.character(sample_home$PlayerName[i])
      
      slot <- slot[!(1:num_outs)]
    }
    
    for(i in 1:nrow(sample_home))
    {
      inning <- ((sample_home$IP[i] %/% 1 + (((sample_home$IP[i] %% 1) * (10/3)))) / 9)
      
      
      sample_home$ER[i] <- ifelse(round((blank_visit$LW[2] * inning) -  (blank_home$LW[3] * inning) - sample_home$LW[i] + (4.25 * inning),digits=0) < 0, 0, sample_home$ER[i] <- round((blank_visit$LW[2] * inning) -  (blank_home$LW[3] * inning) - sample_home$LW[i] + (4.25 * inning),digits=0))
      
      out_9th_home <- (sum(sample_home$IP[i], na.rm = TRUE) * 3)
      
      home_9th_outs <- score$Out[(score$Side == "Top") & (score$Inning == 9) & (score$Pit %in% sample_home$PlayerName[i])]
      
      run_allowed_home_9th <- sample(home_9th_outs, size = 1, replace = FALSE)
      
      score$Add_score[run_allowed_home_9th] <- sample_home$ER[i]
      
      run_not_allowed_home_9th <- home_9th_outs[!(home_9th_outs %in% run_allowed_home_9th)]
      
      score$Add_score[run_not_allowed_home_9th] <- 0
      
      top9th <- score$Out[(score$Side == "Top") & (score$Inning == 9)]
      
      score$`V-Score` <- as.numeric(score$`V-Score`)
      score$`H-Score` <- as.numeric(score$`H-Score`)
      score$Add_score <- as.numeric(score$Add_score)
    }
    
    
    pitching_line_home <- rbind(pitching_line_home, sample_home)
    
    for(i in 1:length(top9th))
    {
      score$`V-Score`[top9th[i]] <- score$`V-Score`[top9th[i]-1] + score$Add_score[top9th[i]]
      score$`H-Score`[top9th[i]] <- score$`H-Score`[top9th[i]-1]
    }
    
  }
}

# TOP 9TH HOME TEAM PITCHING
# Run this on these conditions: 1) Game is tied. 2) Home team is winning by 1, 2, or 3 runs.

if((score$`H-Score`[which((score$Side == "Bottom") & (score$Inning %in% 8) & (score$Out_count %in% 2))] - score$`V-Score`[which((score$Side == "Bottom") & (score$Inning %in% 8) & (score$Out_count %in% 2))]) %in% c(0,1,2,3))
{
  if(no_reliever_home == "YES"){
    print("No reliever needed")}
  
  
  if(!(no_reliever_home == "YES")){
    home_closer <- lineup$MLBId[lineup$Role == "CLOSER" & lineup$Team == final_schedule$Home[x]]
    
    closer_home <- pitching_RP[pitching_RP$MLBId %in% as.character(home_closer),]
    
    if(nrow(closer_home) == 0)
    {
      pitching_RP_home <- pitching_RP_home[!(pitching_RP_home$PlayerName %in% sample_home$PlayerName),]
      
      pitching_RP_home$OUT <- ((pitching_RP_home$IP %/% 1) * 3) + ((pitching_RP_home$IP %% 1) * 10)
      
      continue <- TRUE
      
      while(continue == TRUE)
      {
        sampling <- sample(1:nrow(pitching_RP_home), size = c(1:nrow(pitching_RP_home)), replace = FALSE)
        
        out <- sum(pitching_RP_home$OUT[sampling], na.rm = TRUE)
        
        if(out == 3)
        {
          continue <- FALSE  
        }
        
      }
      
      closer_home <- pitching_RP_home[sampling,]
    }
    
    closer_home$GameDate <- as.Date(closer_home$GameDate)
    
    closer_home <- closer_home[order(closer_home$IP, decreasing = TRUE),]
    
    closer_home <- closer_home[1,]
    
    for(i in 1:nrow(closer_home))
    {
      inning <- ((closer_home$IP[i] %/% 1 + (((closer_home$IP[i] %% 1) * (10/3)))) / 9)
      
      ER <- ifelse((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (closer_home$LW[i]) + (4.25 * inning) < 0, 0, ER <- round((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (closer_home$LW[i]) + (4.25 * inning),digits=0))
      
      closer_home$ER[i] <- ER
    }
    
    pitching_line_home$GameDate <- as.Date(pitching_line_home$GameDate)
    
    if(closer_home$IP[1] > 1)
    {
      closer_home$IP[1] <- 1
      
      sample_home$OUT[1] <- 3
      
      sample_home$LW <- round((sample_home$X1B *-0.46) + (sample_home$X2B * -0.8) + (sample_home$X3B * -1.02) + (sample_home$HR * -1.4) + (sample_home$HBP * -0.33) + (sample_home$BB * -0.33) + 
                                (sample_home$K * 0.1) + (sample_home$WP * -0.395) + (sample_home$BLK * -0.15) + (sample_home$CS * 0.3) + (sample_home$PKO * 0.145) + (sample_home$OUT * 0.25) + (sample_home$SB * -0.15) + (sample_home$SH * -0.146) + (sample_home$SF * -0.2),digits = 3)
      
    }
    
    if(closer_home$IP[1] == 1)
    {
      pitching_line_home <- rbind(pitching_line_home, closer_home)
    }
    
    if(closer_home$IP[1] < 1)
    {
      out_needed_in_9th_home <- 3 - ((((closer_home$IP[1]) %/% 1) * 3) + ((closer_home$IP[1] %% 1) * 10))
      
      inning_needed_in_9th_home <- out_needed_in_9th_home / 10
      
      home_pitcher <- only_active_players$MLBId[(only_active_players$Team_RFB %in% final_schedule$Home[x]) & (only_active_players$Pos %in% 1)]
      
      pitching_line_home$MLBId <- as.character(pitching_line_home$MLBId)
      
      short_relief_home <- pitching_RP[(pitching_RP$MLBId %in% home_pitcher),]
      
      short_relief_home <- short_relief_home[!(short_relief_home$MLBId %in% pitching_line_home$MLBId),]
      
      short_relief_home <- short_relief_home[short_relief_home$IP %in% inning_needed_in_9th_home,]
      
      if(nrow(short_relief_home) == 0)
      {
        closer_home <- pitching_RP[(pitching_RP$MLBId %in% home_pitcher),]
        
        closer_home <- closer_home[!(closer_home$MLBId %in% pitching_line_home$MLBId),]
        
        closer_home <- closer_home[closer_home$IP %in% 1,]
        
        if(nrow(closer_home) == 0)
        {
          closer_home <- pitching_RP[(pitching_RP$MLBId %in% home_pitcher),]
          
          closer_home <- closer_home[!(closer_home$MLBId %in% pitching_line_home$MLBId),]
          
          if(closer_home$PlayerName %in% lineup$fullname[(lineup$Team %in% final_schedule$Home[x]) & (lineup$Role == "CLOSER")]){
            closer_home <- closer_home[closer_home$PlayerName %in% lineup$fullname[(lineup$Team %in% final_schedule$Home[x]) & (lineup$Role == "CLOSER")],]
            
            closer_home <- closer_home[1,]
          }
          
          
          if(!(closer_home$PlayerName %in% lineup$fullname[(lineup$Team %in% final_schedule$Home[x]) & (lineup$Role == "CLOSER")]))
          {
            closer_home <- closer_home[1,]
          }
        }
        
        closer_home <- closer_home[1,]
        
        for(i in 1:nrow(closer_home))
        {
          inning <- ((closer_home$IP[i] %/% 1 + (((closer_home$IP[i] %% 1) * (10/3)))) / 9)
          
          ER <- ifelse((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (closer_home$LW[i]) + (4.25 * inning) < 0, 0, ER <- round((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (closer_home$LW[i]) + (4.25 * inning),digits=0))
          
          closer_home$ER[i] <- ER
        }
        
        closer_home$IP[1] <- 1
        
        sample_home$OUT[1] <- 3
        
        sample_home$LW <- round((sample_home$X1B *-0.46) + (sample_home$X2B * -0.8) + (sample_home$X3B * -1.02) + (sample_home$HR * -1.4) + (sample_home$HBP * -0.33) + (sample_home$BB * -0.33) + 
                                  (sample_home$K * 0.1) + (sample_home$WP * -0.395) + (sample_home$BLK * -0.15) + (sample_home$CS * 0.3) + (sample_home$PKO * 0.145) + (sample_home$OUT * 0.25) + (sample_home$SB * -0.15) + (sample_home$SH * -0.146) + (sample_home$SF * -0.2),digits = 3)
        
        
        pitching_line_home <- rbind(pitching_line_home, closer_home)
      }
      
      if(nrow(short_relief_home) > 0)
      {
        closer_home$GameDate <- as.Date(closer_home$GameDate)
        pitching_line_home$GameDate <- as.Date(pitching_line_home$GameDate)
        short_relief_home$GameDate <- as.Date(short_relief_home$GameDate)
        
        short_relief_home <- short_relief_home[!(short_relief_home$PlayerName %in% closer_home$PlayerName),]
        
        if(nrow(short_relief_home) == 0)
        {
          closer_home <- pitching_RP[(pitching_RP$MLBId %in% home_pitcher),]
          
          closer_home <- closer_home[!(closer_home$MLBId %in% pitching_line_home$MLBId),]
          
          closer_home <- closer_home[closer_home$IP %in% 1,]
          
          closer_home <- closer_home[1,]
          
          for(i in 1:nrow(closer_home))
          {
            inning <- ((closer_home$IP[i] %/% 1 + (((closer_home$IP[i] %% 1) * (10/3)))) / 9)
            
            ER <- ifelse((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (closer_home$LW[i]) + (4.25 * inning) < 0, 0, ER <- round((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (closer_home$LW[i]) + (4.25 * inning),digits=0))
            
            closer_home$ER[i] <- ER
          }
          
          pitching_line_home <- rbind(pitching_line_home, closer_home)
        }
        
        if(nrow(short_relief_home) > 0)
        {
          short_relief_home <- short_relief_home[order(short_relief_home$GameDate, decreasing = TRUE),]
          
          short_relief_home <- short_relief_home[1,]
          
          for(i in 1:nrow(short_relief_home))
          {
            inning <- ((short_relief_home$IP[i] %/% 1 + (((short_relief_home$IP[i] %% 1) * (10/3)))) / 9)
            
            ER <- ifelse((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (short_relief_home$LW[i]) + (4.25 * inning) < 0, 0, ER <- round((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (short_relief_home$LW[i]) + (4.25 * inning),digits=0))
            
            short_relief_home$ER[i] <- ER
          }
          
          closer_home <- rbind(short_relief_home, closer_home)
          
          pitching_line_home <- rbind(pitching_line_home, closer_home)
        }
        
      }
    }
    
    #
    
    slot <- score$Out[(score$Side == "Top") & (score$Inning == 9)]
    
    closer_home$OUT <- ((((closer_home$IP) %/% 1) * 3) + ((closer_home$IP %% 1) * 10))
    
    for(i in 1:nrow(closer_home))
    {
      if(closer_home$IP[i] == 0)
      {
        next;
      }
      
      num_outs <- (closer_home$IP[i] %/% 1) * 3 + ((closer_home$IP[i] %% 1) * 10)
      
      score$Pit[slot[1:num_outs]] <- as.character(closer_home$PlayerName[i])
      
      slot <- slot[!(c(1:3) %in% c(1:num_outs))]
      
    }
    
    for(i in 1:nrow(closer_home))
    {
      inning <- ((closer_home$IP[i] %/% 1 + (((closer_home$IP[i] %% 1) * (10/3)))) / 9)
      
      
      closer_home$ER[i] <- ifelse(round((blank_visit$LW[2] * inning) -  (blank_home$LW[3] * inning) - closer_home$LW[i] + (4.25 * inning),digits=0) < 0, 0, closer_home$ER[i] <- round((blank_visit$LW[2] * inning) -  (blank_home$LW[3] * inning) - closer_home$LW[i] + (4.25 * inning),digits=0))
      
      out_9th_home <- (sum(closer_home$IP[i], na.rm = TRUE) * 3)
      
      home_9th_outs <- score$Out[(score$Side == "Top") & (score$Inning == 9) & (score$Pit %in% closer_home$PlayerName[i])]
      
      if(length(home_9th_outs) == 1)
      {
        run_allowed_home_9th <- home_9th_outs
        
      }
      
      if(length(home_9th_outs) > 1)
      {
        run_allowed_home_9th <- sample(home_9th_outs, size = 1, replace = FALSE)
        
      }
      
      
      score$Add_score[run_allowed_home_9th] <- closer_home$ER[i]
      
      run_not_allowed_home_9th <- home_9th_outs[!(home_9th_outs %in% run_allowed_home_9th)]
      
      score$Add_score[run_not_allowed_home_9th] <- 0
      
      top9th <- score$Out[(score$Side == "Top") & (score$Inning == 9)]
      
      score$`V-Score` <- as.numeric(score$`V-Score`)
      score$`H-Score` <- as.numeric(score$`H-Score`)
      score$Add_score <- as.numeric(score$Add_score)
    }
    
    for(i in 1:length(top9th))
    {
      score$`V-Score`[top9th[i]] <- score$`V-Score`[top9th[i]-1] + score$Add_score[top9th[i]]
      score$`H-Score`[top9th[i]] <- score$`H-Score`[top9th[i]-1]
    }
    #
  }
}


blank <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_home)))

colnames(blank) <- colnames(box_stat_home)

blank$GameDate <- as.Date(blank$GameDate, format= "%Y-%m-%d")

box_stat_home <- rbind(box_stat_home, blank)

# HOME TEAM WON. FINALIZE GAME. (END OF TOP 9TH)
# Conclude game here if home team is winning by the end of top 9th

if((score$`H-Score`[((score$Side %in% "Top") & (score$Out_count %in% 2) & (score$Inning %in% 9))]) > (score$`V-Score`[((score$Side %in% "Top") & (score$Out_count %in% 2) & (score$Inning %in% 9))]))
{
  # Delete bottom 9th rows from 'score'
  
  score <- score[!((score$Side == "Bottom") & (score$Inning == 9)),]
  
  
  # Match pitchers with fielding record (visit)
  
  blank <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_home)))
  
  colnames(blank) <- colnames(box_stat_visit)
  
  blank$GameDate <- as.Date(blank$GameDate, format= "%Y-%m-%d")
  
  box_stat_visit <- rbind(box_stat_visit, blank)
  
  for(i in 1:nrow(pitching_line_visit))
  {
    pitch_fielder_visit <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_home)))
    
    colnames(pitch_fielder_visit) <- colnames(box_stat_visit)
    
    pitch_fielder_visit$LastName <- as.character(pitch_fielder_visit$LastName)
    
    pitch_fielder_visit$FirstName <- as.character(pitch_fielder_visit$FirstName)
    
    pitch_fielder_visit$GameDate <- as.Date(pitch_fielder_visit$GameDate, format = "%Y-%m-%d")
    
    pitch_fielder_visit$Field <- as.double(pitch_fielder_visit$Field)
    
    pitch_fielder_visit$E <- as.double(pitch_fielder_visit$E)
    
    pitch_fielder_visit$Zone <- as.double(pitch_fielder_visit$Zone)
    
    
    fielding_available$MLBId <- as.character(fielding_available$MLBId)
    
    pitching_line_visit$GameDate <- as.Date(pitching_line_visit$GameDate, format = "%Y-%m-%d")
    
    pitch_fielder_visit$LastName[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)) > 0,pitch_fielder_visit$LastName[1] <- as.character(fielding_available2$LastName[which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)]), (pitch_fielder_visit$LastName[1] <- as.character(pitching_line_visit$LastName[i])))
    
    pitch_fielder_visit$FirstName[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)) > 0,pitch_fielder_visit$FirstName[1] <- as.character(fielding_available2$FirstName[which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)]), (pitch_fielder_visit$FirstName[1] <- as.character(pitching_line_visit$FirstName[i])))
    
    pitch_fielder_visit$GameDate[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)) > 0,pitch_fielder_visit$GameDate[1] <- as.character(fielding_available2$GameDate[which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)]), (pitch_fielder_visit$GameDate[1] <- as.character(pitching_line_visit$GameDate[i])))
    
    pitch_fielder_visit$Field[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)) > 0, pitch_fielder_visit$Field[1] <- as.character(fielding_available2$LW[which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)]), (pitch_fielder_visit$Field[1] <- as.double(0)))
    
    pitch_fielder_visit$E[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)) > 0, pitch_fielder_visit$E[1] <- as.character(fielding_available2$E[which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)]), (pitch_fielder_visit$E[1] <- as.double(0)))
    
    YTD$MLBId <- as.character(YTD$MLBId)
    
    ytd_stat <- YTD[(YTD$MLBId %in% pitching_line_visit$MLBId[i]),]
    
    ytd_stat <- unique(ytd_stat)
    
    ytd_stat$Pos <- as.character(ytd_stat$Pos)
    
    pitch_fielder_visit$Zone[1] <- ytd_stat$Zone[1]
    
    box_stat_visit <- rbind(box_stat_visit, pitch_fielder_visit)
    
    blank_visit$Zone <- as.double(blank_visit$Zone)
    
    blank_visit$Zone[1] <- blank_visit$Zone[1] + ytd_stat$Zone[1]
  }
  
  blank_visit$Zone <- as.character(blank_visit$Zone)
  
  
  # Update the overall offense, and overall defense
  
  box_stat_visit$LW <- as.numeric(box_stat_visit$LW)
  box_stat_visit$Bonus <- as.numeric(box_stat_visit$Bonus)
  box_stat_visit$Bases_Taken <- as.numeric(box_stat_visit$Bases_Taken)
  box_stat_visit$Outs_on_Base <- as.numeric(box_stat_visit$Outs_on_Base)
  box_stat_visit$Field <- as.numeric(box_stat_visit$Field)
  box_stat_visit$E <- as.integer(box_stat_visit$E)
  box_stat_visit$Zone <- as.numeric(box_stat_visit$Zone)
  
  
  blank_visit$LW[1] <- sum(box_stat_visit$LW, na.rm = TRUE)
  blank_visit$Bonus[1] <- sum(box_stat_visit$Bonus, na.rm = TRUE)
  blank_visit$Bases_Taken[1] <- sum(box_stat_visit$Bases_Taken, na.rm = TRUE)
  blank_visit$Outs_on_Base[1] <- sum(box_stat_visit$Outs_on_Base, na.rm = TRUE)
  
  if(away_sp_stat$MLBId[1] %in% box_stat_visit$MLBId)
  {
    ifelse(box_stat_visit$Field[which(box_stat_visit$MLBId %in% away_sp_stat$MLBId[1])] %in% NA, blank_visit$Field[1] <- sum(box_stat_visit$Field, na.rm = TRUE),blank_visit$Field[1] <- sum(box_stat_visit$Field, na.rm = TRUE) - box_stat_visit$Field[which(box_stat_visit$MLBId %in% away_sp_stat$MLBId[1])])
    ifelse(box_stat_visit$E[which(box_stat_visit$MLBId %in% away_sp_stat$MLBId[1])] %in% NA, blank_visit$E[1] <- sum(box_stat_visit$E, na.rm = TRUE),blank_visit$E[1] <- sum(box_stat_visit$E, na.rm = TRUE) - box_stat_visit$E[which(box_stat_visit$MLBId %in% away_sp_stat$MLBId[1])])
    ifelse(box_stat_visit$Zone[which(box_stat_visit$MLBId %in% away_sp_stat$MLBId[1])] %in% NA, blank_visit$Zone[1] <- sum(box_stat_visit$Zone, na.rm = TRUE),blank_visit$Zone[1] <- sum(box_stat_visit$Zone, na.rm = TRUE) - box_stat_visit$Zone[which(box_stat_visit$MLBId %in% away_sp_stat$MLBId[1])])
    
    blank_visit$LW[2] <- as.numeric(blank_visit$LW[1]) + as.numeric(blank_visit$Bonus[1]) + as.numeric(blank_visit$Bases_Taken[1]) + as.numeric(blank_visit$Outs_on_Base[1])
    
    blank_visit$LW[3] <- as.numeric(blank_visit$Field[1]) + as.numeric(blank_visit$Zone[1]) + as.numeric(blank_visit$Block[1]) + as.numeric(blank_visit$Frame[1])
    
  }
  
  if(!away_sp_stat$MLBId[1] %in% box_stat_visit$MLBId)
  {
    blank_visit$Field[1] <- sum(box_stat_visit$Field, na.rm = TRUE)
    blank_visit$E[1] <- sum(box_stat_visit$E, na.rm = TRUE)
    blank_visit$Zone[1] <- sum(box_stat_visit$Zone, na.rm = TRUE)
    
    blank_visit$LW[2] <- as.numeric(blank_visit$LW[1]) + as.numeric(blank_visit$Bonus[1]) + as.numeric(blank_visit$Bases_Taken[1]) + as.numeric(blank_visit$Outs_on_Base[1])
    
    blank_visit$LW[3] <- as.numeric(blank_visit$Field[1]) + as.numeric(blank_visit$Zone[1]) + as.numeric(blank_visit$Block[1]) + as.numeric(blank_visit$Frame[1])
    
  }
  
  box_stat_visit$H <- as.numeric(box_stat_visit$H)
  
  blank_visit$H[1] <- sum(box_stat_visit$H, na.rm = TRUE)
  
  # Match pitchers with fielding record (visit)
  
  
  for(i in 1:nrow(pitching_line_home))
  {
    pitch_fielder_home <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_home)))
    
    colnames(pitch_fielder_home) <- colnames(box_stat_home)
    
    pitch_fielder_home$LastName <- as.character(pitch_fielder_home$LastName)
    
    pitch_fielder_home$FirstName <- as.character(pitch_fielder_home$FirstName)
    
    pitch_fielder_home$GameDate <- as.Date(pitch_fielder_home$GameDate, format = "%Y-%m-%d")
    
    pitch_fielder_home$Field <- as.double(pitch_fielder_home$Field)
    
    pitch_fielder_home$E <- as.double(pitch_fielder_home$E)
    
    pitch_fielder_home$Zone <- as.double(pitch_fielder_home$Zone)
    
    fielding_available$MLBId <- as.character(fielding_available$MLBId)
    
    pitching_line_home$GameDate <- as.Date(pitching_line_home$GameDate, format = "%Y-%m-%d")
    
    pitch_fielder_home$LastName[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)) > 0,pitch_fielder_home$LastName[1] <- as.character(fielding_available2$LastName[which(((fielding_available2$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)]), (pitch_fielder_home$LastName[1] <- as.character(pitching_line_home$LastName[i])))
    
    pitch_fielder_home$FirstName[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)) > 0,pitch_fielder_home$FirstName[1] <- as.character(fielding_available2$FirstName[which(((fielding_available2$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)]), (pitch_fielder_home$FirstName[1] <- as.character(pitching_line_home$FirstName[i])))
    
    pitch_fielder_home$GameDate[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)) > 0,pitch_fielder_home$GameDate[1] <- as.character(fielding_available2$GameDate[which(((fielding_available2$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)]), (pitch_fielder_home$GameDate[1] <- as.character(pitching_line_home$GameDate[i])))
    
    pitch_fielder_home$Field[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)) > 0, pitch_fielder_home$Field[1] <- as.character(fielding_available2$LW[which(((fielding_available2$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)]), (pitch_fielder_home$Field[1] <- as.double(0)))
    
    pitch_fielder_home$E[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)) > 0, pitch_fielder_home$E[1] <- as.character(fielding_available2$E[which(((fielding_available2$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)]), (pitch_fielder_home$E[1] <- as.double(0)))
    
    YTD$MLBId <- as.character(YTD$MLBId)
    
    ytd_stat <- YTD[(YTD$MLBId %in% pitching_line_home$MLBId[i]),]
    
    ytd_stat <- unique(ytd_stat)
    
    ytd_stat$Pos <- as.character(ytd_stat$Pos)
    
    pitch_fielder_home$Zone[1] <- ytd_stat$Zone[1]
    
    box_stat_home <- rbind(box_stat_home, pitch_fielder_home)
    
    blank_home$Zone <- as.double(blank_home$Zone)
    
    
    blank_home$Zone[1] <- blank_home$Zone[1] + ytd_stat$Zone[1]
    
  }
  
  blank_home$Zone <- as.character(blank_home$Zone)
  
  
  box_stat_home$LW <- as.numeric(box_stat_home$LW)
  box_stat_home$Bonus <- as.numeric(box_stat_home$Bonus)
  box_stat_home$Bases_Taken <- as.numeric(box_stat_home$Bases_Taken)
  box_stat_home$Outs_on_Base <- as.numeric(box_stat_home$Outs_on_Base)
  box_stat_home$Field <- as.numeric(box_stat_home$Field)
  box_stat_home$E <- as.integer(box_stat_home$E)
  box_stat_home$Zone <- as.numeric(box_stat_home$Zone)
  
  blank_home$LW[1] <- sum(box_stat_home$LW, na.rm = TRUE)
  blank_home$Bonus[1] <- sum(box_stat_home$Bonus, na.rm = TRUE)
  blank_home$Bases_Taken[1] <- sum(box_stat_home$Bases_Taken, na.rm = TRUE)
  blank_home$Outs_on_Base[1] <- sum(box_stat_home$Outs_on_Base, na.rm = TRUE)
  
  if(home_sp_stat$MLBId[1] %in% box_stat_home$MLBId)
  {
    ifelse(box_stat_home$Field[which(box_stat_home$MLBId %in% home_sp_stat$MLBId[1])] %in% NA, blank_home$Field[1] <- sum(box_stat_home$Field, na.rm = TRUE),blank_home$Field[1] <- sum(box_stat_home$Field, na.rm = TRUE) - box_stat_home$Field[which(box_stat_home$MLBId %in% home_sp_stat$MLBId[1])])
    ifelse(box_stat_home$E[which(box_stat_home$MLBId %in% home_sp_stat$MLBId[1])] %in% NA, blank_home$E[1] <- sum(box_stat_home$E, na.rm = TRUE),blank_home$E[1] <- sum(box_stat_home$E, na.rm = TRUE) - box_stat_home$E[which(box_stat_home$MLBId %in% home_sp_stat$MLBId[1])])
    ifelse(box_stat_home$Zone[which(box_stat_home$MLBId %in% home_sp_stat$MLBId[1])] %in% NA, blank_home$Zone[1] <- sum(box_stat_home$Zone, na.rm = TRUE),blank_home$Zone[1] <- sum(box_stat_home$Zone, na.rm = TRUE) - box_stat_home$Zone[which(box_stat_home$MLBId %in% home_sp_stat$MLBId[1])])
    
    blank_home$LW[2] <- as.numeric(blank_home$LW[1]) + as.numeric(blank_home$Bonus[1]) + as.numeric(blank_home$Bases_Taken[1]) + as.numeric(blank_home$Outs_on_Base[1])
    
    blank_home$LW[3] <- as.numeric(blank_home$Field[1]) + as.numeric(blank_home$Zone[1]) + as.numeric(blank_home$Block[1]) + as.numeric(blank_home$Frame[1])
    
  }
  
  if(!home_sp_stat$MLBId[1] %in% box_stat_home$MLBId)
  {
    blank_home$Field[1] <- sum(box_stat_home$Field, na.rm = TRUE)
    blank_home$E[1] <- sum(box_stat_home$E, na.rm = TRUE)
    blank_home$Zone[1] <- sum(box_stat_home$Zone, na.rm = TRUE)
    
    blank_home$LW[2] <- as.numeric(blank_home$LW[1]) + as.numeric(blank_home$Bonus[1]) + as.numeric(blank_home$Bases_Taken[1]) + as.numeric(blank_home$Outs_on_Base[1])
    
    blank_home$LW[3] <- as.numeric(blank_home$Field[1]) + as.numeric(blank_home$Zone[1]) + as.numeric(blank_home$Block[1]) + as.numeric(blank_home$Frame[1])
    
  }
  
  box_stat_home$H <- as.numeric(box_stat_home$H)
  
  blank_home$H[1] <- sum(box_stat_home$H, na.rm = TRUE)
  
  
  ### Calculate winning_team, lead_by, winning pit, losing pit
  
  
  # Winning
  
  for(i in 1:nrow(score))
  {
    
    if((score$`V-Score`[i] - score$`H-Score`[i]) > 0)
    {
      score$Winning_Team[i] <- score$Visit[i]
    }
    
    if((score$`H-Score`[i] - score$`V-Score`[i]) > 0)
    {
      score$Winning_Team[i] <- score$Home[i]
    }
    
    if(score$`V-Score`[i] - score$`H-Score`[i] == 0)
    {
      score$Winning_Team[i] <- "Tie"
    }
    
  }
  
  # Who won and lost?
  
  condition <- TRUE
  
  i <- nrow(score)
  
  while(condition == TRUE)
  {
    i <- i - 1
    print(i)
    condition <- identical(score$Winning_Team[nrow(score)], score$Winning_Team[i])
    
    if(condition == FALSE)
    {
      score$Losing_Pit[(i+1):nrow(score)] <- score$Pit[i+1]
      break;
    }
  }
  
  side <- c("Top","Bottom")
  
  if(score$Inning[i] > 1){
    winning_pitcher <- score$Pit[max(which((score$Side == side[!(side %in% score$Side[i+1])]) & (score$Out < (i+1))))]
    
  }
  
  if(score$Inning[i] == 1){
    winning_pitcher <- score$Pit[min(which((score$Side == side[!(side %in% score$Side[i+1])]) & (score$Out > (i+1))))]
    
  }
  
  losing_pitcher <- score$Pit[i+1]
  
  # Calculate 'Lead_by'
  
  for(i in 1:nrow(score))
  {
    if(score$Winning_Team[i] %in% final_schedule$Home[x])
    {
      score$Lead_By[i] <- score$`H-Score`[i] - score$`V-Score`[i]
    }
    
    if(score$Winning_Team[i] %in% final_schedule$Away[x])
    {
      score$Lead_By[i] <- score$`V-Score`[i] - score$`H-Score`[i]
      
    }
    
    if(score$Winning_Team[i] %in% "Tie")
    {
      score$Lead_By[i] <- 0
    }
  }
  
  
  # Assign decisions on pitchers. 
  
  for(i in 1:nrow(pitching_line_home))
  {
    if(pitching_line_home$PlayerName[i] == winning_pitcher)
    {
      pitching_line_home$DEC[i] <- "W"
    }
    
    if(pitching_line_home$PlayerName[i] == losing_pitcher)
    {
      pitching_line_home$DEC[i] <- "L"
    }
  }
  
  for(i in 1:nrow(pitching_line_visit))
  {
    if(pitching_line_visit$PlayerName[i] == winning_pitcher)
    {
      pitching_line_visit$DEC[i] <- "W"
    }
    
    if(pitching_line_visit$PlayerName[i] == losing_pitcher)
    {
      pitching_line_visit$DEC[i] <- "L"
    }
  }
  
  save_sit <- score$Out[(score$Side == "Top") & (score$Inning %in% 9) & (score$Out_count %in% 2)]
  closing_pitcher <- score$Pit[save_sit]
  
  if(score$Lead_By[save_sit] < 4)
  {
    
    if(nrow(pitching_line_home) > 1){
      
      for(i in 1:nrow(pitching_line_home))
      {
        if(pitching_line_home$PlayerName[i] == closing_pitcher)
        {
          pitching_line_home$DEC[i] <- "SV"
        }
        
      }
      
    }
    
    if(nrow(pitching_line_home) == 1)
    {
      print("Can't and won't assign SV")
    }
    
    if(nrow(pitching_line_visit) > 1){
      
      for(i in 1:nrow(pitching_line_visit))
      {
        if(pitching_line_visit$PlayerName[i] == closing_pitcher)
        {
          pitching_line_visit$DEC[i] <- "SV"
        }
        
      }
    }
    
    if(nrow(pitching_line_visit) == 1){
      print("Can't and won't assign SV")
    }
  }
  
  if("SV" %in% pitching_line_home$DEC)
  {
    num_of_pitchers <- 1:nrow(pitching_line_home)
    
    hold_sit <- which(!(num_of_pitchers %in% which(pitching_line_home$DEC %in% c("W","SV"))))
    
    hold_sit <- hold_sit[!(hold_sit < which(pitching_line_home$DEC == "W"))]
    
    pitching_line_home$DEC[hold_sit] <- "HD"
  }
  
  if("SV" %in% pitching_line_visit$DEC)
  {
    num_of_pitchers <- 1:nrow(pitching_line_visit)
    
    hold_sit <- which(!(num_of_pitchers %in% which(pitching_line_visit$DEC %in% c("W","SV"))))
    
    hold_sit <- hold_sit[!(hold_sit < which(pitching_line_visit$DEC == "W"))]
    
    pitching_line_visit$DEC[hold_sit] <- "HD"
  }
  
  # Paste batting and pitching lines together. (For both teams)
  
  #HOME
  
  blank_home$GameDate <- as.Date(blank_home$GameDate, format = "%Y-%m-%d")
  
  box_stat_home <- rbind(box_stat_home, blank_home)
  
  blank <- data.frame(matrix("", nrow = 1, ncol = ncol(box_stat_home)))
  
  colnames(blank) <- colnames(box_stat_home)
  
  blank$GameDate <- as.Date(blank$GameDate, format = "%Y-%m-%d")
  
  box_stat_home <- rbind(box_stat_home, blank)
  
  label <- read.csv("label.csv", header = TRUE)
  
  label$POS <- "POS"
  
  label[,1] <- as.Date(label[,1], format="%Y-%m-%d")
  
  label <- label[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                    "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                    "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
  
  colnames(label) <- colnames(box_stat_home)
  
  box_stat_home <- rbind(box_stat_home, label)
  
  pitching_line_home$POS <- ""
  
  pitching_line_home <- pitching_line_home[,c("POS","GameDate","FirstName","LastName","LW","DEC","X1","X2","X3","X4","X5","X6",
                                              "X7","IP","BFP","H","X1B","X2B","X3B","HR","ER","SH","SF","HBP","BB","K","WP",
                                              "BLK","IR","IRS","GB","FB","LD","POPU","SB","CS","PKO","OUT","MLBId","PlayerName",
                                              "GameString","GameId","used","uniqueId")]
  
  if(nrow(pitching_line_home) == 1)
  {
    pitching_line_home$POS[1] <- "SP"
    
  }
  
  if(nrow(pitching_line_home) > 1)
  {
    pitching_line_home$POS[1] <- "SP"
    
    pitching_line_home$POS[2:nrow(pitching_line_home)] <- "RP"
  }
  
  colnames(pitching_line_home) <- colnames(box_stat_home)
  
  home_H <- round(((as.numeric(blank_home$H[1]) + sum(as.numeric(pitching_line_visit$H), na.rm = TRUE))) / 2,digits=0)
  home_H2 <- as.integer(home_H)
  home_R <- score$`H-Score`[nrow(score)]
  box_stat_home$E <- as.integer(box_stat_home$E)
  home_E <- blank_home$E[1]
  
  away_H <- round(((as.numeric(blank_visit$H[1]) + sum(as.numeric(pitching_line_home$H), na.rm = TRUE))) / 2,digits=0)
  away_H2 <- as.integer(away_H)
  away_R <- score$`V-Score`[nrow(score)]
  away_E <- blank_visit$E[1]
  
  for(i in 1:ncol(box_stat_home))
  {
    box_stat_home[,i] <- as.character(box_stat_home[,i])
  }
  
  for(i in 1:ncol(pitching_line_home))
  {
    pitching_line_home[,i] <- as.character(pitching_line_home[,i]) 
  }
  
  
  pitching_name_home <- unique(pitching_line_home$PlayerName)
  
  for(i in 1:length(pitching_name_home))
  {
    pitching_line_home$HR[i] <- sum(score$Add_score[which(score$Pit %in% pitching_name_home[i])], na.rm = TRUE)
  }
  
  residue_home <- away_R - sum(as.numeric(pitching_line_home$HR), na.rm = TRUE)
  
  pitching_line_home$HR[which(pitching_line_home$LW == min(pitching_line_home$LW))] <- as.numeric(pitching_line_home$HR[which(pitching_line_home$LW == min(pitching_line_home$LW))]) + residue_home
  
  box_stat_home$E <- as.integer(box_stat_home$E)
  
  E_home <- blank_home$E[1]
  
  if(E_home == 0)
  {
    print("Zero Error")
  }
  
  if(E_home > 0)
  {
    for(i in 1:E_home)
    {
      sampling <- sample(1:nrow(pitching_line_home), size = 1, replace = TRUE)
      
      if(pitching_line_home$HR[sampling] == 0)
      {
        print("Sampling is zero")
      }
      
      if(pitching_line_home$HR[sampling] > 0)
      {
        pitching_line_home$HR[sampling] <- as.numeric(pitching_line_home$HR[sampling]) - 1
      }
    }
  }
  
  
  box_stat_home <- rbind(box_stat_home, pitching_line_home)
  
  overall_pitching <- data.frame(matrix(NA, nrow = 1, ncol = ncol(box_stat_home)))
  
  colnames(overall_pitching) <- colnames(box_stat_home)
  
  overall_pitching$LastName[1] <- as.character("Overall Pitching")
  
  overall_pitching$LW[1] <- as.character(as.numeric(sum(as.numeric(pitching_line_home$LW), na.rm = TRUE)))
  
  box_stat_home <- rbind(box_stat_home, overall_pitching)
  
  # VISIT
  
  blank_visit$GameDate <- as.Date(blank_visit$GameDate, format = "%Y-%m-%d")
  
  box_stat_visit <- rbind(box_stat_visit, blank_visit)
  
  blank <- data.frame(matrix("", nrow = 1, ncol = ncol(box_stat_visit)))
  
  colnames(blank) <- colnames(box_stat_visit)
  
  blank$GameDate <- as.Date(blank$GameDate, format = "%Y-%m-%d")
  
  box_stat_visit <- rbind(box_stat_visit, blank)
  
  label <- read.csv("label.csv", header = TRUE)
  
  label$POS <- "POS"
  
  label[,1] <- as.Date(label[,1], format="%Y-%m-%d")
  
  label <- label[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                    "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                    "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
  
  colnames(label) <- colnames(box_stat_visit)
  
  box_stat_visit <- rbind(box_stat_visit, label)
  
  pitching_line_visit$POS <- ""
  
  pitching_line_visit <- pitching_line_visit[,c("POS","GameDate","FirstName","LastName","LW","DEC","X1","X2","X3","X4","X5","X6",
                                                "X7","IP","BFP","H","X1B","X2B","X3B","HR","ER","SH","SF","HBP","BB","K","WP",
                                                "BLK","IR","IRS","GB","FB","LD","POPU","SB","CS","PKO","OUT","MLBId","PlayerName",
                                                "GameString","GameId","used","uniqueId")]
  
  if(nrow(pitching_line_visit) == 1)
  {
    pitching_line_visit$POS[1] <- "SP"
    
  }
  
  if(nrow(pitching_line_visit) > 1)
  {
    pitching_line_visit$POS[1] <- "SP"
    
    pitching_line_visit$POS[2:nrow(pitching_line_visit)] <- "RP"
  }
  
  colnames(pitching_line_visit) <- colnames(box_stat_visit)
  
  
  for(i in 1:ncol(box_stat_visit))
  {
    box_stat_visit[,i] <- as.character(box_stat_visit[,i])
  }
  
  for(i in 1:ncol(pitching_line_visit))
  {
    pitching_line_visit[,i] <- as.character(pitching_line_visit[,i]) 
  }
  
  ###
  
  
  pitching_name_visit <- unique(pitching_line_visit$PlayerName)
  
  for(i in 1:length(pitching_name_visit))
  {
    pitching_line_visit$HR[i] <- sum(score$Add_score[which(score$Pit %in% pitching_name_visit[i])], na.rm = TRUE)
  }
  
  residue_away <- home_R - sum(as.numeric(pitching_line_visit$HR), na.rm = TRUE)
  
  pitching_line_visit$HR[which(pitching_line_visit$LW == min(pitching_line_visit$LW))] <- as.numeric(pitching_line_visit$HR[which(pitching_line_visit$LW == min(pitching_line_visit$LW))]) + residue_away
  
  box_stat_visit$E <- as.integer(box_stat_visit$E)
  
  E_Visit <- blank_visit$E[1]
  
  if(E_Visit == 0)
  {
    print("Zero Error")
  }
  
  if(E_Visit > 0)
  {
    
    for(i in 1:E_Visit)
    {
      sampling <- sample(1:nrow(pitching_line_visit), size = 1, replace = TRUE)
      
      if(pitching_line_visit$HR[sampling] == 0)
      {
        print("Sampling is zero")
      }
      
      if(pitching_line_visit$HR[sampling] > 0)
      {
        pitching_line_visit$HR[sampling] <- as.numeric(pitching_line_visit$HR[sampling]) - 1
      }
    }
  }
  
  ###
  
  box_stat_visit <- rbind(box_stat_visit, pitching_line_visit)
  
  overall_pitching <- data.frame(matrix(NA, nrow = 1, ncol = ncol(box_stat_visit)))
  
  colnames(overall_pitching) <- colnames(box_stat_visit)
  
  overall_pitching$LastName[1] <- as.character("Overall Pitching")
  
  overall_pitching$LW[1] <- as.character(sum(as.numeric(pitching_line_visit$LW), na.rm = TRUE))
  
  box_stat_visit <- rbind(box_stat_visit, overall_pitching)
  
  box_stat_visit <- rbind(box_stat_visit, blank)
  
  # Paste home and away team together and you have yourself a boxscore.
  
  box_score <- rbind(box_stat_visit, box_stat_home)
  
  # Remove all NA
  for(i in 1:ncol(box_score))
  {
    box_score[which(box_score[,i] %in% NA),i] <- ""
  }
  
  final_score <- data.frame(matrix("", nrow = 3, ncol = ncol(box_stat_visit)))
  
  colnames(final_score) <- colnames(box_stat_visit)
  
  final_score$MLBId <- as.character(final_score$MLBId)
  
  final_score$PlayerName <- as.character(final_score$PlayerName)
  
  final_score$GameString <- as.character(final_score$GameString)
  
  final_score$GameId <- as.character(final_score$GameId)
  
  final_score$PlayerName[1] <- "R"
  
  final_score$GameString[1] <- "H"
  
  final_score$GameId[1] <- "E"
  
  final_score$MLBId[2] <- as.character(paste(final_schedule$Away[x]," at",sep=""))
  
  final_score$MLBId[3] <- final_schedule$Home[x]
  
  final_score$PlayerName[2] <- away_R
  
  final_score$PlayerName[3] <- home_R
  
  
  final_score$GameId[2] <- away_E
  
  final_score$GameId[3] <- home_E
  
  final_score <- rbind(final_score, box_score)
  
  final_score$POS <- as.character(final_score$POS)
  
  final_score$POS[which(final_score$POS == "1")] <- sub("1", "P", final_score$POS[which(final_score$POS == "1")])
  final_score$POS[which(final_score$POS == "2")] <- sub("2", "CA", final_score$POS[which(final_score$POS == "2")])
  final_score$POS[which(final_score$POS == "3")] <- sub("3", "1B", final_score$POS[which(final_score$POS == "3")])
  final_score$POS[which(final_score$POS == "4")] <- sub("4", "2B", final_score$POS[which(final_score$POS == "4")])
  final_score$POS[which(final_score$POS == "5")] <- sub("5", "3B", final_score$POS[which(final_score$POS == "5")])
  final_score$POS[which(final_score$POS == "6")] <- sub("6", "SS", final_score$POS[which(final_score$POS == "6")])
  final_score$POS[which(final_score$POS == "7")] <- sub("7", "LF", final_score$POS[which(final_score$POS == "7")])
  final_score$POS[which(final_score$POS == "8")] <- sub("8", "CF", final_score$POS[which(final_score$POS == "8")])
  final_score$POS[which(final_score$POS == "9")] <- sub("9", "RF", final_score$POS[which(final_score$POS == "9")])
  
  # Away H
  
  final_score$GameString[2] <- round((sum(as.integer(as.character(final_score$R[which((final_score$POS %in% c("SP","RP")) & (final_score$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Home[x])]))])), na.rm = TRUE) + sum(as.integer(as.character(final_score$H[which((final_score$POS %in% c("CA","1B","2B","3B","SS","LF","CF","RF","P","PH","DH")) & (final_score$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x])]))])), na.rm = TRUE)) / 2, digits=0)
  
  # Home H
  
  final_score$GameString[3] <- round((sum(as.integer(as.character(final_score$R[which((final_score$POS %in% c("SP","RP")) & (final_score$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x])]))])), na.rm = TRUE) + sum(as.integer(as.character(final_score$H[which((final_score$POS %in% c("CA","1B","2B","3B","SS","LF","CF","RF","P","PH","DH")) & (final_score$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Home[x])]))])), na.rm = TRUE)) / 2, digits=0)
  
  
  write.csv(final_score, paste("box/",formatted_date,"/",formatted_date,final_schedule$Away[x],"@",final_schedule$Home[x],".csv",sep=""), row.names = FALSE)
  
}

# If visiting team is winning or game is tied, run this. This block further branches out to option of when visiting team win by 1 to 3
# or visiting team winning by more than 3. PULLS RELIEVERS DEPENDING ON SITUATION

if((score$`V-Score`[which(((score$Side == "Top") & (score$Out_count == 2) & (score$Inning == 9)))]) >= (score$`H-Score`[(((score$Side == "Top") & (score$Out_count == 2) & (score$Inning == 9)))]))
{
  
  # If visiting team is winning by lead between 1 and 3, run this.
  
  score$`V-Score` <- as.numeric(score$`V-Score`)
  score$`H-Score` <- as.numeric(score$`H-Score`)
  
  if((score$`V-Score`[which((score$Side == "Top") & (score$Inning == 9) & (score$Out_count == 2))] - score$`H-Score`[which((score$Side == "Top") & (score$Inning == 9) & (score$Out_count == 2))]) %in% c(0,1,2,3))
  {
    
    if(no_reliever_away == "YES")
    {
      print("No Reliever Needed")
    }
    
    if(!(no_reliever_away == "YES"))
    {
      
      # Get Closer
      
      visit_closer <- lineup$MLBId[lineup$Role == "CLOSER" & lineup$Team == final_schedule$Away[x]]
      
      # Get closer's stats
      
      closer_visit <- pitching_RP[pitching_RP$MLBId %in% as.character(visit_closer),]
      
      if(nrow(closer_visit) == 0)
      {
        pitching_RP_visit <- pitching_RP_visit[!(pitching_RP_visit$PlayerName %in% sample_visit$PlayerName),]
        
        pitching_RP_visit$OUT <- ((pitching_RP_visit$IP %/% 1) * 3) + ((pitching_RP_visit$IP %% 1) * 10)
        
        continue <- TRUE
        
        counter <- 0
        
        while(continue == TRUE)
        {
          counter <- counter + 1
          
          sampling <- sample(1:nrow(pitching_RP_visit), size = c(1:nrow(pitching_RP_visit)), replace = FALSE)
          
          out <- sum(pitching_RP_visit$OUT[sampling], na.rm = TRUE)
          
          if(out == 3)
          {
            continue <- FALSE  
          }
          
          if(counter == 500)
          {
            continue2 <- TRUE
            while(continue2 == TRUE)
            {
              
              sampling <- sample((1:nrow(pitching_RP_visit)), size = 1, replace = FALSE)
              
              out <- sum(pitching_RP_visit$OUT[sampling], na.rm = TRUE)
              
              number <- 3 - out
              
              if(number %in% c(1,2))
              {
                pitching_RP_visit$IP[sampling] <- 1
                continue <- FALSE
                continue2 <- FALSE
              }
              
              
            }
          }
          
        }
        
        closer_visit <- pitching_RP_visit[sampling,]
      }
      
      # Force the 'GameDate' column to be a date
      
      #closer_visit$GameDate <- as.Date(closer_visit$GameDate)
      
      # Order the closer stats by date, in decreasing order.
      
      closer_visit <- closer_visit[order(closer_visit$GameDate, decreasing = TRUE),]
      
      # Get the latest stats of closer
      
      closer_visit <- closer_visit[order(closer_visit$IP, decreasing = TRUE),]
      
      closer_visit <- closer_visit[!closer_visit$IP > 1,]
      
      closer_visit <- closer_visit[1,]
      
      if(closer_visit$GameDate %in% c(NA,""))
      {
        closer_visit <- pitching_RP_visit[!(pitching_RP_visit$MLBId %in% pitching_line_visit$MLBId),]
        closer_visit <- closer_visit[closer_visit$OUT < 4,]
        closer_visit <- closer_visit[1,]
      }
      
      #If closer stat is that of 1IP, paste it to pitching_line_visit
      
      if(closer_visit$IP[1] == 1)
      {
        for(i in 1:nrow(closer_visit))
        {
          inning <- (closer_visit$IP[i] %/% 1) + (((closer_visit$IP[i] %% 1) * (10/3)) / 9)
          
          closer_visit$ER[i] <- ifelse(round((blank_home$LW[2] * (((sum(closer_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(closer_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) -  (blank_visit$LW[3] * (((sum(closer_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(closer_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) - sum(closer_visit$LW[i], na.rm = TRUE) + (4.25 * (((sum(closer_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(closer_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)),digits=0) < 0, closer_visit$ER[i] <- 0, closer_visit$ER[i] <- round((blank_home$LW[2] * (((sum(closer_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(closer_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) -  (blank_visit$LW[3] * (((sum(closer_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(closer_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) - sum(closer_visit$LW[i], na.rm = TRUE) + (4.25 * (((sum(closer_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(closer_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)),digits=0))
          
        }
        
        pitching_line_visit <- rbind(pitching_line_visit, closer_visit)
        
        
        short_relief_visit <- closer_visit
        
        
      }
      
      # If closer stat is not exactly 1IP, 
      
      if(closer_visit$IP[1] < 1)
      {
        # Number of outs
        
        out_needed_in_9th_visit <- 3 - ((((closer_visit$IP[1]) %/% 1) * 3) + ((closer_visit$IP[1] %% 1) * 10))
        
        # Calculates more inning required to fill out the 9th inning with three outs.
        
        inning_needed_in_9th_visit <- out_needed_in_9th_visit / 10
        
        # Pitchers that are in visiting pitchers
        
        visit_pitcher <- only_active_players$MLBId[(only_active_players$Team_RFB %in% final_schedule$Away[x]) & (only_active_players$Pos %in% 1)]
        
        # Characterize MLBId column
        
        pitching_line_visit$MLBId <- as.character(pitching_line_visit$MLBId)
        
        # Get relief pitching stats from visiting team
        
        short_relief_visit <- pitching_RP[(pitching_RP$MLBId %in% visit_pitcher),]
        
        # Get relief pitchers only that has not pitched yet.
        
        short_relief_visit <- short_relief_visit[!(short_relief_visit$MLBId %in% pitching_line_visit$MLBId),]
        
        # Get relievers only with required number of innings
        
        short_relief_visit <- short_relief_visit[short_relief_visit$IP %in% inning_needed_in_9th_visit,]
        
        if(nrow(short_relief_visit) == 0)
        {
          new_closer_visit <- pitching_RP[(pitching_RP$MLBId %in% visit_pitcher),]
          
          new_closer_visit <- new_closer_visit[!(new_closer_visit$MLBId %in% pitching_line_visit$MLBId),]
          
          new_closer_visit <- new_closer_visit[new_closer_visit$IP %in% 1,]
          
          new_closer_visit <- new_closer_visit[1,]
          
          pitching_line_visit <- rbind(pitching_line_visit, new_closer_visit)
          
          short_relief_visit <- new_closer_visit
          
          for(i in 1:nrow(short_relief_visit))
          {
            inning <- (short_relief_visit$IP[i] %/% 1) + (((short_relief_visit$IP[i] %% 1) * (10/3)) / 9)
            
            short_relief_visit$ER[i] <- ifelse(round((blank_home$LW[2] * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) -  (blank_visit$LW[3] * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) - sum(short_relief_visit$LW[i], na.rm = TRUE) + (4.25 * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)),digits=0) < 0, short_relief_visit$ER[i] <- 0, short_relief_visit$ER[i] <- round((blank_home$LW[2] * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) -  (blank_visit$LW[3] * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) - sum(short_relief_visit$LW[i], na.rm = TRUE) + (4.25 * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)),digits=0))
            
          }
        }
        
        if(nrow(short_relief_visit) > 0)
        {
          #closer_visit$GameDate <- as.Date(closer_visit$GameDate)
          #pitching_line_visit$GameDate <- as.Date(pitching_line_visit$GameDate)
          #short_relief_visit$GameDate <- as.Date(short_relief_visit$GameDate)
          
          short_relief_visit <- short_relief_visit[!(short_relief_visit$PlayerName %in% closer_visit$PlayerName),]
          
          if(nrow(short_relief_visit) == 0)
          {
            new_closer_visit <- pitching_RP[(pitching_RP$MLBId %in% visit_pitcher),]
            
            new_closer_visit <- new_closer_visit[!(new_closer_visit$MLBId %in% pitching_line_visit$MLBId),]
            
            new_closer_visit <- new_closer_visit[new_closer_visit$IP %in% 1,]
            
            new_closer_visit <- new_closer_visit[1,]
            
            pitching_line_visit <- rbind(pitching_line_visit, new_closer_visit)
            
            short_relief_visit <- new_closer_visit
            
            for(i in 1:nrow(short_relief_visit))
            {
              inning <- (short_relief_visit$IP[i] %/% 1) + (((short_relief_visit$IP[i] %% 1) * (10/3)) / 9)
              
              short_relief_visit$ER[i] <- ifelse(round((blank_home$LW[2] * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) -  (blank_visit$LW[3] * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) - sum(short_relief_visit$LW[i], na.rm = TRUE) + (4.25 * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)),digits=0) < 0, short_relief_visit$ER[i] <- 0, short_relief_visit$ER[i] <- round((blank_home$LW[2] * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) -  (blank_visit$LW[3] * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) - sum(short_relief_visit$LW[i], na.rm = TRUE) + (4.25 * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)),digits=0))
              
            }
          }
          
          if(nrow(short_relief_visit) > 0)
          {
            for(i in 1:nrow(short_relief_visit))
            {
              inning <- (short_relief_visit$IP[i] %/% 1) + (((short_relief_visit$IP[i] %% 1) * (10/3)) / 9)
              
              short_relief_visit$ER[i] <- ifelse(round((blank_home$LW[2] * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) -  (blank_visit$LW[3] * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) - sum(short_relief_visit$LW[i], na.rm = TRUE) + (4.25 * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)),digits=0) < 0, short_relief_visit$ER[i] <- 0, short_relief_visit$ER[i] <- round((blank_home$LW[2] * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) -  (blank_visit$LW[3] * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) - sum(short_relief_visit$LW[i], na.rm = TRUE) + (4.25 * (((sum(short_relief_visit$IP[i], na.rm = TRUE) %/% 1) + (((sum(short_relief_visit$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)),digits=0))
              
            }
            
            short_relief_visit <- short_relief_visit[order(short_relief_visit$GameDate, decreasing = TRUE),]
            
            short_relief_visit <- short_relief_visit[1,]
            
            pitching_line_visit <- rbind(pitching_line_visit, short_relief_visit)
            
            pitching_line_visit <- rbind(pitching_line_visit, closer_visit)
            
            closer_visit <- rbind(short_relief_visit, closer_visit)
            
            short_relief_visit <- closer_visit
            
            
          }
          
        }
      }
      #
      
      count <- score$Out[((score$Side == "Bottom") & (score$Inning == 9))]
      
      for(i in 1:nrow(short_relief_visit))
      {
        if(short_relief_visit$OUT[i] == 2)
        {
          score$Pit[c(min(count[score$Pit[count] %in% c("",NA)]),(min(count[score$Pit[count] %in% c("",NA)]) + 1))] <- as.character(short_relief_visit$PlayerName[i])
        }
        
        if(short_relief_visit$OUT[i] == 1)
        {
          score$Pit[c(min(count[score$Pit[count] %in% c("",NA)]))] <- as.character(short_relief_visit$PlayerName[i])
          
        }
        
        if(short_relief_visit$OUT[i] == 3)
        {
          score$Pit[c(min(count[score$Pit[count] %in% c("",NA)]),(min(count[score$Pit[count] %in% c("",NA)]) + 1),(min(count[score$Pit[count] %in% c("",NA)]) + 2))] <- as.character(short_relief_visit$PlayerName[i])
        }
      }
      
      for(i in 1:nrow(short_relief_visit))
      {
        runs <- short_relief_visit$ER[i]
        ifelse(runs > 0, runs <- runs, runs <- 0)
        sampling <- which(score$Pit %in% short_relief_visit$PlayerName[i])
        sampling <- as.numeric(sampling)
        
        if(runs > 0)
        {
          
          if(length(sampling) == 1)
          {
            run_slot <- sampling
            score$Add_score[run_slot] <- runs
          }
          
          if(length(sampling) > 1)
          {
            run_slot <- sample(sampling, size = 1, replace = FALSE)
            score$Add_score[run_slot] <- runs
          }
        }
        
        if(runs == 0)
        {
          print("Run is zero")
          score$Add_score[sampling] <- 0
        }
        
      }
      
      score$Add_score[which(score$Add_score %in% c(NA,""))] <- 0
      
      score$`V-Score` <- as.numeric(score$`V-Score`)
      score$`H-Score` <- as.numeric(score$`H-Score`)
      score$Add_score <- as.numeric(score$Add_score)
      
      for(i in 1:length(count))
      {
        score$`H-Score`[count[i]] <- score$`H-Score`[count[i]-1] + score$Add_score[count[i]]
        score$`V-Score`[count[i]] <- score$`V-Score`[count[i]-1]
      }
    }
  }
  
  # If visiting team is NOT winning by 1 to 3 runs by end of top 9th, run this:
  
  if((score$`V-Score`[which((score$Side == "Top") & (score$Inning == 9) & (score$Out_count == 2))] - score$`H-Score`[which((score$Side == "Top") & (score$Inning == 9) & (score$Out_count == 2))]) > 3)
  {
    
    if(no_reliever_away == "YES")
    {
      print("No Reliever Needed")
    }
    
    if(!(no_reliever_away == "YES")){
      pitching_away_9th <- lineup$MLBId[lineup$Team %in% final_schedule$Away[x]]
      
      away_relief_9th <- pitching_RP[pitching_RP$MLBId %in% pitching_away_9th,]
      
      away_relief_9th <- away_relief_9th[!(away_relief_9th$MLBId %in% pitching_line_visit$MLBId),]
      
      CONTINUE <- TRUE
      
      count3 <- 0
      
      while(CONTINUE == TRUE)
      {
        count3 <- count3 + 1
        
        sample_relief_away <- away_relief_9th[sample(nrow(away_relief_9th), size = c(1), replace = FALSE),]
        
        sample_relief_away$OUT <- (sample_relief_away$IP %/% 1) * 3 + ((sample_relief_away$IP %% 1) * 10)
        
        number <- sum(sample_relief_away$OUT, na.rm = TRUE)
        
        if((nrow(sample_relief_away) == 1) & (number == 3))
        {
          CONTINUE <- FALSE
        }
        
        if((nrow(sample_relief_away) == 2) & (sample_relief_away$PlayerName[1] != sample_relief_away$PlayerName[2]) & (number == 3))
        {
          CONTINUE <- FALSE
        }
        
        if(count3 == 1000)
        {
          CONTINUE2 <- TRUE
          
          while(CONTINUE2 == TRUE)
          {
            sample_relief_away <- away_relief_9th[sample(nrow(away_relief_9th), size = c(1), replace = FALSE),]
            
            sample_relief_away$OUT <- (sample_relief_away$IP %/% 1) * 3 + ((sample_relief_away$IP %% 1) * 10)
            
            number <- sum(sample_relief_away$OUT, na.rm = TRUE)
            
            if(number %in% c(1,2,4,5))
            {
              sample_relief_away <- sample_relief_away[order(sample_relief_away$GameDate, decrease = TRUE),]
              
              sample_relief_away <- sample_relief_away[1,]
              
              sample_relief_away$IP[1] <- 1
              
              sample_relief_away$OUT[1] <- 3
              
              CONTINUE <- FALSE
              CONTINUE2 <- FALSE
            }
            
          }
        }
        
      }
      
      for(i in 1:nrow(sample_relief_away))
      {
        inning <- (sample_relief_away$IP[i] %/% 1) + (((sample_relief_away$IP[i] %% 1) * (10/3)) / 9)
        
        sample_relief_away$ER[i] <- ifelse(round((blank_home$LW[2] * (((sum(sample_relief_away$IP[i], na.rm = TRUE) %/% 1) + (((sum(sample_relief_away$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) -  (blank_visit$LW[3] * (((sum(sample_relief_away$IP[i], na.rm = TRUE) %/% 1) + (((sum(sample_relief_away$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) - sum(sample_relief_away$LW[i], na.rm = TRUE) + (4.25 * (((sum(sample_relief_away$IP[i], na.rm = TRUE) %/% 1) + (((sum(sample_relief_away$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)),digits=0) < 0, sample_relief_away$ER[i] <- 0, sample_relief_away$ER[i] <- round((blank_home$LW[2] * (((sum(sample_relief_away$IP[i], na.rm = TRUE) %/% 1) + (((sum(sample_relief_away$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) -  (blank_visit$LW[3] * (((sum(sample_relief_away$IP[i], na.rm = TRUE) %/% 1) + (((sum(sample_relief_away$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)) - sum(sample_relief_away$LW[i], na.rm = TRUE) + (4.25 * (((sum(sample_relief_away$IP[i], na.rm = TRUE) %/% 1) + (((sum(sample_relief_away$IP[i], na.rm = TRUE) %% 1) * 10) / 3)) /9)),digits=0))
        
      }
      
      
      pitching_line_visit <- rbind(pitching_line_visit, sample_relief_away) 
      
      count <- score$Out[((score$Side == "Bottom") & (score$Inning == 9))]
      
      for(i in 1:nrow(sample_relief_away))
      {
        if(sample_relief_away$OUT[i] == 2)
        {
          score$Pit[c(min(count[score$Pit[count] %in% c("",NA)]),(min(count[score$Pit[count] %in% c("",NA)]) + 1))] <- as.character(sample_relief_away$PlayerName[i])
          count <- count[3]
        }
        
        if(sample_relief_away$OUT[i] == 1)
        {
          score$Pit[c(min(count[score$Pit[count] %in% c("",NA)]))] <- as.character(sample_relief_away$PlayerName[i])
          count <- count[2:3]
        }
        
        if(sample_relief_away$OUT[i] == 3)
        {
          score$Pit[c(min(count[score$Pit[count] %in% c("",NA)]),(min(count[score$Pit[count] %in% c("",NA)]) + 1),(min(count[score$Pit[count] %in% c("",NA)]) + 2))] <- as.character(sample_relief_away$PlayerName[i])
        }
      }
      
      for(i in 1:nrow(sample_relief_away))
      {
        runs <- sample_relief_away$ER[i]
        ifelse(runs > 0, runs <- runs, runs <- 0)
        slot <- which(score$Pit %in% sample_relief_away$PlayerName[i])
        
        if(runs > 0)
        {
          run_slot <- sample(slot, size = 1, replace = FALSE)
          score$Add_score[run_slot] <- runs
        }
        
        if(runs == 0)
        {
          print("run is zero")
          score$Add_score[slot] <- 0
        }
      }
      
      
      
      score$Add_score[which(score$Add_score %in% c(NA,""))] <- 0
      
      for(i in 1:length(count))
      {
        score$`H-Score`[count[i]] <- score$`H-Score`[count[i]-1] + as.double(score$Add_score[count[i]])
        score$`V-Score`[count[i]] <- score$`V-Score`[count[i]-1]
      }
    }
  }
  
}


# If visitors win after bottom 9th or tied, conclude games here:
if(score$`V-Score`[which((score$Inning %in% 9) &  (score$Side %in% "Top") & (score$Out_count %in% 2))] >= score$`H-Score`[which((score$Inning %in% 9) &  (score$Side %in% "Top") & (score$Out_count %in% 2))])
{
  
  # Match pitchers with fielding record (visit)
  
  blank <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_home)))
  
  colnames(blank) <- colnames(box_stat_visit)
  
  blank$GameDate <- as.Date(blank$GameDate, format= "%Y-%m-%d")
  
  box_stat_visit <- rbind(box_stat_visit, blank)
  
  for(i in 1:nrow(pitching_line_visit))
  {
    
    pitch_fielder_visit <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_home)))
    
    colnames(pitch_fielder_visit) <- colnames(box_stat_visit)
    
    pitch_fielder_visit$LastName <- as.character(pitch_fielder_visit$LastName)
    
    pitch_fielder_visit$FirstName <- as.character(pitch_fielder_visit$FirstName)
    
    pitch_fielder_visit$GameDate <- as.Date(pitch_fielder_visit$GameDate, format = "%Y-%m-%d")
    
    pitch_fielder_visit$Field <- as.double(pitch_fielder_visit$Field)
    
    pitch_fielder_visit$E <- as.double(pitch_fielder_visit$E)
    
    pitch_fielder_visit$Zone <- as.double(pitch_fielder_visit$Zone)
    
    fielding_available$MLBId <- as.character(fielding_available$MLBId)
    
    pitching_line_visit$GameDate <- as.Date(pitching_line_visit$GameDate, format = "%Y-%m-%d")
    
    pitch_fielder_visit$LastName[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)) > 0,pitch_fielder_visit$LastName[1] <- as.character(fielding_available2$LastName[which(((fielding_available$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)]), (pitch_fielder_visit$LastName[1] <- as.character(pitching_line_visit$LastName[i])))
    
    pitch_fielder_visit$FirstName[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)) > 0,pitch_fielder_visit$FirstName[1] <- as.character(fielding_available2$FirstName[which(((fielding_available$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)]), (pitch_fielder_visit$FirstName[1] <- as.character(pitching_line_visit$FirstName[i])))
    
    pitch_fielder_visit$GameDate[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)) > 0,pitch_fielder_visit$GameDate[1] <- as.character(fielding_available2$GameDate[which(((fielding_available$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)]), (pitch_fielder_visit$GameDate[1] <- as.character(pitching_line_visit$GameDate[i])))
    
    pitch_fielder_visit$Field[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)) > 0, pitch_fielder_visit$Field[1] <- as.character(fielding_available2$LW[which(((fielding_available$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)]), (pitch_fielder_visit$Field[1] <- as.double(0)))
    
    pitch_fielder_visit$E[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)) > 0, pitch_fielder_visit$E[1] <- as.character(fielding_available2$E[which(((fielding_available2$MLBId %in% pitching_line_visit$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_visit$GameDate[i])) == TRUE)]), (pitch_fielder_visit$E[1] <- as.double(0)))
    
    YTD$MLBId <- as.character(YTD$MLBId)
    
    ytd_stat <- YTD[(YTD$MLBId %in% pitching_line_visit$MLBId[i]),]
    
    ytd_stat <- unique(ytd_stat)
    
    ytd_stat$Pos <- as.character(ytd_stat$Pos)
    
    pitch_fielder_visit$Zone[1] <- ytd_stat$Zone[1]
    
    box_stat_visit <- rbind(box_stat_visit, pitch_fielder_visit)
    
    blank_visit$Zone <- as.double(blank_visit$Zone)
    
    if(blank_visit$Zone[1] %in% NA)
    {
      blank_visit$Zone[1] <- 0
    }
    
    blank_visit$Zone[1] <- blank_visit$Zone[1] + ytd_stat$Zone[1]
    
    
  }
  
  blank_visit$Zone <- as.character(blank_visit$Zone)
  
  # Update the overall offense, and overall defense
  
  box_stat_visit$LW <- as.numeric(box_stat_visit$LW)
  box_stat_visit$Bonus <- as.numeric(box_stat_visit$Bonus)
  box_stat_visit$Bases_Taken <- as.numeric(box_stat_visit$Bases_Taken)
  box_stat_visit$Outs_on_Base <- as.numeric(box_stat_visit$Outs_on_Base)
  box_stat_visit$Field <- as.numeric(box_stat_visit$Field)
  box_stat_visit$E <- as.integer(box_stat_visit$E)
  box_stat_visit$Zone <- as.numeric(box_stat_visit$Zone)
  
  
  blank_visit$LW[1] <- sum(box_stat_visit$LW, na.rm = TRUE)
  blank_visit$Bonus[1] <- sum(box_stat_visit$Bonus, na.rm = TRUE)
  blank_visit$Bases_Taken[1] <- sum(box_stat_visit$Bases_Taken, na.rm = TRUE)
  blank_visit$Outs_on_Base[1] <- sum(box_stat_visit$Outs_on_Base, na.rm = TRUE)
  
  if(away_sp_stat$MLBId[1] %in% box_stat_visit$MLBId)
  {
    ifelse(box_stat_visit$Field[which(box_stat_visit$MLBId %in% away_sp_stat$MLBId[1])] %in% NA, blank_visit$Field[1] <- sum(box_stat_visit$Field, na.rm = TRUE),blank_visit$Field[1] <- sum(box_stat_visit$Field, na.rm = TRUE) - box_stat_visit$Field[which(box_stat_visit$MLBId %in% away_sp_stat$MLBId[1])])
    ifelse(box_stat_visit$E[which(box_stat_visit$MLBId %in% away_sp_stat$MLBId[1])] %in% NA, blank_visit$E[1] <- sum(box_stat_visit$E, na.rm = TRUE),blank_visit$E[1] <- sum(box_stat_visit$E, na.rm = TRUE) - box_stat_visit$E[which(box_stat_visit$MLBId %in% away_sp_stat$MLBId[1])])
    ifelse(box_stat_visit$Zone[which(box_stat_visit$MLBId %in% away_sp_stat$MLBId[1])] %in% NA, blank_visit$Zone[1] <- sum(box_stat_visit$Zone, na.rm = TRUE),blank_visit$Zone[1] <- sum(box_stat_visit$Zone, na.rm = TRUE) - box_stat_visit$Zone[which(box_stat_visit$MLBId %in% away_sp_stat$MLBId[1])])
    
    blank_visit$LW[2] <- as.numeric(blank_visit$LW[1]) + as.numeric(blank_visit$Bonus[1]) + as.numeric(blank_visit$Bases_Taken[1]) + as.numeric(blank_visit$Outs_on_Base[1])
    
    blank_visit$LW[3] <- as.numeric(blank_visit$Field[1]) + as.numeric(blank_visit$Zone[1]) + as.numeric(blank_visit$Block[1]) + as.numeric(blank_visit$Frame[1])
    
  }
  
  if(!away_sp_stat$MLBId[1] %in% box_stat_visit$MLBId)
  {
    blank_visit$Field[1] <- sum(box_stat_visit$Field, na.rm = TRUE)
    blank_visit$E[1] <- sum(box_stat_visit$E, na.rm = TRUE)
    blank_visit$Zone[1] <- sum(box_stat_visit$Zone, na.rm = TRUE)
    
    blank_visit$LW[2] <- as.numeric(blank_visit$LW[1]) + as.numeric(blank_visit$Bonus[1]) + as.numeric(blank_visit$Bases_Taken[1]) + as.numeric(blank_visit$Outs_on_Base[1])
    
    blank_visit$LW[3] <- as.numeric(blank_visit$Field[1]) + as.numeric(blank_visit$Zone[1]) + as.numeric(blank_visit$Block[1]) + as.numeric(blank_visit$Frame[1])
    
  }
  
  
  box_stat_visit$H <- as.numeric(box_stat_visit$H)
  
  blank_visit$H[1] <- sum(box_stat_visit$H, na.rm = TRUE)
  
  # Match pitchers with fielding record (home)
  
  
  for(i in 1:nrow(pitching_line_home))
  {
    pitch_fielder_home <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_home)))
    
    colnames(pitch_fielder_home) <- colnames(box_stat_home)
    
    pitch_fielder_home$LastName <- as.character(pitch_fielder_home$LastName)
    
    pitch_fielder_home$FirstName <- as.character(pitch_fielder_home$FirstName)
    
    pitch_fielder_home$GameDate <- as.Date(pitch_fielder_home$GameDate, format = "%Y-%m-%d")
    
    pitch_fielder_home$Field <- as.double(pitch_fielder_home$Field)
    
    pitch_fielder_home$E <- as.double(pitch_fielder_home$E)
    
    pitch_fielder_home$Zone <- as.double(pitch_fielder_home$Zone)
    
    
    fielding_available$MLBId <- as.character(fielding_available$MLBId)
    
    pitching_line_home$GameDate <- as.Date(pitching_line_home$GameDate, format = "%Y-%m-%d")
    
    pitch_fielder_home$LastName[1] <- ifelse(length(which(((fielding_available$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)) > 0,pitch_fielder_home$LastName[1] <- as.character(fielding_available$LastName[which(((fielding_available$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)]), (pitch_fielder_home$LastName[1] <- as.character(pitching_line_home$LastName[i])))
    
    pitch_fielder_home$FirstName[1] <- ifelse(length(which(((fielding_available$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)) > 0,pitch_fielder_home$FirstName[1] <- as.character(fielding_available$FirstName[which(((fielding_available$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)]), (pitch_fielder_home$FirstName[1] <- as.character(pitching_line_home$FirstName[i])))
    
    pitch_fielder_home$GameDate[1] <- ifelse(length(which(((fielding_available$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)) > 0,pitch_fielder_home$GameDate[1] <- as.character(fielding_available$GameDate[which(((fielding_available$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)]), (pitch_fielder_home$GameDate[1] <- as.character(pitching_line_home$GameDate[i])))
    
    pitch_fielder_home$Field[1] <- ifelse(length(which(((fielding_available$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)) > 0, pitch_fielder_home$Field[1] <- as.character(fielding_available$LW[which(((fielding_available$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)]), (pitch_fielder_home$Field[1] <- as.double(0)))
    
    pitch_fielder_home$E[1] <- ifelse(length(which(((fielding_available2$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)) > 0, pitch_fielder_home$E[1] <- as.character(fielding_available2$E[which(((fielding_available2$MLBId %in% pitching_line_home$MLBId[i]) & (fielding_available2$GameDate %in% pitching_line_home$GameDate[i])) == TRUE)]), (pitch_fielder_home$E[1] <- as.double(0)))
    
    YTD$MLBId <- as.character(YTD$MLBId)
    
    ytd_stat <- YTD[(YTD$MLBId %in% pitching_line_home$MLBId[i]),]
    
    ytd_stat <- unique(ytd_stat)
    
    ytd_stat$Pos <- as.character(ytd_stat$Pos)
    
    pitch_fielder_home$Zone[1] <- ytd_stat$Zone[1]
    
    box_stat_home <- rbind(box_stat_home, pitch_fielder_home)
    
    blank_home$Zone <- as.double(blank_home$Zone)
    
    blank_home$Zone[1] <- blank_home$Zone[1] + ytd_stat$Zone[1]
    
    
  }
  
  blank_home$Zone <- as.character(blank_home$Zone)
  
  box_stat_home$LW <- as.numeric(box_stat_home$LW)
  box_stat_home$Bonus <- as.numeric(box_stat_home$Bonus)
  box_stat_home$Bases_Taken <- as.numeric(box_stat_home$Bases_Taken)
  box_stat_home$Outs_on_Base <- as.numeric(box_stat_home$Outs_on_Base)
  box_stat_home$Field <- as.numeric(box_stat_home$Field)
  box_stat_home$E <- as.integer(box_stat_home$E)
  box_stat_home$Zone <- as.numeric(box_stat_home$Zone)
  
  blank_home$LW[1] <- sum(box_stat_home$LW, na.rm = TRUE)
  blank_home$Bonus[1] <- sum(box_stat_home$Bonus, na.rm = TRUE)
  blank_home$Bases_Taken[1] <- sum(box_stat_home$Bases_Taken, na.rm = TRUE)
  blank_home$Outs_on_Base[1] <- sum(box_stat_home$Outs_on_Base, na.rm = TRUE)
  
  if(home_sp_stat$MLBId[1] %in% box_stat_home$MLBId)
  {
    ifelse(box_stat_home$Field[which(box_stat_home$MLBId %in% home_sp_stat$MLBId[1])] %in% NA, blank_home$Field[1] <- sum(box_stat_home$Field, na.rm = TRUE),blank_home$Field[1] <- sum(box_stat_home$Field, na.rm = TRUE) - box_stat_home$Field[which(box_stat_home$MLBId %in% home_sp_stat$MLBId[1])])
    ifelse(box_stat_home$E[which(box_stat_home$MLBId %in% home_sp_stat$MLBId[1])] %in% NA, blank_home$E[1] <- sum(as.numeric(box_stat_home$E, na.rm = TRUE)),blank_home$E[1] <- sum(as.numeric(box_stat_home$E), na.rm = TRUE) - box_stat_home$E[which(box_stat_home$MLBId %in% home_sp_stat$MLBId[1])])
    ifelse(box_stat_home$Zone[which(box_stat_home$MLBId %in% home_sp_stat$MLBId[1])] %in% NA, blank_home$Zone[1] <- sum(box_stat_home$Zone, na.rm = TRUE),blank_home$Zone[1] <- sum(box_stat_home$Zone, na.rm = TRUE) - box_stat_home$Zone[which(box_stat_home$MLBId %in% home_sp_stat$MLBId[1])])
    
    blank_home$LW[2] <- as.numeric(blank_home$LW[1]) + as.numeric(blank_home$Bonus[1]) + as.numeric(blank_home$Bases_Taken[1]) + as.numeric(blank_home$Outs_on_Base[1])
    
    blank_home$LW[3] <- as.numeric(blank_home$Field[1]) + as.numeric(blank_home$Zone[1]) + as.numeric(blank_home$Block[1]) + as.numeric(blank_home$Frame[1])
    
  }
  
  if(!home_sp_stat$MLBId[1] %in% box_stat_home$MLBId)
  {
    blank_home$Field[1] <- sum(box_stat_home$Field, na.rm = TRUE)
    blank_home$E[1] <- sum(as.numeric(box_stat_home$E), na.rm = TRUE)
    blank_home$Zone[1] <- sum(box_stat_home$Zone, na.rm = TRUE)
    
    blank_home$LW[2] <- as.numeric(blank_home$LW[1]) + as.numeric(blank_home$Bonus[1]) + as.numeric(blank_home$Bases_Taken[1]) + as.numeric(blank_home$Outs_on_Base[1])
    
    blank_home$LW[3] <- as.numeric(blank_home$Field[1]) + as.numeric(blank_home$Zone[1]) + as.numeric(blank_home$Block[1]) + as.numeric(blank_home$Frame[1])
    
  }
  
  box_stat_home$H <- as.numeric(box_stat_home$H)
  
  blank_home$H[1] <- sum(box_stat_home$H, na.rm = TRUE)
  
  
  score$`V-Score`[52] <- score$`V-Score`[51] + score$Add_score[52]
  score$`H-Score`[52] <- score$`H-Score`[51] + score$Add_score[52]
  
  score$`V-Score`[53] <- score$`V-Score`[52] + score$Add_score[53]
  score$`H-Score`[53] <- score$`H-Score`[52] + score$Add_score[53]
  
  score$`V-Score`[54] <- score$`V-Score`[53] + score$Add_score[54]
  score$`H-Score`[54] <- score$`H-Score`[53] + score$Add_score[54]
  
  ### Calculate winning_team, lead_by, winning pit, losing pit
  
  if(score$`V-Score`[nrow(score)] == score$`H-Score`[nrow(score)])
  {
    home_tie_breaker <- blank_home$LW[2] + blank_home$LW[3] + sum(pitching_line_home$LW, na.rm = TRUE)
    visit_tie_breaker <- blank_visit$LW[2] + blank_visit$LW[3] + sum(pitching_line_visit$LW, na.rm = TRUE)
    
    if(home_tie_breaker > visit_tie_breaker){
      
      visiter_pitcher_name <- pitching_line_visit$PlayerName
      
      extra_run <- which(score$Pit  %in% (pitching_line_visit$PlayerName[which(pitching_line_visit$LW == min(pitching_line_visit$LW))]))
      
      chosen_slot <- sample(extra_run, size = 1, replace = FALSE)
      
      score$Add_score[chosen_slot] <- score$Add_score[chosen_slot] + 1
      
      score$`V-Score`[chosen_slot:nrow(score)] <- ""
      
      score$`H-Score`[chosen_slot:nrow(score)] <- ""
      
      for(i in chosen_slot:nrow(score))
      {
        score$`H-Score` <- as.numeric(score$`H-Score`)
        score$`V-Score` <- as.numeric(score$`V-Score`)
        
        
        if(score$Side[i] == "Bottom")
        {
          score$`H-Score`[i] <- score$`H-Score`[i-1] + score$Add_score[i]
          score$`V-Score`[i] <- score$`V-Score`[i-1]
        }
        
        if(score$Side[i] == "Top")
        {
          score$`V-Score`[i] <- score$`V-Score`[i-1] + score$Add_score[i]
          score$`H-Score`[i] <- score$`H-Score`[i-1]
        }
      }
      
      pitching_line_visit$ER[which(pitching_line_visit$PlayerName %in% pitching_line_visit$PlayerName[which(pitching_line_visit$LW == min(pitching_line_visit$LW))])] <- pitching_line_visit$ER[which(pitching_line_visit$PlayerName %in% pitching_line_visit$PlayerName[which(pitching_line_visit$LW == min(pitching_line_visit$LW))])] + 1
      
      if(score$`V-Score`[which(score$Out == 51)] < score$`H-Score`[which(score$Out == 51)])
      {
        score <- score[1:51,]
      }
      
      pitching_line_visit <- pitching_line_visit[(pitching_line_visit$PlayerName %in% unique(score$Pit[score$Side %in% "Bottom"])),]
      box_stat_visit$PlayerName <- paste(box_stat_visit$FirstName,box_stat_visit$LastName,sep=" ")
      
      remove_visitor <- visiter_pitcher_name[!(visiter_pitcher_name %in% pitching_line_visit$PlayerName)]
      
      box_stat_visit <- box_stat_visit[!(box_stat_visit$PlayerName %in% remove_visitor),]
    }
    
    if(visit_tie_breaker > home_tie_breaker){
      
      extra_run <- which(score$Pit  %in% (pitching_line_home$PlayerName[which(pitching_line_home$LW == min(pitching_line_home$LW))]))
      
      chosen_slot <- sample(extra_run, size = 1, replace = FALSE)
      
      score$Add_score[chosen_slot] <- score$Add_score[chosen_slot] + 1
      
      score$`V-Score`[chosen_slot:nrow(score)] <- ""
      
      score$`H-Score`[chosen_slot:nrow(score)] <- ""
      
      for(i in chosen_slot:nrow(score))
      {
        score$`H-Score` <- as.numeric(score$`H-Score`)
        score$`V-Score` <- as.numeric(score$`V-Score`)
        
        
        if(score$Side[i] == "Bottom")
        {
          score$`H-Score`[i] <- score$`H-Score`[i-1] + score$Add_score[i]
          score$`V-Score`[i] <- score$`V-Score`[i-1]
        }
        
        if(score$Side[i] == "Top")
        {
          score$`V-Score`[i] <- score$`V-Score`[i-1] + score$Add_score[i]
          score$`H-Score`[i] <- score$`H-Score`[i-1]
        }
      }
      
      pitching_line_home$ER[which(pitching_line_home$PlayerName %in% pitching_line_home$PlayerName[which(pitching_line_home$LW == min(pitching_line_home$LW))])] <- pitching_line_home$ER[which(pitching_line_home$PlayerName %in% pitching_line_home$PlayerName[which(pitching_line_home$LW == min(pitching_line_home$LW))])] + 1
      
      
      box_stat_home$PlayerName <- paste(box_stat_home$FirstName,box_stat_home$LastName,sep=" ")
      
    }
  }
  
  # Winning
  
  for(i in 1:nrow(score))
  {
    
    if((score$`V-Score`[i] - score$`H-Score`[i]) > 0)
    {
      score$Winning_Team[i] <- score$Visit[i]
    }
    
    if((score$`H-Score`[i] - score$`V-Score`[i]) > 0)
    {
      score$Winning_Team[i] <- score$Home[i]
    }
    
    if(score$`V-Score`[i] - score$`H-Score`[i] == 0)
    {
      score$Winning_Team[i] <- "Tie"
    }
    
  }
  
  # Who won and lost?
  
  condition <- TRUE
  
  i <- nrow(score)
  
  while(condition == TRUE)
  {
    i <- i - 1
    print(i)
    condition <- identical(score$Winning_Team[nrow(score)], score$Winning_Team[i])
    
    if(condition == FALSE)
    {
      score$Losing_Pit[(i+1):nrow(score)] <- score$Pit[i+1]
      break;
    }
  }
  
  side <- c("Top","Bottom")
  
  if(score$Inning[i] > 1){
    winning_pitcher <- score$Pit[max(which((score$Side == side[!(side %in% score$Side[i+1])]) & (score$Out < (i+1))))]
    
  }
  
  if(score$Inning[i] == 1){
    winning_pitcher <- score$Pit[min(which((score$Side == side[!(side %in% score$Side[i+1])]) & (score$Out > (i+1))))]
    
  }
  
  losing_pitcher <- score$Pit[i+1]
  
  # Calculate 'Lead_by'
  
  for(i in 1:nrow(score))
  {
    if(score$Winning_Team[i] %in% final_schedule$Home[x])
    {
      score$Lead_By[i] <- score$`H-Score`[i] - score$`V-Score`[i]
    }
    
    if(score$Winning_Team[i] %in% final_schedule$Away[x])
    {
      score$Lead_By[i] <- score$`V-Score`[i] - score$`H-Score`[i]
      
    }
    
    if(score$Winning_Team[i] %in% "Tie")
    {
      score$Lead_By[i] <- 0
    }
  }
  
  
  # Assign decisions on pitchers. 
  
  for(i in 1:nrow(pitching_line_home))
  {
    if(pitching_line_home$PlayerName[i] == winning_pitcher)
    {
      pitching_line_home$DEC[i] <- "W"
    }
    
    if(pitching_line_home$PlayerName[i] == losing_pitcher)
    {
      pitching_line_home$DEC[i] <- "L"
    }
  }
  
  for(i in 1:nrow(pitching_line_visit))
  {
    if(pitching_line_visit$PlayerName[i] == winning_pitcher)
    {
      pitching_line_visit$DEC[i] <- "W"
    }
    
    if(pitching_line_visit$PlayerName[i] == losing_pitcher)
    {
      pitching_line_visit$DEC[i] <- "L"
    }
  }
  
  save_sit <- score$Out[nrow(score)]
  closing_pitcher <- score$Pit[save_sit]
  
  if(score$Lead_By[save_sit] < 4)
  {
    if(pitching_line_home$DEC[nrow(pitching_line_home)] %in% c("W","L") | pitching_line_visit$DEC[nrow(pitching_line_visit)] %in% c("W","L"))
    {
      print("Not a save situation")
    }
    
    if(!(pitching_line_home$DEC[nrow(pitching_line_home)] %in% c("W","L")) | !(pitching_line_visit$DEC[nrow(pitching_line_visit)] %in% c("W","L"))){
      
      if(nrow(pitching_line_home) > 1){
        
        
        for(i in 1:nrow(pitching_line_home))
        {
          if(pitching_line_home$PlayerName[i] == closing_pitcher)
          {
            pitching_line_home$DEC[i] <- "SV"
          }
          
        }
      }
      
      if(nrow(pitching_line_home) == 1){
        print("Can't and won't assign SV")
      }
      
      
      if(nrow(pitching_line_visit) > 1){
        for(i in 1:nrow(pitching_line_visit))
        {
          if(pitching_line_visit$PlayerName[i] == closing_pitcher)
          {
            pitching_line_visit$DEC[i] <- "SV"
          }
          
        }
      }
      
      if(nrow(pitching_line_visit) == 1){
        print("Can't and won't assign SV")
      }
      
    }
  }
  
  if("SV" %in% pitching_line_home$DEC)
  {
    num_of_pitchers <- 1:nrow(pitching_line_home)
    
    hold_sit <- which(!(num_of_pitchers %in% which(pitching_line_home$DEC %in% c("W","SV"))))
    
    hold_sit <- hold_sit[!(hold_sit < which(pitching_line_home$DEC == "W"))]
    
    pitching_line_home$DEC[hold_sit] <- "HD"
  }
  
  if("SV" %in% pitching_line_visit$DEC)
  {
    num_of_pitchers <- 1:nrow(pitching_line_visit)
    
    hold_sit <- which(!(num_of_pitchers %in% which(pitching_line_visit$DEC %in% c("W","SV"))))
    
    hold_sit <- hold_sit[!(hold_sit < which(pitching_line_visit$DEC == "W"))]
    
    pitching_line_visit$DEC[hold_sit] <- "HD"
  }
  
  # Paste batting and pitching lines together. (For both teams)
  
  #HOME
  
  blank_home$GameDate <- as.Date(blank_home$GameDate, format = "%Y-%m-%d")
  
  box_stat_home <- rbind(box_stat_home, blank_home)
  
  blank <- data.frame(matrix("", nrow = 1, ncol = ncol(box_stat_home)))
  
  colnames(blank) <- colnames(box_stat_home)
  
  blank$GameDate <- as.Date(blank$GameDate, format = "%Y-%m-%d")
  
  box_stat_home <- rbind(box_stat_home, blank)
  
  label <- read.csv("label.csv", header = TRUE)
  
  label$POS <- "POS"
  
  label <- label[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                    "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                    "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
  
  label[,2] <- as.Date(label[,2], format="%Y-%m-%d")
  
  
  box_stat_home <- rbind(box_stat_home, label)
  
  pitching_line_home$POS <- ""
  
  pitching_line_home <- pitching_line_home[,c("POS","GameDate","FirstName","LastName","LW","DEC","X1","X2","X3","X4","X5","X6",
                                              "X7","IP","BFP","H","X1B","X2B","X3B","HR","ER","SH","SF","HBP","BB","K","WP",
                                              "BLK","IR","IRS","GB","FB","LD","POPU","SB","CS","PKO","OUT","MLBId","PlayerName",
                                              "GameString","GameId","used","uniqueId")]
  
  if(nrow(pitching_line_home) == 1)
  {
    pitching_line_home$POS[1] <- "SP"
    
  }
  
  if(nrow(pitching_line_home) > 1)
  {
    pitching_line_home$POS[1] <- "SP"
    
    pitching_line_home$POS[2:nrow(pitching_line_home)] <- "RP"
  }
  
  colnames(pitching_line_home) <- colnames(box_stat_home)
  
  home_H <- round(((as.numeric(blank_home$H[1]) + sum(as.numeric(pitching_line_visit$H), na.rm = TRUE))) / 2,digits=0)
  home_H2 <- as.integer(home_H)
  home_R <- score$`H-Score`[nrow(score)]
  box_stat_home$E <- as.integer(box_stat_home$E)
  home_E <- blank_home$E[1]
  
  away_H <- round(((as.numeric(blank_visit$H[1]) + sum(as.numeric(pitching_line_home$H), na.rm = TRUE))) / 2,digits=0)
  away_H2 <- as.integer(away_H)
  away_R <- score$`V-Score`[nrow(score)]
  away_E <- blank_visit$E[1]
  
  for(i in 1:ncol(box_stat_home))
  {
    box_stat_home[,i] <- as.character(box_stat_home[,i])
  }
  
  for(i in 1:ncol(pitching_line_home))
  {
    pitching_line_home[,i] <- as.character(pitching_line_home[,i]) 
  }
  
  ###
  
  pitching_name_home <- unique(pitching_line_home$PlayerName)
  
  for(i in 1:length(pitching_name_home))
  {
    pitching_line_home$HR[i] <- sum(score$Add_score[which(score$Pit %in% pitching_name_home[i])], na.rm = TRUE)
  }
  
  residue_home <- away_R - sum(as.numeric(pitching_line_home$HR), na.rm = TRUE)
  
  pitching_line_home$HR[which(pitching_line_home$LW == min(pitching_line_home$LW))] <- as.numeric(pitching_line_home$HR[which(pitching_line_home$LW == min(pitching_line_home$LW))]) + residue_home
  
  box_stat_home$E <- as.integer(box_stat_home$E)
  
  E_home <- blank_home$E[1]
  
  if(E_home == 0)
  {
    print("Zero Error")
  }
  
  if(E_home > 0)
  {
    for(i in 1:E_home)
    {
      sampling <- sample(1:nrow(pitching_line_home), size = 1, replace = TRUE)
      
      if(pitching_line_home$HR[sampling] == 0)
      {
        print("Sampling is zero")
      }
      
      if(pitching_line_home$HR[sampling] > 0)
      {
        pitching_line_home$HR[sampling] <- as.numeric(pitching_line_home$HR[sampling]) - 1
      }
    }
  }
  
  
  ###
  
  box_stat_home <- rbind(box_stat_home, pitching_line_home)
  
  overall_pitching <- data.frame(matrix(NA, nrow = 1, ncol = ncol(box_stat_home)))
  
  colnames(overall_pitching) <- colnames(box_stat_home)
  
  overall_pitching$LastName[1] <- as.character("Overall Pitching")
  
  overall_pitching$LW[1] <- as.character(sum(as.numeric(pitching_line_home$LW), na.rm = TRUE))
  
  box_stat_home <- rbind(box_stat_home, overall_pitching)
  
  
  
  # VISIT
  
  blank_visit$GameDate <- as.Date(blank_visit$GameDate, format = "%Y-%m-%d")
  
  box_stat_visit <- rbind(box_stat_visit, blank_visit)
  
  blank <- data.frame(matrix("", nrow = 1, ncol = ncol(box_stat_visit)))
  
  colnames(blank) <- colnames(box_stat_visit)
  
  blank$GameDate <- as.Date(blank$GameDate, format = "%Y-%m-%d")
  
  box_stat_visit <- rbind(box_stat_visit, blank)
  
  label <- read.csv("label.csv", header = TRUE)
  
  label$POS <- "POS"
  
  label <- label[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                    "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                    "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
  
  label[,2] <- as.Date(label[,2], format="%Y-%m-%d")
  
  box_stat_visit <- rbind(box_stat_visit, label)
  
  pitching_line_visit$POS <- ""
  
  pitching_line_visit <- pitching_line_visit[,c("POS","GameDate","FirstName","LastName","LW","DEC","X1","X2","X3","X4","X5","X6",
                                                "X7","IP","BFP","H","X1B","X2B","X3B","HR","ER","SH","SF","HBP","BB","K","WP",
                                                "BLK","IR","IRS","GB","FB","LD","POPU","SB","CS","PKO","OUT","MLBId","PlayerName",
                                                "GameString","GameId","used","uniqueId")]
  
  if(nrow(pitching_line_visit) == 1)
  {
    pitching_line_visit$POS[1] <- "SP"
    
  }
  
  if(nrow(pitching_line_visit) > 1)
  {
    pitching_line_visit$POS[1] <- "SP"
    
    pitching_line_visit$POS[2:nrow(pitching_line_visit)] <- "RP"
  }
  
  colnames(pitching_line_visit) <- colnames(box_stat_visit)
  
  
  for(i in 1:ncol(box_stat_visit))
  {
    box_stat_visit[,i] <- as.character(box_stat_visit[,i])
  }
  
  for(i in 1:ncol(pitching_line_visit))
  {
    pitching_line_visit[,i] <- as.character(pitching_line_visit[,i]) 
  }
  ###
  
  pitching_name_visit <- unique(pitching_line_visit$PlayerName)
  
  for(i in 1:length(pitching_name_visit))
  {
    pitching_line_visit$HR[i] <- sum(score$Add_score[which(score$Pit %in% pitching_name_visit[i])], na.rm = TRUE)
  }
  
  
  residue_away <- home_R - sum(as.numeric(pitching_line_visit$HR), na.rm = TRUE)
  
  pitching_line_visit$HR[which(pitching_line_visit$LW == min(pitching_line_visit$LW))] <- as.numeric(pitching_line_visit$HR[which(pitching_line_visit$LW == min(pitching_line_visit$LW))]) + residue_away
  
  
  box_stat_visit$E <- as.integer(box_stat_visit$E)
  
  E_Visit <- blank_visit$E[1]
  
  if(E_Visit == 0)
  {
    print("Zero Error")
  }
  
  if(E_Visit > 0)
  {
    for(i in 1:E_Visit)
    {
      sampling <- sample(1:nrow(pitching_line_visit), size = 1, replace = TRUE)
      
      if(pitching_line_visit$HR[sampling] == 0)
      {
        print("Sampling is zero")
      }
      
      if(pitching_line_visit$HR[sampling] > 0)
      {
        pitching_line_visit$HR[sampling] <- as.numeric(pitching_line_visit$HR[sampling]) - 1
      }
    }
  }
  
  
  ###
  box_stat_visit <- rbind(box_stat_visit, pitching_line_visit)
  
  overall_pitching <- data.frame(matrix(NA, nrow = 1, ncol = ncol(box_stat_visit)))
  
  colnames(overall_pitching) <- colnames(box_stat_visit)
  
  overall_pitching$LastName[1] <- as.character("Overall Pitching")
  
  overall_pitching$LW[1] <- as.character(sum(as.numeric(pitching_line_visit$LW), na.rm = TRUE))
  
  box_stat_visit <- rbind(box_stat_visit, overall_pitching)
  
  box_stat_visit <- rbind(box_stat_visit, blank)
  
  # Paste home and away team together and you have yourself a boxscore.
  
  box_score <- rbind(box_stat_visit, box_stat_home)
  
  # Remove all NA
  for(i in 1:ncol(box_score))
  {
    box_score[which(box_score[,i] %in% NA),i] <- ""
  }
  
  final_score <- data.frame(matrix("", nrow = 3, ncol = ncol(box_stat_visit)))
  
  colnames(final_score) <- colnames(box_stat_visit)
  
  final_score$MLBId <- as.character(final_score$MLBId)
  
  final_score$PlayerName <- as.character(final_score$PlayerName)
  
  final_score$GameString <- as.character(final_score$GameString)
  
  final_score$GameId <- as.character(final_score$GameId)
  
  final_score$PlayerName[1] <- "R"
  
  final_score$GameString[1] <- "H"
  
  final_score$GameId[1] <- "E"
  
  final_score$MLBId[2] <- as.character(paste(final_schedule$Away[x]," at",sep=""))
  
  final_score$MLBId[3] <- final_schedule$Home[x]
  
  final_score$PlayerName[2] <- away_R
  
  final_score$PlayerName[3] <- home_R
  
  final_score$GameId[2] <- away_E
  
  final_score$GameId[3] <- home_E
  
  
  
  final_score <- rbind(final_score, box_score)
  
  final_score$POS <- as.character(final_score$POS)
  
  final_score$POS[which(final_score$POS == "1")] <- sub("1", "P", final_score$POS[which(final_score$POS == "1")])
  final_score$POS[which(final_score$POS == "2")] <- sub("2", "CA", final_score$POS[which(final_score$POS == "2")])
  final_score$POS[which(final_score$POS == "3")] <- sub("3", "1B", final_score$POS[which(final_score$POS == "3")])
  final_score$POS[which(final_score$POS == "4")] <- sub("4", "2B", final_score$POS[which(final_score$POS == "4")])
  final_score$POS[which(final_score$POS == "5")] <- sub("5", "3B", final_score$POS[which(final_score$POS == "5")])
  final_score$POS[which(final_score$POS == "6")] <- sub("6", "SS", final_score$POS[which(final_score$POS == "6")])
  final_score$POS[which(final_score$POS == "7")] <- sub("7", "LF", final_score$POS[which(final_score$POS == "7")])
  final_score$POS[which(final_score$POS == "8")] <- sub("8", "CF", final_score$POS[which(final_score$POS == "8")])
  final_score$POS[which(final_score$POS == "9")] <- sub("9", "RF", final_score$POS[which(final_score$POS == "9")])
  
  # Away H
  
  final_score$GameString[2] <- round((sum(as.integer(as.character(final_score$R[which((final_score$POS %in% c("SP","RP")) & (final_score$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Home[x])]))])), na.rm = TRUE) + sum(as.integer(as.character(final_score$H[which((final_score$POS %in% c("CA","1B","2B","3B","SS","LF","CF","RF","P","PH","DH")) & (final_score$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x])]))])), na.rm = TRUE)) / 2, digits=0)
  
  # Home H
  
  final_score$GameString[3] <- round((sum(as.integer(as.character(final_score$R[which((final_score$POS %in% c("SP","RP")) & (final_score$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Away[x])]))])), na.rm = TRUE) + sum(as.integer(as.character(final_score$H[which((final_score$POS %in% c("CA","1B","2B","3B","SS","LF","CF","RF","P","PH","DH")) & (final_score$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Home[x])]))])), na.rm = TRUE)) / 2, digits=0)
  
  
  write.csv(final_score, paste("box/",formatted_date,"/",formatted_date,final_schedule$Away[x],"@",final_schedule$Home[x],".csv",sep=""), row.names = FALSE)
  
  
}