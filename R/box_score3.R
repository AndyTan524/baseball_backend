library(dplyr)

# Read in entire player list

play_pos <- read.csv("playerid_with_pos.csv")
pos_sim <- read.csv("pos_sim.csv",header = TRUE)

# This must be updated on a daily basis so that it shows right players in RFB teams.

for(a in 1:ncol(play_pos))
{
  play_pos[,a] <- as.character(play_pos[,a])
}

# Switch

switch <- "No"

# Load in game schedule

final_schedule <- read.csv("final_schedule_test.csv")

for(a in 1:ncol(final_schedule))
{
  if(a == 1){final_schedule[,a] <- as.Date(final_schedule[,a],format="%Y-%m-%d")}
  else{final_schedule[,a] <- as.character(final_schedule[,a])}
}

# Set the game date.

date <- 20160612
date2 <- 20160613

# Get the formatted date

formatted_date <- "2016-06-12"

# Creating Directory for the day of game

ifelse(!dir.exists(paste(getwd(),"/box/",formatted_date,sep="")), dir.create(paste(getwd(),"/box/",formatted_date,sep="")),FALSE)

# Coerce 'formatted_date' as date

formatted_date <- as.Date(formatted_date,format="%Y-%m-%d")

# Subset the schedule to have only games that on the day of set date

final_schedule <- final_schedule[final_schedule$Date %in% formatted_date,]

# Exclude rain out games

final_schedule <- final_schedule[!final_schedule$Rain %in% "Yes",]

# Team abbreviations

NL <- c("ARI","ATL","CHN","CIN","COL","MIA","LAN","MIL","NYN","PHI","PIT","SD","SF","STL","WAS")
AL <- c("LAA","BAL","BOS","CHA","CLE","DET","HOU","KC","MIN","NYA","OAK","SEA","TB","TEX","TOR")

# Lineup from the Google Spreadsheet

lineup <- read.csv(paste("lineup",date,".csv",sep=""), header = FALSE)

# Set the colnames for 'lineup'

col_line <- c("Role","fullname","Date","POS","Team","MLBId","Fielding Petition")

colnames(lineup) <- col_line

lineup <- lineup[!(lineup$fullname %in% "fullname"),]

lineup <- lineup[!(lineup$fullname %in% ""),]

# Coerce the columns into things accordingly.

lineup$Role <- as.character(lineup$Role)
lineup$fullname <- as.character(lineup$fullname)
lineup$POS <- as.character(lineup$POS)
lineup$Team <- as.character(lineup$Team)
lineup$MLBId <- as.character(lineup$MLBId)
lineup$Date <- as.Date(lineup$Date, format="%Y-%m-%d")

lineup$POS2 <- ""

lineup$POS2[which(lineup$POS == "1B")] <- 3
lineup$POS2[which(lineup$POS == "2B")] <- 4
lineup$POS2[which(lineup$POS == "3B")] <- 5
lineup$POS2[which(lineup$POS == "SS")] <- 6
lineup$POS2[which(lineup$POS == "LF")] <- 7
lineup$POS2[which(lineup$POS == "CF")] <- 8
lineup$POS2[which(lineup$POS == "RF")] <- 9
lineup$POS2[which(lineup$POS == "SP")] <- 1
lineup$POS2[which(lineup$POS == "RP")] <- 1
lineup$POS2[which(lineup$POS == "P")] <- 1
lineup$POS2[which(lineup$POS == "CA")] <- 2
lineup$POS2[which(lineup$POS == "DH")] <- "DH"

# Fill Fielding Petition with "NO" If there is no "YES"

lineup$`Fielding Petition` <- ifelse(lineup$`Fielding Petition` %in% c("Yes","YES","yes","YEs","yEs","yES"),lineup$`Fielding Petition` <- "YES",lineup$`Fielding Petition` <- "NO")

# Player list

only_active_players <- play_pos[(play_pos$Team_RFB %in% lineup$Team) & (play_pos$PlayerName %in% lineup$fullname),]

for(i in 1:ncol(only_active_players))
{
  only_active_players[,i] <- as.character(only_active_players[,i])
}

for(i in 1:nrow(lineup))
{
  if((lineup$fullname[i] %in% only_active_players$PlayerName) %in% TRUE){
    lineup$MLBId[i] <- only_active_players$MLBId[which(only_active_players$PlayerName %in% lineup$fullname[i])]
  }
}

lineup$MLBId[which(lineup$fullname == "Miguel Gonzalez" & lineup$Team == "DET")] <- 544838
lineup$MLBId[which(lineup$fullname == "Matt Duffy" & lineup$Team == "NYA")] <- 622110
lineup$MLBId[which(lineup$fullname == "Matt Duffy" & lineup$Team == "HOU")] <- 592274
lineup$MLBId[which(lineup$fullname == "Miguel Gonzalez" & lineup$Team == "CHA")] <- 456068
lineup$MLBId[which(lineup$fullname == "Chris Young" & lineup$Team == "KC")] <- 432934
lineup$MLBId[which(lineup$fullname == "Chris Young" & lineup$Team == "BOS")] <- 455759
lineup$MLBId[which(lineup$fullname == "Jose Ramirez" & lineup$Team == "CLE")] <- 608070
lineup$MLBId[which(lineup$fullname == "Jose Ramirez" & lineup$Team == "ATL")] <- 542432

fielding_available <- read.csv("Fielding/Fielding_Master_Available2.csv")
fielding_sort <- fielding_available
fielding_sort$GameDate <- as.Date(fielding_sort$GameDate)
###

all_teams <- unique(lineup$Team)

for(r in 1:length(all_teams))
{
  bench <- which(lineup$Role == "Bench" & lineup$Team == all_teams[r])
  
  for(y in 1:length(bench))
  {
    fielding_sort2 <- fielding_sort[which(fielding_sort$MLBId %in% lineup$MLBId[bench[y]]),]
    
    if(nrow(fielding_sort2) > 0)
    {
      fielding_sort2 <- fielding_sort2[order(fielding_sort2$GameDate, decreasing = TRUE),]
      fielding_sort2 <- fielding_sort2[fielding_sort2$GameDate %in% max(fielding_sort2$GameDate),]
      fielding_sort2 <- fielding_sort2[fielding_sort2$INN %in% max(fielding_sort2$INN),]
      lineup$POS[bench[y]] <- as.character(fielding_sort2$Position[1])
    }
    
  }
}

###

# Loads database of all the pitcher that pitched in the past

pitcher_use <- read.csv("pitcher_usage_archive.csv", header = TRUE)

pitcher_use$GameDate <- as.Date(pitcher_use$GameDate, format="%Y-%m-%d")
pitcher_use$date_played <- as.Date(pitcher_use$date_played, format="%Y-%m-%d")

pitcher_use$FirstName <- as.character(pitcher_use$FirstName)
pitcher_use$LastName <- as.character(pitcher_use$LastName)
pitcher_use$POS <- as.character(pitcher_use$POS)
pitcher_use$fullname <- as.character(pitcher_use$fullname)
pitcher_use$MLBId <- as.character(pitcher_use$MLBId)

###

proration <- read.csv("prorate.csv")

proration$prorated_date <- as.Date(proration$prorated_date,format="%Y-%m-%d")
proration$PlayerName <- as.character(proration$PlayerName)
proration$MLBId <- as.character(proration$MLBId)
proration$Inning <- as.numeric(proration$Inning)
proration$OUT <- as.numeric(proration$OUT)
proration$GameString <- as.character(proration$GameString)
proration$ID <- as.character(proration$ID)

###

# Create 'for loop' structure that houses all the codes below. Loop structure will do repeat code below for each
# of the game 15

for(x in c(7))
{
  
  # Subset the box_scoring to have only players involved between two teams
  
  box_scoring <- lineup[lineup$Team %in% c(final_schedule$Away[x],final_schedule$Home[x]),]
  
  box_scoring$Role <- as.character(box_scoring$Role)
  box_scoring$fullname <- as.character(box_scoring$fullname)
  box_scoring$Date <- as.Date(box_scoring$Date,format="%Y-%m-%d")
  box_scoring$POS <- as.character(box_scoring$POS)
  box_scoring$MLBId <- as.character(box_scoring$MLBId)
  box_scoring$Team <- as.character(box_scoring$Team)
  box_scoring$`Fielding Petition` <- as.character(box_scoring$`Fielding Petition`)
  
  # subset visiting team lineup to box_visit
  
  box_visit <- box_scoring[box_scoring$Team == final_schedule$Away[x],]
  
  box_visit <- box_visit[box_visit$Role %in% c("#1","#2","#3","#4","#5","#6","#7","#8","#9"),]
  
  # subset home team line to box_home
  
  box_home <- box_scoring[box_scoring$Team == final_schedule$Home[x],]
  
  box_home <- box_home[box_home$Role %in% c("#1","#2","#3","#4","#5","#6","#7","#8","#9"),]
  
  ######
  
  
  # I am going to examine whether all nine guys in the lineup is set to play appropriate primary position. If so, he remains where he is.
  # If not, he will be deleted. 
  
  home_report_start <- read.csv(paste(getwd(),"/report/bat/",date,"/",final_schedule$Home[x],date,"_batting_report.csv",sep=""))
  
  for(k in 1:nrow(box_home))
  {
    if(((final_schedule$Home[x] %in% NL) & (box_home$POS2[k] == 1)) == TRUE)
    {
      next;
    }
    home_report_start <- read.csv(paste(getwd(),"/report/bat/",date,"/",final_schedule$Home[x],date,"_batting_report.csv",sep=""))
    
    box_home$MLBId <- as.character(box_home$MLBId)
    home_report_start$MLBId <- as.character(home_report_start$MLBId)
    
    home_report_start2 <- home_report_start
    home_report_start2 <- home_report_start2[home_report_start2$X25.man == "YES",]
    
    #If a position of player set in ith row of the lineup is not matched with his primary position, delete him off. Except Outfielder.
    if(!box_home$POS[k] == "DH"){
      
      if(box_home$fullname[k] == "")
      {
        next;
      }
      
      if(is.na(home_report_start$Pos[(home_report_start$MLBId %in% box_home$MLBId[k])]))
      {
        next;
      }
      
      if(unlist(strsplit(as.character(home_report_start2$Pos[home_report_start2$MLBId %in% box_home$MLBId[k]]),"")) %in% c(7,8,9))
      {
        next;
      }
      
      if((box_home$POS2[k] %in% unlist(strsplit(as.character(home_report_start2$primary[(home_report_start2$MLBId %in% box_home$MLBId[k])]),""))) == FALSE)
      {
        box_home$fullname[k] <- ""
        box_home$MLBId[k] <- ""
      }
      
      if((box_home$POS2[k] %in% unlist(strsplit(as.character(home_report_start2$primary[(home_report_start2$MLBId %in% box_home$MLBId[k])]),""))) == TRUE)
      {
        next;
      }
    }
    
    if(box_home$POS[k] == "DH")
    {
      next;
    }
  }
  
  home_report_start$SB <- home_report_start$Start + home_report_start$Bench
  
  
  count <- 0
  count2 <- 0
  
  for(ab in 1:nrow(box_home))
  {
    count <- 0
    
    open <- "YES"
    
    
    box_home$MLBId <- as.character(box_home$MLBId)
    home_report_start$MLBId <- as.character(home_report_start$MLBId)
    
    home_report_start2 <- home_report_start
    home_report_start2$SB <- home_report_start2$Start + home_report_start2$Bench
    
    if(box_home$fullname[ab] == "")
    {
      next;
    }
    
    if(home_report_start2$SB[home_report_start2$MLBId %in% box_home$MLBId[ab]] > 0)
    {
      if(is.na(home_report_start$Pos[(home_report_start$MLBId %in% box_home$MLBId[k])]))
      {
        next;
      }
      
      if(unlist(strsplit(as.character(home_report_start2$Pos[home_report_start2$MLBId %in% box_home$MLBId[ab]]),"")) %in% c(7,8,9))
      {
        
        next;
        
      } 
    }
    
    
    if(((final_schedule$Home[x] %in% NL) & (box_home$POS2[ab] == 1)) == TRUE)
    {
      next;
    }
    
    # Read in the home team's report. Then sum up available 'Start' and 'Bench'
    
    home_report_start <- read.csv(paste(getwd(),"/report/bat/",date,"/",final_schedule$Home[x],date,"_batting_report.csv",sep=""))
    home_report_start$SB <- home_report_start$Start + home_report_start$Bench
    
    box_home$MLBId <- as.character(box_home$MLBId)
    home_report_start$MLBId <- as.character(home_report_start$MLBId)
    
    
    if(!box_home$fullname[ab] == "")
    {
      # If he plays right position but has no stats. Or he does not play right position.
      
      if((home_report_start$SB[home_report_start$MLBId %in% box_home$MLBId[ab]] == 0) | !(box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start$primary[home_report_start$MLBId %in% box_home$MLBId[ab]]),""))))
      {
        # Subset for only 25-man roster
        
        home_report_start2 <- home_report_start[home_report_start$X25.man == "YES",]
        
        if(box_home$POS2[ab] == 1)
        {
          next;
        }
        
        # Subset for only man playing position for ith row.
        
        if((count == 0) & (open == "YES")){
          
          for(j in 1:nrow(home_report_start2))
          {
            home_report_start2 <- home_report_start[home_report_start$X25.man == "YES",]
            
            #If position of ith row in box_home is equal to jth row of home_report_start2, run this
            if((box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start2$primary[j]),""))) == TRUE)
            {
              
              # If player set in lineup already is matched with in the home_report_start2, Skip. You don't want that.
              if(box_home$MLBId[ab] == home_report_start2$MLBId[j])
              {
                if(j == nrow(home_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(!(j == nrow(home_report_start2)))
                {
                  next;
                }
              }
              
              # If a player set in lineup is matched with someone else that plays the same position, run this. 
              # Make sure he is not already set in the other part of lineup with a proper position.
              
              if(!(home_report_start2$MLBId[j] %in% box_home$MLBId) & (home_report_start2$SB[j] > 0))
              {
                if(j == nrow(home_report_start2))
                {
                  box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[j])
                  box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[j])
                  count <- count + 1
                  break;
                }
                
                if(!(j == nrow(home_report_start2)))
                {
                  box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[j])
                  box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[j])
                  count <- count + 1
                  break;
                }
                
              }
              
              if(!(home_report_start2$MLBId[j] %in% box_home$MLBId) & (home_report_start2$SB[j] == 0))
              {
                if(j == nrow(home_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(j != nrow(home_report_start2))
                {
                  next;
                }
                
              }
              
            }
            
            # If positions don't match skip to next row
            
            if((box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start2$primary[j]),""))) == FALSE)
            {
              if(j == nrow(home_report_start2))
              {
                count <- count + 1
                next;
              }
              
              if(!(j == nrow(home_report_start2)))
              {
                next;
              }
              
            }
            
            
          }
        }
        
        if((count == 1) & (open == "YES")){
          
          for(j in 1:nrow(home_report_start2))
          {
            home_report_start2 <- home_report_start[home_report_start$X25.man == "YES",]
            
            #If position of ith row in box_home is equal to jth row of home_report_start2, run this
            if((box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start2$secondary[j]),""))) == TRUE)
            {
              
              # If player set in lineup already is matched with in the home_report_start2, Skip. You don't want that.
              if(box_home$MLBId[ab] == home_report_start2$MLBId[j])
              {
                if(j == nrow(home_report_start2))
                {
                  count <- count + 1
                  break;
                }
                
                if(!(j == nrow(home_report_start2)))
                {
                  next;
                }
              }
              
              # If a player set in lineup is matched with someone else that plays the same position, run this. 
              # Make sure he is not already set in the other part of lineup with a proper position.
              
              if(!(home_report_start2$MLBId[j] %in% box_home$MLBId) & (home_report_start2$SB[j] > 0))
              {
                if(j == nrow(home_report_start2))
                {
                  box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[j])
                  box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
                if(!(j == nrow(home_report_start2)))
                {
                  box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[j])
                  box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
                if(!(home_report_start2$MLBId[j] %in% box_home$MLBId) & (home_report_start2$SB[j] == 0))
                {
                  if(j == nrow(home_report_start2))
                  {
                    count <- count + 1
                    next;
                  }
                  
                  if(j != nrow(home_report_start2))
                  {
                    next;
                  }
                }
                
              }
              
            }
            
            # If positions don't match skip to next row
            
            if((box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start2$tertiary[j]),""))) == FALSE)
            {
              if(j == nrow(home_report_start2))
              {
                count <- count + 1
                next;
              }
              
              if(!(j == nrow(home_report_start2)))
              {
                next;
              }
              
            }
            
            
          }
          
        }
        
        if((count == 2) & (open == "YES")){
          
          for(j in 1:nrow(home_report_start2))
          {
            home_report_start2 <- home_report_start[home_report_start$X25.man == "YES",]
            
            #If position of ith row in box_home is equal to jth row of home_report_start2, run this
            if((box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start2$tertiary[j]),""))) == TRUE)
            {
              
              # If player set in lineup already is matched with in the home_report_start2, Skip. You don't want that.
              if(box_home$MLBId[ab] == home_report_start2$MLBId[j])
              {
                if(j == nrow(home_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(!(j == nrow(home_report_start2)))
                {
                  next;
                }
              }
              
              # If a player set in lineup is matched with someone else that plays the same position, run this. 
              # Make sure he is not already set in the other part of lineup with a proper position.
              
              if(!(home_report_start2$MLBId[j] %in% box_home$MLBId) & (home_report_start2$SB[j] > 0))
              {
                if(j == nrow(home_report_start2))
                {
                  box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[j])
                  box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
                if(!(j == nrow(home_report_start2)))
                {
                  box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[j])
                  box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
              }
              
              if(!(home_report_start2$MLBId[j] %in% box_home$MLBId) & (home_report_start2$SB[j] == 0))
              {
                if(j == nrow(home_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(j != nrow(home_report_start2))
                {
                  next;
                }
              }
              
            }
            
            # If positions don't match skip to next row
            
            if((box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start2$tertiary[j]),""))) == FALSE)
            {
              if(j == nrow(home_report_start2))
              {
                count <- count + 1
                next;
              }
              
              if(!(j == nrow(home_report_start2)))
              {
                next;
              }
              
            }
            
            
          }
        }
        
        if((count == 3) & (open == "YES")){
          
          home_report_start2 <- home_report_start[home_report_start$X25.man == "NO",]
          
          for(j in 1:nrow(home_report_start2))
          {
            
            #If position of ith row in box_home is equal to jth row of home_report_start2, run this
            if((box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start2$primary[j]),""))) == TRUE)
            {
              
              # If player set in lineup already is matched with in the home_report_start2, Skip. You don't want that.
              if(box_home$MLBId[ab] == home_report_start2$MLBId[j])
              {
                if(j == nrow(home_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(!(j == nrow(home_report_start2)))
                {
                  next;
                }
              }
              
              # If a player set in lineup is matched with someone else that plays the same position, run this. 
              # Make sure he is not already set in the other part of lineup with a proper position.
              
              if(!(home_report_start2$MLBId[j] %in% box_home$MLBId) & (home_report_start2$SB[j] > 0))
              {
                if(j == nrow(home_report_start2))
                {
                  box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[j])
                  box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
                if(!(j == nrow(home_report_start2)))
                {
                  box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[j])
                  box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
              }
              
              if(!(home_report_start2$MLBId[j] %in% box_home$MLBId) & (home_report_start2$SB[j] == 0))
              {
                if(j == nrow(home_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(j != nrow(home_report_start2))
                {
                  next;
                }
              }
              
            }
            
            # If positions don't match skip to next row
            
            if((box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start2$primary[j]),""))) == FALSE)
            {
              if(j == nrow(home_report_start2))
              {
                count <- count + 1
                next;
              }
              
              if(!(j == nrow(home_report_start2)))
              {
                next;
              }
              
            }
            
            
          }
        }
        
        if((count == 4) & (open == "YES")){
          
          for(j in 1:nrow(home_report_start))
          {
            home_report_start2 <- home_report_start[home_report_start$X25.man == "NO",]
            
            #If position of ith row in box_home is equal to jth row of home_report_start2, run this
            if((box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start2$secondary[j]),""))) == TRUE)
            {
              
              # If player set in lineup already is matched with in the home_report_start2, Skip. You don't want that.
              if(box_home$MLBId[ab] == home_report_start2$MLBId[j])
              {
                if(j == nrow(home_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(!(j == nrow(home_report_start2)))
                {
                  next;
                }
              }
              
              # If a player set in lineup is matched with someone else that plays the same position, run this. 
              # Make sure he is not already set in the other part of lineup with a proper position.
              
              if(!(home_report_start2$MLBId[j] %in% box_home$MLBId) & (home_report_start2$SB[j] > 0))
              {
                if(j == nrow(home_report_start2))
                {
                  box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[j])
                  box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
                if(!(j == nrow(home_report_start2)))
                {
                  box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[j])
                  box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
              }
              
              if(!(home_report_start2$MLBId[j] %in% box_home$MLBId) & (home_report_start2$SB[j] == 0))
              {
                if(j == nrow(home_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(j != nrow(home_report_start2))
                {
                  next;
                }
              }
              
            }
            
            # If positions don't match skip to next row
            
            if((box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start2$secondary[j]),""))) == FALSE)
            {
              if(j == nrow(home_report_start2))
              {
                count <- count + 1
                next;
              }
              
              if(!(j == nrow(home_report_start2)))
              {
                next;
              }
              
            }
            
            
          }
        }
        
        if((count == 5) & (open == "YES")){
          
          for(j in 1:nrow(home_report_start))
          {
            home_report_start2 <- home_report_start[home_report_start$X25.man == "NO",]
            
            #If position of ith row in box_home is equal to jth row of home_report_start2, run this
            if((box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start2$tertiary[j]),""))) == TRUE)
            {
              
              # If player set in lineup already is matched with in the home_report_start2, Skip. You don't want that.
              if(box_home$MLBId[ab] == home_report_start2$MLBId[j])
              {
                if(j == nrow(home_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(!(j == nrow(home_report_start2)))
                {
                  next;
                }
              }
              
              # If a player set in lineup is matched with someone else that plays the same position, run this. 
              # Make sure he is not already set in the other part of lineup with a proper position.
              
              if(!(home_report_start2$MLBId[j] %in% box_home$MLBId) & (home_report_start2$SB[j] > 0))
              {
                if(j == nrow(home_report_start2))
                {
                  box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[j])
                  box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
                if(!(j == nrow(home_report_start2)))
                {
                  box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[j])
                  box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
              }
              
              if(!(home_report_start2$MLBId[j] %in% box_home$MLBId) & (home_report_start2$SB[j] == 0))
              {
                if(j == nrow(home_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(j != nrow(home_report_start2))
                {
                  next;
                }
              }
              
            }
            
            # If positions don't match skip to next row
            
            if((box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start2$tertiary[j]),""))) == FALSE)
            {
              if(j == nrow(home_report_start2))
              {
                count <- count + 1
                next;
              }
              
              if(!(j == nrow(home_report_start2)))
              {
                next;
              }
              
            }
            
            
          }
        }
        
        pos_sim <- read.csv("pos_sim.csv",header = TRUE)
        
        if((count == 6) & (open == "YES"))
        {
          home_report_start2 <- home_report_start[home_report_start$X25.man == "YES",]
          
          if(box_home$POS2[ab] == "DH")
          {
            next;
          }
          
          if(box_home$POS2[ab] != "DH")
          {
            col_number <- as.numeric(box_home$POS2[ab])
          }
          
          position_similar <- pos_sim[,col_number]
          position_similar <- position_similar[!is.na(position_similar)]
          
          for(j in 2:length(position_similar))
          {
            for(l in 1:nrow(home_report_start2))
            {
              # If position from "Position Similarity" is not in the position player's primary position, skip on.
              if(!position_similar[j] %in% unlist(strsplit(as.character(home_report_start2$primary[l]),"")))
              {
                if(j == length(position_similar))
                {
                  count <- count + 1
                  break;
                }
                
                if(j != length(position_similar))
                {
                  next;
                }
                
              }
              
              # If position from "Position Similarity" is in the position player's primary position, run this.
              if(position_similar[j] %in% unlist(strsplit(as.character(home_report_start2$primary[l]),"")))
              {
                # If a player in question was not already penciled in for the lineup and has a stat, run this.
                
                if((home_report_start2$SB[l] > 0) & !(home_report_start2$MLBId[l] %in% box_home$MLBId))
                {
                  empty_slot <- which(box_home$fullname == "")
                  
                  if(length(empty_slot) > 0)
                  {
                    # If player in question will be needed in the empty slot, run this
                    if(box_home$POS2[empty_slot] %in% unlist(strsplit(as.character(home_report_start2$primary[l]),"")))
                    {
                      if(j == length(position_similar))
                      {
                        count <- count + 1
                        next;
                      }
                      
                      if(j != length(position_similar))
                      {
                        next;
                      }
                    }
                    # If player in question won't be needed in the empty slot, run this
                    if(!box_home$POS2[empty_slot] %in% unlist(strsplit(as.character(home_report_start2$primary[l]),"")))
                    {
                      
                      if(j == length(position_similar))
                      {
                        box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[l])
                        box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[l])
                        count <- count + 1
                        open <- "NO"
                        break;
                      }
                      
                      if(j != length(position_similar))
                      {
                        box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[l])
                        box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[l])
                        count <- count + 1
                        open <- "NO"
                        break;
                      }
                    }
                  }
                }
                
                # If a player in question has no stats or already has been penciled into the lineup, run this.
                if((home_report_start2$SB[l] == 0) | (home_report_start2$MLBId[l] %in% box_home$MLBId))
                {
                  next;
                }
              }
            }
          }
        } 
        
        if((count == 7) & (open == "YES"))
        {
          home_report_start2 <- home_report_start
          
          col_number <- as.numeric(box_home$POS2[ab])
          position_similar <- pos_sim[,col_number]
          position_similar <- position_similar[!is.na(position_similar)]
          
          for(j in 2:length(position_similar))
          {
            for(l in 1:nrow(home_report_start2))
            {
              # If position from "Position Similarity" is not in the position player's primary position, skip on.
              if(!position_similar[j] %in% unlist(strsplit(as.character(home_report_start2$primary[l]),"")))
              {
                if(j == length(position_similar))
                {
                  count <- count + 1
                  break;
                }
                
                if(j != length(position_similar))
                {
                  next;
                }
                
              }
              
              # If position from "Position Similarity" is in the position player's primary position, run this.
              if(position_similar[j] %in% unlist(strsplit(as.character(home_report_start2$primary[l]),"")))
              {
                # If a player in question was not already penciled in for the lineup and has a stat, run this.
                
                if((home_report_start2$SB[l] > 0) & !(home_report_start2$MLBId[l] %in% box_home$MLBId))
                {
                  empty_slot <- which(box_home$fullname == "")
                  
                  if(length(empty_slot) > 0)
                  {
                    # If player in question will be needed in the empty slot, run this
                    if(box_home$POS2[empty_slot] %in% unlist(strsplit(as.character(home_report_start2$primary[l]),"")))
                    {
                      if(j == length(position_similar))
                      {
                        count <- count + 1
                        next;
                      }
                      
                      if(j != length(position_similar))
                      {
                        next;
                      }
                    }
                    # If player in question won't be needed in the empty slot, run this
                    if(!box_home$POS2[empty_slot] %in% unlist(strsplit(as.character(home_report_start2$primary[l]),"")))
                    {
                      
                      if(j == length(position_similar))
                      {
                        box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[l])
                        box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[l])
                        count <- count + 1
                        open <- "NO"
                        break;
                      }
                      
                      if(j != length(position_similar))
                      {
                        box_home$fullname[ab] <- as.character(home_report_start2$PlayerName[l])
                        box_home$MLBId[ab] <- as.character(home_report_start2$MLBId[l])
                        count <- count + 1
                        open <- "NO"
                        break;
                      }
                    }
                  }
                }
                
                # If a player in question has no stats or already has been penciled into the lineup, run this.
                if((home_report_start2$SB[l] == 0) | (home_report_start2$MLBId[l] %in% box_home$MLBId))
                {
                  next;
                }
              }
            }
          }
        }
        
        
        
      }
      
      # If he plays right position and has stats
      
      if(((home_report_start$SB[home_report_start$MLBId %in% box_home$MLBId[ab]] > 0) & (box_home$POS2[ab] %in% unlist(strsplit(as.character(home_report_start$primary[home_report_start$MLBId %in% box_home$MLBId[ab]]),"")))))
      {
        next;
      }
    }
    
    
  }
  
  remaining_position_player25 <- home_report_start[(home_report_start$X25.man == "YES") & !(home_report_start$MLBId %in% box_home$MLBId) & (home_report_start$SB > 0),]
  remaining_position_player40 <- home_report_start[(home_report_start$X25.man == "NO")  & !(home_report_start$MLBId %in% box_home$MLBId) & (home_report_start$SB > 0),]
  
  remaining_position_player40$MLBId <- as.character(remaining_position_player40$MLBId)
  remaining_position_player25$MLBId <- as.character(remaining_position_player25$MLBId)
  
  for(f in 1:nrow(box_home))
  {
    
    count2 <- 0
    open2 <- "YES"
    
    if(box_home$fullname[f] != "")
    {
      next;
    }
    
    # If lineup is set empty, then get in someone that suits the position at ith row.
    
    if(box_home$fullname[f] == "")
    {
      if((count2 == 0) & (open2 == "YES"))
      {
        for(g in 1:nrow(remaining_position_player25))
        {
          
          if((box_home$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player25$primary[g]),""))) == TRUE)
          {
            
            if((g == nrow(remaining_position_player25)) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
            {
              box_home$fullname[f] <- as.character(remaining_position_player25$PlayerName[g])
              box_home$MLBId[f] <- remaining_position_player25$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
            if((g != nrow(remaining_position_player25)) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
            {
              box_home$fullname[f] <- as.character(remaining_position_player25$PlayerName[g])
              box_home$MLBId[f] <- remaining_position_player25$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"            
            }
            
          }
          
          if((box_home$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player25$primary[g]),""))) == FALSE)
          {
            if(g == nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
            {
              count2 <- count2 + 1
              next;
            }
            
            if(g != nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
            {
              next;
            }
          }
          
        }
      }
      
      if((count2 == 1) & (open2 == "YES"))
      {
        for(g in 1:nrow(remaining_position_player25))
        {
          
          if((box_home$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player25$secondary[g]),""))) == TRUE)
          {
            
            if(g == nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
            {
              box_home$fullname[f] <- as.character(remaining_position_player25$PlayerName[g])
              box_home$MLBId[f] <- as.character(remaining_position_player25$MLBId[g])
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
            if(g != nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
            {
              box_home$fullname[f] <- as.character(remaining_position_player25$PlayerName[g])
              box_home$MLBId[f] <- as.character(remaining_position_player25$MLBId[g])
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
          }
          
          if((box_home$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player25$secondary[g]),""))) == FALSE)
          {
            if(g == nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
            {
              count2 <- count2 + 1
              next;
            }
            
            if(g != nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
            {
              next;
            }
          }
          
        }
        
      }
      
      if((count2 == 2) & (open2 == "YES"))
      {
        
        for(g in 1:nrow(remaining_position_player25))
        {
          
          if((box_home$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player25$tertiary[g]),""))) == TRUE)
          {
            
            if(g == nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
            {
              box_home$fullname[f] <- as.character(remaining_position_player25$PlayerName[g])
              box_home$MLBId[f] <- remaining_position_player25$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO" 
            }
            
            if(g != nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
            {
              box_home$fullname[f] <- as.character(remaining_position_player25$PlayerName[g])
              box_home$MLBId[f] <- remaining_position_player25$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
          }
          
          if((box_home$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player25$tertiary[g]),""))) == FALSE)
          {
            if(g == nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
            {
              count2 <- count2 + 1
              next;
            }
            
            if(g != nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
            {
              next;
            }
          }
          
        }
        
      }
      
      if((count2 == 3) & (open2 == "YES"))
      {
        if(nrow(remaining_position_player40) == 0)
        {
          next;
        }
        
        for(g in 1:nrow(remaining_position_player40))
        {
          
          if((box_home$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player40$primary[g]),""))) == TRUE)
          {
            if(nrow(remaining_position_player40) == 0)
            {
              next;
            }
            
            if(g == nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
            {
              box_home$fullname[f] <- as.character(remaining_position_player40$PlayerName[g])
              box_home$MLBId[f] <- remaining_position_player40$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
            if(g != nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
            {
              box_home$fullname[f] <- as.character(remaining_position_player40$PlayerName[g])
              box_home$MLBId[f] <- remaining_position_player40$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
          }
          
          if((box_home$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player40$primary[g]),""))) == FALSE)
          {
            if(nrow(remaining_position_player40) == 0)
            {
              next;
            }
            
            
            if(g == nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
            {
              count2 <- count2 + 1
              next;
            }
            
            if(g != nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
            {
              next;
            }
          }
          
        }
        
      }
      
      if((count2 == 4) & (open2 == "YES"))
      {
        for(g in 1:nrow(remaining_position_player40))
        {
          
          if((box_home$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player40$secondary[g]),""))) == TRUE)
          {
            if(nrow(remaining_position_player40) == 0)
            {
              next;
            }
            
            if(g == nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
            {
              box_home$fullname[f] <- as.character(remaining_position_player40$PlayerName[g])
              box_home$MLBId[f] <- remaining_position_player40$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
            if(g != nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
            {
              box_home$fullname[f] <- as.character(remaining_position_player40$PlayerName[g])
              box_home$MLBId[f] <- remaining_position_player40$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
          }
          
          if((box_home$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player40$secondary[g]),""))) == FALSE)
          {
            if(nrow(remaining_position_player40) == 0)
            {
              next;
            }
            
            
            if(g == nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
            {
              count2 <- count2 + 1
              next;
            }
            
            if(g != nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
            {
              next;
            }
          }
          
        }
        
        
      }
      
      if((count2 == 5) & (open2 == "YES"))
      {
        
        for(g in 1:nrow(remaining_position_player40))
        {
          
          if((box_home$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player40$tertiary[g]),""))) == TRUE)
          {
            if(nrow(remaining_position_player40) == 0)
            {
              next;
            }
            
            
            if(g == nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
            {
              box_home$fullname[f] <- as.character(remaining_position_player40$PlayerName[g])
              box_home$MLBId[f] <- remaining_position_player40$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
            if(g != nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
            {
              box_home$fullname[f] <- as.character(remaining_position_player40$PlayerName[g])
              box_home$MLBId[f] <- remaining_position_player40$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
          }
          
          if((box_home$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player40$tertiary[g]),""))) == FALSE)
          {
            if(nrow(remaining_position_player40) == 0)
            {
              next;
            }
            
            
            if(g == nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
            {
              count2 <- count2 + 1
              next;
            }
            
            if(g != nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
            {
              next;
            }
          }
          
        }
        
      }
      
      if((count2 == 6) & (open2 == "YES"))
      {
        
        col_number <- as.numeric(box_home$POS2[f])
        position_similar <- pos_sim[,col_number]
        position_similar <- position_similar[!is.na(position_similar)]
        
        for(j in 2:length(position_similar))
        {
          for(l in 1:nrow(remaining_position_player25))
          {
            # If position from "Position Similarity" is not in the position player's primary position, skip on.
            if(!position_similar[j] %in% unlist(strsplit(as.character(remaining_position_player25$primary[l]),"")))
            {
              if(j == length(position_similar))
              {
                count2 <- count2 + 1
              }
              
              if(j != length(position_similar))
              {
                next;
              }
              
            }
            
            # If position from "Position Similarity" is in the position player's primary position, run this.
            if(position_similar[j] %in% unlist(strsplit(as.character(remaining_position_player25$primary[l]),"")))
            {
              # If a player in question was not already penciled in for the lineup and has a stat, run this.
              
              if((remaining_position_player25$SB[l] > 0) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
              {
                empty_slot <- which(box_home$fullname == "")
                
                if(length(empty_slot) > 0)
                {
                  # If player in question will be needed in the empty slot, run this
                  if(box_home$POS2[empty_slot] %in% unlist(strsplit(as.character(remaining_position_player25$primary[l]),"")))
                  {
                    if(j == length(position_similar))
                    {
                      count2 <- count2 + 1
                    }
                    
                    if(j != length(position_similar))
                    {
                      next;
                    }
                  }
                  # If player in question won't be needed in the empty slot, run this
                  if(!box_home$POS2[empty_slot] %in% unlist(strsplit(as.character(remaining_position_player25$primary[l]),"")))
                  {
                    
                    if(j == length(position_similar) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
                    {
                      box_home$fullname[f] <- as.character(remaining_position_player25$PlayerName[l])
                      box_home$MLBId[f] <- as.character(remaining_position_player25$MLBId[l])
                      count2 <- count2 + 1
                      open2 <- "NO"
                    }
                    
                    if(j != length(position_similar) & !(remaining_position_player25$MLBId[g] %in% box_home$MLBId))
                    {
                      box_home$fullname[f] <- as.character(remaining_position_player25$PlayerName[l])
                      box_home$MLBId[f] <- as.character(remaining_position_player25$MLBId[l])
                      count2 <- count2 + 1
                      open2 <- "NO"
                    }
                  }
                }
              }
              
              # If a player in question has no stats or already has been penciled into the lineup, run this.
              if((remaining_position_player25$SB[l] == 0) | (box_home$MLBId[f] %in% remaining_position_player25$MLBId))
              {
                next;
              }
            }
          }
        }
        
        
        position_player_lineup <- home_report_start[(home_report_start$X25.man == "YES") & (home_report_start$MLBId %in% box_home$MLBId) & (home_report_start$SB > 0),]
        
        # This code will shuffle around lineups in case someone who plays out of position can be switched with someone else in the lineup to eliminate
        # "Out of position" placement.
        
        if(box_home$MLBId[f] == "")
        {
          next;
        }
        
        if(!unlist(strsplit(as.character(home_report_start$Pos[home_report_start$MLBId %in% box_home$MLBId[f]],""),"")) %in% box_home$POS2[f])
        {
          for(d in 1:nrow(position_player_lineup))
          {
            
            if(box_home$POS2[f] %in% unlist(strsplit(as.character(position_player_lineup$Pos[d]),"")))
            {
              box_home$fullname[which(box_home$MLBId %in% position_player_lineup$MLBId[d])] <- ""
              box_home$MLBId[which(box_home$MLBId %in% position_player_lineup$MLBId[d])] <- ""
              
              box_home$fullname[f] <- as.character(position_player_lineup$PlayerName[d])
              box_home$MLBId[f] <- as.character(position_player_lineup$MLBId[d])
              
            }
            
          }
        }
        
      }
      
      if((count2 == 7) & (open2 == "YES"))
      {
        
        col_number <- as.numeric(box_home$POS2[f])
        position_similar <- pos_sim[,col_number]
        position_similar <- position_similar[!is.na(position_similar)]
        
        for(j in 2:length(position_similar))
        {
          for(l in 1:nrow(remaining_position_player40))
          {
            # If position from "Position Similarity" is not in the position player's primary position, skip on.
            if(!position_similar[j] %in% unlist(strsplit(as.character(remaining_position_player40$primary[l]),"")))
            {
              if(j == length(position_similar))
              {
                count2 <- count2 + 1
              }
              
              if(j != length(position_similar))
              {
                next;
              }
              
            }
            
            # If position from "Position Similarity" is in the position player's primary position, run this.
            if(position_similar[j] %in% unlist(strsplit(as.character(remaining_position_player40$primary[l]),"")))
            {
              # If a player in question was not already penciled in for the lineup and has a stat, run this.
              
              if((remaining_position_player40$SB[l] > 0) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
              {
                empty_slot <- which(box_home$fullname == "")
                
                if(length(empty_slot) > 0)
                {
                  # If player in question will be needed in the empty slot, run this
                  if(box_home$POS2[empty_slot] %in% unlist(strsplit(as.character(remaining_position_player40$primary[l]),"")))
                  {
                    if(j == length(position_similar))
                    {
                      count2 <- count2 + 1
                    }
                    
                    if(j != length(position_similar))
                    {
                      next;
                    }
                  }
                  # If player in question won't be needed in the empty slot, run this
                  if(!box_home$POS2[empty_slot] %in% unlist(strsplit(as.character(remaining_position_player40$primary[l]),"")))
                  {
                    
                    if(j == length(position_similar) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
                    {
                      box_home$fullname[f] <- as.character(remaining_position_player40$PlayerName[l])
                      box_home$MLBId[f] <- as.character(remaining_position_player40$MLBId[l])
                      count2 <- count2 + 1
                      open2 <- "NO"
                    }
                    
                    if(j != length(position_similar) & !(remaining_position_player40$MLBId[g] %in% box_home$MLBId))
                    {
                      box_home$fullname[f] <- as.character(remaining_position_player40$PlayerName[l])
                      box_home$MLBId[f] <- as.character(remaining_position_player40$MLBId[l])
                      count2 <- count2 + 1
                      open2 <- "NO"
                    }
                  }
                }
              }
              
              # If a player in question has no stats or already has been penciled into the lineup, run this.
              if((remaining_position_player40$SB[l] == 0) | (remaining_position_player40$MLBId[l] %in% box_home$MLBId))
              {
                next;
              }
            }
          }
        }
        
        position_player_lineup <- home_report_start[(home_report_start$X25.man == "YES") & (home_report_start$MLBId %in% box_home$MLBId) & (home_report_start$SB > 0),]
        
        # This code will shuffle around lineups in case someone who plays out of position can be switched with someone else in the lineup to eliminate
        # "Out of position" placement.
        
        if(!unlist(strsplit(as.character(home_report_start$Pos[home_report_start$MLBId %in% box_home$MLBId[f]],""),"")) %in% box_home$POS2[f])
        {
          for(d in 1:nrow(position_player_lineup))
          {
            
            if(box_home$POS2[f] %in% unlist(strsplit(as.character(position_player_lineup$Pos[d]),"")))
            {
              box_home$fullname[which(box_home$MLBId %in% position_player_lineup$MLBId[d])] <- ""
              box_home$MLBId[which(box_home$MLBId %in% position_player_lineup$MLBId[d])] <- ""
              
              box_home$fullname[f] <- as.character(position_player_lineup$PlayerName[d])
              box_home$MLBId[f] <- as.character(position_player_lineup$MLBId[d])
              
            }
            
          }
        }
        
      }
    }
  }
  
  box_home$SB <- ""
  
  home_report_start$SB <- home_report_start$Start + home_report_start$Bench
  
  for(h in 1:nrow(box_home))
  {
    if(box_home$fullname[h] != "")
    {
      box_home$SB[h] <- home_report_start$SB[home_report_start$MLBId %in% box_home$MLBId[h]]
      
    }
    
    if(box_home$fullname[h] == "")
    {
      next;
    }
  }
  
  for(z in 1:nrow(box_home))
  {
    if((box_home$SB[z] == 0) & (box_home$POS2[z] == 1))
    {
      print("Don't erase pitchers")
    }
    
    if((box_home$SB[z] == 0) & (box_home$POS2[z] %in% c(2,3,4,5,6,7,8,9))){
      box_home$fullname[z] <- ""
      box_home$MLBId[z] <- "" 
    }
  }
  
  
  
  # I am going to examine whether all nine guys in the lineup is set to play appropriate primary position. If so, he remains where he is.
  # If not, he will be deleted. 
  
  visit_report_start <- read.csv(paste(getwd(),"/report/bat/",date,"/",final_schedule$Away[x],date,"_batting_report.csv",sep=""))
  
  for(k in 1:nrow(box_visit))
  {
    if(((final_schedule$Home[x] %in% NL) & (box_visit$POS2[k] == 1)) == TRUE)
    {
      next;
    }
    visit_report_start <- read.csv(paste(getwd(),"/report/bat/",date,"/",final_schedule$Away[x],date,"_batting_report.csv",sep=""))
    
    box_visit$MLBId <- as.character(box_visit$MLBId)
    visit_report_start$MLBId <- as.character(visit_report_start$MLBId)
    
    visit_report_start2 <- visit_report_start
    visit_report_start2 <- visit_report_start2[visit_report_start2$X25.man == "YES",]
    
    #If a position of player set in ith row of the lineup is not matched with his primary position, delete him off. Except Outfielder.
    if(!box_visit$POS[k] == "DH"){
      
      if(box_visit$fullname[k] == "")
      {
        next;
      }
      
      if(is.na(visit_report_start$Pos[(visit_report_start$MLBId %in% box_visit$MLBId[k])]))
      {
        next;
      }
      
      if(unlist(strsplit(as.character(visit_report_start2$Pos[visit_report_start2$MLBId %in% box_visit$MLBId[k]]),"")) %in% c(7,8,9))
      {
        next;
      }
      
      if((box_visit$POS2[k] %in% unlist(strsplit(as.character(visit_report_start2$primary[(visit_report_start2$MLBId %in% box_visit$MLBId[k])]),""))) == FALSE)
      {
        box_visit$fullname[k] <- ""
        box_visit$MLBId[k] <- ""
      }
      
      if((box_visit$POS2[k] %in% unlist(strsplit(as.character(visit_report_start2$primary[(visit_report_start2$MLBId %in% box_visit$MLBId[k])]),""))) == TRUE)
      {
        next;
      }
    }
    
    if(box_visit$POS[k] == "DH")
    {
      next;
    }
  }
  
  visit_report_start$SB <- visit_report_start$Start + visit_report_start$Bench
  
  
  count <- 0
  count2 <- 0
  
  for(ab in 1:nrow(box_visit))
  {
    count <- 0
    
    open <- "YES"
    
    
    box_visit$MLBId <- as.character(box_visit$MLBId)
    visit_report_start$MLBId <- as.character(visit_report_start$MLBId)
    
    visit_report_start2 <- visit_report_start
    visit_report_start2$SB <- visit_report_start2$Start + visit_report_start2$Bench
    
    if(box_visit$fullname[ab] == "")
    {
      next;
    }
    
    if(visit_report_start2$SB[visit_report_start2$MLBId %in% box_visit$MLBId[ab]] > 0)
    {
      if(is.na(visit_report_start$Pos[(visit_report_start$MLBId %in% box_visit$MLBId[k])]))
      {
        next;
      }
      
      if(unlist(strsplit(as.character(visit_report_start2$Pos[visit_report_start2$MLBId %in% box_visit$MLBId[ab]]),"")) %in% c(7,8,9))
      {
        
        next;
        
      } 
    }
    
    
    if(((final_schedule$Home[x] %in% NL) & (box_visit$POS2[ab] == 1)) == TRUE)
    {
      next;
    }
    
    # Read in the visit team's report. Then sum up available 'Start' and 'Bench'
    
    visit_report_start <- read.csv(paste(getwd(),"/report/bat/",date,"/",final_schedule$Away[x],date,"_batting_report.csv",sep=""))
    visit_report_start$SB <- visit_report_start$Start + visit_report_start$Bench
    
    box_visit$MLBId <- as.character(box_visit$MLBId)
    visit_report_start$MLBId <- as.character(visit_report_start$MLBId)
    
    
    if(!box_visit$fullname[ab] == "")
    {
      # If he plays right position but has no stats. Or he does not play right position.
      
      if((visit_report_start$SB[visit_report_start$MLBId %in% box_visit$MLBId[ab]] == 0) | !(box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start$primary[visit_report_start$MLBId %in% box_visit$MLBId[ab]]),""))))
      {
        if(box_visit$POS2[ab] == 1)
        {
          next;
        }
        
        # Subset for only 25-man roster
        
        visit_report_start2 <- visit_report_start[visit_report_start$X25.man == "YES",]
        
        # Subset for only man playing position for ith row.
        
        if((count == 0) & (open == "YES")){
          
          for(j in 1:nrow(visit_report_start2))
          {
            visit_report_start2 <- visit_report_start[visit_report_start$X25.man == "YES",]
            
            #If position of ith row in box_visit is equal to jth row of visit_report_start2, run this
            if((box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start2$primary[j]),""))) == TRUE)
            {
              
              # If player set in lineup already is matched with in the visit_report_start2, Skip. You don't want that.
              if(box_visit$MLBId[ab] == visit_report_start2$MLBId[j])
              {
                if(j == nrow(visit_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(!(j == nrow(visit_report_start2)))
                {
                  next;
                }
              }
              
              # If a player set in lineup is matched with someone else that plays the same position, run this. 
              # Make sure he is not already set in the other part of lineup with a proper position.
              
              if(!(visit_report_start2$MLBId[j] %in% box_visit$MLBId) & (visit_report_start2$SB[j] > 0))
              {
                if(j == nrow(visit_report_start2))
                {
                  box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[j])
                  box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[j])
                  count <- count + 1
                  break;
                }
                
                if(!(j == nrow(visit_report_start2)))
                {
                  box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[j])
                  box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[j])
                  count <- count + 1
                  break;
                }
                
              }
              
              if(!(visit_report_start2$MLBId[j] %in% box_visit$MLBId) & (visit_report_start2$SB[j] == 0))
              {
                if(j == nrow(visit_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(j != nrow(visit_report_start2))
                {
                  next;
                }
                
              }
              
            }
            
            # If positions don't match skip to next row
            
            if((box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start2$primary[j]),""))) == FALSE)
            {
              if(j == nrow(visit_report_start2))
              {
                count <- count + 1
                next;
              }
              
              if(!(j == nrow(visit_report_start2)))
              {
                next;
              }
              
            }
            
            
          }
        }
        
        if((count == 1) & (open == "YES")){
          
          for(j in 1:nrow(visit_report_start2))
          {
            visit_report_start2 <- visit_report_start[visit_report_start$X25.man == "YES",]
            
            #If position of ith row in box_visit is equal to jth row of visit_report_start2, run this
            if((box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start2$secondary[j]),""))) == TRUE)
            {
              
              # If player set in lineup already is matched with in the visit_report_start2, Skip. You don't want that.
              if(box_visit$MLBId[ab] == visit_report_start2$MLBId[j])
              {
                if(j == nrow(visit_report_start2))
                {
                  count <- count + 1
                  break;
                }
                
                if(!(j == nrow(visit_report_start2)))
                {
                  next;
                }
              }
              
              # If a player set in lineup is matched with someone else that plays the same position, run this. 
              # Make sure he is not already set in the other part of lineup with a proper position.
              
              if(!(visit_report_start2$MLBId[j] %in% box_visit$MLBId) & (visit_report_start2$SB[j] > 0))
              {
                if(j == nrow(visit_report_start2))
                {
                  box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[j])
                  box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
                if(!(j == nrow(visit_report_start2)))
                {
                  box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[j])
                  box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
                if(!(visit_report_start2$MLBId[j] %in% box_visit$MLBId) & (visit_report_start2$SB[j] == 0))
                {
                  if(j == nrow(visit_report_start2))
                  {
                    count <- count + 1
                    next;
                  }
                  
                  if(j != nrow(visit_report_start2))
                  {
                    next;
                  }
                }
                
              }
              
            }
            
            # If positions don't match skip to next row
            
            if((box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start2$tertiary[j]),""))) == FALSE)
            {
              if(j == nrow(visit_report_start2))
              {
                count <- count + 1
                next;
              }
              
              if(!(j == nrow(visit_report_start2)))
              {
                next;
              }
              
            }
            
            
          }
          
        }
        
        if((count == 2) & (open == "YES")){
          
          for(j in 1:nrow(visit_report_start2))
          {
            visit_report_start2 <- visit_report_start[visit_report_start$X25.man == "YES",]
            
            #If position of ith row in box_visit is equal to jth row of visit_report_start2, run this
            if((box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start2$tertiary[j]),""))) == TRUE)
            {
              
              # If player set in lineup already is matched with in the visit_report_start2, Skip. You don't want that.
              if(box_visit$MLBId[ab] == visit_report_start2$MLBId[j])
              {
                if(j == nrow(visit_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(!(j == nrow(visit_report_start2)))
                {
                  next;
                }
              }
              
              # If a player set in lineup is matched with someone else that plays the same position, run this. 
              # Make sure he is not already set in the other part of lineup with a proper position.
              
              if(!(visit_report_start2$MLBId[j] %in% box_visit$MLBId) & (visit_report_start2$SB[j] > 0))
              {
                if(j == nrow(visit_report_start2))
                {
                  box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[j])
                  box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
                if(!(j == nrow(visit_report_start2)))
                {
                  box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[j])
                  box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
              }
              
              if(!(visit_report_start2$MLBId[j] %in% box_visit$MLBId) & (visit_report_start2$SB[j] == 0))
              {
                if(j == nrow(visit_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(j != nrow(visit_report_start2))
                {
                  next;
                }
              }
              
            }
            
            # If positions don't match skip to next row
            
            if((box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start2$tertiary[j]),""))) == FALSE)
            {
              if(j == nrow(visit_report_start2))
              {
                count <- count + 1
                next;
              }
              
              if(!(j == nrow(visit_report_start2)))
              {
                next;
              }
              
            }
            
            
          }
        }
        
        if((count == 3) & (open == "YES")){
          
          visit_report_start2 <- visit_report_start[visit_report_start$X25.man == "NO",]
          
          for(j in 1:nrow(visit_report_start2))
          {
            
            #If position of ith row in box_visit is equal to jth row of visit_report_start2, run this
            if((box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start2$primary[j]),""))) == TRUE)
            {
              
              # If player set in lineup already is matched with in the visit_report_start2, Skip. You don't want that.
              if(box_visit$MLBId[ab] == visit_report_start2$MLBId[j])
              {
                if(j == nrow(visit_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(!(j == nrow(visit_report_start2)))
                {
                  next;
                }
              }
              
              # If a player set in lineup is matched with someone else that plays the same position, run this. 
              # Make sure he is not already set in the other part of lineup with a proper position.
              
              if(!(visit_report_start2$MLBId[j] %in% box_visit$MLBId) & (visit_report_start2$SB[j] > 0))
              {
                if(j == nrow(visit_report_start2))
                {
                  box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[j])
                  box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
                if(!(j == nrow(visit_report_start2)))
                {
                  box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[j])
                  box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
              }
              
              if(!(visit_report_start2$MLBId[j] %in% box_visit$MLBId) & (visit_report_start2$SB[j] == 0))
              {
                if(j == nrow(visit_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(j != nrow(visit_report_start2))
                {
                  next;
                }
              }
              
            }
            
            # If positions don't match skip to next row
            
            if((box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start2$primary[j]),""))) == FALSE)
            {
              if(j == nrow(visit_report_start2))
              {
                count <- count + 1
                next;
              }
              
              if(!(j == nrow(visit_report_start2)))
              {
                next;
              }
              
            }
            
            
          }
        }
        
        if((count == 4) & (open == "YES")){
          
          for(j in 1:nrow(visit_report_start))
          {
            visit_report_start2 <- visit_report_start[visit_report_start$X25.man == "NO",]
            
            #If position of ith row in box_visit is equal to jth row of visit_report_start2, run this
            if((box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start2$secondary[j]),""))) == TRUE)
            {
              
              # If player set in lineup already is matched with in the visit_report_start2, Skip. You don't want that.
              if(box_visit$MLBId[ab] == visit_report_start2$MLBId[j])
              {
                if(j == nrow(visit_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(!(j == nrow(visit_report_start2)))
                {
                  next;
                }
              }
              
              # If a player set in lineup is matched with someone else that plays the same position, run this. 
              # Make sure he is not already set in the other part of lineup with a proper position.
              
              if(!(visit_report_start2$MLBId[j] %in% box_visit$MLBId) & (visit_report_start2$SB[j] > 0))
              {
                if(j == nrow(visit_report_start2))
                {
                  box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[j])
                  box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
                if(!(j == nrow(visit_report_start2)))
                {
                  box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[j])
                  box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
              }
              
              if(!(visit_report_start2$MLBId[j] %in% box_visit$MLBId) & (visit_report_start2$SB[j] == 0))
              {
                if(j == nrow(visit_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(j != nrow(visit_report_start2))
                {
                  next;
                }
              }
              
            }
            
            # If positions don't match skip to next row
            
            if((box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start2$secondary[j]),""))) == FALSE)
            {
              if(j == nrow(visit_report_start2))
              {
                count <- count + 1
                next;
              }
              
              if(!(j == nrow(visit_report_start2)))
              {
                next;
              }
              
            }
            
            
          }
        }
        
        if((count == 5) & (open == "YES")){
          
          for(j in 1:nrow(visit_report_start))
          {
            visit_report_start2 <- visit_report_start[visit_report_start$X25.man == "NO",]
            
            #If position of ith row in box_visit is equal to jth row of visit_report_start2, run this
            if((box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start2$tertiary[j]),""))) == TRUE)
            {
              
              # If player set in lineup already is matched with in the visit_report_start2, Skip. You don't want that.
              if(box_visit$MLBId[ab] == visit_report_start2$MLBId[j])
              {
                if(j == nrow(visit_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(!(j == nrow(visit_report_start2)))
                {
                  next;
                }
              }
              
              # If a player set in lineup is matched with someone else that plays the same position, run this. 
              # Make sure he is not already set in the other part of lineup with a proper position.
              
              if(!(visit_report_start2$MLBId[j] %in% box_visit$MLBId) & (visit_report_start2$SB[j] > 0))
              {
                if(j == nrow(visit_report_start2))
                {
                  box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[j])
                  box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
                if(!(j == nrow(visit_report_start2)))
                {
                  box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[j])
                  box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[j])
                  count <- count + 1
                  open <- "NO"
                  break;
                }
                
              }
              
              if(!(visit_report_start2$MLBId[j] %in% box_visit$MLBId) & (visit_report_start2$SB[j] == 0))
              {
                if(j == nrow(visit_report_start2))
                {
                  count <- count + 1
                  next;
                }
                
                if(j != nrow(visit_report_start2))
                {
                  next;
                }
              }
              
            }
            
            # If positions don't match skip to next row
            
            if((box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start2$tertiary[j]),""))) == FALSE)
            {
              if(j == nrow(visit_report_start2))
              {
                count <- count + 1
                next;
              }
              
              if(!(j == nrow(visit_report_start2)))
              {
                next;
              }
              
            }
            
            
          }
        }
        
        pos_sim <- read.csv("pos_sim.csv",header = TRUE)
        
        if((count == 6) & (open == "YES"))
        {
          visit_report_start2 <- visit_report_start[visit_report_start$X25.man == "YES",]
          
          if(box_visit$POS2[ab] == "DH")
          {
            next;
          }
          
          if(box_visit$POS2[ab] != "DH")
          {
            col_number <- as.numeric(box_visit$POS2[ab])
          }
          
          position_similar <- pos_sim[,col_number]
          position_similar <- position_similar[!is.na(position_similar)]
          
          for(j in 2:length(position_similar))
          {
            for(l in 1:nrow(visit_report_start2))
            {
              # If position from "Position Similarity" is not in the position player's primary position, skip on.
              if(!position_similar[j] %in% unlist(strsplit(as.character(visit_report_start2$primary[l]),"")))
              {
                if(j == length(position_similar))
                {
                  count <- count + 1
                  break;
                }
                
                if(j != length(position_similar))
                {
                  next;
                }
                
              }
              
              # If position from "Position Similarity" is in the position player's primary position, run this.
              if(position_similar[j] %in% unlist(strsplit(as.character(visit_report_start2$primary[l]),"")))
              {
                # If a player in question was not already penciled in for the lineup and has a stat, run this.
                
                if((visit_report_start2$SB[l] > 0) & !(visit_report_start2$MLBId[l] %in% box_visit$MLBId))
                {
                  empty_slot <- which(box_visit$fullname == "")
                  
                  if(length(empty_slot) > 0)
                  {
                    # If player in question will be needed in the empty slot, run this
                    if(box_visit$POS2[empty_slot] %in% unlist(strsplit(as.character(visit_report_start2$primary[l]),"")))
                    {
                      if(j == length(position_similar))
                      {
                        count <- count + 1
                        next;
                      }
                      
                      if(j != length(position_similar))
                      {
                        next;
                      }
                    }
                    # If player in question won't be needed in the empty slot, run this
                    if(!box_visit$POS2[empty_slot] %in% unlist(strsplit(as.character(visit_report_start2$primary[l]),"")))
                    {
                      
                      if(j == length(position_similar))
                      {
                        box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[l])
                        box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[l])
                        count <- count + 1
                        open <- "NO"
                        break;
                      }
                      
                      if(j != length(position_similar))
                      {
                        box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[l])
                        box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[l])
                        count <- count + 1
                        open <- "NO"
                        break;
                      }
                    }
                  }
                }
                
                # If a player in question has no stats or already has been penciled into the lineup, run this.
                if((visit_report_start2$SB[l] == 0) | (visit_report_start2$MLBId[l] %in% box_visit$MLBId))
                {
                  next;
                }
              }
            }
          }
        } 
        
        if((count == 7) & (open == "YES"))
        {
          visit_report_start2 <- visit_report_start
          
          col_number <- as.numeric(box_visit$POS2[ab])
          position_similar <- pos_sim[,col_number]
          position_similar <- position_similar[!is.na(position_similar)]
          
          for(j in 2:length(position_similar))
          {
            for(l in 1:nrow(visit_report_start2))
            {
              # If position from "Position Similarity" is not in the position player's primary position, skip on.
              if(!position_similar[j] %in% unlist(strsplit(as.character(visit_report_start2$primary[l]),"")))
              {
                if(j == length(position_similar))
                {
                  count <- count + 1
                  break;
                }
                
                if(j != length(position_similar))
                {
                  next;
                }
                
              }
              
              # If position from "Position Similarity" is in the position player's primary position, run this.
              if(position_similar[j] %in% unlist(strsplit(as.character(visit_report_start2$primary[l]),"")))
              {
                # If a player in question was not already penciled in for the lineup and has a stat, run this.
                
                if((visit_report_start2$SB[l] > 0) & !(visit_report_start2$MLBId[l] %in% box_visit$MLBId))
                {
                  empty_slot <- which(box_visit$fullname == "")
                  
                  if(length(empty_slot) > 0)
                  {
                    # If player in question will be needed in the empty slot, run this
                    if(box_visit$POS2[empty_slot] %in% unlist(strsplit(as.character(visit_report_start2$primary[l]),"")))
                    {
                      if(j == length(position_similar))
                      {
                        count <- count + 1
                        next;
                      }
                      
                      if(j != length(position_similar))
                      {
                        next;
                      }
                    }
                    # If player in question won't be needed in the empty slot, run this
                    if(!box_visit$POS2[empty_slot] %in% unlist(strsplit(as.character(visit_report_start2$primary[l]),"")))
                    {
                      
                      if(j == length(position_similar))
                      {
                        box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[l])
                        box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[l])
                        count <- count + 1
                        open <- "NO"
                        break;
                      }
                      
                      if(j != length(position_similar))
                      {
                        box_visit$fullname[ab] <- as.character(visit_report_start2$PlayerName[l])
                        box_visit$MLBId[ab] <- as.character(visit_report_start2$MLBId[l])
                        count <- count + 1
                        open <- "NO"
                        break;
                      }
                    }
                  }
                }
                
                # If a player in question has no stats or already has been penciled into the lineup, run this.
                if((visit_report_start2$SB[l] == 0) | (visit_report_start2$MLBId[l] %in% box_visit$MLBId))
                {
                  next;
                }
              }
            }
          }
        }
        
        
        
      }
      
      # If he plays right position and has stats
      
      if(((visit_report_start$SB[visit_report_start$MLBId %in% box_visit$MLBId[ab]] > 0) & (box_visit$POS2[ab] %in% unlist(strsplit(as.character(visit_report_start$primary[visit_report_start$MLBId %in% box_visit$MLBId[ab]]),"")))))
      {
        next;
      }
    }
    
    
  }
  
  remaining_position_player25 <- visit_report_start[(visit_report_start$X25.man == "YES") & !(visit_report_start$MLBId %in% box_visit$MLBId) & (visit_report_start$SB > 0),]
  remaining_position_player40 <- visit_report_start[(visit_report_start$X25.man == "NO")  & !(visit_report_start$MLBId %in% box_visit$MLBId) & (visit_report_start$SB > 0),]
  
  remaining_position_player40$MLBId <- as.character(remaining_position_player40$MLBId)
  remaining_position_player25$MLBId <- as.character(remaining_position_player25$MLBId)
  
  for(f in 1:nrow(box_visit))
  {
    
    count2 <- 0
    open2 <- "YES"
    
    if(box_visit$fullname[f] != "")
    {
      next;
    }
    
    # If lineup is set empty, then get in someone that suits the position at ith row.
    
    if(box_visit$fullname[f] == "")
    {
      if((count2 == 0) & (open2 == "YES"))
      {
        for(g in 1:nrow(remaining_position_player25))
        {
          
          if((box_visit$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player25$primary[g]),""))) == TRUE)
          {
            
            if((g == nrow(remaining_position_player25)) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
            {
              box_visit$fullname[f] <- as.character(remaining_position_player25$PlayerName[g])
              box_visit$MLBId[f] <- remaining_position_player25$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
            if((g != nrow(remaining_position_player25)) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
            {
              box_visit$fullname[f] <- as.character(remaining_position_player25$PlayerName[g])
              box_visit$MLBId[f] <- remaining_position_player25$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"            
            }
            
          }
          
          if((box_visit$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player25$primary[g]),""))) == FALSE)
          {
            if(g == nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
            {
              count2 <- count2 + 1
              next;
            }
            
            if(g != nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
            {
              next;
            }
          }
          
        }
      }
      
      if((count2 == 1) & (open2 == "YES"))
      {
        for(g in 1:nrow(remaining_position_player25))
        {
          
          if((box_visit$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player25$secondary[g]),""))) == TRUE)
          {
            
            if(g == nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
            {
              box_visit$fullname[f] <- as.character(remaining_position_player25$PlayerName[g])
              box_visit$MLBId[f] <- as.character(remaining_position_player25$MLBId[g])
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
            if(g != nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
            {
              box_visit$fullname[f] <- as.character(remaining_position_player25$PlayerName[g])
              box_visit$MLBId[f] <- as.character(remaining_position_player25$MLBId[g])
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
          }
          
          if((box_visit$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player25$secondary[g]),""))) == FALSE)
          {
            if(g == nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
            {
              count2 <- count2 + 1
              next;
            }
            
            if(g != nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
            {
              next;
            }
          }
          
        }
        
      }
      
      if((count2 == 2) & (open2 == "YES"))
      {
        
        for(g in 1:nrow(remaining_position_player25))
        {
          
          if((box_visit$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player25$tertiary[g]),""))) == TRUE)
          {
            
            if(g == nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
            {
              box_visit$fullname[f] <- as.character(remaining_position_player25$PlayerName[g])
              box_visit$MLBId[f] <- remaining_position_player25$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO" 
            }
            
            if(g != nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
            {
              box_visit$fullname[f] <- as.character(remaining_position_player25$PlayerName[g])
              box_visit$MLBId[f] <- remaining_position_player25$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
          }
          
          if((box_visit$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player25$tertiary[g]),""))) == FALSE)
          {
            if(g == nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
            {
              count2 <- count2 + 1
              next;
            }
            
            if(g != nrow(remaining_position_player25) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
            {
              next;
            }
          }
          
        }
        
      }
      
      if((count2 == 3) & (open2 == "YES"))
      {
        for(g in 1:nrow(remaining_position_player40))
        {
          
          if((box_visit$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player40$primary[g]),""))) == TRUE)
          {
            if(nrow(remaining_position_player40) == 0)
            {
              next;
            }
            
            if(g == nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
            {
              box_visit$fullname[f] <- as.character(remaining_position_player40$PlayerName[g])
              box_visit$MLBId[f] <- remaining_position_player40$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
            if(g != nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
            {
              box_visit$fullname[f] <- as.character(remaining_position_player40$PlayerName[g])
              box_visit$MLBId[f] <- remaining_position_player40$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
          }
          
          if((box_visit$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player40$primary[g]),""))) == FALSE)
          {
            if(nrow(remaining_position_player40) == 0)
            {
              next;
            }
            
            
            if(g == nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
            {
              count2 <- count2 + 1
              next;
            }
            
            if(g != nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
            {
              next;
            }
          }
          
        }
        
      }
      
      if((count2 == 4) & (open2 == "YES"))
      {
        for(g in 1:nrow(remaining_position_player40))
        {
          
          if((box_visit$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player40$secondary[g]),""))) == TRUE)
          {
            if(nrow(remaining_position_player40) == 0)
            {
              next;
            }
            
            
            if(g == nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
            {
              box_visit$fullname[f] <- as.character(remaining_position_player40$PlayerName[g])
              box_visit$MLBId[f] <- remaining_position_player40$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
            if(g != nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
            {
              box_visit$fullname[f] <- as.character(remaining_position_player40$PlayerName[g])
              box_visit$MLBId[f] <- remaining_position_player40$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
          }
          
          if((box_visit$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player40$secondary[g]),""))) == FALSE)
          {
            if(nrow(remaining_position_player40) == 0)
            {
              next;
            }
            
            
            if(g == nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
            {
              count2 <- count2 + 1
              next;
            }
            
            if(g != nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
            {
              next;
            }
          }
          
        }
        
        
      }
      
      if((count2 == 5) & (open2 == "YES"))
      {
        
        for(g in 1:nrow(remaining_position_player40))
        {
          
          if((box_visit$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player40$tertiary[g]),""))) == TRUE)
          {
            if(nrow(remaining_position_player40) == 0)
            {
              next;
            }
            
            if(g == nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
            {
              box_visit$fullname[f] <- as.character(remaining_position_player40$PlayerName[g])
              box_visit$MLBId[f] <- remaining_position_player40$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
            if(g != nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
            {
              box_visit$fullname[f] <- as.character(remaining_position_player40$PlayerName[g])
              box_visit$MLBId[f] <- remaining_position_player40$MLBId[g]
              count2 <- count2 + 1
              open2 <- "NO"
            }
            
          }
          
          if((box_visit$POS2[f] %in% unlist(strsplit(as.character(remaining_position_player40$tertiary[g]),""))) == FALSE)
          {
            if(nrow(remaining_position_player40) == 0)
            {
              next;
            }
            
            if(g == nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
            {
              count2 <- count2 + 1
              next;
            }
            
            if(g != nrow(remaining_position_player40) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
            {
              next;
            }
          }
          
        }
        
      }
      
      if((count2 == 6) & (open2 == "YES"))
      {
        
        col_number <- as.numeric(box_visit$POS2[f])
        position_similar <- pos_sim[,col_number]
        position_similar <- position_similar[!is.na(position_similar)]
        
        for(j in 2:length(position_similar))
        {
          for(l in 1:nrow(remaining_position_player25))
          {
            # If position from "Position Similarity" is not in the position player's primary position, skip on.
            if(!position_similar[j] %in% unlist(strsplit(as.character(remaining_position_player25$primary[l]),"")))
            {
              if(j == length(position_similar))
              {
                count2 <- count2 + 1
              }
              
              if(j != length(position_similar))
              {
                next;
              }
              
            }
            
            # If position from "Position Similarity" is in the position player's primary position, run this.
            if(position_similar[j] %in% unlist(strsplit(as.character(remaining_position_player25$primary[l]),"")))
            {
              # If a player in question was not already penciled in for the lineup and has a stat, run this.
              
              if((remaining_position_player25$SB[l] > 0) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
              {
                empty_slot <- which(box_visit$fullname == "")
                
                if(length(empty_slot) > 0)
                {
                  # If player in question will be needed in the empty slot, run this
                  if(box_visit$POS2[empty_slot] %in% unlist(strsplit(as.character(remaining_position_player25$primary[l]),"")))
                  {
                    if(j == length(position_similar))
                    {
                      count2 <- count2 + 1
                    }
                    
                    if(j != length(position_similar))
                    {
                      next;
                    }
                  }
                  # If player in question won't be needed in the empty slot, run this
                  if(!box_visit$POS2[empty_slot] %in% unlist(strsplit(as.character(remaining_position_player25$primary[l]),"")))
                  {
                    
                    if(j == length(position_similar) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
                    {
                      box_visit$fullname[f] <- as.character(remaining_position_player25$PlayerName[l])
                      box_visit$MLBId[f] <- as.character(remaining_position_player25$MLBId[l])
                      count2 <- count2 + 1
                      open2 <- "NO"
                    }
                    
                    if(j != length(position_similar) & !(remaining_position_player25$MLBId[g] %in% box_visit$MLBId))
                    {
                      box_visit$fullname[f] <- as.character(remaining_position_player25$PlayerName[l])
                      box_visit$MLBId[f] <- as.character(remaining_position_player25$MLBId[l])
                      count2 <- count2 + 1
                      open2 <- "NO"
                    }
                  }
                }
              }
              
              # If a player in question has no stats or already has been penciled into the lineup, run this.
              if((remaining_position_player25$SB[l] == 0) | (box_visit$MLBId[f] %in% remaining_position_player25$MLBId))
              {
                next;
              }
            }
          }
        }
        
        
        position_player_lineup <- visit_report_start[(visit_report_start$X25.man == "YES") & (visit_report_start$MLBId %in% box_visit$MLBId) & (visit_report_start$SB > 0),]
        
        # This code will shuffle around lineups in case someone who plays out of position can be switched with someone else in the lineup to eliminate
        # "Out of position" placement.
        
        if(box_visit$MLBId[f] == "")
        {
          next;
        }
        
        if(!unlist(strsplit(as.character(visit_report_start$Pos[visit_report_start$MLBId %in% box_visit$MLBId[f]],""),"")) %in% box_visit$POS2[f])
        {
          for(d in 1:nrow(position_player_lineup))
          {
            
            if(box_visit$POS2[f] %in% unlist(strsplit(as.character(position_player_lineup$Pos[d]),"")))
            {
              box_visit$fullname[which(box_visit$MLBId %in% position_player_lineup$MLBId[d])] <- ""
              box_visit$MLBId[which(box_visit$MLBId %in% position_player_lineup$MLBId[d])] <- ""
              
              box_visit$fullname[f] <- as.character(position_player_lineup$PlayerName[d])
              box_visit$MLBId[f] <- as.character(position_player_lineup$MLBId[d])
              
            }
            
          }
        }
        
      }
      
      if((count2 == 7) & (open2 == "YES"))
      {
        
        col_number <- as.numeric(box_visit$POS2[f])
        position_similar <- pos_sim[,col_number]
        position_similar <- position_similar[!is.na(position_similar)]
        
        for(j in 2:length(position_similar))
        {
          for(l in 1:nrow(remaining_position_player40))
          {
            # If position from "Position Similarity" is not in the position player's primary position, skip on.
            if(!position_similar[j] %in% unlist(strsplit(as.character(remaining_position_player40$primary[l]),"")))
            {
              if(j == length(position_similar))
              {
                count2 <- count2 + 1
              }
              
              if(j != length(position_similar))
              {
                next;
              }
              
            }
            
            # If position from "Position Similarity" is in the position player's primary position, run this.
            if(position_similar[j] %in% unlist(strsplit(as.character(remaining_position_player40$primary[l]),"")))
            {
              # If a player in question was not already penciled in for the lineup and has a stat, run this.
              
              if((remaining_position_player40$SB[l] > 0) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
              {
                empty_slot <- which(box_visit$fullname == "")
                
                if(length(empty_slot) > 0)
                {
                  # If player in question will be needed in the empty slot, run this
                  if(box_visit$POS2[empty_slot] %in% unlist(strsplit(as.character(remaining_position_player40$primary[l]),"")))
                  {
                    if(j == length(position_similar))
                    {
                      count2 <- count2 + 1
                    }
                    
                    if(j != length(position_similar))
                    {
                      next;
                    }
                  }
                  # If player in question won't be needed in the empty slot, run this
                  if(!box_visit$POS2[empty_slot] %in% unlist(strsplit(as.character(remaining_position_player40$primary[l]),"")))
                  {
                    
                    if(j == length(position_similar) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
                    {
                      box_visit$fullname[f] <- as.character(remaining_position_player40$PlayerName[l])
                      box_visit$MLBId[f] <- as.character(remaining_position_player40$MLBId[l])
                      count2 <- count2 + 1
                      open2 <- "NO"
                    }
                    
                    if(j != length(position_similar) & !(remaining_position_player40$MLBId[g] %in% box_visit$MLBId))
                    {
                      box_visit$fullname[f] <- as.character(remaining_position_player40$PlayerName[l])
                      box_visit$MLBId[f] <- as.character(remaining_position_player40$MLBId[l])
                      count2 <- count2 + 1
                      open2 <- "NO"
                    }
                  }
                }
              }
              
              # If a player in question has no stats or already has been penciled into the lineup, run this.
              if((remaining_position_player40$SB[l] == 0) | (remaining_position_player40$MLBId[l] %in% box_visit$MLBId))
              {
                next;
              }
            }
          }
        }
        
        position_player_lineup <- visit_report_start[(visit_report_start$X25.man == "YES") & (visit_report_start$MLBId %in% box_visit$MLBId) & (visit_report_start$SB > 0),]
        
        # This code will shuffle around lineups in case someone who plays out of position can be switched with someone else in the lineup to eliminate
        # "Out of position" placement.
        
        if(!unlist(strsplit(as.character(visit_report_start$Pos[visit_report_start$MLBId %in% box_visit$MLBId[f]],""),"")) %in% box_visit$POS2[f])
        {
          for(d in 1:nrow(position_player_lineup))
          {
            
            if(box_visit$POS2[f] %in% unlist(strsplit(as.character(position_player_lineup$Pos[d]),"")))
            {
              box_visit$fullname[which(box_visit$MLBId %in% position_player_lineup$MLBId[d])] <- ""
              box_visit$MLBId[which(box_visit$MLBId %in% position_player_lineup$MLBId[d])] <- ""
              
              box_visit$fullname[f] <- as.character(position_player_lineup$PlayerName[d])
              box_visit$MLBId[f] <- as.character(position_player_lineup$MLBId[d])
              
            }
            
          }
        }
        
      }
    }
  }
  
  box_visit$SB <- ""
  
  visit_report_start$SB <- visit_report_start$Start + visit_report_start$Bench
  
  for(h in 1:nrow(box_visit))
  {
    if(box_visit$fullname[h] != "")
    {
      box_visit$SB[h] <- visit_report_start$SB[visit_report_start$MLBId %in% box_visit$MLBId[h]]
      
    }
    
    if(box_visit$fullname[h] == "")
    {
      next;
    }
  }
  
  for(z in 1:nrow(box_visit))
  {
    if((box_visit$SB[z] == 0) & (box_visit$POS2[z] == 1))
    {
      print("Don't erase pitchers")
    }
    
    if((box_visit$SB[z] == 0) & (box_visit$POS2[z] %in% c(2,3,4,5,6,7,8,9))){
      box_visit$fullname[z] <- ""
      box_visit$MLBId[z] <- "" 
    }
  }
  
  ######
  
  pitching_SP <- read.csv("Pitching/Pitching_Master_Starts2.csv")
  
  for(v in 1:ncol(pitching_SP))
  {
    if(v %in% c(6:38))
    {
      pitching_SP[,v] <- as.numeric(pitching_SP[,v])
    }
    
    if(v %in% c(2,3,5,39:44))
    {
      
    }
    
    if(v %in% c(1))
    {
      pitching_SP[,v] <- as.Date(pitching_SP[,v],format="%Y-%m-%d")
    }
    
    
  }
  
  pitching_RP <- read.csv("Pitching/Pitching_Master_RP2.csv")
  
  pitching_RP$LW <- as.numeric(pitching_RP$LW)
  pitching_SP$LW <- as.numeric(pitching_SP$LW)
  
  for(v in 1:ncol(pitching_RP))
  {
    if(v %in% c(6:38))
    {
      pitching_RP[,v] <- as.numeric(pitching_RP[,v])
    }
    
    if(v %in% c(2,3,5,39:44))
    {
      
    }
    
    if(v %in% c(1))
    {
      pitching_RP[,v] <- as.Date(pitching_RP[,v],format="%Y-%m-%d")
    }
    
  }
  
  # Exclude position player pitching
  
  pitching_RP <- pitching_RP[!pitching_RP$MLBId %in% lineup$MLBId[lineup$Role %in% c("Bench", "#1","#2","3","4","5","6","7","8","9")],]
  
  # Check to see if pitcher slated to start does have start available
  if(((lineup$fullname[which((lineup$Team %in% final_schedule$Home[x]) & (lineup$Date %in% formatted_date) & (lineup$POS %in% "SP"))]) %in% pitching_SP$PlayerName)){
    home_starter <- lineup$fullname[which((lineup$Team %in% final_schedule$Home[x]) & (lineup$Date %in% formatted_date) & (lineup$POS %in% "SP"))]
    box_home$fullname[box_home$POS %in% c("P")] <- home_starter
    box_home$MLBId[box_home$fullname %in% home_starter] <- lineup$MLBId[lineup$fullname %in% home_starter]
  }
  
  if((lineup$fullname[which((lineup$Team %in% final_schedule$Away[x]) & (lineup$Date %in% formatted_date) & (lineup$POS %in% "SP"))]) %in% pitching_SP$PlayerName){
    visit_starter <- lineup$fullname[which((lineup$Team %in% final_schedule$Away[x]) & (lineup$Date %in% formatted_date) & (lineup$POS %in% "SP"))]
    box_visit$fullname[which(box_visit$POS %in% c("P"))] <- visit_starter
    box_visit$MLBId[box_visit$fullname %in% visit_starter] <- lineup$MLBId[lineup$fullname %in% visit_starter]
  }
  
  # In case POS column has "RP" instead of "SP for the rotations, it will correct it.
  
  lineup$POS[which(lineup$Role %in% c("SP1","SP2","SP3","SP4","SP5"))] <- "SP"
  
  # If pitcher slated to start, according to master line up, does not have start available, pick starter with most rest and start available
  
  if(!((lineup$fullname[which((lineup$Team %in% final_schedule$Home[x]) & (lineup$Date %in% formatted_date) & (lineup$POS %in% "SP"))]) %in% pitching_SP$PlayerName))
  {
    # Home starters with starts available
    
    starter_home_available <- unique(pitching_SP$MLBId[(pitching_SP$MLBId %in% (lineup$MLBId[(lineup$POS == "SP") & (lineup$Team == final_schedule$Home[x])]))])
    
    # Home starter with proper rest
    
    home_starter_rest <- pitcher_use[pitcher_use$MLBId %in% starter_home_available,]
    
    fullname_home <- unique(home_starter_rest$fullname)
    
    rest_day <- vector()
    
    for(i in 1:length(fullname_home))
    {
      rest_day[i] <- as.numeric(formatted_date - max(pitcher_use$date_played[which(pitcher_use$fullname %in% fullname_home[i])]))
      
    }
    
    home_starter <- fullname_home[max(which(rest_day == max(rest_day)))]
    
    if(final_schedule$Home[x] %in% NL){
      box_home$fullname[which(box_home$POS %in% c("1","P"))] <- home_starter
      box_home$MLBId[which(box_home$POS %in% c("1","P"))] <- lineup$MLBId[min(which(lineup$fullname %in% home_starter))]
    }
    
    if(final_schedule$Home[x] %in% AL){
      print("Game not taking place in AL ballparks. So no need to do this")
    }
    
  }
  
  if(!((lineup$fullname[which((lineup$Team %in% final_schedule$Away[x]) & (lineup$Date %in% formatted_date) & (lineup$POS %in% "SP"))]) %in% pitching_SP$PlayerName))
  {
    # Home starters with starts available
    
    starter_visit_available <- unique(pitching_SP$MLBId[(pitching_SP$MLBId %in% (lineup$MLBId[(lineup$POS == "SP") & (lineup$Team == final_schedule$Away[x])]))])
    
    # Home starter with proper rest
    
    visit_starter_rest <- pitcher_use[pitcher_use$MLBId %in% starter_visit_available,]
    
    fullname_visit <- unique(visit_starter_rest$fullname)
    
    rest_day <- vector()
    
    for(i in 1:length(fullname_visit))
    {
      rest_day[i] <- as.numeric(formatted_date - max(pitcher_use$date_played[which(pitcher_use$fullname %in% fullname_visit[i])]))
      
    }
    
    visit_starter <- fullname_visit[max(which(rest_day == max(rest_day)))]
    
    if(final_schedule$Home[x] %in% NL){
      box_visit$fullname[which(box_visit$POS %in% c("1","P"))] <- visit_starter
      box_visit$MLBId[which(box_visit$POS %in% c("1","P"))] <- lineup$MLBId[min(which(lineup$fullname %in% visit_starter))]
    }
    
    if(final_schedule$Home[x] %in% AL){
      print("Game not taking place in AL ballparks. So no need to do this")
    }
  }
  
  away_sp <- visit_starter
  home_sp <- home_starter
  
  away_sp_stat <- pitching_SP[pitching_SP$PlayerName %in% away_sp,]
  away_sp_stat <- away_sp_stat[away_sp_stat$GameDate %in% max(away_sp_stat$GameDate),]
  
  home_sp_stat <- pitching_SP[pitching_SP$PlayerName %in% home_sp,]
  home_sp_stat <- home_sp_stat[home_sp_stat$GameDate %in% max(home_sp_stat$GameDate),]
  
  
  box_home$POS[which(box_home$POS == "SP")] <- sub("SP", "1", box_home$POS[which(box_home$POS == "SP")])
  box_home$POS[which(box_home$POS == "RP")] <- sub("RP", "1", box_home$POS[which(box_home$POS == "RP")])
  box_home$POS[which(box_home$POS == "CA")] <- sub("CA", "2", box_home$POS[which(box_home$POS == "CA")])
  box_home$POS[which(box_home$POS == "1B")] <- sub("1B", "3", box_home$POS[which(box_home$POS == "1B")])
  box_home$POS[which(box_home$POS == "2B")] <- sub("2B", "4", box_home$POS[which(box_home$POS == "2B")])
  box_home$POS[which(box_home$POS == "3B")] <- sub("3B", "5", box_home$POS[which(box_home$POS == "3B")])
  box_home$POS[which(box_home$POS == "SS")] <- sub("SS", "6", box_home$POS[which(box_home$POS == "SS")])
  box_home$POS[which(box_home$POS == "LF")] <- sub("LF", "7", box_home$POS[which(box_home$POS == "LF")])
  box_home$POS[which(box_home$POS == "CF")] <- sub("CF", "8", box_home$POS[which(box_home$POS == "CF")])
  box_home$POS[which(box_home$POS == "RF")] <- sub("RF", "9", box_home$POS[which(box_home$POS == "RF")])
  box_home$POS[which(box_home$POS == "P")] <- sub("P", "1", box_home$POS[which(box_home$POS == "P")])
  
  
  box_visit$POS[which(box_visit$POS == "SP")] <- sub("SP", "1", box_visit$POS[which(box_visit$POS == "SP")])
  box_visit$POS[which(box_visit$POS == "RP")] <- sub("RP", "1", box_visit$POS[which(box_visit$POS == "RP")])
  box_visit$POS[which(box_visit$POS == "CA")] <- sub("CA", "2", box_visit$POS[which(box_visit$POS == "CA")])
  box_visit$POS[which(box_visit$POS == "1B")] <- sub("1B", "3", box_visit$POS[which(box_visit$POS == "1B")])
  box_visit$POS[which(box_visit$POS == "2B")] <- sub("2B", "4", box_visit$POS[which(box_visit$POS == "2B")])
  box_visit$POS[which(box_visit$POS == "3B")] <- sub("3B", "5", box_visit$POS[which(box_visit$POS == "3B")])
  box_visit$POS[which(box_visit$POS == "SS")] <- sub("SS", "6", box_visit$POS[which(box_visit$POS == "SS")])
  box_visit$POS[which(box_visit$POS == "LF")] <- sub("LF", "7", box_visit$POS[which(box_visit$POS == "LF")])
  box_visit$POS[which(box_visit$POS == "CF")] <- sub("CF", "8", box_visit$POS[which(box_visit$POS == "CF")])
  box_visit$POS[which(box_visit$POS == "RF")] <- sub("RF", "9", box_visit$POS[which(box_visit$POS == "RF")])
  box_visit$POS[which(box_visit$POS == "P")] <- sub("P", "1", box_visit$POS[which(box_visit$POS == "P")])
  
  
  # Create 9 columns that are empty. Each columns are name as following:
  # LW, Bonus, Bases Taken, Outs On Base, Field, E, Zone, Block, Frame
  
  stats_visit <- data.frame(matrix("",nrow=nrow(box_visit), ncol=9))
  
  stats_col <- c("LW","Bonus","Bases_Taken","Outs_On_Base","Field","E","Zone","Block","Frame")
  
  colnames(stats_visit) <- stats_col
  
  # Call batting master files
  
  batting_bench <- read.csv("Batting/Batting_Bench2.csv")
  
  batting_bench$uniqueId <- paste(batting_bench$MLBId, batting_bench$GameString)
  
  
  batting_pinch <- read.csv("Batting/Batting_Master_Pinch2.csv")
  
  batting_pinch$uniqueId <- paste(batting_pinch$MLBId, batting_pinch$GameString)
  
  
  batting_start <- read.csv("Batting/Batting_Master_Starts2.csv")
  
  batting_start$uniqueId <- paste(batting_start$MLBId, batting_start$GameString)
  
  batting_bench$LW <- as.numeric(batting_bench$LW)
  batting_start$LW <- as.numeric(batting_start$LW)
  batting_pinch$LW <- as.numeric(batting_pinch$LW)
  
  for(v in 1:ncol(batting_bench))
  {
    if(v %in% c(7:39))
    {
      batting_bench[,v] <- as.numeric(batting_bench[,v])
      batting_pinch[,v] <- as.numeric(batting_pinch[,v])
      batting_start[,v] <- as.numeric(batting_start[,v])
    }
    
    if(v %in% c(2:5,40:43))
    {
      batting_bench[,v] <- as.character(batting_bench[,v])
      batting_pinch[,v] <- as.character(batting_pinch[,v])
      batting_start[,v] <- as.character(batting_start[,v])
    }
    
    if(v %in% c(1))
    {
      batting_bench[,v] <- as.Date(batting_bench[,v],format="%Y-%m-%d")
      batting_pinch[,v] <- as.Date(batting_pinch[,v],format="%Y-%m-%d")
      batting_start[,v] <- as.Date(batting_start[,v],format="%Y-%m-%d")
    }
    
    
  }
  
  # Call Fielding master files
  
  fielding_available <- read.csv("Fielding/Fielding_Master_Available2.csv")
  
  fielding_available$uniqueId <- paste(fielding_available$MLBId, fielding_available$GameString, sep=" ")
  
  fielding_used <- read.csv("Fielding/Fielding_Master_Used2.csv")
  
  fielding_used <- fielding_used[,c("Used","LastName","FirstName","GameDate","GameId","GameString","MLBId","PlayerName","PlayerId","Team","TeamNbr","LW","Pos",
                                    "Position","PrevDayGamesPlayed","G","GS","INN","PO","A","E","DP","TP","PB","FPctP","SB","CS","PKOF","FPctC","Outs","Chances","Zone",
                                    "FPct","Pivots2B","Cint","uniqueId")]
  
  fielding_available$LW <- as.numeric(as.character(fielding_available$LW))
  fielding_used$LW <- as.numeric(as.character(fielding_used$LW))
  
  
  for(v in 1:ncol(fielding_available))
  {
    if(v %in% c(11,13,15:35))
    {
      fielding_available[,v] <- as.numeric(fielding_available[,v])
      fielding_used[,v] <- as.numeric(fielding_used[,v])
      
    }
    
    if(v %in% c(1:3,5:10,14,36))
    {
      fielding_available[,v] <- as.character(fielding_available[,v])
      fielding_used[,v] <- as.character(fielding_used[,v])
      
    }
    
    if(v %in% c(4))
    {
      fielding_available[,v] <- as.Date(fielding_available[,v],format="%Y-%m-%d")
      fielding_used[,v] <- as.Date(fielding_used[,v],format="%Y-%m-%d")
      
    }
    
  }
  
  # Call Basrunning master files
  
  base_available <- read.csv("Baserunning/Baserunning_Master_Available2.csv")
  
  base_available$uniqueId <- paste(base_available$MLBId, base_available$GameString)
  
  base_used <- read.csv("Baserunning/Baserunning_Master_Used2.csv")
  
  for(v in 1:ncol(base_available))
  {
    if(v %in% c(1:3,7:12,34))
    {
      base_available[,v] <- as.character(base_available[,v])
      base_used[,v] <- as.character(base_used[,v])
      
    }
    
    if(v %in% c(5,6,13:33))
    {
      base_available[,v] <- as.numeric(base_available[,v])
      base_used[,v] <- as.numeric(base_used[,v])
      
    }
    
    if(v %in% c(4))
    {
      base_available[,v] <- as.Date(base_available[,v])
      base_used[,v] <- as.Date(base_used[,v])
      
    }
    
  }
  
  # Call YTD master files
  
  ytd <- read.csv("YTD_Fielding/YTD_Fielding2.csv")
  
  
  ###START_VISIT###
  
  batting_col <- c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                   "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                   "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")
  
  box_stat_visit <- data.frame(matrix(NA, nrow = 1, ncol = length(batting_col)))
  
  colnames(box_stat_visit) <- batting_col
  
  box_stat_visit$LW <- as.numeric(as.character(box_stat_visit$LW))
  
  for(v in 1:ncol(box_stat_visit))
  {
    if(v %in% c(1,3:4,39:44))
    {
      box_stat_visit[,v] <- as.character(box_stat_visit[,v])
      
    }
    
    if(v %in% c(6:38))
    {
      box_stat_visit[,v] <- as.numeric(box_stat_visit[,v])
      
      
    }
    
    if(v %in% c(2))
    {
      box_stat_visit[,v] <- as.Date(box_stat_visit[,v],format="%Y-%m-%d")
      
      
    }
    
  }
  
  
  pos_sim <- read.csv("pos_sim.csv",header = TRUE)
  
  # Load Visitor's Manager Report
  
  report_visit <- read.csv(paste("report/bat/",date,"/",final_schedule$Away[x],date,"_batting_report.csv",sep=""))
  
  
  
  report_visit$POS <- ""
  
  # 
  
  for(k in 1:nrow(report_visit))
  {
    if(report_visit$MLBId[k] %in% lineup$MLBId)
    {
      report_visit$POS[k] <- lineup$POS2[lineup$MLBId %in% report_visit$MLBId[k]]
    }
    
    if(!(report_visit$MLBId[k] %in% lineup$MLBId))
    {
      report_visit$POS[k] <- ""
    }
  }
  
  report_visit$playable <- ""
  
  report_visit$playable <- report_visit$Start + report_visit$Bench
  
  not_available <- report_visit$MLBId[which(report_visit$playable == 0)]
  
  replace <- box_visit$MLBId[box_visit$MLBId %in% not_available]
  
  lineup_replace <- which(box_visit$MLBId %in% replace)
  
  bench <- lineup$MLBId[(lineup$Role == "Bench") & (lineup$Team == final_schedule$Away[x])]
  
  report_bench <- report_visit[report_visit$MLBId %in% bench,]
  
  report_bench <- report_bench[!report_bench$playable == 0,]
  
  if(length(lineup_replace) == 0){
    
    print("Lineup required no modification")
    
  }
  
  if(length(lineup_replace) > 0)
  {
    for(l in 1:length(lineup_replace))
    {
      if(box_visit$POS[lineup_replace[l]] == 1)
      {
        next;
      }
      
      if(box_visit$POS[lineup_replace[l]] == "DH")
      {
        available_DH <- report_visit$MLBId[which((report_visit$Start > 0) | (report_visit$Bench > 0))]
        available_DHs <- available_DH[!(available_DH %in% box_visit$MLBId)]
        
        report_visit2 <- report_visit[which(report_visit$MLBId %in% available_DHs),]
        report_visit2 <- report_visit2[report_visit2$X25.man == "YES",]
        
        selected_DH <- report_visit2[sample(1:nrow(report_visit2), size = 1, replace = FALSE),]
        
        box_visit$fullname[which(box_visit$POS == "DH")] <- as.character(selected_DH$PlayerName)
        box_visit$MLBId[which(box_visit$POS == "DH")] <- as.character(selected_DH$MLBId)
        
        next;
      }
      
      col_num <- as.numeric(box_visit$POS2[lineup_replace[l]])
      
      pos_similar <- pos_sim[,col_num]
      
      pos_similar <- pos_similar[!is.na(pos_similar)]
      
      for(m in 1:length(pos_similar))
      {
        if(pos_similar[m] %in% report_bench$POS)
        {
          box_visit$fullname[lineup_replace[l]] <- as.character(report_bench$PlayerName[which(report_bench$POS %in% pos_similar[m])])
          
          chosen_player <- as.character(report_bench$PlayerName[which(report_bench$POS %in% pos_similar[m])])
          
          box_visit$MLBId[lineup_replace[l]] <- as.character(report_bench$MLBId[which(report_bench$POS %in% pos_similar[m])])
          
          report_bench <- report_bench[!report_bench$PlayerName %in% chosen_player,]
          
          break;
        }
        
        if(switch == "Yes")
          if(!(pos_similar[m] %in% report_bench$POS))
          {
            if(m == length(pos_similar))
            {
              stop(paste("You need to give MLE in place of ", box_visit$fullname[lineup_replace[l]], " because there is no one who can replace him on BENCH",sep=" "))
            }
            next;
          }
        
        
      }
      
      
      
    }
  }
  
  # Load Home's Manager Report
  
  report_home <- read.csv(paste("report/bat/",date,"/",final_schedule$Home[x],date,"_batting_report.csv",sep=""))
  
  
  
  report_home$POS <- ""
  
  # 
  
  for(k in 1:nrow(report_home))
  {
    if(report_home$MLBId[k] %in% lineup$MLBId)
    {
      report_home$POS[k] <- lineup$POS2[lineup$MLBId %in% report_home$MLBId[k]]
    }
    
    if(!(report_home$MLBId[k] %in% lineup$MLBId))
    {
      report_home$POS[k] <- ""
    }
  }
  
  report_home$playable <- ""
  
  report_home$playable <- report_home$Start + report_home$Bench
  
  not_available <- report_home$MLBId[which(report_home$playable == 0)]
  
  replace <- box_home$MLBId[box_home$MLBId %in% not_available]
  
  lineup_replace <- which(box_home$MLBId %in% replace)
  
  bench <- lineup$MLBId[(lineup$Role == "Bench") & (lineup$Team == final_schedule$Home[x])]
  
  report_bench <- report_home[report_home$MLBId %in% bench,]
  
  report_bench <- report_bench[!report_bench$playable == 0,]
  
  if(length(lineup_replace) == 0){
    
    print("Lineup required no modification")
    
  }
  
  if(length(lineup_replace) > 0)
  {
    for(l in 1:length(lineup_replace))
    {
      if(box_home$POS[lineup_replace[l]] == 1)
      {
        next;
      }
      
      if(box_home$POS[lineup_replace[l]] == "DH")
      {
        available_DH <- report_home$MLBId[which((report_home$Start > 0) | (report_home$Bench > 0))]
        available_DHs <- available_DH[!(available_DH %in% box_home$MLBId)]
        
        report_home2 <- report_home[which(report_home$MLBId %in% available_DHs),]
        report_home2 <- report_home2[report_home2$X25.man == "YES",]
        
        selected_DH <- report_home2[sample(1:nrow(report_home2), size = 1, replace = FALSE),]
        
        box_home$fullname[which(box_home$POS == "DH")] <- as.character(selected_DH$PlayerName)
        box_home$MLBId[which(box_home$POS == "DH")] <- as.character(selected_DH$MLBId)
        
        next;
      }
      
      col_num <- as.numeric(box_home$POS2[lineup_replace[l]])
      
      pos_similar <- pos_sim[,col_num]
      
      pos_similar <- pos_similar[!is.na(pos_similar)]
      
      for(m in 1:length(pos_similar))
      {
        if(pos_similar[m] %in% report_bench$POS)
        {
          box_home$fullname[lineup_replace[l]] <- as.character(report_bench$PlayerName[which(report_bench$POS %in% pos_similar[m])])
          
          chosen_player <- as.character(report_bench$PlayerName[which(report_bench$POS %in% pos_similar[m])])
          
          box_home$MLBId[lineup_replace[l]] <- as.character(report_bench$MLBId[which(report_bench$POS %in% pos_similar[m])])
          
          report_bench <- report_bench[!report_bench$PlayerName %in% chosen_player,]
          
          break;
        }
        
        if(switch == "Yes")
        {
          if(!(pos_similar[m] %in% report_bench$POS))
          {
            if(m == length(pos_similar))
            {
              stop(paste("You need to give MLE in place of ", box_home$fullname[lineup_replace[l]], " because there is no one who can replace him on BENCH",sep=" "))
            }
            next;
          }
        }
        
      }
      
      
      
    }
  }
  
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
        
        next;
        
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
        
        next;
        
      }
      
      if((box_visit$MLBId[i] %in% batting_pinch$MLBId) & !(box_visit$MLBId[i] %in% batting_start$MLBId) & !(box_visit$MLBId[i] %in% batting_bench$MLBId))
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
        
        next;
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
          MLE_fill <- c("P",format(away_sp_stat$GameDate, "%Y-%m-%d"), as.character(away_sp_stat$FirstName), as.character(away_sp_stat$LastName), -0.50, 0, 0, 0 , 0, 0, 0 , "", "",
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
          MLE_fill <- c("P",format(away_sp_stat$GameDate, "%Y-%m-%d"), as.character(away_sp_stat$FirstName), as.character(away_sp_stat$LastName), -0.25, 0, 0, 0 , 0, 0, 0 , "", "",
                        1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,3,as.character(away_sp_stat$MLBId),as.character(away_sp_stat$PlayerName),as.character(away_sp_stat$GameString),as.character(away_sp_stat$GameId),as.character(away_sp_stat$uniqueId), "")
          
          for(n in 1:length(MLE_fill))
          {
            MLE[1,n] <- MLE_fill[n]
          }
          
          box_stat_visit <- rbind(box_stat_visit, MLE)        
        }
        
        next;
        
      }
    }
    
    
    
  }
  
  box_stat_visit <- box_stat_visit[!(box_stat_visit$PlayerName %in% c("",NA)),]
  
  count_2 <- which(box_stat_visit$PA %in% 2)
  count_1 <- which(box_stat_visit$PA %in% 1)
  
  blanking <- data.frame(matrix("", nrow = 1, ncol=length(colnames(box_stat_visit))))
  colnames(blanking) <- colnames(box_stat_visit)
  blanking$GameDate <- as.Date(blanking$GameDate, format="%Y-%m-%d")
  box_stat_visit <- rbind(box_stat_visit,blanking)
  
  if(length(count_1) == 0)
  {
    print("No need to pull in pinch hitter data")
  }
  
  if(length(count_1) > 0)
  {
    for(s in 1:length(count_1))
    {
      POS <- box_stat_visit$POS[count_1[s]]
      team_name <- final_schedule$Away[x]
      
      batting_bench2 <- batting_bench
      batting_pinch2 <- batting_pinch
      batting_start2 <- batting_start
      
      fielding_available2 <- fielding_available
      base_available2 <- base_available
      
      if(POS %in% "1"){
        
        box_stat_visit$MLBId <- as.character(box_stat_visit$MLBId)
        
        unplayed <- only_active_players$MLBId[(only_active_players$Team_RFB %in% final_schedule$Away[x]) & !(only_active_players$MLBId %in% box_stat_visit$MLBId)]
        
        unplayed <- unique(unplayed)
        
        bat_pinch2 <- batting_pinch2[(batting_pinch2$MLBId %in% unplayed),]
        
        bat_pinch2$GameDate <- as.character(bat_pinch2$GameDate)
        
        bat_pinch2 <- bat_pinch2[!(bat_pinch2$MLBId %in% box_stat_visit$MLBId),]
        
        bat_pinch2 <- bat_pinch2[!(bat_pinch2$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Away[x] & lineup$POS %in% c("SP","RP")]),]
        
        bat_pinch2 <- bat_pinch2[bat_pinch2$MLBId %in% lineup$MLBId,]
        
        stat <- bat_pinch2
        
        if(nrow(stat) > 0){
          
          stat$POS <- ""
          
          stat$POS <- "PH"
          
          stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                          "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                          "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
          
          stat <- stat[order(stat$GameDate, decreasing = TRUE),]
          
          stat <- stat[1,]
          
          box_stat_visit <- rbind(box_stat_visit,stat)
          
        }
        
        unplayed <- only_active_players$MLBId[(only_active_players$Team_RFB %in% final_schedule$Away[x]) & !(only_active_players$MLBId %in% box_stat_visit$MLBId)]
        
        unplayed <- unique(unplayed)
        
        bat_pinch2 <- batting_pinch2[(batting_pinch2$MLBId %in% unplayed),]
        
        bat_pinch2 <- bat_pinch2[bat_pinch2$MLBId %in% lineup$MLBId,]
        
        bat_pinch2$GameDate <- as.character(bat_pinch2$GameDate)
        
        stat <- bat_pinch2
        
        if(nrow(stat) > 0){
          
          stat$POS <- ""
          
          stat$POS <- "PH"
          
          stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                          "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                          "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
          
          stat <- stat[1,]
          
          
        }
        
        if(nrow(stat) == 0)
        {
          print("No pinch hitter to replace pitcher")
        }
        box_stat_visit <- rbind(box_stat_visit,stat)
        
      }
      
      if(!(POS %in% "1")){
        fielder_available <- fielding_available2[which((fielding_available2$Team %in% team_name) & (fielding_available2$Pos %in% c(POS))),]
        
        fielder_available <- fielder_available[(fielder_available$MLBId %in% lineup$MLBId),]
        
        fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_visit$MLBId)),]
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        
        if(nrow(fielder_available) > 0)
        {
          bat_bench <- batting_bench2[batting_bench2$MLBId %in% fielder_available$MLBId,]
          
          bat_bench$GameDate <- as.character(bat_bench$GameDate)
          
          bat_bench <- bat_bench[!(bat_bench$MLBId %in% box_stat_visit$MLBId),]
          
          bat_bench <- bat_bench[!(bat_bench$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Away[x] & lineup$POS %in% c("SP","RP")]),]
          
          bat_bench <- bat_bench[bat_bench$MLBId %in% lineup$MLBId,]
          
          stat <- bat_bench
          
          if(nrow(stat) > 0){
            
            stat <- stat[order(stat$GameDate, decreasing = TRUE),]
            
            stat$POS <- ""
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            stat$POS <- POS
            
            stat <- stat[1,]
            
          }
          
          if(nrow(stat) == 0)
          {
            potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
            
            potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_visit$MLBId)]
            
            potential_pinch_hitter <- unique(potential_pinch_hitter)
            
            bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
            
            bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
            
            bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
            
            bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Away[x] & lineup$POS %in% c("SP","RP")]),]
            
            bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
            
            stat <- bat_pinch
            
            if(nrow(stat) > 0)
            {
              stat$POS <- ""
              
              stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                              "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                              "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
              
              stat$POS <- "PH"
              
              stat <- stat[1,]
              
              potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
              
              potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_visit$MLBId)]
              
              potential_pinch_hitter <- unique(potential_pinch_hitter)
              
              bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
              
              bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
              
              bat_pinch <- bat_pinch[bat_pinch$MLBId %in% box_stat_visit$MLBId,]
              
              if(nrow(bat_pinch) > 0){
                bat_pinch$POS <- ""
                
                bat_pinch <- bat_pinch[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                          "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                          "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
                
                bat_pinch$POS <- "PH"
                
                stat <- rbind(stat,bat_pinch[1,])
              }
              
              if(nrow(bat_pinch) == 0){
                print("No pinch hitter available")
              }
              
            }
            
            if(nrow(stat) == 0)
            {
              print("No pinch hitter available")
            }
            
          }
          if(nrow(stat) > 0){
            box_stat_visit <- rbind(box_stat_visit,stat)
          }
          
          if(nrow(stat) == 0){
            print("No stat to append to box_stat_visit")
          }
        }
        
        
        if(nrow(fielder_available) == 0)
        {
          potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
          
          potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_visit$MLBId)]
          
          potential_pinch_hitter <- potential_pinch_hitter[potential_pinch_hitter %in% lineup$MLBId]
          
          potential_pinch_hitter <- unique(potential_pinch_hitter)
          
          bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
          
          bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
          
          bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Away[x] & lineup$POS %in% c("SP","RP")]),]
          
          bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
          
          if(nrow(stat) > 0){
            stat <- bat_pinch[1,]
            
            stat$POS <- ""
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            stat$POS <- "PH"
            
            potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
            
            potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_visit$MLBId)]
            
            potential_pinch_hitter <- unique(potential_pinch_hitter)
            
            bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
            
            bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
            
            bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Away[x] & lineup$POS %in% c("SP","RP")]),]
            
            bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
            
            if(nrow(bat_pinch) > 0)
            {
              
              bat_pinch$POS <- ""
              
              bat_pinch <- bat_pinch[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                        "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                        "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
              
              bat_pinch$POS <- "PH"
              
              stat <- rbind(stat,bat_pinch[1,])
            }
            
          }
          
          if(nrow(stat) == 0)
          {
            print("No pinch hitter available")
          }
          
          
          
          
        }
        
        if(nrow(stat) > 0)
        {
          box_stat_visit <- rbind(box_stat_visit, stat)
        }
        
        if(nrow(stat) == 0)
        {
          print("No pinch hitter stats to append to box_stat_visit")
        }
        
      }
    }
  }
  
  if(length(count_2) == 0)
  {
    print("No need to bring in bench player")
  }
  
  if(length(count_2) > 0)
  {
    
    
    for(s in 1:length(count_2))
    {
      
      batting_bench2 <- batting_bench
      batting_pinch2 <- batting_pinch
      batting_start2 <- batting_start
      
      POS <- box_stat_visit$POS[count_2[s]]
      
      team_name <- box_visit$Team[s]
      
      if(POS %in% "1"){
        
        box_stat_visit$MLBId <- as.character(box_stat_visit$MLBId)
        
        unplayed <- only_active_players$MLBId[(only_active_players$Team_RFB %in% final_schedule$Away[x]) & !(only_active_players$MLBId %in% box_stat_visit$MLBId)]
        
        unplayed <- unique(unplayed)
        
        bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% unplayed),]
        
        bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
        
        bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
        
        bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% box_stat_visit$MLBId),]
        
        bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Away[x] & lineup$POS %in% c("SP","RP")]),]
        
        stat <- bat_pinch
        
        if(nrow(stat) > 0){
          
          stat$POS <- "PH"
          
          
          stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                          "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                          "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
          
          
          stat <- stat[1,]
          
          
        }
        
        if(nrow(stat) == 0)
        {
          potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
          
          potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_visit$MLBId)]
          
          potential_pinch_hitter <- unique(potential_pinch_hitter)
          
          potential_pinch_hitter <- potential_pinch_hitter[potential_pinch_hitter %in% lineup$MLBId]
          
          bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
          
          bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Away[x] & lineup$POS %in% c("SP","RP")]),]
          
          bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
          
          bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
          
          bat_pinch$POS <- ""
          
          bat_pinch <- bat_pinch[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                    "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                    "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
          
          bat_pinch$POS <- "PH"
          
          stat <- bat_pinch[1,] 
          
          stat$GameDate <- as.character(stat$GameDate)
          
          box_stat_visit$GameDate <- as.character(box_stat_visit$GameDate)
          
        }
        
        box_stat_visit <- rbind(box_stat_visit,stat)
        
        
      }
      
      if(!(POS %in% 1)){
        fielder_available <- fielding_available2[which((fielding_available2$Team %in% team_name) & (fielding_available2$Pos %in% POS)),]
        
        fielder_available <- fielder_available[fielder_available$MLBId %in% lineup$MLBId[(lineup$Team == team_name) & !(lineup$POS %in% c("SP","RP","P"))],]
        
        fielder_available <- fielder_available[(fielder_available$MLBId %in% lineup$MLBId),]
        
        fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_visit$MLBId)),]
        
        fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        
        if(nrow(fielder_available) > 0)
        {
          bat_pinch <- batting_pinch2[batting_pinch2$MLBId %in% fielder_available$MLBId,]
          
          bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% box_stat_visit$MLBId),]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Away[x] & lineup$POS %in% c("SP","RP")]),]
          
          bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
          
          stat <- bat_pinch
          
          if(nrow(stat) > 0){
            
            stat$POS <- ""
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            stat <- stat[stat$GameDate %in% max(stat$GameDate),]
            
            stat$POS <- POS
            
            stat <- stat[1,]
            
          }
          
          if(nrow(stat) == 0)
          {
            potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
            
            potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_visit$MLBId)]
            
            potential_pinch_hitter <- potential_pinch_hitter[potential_pinch_hitter %in% lineup$MLBId]
            
            potential_pinch_hitter <- unique(potential_pinch_hitter)
            
            bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
            
            bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
            
            bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Away[x] & lineup$POS %in% c("SP","RP")]),]
            
            bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
            
            bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
            
            stat <- bat_pinch
            
            stat <- stat[1,]
            
            stat$POS <- ""
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            stat$POS <- "PH"
            
            stat$GameDate <- as.character(stat$GameDate)
            
            box_stat_visit$GameDate <- as.Date(box_stat_visit$GameDate, format="%Y-%m-%d")
            
          }
          
          box_stat_visit <- rbind(box_stat_visit,stat)
        }
        
        if(nrow(fielder_available) == 0)
        {
          potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
          
          potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_visit$MLBId)]
          
          potential_pinch_hitter <- potential_pinch_hitter[potential_pinch_hitter %in% lineup$MLBId]
          
          potential_pinch_hitter <- unique(potential_pinch_hitter)
          
          bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
          
          bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Away[x] & lineup$POS %in% c("SP","RP")]),]
          
          fielding_available2$GameDate <- as.Date(fielding_available2$GameDate)
          
          fielding_available2$MLBId <- as.character(fielding_available2$MLBId)
          
          bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
          
          bat_pinch$MLBId <- as.character(bat_pinch$MLBId)
          
          bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
          
          bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
          
          if(nrow(bat_pinch) > 0)
          {
            
            bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
            
            bat_pinch$POS <- ""
            
            bat_pinch <- bat_pinch[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                      "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                      "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            bat_pinch$POS <- "PH"
            
            stat <- bat_pinch[1,]
            
            stat$POS <- ""
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            stat$POS <- "PH"
            
            box_stat_visit$GameDate <- as.Date(box_stat_visit$GameDate, format="%Y-%m-%d")
            
            box_stat_visit <- rbind(box_stat_visit, stat)
          }
          
          if(nrow(bat_pinch) == 0)
          {
            print("No pinch hitter available")
          }
          
          
          
        }
        
      }
      
    }
  }
  
  
  # Sum LW, Bonus, Bases Taken, Outs On base, Field, E, Zone, Block, Frame columns.
  
  box_stat_visit$Bonus <- as.numeric(as.character(box_stat_visit$Bonus))
  
  box_stat_visit$Bonus[1] <- as.numeric(as.character((as.numeric(as.character(box_stat_visit$R[1])) * 0.25) + (as.numeric(as.character(box_stat_visit$RBI[1])) * 0.15)))
  box_stat_visit$Bonus[2] <- as.numeric(as.character((as.numeric(as.character(box_stat_visit$R[2])) * 0.22) + (as.numeric(as.character(box_stat_visit$RBI[2])) * 0.17)))
  box_stat_visit$Bonus[3] <- as.numeric(as.character((as.numeric(as.character(box_stat_visit$R[3])) * 0.20) + (as.numeric(as.character(box_stat_visit$RBI[3])) * 0.25)))
  box_stat_visit$Bonus[4] <- as.numeric(as.character((as.numeric(as.character(box_stat_visit$R[4])) * 0.17) + (as.numeric(as.character(box_stat_visit$RBI[4])) * 0.22)))
  box_stat_visit$Bonus[5] <- as.numeric(as.character((as.numeric(as.character(box_stat_visit$R[5])) * 0.12) + (as.numeric(as.character(box_stat_visit$RBI[5])) * 0.20)))
  box_stat_visit$Bonus[6] <- as.numeric(as.character((as.numeric(as.character(box_stat_visit$R[6])) * 0.12) + (as.numeric(as.character(box_stat_visit$RBI[6])) * 0.20)))
  box_stat_visit$Bonus[7] <- as.numeric(as.character((as.numeric(as.character(box_stat_visit$R[7])) * 0.07) + (as.numeric(as.character(box_stat_visit$RBI[7])) * 0.10)))
  box_stat_visit$Bonus[8] <- as.numeric(as.character((as.numeric(as.character(box_stat_visit$R[8])) * 0.05) + (as.numeric(as.character(box_stat_visit$RBI[8])) * 0.07)))
  box_stat_visit$Bonus[9] <- as.numeric(as.character((as.numeric(as.character(box_stat_visit$R[9])) * 0.15) + (as.numeric(as.character(box_stat_visit$RBI[9])) * 0.05)))
  
  # Assign Bases_Taken and Outs_on_Base
  
  for(i in 1:nrow(box_stat_visit))
  {
    base_available$GameDate <- as.Date(base_available$GameDate)
    box_stat_visit$GameDate <- as.Date(box_stat_visit$GameDate)
    
    bs_stat <- base_available[(base_available$GameDate %in% box_stat_visit$GameDate[i]) & (base_available$FirstName %in% box_stat_visit$FirstName[i]) & (base_available$LastName %in% box_stat_visit$LastName[i]),]
    bs_stat <- unique(bs_stat)
    
    box_stat_visit$Bases_Taken <- as.numeric(as.character(box_stat_visit$Bases_Taken))
    box_stat_visit$Outs_on_Base <- as.numeric(as.character(box_stat_visit$Outs_on_Base))
    
    if(nrow(bs_stat) == 0)
    {
      box_stat_visit$Bases_Taken[i] <- as.numeric(0)
      box_stat_visit$Outs_on_Base[i] <- as.numeric(0)
    }
    
    if(nrow(bs_stat) > 0)
    {
      box_stat_visit$Bases_Taken[i] <- as.numeric(as.character(bs_stat$BT[which(bs_stat$MLBId %in% box_stat_visit$MLBId[i])]))
      box_stat_visit$Outs_on_Base[i] <- as.numeric(as.character(bs_stat$BO[which(bs_stat$MLBId %in% box_stat_visit$MLBId[i])]))
    }
    
    
  }
  
  # Assign Field and E to everyone eligible. Assign nothing to DH
  
  position_player <- read.csv("position_player.csv")
  
  for(i in 1:nrow(box_stat_visit))
  {
    waive <- "NO"
    
    fielding_available$GameDate <- as.Date(fielding_available$GameDate)
    
    field_stat <- fielding_available[(fielding_available$MLBId %in% box_stat_visit$MLBId[i]) & (fielding_available$GameString %in% box_stat_visit$GameString[i]),]
    field_stat <- unique(field_stat)
    
    if(nrow(field_stat) > 0)
    {
      field_stat <- field_stat[field_stat$GameDate == max(field_stat$GameDate),]
      
      field_stat <- field_stat[1,]
      
      POS <-box_visit$POS[i]
    }
    
    if(!(box_stat_visit$POS[i] %in% c(NA,"NA","")))
    {
      if(nrow(field_stat) > 0)
      {
        if((field_stat$LW %in% NA))
        {
          field_stat$LW[which(field_stat$LW %in% NA)] <- 0
        }
      }
      
    }
    
    if(nrow(field_stat) > 0){
      
      box_stat_visit$Field <- as.numeric(as.character(box_stat_visit$Field))
      box_stat_visit$E <- as.integer(as.character(box_stat_visit$E))
      
      as.numeric(strsplit(as.character(a), "")[[1]])
      
      possible_pos <- unlist(strsplit(as.character(position_player$Pos[which(position_player$MLBId %in% box_stat_visit$MLBId[i])]),""))
      
      if(box_stat_visit$POS[i] %in% c("PH","DH",""))
      {
        next;
      }
      
      if(box_stat_visit$MLBId[i] %in% lineup$MLBId)
      {
        if(lineup$`Fielding Petition`[lineup$MLBId %in% box_stat_visit$MLBId[i]] %in% c("Yes","YES","yes","YEs","yEs","yES"))
        {
          box_stat_visit$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
          box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
          next;
        }
      }
      
      if(length(possible_pos) > 0){
        
        out_field <- c(7,8,9)
        
        if(box_stat_visit$POS[i] %in% out_field){
          
          if((unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_visit$MLBId[i])]),"")) %in% c(8)) & (box_stat_visit$POS[i] %in% c(7,9)))
          {
            box_stat_visit$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
            box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
            next;
          }
          
          # Run this if fielder's primary position is NOT one of LF,CF or RF
          
          if(!unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_visit$MLBId[i])]),"")) %in% c(7,8,9))
          {
            waive <- "NO"
          }
          
          # Run this if fielder's primary position is one of LF,CF or RF
          if(unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_visit$MLBId[i])]),"")) %in% c(7,8,9))
          {
            # Run this if fielder plays LF,CF, or RF today and his primary position is equal to position he plays today 
            if((box_stat_visit$POS[i] %in% c(7,8,9)) & ((out_field[which(out_field %in% unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_visit$MLBId[i])]),"")))]) %in% out_field))
            {
              waive <- "YES"
            }
            
            if(!(box_stat_visit$POS[i] %in% c(7,8,9)))
            {
              waive <- "NO"
            }
          }
          
        }
        
        if(!box_stat_visit$POS[i] %in% out_field)
        {
          waive <- "NO"
        }
        
        # Run this if set position today equals player's primary position
        if(box_stat_visit$POS[i] %in% unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_visit$MLBId[i])]),"")))
        {
          
          box_stat_visit$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
          box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
          next;
        }
        # Run this if set position today equals player's secondary position
        
        if(box_stat_visit$POS[i] %in% unlist(strsplit(as.character(position_player$secondary[which(position_player$MLBId %in% box_stat_visit$MLBId[i])]),"")))
        {
          
          if(as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])])) >= 0)
          {
            box_stat_visit$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])])) * 0.75
            box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
            next;
          }
          
          if(as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])])) <= 0)
          {
            box_stat_visit$Field[i] <- (as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])])) * (-0.75)) + (as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])])) * (-1))
            box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
            next;
          }
          
        }
        # Run this if set position today equals player's tertiary position
        
        if(box_stat_visit$POS[i] %in% unlist(strsplit(as.character(position_player$tertiary[which(position_player$MLBId %in% box_stat_visit$MLBId[i])]),"")))
        {
          
          if(as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])])) >= 0)
          {
            box_stat_visit$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])])) * 0.5
            box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
            next;
          }
          
          if(as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])])) <= 0)
          {
            box_stat_visit$Field[i] <- (as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])])) * (0.50)) + (as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])])) * (1))
            box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
            next;
          }
          
          
        }
        
        
        # Run this if you can't find a player in question in the position_player database
        if(position_player$primary[which(position_player$MLBId %in% box_stat_visit$MLBId[i])] %in% c(NA,"NA"))
        {
          next;
        }
        
        # Run this if fielder in question does not play his primary position
        if(!box_stat_visit$POS[i] %in% unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_visit$MLBId[i])]),"")))
        {
          if(box_stat_visit$POS[i] %in% c(7,8,9)){
            if(waive=="NO"){
              col_num <- position_player$most_inning_primary[which(position_player$MLBId %in% box_stat_visit$MLBId[i])]
              
              col_num <- as.numeric(col_num)
              
              ranking <- pos_sim[,col_num]
              
              penalty_slot <- which(ranking %in% box_stat_visit$POS[i])
            }
            
            if(waive == "YES")
            {
              out_field <- c(7,8,9)
              
              col_num <- out_field[which(out_field %in% unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_visit$MLBId[i])]),"")))]
              
              if(TRUE %in% (c(7,9) %in% col_num))
              {
                col_num <- unlist(strsplit(as.character(position_player$most_inning_primary[which(position_player$MLBId %in% box_stat_visit$MLBId[i])]),""))
              }
              
              col_num <- as.numeric(col_num)
              
              ranking <- pos_sim[,col_num]
              
              penalty_slot <- which(ranking %in% box_stat_visit$POS[i])
              
            }
          }
          
          if(!box_stat_visit$POS[i] %in% c(7,8,9))
          {
            col_num <- position_player$most_inning_primary[which(position_player$MLBId %in% box_stat_visit$MLBId[i])]
            
            col_num <- as.numeric(col_num)
            
            ranking <- pos_sim[,col_num]
            
            penalty_slot <- which(ranking %in% box_stat_visit$POS[i])
            
            if(length(penalty_slot) == 0 & (box_stat_visit$PA[i] %in% c(1)))
            {
              box_stat_visit$POS[i] <- "PH"
            }
          }
          
          if(length(penalty_slot) > 0)
          {
            
            if(penalty_slot == 1)
            {
              box_stat_visit$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
              box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
              
            }
            
            if(penalty_slot == 2)
            {
              if(!col_num %in% c(7,8,9))
              {
                box_stat_visit$Field[i] <- as.numeric(-0.25)
                box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
              }
              
              if(col_num %in% c(7,8,9))
              {
                box_stat_visit$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
                box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
                
              }
              
            }
            
            if(penalty_slot == 3)
            {
              
              if(!col_num %in% c(8))
              {
                box_stat_visit$Field[i] <- as.numeric(-0.50)
                box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
                
              }
              
              if(col_num %in% c(8))
              {
                box_stat_visit$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
                box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
                
              }
            }
            
            if(penalty_slot == 4)
            {
              box_stat_visit$Field[i] <- as.numeric(-0.75)
              box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
              
            }
            
            if(penalty_slot == 5)
            {
              box_stat_visit$Field[i] <- as.numeric(-1.00)
              box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
              
            }
            
            if(penalty_slot == 6)
            {
              box_stat_visit$Field[i] <- as.numeric(-1.25)
              box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
              
            }
            
            if(penalty_slot == 7)
            {
              box_stat_visit$Field[i] <- as.numeric(-1.5)
              box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
              
            }
            
          }
          
          if(length(penalty_slot) == 0)
          {
            next;
          }
          
        }
        
      }
      
      if(length(possible_pos) == 0)
      {
        box_stat_visit$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
        
        box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
        
      }
      
      box_stat_visit$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_visit$MLBId[i])]))
      
    }
    
    if((nrow(field_stat) == 0) || (POS %in% "DH") || (length(POS) == 0))
    {
      box_stat_visit$Field <- as.numeric(as.character(box_stat_visit$Field))
      box_stat_visit$E <- as.integer(as.character(box_stat_visit$E))
      
      box_stat_visit$Field[i] <- ""
      box_stat_visit$E[i] <- ""
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
  # Assign Zone, Block, and Frame
  
  for(i in 1:nrow(box_stat_visit))
  {
    YTD$MLBId <- as.character(YTD$MLBId)
    
    ytd_stat <- YTD[(YTD$MLBId %in% box_stat_visit$MLBId[i]),]
    ytd_stat <- unique(ytd_stat)
    
    ytd_stat$Pos <- as.character(ytd_stat$Pos)
    
    POS <- box_visit$POS[i]
    
    ytd_stat <- ytd_stat[ytd_stat$Pos %in% POS,]
    
    box_stat_visit$Zone <- as.numeric(as.character(box_stat_visit$Zone))
    box_stat_visit$Block <- as.numeric(as.character(box_stat_visit$Block))
    box_stat_visit$Frame <- as.numeric(as.character(box_stat_visit$Frame))
    
    
    box_stat_visit$Zone[i] <- as.numeric(as.character(ytd_stat$Zone[1]))
    box_stat_visit$Block[i] <- as.numeric(as.character(ytd_stat$Block[1]))
    box_stat_visit$Frame[i] <- as.numeric(as.character(ytd_stat$Frame[1]))
    
  }
  
  box_stat_visit$Block[which(box_stat_visit$Block %in% NA)] <- ""
  box_stat_visit$Frame[which(box_stat_visit$Frame %in% NA)] <- ""
  box_stat_visit$Zone[which(box_stat_visit$Zone %in% NA)] <- ""
  box_stat_visit$Field[which(box_stat_visit$Field %in% NA)] <- ""
  box_stat_visit$E[which(box_stat_visit$E %in% NA)] <- ""
  
  
  blank_visit <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_visit)))
  
  colnames(blank_visit) <- colnames(box_stat_visit)
  
  # Everything beyond this must be pushed behind, to the part when pitching is all taken care of
  
  blank_visit <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_visit)))
  
  colnames(blank_visit) <- colnames(box_stat_visit)
  
  blank_visit$LastName <- "Total"
  
  
  blank_visit$Bonus <- sum(as.numeric(as.character(box_stat_visit$Bonus)),na.rm = TRUE)
  blank_visit$Bases_Taken <- sum(as.numeric(as.character(box_stat_visit$Bases_Taken)),na.rm = TRUE)
  blank_visit$Outs_on_Base <- sum(as.numeric(as.character(box_stat_visit$Outs_on_Base)),na.rm = TRUE)
  blank_visit$Field <- sum(as.numeric(as.character(box_stat_visit$Field)),na.rm = TRUE)
  blank_visit$E <- sum(as.numeric(as.character(box_stat_visit$E)),na.rm = TRUE)
  blank_visit$Zone <- sum(as.numeric(as.character(box_stat_visit$Zone)),na.rm = TRUE)
  blank_visit$Block <- sum(as.numeric(as.character(box_stat_visit$Block)),na.rm = TRUE)
  blank_visit$Frame <- sum(as.numeric(as.character(box_stat_visit$Frame)),na.rm = TRUE)
  
  box_stat_visit$LW <- as.numeric(as.character(box_stat_visit$LW))
  
  blank_visit$LW <- as.numeric(as.character(blank_visit$LW))
  blank_visit$LW[1] <- sum(box_stat_visit$LW,na.rm = TRUE)
  
  box_stat_visit$H <- as.numeric(as.character(box_stat_visit$H))
  
  blank_visit$H <- as.numeric(as.character(blank_visit$H))
  blank_visit$H[1] <- sum(box_stat_visit$H,na.rm = TRUE)
  
  # Calculate 'Overall Offense'
  
  blank_visit2 <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_visit)))
  
  colnames(blank_visit2) <- colnames(box_stat_visit)
  
  blank_visit2$LastName <- as.character(blank_visit2$LastName)
  
  blank_visit2$LastName[1] <- "Overall Offense"
  
  blank_visit2$LW <- as.numeric(blank_visit$LW[1]) + as.numeric(blank_visit$Bonus[1]) + as.numeric(blank_visit$Bases_Taken[1]) + as.numeric(blank_visit$Outs_on_Base[1])
  
  blank_visit <- rbind(blank_visit, blank_visit2)
  
  blank_visit3 <- data.frame(matrix("",nrow= 1 ,ncol=ncol(box_stat_visit)))
  
  colnames(blank_visit3) <- colnames(box_stat_visit)
  
  blank_visit3$LastName <- "Overall Defense"
  
  blank_visit3$LW <- as.numeric(blank_visit$Field[1]) + as.numeric(blank_visit$Zone[1]) + as.numeric(blank_visit$Block[1]) + as.numeric(blank_visit$Frame[1])
  
  blank_visit <- rbind(blank_visit,blank_visit3)
  
  ###START_home###
  
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
    
    if(box_home$POS[i] %in% c(2,3,4,5,6,7,8,9,"DH")){
      if((box_home$MLBId[i] %in% batting_start$MLBId) | (box_home$MLBId[i] %in% batting_bench$MLBId)){
        
        # Match starter ID from batting_start to ID from box_home in ith row. This pulls data
        if(box_home$MLBId[i] %in% batting_start$MLBId){
          
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
        
        if((box_home$MLBId[i] %in% batting_bench$MLBId) & !(box_home$MLBId[i] %in% batting_start$MLBId))
        {
          
          
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
    }
    
    if(box_home$POS[i] %in% c(1))
    {
      
      # Match starter ID from batting_start to ID from box_home in ith row. This pulls data
      if(box_home$MLBId[i] %in% batting_start$MLBId){
        
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
        
        next;
        
      }
      
      if((box_home$MLBId[i] %in% batting_bench$MLBId) & !(box_home$MLBId[i] %in% batting_start$MLBId))
      {
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
        
        next;
      }
      
      if((box_home$MLBId[i] %in% batting_pinch$MLBId) & !(box_home$MLBId[i] %in% batting_start$MLBId) & !(box_home$MLBId[i] %in% batting_bench$MLBId))
      {
        stat <- batting_pinch[which(batting_pinch$MLBId %in% box_home$MLBId[i]),]
        
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
        
        next;
      }
      
      if((!(box_home$MLBId[i] %in% batting_start$MLBId) & !(box_home$MLBId[i] %in% batting_bench$MLBId) & !(box_home$MLBId[i] %in% batting_pinch$MLBId)))
      {
        print(paste("Giving MLE for ",box_home$fullname[i],sep=""))
        
        if(home_sp_stat$IP > 5)
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
          MLE_fill <- c("P",format(home_sp_stat$GameDate, "%Y-%m-%d"), as.character(home_sp_stat$FirstName), as.character(home_sp_stat$LastName), -0.75, 0, 0, 0 , 0, 0, 0 , "", "",
                        3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,3,as.character(home_sp_stat$MLBId),as.character(home_sp_stat$PlayerName),as.character(home_sp_stat$GameString),as.character(home_sp_stat$GameId),as.character(home_sp_stat$uniqueId), "")
          
          for(n in 1:length(MLE_fill))
          {
            MLE[1,n] <- MLE_fill[n]
          }
          
          box_stat_home <- rbind(box_stat_home, MLE)        
        }
        
        if((home_sp_stat$IP >= 3.01) & (home_sp_stat$IP < 5.01))
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
          MLE_fill <- c("P",format(home_sp_stat$GameDate, "%Y-%m-%d"), as.character(home_sp_stat$FirstName), as.character(home_sp_stat$LastName), -0.50, 0, 0, 0 , 0, 0, 0 , "", "",
                        2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,3,as.character(home_sp_stat$MLBId),as.character(home_sp_stat$PlayerName),as.character(home_sp_stat$GameString),as.character(home_sp_stat$GameId),as.character(home_sp_stat$uniqueId), "")
          
          for(n in 1:length(MLE_fill))
          {
            MLE[1,n] <- MLE_fill[n]
          }
          
          box_stat_home <- rbind(box_stat_home, MLE)        
        }
        
        if(home_sp_stat$IP < 3.01)
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
          MLE_fill <- c("P",format(home_sp_stat$GameDate, "%Y-%m-%d"), as.character(home_sp_stat$FirstName), as.character(home_sp_stat$LastName), -0.25, 0, 0, 0 , 0, 0, 0 , "", "",
                        1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,3,as.character(home_sp_stat$MLBId),as.character(home_sp_stat$PlayerName),as.character(home_sp_stat$GameString),as.character(home_sp_stat$GameId),as.character(home_sp_stat$uniqueId), "")
          
          for(n in 1:length(MLE_fill))
          {
            MLE[1,n] <- MLE_fill[n]
          }
          
          box_stat_home <- rbind(box_stat_home, MLE)        
        }
      }
    }
    
    
    
  }
  
  box_stat_home <- box_stat_home[!(box_stat_home$PlayerName %in% c("",NA)),]
  
  count_2 <- which(box_stat_home$PA %in% 2)
  count_1 <- which(box_stat_home$PA %in% 1)
  
  blanking <- data.frame(matrix("", nrow = 1, ncol=length(colnames(box_stat_home))))
  colnames(blanking) <- colnames(box_stat_home)
  blanking$GameDate <- as.Date(blanking$GameDate, format="%Y-%m-%d")
  box_stat_home <- rbind(box_stat_home,blanking)
  
  if(length(count_1) == 0)
  {
    print("No need to pull in pinch hitter data")
  }
  
  if(length(count_1) > 0)
  {
    for(s in 1:length(count_1))
    {
      POS <- box_stat_home$POS[count_1]
      team_name <- box_home$Team[count_1[s]]
      
      batting_bench2 <- batting_bench
      batting_pinch2 <- batting_pinch
      batting_start2 <- batting_start
      
      fielding_available2 <- fielding_available
      base_available2 <- base_available
      
      if(POS %in% "1"){
        
        box_stat_home$MLBId <- as.character(box_stat_home$MLBId)
        
        unplayed <- only_active_players$MLBId[(only_active_players$Team_RFB %in% final_schedule$Home[x]) & !(only_active_players$MLBId %in% box_stat_home$MLBId)]
        
        unplayed <- unique(unplayed)
        
        bat_pinch2 <- batting_pinch2[(batting_pinch2$MLBId %in% unplayed),]
        
        bat_pinch2$GameDate <- as.character(bat_pinch2$GameDate)
        
        bat_pinch2 <- bat_pinch2[!(bat_pinch2$MLBId %in% box_stat_home$MLBId),]
        
        bat_pinch2 <- bat_pinch2[!(bat_pinch2$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Home[x] & lineup$POS %in% c("SP","RP")]),]
        
        bat_pinch2 <- bat_pinch2[bat_pinch2$MLBId %in% lineup$MLBId,]
        
        stat <- bat_pinch2[bat_pinch2$GameDate %in% max(bat_pinch2$GameDate),]
        
        if(nrow(stat) > 0){
          
          stat$POS <- ""
          
          stat$POS <- "PH"
          
          stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                          "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                          "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
          
          stat <- stat[1,]
          
          box_stat_home <- rbind(box_stat_home,stat)
          
        }
        
        unplayed <- only_active_players$MLBId[(only_active_players$Team_RFB %in% final_schedule$Home[x]) & !(only_active_players$MLBId %in% box_stat_home$MLBId)]
        
        unplayed <- unique(unplayed)
        
        bat_pinch2 <- batting_pinch2[(batting_pinch2$MLBId %in% unplayed),]
        
        bat_pinch2 <- bat_pinch2[bat_pinch2$MLBId %in% lineup$MLBId,]
        
        bat_pinch2 <- bat_pinch2[!(bat_pinch2$MLBId) %in% (lineup$MLBId[(lineup$Team %in% final_schedule$Home[x]) & (lineup$POS %in% c("SP","RP"))]),]
        
        bat_pinch2$GameDate <- as.character(bat_pinch2$GameDate)
        
        stat <- bat_pinch2
        
        if(nrow(stat) > 0){
          
          stat$POS <- ""
          
          stat$POS <- "PH"
          
          stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                          "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                          "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
          
          stat <- stat[1,]
          
          
        }
        
        if(nrow(stat) == 0)
        {
          print("No pinch hitter to replace pitcher")
        }
        box_stat_home <- rbind(box_stat_home,stat)
        
      }
      
      if(!(POS %in% "1")){
        fielder_available <- fielding_available2[which((fielding_available2$Team %in% team_name) & (fielding_available2$Pos %in% c(POS))),]
        
        fielder_available <- fielder_available[(fielder_available$MLBId %in% lineup$MLBId),]
        
        fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
        
        fielder_available$MLBId <- as.character(fielder_available$MLBId)
        
        
        if(nrow(fielder_available) > 0)
        {
          bat_bench <- batting_bench2[batting_bench2$MLBId %in% fielder_available$MLBId,]
          
          bat_bench$GameDate <- as.character(bat_bench$GameDate)
          
          bat_bench <- bat_bench[!(bat_bench$MLBId %in% box_stat_home$MLBId),]
          
          bat_bench <- bat_bench[!(bat_bench$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Home[x] & lineup$POS %in% c("SP","RP")]),]
          
          bat_bench <- bat_bench[bat_bench$MLBId %in% lineup$MLBId,]
          
          stat <- bat_bench
          
          if(nrow(stat) > 0){
            
            stat <- stat[order(stat$GameDate, decreasing = TRUE),]
            
            stat$POS <- ""
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            stat$POS <- POS
            
            stat <- stat[1,]
            
          }
          
          if(nrow(stat) == 0)
          {
            potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
            
            potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
            
            potential_pinch_hitter <- unique(potential_pinch_hitter)
            
            bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
            
            bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
            
            bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
            
            bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Home[x] & lineup$POS %in% c("SP","RP")]),]
            
            bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
            
            stat <- bat_pinch
            
            if(nrow(stat) > 0)
            {
              stat$POS <- ""
              
              stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                              "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                              "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
              
              stat$POS <- "PH"
              
              stat <- stat[1,]
              
              potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
              
              potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
              
              potential_pinch_hitter <- unique(potential_pinch_hitter)
              
              bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
              
              bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
              
              bat_pinch <- bat_pinch[bat_pinch$MLBId %in% box_stat_home$MLBId,]
              
              if(nrow(bat_pinch) > 0){
                bat_pinch$POS <- ""
                
                bat_pinch <- bat_pinch[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                          "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                          "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
                
                bat_pinch$POS <- "PH"
                
                stat <- rbind(stat,bat_pinch[1,])
              }
              
              if(nrow(bat_pinch) == 0){
                print("No pinch hitter available")
              }
              
            }
            
            if(nrow(stat) == 0)
            {
              print("No pinch hitter available")
            }
            
          }
          if(nrow(stat) > 0){
            box_stat_home <- rbind(box_stat_home,stat)
          }
          
          if(nrow(stat) == 0){
            print("No stat to append to box_stat_home")
          }
        }
        
        
        if(nrow(fielder_available) == 0)
        {
          potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
          
          potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
          
          potential_pinch_hitter <- potential_pinch_hitter[potential_pinch_hitter %in% lineup$MLBId]
          
          potential_pinch_hitter <- unique(potential_pinch_hitter)
          
          bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
          
          bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
          
          bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Home[x] & lineup$POS %in% c("SP","RP")]),]
          
          bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
          
          if(nrow(stat) > 0){
            stat <- bat_pinch[1,]
            
            stat$POS <- ""
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            stat$POS <- "PH"
            
            potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
            
            potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
            
            potential_pinch_hitter <- unique(potential_pinch_hitter)
            
            bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
            
            bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% stat$MLBId),]
            
            bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Home[x] & lineup$POS %in% c("SP","RP")]),]
            
            bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
            
            if(nrow(bat_pinch) > 0)
            {
              
              bat_pinch$POS <- ""
              
              bat_pinch <- bat_pinch[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                        "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                        "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
              
              bat_pinch$POS <- "PH"
              
              stat <- rbind(stat,bat_pinch[1,])
            }
            
          }
          
          if(nrow(stat) == 0)
          {
            print("No pinch hitter available")
          }
          
          
          
          
        }
        
        if(nrow(stat) > 0)
        {
          box_stat_home <- rbind(box_stat_home, stat)
        }
        
        if(nrow(stat) == 0)
        {
          print("No pinch hitter stats to append to box_stat_home")
        }
        
      }
    }
  }
  
  if(length(count_2) == 0)
  {
    print("No need to bring in bench player")
  }
  
  if(length(count_2) > 0)
  {
    
    
    for(s in 1:length(count_2))
    {
      
      batting_bench2 <- batting_bench
      batting_pinch2 <- batting_pinch
      batting_start2 <- batting_start
      
      POS <- box_stat_home$POS[count_2[s]]
      
      team_name <- final_schedule$Home[x]
      
      if(POS %in% "1"){
        
        
        batter_pinch_id <- lineup$MLBId[(lineup$Team %in% final_schedule$Home[x]) & !(lineup$POS %in% c("SP","RP","P"))]
        
        pinch_stats <- batting_pinch[batting_pinch$MLBId %in% batter_pinch_id,]
        
        pinch_stats <- pinch_stats[!(pinch_stats$MLBId %in% box_stat_home$MLBId),]
        
        pinch_stats <- pinch_stats[!(pinch_stats$uniqueId %in% box_stat_home$uniqueId),]
        
        if(nrow(pinch_stats) > 0){
          box_stat_home$MLBId <- as.character(box_stat_home$MLBId)
          
          unplayed <- only_active_players$MLBId[(only_active_players$Team_RFB %in% final_schedule$Home[x]) & !(only_active_players$MLBId %in% box_stat_home$MLBId)]
          
          unplayed <- unique(unplayed)
          
          bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% unplayed),]
          
          bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
          
          bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% box_stat_home$MLBId),]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Home[x] & lineup$POS %in% c("SP","RP")]),]
          
          stat <- bat_pinch
          
          if(nrow(stat) > 0){
            
            stat$POS <- "PH"
            
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            
            stat <- stat[1,]
            
            
          }
          
          if(nrow(stat) == 0)
          {
            potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
            
            potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
            
            potential_pinch_hitter <- unique(potential_pinch_hitter)
            
            potential_pinch_hitter <- potential_pinch_hitter[potential_pinch_hitter %in% lineup$MLBId]
            
            bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
            
            bat_pinch$POS <- ""
            
            bat_pinch$POS <- "PH"
            
            bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
            
            bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Home[x] & lineup$POS %in% c("SP","RP")]),]
            
            bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
            
            bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
            
            bat_pinch <- bat_pinch[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                      "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                      "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            
            stat <- bat_pinch[1,] 
            
            stat$GameDate <- as.character(stat$GameDate)
            
            box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
            
          }
          
          box_stat_home <- rbind(box_stat_home,stat)
          
        }
        
        if(nrow(pinch_stats) == 0){
          
          
          box_stat_home$MLBId <- as.character(box_stat_home$MLBId)
          
          unplayed <- only_active_players$MLBId[(only_active_players$Team_RFB %in% final_schedule$Home[x]) & !(only_active_players$MLBId %in% box_stat_home$MLBId)]
          
          unplayed <- unique(unplayed)
          
          bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% unplayed),]
          
          bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
          
          bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% box_stat_home$MLBId),]
          
          bat_pinch <- bat_pinch[(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Home[x]]),]
          
          stat <- bat_pinch
          
          if(nrow(stat) > 0){
            
            stat$POS <- "PH"
            
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            
            stat <- stat[1,]
            
            
          }
          
          if(nrow(stat) == 0)
          {
            potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
            
            potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
            
            potential_pinch_hitter <- unique(potential_pinch_hitter)
            
            potential_pinch_hitter <- potential_pinch_hitter[potential_pinch_hitter %in% lineup$MLBId]
            
            bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
            
            bat_pinch$POS <- ""
            
            bat_pinch$POS <- "PH"
            
            bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
            
            bat_pinch <- bat_pinch[(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Home[x]]),]
            
            bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
            
            bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
            
            bat_pinch <- bat_pinch[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                      "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                      "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            
            stat <- bat_pinch[1,] 
            
            stat$GameDate <- as.character(stat$GameDate)
            
            box_stat_home$GameDate <- as.character(box_stat_home$GameDate)
            
          }
          
          box_stat_home <- rbind(box_stat_home,stat)
          
          
        }
      }
      
      if(!(POS %in% 1)){
        
        fielder_available <- fielding_available2[(fielding_available2$MLBId %in% lineup$MLBId[(lineup$Team %in% final_schedule$Home[x]) & !(lineup$POS %in% c("SP","RP"))]),]
        
        fielder_available <- fielder_available[fielder_available$MLBId %in% lineup$MLBId[(lineup$Team == team_name) & !(lineup$POS %in% c("SP","RP","P"))],]
        
        fielder_available <- fielder_available[(fielder_available$MLBId %in% lineup$MLBId),]
        
        fielder_available <- fielder_available[which(!(fielder_available$MLBId %in% box_stat_home$MLBId)),]
        
        fielder_available$PlayerName <- as.character(fielder_available$PlayerName)
        
        fielder_available$GameDate <- as.Date(fielder_available$GameDate)
        
        if(nrow(fielder_available) > 0)
        {
          bat_pinch <- batting_pinch2[batting_pinch2$MLBId %in% fielder_available$MLBId,]
          
          bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% box_stat_home$MLBId),]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Home[x] & lineup$POS %in% c("SP","RP")]),]
          
          bat_pinch$GameDate <- as.character(bat_pinch$GameDate)
          
          stat <- bat_pinch
          
          if(nrow(stat) > 0){
            
            stat$POS <- ""
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            stat$POS <- POS
            
            stat <- stat[1,]
            
          }
          
          if(nrow(stat) == 0)
          {
            potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
            
            potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
            
            potential_pinch_hitter <- potential_pinch_hitter[potential_pinch_hitter %in% lineup$MLBId]
            
            potential_pinch_hitter <- unique(potential_pinch_hitter)
            
            bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
            
            bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
            
            bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Home[x] & lineup$POS %in% c("SP","RP")]),]
            
            bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
            
            bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
            
            stat <- bat_pinch
            
            stat <- stat[1,]
            
            stat$POS <- ""
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            stat$POS <- "PH"
            
            stat$GameDate <- as.character(stat$GameDate)
            
            box_stat_home$GameDate <- as.Date(box_stat_home$GameDate, format="%Y-%m-%d")
            
          }
          
          box_stat_home <- rbind(box_stat_home,stat)
        }
        
        if(nrow(fielder_available) == 0)
        {
          potential_pinch_hitter <- only_active_players$MLBId[only_active_players$Team_RFB %in% team_name]
          
          potential_pinch_hitter <- potential_pinch_hitter[!(potential_pinch_hitter %in% box_stat_home$MLBId)]
          
          potential_pinch_hitter <- potential_pinch_hitter[potential_pinch_hitter %in% lineup$MLBId]
          
          potential_pinch_hitter <- unique(potential_pinch_hitter)
          
          bat_pinch <- batting_pinch2[(batting_pinch2$MLBId %in% potential_pinch_hitter),]
          
          bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
          
          bat_pinch <- bat_pinch[!(bat_pinch$MLBId %in% lineup$MLBId[lineup$Team %in% final_schedule$Home[x] & lineup$POS %in% c("SP","RP")]),]
          
          fielding_available2$GameDate <- as.Date(fielding_available2$GameDate)
          
          fielding_available2$MLBId <- as.character(fielding_available2$MLBId)
          
          bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
          
          bat_pinch$MLBId <- as.character(bat_pinch$MLBId)
          
          bat_pinch$GameDate <- as.Date(bat_pinch$GameDate)
          
          bat_pinch <- bat_pinch[bat_pinch$MLBId %in% lineup$MLBId,]
          
          if(nrow(bat_pinch) > 0)
          {
            
            bat_pinch <- bat_pinch[order(bat_pinch$GameDate, decreasing = TRUE),]
            
            bat_pinch$POS <- ""
            
            bat_pinch <- bat_pinch[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                                      "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                                      "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            bat_pinch$POS <- "PH"
            
            stat <- bat_pinch[1,]
            
            stat$POS <- ""
            
            stat <- stat[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases_Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                            "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB",
                            "LD","POPU","BH","IFH","OUTS","MLBId","PlayerName","GameString","GameId","uniqueId","used")]
            
            stat$POS <- "PH"
            
            box_stat_home$GameDate <- as.Date(box_stat_home$GameDate, format="%Y-%m-%d")
            
            box_stat_home <- rbind(box_stat_home, stat)
          }
          
          if(nrow(bat_pinch) == 0)
          {
            print("No pinch hitter available")
          }
          
          
          
        }
        
      }
      
    }
  }
  
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
  
  box_stat_home$GameDate <- as.Date(box_stat_home$GameDate, format= "%Y-%m-%d")
  
  box_stat_home <- box_stat_home[!(box_stat_home$GameDate %in% ""),]
  
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
    waive <- "NO"
    
    fielding_available$GameDate <- as.Date(fielding_available$GameDate)
    
    field_stat <- fielding_available[(fielding_available$MLBId %in% box_stat_home$MLBId[i]) & (fielding_available$GameString %in% box_stat_home$GameString[i]),]
    field_stat <- unique(field_stat)
    
    if(nrow(field_stat) > 0)
    {
      field_stat <- field_stat[field_stat$GameDate == max(field_stat$GameDate),]
      
      field_stat <- field_stat[1,]
      
      POS <-box_home$POS[i]
    }
    
    if(!(box_stat_home$POS[i] %in% c(NA,"NA","")))
    {
      if(nrow(field_stat) > 0)
      {
        if((field_stat$LW %in% NA))
        {
          field_stat$LW[which(field_stat$LW %in% NA)] <- 0
        }
      }
      
    }
    
    if(nrow(field_stat) > 0){
      
      box_stat_home$Field <- as.numeric(as.character(box_stat_home$Field))
      box_stat_home$E <- as.integer(as.character(box_stat_home$E))
      
      as.numeric(strsplit(as.character(a), "")[[1]])
      
      possible_pos <- unlist(strsplit(as.character(position_player$Pos[which(position_player$MLBId %in% box_stat_home$MLBId[i])]),""))
      
      if(box_stat_home$POS[i] %in% c("PH","DH",""))
      {
        next;
      }
      
      #Fielding petition block
      if(box_stat_home$MLBId[i] %in% lineup$MLBId)
      {
        # Run this if the player is flagged for "Fielding Petition". He won't be penalized
        if(lineup$`Fielding Petition`[lineup$MLBId %in% box_stat_home$MLBId[i]] %in% c("Yes","YES","yes","YEs","yEs","yES"))
        {
          box_stat_home$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
          box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
          next;
        }
      }
      
      if(length(possible_pos) > 0){
        
        out_field <- c(7,8,9)
        
        if(box_stat_home$POS[i] %in% out_field){
          
          if((unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_home$MLBId[i])]),"")) %in% c(8)) & (box_stat_home$POS[i] %in% c(7,9)))
          {
            box_stat_home$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
            box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
            next;
          }
          
          # Run this if fielder's primary position is NOT one of LF,CF or RF
          
          if(!unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_home$MLBId[i])]),"")) %in% c(7,8,9))
          {
            waive <- "NO"
          }
          
          # Run this if fielder's primary position is one of LF,CF or RF
          if(unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_home$MLBId[i])]),"")) %in% c(7,8,9))
          {
            # Run this if fielder plays LF,CF, or RF today and his primary position is equal to position he plays today 
            if((box_stat_home$POS[i] %in% c(7,8,9)) & ((out_field[which(out_field %in% unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_home$MLBId[i])]),"")))]) %in% out_field))
            {
              waive <- "YES"
            }
            
            if(!(box_stat_home$POS[i] %in% c(7,8,9)))
            {
              waive <- "NO"
            }
          }
          
        }
        
        if(!box_stat_home$POS[i] %in% out_field)
        {
          waive <- "NO"
        }
        
        # Run this if set position today equals player's primary position
        if(box_stat_home$POS[i] %in% unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_home$MLBId[i])]),"")))
        {
          
          box_stat_home$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
          box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
          next;
        }
        # Run this if set position today equals player's secondary position
        
        if(box_stat_home$POS[i] %in% unlist(strsplit(as.character(position_player$secondary[which(position_player$MLBId %in% box_stat_home$MLBId[i])]),"")))
        {
          
          if(as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])])) >= 0)
          {
            box_stat_home$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])])) * 0.75
            box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
            next;
          }
          
          if(as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])])) <= 0)
          {
            box_stat_home$Field[i] <- (as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])])) * (-0.75)) + (as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])])) * (-1))
            box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
            next;
          }
          
        }
        # Run this if set position today equals player's tertiary position
        
        if(box_stat_home$POS[i] %in% unlist(strsplit(as.character(position_player$tertiary[which(position_player$MLBId %in% box_stat_home$MLBId[i])]),"")))
        {
          
          if(as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])])) >= 0)
          {
            box_stat_home$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])])) * 0.5
            box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
            next;
          }
          
          if(as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])])) <= 0)
          {
            box_stat_home$Field[i] <- (as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])])) * (0.50)) + (as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])])) * (1))
            box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
            next;
          }
          
          
        }
        
        
        # Run this if you can't find a player in question in the position_player database
        if(position_player$primary[which(position_player$MLBId %in% box_stat_home$MLBId[i])] %in% c(NA,"NA"))
        {
          next;
        }
        
        # Run this if fielder in question does not play his primary position
        if(!box_stat_home$POS[i] %in% unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_home$MLBId[i])]),"")))
        {
          if(box_stat_home$POS[i] %in% c(7,8,9)){
            if(waive=="NO"){
              col_num <- position_player$most_inning_primary[which(position_player$MLBId %in% box_stat_home$MLBId[i])]
              
              col_num <- as.numeric(col_num)
              
              ranking <- pos_sim[,col_num]
              
              penalty_slot <- which(ranking %in% box_stat_home$POS[i])
            }
            
            if(waive == "YES")
            {
              out_field <- c(7,8,9)
              
              col_num <- out_field[which(out_field %in% unlist(strsplit(as.character(position_player$primary[which(position_player$MLBId %in% box_stat_home$MLBId[i])]),"")))]
              
              if(TRUE %in% (c(7,9) %in% col_num))
              {
                col_num <- unlist(strsplit(as.character(position_player$most_inning_primary[which(position_player$MLBId %in% box_stat_home$MLBId[i])]),""))
              }
              
              col_num <- as.numeric(col_num)
              
              ranking <- pos_sim[,col_num]
              
              penalty_slot <- which(ranking %in% box_stat_home$POS[i])
              
            }
          }
          
          if(!box_stat_home$POS[i] %in% c(7,8,9))
          {
            col_num <- position_player$most_inning_primary[which(position_player$MLBId %in% box_stat_home$MLBId[i])]
            
            col_num <- as.numeric(col_num)
            
            ranking <- pos_sim[,col_num]
            
            penalty_slot <- which(ranking %in% box_stat_home$POS[i])
            
            if(length(penalty_slot) == 0 & (box_stat_home$PA[i] %in% c(1)))
            {
              box_stat_home$POS[i] <- "PH"
            }
          }
          
          if(length(penalty_slot) > 0)
          {
            
            if(penalty_slot == 1)
            {
              box_stat_home$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
              box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
              
            }
            
            if(penalty_slot == 2)
            {
              if(!col_num %in% c(7,8,9))
              {
                box_stat_home$Field[i] <- as.numeric(-0.25)
                box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
              }
              
              if(col_num %in% c(7,8,9))
              {
                box_stat_home$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
                box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
                
              }
              
            }
            
            if(penalty_slot == 3)
            {
              
              if(!col_num %in% c(8))
              {
                box_stat_home$Field[i] <- as.numeric(-0.50)
                box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
                
              }
              
              if(col_num %in% c(8))
              {
                box_stat_home$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
                box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
                
              }
            }
            
            if(penalty_slot == 4)
            {
              box_stat_home$Field[i] <- as.numeric(-0.75)
              box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
              
            }
            
            if(penalty_slot == 5)
            {
              box_stat_home$Field[i] <- as.numeric(-1.00)
              box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
              
            }
            
            if(penalty_slot == 6)
            {
              box_stat_home$Field[i] <- as.numeric(-1.25)
              box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
              
            }
            
            if(penalty_slot == 7)
            {
              box_stat_home$Field[i] <- as.numeric(-1.5)
              box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
              
            }
            
          }
          
          if(length(penalty_slot) == 0)
          {
            next;
          }
          
        }
        
      } 
      
      if(length(possible_pos) == 0)
      {
        box_stat_home$Field[i] <- as.numeric(as.character(field_stat$LW[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
        
        box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
        
      }
      
      box_stat_home$E[i] <- as.integer(as.character(field_stat$E[which(field_stat$MLBId %in% box_stat_home$MLBId[i])]))
      
    }
    
    if((nrow(field_stat) == 0) || (POS %in% "DH") || (length(POS) == 0))
    {
      box_stat_home$Field <- as.numeric(as.character(box_stat_home$Field))
      box_stat_home$E <- as.integer(as.character(box_stat_home$E))
      
      box_stat_home$Field[i] <- ""
      box_stat_home$E[i] <- ""
    }
  }
  
  for(i in 1:nrow(box_stat_home))
  {
    YTD$MLBId <- as.character(YTD$MLBId)
    
    ytd_stat <- YTD[(YTD$MLBId %in% box_stat_home$MLBId[i]),]
    ytd_stat <- unique(ytd_stat)
    
    ytd_stat$Pos <- as.character(ytd_stat$Pos)
    
    POS <- box_home$POS[i]
    
    if(box_home$POS[i] %in% c(7,8,9))
    {
      POS <- c(7,8,9)
    }
    
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
  
  blank_home[,1] <- as.double(blank_home[,1])
  blank_home[,5] <- as.double(blank_home[,5])
  blank_home[,6] <- as.double(blank_home[,6])
  blank_home[,7] <- as.double(blank_home[,7])
  blank_home[,8] <- as.character(blank_home[,8])
  blank_home[,9] <- as.character(blank_home[,9])
  blank_home[,10] <- as.character(blank_home[,10])
  blank_home[,11] <- as.character(blank_home[,11])
  blank_home[,12] <- as.character(blank_home[,12])
  blank_home[,13] <- as.character(blank_home[,13])
  
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
  
  ##PART1##
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
  
  score$external_visit <- ""
  score$external_home <- ""
  
  score$external_visit <- as.numeric(score$external_visit)
  score$external_home <- as.numeric(score$external_home)
  
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
  
  home_team <- home_team[home_team < 49]
  
  visiting_pitcher <- only_active_players$MLBId[(only_active_players$Team_RFB %in% final_schedule$Away[x]) & (only_active_players$Pos %in% "1")]
  
  pitching_RP_visit <- pitching_RP[pitching_RP$MLBId %in% visiting_pitcher,]
  
  away_rp_report <- read.csv(paste("report/RP/",date,"/",final_schedule$Away[x],date,"_RP_report.csv",sep=""))
  
  cannot_use_away <- away_rp_report$MLBId[away_rp_report$Cannot.Use == "TRUE"]
  
  pitching_RP_visit <- pitching_RP_visit[!(pitching_RP_visit$MLBId %in% cannot_use_away),]
  
  for(i in 1:nrow(pitching_RP_visit))
  {
    pitching_RP_visit$OUT[i] <- (pitching_RP_visit$IP[i] %/% 1) * 3 + ((pitching_RP_visit$IP[i] %% 1) * 10)
  }
  
  if(length(visit_team) == 0)
  {
    print("Pitcher went at least 8th inning")
    no_setup_visit <- "YES"
    
  }
  
  ##PART2##
  
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
    
    sample_visit <- sample_visit[order(sample_visit$OUT,decreasing = FALSE),]
    
    #sample_visit <- sample_visit[!(sample_visit$IP == 0),]
    
    num_of_zero <- which(sample_visit$IP == 0)
    
    if(length(num_of_zero) > 1)
    {
      sample_visit <- sample_visit[!c(1:nrow(sample_visit)) %in% min(num_of_zero),]
    }
    
    if(length(num_of_zero) == 1)
    {
      sample_visit <- sample_visit
    }
    
    if(length(num_of_zero) == 0)
    {
      sample_visit <- sample_visit
    }
    
    for(i in 1:nrow(sample_visit))
    {
      inning <- ((sample_visit$IP[i] %/% 1 + (((sample_visit$IP[i] %% 1) * (10/3)))) / 9)
      
      ER <- ifelse((blank_home$LW[2] * inning) - (blank_visit$LW[3] * inning) - (sample_visit$LW[i]) + (4.25 * inning) < 0, 0, ER <- round((blank_home$LW[2] * inning) - (blank_visit$LW[3] * inning) - (sample_visit$LW[i]) + (4.25 * inning),digits=0))
      
      if(ER < 0)
      {
        sample_visit$ER[i] <- 0
        
      }
      
      if(ER >= 0)
      {
        sample_visit$ER[i] <- ER
        
      }
      
    }
    
    name <- as.character(sample_visit$PlayerName)
    
    for(i in 1:length(name))
    {
      if(sample_visit$IP[i] != 0){
        num <- sample_visit$OUT[sample_visit$PlayerName %in% name[i]]
        score$Pit[visit_team[1:num]] <- name[i]
        visit_team <- visit_team[(num+1):length(visit_team)]
      }
      
      if(sample_visit$IP[i] == 0)
      {
        next;
      }
      
    }
    
  }
  
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
  
  ##PART3##
  
  if(length(home_team) > 0)
  {
    
    continue <- TRUE
    
    attempt <- 0
    
    while(continue == TRUE)
    {
      attempt <- attempt + 1
      
      sample_home <- pitching_RP_home[sample((1:nrow(pitching_RP_home)), size = sample(nrow(pitching_RP_home), size = 1, replace = FALSE), replace = FALSE),]
      
      sample_home <- sample_home[!(sample_home$MLBId %in% lineup$MLBId[(lineup$Role == "CLOSER") & (lineup$Team %in% final_schedule$Home[x])]),]
      
      number <- sum(sample_home$OUT)
      
      if((number == length(home_team)) & (length(unique(sample_home$PlayerName)) == nrow(sample_home)) & ((away_sp %in% sample_home$PlayerName) == FALSE))
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
          
          if(((number - length(home_team)) == c(1)) & (away_sp %in% sample_home$PlayerName == FALSE) & (length(unique(sample_home$PlayerName)) == nrow(sample_home)))
          {
            sample_home <- sample_home
            continue <- FALSE
            continue2 <- FALSE
          }
          
        }
      }
    }
    
    sample_home <- sample_home[order(sample_home$OUT,decreasing = FALSE),]
    
    #sample_home <- sample_home[!(sample_home$IP == 0),]
    
    num_of_zero <- which(sample_home$IP == 0)
    
    if(length(num_of_zero) > 1)
    {
      sample_home <- sample_home[!c(1:nrow(sample_home)) %in% min(num_of_zero),]
    }
    
    if(length(num_of_zero) == 1)
    {
      sample_home <- sample_home
    }
    
    if(length(num_of_zero) == 0)
    {
      sample_home <- sample_home
    }
    
    for(i in 1:nrow(sample_home))
    {
      inning <- ((sample_home$IP[i] %/% 1 + (((sample_home$IP[i] %% 1) * (10/3)))) / 9)
      
      ER <- ifelse((blank_home$LW[2] * inning) - (blank_home$LW[3] * inning) - (sample_home$LW[i]) + (4.25 * inning) < 0, 0, ER <- round((blank_home$LW[2] * inning) - (blank_home$LW[3] * inning) - (sample_home$LW[i]) + (4.25 * inning),digits=0))
      
      if(ER < 0)
      {
        sample_home$ER[i] <- 0
        
      }
      
      if(ER >= 0)
      {
        sample_home$ER[i] <- ER
        
      }
      
    }
    
    name <- as.character(sample_home$PlayerName)
    
    for(i in 1:length(name))
    {
      if(sample_home$IP[i] != 0){
        num <- sample_home$OUT[sample_home$PlayerName %in% name[i]]
        score$Pit[home_team[1:num]] <- name[i]
        home_team <- home_team[(num+1):length(home_team)]
      }
      
      if(sample_home$IP[i] == 0)
      {
        next;
      }
      
    }
    
  }
  
  ##PART4##
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
    inning <- ((pitching_line_home$IP[i] %/% 1 + (((pitching_line_home$IP[i] %% 1) * (10/3)))) / 8)
    
    ER <- ifelse(((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (pitching_line_home$LW[i]) + (4.25 * inning)) < 0, 0, ER <- round(((blank_visit$LW[2] * inning) - (blank_home$LW[3] * inning) - (pitching_line_home$LW[i]) + (4.25 * inning)),digits=0))
    
    if(ER < 0)
    {
      pitching_line_home$ER[i] <- 0
      
    }
    
    if(ER >= 0)
    {
      pitching_line_home$ER[i] <- ER
      
    }
  }
  
  for(i in 1:nrow(pitching_line_visit))
  {
    inning <- ((pitching_line_visit$IP[i] %/% 1 + (((pitching_line_visit$IP[i] %% 1) * (10/3)))) / 8)
    
    ER <- ifelse((blank_home$LW[2] * inning) - (blank_visit$LW[3] * inning) - (pitching_line_visit$LW[i]) + (4.25 * inning) < 0, 0, ER <- round((blank_home$LW[2] * inning) - (blank_visit$LW[3] * inning) - (pitching_line_visit$LW[i]) + (4.25 * inning),digits=0))
    
    if(ER < 0)
    {
      pitching_line_visit$ER[i] <- 0
      
    }
    
    if(ER >= 0)
    {
      pitching_line_visit$ER[i] <- ER
      
    }
    
  }
  
  if(nrow(pitching_line_visit) == 1)
  {
    innings <- (pitching_line_visit$OUT[1]%/%3) + (((pitching_line_visit$OUT[1] %% 3)/10)/0.3)
    
  }
  
  if(nrow(pitching_line_visit) > 1)
  {
    innings <- (sum((pitching_line_visit$OUT[2:nrow(pitching_line_visit)]))%/%3) + (((sum((pitching_line_visit$OUT[2:nrow(pitching_line_visit)])) %% 3)/10)/0.3)
    
  }

  ifelse((sum(blank_home$LW,na.rm=TRUE) * (innings/8)) - (sum(blank_visit$LW,na.rm=TRUE) * (innings/8)) - (sum(pitching_line_visit$LW,na.rm=TRUE)) + (4.25 * (innings/8)) < 0, total_ER_visit <- 0, total_ER_visit <- round((sum(blank_home$LW,na.rm=TRUE) * (innings/8)) - (sum(blank_visit$LW,na.rm=TRUE) * (innings/8)) - (sum(pitching_line_visit$LW,na.rm=TRUE)) + (4.25 * (innings/8)),digits=0))
  
  zero_remainder_visit <- total_ER_visit - sum(pitching_line_visit$ER[2:nrow(pitching_RP_visit)],na.rm=TRUE)
  
  pitching_line_visit$ER[which(pitching_line_visit$IP == 0)] <- zero_remainder_visit
  
  if(nrow(pitching_line_home) == 1)
  {
    innings <- (pitching_line_home$OUT[1]%/%3) + (((pitching_line_home$OUT[1] %% 3)/10)/0.3)
    
  }
  
  if(nrow(pitching_line_home) > 1)
  {
    innings <- (sum((pitching_line_home$OUT[2:nrow(pitching_line_home)]))%/%3) + (((sum((pitching_line_home$OUT[2:nrow(pitching_line_home)])) %% 3)/10)/0.3)
    
  }
  

  ifelse((sum(blank_visit$LW,na.rm=TRUE) * (innings/8)) - (sum(blank_home$LW,na.rm=TRUE) * (innings/8)) - (sum(pitching_line_home$LW,na.rm=TRUE)) + (4.25 * (innings/8)) < 0, total_ER_home <- 0, total_ER_home <- round((sum(blank_visit$LW,na.rm=TRUE) * (innings/8)) - (sum(blank_home$LW,na.rm=TRUE) * (innings/8)) - (sum(pitching_line_home$LW,na.rm=TRUE)) + (4.25 * (innings/8)),digits=0))
  
  zero_remainder_home <- total_ER_home - sum(pitching_line_home$ER[2:nrow(pitching_RP_home)],na.rm=TRUE)
  
  pitching_line_home$ER[which(pitching_line_home$IP == 0)] <- zero_remainder_home
  
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
    
    if(length(which(score$Pit %in% as.character(pitching_line_visit$PlayerName[i]))) > 0)
    {
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
    
    if(length(which(score$Pit %in% pitching_line_visit$PlayerName[i])) == 0)
    {
      next;
    }
    
    
  }
  
  for(i in 1:nrow(pitching_line_home))
  {
    runs <- pitching_line_home$ER[i]
    ifelse(runs > 0, runs <- runs, runs <- 0)
    
    if(length(which(score$Pit %in% as.character(pitching_line_home$PlayerName[i]))) > 0)
    {
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
        print("run is zero")
        score$Add_score[slot] <- 0
        
      }
    }
    
    if(length(which(score$Pit %in% pitching_line_home$PlayerName[i])) == 0)
    {
      next;
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
  
  if(length(which(pitching_line_visit$IP == 0)) == 1)
  {
    which_zero_visit <- which(pitching_line_visit$IP == 0)
    
  if(which_zero_visit >=2)
  {
    visit_outs <- which(score$Side == "Bottom")
    score$external_visit[visit_outs[pitching_line_visit$OUT[1:(which_zero_visit-1)]+1]] <- pitching_line_visit$ER[which_zero_visit]
    visit_external <- which((visit_outs > (visit_outs[pitching_line_visit$OUT[1:(which_zero_visit-1)]])) & (visit_outs < 52))
    score$external_visit[visit_outs[pitching_line_visit$OUT[1:(which_zero_visit-1)]+1]:48] <- pitching_line_visit$ER[which_zero_visit]
    
  }
    
    score$external_visit[score$external_visit[1:48] %in% NA] <- 0
    score$external_visit[49:54] <- NA
    score$`H-Score`[1:48] <- score$`H-Score`[1:48] + score$external_visit[1:48]
  }
  
  if(length(which(pitching_line_home$IP == 0)) == 1)
  {
    which_zero_home <- which(pitching_line_home$IP == 0)
    
    if(which_zero_home >=2)
    {
      home_outs <- which(score$Side == "Bottom")
      score$external_home[home_outs[pitching_line_home$OUT[1:(which_zero_home-1)]+1]] <- pitching_line_home$ER[which_zero_home]
      home_external <- which((home_outs > (home_outs[pitching_line_home$OUT[1:(which_zero_home-1)]])) & (home_outs < 52))
      score$external_home[home_outs[pitching_line_home$OUT[1:(which_zero_home-1)]+1]:48] <- pitching_line_home$ER[which_zero_home]
      
    }
    
    score$external_home[score$external_home[1:48] %in% NA] <- 0
    score$external_home[49:54] <- NA
    score$`V-Score`[1:48] <- score$`V-Score`[1:48] + score$external_home[1:48]
  }
  
  ##PART5##
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
        
        if(ER < 0)
        {
          sample_home$ER[i] <- 0
          
        }
        
        if(ER >= 0)
        {
          sample_home$ER[i] <- ER
          
        }
        
        
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
  
  ##PART6##
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
        
        if(ER < 0)
        {
          closer_home$ER[i] <- 0
          
        }
        
        if(ER >= 0)
        {
          closer_home$ER[i] <- ER
          
        }
        
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
            
            if(ER < 0)
            {
              closer_home$ER[i] <- 0
              
            }
            
            if(ER >= 0)
            {
              closer_home$ER[i] <- ER
              
            }
            
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
              
              if(ER < 0)
              {
                closer_home$ER[i] <- 0
                
              }
              
              if(ER >= 0)
              {
                closer_home$ER[i] <- ER
                
              }
              
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
              
              if(ER < 0)
              {
                short_relief_home$ER[i] <- 0
                
              }
              
              if(ER >= 0)
              {
                short_relief_home$ER[i] <- ER
                
              }
              
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
  
  ##PART7##
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
    
    if((score$Out[max(which(score$Pit == winning_pitcher))] < 27) & (final_schedule$Home[x] %in% score$Home))
    {
      for(tf in 2:nrow(pitching_line_home))
      {
        if(sum(pitching_line_home$OUT[1:tf],na.rm=TRUE) >= 15)
        {
          winning_pitcher <- as.character(pitching_line_home$PlayerName[tf])
          break;
        }
        
        if(!sum(pitching_line_home$OUT[1:tf],na.rm=TRUE) >= 15)
        {
          next;
        }
      }
    }
    
    if((score$Out[max(which(score$Pit == winning_pitcher))] < 30) & (final_schedule$Away[x] %in% score$Visit))
    {
      for(tg in 2:nrow(pitching_line_visit))
      {
        if(sum(pitching_line_visit$OUT[1:tg],na.rm=TRUE) >= 15)
        {
          winning_pitcher <- as.character(pitching_line_visit$PlayerName[tg])
          break;
        }
        
        if(!sum(pitching_line_visit$OUT[1:tg],na.rm=TRUE) >= 15)
        {
          next;
        }
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
  
  ##PART8##
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
          number_match <- as.numeric(27) - as.numeric(away_sp_stat$OUT[1])
          
          number_match <- as.numeric(number_match)
          
          count3 <- count3 + 1
          
          sample_relief_away <- away_relief_9th[sample(nrow(away_relief_9th), size = c(1), replace = FALSE),]
          
          sample_relief_away$OUT <- (sample_relief_away$IP %/% 1) * 3 + ((sample_relief_away$IP %% 1) * 10)
          
          number <- as.numeric(sum(sample_relief_away$OUT, na.rm = TRUE))
          
          if((nrow(sample_relief_away) == 1) & (as.numeric(as.character(number)) == as.numeric(as.character(number_match))))
          {
            CONTINUE <- FALSE
          }
          
          if((nrow(sample_relief_away) == 2) & (sample_relief_away$PlayerName[1] != sample_relief_away$PlayerName[2]) & ((as.numeric(as.character(number)) == as.numeric(as.character(number_match)))))
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
        
        if(away_sp_stat$OUT[1] == 25)
        {
          count <- c(53,54)
        }
        
        if(away_sp_stat$OUT[1] == 26)
        {
          count <- c(54)
        }
        
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
  
  ##PART9##
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
    
    
    if((score$Out[max(which(score$Pit == winning_pitcher))] < 27) & (final_schedule$Home[x] %in% score$Home))
    {
      for(tf in 2:nrow(pitching_line_home))
      {
        if(sum(pitching_line_home$OUT[1:tf],na.rm=TRUE) >= 15)
        {
          winning_pitcher <- as.character(pitching_line_home$PlayerName[tf])
          break;
        }
        
        if(!sum(pitching_line_home$OUT[1:tf],na.rm=TRUE) >= 15)
        {
          next;
        }
      }
    }
    
    if((score$Out[max(which(score$Pit == winning_pitcher))] < 30) & (final_schedule$Away[x] %in% score$Visit))
    {
      for(tg in 2:nrow(pitching_line_visit))
      {
        if(sum(pitching_line_visit$OUT[1:tg],na.rm=TRUE) >= 15)
        {
          winning_pitcher <- as.character(pitching_line_visit$PlayerName[tg])
          break;
        }
        
        if(!sum(pitching_line_visit$OUT[1:tg],na.rm=TRUE) >= 15)
        {
          next;
        }
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
  
  
}