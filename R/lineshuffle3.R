# Today's task.

#0) Subset only one row for the MLe assignment
#1) Add feature in selection 1 to give an option of calling someone up outside 25 man roster if there were not enough man to give
# MLe to.
#2) Add feature in selection 2 to give an option of  giving MLe to someone if you couldn't call everyone up from outside 25 man

library(doSNOW)

setwd("/Volumes/BOOTCAMP/RFB")

cl <- makeCluster(4, type="SOCK") # for 4 cores machine
registerDoSNOW (cl)

con <- dbConnect(drv = MySQL(),user="jamesryu",password="ripken77",dbname="lineup",host="mysql.rfbdatabase.com")

final_schedule <- dbReadTable(con,"final_schedule")

for(a in 1:ncol(final_schedule))
{
  if(a == 1){final_schedule[,a] <- as.Date(final_schedule[,a],format="%Y-%m-%d")}
  else{final_schedule[,a] <- as.character(final_schedule[,a])}
}

batting <- read.csv("Batting/Batting_Master_Starts2.csv")

batting <- dbReadTable(con,"BAT_START")

BS <- dbReadTable(con,"BAT_START")

BB <- dbReadTable(con,"BAT_BENCH")

BP <- dbReadTable(con,"BAT_PINCH")

bat_master <- rbind(BS,BB)

dbDisconnect(con)
# Set the game date.

date <- 20170406
date2 <- 20170407

# Get the formatted date

formatted_date <- "2017-04-06"

# Creating Directory for the day of game

ifelse(!dir.exists(paste(getwd(),"/box/",formatted_date,sep="")), dir.create(paste(getwd(),"/box/",formatted_date,sep="")),FALSE)

# Coerce 'formatted_date' as date

formatted_date <- as.Date(formatted_date,format="%Y-%m-%d")

# Subset the schedule to have only games that on the day of set date

final_schedule <- final_schedule[final_schedule$Date %in% formatted_date,]

# Exclude rain out games

final_schedule <- final_schedule[!final_schedule$Rain %in% "Yes",]

all_pro <- c(final_schedule$Visit,final_schedule$Home)

#lineup <- read.csv(paste("lineup",date,".csv",sep=""))

con <- dbConnect(drv = MySQL(),user="jamesryu",password="ripken77",dbname="lineup",host="mysql.rfbdatabase.com")

lineup <- dbReadTable(con,"lineups")

play_pos <- read.csv("playerid_with_pos.csv")
play_pos <- dbReadTable(con,"playerid_with_pos")
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
    lineup$MLBId[i] <- as.character(only_active_players$MLBId[which(only_active_players$PlayerName %in% lineup$fullname[i])])
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

lineup <- lineup[,!colnames(lineup) %in% NA]

#1:length(all_pro)

#try 20,21 again later
for(iiii in 1)
{
  print(paste("Now at ",iiii,sep=""))
  
  batting_scan_start <- read.csv("Batting/Batting_Master_Starts2.csv")
  batting_scan_bench <- read.csv("Batting/Batting_Bench2.csv")
  
  batting_scan_start <- dbReadTable(con,"BAT_START")
  
  batting_scan_bench <- dbReadTable(con,"BAT_BENCH")
  
  MLE_keeper <- data.frame(matrix(NA,nrow=1,ncol=43))
  
  colnames(MLE_keeper) <- colnames(batting_scan_start)
  
  # Read bat report for all 25-man roster and minor leaguers
  
  bat <- dbReadTable(con,paste0(all_pro[iiii],date,"_batting_report"))
  
  bat$SB <- as.numeric(bat$Start) + as.numeric(bat$Bench)
  
  NL <- c("ARI","ATL","CHN","CIN","COL","MIA","LAN","MIL","NYN","PHI","PIT","SD","SF","STL","WAS")
  AL <- c("LAA","BAL","BOS","CHA","CLE","DET","HOU","KC","MIN","NYA","OAK","SEA","TB","TEX","TOR")
  
  pinching_scan <- read.csv("Batting/Batting_Master_Pinch2.csv")
  
  home <- which(final_schedule$Home %in% all_pro[iiii])
  visit <- which(final_schedule$Visit %in% all_pro[iiii])
  
  # 
  if(length(home) > 0){pos <- home}
  if(length(visit) > 0){pos <- visit}
  
  # If home venue is in AL, run it:
  
  if(final_schedule$Home[pos] %in% AL)
  {
    count <- 0
    
    if(length(which((lineup$POS == "P") & (lineup$Team == all_pro[iiii]))) == 0)
    {
      print("Lineup already has DH slot.")
    }
    
    if(length(which((lineup$POS == "P") & (lineup$Team == all_pro[iiii]))) > 0)
    {
      lineup$fullname[(lineup$POS == "P") & (lineup$Team == all_pro[iiii])] <- ""
      lineup$MLBId[(lineup$POS == "P") & (lineup$Team == all_pro[iiii])] <- ""
      lineup$POS2[(lineup$POS == "P") & (lineup$Team == all_pro[iiii])] <- "DH"
      lineup$POS[(lineup$POS == "P") & (lineup$Team == all_pro[iiii])] <- "DH" 
    }
    
    lining <- lineup[(lineup$Team %in% all_pro[iiii]) & (lineup$Role %in% c("#1","#2","#3","#4","#5","#6","#7","#8","#9")),]
    
    lining$S <- ""
    
    for(i in 1:nrow(lining))
    {
      if(length(which(batting_scan_start$MLBId %in% lining$MLBId[i])) > 0)
      {
        lining$S[i] <- "YES"
      }
      count2 <- length(which(lining$S == "YES"))
    }
    
    benchslot <- which(lineup$Role == "Bench" & lineup$Team == all_pro[iiii])
    
    benchp <- benchslot[lineup$MLBId[which(lineup$Role == "Bench" & lineup$Team == all_pro[iiii])] %in% bat$MLBId[bat$SB > 0]]
    
    if(length(benchp) > 0)
    {
      selected <- benchp[sample(1:length(benchp),size=1,replace=FALSE)]
      
      lineup$fullname[(lineup$POS == "DH") & (lineup$Team == all_pro[iiii])] <- lineup$fullname[selected]
      lineup$MLBId[(lineup$POS == "DH") & (lineup$Team == all_pro[iiii])] <- lineup$MLBId[selected]
      
      lineup <- lineup[-c(selected),]
    }
    
    if(length(benchp) == 0)
    {
      selected <- benchslot[sample(1:length(benchslot),size=1,replace=FALSE)]
      
      lineup$fullname[(lineup$POS == "DH") & (lineup$Team == all_pro[iiii])] <- lineup$fullname[selected]
      lineup$MLBId[(lineup$POS == "DH") & (lineup$Team == all_pro[iiii])] <- lineup$MLBId[selected]
      
      lineup <- lineup[-c(selected),]
    }
    
    for(vx in 1:nrow(lining))
    {
      if(lining$fullname[vx] %in% bat$PlayerName)
      {
        # Start+Bench stat is more than 0:
        if(bat$SB[bat$PlayerName %in% lining$fullname[vx]] > 0)
        {
          count <- count + 1
        }
        # If player has no stats
        if(bat$SB[bat$PlayerName %in% lining$fullname[vx]] == 0)
        {
          next;
        }
        
      }
      
      if(!lining$fullname[vx] %in% bat$PlayerName)
      {
        next;
      }
      
    }
    
    available_pos <- c("CA","1B","2B","3B","SS","LF","CF","RF","P")
    # If lineup has multiple positions, get it fixed
    if(FALSE %in% (available_pos %in% lining$POS)){
      
      pos_tab <- data.frame(num=c(2,3,4,5,6,7,8,9),pos=c("CA","1B","2B","3B","SS","LF","CF","RF"))
      
      missing <- available_pos[!available_pos %in% lining$POS]
      
      count_pos <- table(lining$POS)
      
      count_pos <- as.data.frame(count_pos)
      
      count_pos$pos <- ""

      count_pos$pos[which(count_pos$Var1 %in% "CA")] <- 2
      count_pos$pos[which(count_pos$Var1 %in% "1B")] <- 3
      count_pos$pos[which(count_pos$Var1 %in% "2B")] <- 4
      count_pos$pos[which(count_pos$Var1 %in% "3B")] <- 5
      count_pos$pos[which(count_pos$Var1 %in% "SS")] <- 6
      count_pos$pos[which(count_pos$Var1 %in% "LF")] <- 7
      count_pos$pos[which(count_pos$Var1 %in% "CF")] <- 8
      count_pos$pos[which(count_pos$Var1 %in% "RF")] <- 9
      count_pos$pos[which(count_pos$Var1 %in% "DH")] <- "DH"
      
      mult <- which(count_pos$Freq > 1)
      
      switch <- as.character(count_pos$Var1[mult])
      
      slots <- which(lining$POS %in% switch) 
      
      if(length(slots) > 0){
        
        for(t in 1:length(missing)){
          
          lining$POS[slots[1]] <- missing
          
          lining$fullname[slots[1]] <- ""
          lining$MLBId[slots[1]] <- ""
          lining$fullname[slots[1]] <- ""
          lining$POS2[slots[1]] <- pos_tab$num[pos_tab$pos == missing]
          
        }
      }
      
    }
  }
  
  # If home venue is in NL park, run this:
  if(final_schedule$Home[pos] %in% NL)
  {
    
    # Run this if lineup of the team of interest has no DH slot
    if(length(which((lineup$POS == "DH") & (lineup$Team == all_pro[iiii]))) == 0)
    {
      print("Lineup already has pitching slot.")
    }
    
    # Run this if lineup of the team of interest has DH slot
    if(length(which((lineup$POS %in% "DH") & (lineup$Team %in% all_pro[iiii]))) > 0)
    {
      
      # Saves the name of player who was in DH slot 
      the_name <- as.character(lineup$fullname[(lineup$POS == "DH") & (lineup$Team == all_pro[iiii])])
      
      # Saves the id of player who was in DH slot
      the_id <- as.character(lineup$MLBId[(lineup$POS == "DH") & (lineup$Team == all_pro[iiii])])
      
      # Saves the pos of player who was in DH slot
      the_pos <- as.character(lineup$POS[(lineup$POS == "DH") & (lineup$Team == all_pro[iiii])])
      
      # Saves the pos2 of player who was in DH slot
      the_pos2 <- as.character(lineup$POS[(lineup$POS2 == "DH") & (lineup$Team == all_pro[iiii])])
      
      # Delete the name of a player who was in DH slot
      lineup$fullname[(lineup$POS == "DH") & (lineup$Team == all_pro[iiii])] <- ""
      
      # Delete the mlbid of a player who was in DH slot
      lineup$MLBId[(lineup$POS == "DH") & (lineup$Team == all_pro[iiii])] <- ""
      
      # Replace "DH" with "1"
      lineup$POS2[(lineup$POS == "DH") & (lineup$Team == all_pro[iiii])] <- "1"
      
      # Replace "DH" with "P"
      lineup$POS[(lineup$POS == "DH") & (lineup$Team == all_pro[iiii])] <- "P" 
      
      # Figures out which row number of last bench player in the team
      bench_insert <- max(which((lineup$Role == "Bench") & (lineup$Team %in% all_pro[iiii])))
      
      # Create a data frame where you will fill it with DH hitter's name
      middle <- data.frame(matrix(NA,nrow=1,ncol=ncol(lineup)))
      
      # Set the column names of "middle"
      colnames(middle) <- colnames(lineup)
      
      middle$Role[1] <- "Bench"
      middle$fullname[1] <- the_name
      middle$Date[1] <- NA
      middle$POS[1] <- the_pos
      middle$Team[1] <- all_pro[iiii]
      middle$MLBId[1] <- the_id
      middle$`Fielding Petition`[1] <- "NO"
      middle$POS2[1] <- the_pos2
      
      # Insert the "middle" between the team's last bench player and the new team
      
      lineup <- rbind(lineup[1:bench_insert,],middle,lineup[(bench_insert+1):nrow(lineup),])
      
      # If pitching slot of the batting lineup has no name, run this:
      
      if(lineup$fullname[(lineup$POS == "P") & (lineup$Team == all_pro[iiii])] %in% c("",NA,"NA"))
      {
        # May need fix: Currently set to include pitcher name and MLBId set to game date
        lineup$fullname[which((lineup$POS == "P") & (lineup$Team == all_pro[iiii]))] <- lineup$fullname[which((lineup$POS == "SP") & (lineup$Date == formatted_date) & (lineup$Team == all_pro[iiii]))]
        lineup$MLBId[which((lineup$POS == "P") & (lineup$Team == all_pro[iiii]))] <- lineup$MLBId[which((lineup$POS == "SP") & (lineup$Date == formatted_date) & (lineup$Team == all_pro[iiii]))]
        
      }
    }
    
    # Subset lineup to include starting lineup from all_pro[iiii]
    lining <- lineup[(lineup$Team %in% all_pro[iiii]) & (lineup$Role %in% c("#1","#2","#3","#4","#5","#6","#7","#8","#9")),]
    
    # Create S column 
    lining$S <- ""
    
    
    for(i in 1:nrow(lining))
    {
      # If there is one or more start from a player i, give "YES" in lining$s[i]
      if(length(which(batting_scan_start$MLBId %in% lining$MLBId[i])) > 0)
      {
        lining$S[i] <- "YES"
      }
      # Count how many players in the starting lineup has start stats
      count2 <- length(which(lining$S == "YES"))
    }
    ###
    
    # Initialize 'count' variable to 0
    count <- 0
    
    for(vx in 1:nrow(lining))
    {
      # If player in position vx in lining has no match in bat
      if(lining$fullname[vx] %in% bat$PlayerName)
      {
        # Skip if lining$POS equals to "P
        if(lining$POS[vx] == "P")
        {
          next;
        }
        
        # If player in vx position has one or more stats, add 1 to 'count'
        if(bat$SB[bat$PlayerName %in% lining$fullname[vx]] > 0)
        {
          count <- count + 1
        }
        
        # If player in vx position has zero stats
        if(bat$SB[bat$PlayerName %in% lining$fullname[vx]] == 0)
        {
          next;
        }
      }
      
      #If player in vx position of lining has no match in 'bat'
      if(!lining$fullname[vx] %in% bat$PlayerName)
      {
        next;
      }
      
    }
    
    available_pos <- c("CA","1B","2B","3B","SS","LF","CF","RF","P")
    # If lineup has multiple positions, get it fixed
    if(FALSE %in% (available_pos %in% lining$POS)){
      
      pos_tab <- data.frame(num=c(2,3,4,5,6,7,8,9),pos=c("CA","1B","2B","3B","SS","LF","CF","RF"))
      
      missing <- available_pos[!available_pos %in% lining$POS]
      
      count_pos <- table(lining$POS)
      
      count_pos <- as.data.frame(count_pos)
      
      count_pos$pos <- c(2,3,4,5,6,7,8,9)
      
      mult <- which(count_pos$Freq > 1)
      
      
      switch <- as.character(count_pos$Var1[mult])
      
      slots <- which(lining$POS %in% switch) 
      
      if(length(slots) > 0){
         
        for(t in 1:length(missing)){
          
          lining$POS[slots[1]] <- missing
          
          lining$fullname[slots[1]] <- ""
          lining$MLBId[slots[1]] <- ""
          lining$fullname[slots[1]] <- ""
          lining$POS2[slots[1]] <- pos_tab$num[pos_tab$pos == missing]
          
        }
      }
      
    }
  }
  
  # Run this if the starting lineup has less than 9 players with no starting stats
  if(count2 < 9)
  {
    # Count number of players with no starter stat
    less_pos <- which(lining$S == "")
    
    # Initialize PA_needed to 0
    PA_needed <- length(less_pos)
  
  }
  
  # If there is at least one player with no starter stat
  #if(length(less_pos) > 0)
  #{
  # for(i in 1:length(less_pos))
  #{
  # 
  # if(less_pos[i] != 8)
  #{
  # bat_master2 <- bat_master[bat_master$MLBId %in% lining$MLBId[less_pos[i]],]
  #bat_master2 <- bat_master2[order(bat_master2$GameDate),]
  #bat_master2 <- bat_master2[1,]
  #PA_needed <- PA_needed + 1
  #}
  
  #if(less_pos[i] == 9)
  #{
  # bat_master2 <- bat_master[bat_master$MLBId %in% lining$MLBId[less_pos[i]],]
  #bat_master2 <- bat_master2[order(bat_master2$GameDate),]
  #bat_master2 <- bat_master2[1,]
  #PA_needed <- PA_needed + 1
  #}
  
  #}
  #}
  
  away_which <- vector()
  home_which <- vector()
  
  # Run if all_pro[iiii] is in final_schedule$Visit
  if(all_pro[iiii] %in% final_schedule$Visit)
  {
    # At which row of schedule is all_pro[iiii] in
    away_which <- which(final_schedule$Visit %in% all_pro[iiii])
  }
  
  # Run if all_pro[iiii] is not in final_schedule$Visit
  if(!all_pro[iiii] %in% final_schedule$Visit)
  {
    away_which <- which(final_schedule$Visit %in% all_pro[iiii])
  }
  
  # Run if all_pro[iiii] is in final_schedule$Home
  if(all_pro[iiii] %in% final_schedule$Home)
  {
    home_which <- which(final_schedule$Home %in% all_pro[iiii])
  }
  
  # Run if all_pro[iiii] is not in final_schedule$Home
  if(!all_pro[iiii] %in% final_schedule$Home)
  {
    home_which <- which(final_schedule$Home %in% all_pro[iiii])
  }
  
  pos_sim <- read.csv("pos_sim.csv")
  
  lining$eligible <- ""
  
  for(v in 1:nrow(lining)){
    
    if(lining$fullname[v] %in% c("NA",NA,"")){
      next;
    }
    
    if((!lining$fullname[v] %in% c("NA",NA,"")) & (!lining$POS[v] %in% "P")){
      possibility <- pos_sim[,bat$most_inning_primary[bat$MLBId %in% lining$MLBId[v]]]
      possibility <- possibility[!possibility %in% "NO"]
      possibility <- unlist(strsplit(as.character(possibility),""))
      
      if(!lining$POS2[v] %in% possibility){
        lining$eligible[v] <- "NO"
      }
    }
  }
  
  # If length of away_which is one or higher
  if(length(away_which) > 0)
  {
    # count is 8 or higher and count2 is 8 or higher and final_schedule$Home[away_which] is in NL
    if((count >= 8) & (count2 >= 8) & ((final_schedule$Home[away_which]) %in% NL) & !("NO" %in% lining$eligible))
    {
      print(paste(all_pro[iiii]," lineup untouched.",sep=""))
      next;
    }
    
  }
  
  if(length(home_which) > 0)
  {
    if((count >= 8) & (count2 >= 8) & ((final_schedule$Home[home_which]) %in% NL) & !("NO" %in% lining$eligible))
    {
      print(paste(all_pro[iiii]," lineup untouched.",sep=""))
      next;
    }
  }
  
  if(length(away_which) > 0)
  {
    if((count >= 9) & (count2 == 9) & ((final_schedule$Visit[away_which]) %in% AL) & !("NO" %in% lining$eligible))
    {
      print(paste(all_pro[iiii]," lineup untouched.",sep=""))
      next;
    }
  }
  
  if(length(home_which) > 0)
  {
    if((count >= 9) & (count2 == 9) & ((final_schedule$Home[home_which]) %in% AL) & !("NO" %in% lining$eligible))
    {
      print(paste(all_pro[iiii]," lineup untouched.",sep=""))
      next;
    }
  }
  
  # Subset bat to only have 25 men in the roster
  
  bat <- bat[bat$X25.man == "YES",]
  
  # Remove pitchers
  
  removals <- vector()
  
  for(zz in 1:nrow(bat)){
    
    if(c(NA,"") %in% unlist(strsplit(as.character(bat$primary[zz]),""))){
      next;
    }
    
    if(1 %in% unlist(strsplit(as.character(bat$primary[zz]),""))){
      
      removals[zz] <- zz
    }
  }
  
  removals <- removals[!removals %in% NA]
  
  removals <- as.numeric(removals)
  
  bat <- bat[!c(1:nrow(bat)) %in% removals,]
  # Remove rows with NA in the "Pos" column
  
  bat <- bat[!bat$Pos %in% c(NA,"NA"),]
  
  # Create SB and available columns
  
  bat$SB <- ""
  bat$available <- ""
  
  # Load batting report of team into bat2
  
  bat2 <- dbReadTable(con,paste0(all_pro[iiii],date,"_batting_report"))
  
  # Create SB and available columns in bat2
  
  bat2$SB <- ""
  bat2$available <- ""
  
  bat2 <- bat2[!c(1:nrow(bat2)) %in% removals,]
  
  bat2 <- bat2[bat2$X25.man == "NO",]
  
  bat2 <- bat2[!bat2$Pos %in% c(NA,"NA"),]
  
  # Load Position similarities chart
  
  pos_sim <- read.csv("pos_sim.csv")
  
  # Create data frame called "depth"
  
  depth <- data.frame(matrix("",nrow=20,ncol=8))
  
  # Penalty
  
  penalty <- c(-5,-9,-15,-23,-33,-45)
  
  # Name columns of depth
  
  colnames(depth) <- c("C","1B","2B","3B","SS","LF","CF","RF")
  
  # Coerce all columns to depth
  
  for(a in 1:ncol(depth))
  {
    depth[,a] <- as.character(depth[,a])
  }
  
  # Create data frame called "depth2"
  
  depth2 <- data.frame(matrix("",nrow=20,ncol=8))
  
  # Name columns of depth2
  
  colnames(depth2) <- c("C","1B","2B","3B","SS","LF","CF","RF")
  
  # Coerce columns to characters
  
  for(aa in 1:ncol(depth2))
  {
    depth2[,aa] <- as.character(depth2[,aa])
  }
  
  # Create data frame called "depth3"
  
  depth3 <- data.frame(matrix("",nrow=20,ncol=8))
  
  # Name all columns of depth3
  
  colnames(depth3) <- c("C","1B","2B","3B","SS","LF","CF","RF")
  
  # Coerce columns in depth3 to characters
  
  for(aaa in 1:ncol(depth3))
  {
    depth3[,aaa] <- as.character(depth3[,aaa])
  }
  
  # Load start and bench stats

  start <- dbReadTable(con,"BAT_START")
  
  bench <- dbReadTable(con,"BAT_BENCH")
  
  # row bind start and bench
  all <- rbind(start,bench)
  
  # Coerce 'GameDate' column to date format
  all$GameDate <- as.Date(all$GameDate)
  
  # Subset all to have MLBId.
  all2 <- all[all$MLBId %in% bat$MLBId,]
  
  # Characterize PlayerName
  all2$PlayerName <- as.character(all2$PlayerName)
  
  # Create data frame called MLE_keeper_pinch
  MLE_keeper_pinch <- data.frame(matrix(NA,nrow=1,ncol=43))
  
  # Set column names of MLE_keeper_pinch
  colnames(MLE_keeper_pinch) <- colnames(all)
  
  # For each position (2 to 9), list players who can play each position primarily
  for(i in 2:9)
  {
    count <- 0
    
    for(j in 1:nrow(bat))
    {
      positions <- (unlist(strsplit(as.character(bat$primary[j]),"")))
      
      for(k in 1:length(positions))
      {
        if(i %in% positions[k])
        {
          count <- count + 1
          depth[count,i-1] <- as.character(bat$PlayerName[j])
        }
        
        if(!i %in% positions[k])
        {
          next;
        }
      }
    }
  }
  
  # For each position (2 to 9), list players who can play each position secondarily
  
  for(ii in 2:9)
  {
    count <- 0
    
    for(jj in 1:nrow(bat))
    {
      positions <- (unlist(strsplit(as.character(bat$secondary[jj]),"")))
      
      for(kk in 1:length(positions))
      {
        if(ii %in% positions[kk])
        {
          count <- count + 1
          depth2[count,ii-1] <- as.character(bat$PlayerName[jj])
        }
        
        if(!ii %in% positions[kk])
        {
          next;
        }
      }
    }
  }
  
  # For each position (2 to 9), list players who can play each position tertiarily
  
  for(iii in 2:9)
  {
    count <- 0
    
    for(jjj in 1:nrow(bat))
    {
      positions <- (unlist(strsplit(as.character(bat$tertiary[jjj]),"")))
      
      for(kkk in 1:length(positions))
      {
        if(iii %in% positions[kkk])
        {
          count <- count + 1
          depth3[count,iii-1] <- as.character(bat$PlayerName[jjj])
        }
        
        if(!iii %in% positions[kkk])
        {
          next;
        }
      }
    }
  }
  
  catcher <- c(depth$C,depth2$C,depth3$C)
  catcher <- catcher[catcher != ""]
  
  first <- c(depth$`1B`,depth2$`1B`,depth3$`1B`)
  first <- first[first != ""]
  
  second <- c(depth$`2B`,depth2$`2B`,depth3$`2B`)
  second <- second[second != ""]
  
  third <- c(depth$`3B`,depth2$`3B`,depth3$`3B`)
  third <- third[third != ""]
  
  short <- c(depth$SS,depth2$SS,depth3$SS)
  short <- short[short != ""]
  
  left <- c(depth$LF,depth2$LF,depth3$LF)
  left <- left[left != ""]
  
  center <- c(depth$CF,depth2$CF,depth3$CF)
  center <- center[center != ""]
  
  right <- c(depth$RF,depth2$RF,depth3$RF)
  right <- right[right != ""]
  
  final <- expand.grid(list(catcher,first,second,third,short,left,center,right))
  
  #rm(catcher,first,second,third,short,left,center,right)
  
  colnames(final) <- c("CA","1B","2B","3B","SS","LF","CF","RF")
  
  two <- as.character(final$CA)
  three <- as.character(final$`1B`)
  four <- as.character(final$`2B`)
  five <- as.character(final$`3B`)
  six <- as.character(final$SS)
  seven <- as.character(final$LF)
  eight <- as.character(final$CF)
  nine <- as.character(final$RF)
  
  which_dup <- vector()
  
  # 
  for(i in 1:length(final$CA))
  {
    ifelse(any(duplicated(c(two[i],three[i],four[i],five[i],six[i],seven[i],eight[i],nine[i]))),which_dup[i] <- i,next)
    print(i)
  }
  
  which_dup <- which_dup[!is.na(which_dup)]
  
  final <- final[!(c(1:nrow(final)) %in% which_dup),]
  
  bat$SB <- as.numeric(bat$Start) + as.numeric(bat$Bench)
  
  bat$available <- ""
  
  for(z in 1:nrow(bat))
  {
    ifelse(bat$SB[z] > 0,bat$available[z] <- "YES",bat$available[z] <- "NO")
  }
  
  final$SB <- ""
  
  final$SB <- as.integer(0)
  
  # If a player in y row at each position has stats available (S or B), add 1 to SB column at row y
  for(y in 1:nrow(final))
  {
    final$SB[y] <- ifelse(bat$available[bat$PlayerName %in% as.character(final$CA[y])] == "YES",final$SB[y] <- as.integer(final$SB[y]) + 1,final$SB[y] <- as.integer(final$SB[y]) + 0)
    final$SB[y] <- ifelse(bat$available[bat$PlayerName %in% as.character(final$`1B`[y])] == "YES",final$SB[y] <- as.integer(final$SB[y]) + 1,final$SB[y] <- as.integer(final$SB[y]) + 0)
    final$SB[y] <- ifelse(bat$available[bat$PlayerName %in% as.character(final$`2B`[y])] == "YES",final$SB[y] <- as.integer(final$SB[y]) + 1,final$SB[y] <- as.integer(final$SB[y]) + 0)
    final$SB[y] <- ifelse(bat$available[bat$PlayerName %in% as.character(final$`3B`[y])] == "YES",final$SB[y] <- as.integer(final$SB[y]) + 1,final$SB[y] <- as.integer(final$SB[y]) + 0)
    final$SB[y] <- ifelse(bat$available[bat$PlayerName %in% as.character(final$SS[y])] == "YES",final$SB[y] <- as.integer(final$SB[y]) + 1,final$SB[y] <- as.integer(final$SB[y]) + 0)
    final$SB[y] <- ifelse(bat$available[bat$PlayerName %in% as.character(final$LF[y])] == "YES",final$SB[y] <- as.integer(final$SB[y]) + 1,final$SB[y] <- as.integer(final$SB[y]) + 0)
    final$SB[y] <- ifelse(bat$available[bat$PlayerName %in% as.character(final$CF[y])] == "YES",final$SB[y] <- as.integer(final$SB[y]) + 1,final$SB[y] <- as.integer(final$SB[y]) + 0)
    final$SB[y] <- ifelse(bat$available[bat$PlayerName %in% as.character(final$RF[y])] == "YES",final$SB[y] <- as.integer(final$SB[y]) + 1,final$SB[y] <- as.integer(final$SB[y]) + 0)
    print(y) 
  }
  
  # Order by SB
  final <- final[order(final$SB,decreasing=TRUE),]
  
  # Select lineup combo with highest SB
  final <- final[final$SB == max(final$SB),]
  
  # Create Penalty column
  final$Penalty <- ""
  
  final$Penalty <- as.numeric(final$Penalty)
  
  final$Penalty <- 0
  
  # Blank the player that has no Start of Bench stats
  for(d in 1:nrow(final))
  {
    final$CA[which(!final$CA %in% bat$PlayerName[bat$SB > 0])] <- ""
    final$`1B`[which(!final$`1B` %in% bat$PlayerName[bat$SB > 0])] <- ""
    final$`2B`[which(!final$`2B` %in% bat$PlayerName[bat$SB > 0])] <- ""
    final$`3B`[which(!final$`3B` %in% bat$PlayerName[bat$SB > 0])] <- ""
    final$SS[which(!final$SS %in% bat$PlayerName[bat$SB > 0])] <- ""
    final$LF[which(!final$LF %in% bat$PlayerName[bat$SB > 0])] <- ""
    final$CF[which(!final$CF %in% bat$PlayerName[bat$SB > 0])] <- ""
    final$RF[which(!final$RF %in% bat$PlayerName[bat$SB > 0])] <- ""
    
  }
  
  # Retain column names that is not "SB" or "Penalty"
  col_lengths <- colnames(final)[!colnames(final) %in% c("SB","Penalty")]
  
  # Assign penalty to all players in each row
  for(f in 1:length(col_lengths))
  {
    name <- as.character(unique(final[,f]))
    
    for(e in 1:length(name))
    {
      out_position <- vector()
      
      primary_pos <- bat$most_inning_primary[bat$PlayerName == name[e]]
      
      out_of_pos <- pos_sim[,f+1]
      
      out_of_pos <- out_of_pos[!is.na(out_of_pos)]
      
      out_position <- which(out_of_pos %in% (f+1))
      
      if((f+1) %in% unlist(strsplit(as.character(bat$primary[bat$PlayerName == name[e]]),"")))
      {
        final$Penalty[which(final[,f] == name[e])] <- final$Penalty[which(final[,f] == name[e])] + 1
      }
      
      if((f+1) %in% unlist(strsplit(as.character(bat$secondary[bat$PlayerName == name[e]]),"")))
      {
        final$Penalty[which(final[,f] == name[e])] <- final$Penalty[which(final[,f] == name[e])] - 1
      }
      
      if((f+1) %in% unlist(strsplit(as.character(bat$tertiary[bat$PlayerName == name[e]]),"")))
      {
        final$Penalty[which(final[,f] == name[e])] <- final$Penalty[which(final[,f] == name[e])] - 3
      }
      
      if(out_position == 2)
      {
        final$Penalty[which(final[,f] == name[e])] <- final$Penalty[which(final[,f] == name[e])] - 5
        
      }
      
      if(out_position == 3)
      {
        final$Penalty[which(final[,f] == name[e])] <- final$Penalty[which(final[,f] == name[e])] - 9
        
      }
      
      if(out_position == 4)
      {
        final$Penalty[which(final[,f] == name[e])] <- final$Penalty[which(final[,f] == name[e])] - 15
        
      }
      
      if(out_position == 5)
      {
        final$Penalty[which(final[,f] == name[e])] <- final$Penalty[which(final[,f] == name[e])] - 23
        
      }
      
      if(out_position == 6)
      {
        final$Penalty[which(final[,f] == name[e])] <- final$Penalty[which(final[,f] == name[e])] - 33
        
      }
      
      if(out_position == 7)
      {
        final$Penalty[which(final[,f] == name[e])] <- final$Penalty[which(final[,f] == name[e])] - 45
        
      }
    }
  }
  
  for(tt in 1:length(col_lengths))
  {
    final[,tt] <- as.character(final[,tt])
  }
  
  if(any(final$SB < 8))
  {
    # h for column
    for(h in 1:length(col_lengths))
    {
      empty <- which(final[,h] %in% NA)
      
      if(length(empty) == 0)
      {
        next;
      }
      
      for(k in 1:length(empty)){
        
        if(TRUE %in% (final[empty[k],h] %in% NA))
        {
          
          # k for row
          
          lineup_row <- as.vector(t(final[empty[k],])[1:length(col_lengths)])
          lineup_row <- lineup_row[!is.na(lineup_row)]
          not_in_start <- bat$PlayerName[!(bat$PlayerName %in% lineup_row) & (bat$SB > 0)]
          not_in_start <- as.character(not_in_start)
          
          if(length(not_in_start) > 1)
          {
            name_number <- vector()
            naming <- vector()
            
            for(s in 1:length(not_in_start))
            {
              possible <- pos_sim[,as.numeric(bat$most_inning_primary[bat$PlayerName == not_in_start[s]])]
              possible <- possible[!is.na(possible)]
              number <- which(possible %in% (h+1))
              
              if(length(number) > 0)
              {
                name_number[s] <- number
              }
              
              if(length(number) == 0)
              {
                name_number[s] <- NA
              }
              
              if(length(number) > 0)
              {
                naming[s] <- not_in_start[s]
                
              }
              
              if(length(number) == 0)
              {
                naming[s] <- NA
              }
            }
            
            if(length(name_number) > 0)
            {
              naming <- naming[!naming %in% NA]
              
              name_number <- name_number[!name_number %in% NA]
              
              chosen_player <- naming[which(name_number == min(name_number))]
              chosen_number <- name_number[which(name_number == min(name_number))]
              
              if(length(chosen_number) > 1)
              {
                chosen_number <- chosen_number[1]
              }
              
              if(length(chosen_player) > 1)
              {
                chosen_player <- chosen_player[1]
              }
              
              if(length(chosen_number) == 0)
              {
                next;
              }
              
              if(chosen_number == 2)
              {
                final[empty[k],h] <- as.character(chosen_player)
                final$Penalty[empty[k]] <- final$Penalty[empty[k]] - 5
                final$SB[empty[k]] <- final$SB[empty[k]] + 1
              }
              
              if(chosen_number == 3)
              {
                final[empty[k],h] <- as.character(chosen_player)
                final$Penalty[empty[k]] <- final$Penalty[empty[k]] - 9
                final$SB[empty[k]] <- final$SB[empty[k]] + 1
                
              }
              
              if(chosen_number == 4)
              {
                final[empty[k],h] <- as.character(chosen_player)
                final$Penalty[empty[k]] <- final$Penalty[empty[k]] - 13
                final$SB[empty[k]] <- final$SB[empty[k]] + 1
                
              }
              
              if(chosen_number == 5)
              {
                final[empty[k],h] <- as.character(chosen_player)
                final$Penalty[empty[k]] <- final$Penalty[empty[k]] - 17
                final$SB[empty[k]] <- final$SB[empty[k]] + 1
                
              }
              
              if(chosen_number == 6)
              {
                final[empty[k],h] <- as.character(chosen_player)
                final$Penalty[empty[k]] <- final$Penalty[empty[k]] - 21
                final$SB[empty[k]] <- final$SB[empty[k]] + 1
                
              }
              
              if(chosen_number == 7)
              {
                final[empty[k],h] <- as.character(chosen_player)
                final$Penalty[empty[k]] <- final$Penalty[empty[k]] - 25
                final$SB[empty[k]] <- final$SB[empty[k]] + 1
                
              }
              
            }
            
            if(length(name_number) == 0)
            {
              next;
            }
            
          }
          
          if(length(not_in_start) == 1)
          {
            
            possible <- pos_sim[,as.numeric(bat$most_inning_primary[bat$PlayerName == not_in_start])]
            possible <- possible[!is.na(possible)]
            number <- which(possible %in% (h+1))
            
            if(length(number) > 0){
              if(number == 2)
              {
                final[empty[k],h] <- as.character(not_in_start)
                final$Penalty[empty[k]] <- final$Penalty[empty[k]] - 5
                final$SB[empty[k]] <- final$SB[empty[k]] + 1
              }
              
              if(number == 3)
              {
                final[empty[k],h] <- as.character(not_in_start)
                final$Penalty[empty[k]] <- final$Penalty[empty[k]] - 9
                final$SB[empty[k]] <- final$SB[empty[k]] + 1
                
              }
              
              if(number == 4)
              {
                final[empty[k],h] <- as.character(not_in_start)
                final$Penalty[empty[k]] <- final$Penalty[empty[k]] - 13
                final$SB[empty[k]] <- final$SB[empty[k]] + 1
                
              }
              
              if(number == 5)
              {
                final[empty[k],h] <- as.character(not_in_start)
                final$Penalty[empty[k]] <- final$Penalty[empty[k]] - 17
                final$SB[empty[k]] <- final$SB[empty[k]] + 1
                
              }
              
              if(number == 6)
              {
                final[empty[k],h] <- as.character(not_in_start)
                final$Penalty[empty[k]] <- final$Penalty[empty[k]] - 21
                final$SB[empty[k]] <- final$SB[empty[k]] + 1
                
              }
              
              if(number == 7)
              {
                final[empty[k],h] <- as.character(not_in_start)
                final$Penalty[empty[k]] <- final$Penalty[empty[k]] - 25
                final$SB[empty[k]] <- final$SB[empty[k]] + 1
                
              }
            }
            
            if(length(number) == 0)
            {
              next;
            }
            
            
          }
          
          if(length(not_in_start) == 0)
          {
            #next;
          }
          
          
          
        }
        
        if(FALSE %in% (final[,h] %in% NA))
        {
          next;
        }
      }
    }
    
    
    
  }
  
  NL <- c("ARI","ATL","CHN","CIN","COL","MIA","LAN","MIL","NYN","PHI","PIT","SD","SF","STL","WAS")
  AL <- c("LAA","BAL","BOS","CHA","CLE","DET","HOU","KC","MIN","NYA","OAK","SEA","TB","TEX","TOR")
  
  away_which <- vector()
  home_which <- vector()
  
  if(all_pro[iiii] %in% final_schedule$Visit)
  {
    away_which <- which(final_schedule$Visit %in% all_pro[iiii])
  }
  
  if(!all_pro[iiii] %in% final_schedule$Visit)
  {
    away_which <- which(final_schedule$Visit %in% all_pro[iiii])
  }
  
  if(all_pro[iiii] %in% final_schedule$Home)
  {
    home_which <- which(final_schedule$Home %in% all_pro[iiii])
  }
  
  if(!all_pro[iiii] %in% final_schedule$Home)
  {
    home_which <- which(final_schedule$Home %in% all_pro[iiii])
  }
  
  if(length(home_which) > 0)
  {
    # 
    if((final_schedule$Home[home_which] %in% AL == TRUE))
    {
      final$DH <- ""
      
      for(kk in 1:nrow(final))
      {
        lineup_row <- as.vector(t(final[kk,])[1:length(col_lengths)])
        lineup_row <- lineup_row[!is.na(lineup_row)]
        
        not_in_start <- bat$PlayerName[!(bat$PlayerName %in% lineup_row) & (bat$SB > 0)]
        not_in_start <- bat[bat$PlayerName %in% not_in_start,]
        not_in_start <- not_in_start[not_in_start$Pos != "",]
        not_in_start <- not_in_start[not_in_start$SB > 0,]
        not_in_start <- not_in_start[sample(1:nrow(not_in_start),size=1,replace = FALSE),]
        not_in_start <- not_in_start$PlayerName[1]
        
        not_in_start <- as.character(not_in_start)
        
        final[kk,"DH"] <- not_in_start
        
        if(final[kk,"DH"] %in% c(NA,"NA")){
          
          final[kk,"SB"] <- as.numeric(final[kk,"SB"])
        }
        
        if(!final[kk,"DH"] %in% c(NA,"NA")){
          final[kk,"SB"] <- as.numeric(final[kk,"SB"]) + 1
          
        }
      }
      
    }
    
    if(final_schedule$Home[home_which] %in% NL == TRUE)
    {
      
    }
  }
  
  if(length(away_which) > 0)
  {
    # Run if game is played in AL ballpark
    # This block is for adding DH to the lineup that is missing DH
    if((final_schedule$Home[away_which] %in% AL == TRUE))
    {
      final$DH <- ""
      
      for(kk in 1:nrow(final))
      { 
        # Get a list of players in final[kk,]. (Who are eligible to play with start or bench stats)
        lineup_row <- as.vector(t(final[kk,])[1:length(col_lengths)])
        lineup_row <- lineup_row[!is.na(lineup_row)]
        
        # Find players with at least one stat and on bench
        not_in_start <- bat$PlayerName[!(bat$PlayerName %in% lineup_row) & (bat$SB > 0)]
        not_in_start <- bat[bat$PlayerName %in% not_in_start,]
        not_in_start <- not_in_start[not_in_start$Pos != "",]
        not_in_start <- not_in_start[not_in_start$SB > 0,]
        not_in_start <- not_in_start[sample(1:nrow(not_in_start),size=1,replace = FALSE),]
        not_in_start <- not_in_start$PlayerName[1]
        
        not_in_start <- as.character(not_in_start)
        
        final[kk,"DH"] <- not_in_start
        
        if(final[kk,"DH"] %in% c(NA,"NA")){
          
          final[kk,"SB"] <- as.numeric(final[kk,"SB"])
        }
        
        if(!final[kk,"DH"] %in% c(NA,"NA")){
          final[kk,"SB"] <- as.numeric(final[kk,"SB"]) + 1
          
        }
      }
      
    }
    
    if(final_schedule$Home[away_which] %in% NL == TRUE)
    {
      
    }
  }
  
  # Count S and B separately
  
  final$S <- ""
  final$B <- ""
  
  final$B <- as.numeric(final$B)
  final$S <- as.numeric(final$S)
  
  final$B <- 0
  final$P <- 0
  
  # Read bench and starting stats
 
  bat_S_scan <- dbReadTable(con,"BAT_START")
  
  bat_B_scan <- dbReadTable(con,"BAT_BENCH")

  batBS <- rbind(bat_B_scan,bat_S_scan)
  
  for(l in 1:nrow(final))
  {
    print(paste0(l," of ",nrow(final)))
    if("DH" %in% colnames(final))
    {
      column <- c("CA","1B","2B","3B","SS","LF","CF","RF","DH")
      
    }
    
    if(!"DH" %in% colnames(final))
    {
      column <- c("CA","1B","2B","3B","SS","LF","CF","RF")
    }
    
    # Count number of bench stats used. (So you can determine how many PHs are needed)
    for(ll in 1:length(column))
    {
      if(final[l,column[ll]] %in% c(NA,"NA")){
        next;
      }
      
      if(!final[l,column[ll]] %in% c(NA,"NA")){
        batBS2 <- batBS[which(batBS$PlayerName %in% final[l,column[ll]]),]
        batBS2 <- batBS2[order(batBS2$GameDate,decreasing=TRUE),]
        PAs <- batBS2$PA[1]
        
        if(PAs %in% NA){
          next;
        }
        
        if(!PAs %in% NA){
          if(PAs >=3)
          {
            final$B[l] <- final$B[l] + 0
          }
          
          if(PAs == 2)
          {
            final$B[l] <- final$B[l] + 1
          }
        }
        
        
      }
      
    }
  }
  
  bat$starts <- ""
  bat$starts[which(bat$Start > 0)] <- "YES"
  
  for(i in 1:nrow(final)){
    
    if(any(colnames(final) %in% "DH")){
      
      final$S[i] <- length(which(bat$starts[bat$PlayerName %in% as.vector(t(final[i,c("CA","1B","2B","3B","SS","LF","CF","RF","DH")]))] == "YES"))
    } 
    
    if(!any(colnames(final) %in% "DH")){
      final$S[i] <- length(which(bat$starts[bat$PlayerName %in% as.vector(t(final[i,c("CA","1B","2B","3B","SS","LF","CF","RF")]))] == "YES"))
      
    }
  }
  
  # MLe count
  
  final$PH <- ""
  final$num_NA <- ""
  
  # Count number of PH possible in each combination of lineup
  # Number of players missing from each row is counted
  for(k in 1:nrow(final))
  {
    number_of_NA <- as.vector(t(final[k,])[1:8])
    number_of_NA <- length(number_of_NA[is.na(number_of_NA)])
    
    lineup_row <- as.vector(t(final[k,])[1:8])
    lineup_row <- lineup_row[!is.na(lineup_row)]
    
    pinch_hitter <- bat[(bat$Pos != "") & (bat$Pinch > 0),]
    
    pinch_hitter <- pinch_hitter$PlayerName[!pinch_hitter$PlayerName %in% lineup_row]
    
    final$PH[k] <- length(pinch_hitter)
    final$num_NA[k] <- number_of_NA  
  }
  
  final$PA <- ""
  final$PA <- as.numeric(0)
  
  for(t in 1:nrow(final))
  {
    name <- as.vector(t(final[t,])[1:8])
    name <- name[!is.na(name)]
    
    # If player in name[s] has 3 or more PAs, then add 1 to final$PA[t]
    for(s in 1:length(name))
    {
      all3 <- all2[all2$PlayerName %in% name[s],]
      
      if(nrow(all3) == 0){
        next;
      }
      
      if(all3$GameDate %in% c(NA,"NA",""))
      {
        final$PA[t] <- final$PA[t] + 1
      }
      
      if(!all3$GameDate %in% c(NA,"NA","")){
        
        if((as.numeric(all3$PA[all3$GameDate == max(all3$GameDate)][1])) >=3)
        {
          final$PA[t] <- final$PA[t] + 1
        }
        
        if((as.numeric(all3$PA[all3$GameDate == max(all3$GameDate)][1])) < 3)
        {
          next;
        }
      }
      
    }
  }
  
  ###
  if(length(away_which) > 0){
    
    if(final_schedule$Home[away_which] %in% NL == TRUE){
      
      final$SP <- ""
      
      date_char <- as.Date(as.character(date),"%Y%m%d")
      
      final$SP <- lineup$fullname[(lineup$Team %in% all_pro[iiii]) & (lineup$Date %in% date_char)]
      
      id <- lineup$MLBId[(lineup$Team %in% all_pro[iiii]) & (lineup$Date %in% date_char)]
      
      starter <- dbReadTable(con,"BAT_START")
      
      starter2 <- starter[max(which((starter$MLBId %in% id))),]
      
      bat_master2 <- bat_master[(bat_master$MLBId %in% id),]
      
      if(nrow(bat_master2) > 0){
        bat_master2 <- bat_master2[nrow(bat_master2),]
        PAss <- bat_master2$PA[1]
        
        if(PAss == 3){
          final$PA <- final$PA + 1
        }
        
        if(PAss == 2){
          final$B <- as.numeric(final$B) + 1
        }
        
        if(PAss == 1){
          final$P <- final$P + 2
        }
      }
      
      if(nrow(bat_master2) == 0){
        
        IPs <- starter2$IP[which((starter$GameId %in% "MLE") & (starter$MLBId %in% id))]
        
        if(length(IPs) == 0){
          IPs <- starter2$IP[1]
          
        }
        
        if((IPs >= 6)){
          
          final$PA <- final$PA + 1
          
        }
        
        if((IPs >= 3) & (IPs <= 5.99)){
          final$B <- as.numeric(final$B) + 1
        }
        
        if(IPs <= 2.99){
          final$P <- final$P + 2
        }
      }
      
    }
  }
  
  ###
  
  final <- final[(final$S == max(final$S)),]
  
  final <- final[(final$SB == max(final$SB)),]
  
  final <- unique(final)
  
  final <- final[final$Penalty == max(final$Penalty),]
  
  final$Diff <- as.numeric(final$PH) - as.numeric(final$B) - as.numeric(as.numeric(final$P) * 2)
  
  final <- final[which(final$Diff == max(final$Diff)),]
  
  final_selection <- sample(1:nrow(final),size=1,replace = FALSE)
  
  final <- final[final_selection,]
  
  # If final has 8 > SB and Diff is more than 0, run it.
  
  if(final$SB[1] >=8 & final$Diff[1] < 0)
  {
    counting <- final$Diff[1]
    
    no_PH <- bat[!(bat$PlayerName %in% as.vector(t(final[1,column]))),]
    
    if(counting < 0)
    {
      counting <- counting * -1
      
      for(f in 1:counting){
        
        no_PH <- no_PH[no_PH$PlayerName %in% (bat$PlayerName[which(bat$Pinch == 0)]),]
        
        choosing <- sample(1:nrow(no_PH),size=1,replace=FALSE)
        
        mle_piece <- data.frame(matrix(NA,nrow=1,ncol=43))
        
        colnames(mle_piece) <- colnames(all)
        
        mle_piece[,6:43] <- c(-0.25,0,0,0,NA,0,0,"","",1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,"MLE","MLE","","")
        
        mle_piece$MLBId[1] <- no_PH$MLBId[no_PH$PlayerName %in% as.character(no_PH$PlayerName[choosing])]
        
        mle_piece$PlayerName[1] <- as.character(no_PH$PlayerName[no_PH$PlayerName %in% as.character(no_PH$PlayerName[choosing])])
        
        ifelse(length(unlist(strsplit(mle_piece$PlayerName," "))) == 2,mle_piece$FirstName[1] <- unlist(strsplit(mle_piece$PlayerName," "))[1],paste(unlist(strsplit(mle_piece$PlayerName," "))[1],unlist(strsplit(mle_piece$PlayerName," "))[2],sep=" "))
        
        mle_piece$LastName[1] <- unlist(strsplit(mle_piece$PlayerName," "))[2]
        
        if(!mle_piece$MLBId %in% MLE_keeper_pinch$MLBId)
        {
          MLE_keeper_pinch <- rbind(MLE_keeper_pinch, mle_piece)
        }
        
        no_PH$Pinch[no_PH$MLBId %in% mle_piece$MLBId[1]] <- 1
      }
      
      
    }
    
    MLE_keeper_pinch <- MLE_keeper_pinch[!MLE_keeper_pinch$FirstName %in% c("NA",NA,""),]
  
  }
  
  if(any(final$SB < 8))
  {
    # Which position is unfilled
    empty <- which(as.vector(t(final[1,])[1:8]) %in% c(NA,"")) + 1
    
    eligible <- vector()
    
    for(cc in 2:ncol(pos_sim)){
      col <- as.character(pos_sim[,cc])
      p <- empty %in% unlist(strsplit(as.character(col[!col %in% "NO"]),""))
      
      if(p == TRUE){eligible[cc] <- TRUE}
      
      if(p == FALSE){eligible[cc] <- FALSE}
    }
    
    eligible <- eligible[!eligible %in% NA]
    
    for(bb in 1:length(empty))
    {
      # Which player is slotted in the lineup already?
      used <- as.vector(t(final[1,])[1:8])
      
      used <- used[!is.na(used)]
      
      # Players not slotted in linup
      bat <- bat[!(bat$PlayerName %in% used),]
      
      # Players outside of 25-man roster
      bat2 <- bat2[!(bat2$PlayerName %in% used),]
      
      MLE_eli <- bat[(bat$MLE %in% "YES"),]
      
      can_play <- which(eligible %in% TRUE) + 1
      
      keeper <- vector()
      
      if(nrow(MLE_eli) > 0){
        for(bbb in 1:nrow(MLE_eli)){
          
          out_of <- as.character(pos_sim[,MLE_eli$most_inning_primary[bbb]][!pos_sim[,MLE_eli$most_inning_primary[bbb]] %in% c("NO",NA)])
          all_pos <- MLE_eli$Pos[bbb]
          all_pos <- unlist(strsplit(as.character(all_pos),""))
          
          total <- unique(c(out_of,all_pos))
          
          if(empty %in% total){
            keeper[bbb] <- bbb
          }
          if(!empty %in% total){
            keeper[bbb] <- "NO"
          }
        }
        keeper <- keeper[!keeper %in% "NO"]
        keeper <- as.numeric(keeper)
        MLE_eli <- MLE_eli[keeper,]
        
      }
      
      priority <- which(MLE_eli$most_inning_primary %in% empty)
      
      if(length(priority) > 0){
        choose <- min(priority,na.rm = TRUE)
        
        check <- c("NA","CA","1B","2B","3B","SS","LF","CF","RF")
        
        final[,check[empty]] <- as.character(MLE_eli$PlayerName[choose])
        
        mle_info <- read.csv(paste0("batter/",MLE_eli$MLBId[choose],".csv"))
      }
      
      if((length(priority) == 0) | (nrow(MLE_eli) == 0)){
        check <- c("NA","CA","1B","2B","3B","SS","LF","CF","RF")
        
        num <- sample(x=1:nrow(MLE_eli),size = 1,replace = FALSE)
        final[,check[empty]] <- as.character(MLE_eli$PlayerName[num])
        
        mle_info <- read.csv(paste0("batter/",MLE_eli$MLBId[num],".csv"))
      }
      
      #mle_info$Bases_Taken <- 0
      #mle_info$Outs_on_Base <- 0
      
      mle_info <- mle_info[,c("GameDate","FirstName","LastName","MLBId","PlayerName","LW","Bonus","Bases_Taken","Outs_on_Base",
                              "Field","E","Zone","Block","Frame","PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF",
                              "HBP","BB","K","SB","CS","GIDP","HFC","GB","FB","LD","POPU","BH","IFH","OUTS","GameString",
                              "GameId","uniqueId","used")]
      #mle_info$used <- "NO"
      mle_info2 <- mle_info[max(which(mle_info$used %in% "NO")),]
      
      mle_info2[,c("H","X1B","X2B","X3B","HR","SAC","SF","HBP","BB","SB")] <- floor(mle_info2[,c("H","X1B","X2B","X3B","HR","SAC","SF","HBP","BB","SB")] / 2)
      mle_info2[,c("K","CS","GIDP","OUTS")] <- ceiling(mle_info2[,c("K","CS","GIDP","OUTS")] * 2)
      
      mle_info2$LW <- (mle_info2$X1B * 0.46) + (mle_info2$X2B * 0.8) + (mle_info2$X3B * 1.02) + (mle_info2$HR * 1.4) + (mle_info2$SAC * 0.146) +
        (mle_info2$SF * 0.2) + (mle_info2$HBP * 0.33) + (mle_info2$K * -0.073) + (mle_info2$SB * 0.3) + (mle_info2$CS * -0.6) + 
        (mle_info2$GIDP * -0.46) + (mle_info2$OUTS * -0.25) + (mle_info2$BB * 0.33)
      
      mle_info$used[max(which(mle_info$used %in% "NO"))] <- "YES"
      
      MLE_keeper <- rbind(MLE_keeper,mle_info2)
      
      write.csv(mle_info,paste0("batter/",mle_info2$MLBId[1],".csv"),row.names = FALSE)
      
      #write.csv(batting,"Batting/Batting_Master_Starts2.csv",row.names=FALSE)
      
      final$SB[1] <- as.numeric(final$SB[1]) + 1
      final$num_NA[1] <- as.numeric(final$num_NA[1]) - 1
      
    }
    
    if(final$Diff[1] < 0)
    {
      counting <- final$Diff[1]
      
      no_PH <- bat[!(bat$PlayerName %in% as.vector(t(final[1,column]))),]
      
      while(counting < 0)
      {
        no_PH <- no_PH[no_PH$PlayerName %in% (bat$PlayerName[which(bat$Pinch == 0)]),]
        
        choosing <- sample(1:nrow(no_PH),size=1,replace=FALSE)
        
        mle_piece <- data.frame(matrix(NA,nrow=1,ncol=43))
        
        colnames(mle_piece) <- colnames(all)
        
        mle_piece[,6:43] <- c(-0.25,0,0,0,NA,0,0,"","",1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,"MLE","MLE","","")
        
        mle_piece$MLBId[1] <- no_PH$MLBId[no_PH$PlayerName %in% as.character(no_PH$PlayerName[choosing])]
        
        mle_piece$PlayerName[1] <- as.character(no_PH$PlayerName[no_PH$PlayerName %in% as.character(no_PH$PlayerName[choosing])])
        
        ifelse(length(unlist(strsplit(mle_piece$PlayerName," "))) == 2,mle_piece$FirstName[1] <- unlist(strsplit(mle_piece$PlayerName," "))[1],paste(unlist(strsplit(mle_piece$PlayerName," "))[1],unlist(strsplit(mle_piece$PlayerName," "))[2],sep=" "))
        
        mle_piece$LastName[1] <- unlist(strsplit(mle_piece$PlayerName," "))[2]
        
        if(!mle_piece$MLBId %in% MLE_keeper$MLBId)
        {
          MLE_keeper <- rbind(MLE_keeper_pinch, mle_piece)
        }
        
        no_PH$Pinch[no_PH$MLBId %in% mle_piece$MLBId[1]] <- 1
        
        counting <- counting - 1
        
        position_report <- read.csv("position_player.csv")
        position_report <- dbReadTable(con,"position_player")
        
        position_report$Pinch[position_report$MLBId %in% mle_piece$MLBId[1]] <- position_report$Pinch[position_report$MLBId %in% mle_piece$MLBId[1]] + 1
        
        write.csv(position_report,"position_player.csv",row.names = FALSE)
        dbWriteTable(con,"position_player",position_report,overwrite=TRUE)
      }
      
      
    }
  }
  
  #MLE_keeper <- data.frame(matrix(NA,nrow=1,ncol=43))
  position_report <- read.csv("position_player.csv")
  position_report <- dbReadTable(con,"position_player")
  
  # Run this block if: 1) If team is playing in AL ball park 2) You need to find DH or give MLE to someone at DH slot.
  
  if(any((colnames(final) %in% "DH"))){
    
    if(final[1,"DH"] %in% c("","NA",NA)){
      
      bat <- dbReadTable(con,paste0(all_pro[iiii],date,"_batting_report"))
      bat <- bat[!bat$Pos %in% c("NA",NA),]
      
      bat2 <- bat[bat$X25.man == "YES",]
      
      on_the_list <- t(final[1,])[1:8]
      
      remained_player <- as.character(bat2$PlayerName[!bat2$PlayerName %in% on_the_list])
      
      bat2$B_S <- bat2$Start + bat2$Bench
      
      remained_id <- bat$MLBId[bat$PlayerName %in% remained_player]
      
      bat2 <- bat2[bat2$MLBId %in% remained_id,]
      
      mle_eligible <- which(bat2$MLE == "YES")
     
      if(length(mle_eligible) > 0){
        
        for(dd in 1:length(mle_eligible))
        {
          # Which player is slotted in the lineup already?
          used <- as.vector(t(final[1,])[1:8])
          
          used <- used[!is.na(used)]
          
          # Players not slotted in linup
          bat2 <- bat2[!(bat2$PlayerName %in% used),]
          
          MLE_eli <- bat2[(bat2$MLE %in% "YES"),]
          
          if(nrow(MLE_eli) > 0){
          
            MLE_eli <- MLE_eli[sample(x=nrow(MLE_eli),size = 1,replace = FALSE),]
            mle_info <- read.csv(paste0("batter/",MLE_eli$MLBId[1],".csv"))
            
          }
          
          mle_info$Bases_Taken <- 0
          mle_info$Outs_on_Base <- 0
          
          mle_info <- mle_info[,c("GameDate","FirstName","LastName","MLBId","PlayerName","LW","Bonus","Bases_Taken","Outs_on_Base",
                                  "Field","E","Zone","Block","Frame","PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF",
                                  "HBP","BB","K","SB","CS","GIDP","HFC","GB","FB","LD","POPU","BH","IFH","OUTS","GameString",
                                  "GameId","uniqueId","used")]
          mle_info$used <- "NO"
          mle_info2 <- mle_info[max(which(mle_info$used %in% "NO")),]
          
          mle_info2[,c("H","X1B","X2B","X3B","HR","SAC","SF","HBP","BB","SB")] <- floor(mle_info2[,c("H","X1B","X2B","X3B","HR","SAC","SF","HBP","BB","SB")] / 2)
          mle_info2[,c("K","CS","GIDP","OUTS")] <- ceiling(mle_info2[,c("K","CS","GIDP","OUTS")] * 2)
          
          mle_info2$LW <- (mle_info2$X1B * 0.46) + (mle_info2$X2B * 0.8) + (mle_info2$X3B * 1.02) + (mle_info2$HR * 1.4) + (mle_info2$SAC * 0.146) +
            (mle_info2$SF * 0.2) + (mle_info2$HBP * 0.33) + (mle_info2$K * -0.073) + (mle_info2$SB * 0.3) + (mle_info2$CS * -0.6) + 
            (mle_info2$GIDP * -0.46) + (mle_info2$OUTS * -0.25) + (mle_info2$BB * 0.33)
          
          mle_info$used[max(which(mle_info$used %in% "NO"))] <- "YES"
          
          MLE_keeper <- rbind(MLE_keeper,mle_info2)
          
          write.csv(mle_info,paste0("batter/",mle_info2$MLBId[1],".csv"),row.names = FALSE)
          
          #write.csv(batting,"Batting/Batting_Master_Starts2.csv",row.names=FALSE)
          
          final$SB[1] <- as.numeric(final$SB[1]) + 1
          final$num_NA[1] <- as.numeric(final$num_NA[1]) - 1
          final$DH[1] <- as.character(bat$PlayerName[bat$MLBId %in% mle_info2$MLBId[1]])
        }
      }
      
      if(length(mle_eligible) == 0){
        
        final[1,"DH"] <- remained_player[sample(1:length(remained_player),size = 1,replace=FALSE)]
        final$SB[1] <- final$SB[1] + 1
        ifelse(bat2$Pinch[bat2$PlayerName %in% final[1,"DH"]] > 0, final$PH[1] <- as.numeric(final$PH[1]) - 1, final$PH[1] <- as.numeric(final$PH[1]))
        final$PA[1] <- final$PA[1] + 1
        
        ###
        
        mle_piece <- data.frame(matrix(NA,nrow=1,ncol=43))
        
        colnames(mle_piece) <- colnames(all)
        
        mle_piece[,6:43] <- c(-0.75,0,0,0,NA,0,0,"","",3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,3,"MLE","MLE","","")
        
        mle_piece$GameDate <- as.Date(mle_piece$GameDate)
        
        mle_piece$GameDate[1] <- date_char
        
        mle_piece$MLBId[1] <- bat2$MLBId[bat2$PlayerName %in% as.character(bat2$PlayerName[bat2$PlayerName %in% final[1,"DH"]])]
        
        mle_piece$PlayerName[1] <- as.character(bat2$PlayerName[bat2$PlayerName %in% as.character(bat2$PlayerName[which(bat2$PlayerName %in% final[1,"DH"])])])
        
        ifelse(length(unlist(strsplit(mle_piece$PlayerName," "))) == 2,mle_piece$FirstName[1] <- unlist(strsplit(mle_piece$PlayerName[1]," "))[1],mle_piece$FirstName[1]  <- paste(unlist(strsplit(mle_piece$PlayerName[1]," "))[1],sep=" "))
        
        mle_piece$LastName[1] <- unlist(strsplit(mle_piece$PlayerName," "))[2]
        
        if(!mle_piece$MLBId %in% MLE_keeper$MLBId)
        {
          MLE_keeper <- rbind(MLE_keeper, mle_piece)
        }
        
        ###
      }
    }
    
  }
  
  colnames(MLE_keeper) <- colnames(all)
  
  MLE_keeper <- MLE_keeper[!MLE_keeper$PlayerName %in% c("","NA",NA),]
  
  #MLE_keeper <- MLE_keeper[!MLE_keeper$PlayerName %in% as.vector(t(final[1,]))[1:8],]
  
  MLE_keeper$uniqueId <- paste(MLE_keeper$MLBId,MLE_keeper$GameString,sep=" ")
  
  ###
  
  MLE_keeper_pinch <- MLE_keeper_pinch[!MLE_keeper_pinch$PlayerName %in% c("","NA",NA),]
  
  MLE_keeper_pinch <- MLE_keeper_pinch[!MLE_keeper_pinch$PlayerName %in% as.vector(t(final[1,]))[1:8],]
  
  MLE_keeper_pinch$uniqueId <- paste(MLE_keeper_pinch$MLBId,MLE_keeper_pinch$GameString,sep=" ")
  
  
  #if(nrow(MLE_keeper) > 0){
   # 
    #for(re in 1:nrow(MLE_keeper))
    #{
     # if(nrow(MLE_keeper) > 0)
      #{
       # if(!MLE_keeper$uniqueId[re] %in% batting$uniqueId)
        #{
         # batting <- rbind(batting, MLE_keeper[re,])
          #
          #bat$Start[bat$MLBId %in% MLE_keeper$MLBId[re]] <- as.numeric(bat$Start[bat$MLBId %in% MLE_keeper$MLBId[re]]) + 1
        #  
         # position_report$Start[position_report$MLBId %in% MLE_keeper$MLBId[re]] <- as.numeric(position_report$Start[position_report$MLBId %in% MLE_keeper$MLBId[re]]) + 1
        #}
        
      #  if(MLE_keeper$uniqueId[re] %in% batting$uniqueId)
       # {
        #  next;
        #}
      #}
      
      #if(nrow(MLE_keeper) == 0)
      #{
      #  next;
      #}
  #  }  
  #}
  
  pos_tab <- data.frame(num=c(2,3,4,5,6,7,8,9),pos=c("CA","1B","2B","3B","SS","LF","CF","RF"))
  
  for(g in 1:nrow(lining)){
    
    if(lining$POS[g] == "P"){
      next;
    }
    
    if(lining$POS[g] == "DH"){
      lining$fullname[g] <- final[1,colnames(final) %in% lining$POS[g]]
      lining$MLBId[g] <- position_report$MLBId[(position_report$PlayerName %in% lining$fullname[g]) & (position_report$Team_RFB %in% lining$Team[g])]
    }
    
    if(lining$POS[g] != "DH"){
      lining$fullname[g] <- final[1,colnames(final) %in% lining$POS[g]]
      lining$MLBId[g] <- position_report$MLBId[(position_report$PlayerName %in% lining$fullname[g]) & (position_report$Team_RFB %in% lining$Team[g])]
      lining$POS2[g] <- pos_tab$num[pos_tab$pos %in% lining$POS[g]]
    }
    
  }
  
  #write.csv(bat,paste("report/bat/",date,"/",all_pro[iiii],date,"_batting_report.csv",sep=""),row.names=FALSE)
  
  lineup[which((lineup$Role %in% "#1") & (lineup$Team %in% all_pro[iiii])),] <- lining[(lining$Role %in% "#1") & (lining$Team %in% all_pro[iiii]),1:8]
  lineup[which((lineup$Role %in% "#2") & (lineup$Team %in% all_pro[iiii])),] <- lining[(lining$Role %in% "#2") & (lining$Team %in% all_pro[iiii]),1:8]
  lineup[which((lineup$Role %in% "#3") & (lineup$Team %in% all_pro[iiii])),] <- lining[(lining$Role %in% "#3") & (lining$Team %in% all_pro[iiii]),1:8]
  lineup[which((lineup$Role %in% "#4") & (lineup$Team %in% all_pro[iiii])),] <- lining[(lining$Role %in% "#4") & (lining$Team %in% all_pro[iiii]),1:8]
  lineup[which((lineup$Role %in% "#5") & (lineup$Team %in% all_pro[iiii])),] <- lining[(lining$Role %in% "#5") & (lining$Team %in% all_pro[iiii]),1:8]
  lineup[which((lineup$Role %in% "#6") & (lineup$Team %in% all_pro[iiii])),] <- lining[(lining$Role %in% "#6") & (lining$Team %in% all_pro[iiii]),1:8]
  lineup[which((lineup$Role %in% "#7") & (lineup$Team %in% all_pro[iiii])),] <- lining[(lining$Role %in% "#7") & (lining$Team %in% all_pro[iiii]),1:8]
  lineup[which((lineup$Role %in% "#8") & (lineup$Team %in% all_pro[iiii])),] <- lining[(lining$Role %in% "#8") & (lining$Team %in% all_pro[iiii]),1:8]
  lineup[which((lineup$Role %in% "#9") & (lineup$Team %in% all_pro[iiii])),] <- lining[(lining$Role %in% "#9") & (lining$Team %in% all_pro[iiii]),1:8]
  
  if("DH" %in% colnames(final))
  {
    lineup$fullname[(lineup$Team == all_pro[iiii]) & (lineup$POS == "DH") & (lineup$Role %in% c("#1","#2","#3","#4","#5","#6","#7","#8","#9"))] <- as.character(final[1,"DH"])
  }
  
  lineup$fullname[(lineup$Role == "Bench") & (lineup$Team == all_pro[iiii])] <- ""
  lineup$POS[(lineup$Role == "Bench") & (lineup$Team == all_pro[iiii])] <- ""
  
  lineup <- lineup[!((lineup$fullname %in% "") & (lineup$Team %in% all_pro[iiii]) & (lineup$Role %in% "Bench")),]
  
  bat4 <- read.csv(paste("report/bat/",date,"/",all_pro[iiii],date,"_batting_report.csv",sep=""))
  
  namings <- bat4[!(bat4$Pos == "") & !(bat4$PlayerName %in% lineup$fullname[(lineup$Role %in% c("#1","#2","#3","#4","#5","#6","#7","#8","#9")) & (lineup$Team == all_pro[iiii])]),]
  
  namings <- namings[!namings$PlayerName %in% c(NA,"NA"),]
  
  namings <- namings$PlayerName[!namings$X25.man == "NO"]

  benching <- data.frame(matrix(NA,nrow=length(namings),ncol=ncol(lineup)))
  
  colnames(benching) <- colnames(lineup)
  benching$Role <- "Bench"
  benching$Team <- all_pro[iiii]
  benching$`Fielding Petition` <- "NO"
  
  for(dos in 1:length(namings))
  {
    benching$fullname[dos] <- as.character(namings[dos])
    benching$MLBId[dos] <- as.character(bat4$MLBId[(bat4$PlayerName %in% namings[dos])])
    benching$POS2[dos] <- as.character(bat4$most_inning_primary[(bat4$PlayerName %in% namings[dos])])
    
    if(length(pos_tab$pos[pos_tab$num %in% benching$POS2[dos]]) == 0){
      next;
    }
    
    benching$POS[dos] <- as.character(pos_tab$pos[pos_tab$num %in% benching$POS2[dos]])
    
  }
  
  benching <- benching[!benching$POS %in% NA,]
  
  lineup_part1 <- lineup[1:max(which(lineup$Team %in% all_pro[iiii])),]
  lineup_part2 <- lineup[(max(which(lineup$Team %in% all_pro[iiii]))+1):nrow(lineup),]
  
  lineup <- rbind(lineup_part1,benching,lineup_part2)
  
  if(nrow(MLE_keeper) > 0){
    batting <- rbind(batting,MLE_keeper)
  }
  
  if(nrow(MLE_keeper_pinch) > 0){
    pinching_scan <- rbind(pinching_scan,MLE_keeper_pinch)
  }

  write.csv(lineup,paste("lineup",date,".csv",sep=""),row.names=FALSE)
  dbWriteTable(con,"lineups",lineup,overwrite=TRUE)

write.csv(batting,"Batting/Batting_Master_Starts2.csv",row.names=FALSE)
dbWriteTable(con,"BAT_START",batting,overwrite=TRUE)
write.csv(pinching_scan,"Batting/Batting_Master_Pinch2.csv",row.names=FALSE)
dbWriteTable(con,"BAT_BENCH",batting,overwrite=TRUE)

}
dbDisconnect(con)

#source("postgame/4)batting report.R")
