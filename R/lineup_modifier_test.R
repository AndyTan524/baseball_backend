# Load Visitor's Manager Report

teamaway = final_schedule$Visit[x+1]
filenametest = paste("report/bat/",date,"/",final_schedule$Visit[x+1],date,"_batting_report.csv",sep="")
#report_visit <- read.csv(paste("report/bat/",date,"/",final_schedule$Away[x],date,"_batting_report.csv",sep=""))
report_visit <- read.csv(paste("report/bat/",date,"/",final_schedule$Visit[x+1],date,"_batting_report.csv",sep=""))


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