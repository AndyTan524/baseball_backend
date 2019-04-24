library(gsheet)
library(googlesheets)
library(xlsx)

date <- 20160707

formatted_date <- "2016-07-07"

formatted_date <- as.Date(formatted_date,format="%Y-%m-%d")

# Subset the schedule to have only games that on the day of set date

final_schedule <- read.csv("final_schedule_test.csv")

for(a in 1:ncol(final_schedule))
{
  if(a == 1){final_schedule[,a] <- as.Date(final_schedule[,a],format="%Y-%m-%d")}
  else{final_schedule[,a] <- as.character(final_schedule[,a])}
}

final_schedule <- final_schedule[final_schedule$Date %in% formatted_date,]

# Exclude rain out games

final_schedule <- final_schedule[!final_schedule$Rain %in% "Yes",]


# Lineup from the Google Spreadsheet

lineup <- read.csv(paste("lineup",date,".csv",sep=""), header = FALSE)

col_line <- c("Role","fullname","Date","POS","Team","MLBId","Fielding Petition")

colnames(lineup) <- col_line

lineup$Date <- as.Date(lineup$Date,format="%Y-%m-%d")

# Team abbreviations

pitcher_use_rest <- read.csv("pitcher_rest_days_with_SP_RP.csv")

probable <- data.frame(matrix(NA,nrow=nrow(final_schedule),ncol=3))

colnames(probable) <- c("Game","Away Starter","Home Starter")

# HOME

for(i in 1:nrow(final_schedule))
{
  # Copy and paste AWAY and HOME team in "ith" row
  
  probable$Game[i] <- paste(final_schedule$Away[i],"@",final_schedule$Home[i],sep="")

  if(length(which((lineup$Date[(lineup$POS == "SP") & (lineup$Team == final_schedule$Home[i])] %in% formatted_date))) > 1)
  {
    
    starter <- lineup$fullname[which((lineup$Team %in% final_schedule$Home[i]) & (lineup$POS %in% "SP"))]
    
    pitcher_use_rest2 <- pitcher_use_rest[which((pitcher_use_rest$Start > 0) & (pitcher_use_rest$Team == final_schedule$Home[i]) & (pitcher_use_rest$Rest_days > 3)),]
    
    pitcher_use_rest2 <- pitcher_use_rest2[pitcher_use_rest2$FullName %in% starter,]
    
    probable$`Home Starter`[i] <- as.character(pitcher_use_rest2$FullName[which((pitcher_use_rest2$Rest_days == min(pitcher_use_rest2$Rest_days)) & (pitcher_use_rest2$Rest_days > 3))])
    
  }
  
  if(!length(which((lineup$Date[(lineup$POS == "SP") & (lineup$Team == final_schedule$Home[i])] %in% formatted_date))) > 1)
  {
    if(TRUE %in% ((pitcher_use_rest$FullName[(pitcher_use_rest$Team == final_schedule$Home[i]) & (pitcher_use_rest$Start > 0) & (pitcher_use_rest$Rest_days > 3)]) %in% (lineup$fullname[(lineup$Team == final_schedule$Home[i]) & (lineup$POS == "SP")])))  
    {
      starter <- lineup$fullname[which((lineup$Team %in% final_schedule$Home[i]) & (lineup$POS %in% "SP"))]
      
      pitcher_use_rest2 <- pitcher_use_rest[which((pitcher_use_rest$Start > 0) & (pitcher_use_rest$Team == final_schedule$Home[i]) & (pitcher_use_rest$Rest_days > 3)),]
      
      pitcher_use_rest2 <- pitcher_use_rest2[pitcher_use_rest2$FullName %in% starter,]
      
      probable$`Home Starter`[i] <- as.character(pitcher_use_rest2$FullName[which(max(pitcher_use_rest2$Rest_days) == pitcher_use_rest2$Rest_days)])
    }
  
  }
  
  if((pitcher_use_rest$Rest_days[(pitcher_use_rest$FullName) %in% (lineup$fullname[(lineup$Team %in% final_schedule$Home[i]) & (lineup$Date %in% formatted_date)])] > 3) & (pitcher_use_rest$Start[(pitcher_use_rest$FullName) %in% (lineup$fullname[(lineup$Team %in% final_schedule$Home[i]) & (lineup$Date %in% formatted_date)])] >= 1))
  {
    probable$`Home Starter`[i] <- as.character(lineup$fullname[(lineup$Team %in% final_schedule$Home[i]) & (lineup$Date %in% formatted_date)])
  }
  
  
  
}

#Away

for(j in 1:nrow(final_schedule))
{
  # Copy and paste AWAY and HOME team in "ith" row
  
  probable$Game[j] <- paste(final_schedule$Away[j],"@",final_schedule$Home[j],sep="")
  
  if(length(which((lineup$Date[(lineup$POS == "SP") & (lineup$Team == final_schedule$Away[j])] %in% formatted_date))) > 1)
  {
    
    starter <- lineup$fullname[which((lineup$Team %in% final_schedule$Away[j]) & (lineup$POS %in% "SP"))]
    
    pitcher_use_rest2 <- pitcher_use_rest[which((pitcher_use_rest$Start > 0) & (pitcher_use_rest$Team == final_schedule$Away[j]) & (pitcher_use_rest$Rest_days > 3)),]
    
    pitcher_use_rest2 <- pitcher_use_rest2[pitcher_use_rest2$FullName %in% starter,]
    
    probable$`Away Starter`[j] <- as.character(pitcher_use_rest2$FullName[which((pitcher_use_rest2$Rest_days == min(pitcher_use_rest2$Rest_days)) & (pitcher_use_rest2$Rest_days > 3))])
    
  }
  
  if(!length(which((lineup$Date[(lineup$POS == "SP") & (lineup$Team == final_schedule$Away[j])] %in% formatted_date))) > 1)
  {
    if(TRUE %in% ((pitcher_use_rest$FullName[(pitcher_use_rest$Team == final_schedule$Away[j]) & (pitcher_use_rest$Start > 0) & (pitcher_use_rest$Rest_days > 3)]) %in% (lineup$fullname[(lineup$Team == final_schedule$Away[j]) & (lineup$POS == "SP")])))  
    {
      starter <- lineup$fullname[which((lineup$Team %in% final_schedule$Away[j]) & (lineup$POS %in% "SP"))]
      
      pitcher_use_rest2 <- pitcher_use_rest[which((pitcher_use_rest$Start > 0) & (pitcher_use_rest$Team == final_schedule$Away[j]) & (pitcher_use_rest$Rest_days > 3)),]
      
      pitcher_use_rest2 <- pitcher_use_rest2[pitcher_use_rest2$FullName %in% starter,]
      
      probable$`Away Starter`[j] <- as.character(pitcher_use_rest2$FullName[which(max(pitcher_use_rest2$Rest_days) == pitcher_use_rest2$Rest_days)])
    }
    
  }
  
  if((pitcher_use_rest$Rest_days[(pitcher_use_rest$FullName) %in% (lineup$fullname[(lineup$Team %in% final_schedule$Away[j]) & (lineup$Date %in% formatted_date)])] > 3) & (pitcher_use_rest$Start[(pitcher_use_rest$FullName) %in% (lineup$fullname[(lineup$Team %in% final_schedule$Away[j]) & (lineup$Date %in% formatted_date)])] >= 1))
  {
    probable$`Away Starter`[j] <- as.character(lineup$fullname[(lineup$Team %in% final_schedule$Away[j]) & (lineup$Date %in% formatted_date)])
  }
  
  
  
}

crunch <- read.csv("crunchbaseball.csv")

#Home MLe
for(k in 1:nrow(probable))
{
  
  if(probable$`Home Starter`[k] %in% c("",NA))
  {
    pitch_home_report <- read.csv(paste(getwd(),"/report/pit/",date,"/",final_schedule$Home[k],date,"_pitching_report.csv",sep=""))
    
    pitch_home_report$MLe <- ""
    
    play_pos <- read.csv("playerid_with_pos.csv")
    
    play_pos2 <- play_pos[play_pos$Team_RFB == final_schedule$Home[k],]
    
    play_pos2 <- play_pos2[play_pos2$Pos == 1,]
    
    for(kk in 1:nrow(pitch_home_report))
    {
      if(length(crunch$MLE_Eligibility[which(crunch$mlb_id %in% pitch_home_report$MLBId[kk])]) > 0)
      {
        pitch_home_report$MLe[kk] <- as.character(crunch$MLE_Eligibility[which(crunch$mlb_id %in% pitch_home_report$MLBId[kk])])
        
      }
      
      if(length(crunch$MLE_Eligibility[which(crunch$mlb_id %in% pitch_home_report$MLBId[kk])]) == 0)
      {
       next; 
      }
    }
    
    pitch_home_report <- pitch_home_report[pitch_home_report$MLe == "YES",]
    
    pitch_home_report <- pitch_home_report[pitch_home_report$MLBId %in% play_pos2$MLBId,]
    
    pitch_home_report$rest_days <- ""
    
    availability_home_report <- read.csv(paste(getwd(),"/report/RP/",date,"/",final_schedule$Home[k],date,"_RP_report.csv",sep=""))
    
    unavailable <- availability_home_report$MLBId[availability_home_report$Cannot.Use == TRUE]
    
    pitch_home_report <- pitch_home_report[!pitch_home_report$MLBId %in% unavailable,]
    
    for(kkk in 1:nrow(pitch_home_report))
    {
      pitch_home_report$rest_days[kkk] <- pitcher_use_rest$Rest_days[pitcher_use_rest$MLBId %in% pitch_home_report$MLBId[kkk]]
    }
  }
  
  if(!probable$`Home Starter`[k] %in% c("",NA))
  {
   next; 
  }
  
  write.csv(pitch_home_report,paste("mlebox/mle_",final_schedule$Home[k],".csv",sep=""),row.names = FALSE)
  
}

for(l in 1:nrow(probable))
{
  
  if(probable$`Away Starter`[l] %in% c("",NA))
  {
    pitch_visit_report <- read.csv(paste(getwd(),"/report/pit/",date,"/",final_schedule$Away[l],date,"_pitching_report.csv",sep=""))
    
    pitch_visit_report$MLe <- ""
    
    play_pos <- read.csv("playerid_with_pos.csv")
    
    play_pos2 <- play_pos[play_pos$Team_RFB == final_schedule$Away[l],]
    
    play_pos2 <- play_pos2[play_pos2$Pos == 1,]
    
    for(ll in 1:nrow(pitch_visit_report))
    {
      if(length(crunch$MLE_Eligibility[which(crunch$mlb_id %in% pitch_visit_report$MLBId[ll])]) > 0)
      {
        pitch_visit_report$MLe[ll] <- as.character(crunch$MLE_Eligibility[which(crunch$mlb_id %in% pitch_visit_report$MLBId[ll])])
        
      }
      
      if(length(crunch$MLE_Eligibility[which(crunch$mlb_id %in% pitch_visit_report$MLBId[ll])]) == 0)
      {
        next; 
      }
    }
    
    pitch_visit_report <- pitch_visit_report[pitch_visit_report$MLe == "YES",]
    
    pitch_visit_report <- pitch_visit_report[pitch_visit_report$MLBId %in% play_pos2$MLBId,]
    
    pitch_visit_report$rest_days <- ""
    
    availability_visit_report <- read.csv(paste(getwd(),"/report/RP/",date,"/",final_schedule$Away[l],date,"_RP_report.csv",sep=""))
    
    unavailable <- availability_visit_report$MLBId[availability_visit_report$Cannot.Use == TRUE]
    
    pitch_visit_report <- pitch_visit_report[!pitch_visit_report$MLBId %in% unavailable,]
    
    for(lll in 1:nrow(pitch_visit_report))
    {
      pitch_visit_report$rest_days[lll] <- pitcher_use_rest$Rest_days[pitcher_use_rest$MLBId %in% pitch_visit_report$MLBId[lll]]
    }
  }
  
  if(!probable$`Away Starter`[l] %in% c("",NA))
  {
    next; 
  }
  
  write.csv(pitch_visit_report,paste("mlebox/mle_",final_schedule$Away[l],".csv",sep=""),row.names = FALSE)
  
}

write.xlsx(probable,"probable20160707.xlsx",sheetName = "Sheet")

gs_upload("probable20160707.xlsx")

