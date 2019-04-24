
date <- 20160709
date2 <- as.Date("2016-07-09")

pitchdb <- read.csv("pitcher_rest_days_with_SP_RP.csv")
allpitchdb <- read.csv("pitcher_usage_archive.csv")
lineup <- read.csv(paste("lineup",date,".csv",sep=""))

starter <- read.csv("Pitching/Pitching_Master_Starts2.csv")

lineup$Date <- as.Date(lineup$Date)

final_schedule <- read.csv("final_schedule.csv")

final_schedule$Date <- as.Date(final_schedule$Date)

final_schedule <- final_schedule[final_schedule$Date == date2,]

final_schedule$Away <- as.character(final_schedule$Away)
final_schedule$Home <- as.character(final_schedule$Home)

team <- c(final_schedule$Away,final_schedule$Home)

lineup$start_avail <- ""

start_MLE_box <- data.frame(matrix(NA,nrow=1,ncol=ncol(starter)))

colnames(start_MLE_box) <- colnames(starter)

start_MLE_box$GameDate <- as.Date(start_MLE_box$GameDate)

for(i in 1:length(team))
{
  final_schedule2 <- read.csv("final_schedule.csv")
  
  final_schedule2 <- final_schedule2[final_schedule2$Away %in% team[i] | final_schedule2$Home %in% team[i],]
  
  final_schedule2$Date <- as.Date(final_schedule2$Date)
  
  RP <- read.csv(paste(getwd(),"/report/RP/",date,"/",team[i],date,"_RP_report.csv",sep=""))

  RP <- RP[RP$POS %in% "SP",]
  
  RP <- RP[RP$MLBId %in% lineup$MLBId[(lineup$MLBId %in% lineup$MLBId[(lineup$Team %in% team[i]) & (lineup$Role %in% c("SP1","SP2","SP3","SP4","SP5"))]) & (lineup$Role %in% c("SP1","SP2","SP3","SP4","SP5"))],]
  
  name <- RP$MLBId
  
  pitchdb2 <- pitchdb[((pitchdb$Team == team[i]) & (pitchdb$MLBId %in% name)) | (pitchdb$MLBId %in% lineup$MLBId[(lineup$Team == team[i]) & (lineup$Role %in% c("SP1","SP2","SP3","SP4","SP5","SP6"))]),]
  
    pitchdb3 <- pitchdb2[order((pitchdb2$Rest_days),(pitchdb2$Start),decreasing=TRUE),]
    
    dates <- final_schedule2$Date[c(seq(which(final_schedule2$Date == date2),which(final_schedule2$Date == date2)+(nrow(pitchdb3)-1),by=1))]
    
    bullpen <- lineup$MLBId[(lineup$Team == team[i]) & (lineup$POS %in% "RP")]
    
    today_starter <- pitchdb3$MLBId[which((pitchdb3$Start >= 1) & (pitchdb3$Rest_days > 3) & !(pitchdb3$MLBId %in% bullpen))]
    
    Pitching_Report <- read.csv(paste(getwd(),"/report/pit/",date,"/",team[i],date,"_pitching_report.csv",sep=""))
    
    if(length(today_starter) > 0)
    {
      today_starter <- today_starter[1]
      
      lineup$Date[lineup$MLBId %in% today_starter] <- dates[1]
      
      dates <- dates[2:length(dates)]
      
      pitchdb3 <- pitchdb3[!pitchdb3$MLBId %in% today_starter,]
      
      pitchdb3 <- pitchdb3[!pitchdb3$MLBId %in% bullpen,]
      
      dates <- dates[1:nrow(pitchdb3)]
      
      for(j in 2:(nrow(pitchdb3)+1))
      {
        lineup$Date[which((lineup$MLBId %in% pitchdb3$MLBId[j-1]) & (lineup$POS %in% c("SP","RP")))] <- dates[j-1]
      }
    }
    
    if(length(today_starter) == 0)
    {
      for(j in 1:nrow(pitchdb3))
      {
        lineup$Date[which((lineup$MLBId %in% pitchdb3$MLBId[j]) & (lineup$POS %in% c("SP","RP")))] <- dates[j]
      }
    }
    
    lineup$start_avail[(lineup$Team %in% team[i]) & (lineup$Date %in% date2)] <- Pitching_Report$Start[Pitching_Report$MLBId %in% lineup$MLBId[(lineup$Team %in% team[i]) & (lineup$Date %in% date2)]]
  
    if((lineup$start_avail[(lineup$Team %in% team[i]) & (lineup$Date %in% date2)]) > 0)
    {
      next;
    }
    
    if((lineup$start_avail[(lineup$Team %in% team[i]) & (lineup$Date %in% date2)]) == 0)
    {
      
      start_MLE_seg <- data.frame(matrix(NA,nrow=1,ncol=ncol(starter)))
      
      colnames(start_MLE_seg) <- colnames(starter)
      
      start_MLE_seg$GameDate <- as.Date(start_MLE_seg$GameDate)
      
      start_MLE_seg$FirstName[1] <- strsplit(as.character(lineup$fullname[lineup$MLBId %in% (lineup$MLBId[(lineup$Team %in% team[i]) & (lineup$Date %in% date2)])]),split = " ")[[1]][1]
      
      start_MLE_seg$LastName[1] <- strsplit(as.character(lineup$fullname[lineup$MLBId %in% (lineup$MLBId[(lineup$Team %in% team[i]) & (lineup$Date %in% date2)])]),split = " ")[[1]][2]
     
      start_MLE_seg$IP[1] <- 5.0
    
      start_MLE_seg$X1B[1] <-3
      
      start_MLE_seg$X2B[1] <- 2
      
      start_MLE_seg$HR[1] <- 1
      
      start_MLE_seg$BB[1] <- 2
      
      start_MLE_seg$K[1] <- 3
      
      start_MLE_seg$GB[1] <- 12
      
      start_MLE_seg$OUT[1] <- 15

      start_MLE_seg$MLBId[1] <-lineup$MLBId[(lineup$Team %in% team[i]) & (lineup$Date %in% date2)]
      
      start_MLE_seg$PlayerName[1] <- as.character(lineup$fullname[lineup$MLBId %in% (lineup$MLBId[(lineup$Team %in% team[i]) & (lineup$Date %in% date2)])])
        
      start_MLE_seg$GameString[1] <- "MLE"
      
      start_MLE_seg$GameId[1] <- "MLE"
      
      start_MLE_seg$uniqueId[1] <- paste(lineup$MLBId[(lineup$Team %in% team[i]) & (lineup$Date %in% date2)],"MLE",sep=" ")
      
      start_MLE_seg$H[1] <- 6
      
      start_MLE_seg$BFP[1] <- 23
      
      start_MLE_seg$LW[1] <- -0.99
      
      start_MLE_seg[1,c(18,20:23,26:29,31:36)] <- 0
      
      start_MLE_box <- rbind(start_MLE_box,start_MLE_seg)
    }
}

start_MLE_box <- start_MLE_box[!start_MLE_box$First %in% c(NA,"NA"),]
write.csv(start_MLE_box,"Pitching/MLE_starter.csv",row.names = FALSE)

lineup$start_avail <- NULL

write.csv(lineup,paste("lineup",date,".csv",sep=""),row.names = FALSE)
