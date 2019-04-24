schedule <- read.csv("schedule2.csv")

for(i in 1:ncol(schedule)){schedule[,i] <- as.character(schedule[,i])}

schedule$Date <- sub("Apr", "04", schedule$Date)
schedule$Date <- sub("May", "05", schedule$Date)
schedule$Date <- sub("Jun", "06", schedule$Date)
schedule$Date <- sub("Jul", "07", schedule$Date)
schedule$Date <- sub("Aug", "08", schedule$Date)
schedule$Date <- sub("Sep", "09", schedule$Date)
schedule$Date <- sub("Oct", "10", schedule$Date)

schedule$Date <- paste(schedule$Date,"-2016",sep="")

schedule$Date <- ifelse(nchar(schedule$Date) == 9, paste("0",schedule$Date,sep=""), paste("", schedule$Date,sep=""))

schedule$Date <-strftime(strptime(schedule$Date,"%d-%m-%Y"),"%Y-%m-%d")

schedule_col <- colnames(schedule)

schedule_col <- sub("LAA","LAA", schedule_col)
schedule_col <- sub("LAD","LAN", schedule_col)
schedule_col <- sub("CWS","CHA", schedule_col)
schedule_col <- sub("CHC","CHN", schedule_col)
schedule_col <- sub("NYM","NYN", schedule_col)
schedule_col <- sub("NYY","NYA", schedule_col)
schedule_col <- sub("WSH","WAS", schedule_col)

colnames(schedule) <- schedule_col

for(j in 1:ncol(schedule))
{
  schedule[,j] <- sub("StL","STL",schedule[,j])
  schedule[,j] <- sub("LAD","LAN", schedule[,j])
  schedule[,j] <- sub("CWS","CHA", schedule[,j])
  schedule[,j] <- sub("CHC","CHN", schedule[,j])
  schedule[,j] <- sub("NYM","NYN", schedule[,j])
  schedule[,j] <- sub("NYY","NYA", schedule[,j])
  schedule[,j] <- sub("WSH","WAS", schedule[,j])
}

schedule2 <- schedule

scheduling <- data.frame(matrix("",nrow=1,ncol=3))

colnames(scheduling) <- c("Date","Away","Home")

for(l in 1:nrow(schedule2))
{
  print(paste(l," of 183",sep=""))
  day_schedule <- data.frame(matrix("",nrow=(ncol(schedule2) + 1),ncol=3))
  colnames(day_schedule) <- c("Date","Away","Home")
  day_schedule$Date <- as.character(day_schedule$Date)
  day_schedule$Away <- as.character(day_schedule$Away)
  day_schedule$Home <- as.character(day_schedule$Home)
  
  for(k in 2:ncol(schedule2))
  {
    
    if(grepl("@",schedule2[l,k]) == TRUE)
    {
      day_schedule$Home[k] <- as.character(sub("@","",schedule2[l,k]))
      day_schedule$Away[k] <- as.character(colnames(schedule2)[k])
      day_schedule$Date[k] <- as.character(schedule2$Date[l])
    }
    
    if(!(grepl("@",schedule2[l,k]) == TRUE)){
      day_schedule$Home[k] <- as.character(colnames(schedule2)[k])
      day_schedule$Away[k] <- as.character(schedule2[l,k])
      day_schedule$Date[k] <- as.character(schedule2$Date[l])
      
    }
    
    
    
  }
  
  day_schedule <- day_schedule[which(!(day_schedule$Date == "")),]
  
  day_schedule <- unique(day_schedule)
  
  scheduling <- rbind(scheduling, day_schedule)
  
}

scheduling <- scheduling[!(scheduling$Away %in% ""),]

write.csv(scheduling,"final_schedule2.csv", row.names = FALSE)