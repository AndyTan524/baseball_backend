trans16 <- read.csv("16transaction.csv")

date <- unique(trans16$Date)

date <- as.Date(date,format="%m/%d/%y")

date <- date[!is.na(date)]

trans16$Date <- as.Date(trans16$Date,format="%m/%d/%y")


for(i in 1:length(date))
{
  if(i < length(date))
  {
    min <- which((trans16$Date == as.Date(date[i],format="%Y-%m-%d")))
    max <- which((trans16$Date == as.Date(date[i+1],format="%Y-%m-%d")))
    
    bridge <- min:(max-1)
    remove <- vector()
    
    for(k in 1:length(bridge))
    {
      if(trans16$Date[bridge[k]] %in% date[i])
      {
        remove[k] <- bridge[k] 
      }
      
      if(!trans16$Date[bridge[k]] %in% date[i])
      {
        next;
      }
    }
    
    bridge <- bridge[!bridge %in% remove]
    
    trans16$Date[bridge] <- date[i]
    
  }
  
  if(i == length(date))
  {
    min <-which((trans16$Date == as.Date(date[i],format="%Y-%m-%d")))
    max <- nrow(trans16)
    
    bridge2 <- min:max
    remove2 <- vector()
    
    for(k in 1:length(bridge2))
    {
      if(trans16$Date[bridge2[k]] %in% date[i])
      {
        remove2[k] <- bridge2[k] 
      }
      
      if(!trans16$Date[bridge2[k]] %in% date[i])
      {
        next;
      }
    }
    
    bridge2 <- bridge2[!bridge2 %in% remove2]
    
    trans16$Date[bridge2] <- date[i]
    
  }
}

write.csv(trans16,"16transaction.csv",row.names = FALSE)
