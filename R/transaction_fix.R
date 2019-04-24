crunch <- read.csv("crunchoct.csv")

transaction <- read.csv("transactions.csv")

transaction$First <- ""
transaction$Last <- ""

for(i in 1:nrow(transaction))
{
  string <- unlist(strsplit(as.character(transaction$Player[i])," "))
  
  for(j in 1:length(string))
  {
    if(grepl(",",string[j]) == TRUE)
    {
      position <- grep(",",string)
      
      string[j] <- gsub(",","",string[j])
      
      for(k in 1:position)
      {
        transaction$Last[i] <- paste(transaction$Last[i],string[k],sep=" ")
      }
    }
    
    if(grepl(",",string[j]) == FALSE)
    {
      
    }
  }
  
  transaction$Last <- gsub(" ","",transaction$Last)

  string2 <- string[!grepl(",",string)]
  string2 <- string2[!string2 %in% transaction$Last[i]]
  

    if(length(string2) > 1)
    {
      for(m in 1:(length(string2)))
      {
        transaction$First[i] <- paste(transaction$First[i],string2[m],sep=" ")
      }
      
    }
    
    if(length(string2) == 1)
    {
      transaction$First[i] <- unlist(strsplit(as.character(transaction$Player[i])," "))[length(unlist(strsplit(as.character(transaction$Player[i])," ")))]
      transaction$First[i] <- gsub(",","",transaction$First[i])
    }
  
  
  transaction$full_name[i] <- paste(transaction$First[i],transaction$Last[i],sep=" ")
  
}

transaction$MLB.ID <- as.character(transaction$MLB.ID)

for(n in 1:nrow(transaction))
{
  if(transaction$MLB.ID[n] %in% c("","n/a"))
  {
    if(transaction$full_name[n] %in% crunch$mlb_name)
    {
      transaction$MLB.ID[n] <- as.character(crunch$mlb_id[which(crunch$mlb_name %in% transaction$full_name[n])])
    }
    
    if(!transaction$full_name[n] %in% crunch$mlb_name)
    {
      
    }
  }
  
  if(!(transaction$MLB.ID[n] %in% c("","n/a")))
  {
    next;
  }
}

unique(transaction$Player[which(transaction$MLB.ID %in% c("","n/a"))])

write.csv(transaction, "transaction_update.csv",row.names = FALSE)