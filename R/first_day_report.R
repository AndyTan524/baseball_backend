pit <- list.files(path="report/pit/20160710/")

lineup <- read.csv("lineup20170406.csv")

team <- as.character(unique(lineup$Team))
  
master_crunch <- read.csv("master_crunch.csv")

master_crunch$MLE_Eligibility <- ""

for(i in 7:length(team)){
  
  lineup2 <- lineup[lineup$Team %in% team[i],]
  
  name <- c("Name","POS","MLBId","Days_in_a_row","uniqueId","Cannot Use","25-man","FirstName","LastName","IP","MLE")
  
  master <- data.frame(matrix(NA,nrow=length(which(lineup2$POS %in% c("SP","RP"))),ncol=length(name)))
  
  colnames(master) <- name
  
  master$Name <- as.character(unique(lineup2$fullname[which(lineup2$POS %in% c("SP","RP"))]))
  
  master$Days_in_a_row <- 0
  master$`Cannot Use` <- FALSE
  master$`25-man` <- "YES"
  master$IP <- 0
  
  for(j in 1:nrow(master)){
    master$FirstName[j] <- unlist(strsplit(as.character(master$Name[j])," "))[1]
    master$LastName[j] <- unlist(strsplit(as.character(master$Name[j])," "))[2]
    master$MLBId[j] <- lineup2$MLBId[which(lineup2$fullname %in% as.character(master$Name[j]))]
    master$uniqueId[j] <- paste0(master$MLBId[j]," ",master$Name[j])
    master$POS[j] <- as.character(lineup2$POS[lineup2$MLBId %in% master$MLBId[j]])
  }
  
  master$MLE <- ""
    
  write.csv(master,paste0("report/RP/20170406/",team[i],20170406,"_RP_report.csv"),row.names=FALSE)
}
