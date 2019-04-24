boxscores <- list.files("newbox/")

boxname <- c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases.Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
             "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB","LD","POPU",
             "BH","IFH","OUTS","MLBId","PlayerName")

master_box2 <- data.frame(matrix(NA,nrow=1,ncol=length(boxname)))

colnames(master_box2) <- boxname

for(i in 1:length(boxscores))
{
  box <- read.csv(paste(getwd(),"/newbox/",boxscores[i],sep=""))
  
  colnames(box) <- boxname
  
  box <- box[,c("POS","GameDate","FirstName","LastName","LW","Bonus","Bases.Taken","Outs_on_Base","Field","E","Zone","Block","Frame",
                "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB","LD","POPU",
                "BH","IFH","OUTS","MLBId","PlayerName")]

  box <- box[!((box$POS=="") & (box$GameDate == "") & (box$FirstName == "") & (box$LastName == "")),]
  
  Pitcher <- which((box$POS == "P"))
  
  box <- box[!((box$GameDate %in% box$GameDate[Pitcher]) & (box$FirstName %in% box$FirstName[Pitcher]) & (box$LastName %in% box$LastName[Pitcher]) & (box$POS %in% "")),]
  
  
  master_box2 <- rbind(master_box2,box)
}

master_box2 <- master_box2[!master_box2$LastName %in% c("Total","Overall Offense","Overall Defense","LastName","Overall Pitching"),]

master_box2$PlayerName <- paste(master_box2$FirstName,master_box2$LastName,sep= " ")

relief <- which(nchar(master_box2$PlayerName) > 0 & master_box2$MLBId %in% "" & master_box2$PlayerName != "NA NA")

for(i in 3:length(relief))
{
  num <-relief[i]
  
  possibility <- which((master_box2$PlayerName %in% master_box2$PlayerName[relief[i]]))
  
  possibility <- possibility[possibility != num]
  
  master_box2$MLBId[num] <- master_box2$MLBId[possibility[1]]
}

write.csv(master_box2,"late_season_master_stat.csv",row.names=FALSE)

