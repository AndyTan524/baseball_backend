library(dplyr)

all_files <- list.files(path = paste("BIS/"))

all_fielding <- all_files[grep("^Fielding_",all_files)]

all_YTD <- all_files[grep("^YTDFielding_",all_files)]

sample_field <- read.csv("BIS/Fielding_20160404.csv")

sample_YTD <- read.csv("BIS/YTDFielding_20160404.csv")

master_field <- data.frame(matrix(NA,nrow=1,ncol=22))

colnames(master_field) <- c("LastName","FirstName","MLBId","PlayerName","Team","Pos","G","GS","INN","PO","A","E",
                            "DP","TP","PB","SB","CS","PKOF","Outs","Chances","Cint","date")

for(i in 1:length(all_fielding))
{
  fielding <- read.csv(paste("BIS/",all_fielding[i],sep=""))
  fielding <- fielding[,c("LastName","FirstName","MLBId","PlayerName","Team","Pos","G","GS","INN","PO","A","E",
                          "DP","TP","PB","SB","CS","PKOF","Outs","Chances","Cint")]

  fielding$date <- as.character(as.Date(substr(all_fielding[i],10,17),"%Y%m%d"))
  
  master_field <- rbind(master_field, fielding)
  
}

master_field <- master_field[!master_field$Team %in% NA,]

for(i in 1:nrow(master_field))
{
  master_field$FirstName[i] <- as.character(sub(paste(master_field$LastName[i]," ",sep=""), "", master_field$PlayerName[i]))
}

master_field <- master_field[!master_field$G == 0,]

crunches <- read.csv('crunch_master.csv',encoding="latin1")

master_field$MLB_name <- ""

for(l in 1:nrow(master_field))
{
  master_field$MLB_name[l] <- as.character(crunches$yahoo_name[crunches$mlb_id %in% master_field$MLBId[l]])
}

for(k in 1:nrow(FIELD_STAT))
{
  
  if(FIELD_STAT$MLB_name[k] == "")
  {
    next;
  }
  
  if(FIELD_STAT$MLB_name[k] != "")
  {
    names <- unlist(strsplit(x = FIELD_STAT$MLB_name[k],split = " "))
    
    if(length(names) == 2)
    {
      FIELD_STAT$FirstName[k] <- names[1]
      FIELD_STAT$LastName[k] <- names[2]
      
    }
    
    if(length(names) > 2)
    {
      FIELD_STAT$FirstName[k] <- names[1]
      FIELD_STAT$LastName[k] <- paste0(names[2:length(names)],collapse = ' ')
    }
  }
  
}

# Rearrange 'lastname firstname' format to 'firstname lastname' in PlayerName

master_field$PlayerName <- paste(master_field$FirstName," ",master_field$LastName,sep="")

master_field$LW <- ""

master_field <- master_field[,c("LastName","FirstName","MLBId","PlayerName","Team","Pos","LW","G","GS","INN","PO","A","E",
                                "DP","TP","PB","SB","CS","PKOF","Outs","Chances","Cint")]

for(i in 7:ncol(master_field))
{
  master_field[,i] <- as.double(master_field[,i])
}

master_field$LW[which(master_field$Pos == 1)] <- (master_field$PO[which(master_field$Pos == 1)] * 0.13455) + (master_field$A[which(master_field$Pos == 1)] * 0.13754) - (master_field$E[which(master_field$Pos == 1)] * 0.508)
master_field$LW[which(master_field$Pos == 2)] <- (master_field$A[which(master_field$Pos == 2)] * 0.13754) + (master_field$PKOF[which(master_field$Pos == 2)] * 0.234) + (master_field$CS[which(master_field$Pos == 2)] * 0.234) - (master_field$E[which(master_field$Pos == 2)] * 0.508) - (master_field$PB[which(master_field$Pos == 2)] * 0.269) - (master_field$Cint[which(master_field$Pos == 2)] *0.392) - (master_field$SB[which(master_field$Pos == 2)]*0.088)
master_field$LW[which(master_field$Pos == 3)] <- (master_field$PO[which(master_field$Pos == 3)] * 0.00897) + (master_field$A[which(master_field$Pos == 3)] * 0.13754) - (master_field$E[which(master_field$Pos == 3)] * 0.508)
master_field$LW[which(master_field$Pos == 4)] <- (master_field$PO[which(master_field$Pos == 4)] * 0.06122) + (master_field$A[which(master_field$Pos == 4)] * 0.085843) - (master_field$E[which(master_field$Pos == 4)] * 0.508)
master_field$LW[which(master_field$Pos == 5)] <- (master_field$PO[which(master_field$Pos == 5)] * 0.14352) + (master_field$A[which(master_field$Pos == 5)] * 0.115115) - (master_field$E[which(master_field$Pos == 5)] * 0.508)
master_field$LW[which(master_field$Pos == 6)] <- (master_field$PO[which(master_field$Pos == 6)] * 0.125161) + (master_field$A[which(master_field$Pos == 6)] * 0.085843) - (master_field$E[which(master_field$Pos == 6)] * 0.508)
master_field$LW[which(master_field$Pos %in% c(7,8,9))] <- (master_field$PO[which(master_field$Pos %in% c(7,8,9))] * 0.130065) + (master_field$A[which(master_field$Pos %in% c(7,8,9))] * 0.148005) - (master_field$E[which(master_field$Pos %in% c(7,8,9))] * 0.508)

master_field$MLBId <- as.character(master_field$MLBId)
master_field$Pos <- as.character(master_field$Pos)

master_field$OUTING <- ((master_field$INN %/% 1)*3) + ((master_field$INN %% 1)*10)

FIELD_STAT <- master_field %>% group_by(MLBId,Pos) %>% summarise(LW = sum(LW,na.rm=TRUE), G = sum(G,na.rm=TRUE), GS = sum(GS,na.rm=TRUE), INN = sum(INN,na.rm=TRUE), PO = sum(PO,na.rm=TRUE), A = sum(A,na.rm=TRUE), E = sum(E,na.rm=TRUE), DP = sum(DP,na.rm=TRUE), TP = sum(TP,na.rm=TRUE), PB = sum(PB,na.rm=TRUE), SB = sum(SB,na.rm=TRUE), CS = sum(CS,na.rm=TRUE), PKOF = sum(PKOF,na.rm=TRUE), Outs = sum(OUTING,na.rm=TRUE), Chances = sum(Chances,na.rm=TRUE), Cint = sum(Cint,na.rm=TRUE))

#FIELD_STAT <- FIELD_STAT[order(FIELD_STAT$PlayerName,decreasing = FALSE),]

FIELD_STAT <- FIELD_STAT[order(FIELD_STAT$LW,decreasing = TRUE),]

FIELD_STAT$INN <- (FIELD_STAT$Outs %/% 3) + ((FIELD_STAT$Outs %% 3) / 10)

FIELD_STAT <- FIELD_STAT[,c("FirstName","LastName","MLB_name","MLBId","Pos","LW","G","GS","INN","PO","A","E",
                            "DP","TP","PB","SB","CS","PKOF","Outs","Chances","Cint")]

FIELD_STAT$Pos[which(FIELD_STAT$Pos == 1)] <- "P"
FIELD_STAT$Pos[which(FIELD_STAT$Pos == 2)] <- "C"
FIELD_STAT$Pos[which(FIELD_STAT$Pos == 3)] <- "1B"
FIELD_STAT$Pos[which(FIELD_STAT$Pos == 4)] <- "2B"
FIELD_STAT$Pos[which(FIELD_STAT$Pos == 5)] <- "3B"
FIELD_STAT$Pos[which(FIELD_STAT$Pos == 6)] <- "SS"
FIELD_STAT$Pos[which(FIELD_STAT$Pos == 7)] <- "LF"
FIELD_STAT$Pos[which(FIELD_STAT$Pos == 8)] <- "CF"
FIELD_STAT$Pos[which(FIELD_STAT$Pos == 9)] <- "RF"

write.csv(FIELD_STAT,"FIELD_STAT.csv",row.names = FALSE)
  