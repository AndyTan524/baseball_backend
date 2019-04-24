library(dplyr)

zone <- read.csv("zone_block_frame.csv")
field <- read.csv("FIELD_STAT.csv")

field$Pos[field$Pos == 1] <- "P"
field$Pos[field$Pos == 2] <- "CA"
field$Pos[field$Pos == 3] <- "1B"
field$Pos[field$Pos == 4] <- "2B"
field$Pos[field$Pos == 5] <- "3B"
field$Pos[field$Pos == 6] <- "SS"
field$Pos[field$Pos == 7] <- "LF"
field$Pos[field$Pos == 8] <- "CF"
field$Pos[field$Pos == 9] <- "RF"

field$uniqueId <- paste(field$MLBId,field$Team,field$Pos,sep=" ")
zone$uniqueId <- paste(zone$MLBId,zone$Team,zone$Pos,sep=" ")

master <- merge(field,zone,by=c("uniqueId"))

master_one <- master %>% group_by(uniqueId,Pos) %>% summarize(LW = sum(LW,na.rm=TRUE),G = sum(G,na.rm=TRUE),GS = sum(GS,na.rm=TRUE), INN= sum(INN,na.rm=TRUE),PO = sum(PO,na.rm=TRUE), A = sum(A,na.rm=TRUE), E = sum(E,na.rm=TRUE), DP = sum(DP,na.rm=TRUE), TP = sum(TP,na.rm=TRUE), PB = sum(PB,na.rm=TRUE),
                                                                        SB = sum(SB,na.rm=TRUE), CS = sum(CS,na.rm=TRUE), PKOF = sum(PKOF,na.rm=TRUE), Outs = sum(Outs,na.rm=TRUE), Chances = sum(Chances,na.rm=TRUE), Cint = sum(Cint,na.rm=TRUE), Zone = sum(Zone,na.rm=TRUE), Block = sum(Block,na.rm=TRUE), Frame = sum(Frame,na.rm=TRUE))

master_one$LW <- round(master_one$LW,digits=2)

crunch <- read.csv("crunches.csv")

master_one$MLBId <- as.integer(substr(master_one$uniqueId,1,6))

master_one$Name <- ""

master_one$Name <- as.character(master_one$Name)

crunch$mlb_id <- as.integer(crunch$mlb_id)

for(i in 1:nrow(master_one))
{
 master_one$Name[i] <- as.character(crunch$bref_name[which(crunch$mlb_id %in% master_one$MLBId[i])])
}

master_one <- master_one[,c("uniqueId","MLBId","Name","Pos","LW","G","GS","PO","A","E","DP","TP","PB","SB","CS","PKOF","Outs","Chances","Cint","Zone","Block","Frame")]

master_two <- master_one %>% group_by(MLBId) %>% summarize(LW = sum(LW,na.rm=TRUE),G = sum(G,na.rm=TRUE),GS = sum(GS,na.rm=TRUE), PO = sum(PO,na.rm=TRUE), A = sum(A,na.rm=TRUE), E = sum(E,na.rm=TRUE), DP = sum(DP,na.rm=TRUE), TP = sum(TP,na.rm=TRUE), PB = sum(PB,na.rm=TRUE),
                                                             SB = sum(SB,na.rm=TRUE), CS = sum(CS,na.rm=TRUE), PKOF = sum(PKOF,na.rm=TRUE), Outs = sum(Outs,na.rm=TRUE), Chances = sum(Chances,na.rm=TRUE), Cint = sum(Cint,na.rm=TRUE), Zone = sum(Zone,na.rm=TRUE), Block = sum(Block,na.rm=TRUE), Frame = sum(Frame,na.rm=TRUE))

master_two$Name <- ""

master_two$Name <- as.character(master_two$Name)


for(i in 1:nrow(master_two))
{
  master_two$Name[i] <- as.character(crunch$bref_name[which(crunch$mlb_id %in% master_two$MLBId[i])])
}

master_two$Primary_position <- ""

master_two <- master_two[,c("MLBId","Name","Primary_position","LW","G","GS","PO","A","E","DP","TP","PB","SB","CS","PKOF","Outs","Chances","Cint","Zone","Block","Frame")]

for(i in 1:nrow(master_two))
{
  master_two$Primary_position[i] <- as.character(crunch$mlb_pos[which(crunch$mlb_id %in% master_two$MLBId[i])])
}

write.csv(master_one,"field_by_positions.csv",row.names = FALSE)
write.csv(master_two,"field_by_player.csv",row.names = FALSE)
