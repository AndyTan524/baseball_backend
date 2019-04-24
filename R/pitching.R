#Adjusting pitching box score thing

pitching <- function(){

box_score_pit <- box_score_pit[box_score_pit$POS %in% c("SP","RP"),]

box_pit_col <- c("GameDate", "GameString","FirstName","LastName","POS","LW","DEC","X1","X2","X3","X4","X5","X6","X7","IP","BFP","H","X1B","X2B","X3B","HR","ER","SH","SF","HBP","BB","K","WP","BLK",
  "IR","IRS","GB","FB","LD","POPU","SB","CS","PKO","OUT")

colnames(box_score_pit) <- box_pit_col

for(i in 1:4)
{
  box_score_pit[,i] <- as.character(box_score_pit[,i])
}

box_score_pit$uniqueId <- paste(box_score_pit$GameDate, box_score_pit$LastName, box_score_pit$IP, box_score_pit$H,sep=" ")

# Load all pitching master files

pitching_master_RP <- read.csv("Pitching/Pitching_Master_RP.csv")
pitching_master_starts <- read.csv("Pitching/Pitching_Master_Starts.csv")
pitching_master_used <- read.csv("Pitching/Pitching_Master_Used.csv")

pitching_master_RP$GameDate <- as.Date(pitching_master_RP$GameDate)
pitching_master_starts$GameDate <- as.Date(pitching_master_starts$GameDate)
pitching_master_used$GameDate <- as.Date(pitching_master_used$GameDate)


pitching_master_RP$uniqueId <- ""
pitching_master_starts$uniqueId <- ""
pitching_master_used$uniqueId <- ""

# Make a list of column names for pitching master files

pit_col <- c("GameDate","FirstName","LastName","LW","DEC","X1","X2","X3","X4","X5","X6","X7","IP","BFP","H","X1B","X2B","X3B","HR","ER","SH","SF","HBP","BB","K","WP","BLK","IR","IRS","GB","FB","LD","POPU","SB","CS","PKO","OUT","MLBId","PlayerName","GameString","GameId","used","uniqueId")

colnames(pitching_master_RP) <- pit_col
colnames(pitching_master_starts) <- pit_col
colnames(pitching_master_used) <- pit_col

# Create unique ID column to identify players

pitching_master_RP$uniqueId <- paste(pitching_master_RP$GameDate,pitching_master_RP$LastName, pitching_master_RP$IP,pitching_master_RP$H,sep=" ")
pitching_master_starts$uniqueId <- paste(pitching_master_starts$GameDate,pitching_master_starts$LastName, pitching_master_starts$IP,pitching_master_starts$H,sep=" ")
pitching_master_used$uniqueId <- paste(pitching_master_used$GameDate,pitching_master_used$LastName, pitching_master_used$IP,pitching_master_used$H,sep=" ")

# Loading daily pitching stats

pitching <- read.csv(paste("BIS/Pitching_",date,".csv",sep=""))
pitching$GameDate <- strptime(as.character(pitching$GameDate), "%m/%d/%Y")
pitching$GameDate <- format(pitching$GameDate, "%Y-%m-%d")


# Characterize

col_char_pit <- c("GameId","GameString","LastName","FirstName","MLBId","PlayerName","PlayerId","Team")

for(i in 1:length(col_char_pit))
{
  pitching[,col_char_pit[i]] <- as.character(pitching[,col_char_pit[i]])
}

# Delete players with no pitching stats

pitching <- pitching[!(pitching$GameString %in% NA),]

# Modify pitching stats columns to make it identical to all pitching batting files. Add OUTS uniqueId columns

pitching$OUT <- pitching$BFP - pitching$H - pitching$HBP - pitching$BB

pitching$uniqueId <- ""

pitching$LW <- ""

pitching$DEC <- ""

pitching$X1 <- ""

pitching$X2 <- ""

pitching$X3 <- ""

pitching$X4 <- ""

pitching$X5 <- ""

pitching$X6 <- ""

pitching$X7 <- ""

pitching$used <- ""

pitching <- select(pitching, GameDate, FirstName, LastName, LW, DEC, X1, X2, X3, X4, X5, X6, X7, IP, BFP, H, X1B, X2B, X3B, HR ,ER, SH, SF, HBP, BB, K, WP, BLK, IR, IRS,
                   GB, FB, LD, POPU, SB, CS, PKO, OUT, MLBId, PlayerName, GameString, GameId, used, uniqueId, GS)

# Write a code to calculate LW
#=(Q2*-0.46)+(R2*-0.8)+(S2*-1.02)+(T2*-1.4)+(X2*-0.33)+(Y2*-0.33)+(Z2*0.073)+
#(AA2*-0.395)+(AB2*-0.15)+(AJ2*0.3)+(AK2*0.6)+(AL2*0.25)

pitching$LW <- (pitching$X1B *-0.46) + (pitching$X2B * -0.8) + (pitching$X3B * -1.02) + (pitching$HR * -1.4) + (pitching$HBP * -0.33) + (pitching$BB * -0.33) + 
  (pitching$K * 0.073) + (pitching$WP * -0.395) + (pitching$BLK * -0.15) + (pitching$CS * 0.3) + (pitching$PKO * 0.6) + (pitching$OUT * 0.25)

# Remove lastnames from PlayerName, so that only thing remaining is just first names. Then remains of PlayerName
# goes to FirstName column

for(i in 1:nrow(pitching))
{
  pitching$FirstName[i] <- as.character(sub(paste(pitching$LastName[i]," ",sep=""), "", pitching$PlayerName[i]))
}

# Rearrange 'lastname firstname' format to 'firstname lastname' in PlayerName

pitching$PlayerName <- paste(pitching$FirstName," ",pitching$LastName,sep="")


# Create uniqueID on pitching. Then match uniqueID from 'pitching' with that of box_score

pitching$uniqueId <- paste(pitching$GameDate, pitching$LastName, pitching$IP, pitching$H,sep=" ")

# Get rid of players with no pitching stats

pitching <- pitching[!(pitching$GameDate %in% c(NA,"")),]


# Sort today's relievers and starters accordingly, based on GS

start_avail <- pitching[pitching$GS %in% 1,]
relief_avail <- pitching[pitching$GS %in% 0,]

start_avail$GS <- NULL
relief_avail$GS <- NULL

pitching_master_starts <- rbind(pitching_master_starts, start_avail)
pitching_master_RP <- rbind(pitching_master_RP, relief_avail)

# Check from daily 'pitching' file on whether any stats are used.
# If so, write something to transfer to used master files.

used_start <- pitching_master_starts[pitching_master_starts$uniqueId %in% box_score_pit$uniqueId,]
used_RP <- pitching_master_RP[pitching_master_RP$uniqueId %in% box_score_pit$uniqueId,]

pitching_master_used <- rbind(pitching_master_used, used_start, used_RP)

# Check for used stats in pitching mater files (availables), then 'X' the used ones.
# Subset the 'X'ed rows and transfer to 'used' batting master files. Delete the 'X'ed
# rows from the batting master files (availables)

pitching_master_starts$used[pitching_master_starts$uniqueId %in% box_score_pit$uniqueId] <- "X"
pitching_master_RP$used[pitching_master_RP$uniqueId %in% box_score_pit$uniqueId] <- "X"

pitching_master_starts <- pitching_master_starts[!(pitching_master_starts$used %in% "X"),]
pitching_master_RP <- pitching_master_RP[!(pitching_master_RP$used %in% "X"),]

}
