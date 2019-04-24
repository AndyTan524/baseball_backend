#2:31pm - 

# Load all fielding master files

fielding_master_available <- read.csv("Fielding/Fielding_Master_Available.csv")
fielding_master_used <- read.csv("Fielding/Fielding_Master_Used.csv")

# Characterize

char_field <- c("Used","LastName","FirstName","GameDate","GameId","GameString","MLBId","PlayerName")

for(i in 1:length(char_field))
{
  fielding_master_available[,char_field[i]] <- as.character(fielding_master_available[,char_field[i]])
}

# Position column creator for fielding_master_used

pos_num <- c(1,2,3,4,5,6,7,8,9)
pos_let <- c("P","CA","1B","2B","3B","SS","LF","CF","RF")

fielding_master_used$Position <- ""

for(k in 1:length(pos_num))
{
  fielding_master_used$Position[which(fielding_master_used$Pos %in% pos_num[k])] <- pos_let[k]
}

# Create unique ID column to identify players

fielding_master_available$uniqueId <-  paste(fielding_master_available$FirstName, fielding_master_available$LastName, fielding_master_available$GameDate,sep=" ")
fielding_master_used$uniqueId <-  paste(fielding_master_used$FirstName, fielding_master_used$LastName, fielding_master_used$GameDate,sep=" ")

# Loading daily fielding stats

fielding <- read.csv(paste("archive/Fielding_",date,".csv",sep=""))

unavailable_columns <- colnames(fielding_master_available)[!(colnames(fielding_master_available) %in% colnames(fielding))]

for(j in 1:length(unavailable_columns))
{
  fielding[,unavailable_columns[j]] <- ""
}

fielding <- select(fielding, Used, LastName, FirstName, GameDate, GameId, GameString, MLBId, PlayerName, PlayerId, Team,
       TeamNbr, LW, Pos, Position,PrevDayGamesPlayed, G, GS, INN, PO, A, E, DP, TP, PB, FPctP, SB, CS, PKOF, FPctC, Outs,
       Chances, Zone, FPct, Pivots2B, Cint, uniqueId)

# Characterizing 'fielding'

for(n in 1:length(char_field))
{
  fielding[,char_field[n]] <- as.character(fielding[,char_field[n]])
}

# Delete players with no fielding stats

fielding <- fielding[!(fielding$GameString == ""),]

# Fill 'Position' columns in 'fielding' data frame

for(l in 1:length(pos_num))
{
  fielding$Position[which(fielding$Pos %in% pos_num[l])] <- pos_let[l]
}


# Write a code to calculate Fielding
#Fielding
#Pitchers
#=(T2*0.13455)+(U5*0.13754)-(V5*0.508)

fielding$LW[which(fielding$Pos %in% pos_num[1])] <- round(((fielding$PO[which(fielding$Pos %in% pos_num[1])] * 0.13455) + (fielding$A[which(fielding$Pos %in% pos_num[1])] * 0.13754) - (fielding$E[which(fielding$Pos %in% pos_num[1])] * 0.508)),digits=2)

#Catchers
#=(U17*0.13754)+(AC17*0.234)+(AB17*0.234)-(V17*0.508)-(Y17*0.269)-(AJ17*0.392)-(AA17*0.088)

fielding$LW[which(fielding$Pos %in% pos_num[2])] <- round(((fielding$A[which(fielding$Pos %in% pos_num[2])] * 0.13754) + (fielding$PKOF[which(fielding$Pos %in% pos_num[2])] * 0.234) + (fielding$CS[which(fielding$Pos %in% pos_num[2])] * 0.234) - (fielding$E[which(fielding$Pos %in% pos_num[2])] * 0.508) - (fielding$PB[which(fielding$Pos %in% pos_num[2])] * 0.269) - (fielding$Cint[which(fielding$Pos %in% pos_num[2])] * 0.392) - (fielding$SB[which(fielding$Pos %in% pos_num[2])] * 0.088)),digits=2)

#1B
#=(T41*0.00897)+(U41*0.13754)-(V41*0.508)

fielding$LW[which(fielding$Pos %in% pos_num[3])] <- round(((fielding$PO[which(fielding$Pos %in% pos_num[3])] * 0.00897) + (fielding$A[which(fielding$Pos %in% pos_num[3])] * 0.13754) - (fielding$E[which(fielding$Pos %in% pos_num[3])] * 0.508)),digits=2)


#2B
#=(T45*0.06122)+(U45*0.085843)-(V45*0.508)

fielding$LW[which(fielding$Pos %in% pos_num[4])] <- round(((fielding$PO[which(fielding$Pos %in% pos_num[4])] * 0.06122) + (fielding$A[which(fielding$Pos %in% pos_num[4])] * 0.085843) - (fielding$E[which(fielding$Pos %in% pos_num[4])] * 0.508)),digits=2)


#3B
#=(T43*0.14352)+(U43*0.115115)-(V43*0.508)

fielding$LW[which(fielding$Pos %in% pos_num[5])] <- round(((fielding$PO[which(fielding$Pos %in% pos_num[5])] * 0.14352) + (fielding$A[which(fielding$Pos %in% pos_num[5])] * 0.115115) - (fielding$E[which(fielding$Pos %in% pos_num[5])] * 0.508)),digits=2)

#SS
#=(T63*0.125161)+(U63*0.085843)-(V63*0.508)

fielding$LW[which(fielding$Pos %in% pos_num[6])] <- round(((fielding$PO[which(fielding$Pos %in% pos_num[6])] * 0.125161) + (fielding$A[which(fielding$Pos %in% pos_num[6])] * 0.085843) - (fielding$E[which(fielding$Pos %in% pos_num[6])] * 0.508)),digits=2)


#OF
#=(T64*0.130065)+(U64*0.148005)-(V64*0.508)


fielding$LW[which(fielding$Pos %in% pos_num[c(7,8,9)])] <- round(((fielding$PO[which(fielding$Pos %in% pos_num[c(7,8,9)])] * 0.130065) + (fielding$A[which(fielding$Pos %in% pos_num[c(7,8,9)])] * 0.148005) - (fielding$E[which(fielding$Pos %in% pos_num[c(7,8,9)])] * 0.508)),digits=2)


# Remove lastnames from PlayerName, so that only thing remaining is just first names. Then remains of PlayerName
# goes to FirstName column

for(m in 1:nrow(fielding))
{
  fielding$FirstName[m] <- as.character(sub(paste(fielding$LastName[m]," ",sep=""), "", fielding$PlayerName[m]))
}

# Rearrange 'lastname firstname' format to 'firstname lastname' in PlayerName

fielding$PlayerName <- paste(fielding$FirstName," ",fielding$LastName,sep="")


# Create uniqueID on fielding. Then match uniqueID from 'fielding' with that of box_score

fielding$uniqueId <- paste(fielding$FirstName, fielding$LastName, fielding$GameDate,sep=" ")


# Add stats from today to the fielding_master_available

fielding_master_available <- rbind(fielding_master_available, fielding)

# Check for used stats in fielding master files (availables), then 'X' the used ones.
# Subset the 'X'ed rows and transfer to 'used' batting master files. Delete the 'X'ed
# rows from the batting master files (availables)

fielding_master_available$Used[fielding_master_available$uniqueId %in% box_score$uniqueId] <- "X"

fielding_used <- fielding_master_available[fielding_master_available$Used %in% "X",]

fielding_master_used <- rbind(fielding_master_used, fielding_used)

fielding_master_available <- fielding_master_available[!(fielding_master_available$Used %in% "X"),]