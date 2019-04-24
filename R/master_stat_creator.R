library(plyr)
library(dplyr)

col_bat <- c("GameId", "GameString", "GameDate", "LastName", "FirstName", "MLBId", "PlayerName", "PlayerId", "Team", "TeamNbr", "PrevDayGamesPlayed", "G", "AB", "R", "H", "X1B", "X2B", "X3B", "HR", "RBI", "SAC", "SF", "HBP", "BB", "IBB", "K", "SB", "CS", "GIDP", "HFC", "GB", "FB", "GB.FB", "LD", "POPU", "BH", "BHAtt", "AVG", "SLG", "PA", "IFH")
col_pit <- c("GameId", "GameString", "GameDate", "LastName", "FirstName", "MLBId", "PlayerName", "PlayerId", "Team", "TeamNbr", "PrevDayGamesPlayed", "W", "L", "Sv", "BS", "HLD", "G", "GS", "CG", "GF", "QS", "SHO", "IP", "BFP", "H","X1B", "X2B", "X3B", "HR", "R", "ER", "SH", "SF", "HBP", "BB", "IBB", "BB_noIBB", "K", "WP", "BLK", "NoH", "PerfectG", "IR","IRS", "GB", "FB", "GB.FB", "LD", "POPU", "SB", "CS", "PKO", "StIP", "RelIP", "ERA", "SVO")

master_bat <- data.frame(matrix(NA,nrow=1,ncol=length(col_bat)))
master_pit <- data.frame(matrix(NA,nrow=1,ncol=length(col_pit)))

colnames(master_bat) <- col_bat
colnames(master_pit) <- col_pit

file <- list.files("BIS/")
batting_file <- file[grepl("Batting",file)]
pitch_file <- file[grepl("Pitching",file)]

for(i in 1:length(batting_file))
{
  batting <- read.csv(paste("BIS/",batting_file[i],sep=""))
  batting <- select(batting, GameId, GameString, GameDate, LastName, FirstName, MLBId, PlayerName, PlayerId,
                    Team, TeamNbr, PrevDayGamesPlayed, G, AB, R, H, X1B, X2B, X3B, HR, RBI, SAC, SF, HBP, BB,
                    IBB, K, SB, CS, GIDP, HFC, GB, FB, GB.FB, LD, POPU, BH, BHAtt, AVG, SLG, PA, IFH)
  
  pitching <- read.csv(paste("BIS/",pitch_file[i],sep=""))
  pitching <- select(pitching, GameId, GameString, GameDate, LastName, FirstName, MLBId, PlayerName, PlayerId,
                     Team, TeamNbr, PrevDayGamesPlayed, W, L, Sv, BS, HLD, G, GS, CG, GF, QS, SHO, IP, BFP, H,
                     X1B, X2B, X3B, HR, R, ER, SH, SF, HBP, BB, IBB, BB_noIBB, K, WP, BLK, NoH, PerfectG, IR,
                     IRS, GB, FB, GB.FB, LD, POPU, SB, CS, PKO, StIP, RelIP, ERA, SVO)
  
  master_bat <- rbind(master_bat, batting)
  master_pit <- rbind(master_pit, pitching)
  
}

master_bat <- master_bat[!(master_bat$MLBId %in% NA),]
master_pit <- master_pit[!(master_pit$MLBId %in% NA),]

master_bat <- master_bat[!(master_bat$GameId %in% NA),]
master_pit <- master_pit[!(master_pit$GameId %in% NA),]

write.csv(master_bat, "master_bat_stats.csv", row.names = FALSE)
write.csv(master_pit, "master_pit_stats.csv", row.names = FALSE)