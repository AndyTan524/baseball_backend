library(plyr)
library(dplyr)

col_base <- c("GameId", "GameString", "GameDate", "LastName", "FirstName", "MLBId", "PlayerName", "PlayerId", "Team", "TeamNbr", "PrevDayGamesPlayed", "G",
              "StolenBases2B","StolenBases3B","StolenBasesHP","CaughtStealing2B","CaughtStealing3B","CaughtStealingHP","Pickoffs","OutsOnBase","BatterAdvOnHitOut",
              "AdvOnFlyBallOut","DoubledOffLD","AdvOnWpPbOut","BasesTaken","AdvancedOnFlyBall","FirstToThird1B","FirstToHome1B","FirstToHome2B","SecondToHome1B")

master_base <- data.frame(matrix(NA,nrow=1,ncol=length(col_base)))

colnames(master_base) <- col_base

file <- list.files("BIS/")
base_file <- file[grepl("Baserunning",file)]


for(i in 1:length(base_file))
{
  base <- read.csv(paste("BIS/",base_file[i],sep=""))
  base <- select(base, GameId, GameString, GameDate, LastName, FirstName, MLBId, PlayerName, PlayerId,
                    Team, TeamNbr, PrevDayGamesPlayed, G, StolenBases2B,StolenBases3B,StolenBasesHP,
                 CaughtStealing2B,CaughtStealing3B,CaughtStealingHP,Pickoffs,OutsOnBase,BatterAdvOnHitOut,
                 AdvOnFlyBallOut,DoubledOffLD,AdvOnWpPbOut,BasesTaken,AdvancedOnFlyBall,FirstToThird1B,FirstToHome1B,FirstToHome2B,SecondToHome1B)
  
  master_base <- rbind(master_base, base)
  
  
}

master_base <- master_base[!(master_base$MLBId %in% NA),]

master_base <- master_base[!(master_base$GameId %in% NA),]


write.csv(master_base, "master_base_stats.csv", row.names = FALSE)
