master_field_stat <- read.csv("master_field_game_score_stat.csv")
master_bat_stat <- read.csv("master_bat_game_score_stat.csv")

master_bat_stat_home <- master_bat_stat[master_bat_stat$MLBId %in% box_home$MLBId,]
master_field_stat_home <- master_field_stat[master_field_stat$MLBId %in% box_home$MLBId,]

master_spd_home <- merge(master_bat_stat_home,master_field_stat_home,by = c("MLBId"))

master_spd_home$spd <- ""

master_spd_home$spd <- as.numeric(master_spd_home$spd)

f1_v <- (((((master_spd_home$SB) + 3)) / ((master_spd_home$SB) + (master_spd_home$CS) + 7)) - 0.4) * 20

f2_v <- (1/0.07) * sqrt((((master_spd_home$SB) + (master_spd_home$CS)) / ((master_spd_home$X1B) + (master_spd_home$BB) + (master_spd_home$HBP))))

f3_v <- ((master_spd_home$X3B) / (master_spd_home$AB - master_spd_home$HR - master_spd_home$K)) * (1/0.0016)

f4_v <- (((master_spd_home$R - master_spd_home$HR) / (master_spd_home$H + master_spd_home$BB + master_spd_home$HBP - master_spd_home$HR)) - 0.1) * 25

f5_v <- ((0.063) - (master_spd_home$GDP / (master_spd_home$AB - master_spd_home$HR - master_spd_home$K))) * (1/0.007)

master_spd_home$f6_v <- ""

master_spd_home$f6_v <- as.numeric(master_spd_home$f6_v)

master_spd_home$f6_v[which(master_spd_home$POS == "3B")] <- (4/2.65) * ((master_spd_home$PO[which(master_spd_home$POS == "3B")] + master_spd_home$A[which(master_spd_home$POS == "3B")]) /  master_spd_home$G[which(master_spd_home$POS == "3B")])
master_spd_home$f6_v[which(master_spd_home$POS == "P")] <- 0
master_spd_home$f6_v[which(master_spd_home$POS == "C")] <- 1
master_spd_home$f6_v[which(master_spd_home$POS == "1B")] <- 2
master_spd_home$f6_v[which(master_spd_home$POS == "2B")] <- (5/4) * ((master_spd_home$PO[which(master_spd_home$POS == "2B")] + master_spd_home$A[which(master_spd_home$POS == "2B")]) /  master_spd_home$G[which(master_spd_home$POS == "2B")])
master_spd_home$f6_v[which(master_spd_home$POS == "SS")] <- (4.6/7) * ((master_spd_home$PO[which(master_spd_home$POS == "SS")] + master_spd_home$A[which(master_spd_home$POS == "SS")]) /  master_spd_home$G[which(master_spd_home$POS == "SS")])
master_spd_home$f6_v[which(master_spd_home$POS %in% c("CF","LF","RF","OF"))] <- (3) * ((master_spd_home$PO[which(master_spd_home$POS %in% c("CF","LF","RF","OF"))] + master_spd_home$A[which(master_spd_home$POS %in% c("CF","LF","RF","OF"))]) /  master_spd_home$G[which(master_spd_home$POS %in% c("CF","LF","RF","OF"))])

master_spd_home$spd <- (f1_v + f2_v + f3_v + f4_v + f5_v + master_spd_home$f6_v) / 6

DH <- which(master_spd_home$POS == "DH")

master_spd_home$spd[DH] <- (f1_v[DH] + f2_v[DH] + f3_v[DH] + f4_v[DH] + f5_v[DH]) / 5

master_spd_home$spd <- master_spd_home$spd / 100

###

master_spd_home$arbw <- ""

master_spd_home$arbw <- as.numeric(master_spd_home$arbw)

for(zz in 1:nrow(master_spd_home)){
  master_spd_home$ISO[zz] <- ((master_spd_home$X2B[zz]) + (master_spd_home$X3B[zz] * 2) + (master_spd_home$HR[zz] * 3)) / master_spd_home$AB[zz]
  
}

master_spd_home$arbw <- (master_spd_home$ISO * 3) / 10

master_spd_home$rbi_bonus <- ""
master_spd_home$run_bonus <- ""

master_spd_home$rbi_bonus <- as.numeric(master_spd_home$rbi_bonus)
master_spd_home$run_bonus <- as.numeric(master_spd_home$run_bonus)

master_spd_home$rbi_bonus <- 0
master_spd_home$run_bonus <- 0

####

grouping <- data.frame(matrix(NA,nrow=9,ncol=4))

colnames(grouping) <- c("Hitter","one","two","three")

grouping$Hitter <- c(1:9)
grouping$one <- c(2:9,1)
grouping$two <- c(3:9,1:2)
grouping$three <- c(4:9,1:3)

box_home_copy <- box_stat_home

for(y in 1:9){
  current_hitter_home <- box_home_copy[which(box_home_copy$MLBId %in% master_spd_home$MLBId[y]),c("MLBId","PlayerName","X1B","X2B","X3B","BB","HBP","SB","CS")]
  current_hitter_home[,3:9] <- as.numeric(current_hitter_home[,3:9])
  
  current_hitter_home$total <- current_hitter_home$X1B + current_hitter_home$X2B + current_hitter_home$X3B + current_hitter_home$BB + current_hitter_home$HBP
  
  if(current_hitter_home$total == 0){
    
    master_spd_home$run_bonus[y] <- 0
    master_spd_home$rbi_bonus[y] <- 0
    next;
  }
  
  if(current_hitter_home$total > 0){
    log <- data.frame(matrix(NA,nrow=current_hitter_home$total,ncol=3))
    colnames(log) <- c("events","used","on") 
    log$used <- "NO"
    log$on <- NA
  }
  
  for(zzzzz in 14:38){
    box_home_copy[,zzzzz] <- as.numeric(box_home_copy[,zzzzz])
  }
  
  box_home_copy <- box_home_copy[1:9,]
  
  cs <- which(box_home_copy$CS > 0)
  
  if(length(cs) > 0){
    for(tint in 1:length(cs)){
      
      hit <- box_home_copy$H[cs[tint]] > 0
      walk <- box_home_copy$BB[cs[tint]] > 0
      
      if(hit == TRUE & walk == TRUE){
        nums <- 2
        select <- sample(x = 1:nums,size = 1,replace = FALSE)
        
        if(select == 1){
          box_home_copy$X1B[cs[tint]] <- box_home_copy$X1B[cs[tint]] - 1
          box_home_copy$H[cs[tint]] <- box_home_copy$H[cs[tint]] - 1
          box_home_copy$CS[cs[tint]] <- box_home_copy$CS[cs[tint]] - 1
          next;
        }
        
        if(select == 2){
          box_home_copy$BB[cs[tint]] <- box_home_copy$BB[cs[tint]] - 1
          box_home_copy$CS[cs[tint]] <- box_home_copy$CS[cs[tint]] - 1
          next;
        }
      }
      
      if((hit == TRUE & walk == FALSE)){
        box_home_copy$X1B[cs[tint]] <- box_home_copy$X1B[cs[tint]] - 1
        box_home_copy$H[cs[tint]] <- box_home_copy$H[cs[tint]] - 1
        box_home_copy$CS[cs[tint]] <- box_home_copy$CS[cs[tint]] - 1
        next;
      }
      
      if((hit == FALSE & walk == TRUE)){
        box_home_copy$BB[cs[tint]] <- box_home_copy$BB[cs[tint]] - 1
        box_home_copy$CS[cs[tint]] <- box_home_copy$CS[cs[tint]] - 1
        next;
      }
      
    }
  }
  
  for(yy in 3:7){
    
    event_name <- c(NA,NA,"Single","Double","Triple","Walk","HBP")
    
    if(current_hitter_home[1,yy] > 0){
      log[min(which(log$event %in% NA)):(current_hitter_home[1,yy] + min(which(log$event %in% NA)) - 1),c("events")] <- event_name[yy]
    }
    
    if(current_hitter_home[1,yy] == 0){
      next;
    }
    
  }
  
  box_stat_home2 <- box_stat_home[t(grouping[which(box_home_copy$MLBId %in% master_spd_home$MLBId[y]),2:4]),c("MLBId","PlayerName","X1B","X2B","X3B","HR")]
  
  box_stat_home2[,3] <- as.numeric(box_stat_home2[,3])
  box_stat_home2[,4] <- as.numeric(box_stat_home2[,4])
  box_stat_home2[,5] <- as.numeric(box_stat_home2[,5])
  box_stat_home2[,6] <- as.numeric(box_stat_home2[,6])
  box_stat_home2$total <- ""
  box_stat_home2$total <- as.numeric(box_stat_home2$total)
  box_stat_home2$total <- box_stat_home2$X1B + box_stat_home2$X2B + box_stat_home2$X3B + box_stat_home2$HR
  
  for(gggg in 1:nrow(log)){
    
    if(log$events[gggg] %in% c("Single","Walk","HBP")){
      
      if(current_hitter_home$SB[1] > 0){
        current_hitter_home$SB[1] <- current_hitter_home$SB[1] - 1
        log$on[gggg] <- 2
      }
      
      if(current_hitter_home$SB[1] == 0){
        log$on[gggg] <- 1
      }  
      
    }
    
    if(log$events[gggg] == "Double"){
      
      log$on[gggg] <- 2
      
    }
    
    if(log$events[gggg] == "Triple"){
      log$on[gggg] <- 3
    }
    
    for(yyy in 1:nrow(box_stat_home2)){
      
      if(box_stat_home2$total[yyy] > 0){
        
        log2 <- data.frame(matrix(NA,nrow=box_stat_home2$total[yyy],ncol=2))
        colnames(log2) <- c("events","used")
        log2$used <- "NO"
        
        for(yyyy in 3:6){
          
          event_name <- c(NA,NA,"Single","Double","Triple","HR")
          
          if(box_stat_home2[yyy,yyyy] > 0){
            log2[min(which(log2$event %in% NA)):(box_stat_home2[yyy,yyyy] + min(which(log2$event %in% NA)) - 1),c("events")] <- event_name[yyyy]
          }
          
          if(box_stat_home2[yyy,yyyy] == 0){
            next;
          }
          
        }
        
        if(box_stat_home2[yyy,6] > 0){
          
          box_stat_home2[yyy,6] <- box_stat_home2[yyy,6] - 1
          box_stat_home2[yyy,7] <- box_stat_home2[yyy,7] - 1
          if(log$events[gggg] == "Single"){
            
            master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
            master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
            break;
          }
          
          if(log$events[gggg] == "Double"){
            
            master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
            master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
            break;
          }
          
          if(log$events[gggg] == "Triple"){
            
            master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
            master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
            break;
          }
          
          if(log$events[gggg] == "Walk"){
            
            master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
            master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
            break;
          }
          
          if(log$events[gggg] == "HBP"){
            
            master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
            master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
            break;
          }
          
        }
        
        if(box_stat_home2[yyy,5] > 0){
          box_stat_home2[yyy,5] <- box_stat_home2[yyy,5] - 1
          box_stat_home2[yyy,7] <- box_stat_home2[yyy,7] - 1
          
          if(log$events[gggg] == "Single"){
            
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
            
          }
          
          if(log$events[gggg] == "Double"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Triple"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Walk"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "HBP"){
            
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
        }
        
        if(box_stat_home2[yyy,4] > 0){
          box_stat_home2[yyy,4] <- box_stat_home2[yyy,4] - 1
          box_stat_home2[yyy,7] <- box_stat_home2[yyy,7] - 1
          
          if(log$events[gggg] == "Single"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Double"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Triple"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Walk"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "HBP"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
        }
        
        if(box_stat_home2[yyy,3] > 0){
          
          box_stat_home2[yyy,3] <- box_stat_home2[yyy,3] - 1
          box_stat_home2[yyy,7] <- box_stat_home2[yyy,7] - 1
          
          if(log$events[gggg] == "Single"){
            log$on[gggg] <- log$on[gggg] + 2
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Double"){
            log$on[gggg] <- log$on[gggg] + 2
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Triple"){
            log$on[gggg] <- log$on[gggg] + 2
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Walk"){
            log$on[gggg] <- log$on[gggg] + 2
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "HBP"){
            log$on[gggg] <- log$on[gggg] + 2
            
            if(log$on[gggg] > 3){
              
              master_spd_home$run_bonus[y] <- master_spd_home$run_bonus[y] + (master_spd_home$spd[y])
              master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] <- master_spd_home$rbi_bonus[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])] + (master_spd_home$arbw[which(master_spd_home$MLBId %in% box_stat_home2$MLBId[yyy])])
              break;
            }
          }
          
        }
        
      }
      
      if(box_stat_home2$total[yyy] == 0){
        next;
      }
      
    }
  }
  
  
  
  
}


