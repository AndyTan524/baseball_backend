master_field_stat <- read.csv("master_field_game_score_stat.csv")
master_bat_stat <- read.csv("master_bat_game_score_stat.csv")

master_bat_stat_visit <- master_bat_stat[master_bat_stat$MLBId %in% box_visit$MLBId,]
master_field_stat_visit <- master_field_stat[master_field_stat$MLBId %in% box_visit$MLBId,]

master_spd_visit <- merge(master_bat_stat_visit,master_field_stat_visit,by = c("MLBId"))

master_spd_visit$spd <- ""

master_spd_visit$spd <- as.numeric(master_spd_visit$spd)

f1_v <- (((((master_spd_visit$SB) + 3)) / ((master_spd_visit$SB) + (master_spd_visit$CS) + 7)) - 0.4) * 20

f2_v <- (1/0.07) * sqrt((((master_spd_visit$SB) + (master_spd_visit$CS)) / ((master_spd_visit$X1B) + (master_spd_visit$BB) + (master_spd_visit$HBP))))

f3_v <- ((master_spd_visit$X3B) / (master_spd_visit$AB - master_spd_visit$HR - master_spd_visit$K)) * (1/0.0016)

f4_v <- (((master_spd_visit$R - master_spd_visit$HR) / (master_spd_visit$H + master_spd_visit$BB + master_spd_visit$HBP - master_spd_visit$HR)) - 0.1) * 25

f5_v <- ((0.063) - (master_spd_visit$GDP / (master_spd_visit$AB - master_spd_visit$HR - master_spd_visit$K))) * (1/0.007)

master_spd_visit$f6_v <- ""

master_spd_visit$f6_v <- as.numeric(master_spd_visit$f6_v)

master_spd_visit$f6_v[which(master_spd_visit$POS == "3B")] <- (4/2.65) * ((master_spd_visit$PO[which(master_spd_visit$POS == "3B")] + master_spd_visit$A[which(master_spd_visit$POS == "3B")]) /  master_spd_visit$G[which(master_spd_visit$POS == "3B")])
master_spd_visit$f6_v[which(master_spd_visit$POS == "P")] <- 0
master_spd_visit$f6_v[which(master_spd_visit$POS == "C")] <- 1
master_spd_visit$f6_v[which(master_spd_visit$POS == "1B")] <- 2
master_spd_visit$f6_v[which(master_spd_visit$POS == "2B")] <- (5/4) * ((master_spd_visit$PO[which(master_spd_visit$POS == "2B")] + master_spd_visit$A[which(master_spd_visit$POS == "2B")]) /  master_spd_visit$G[which(master_spd_visit$POS == "2B")])
master_spd_visit$f6_v[which(master_spd_visit$POS == "SS")] <- (4.6/7) * ((master_spd_visit$PO[which(master_spd_visit$POS == "SS")] + master_spd_visit$A[which(master_spd_visit$POS == "SS")]) /  master_spd_visit$G[which(master_spd_visit$POS == "SS")])
master_spd_visit$f6_v[which(master_spd_visit$POS %in% c("CF","LF","RF","OF"))] <- (3) * ((master_spd_visit$PO[which(master_spd_visit$POS %in% c("CF","LF","RF","OF"))] + master_spd_visit$A[which(master_spd_visit$POS %in% c("CF","LF","RF","OF"))]) /  master_spd_visit$G[which(master_spd_visit$POS %in% c("CF","LF","RF","OF"))])

master_spd_visit$spd <- (f1_v + f2_v + f3_v + f4_v + f5_v + master_spd_visit$f6_v) / 6

DH <- which(master_spd_visit$POS == "DH")

master_spd_visit$spd[DH] <- (f1_v[DH] + f2_v[DH] + f3_v[DH] + f4_v[DH] + f5_v[DH]) / 5

master_spd_visit$spd <- master_spd_visit$spd / 100

###

master_spd_visit$arbw <- ""

master_spd_visit$arbw <- as.numeric(master_spd_visit$arbw)

for(zz in 1:nrow(master_spd_visit)){
  master_spd_visit$ISO[zz] <- ((master_spd_visit$X2B[zz]) + (master_spd_visit$X3B[zz] * 2) + (master_spd_visit$HR[zz] * 3)) / master_spd_visit$AB[zz]
  
}

master_spd_visit$arbw <- (master_spd_visit$ISO * 3) / 10

master_spd_visit$rbi_bonus <- ""
master_spd_visit$run_bonus <- ""

master_spd_visit$rbi_bonus <- as.numeric(master_spd_visit$rbi_bonus)
master_spd_visit$run_bonus <- as.numeric(master_spd_visit$run_bonus)

master_spd_visit$rbi_bonus <- 0
master_spd_visit$run_bonus <- 0

####

grouping <- data.frame(matrix(NA,nrow=9,ncol=4))

colnames(grouping) <- c("Hitter","one","two","three")

grouping$Hitter <- c(1:9)
grouping$one <- c(2:9,1)
grouping$two <- c(3:9,1:2)
grouping$three <- c(4:9,1:3)

box_visit_copy <- box_stat_visit

for(y in 1:9){
  current_hitter_visit <- box_visit_copy[which(box_visit_copy$MLBId %in% master_spd_visit$MLBId[y]),c("MLBId","PlayerName","X1B","X2B","X3B","BB","HBP","SB","CS")]
  current_hitter_visit[,3:9] <- as.numeric(current_hitter_visit[,3:9])
  
  current_hitter_visit$total <- current_hitter_visit$X1B + current_hitter_visit$X2B + current_hitter_visit$X3B + current_hitter_visit$BB + current_hitter_visit$HBP
  
  if(current_hitter_visit$total == 0){
    
    master_spd_visit$run_bonus[y] <- 0
    master_spd_visit$rbi_bonus[y] <- 0
    next;
  }
  
  if(current_hitter_visit$total > 0){
    log <- data.frame(matrix(NA,nrow=current_hitter_visit$total,ncol=3))
    colnames(log) <- c("events","used","on") 
    log$used <- "NO"
    log$on <- NA
  }
  
  for(zzzzz in 14:38){
    box_visit_copy[,zzzzz] <- as.numeric(box_visit_copy[,zzzzz])
  }
  
  box_visit_copy <- box_visit_copy[1:9,]

  cs <- which(box_visit_copy$CS > 0)
  
  if(length(cs) > 0){
    for(tint in 1:length(cs)){
      
      hit <- box_visit_copy$H[cs[tint]] > 0
      walk <- box_visit_copy$BB[cs[tint]] > 0
      
      if(hit == TRUE & walk == TRUE){
        nums <- 2
        select <- sample(x = 1:nums,size = 1,replace = FALSE)
        
        if(select == 1){
          box_visit_copy$X1B[cs[tint]] <- box_visit_copy$X1B[cs[tint]] - 1
          box_visit_copy$H[cs[tint]] <- box_visit_copy$H[cs[tint]] - 1
          box_visit_copy$CS[cs[tint]] <- box_visit_copy$CS[cs[tint]] - 1
          next;
        }
        
        if(select == 2){
          box_visit_copy$BB[cs[tint]] <- box_visit_copy$BB[cs[tint]] - 1
          box_visit_copy$CS[cs[tint]] <- box_visit_copy$CS[cs[tint]] - 1
          next;
        }
      }
      
      if((hit == TRUE & walk == FALSE)){
        box_visit_copy$X1B[cs[tint]] <- box_visit_copy$X1B[cs[tint]] - 1
        box_visit_copy$H[cs[tint]] <- box_visit_copy$H[cs[tint]] - 1
        box_visit_copy$CS[cs[tint]] <- box_visit_copy$CS[cs[tint]] - 1
        next;
      }
      
      if((hit == FALSE & walk == TRUE)){
        box_visit_copy$BB[cs[tint]] <- box_visit_copy$BB[cs[tint]] - 1
        box_visit_copy$CS[cs[tint]] <- box_visit_copy$CS[cs[tint]] - 1
        next;
      }
      
    }
  }
  
  for(yy in 3:7){
    
    event_name <- c(NA,NA,"Single","Double","Triple","Walk","HBP")
    
    if(current_hitter_visit[1,yy] > 0){
      log[min(which(log$event %in% NA)):(current_hitter_visit[1,yy] + min(which(log$event %in% NA)) - 1),c("events")] <- event_name[yy]
    }
    
    if(current_hitter_visit[1,yy] == 0){
      next;
    }
    
  }
  
  box_stat_visit2 <- box_stat_visit[t(grouping[which(box_visit_copy$MLBId %in% master_spd_visit$MLBId[y]),2:4]),c("MLBId","PlayerName","X1B","X2B","X3B","HR")]
  
  box_stat_visit2[,3] <- as.numeric(box_stat_visit2[,3])
  box_stat_visit2[,4] <- as.numeric(box_stat_visit2[,4])
  box_stat_visit2[,5] <- as.numeric(box_stat_visit2[,5])
  box_stat_visit2[,6] <- as.numeric(box_stat_visit2[,6])
  box_stat_visit2$total <- ""
  box_stat_visit2$total <- as.numeric(box_stat_visit2$total)
  box_stat_visit2$total <- box_stat_visit2$X1B + box_stat_visit2$X2B + box_stat_visit2$X3B + box_stat_visit2$HR

  for(gggg in 1:nrow(log)){
    
    if(log$events[gggg] %in% c("Single","Walk","HBP")){

     if(current_hitter_visit$SB[1] > 0){
       current_hitter_visit$SB[1] <- current_hitter_visit$SB[1] - 1
       log$on[gggg] <- 2
     }
    
      if(current_hitter_visit$SB[1] == 0){
        log$on[gggg] <- 1
      }  
    
    }
    
    if(log$events[gggg] == "Double"){
      
        log$on[gggg] <- 2
    
    }
    
    if(log$events[gggg] == "Triple"){
      log$on[gggg] <- 3
    }
    
    for(yyy in 1:nrow(box_stat_visit2)){
      
      if(box_stat_visit2$total[yyy] > 0){
        
        log2 <- data.frame(matrix(NA,nrow=box_stat_visit2$total[yyy],ncol=2))
        colnames(log2) <- c("events","used")
        log2$used <- "NO"
        
        for(yyyy in 3:6){
          
          event_name <- c(NA,NA,"Single","Double","Triple","HR")
          
          if(box_stat_visit2[yyy,yyyy] > 0){
            log2[min(which(log2$event %in% NA)):(box_stat_visit2[yyy,yyyy] + min(which(log2$event %in% NA)) - 1),c("events")] <- event_name[yyyy]
          }
          
          if(box_stat_visit2[yyy,yyyy] == 0){
            next;
          }
          
        }
        
        if(box_stat_visit2[yyy,6] > 0){
          
          box_stat_visit2[yyy,6] <- box_stat_visit2[yyy,6] - 1
          box_stat_visit2[yyy,7] <- box_stat_visit2[yyy,7] - 1
          if(log$events[gggg] == "Single"){
            
            master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
            master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
            break;
          }
          
          if(log$events[gggg] == "Double"){
            
            master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
            master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
            break;
          }
          
          if(log$events[gggg] == "Triple"){
            
            master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
            master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
            break;
          }
          
          if(log$events[gggg] == "Walk"){
            
            master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
            master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
            break;
          }
          
          if(log$events[gggg] == "HBP"){
            
            master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
            master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
            break;
          }
          
        }
        
        if(box_stat_visit2[yyy,5] > 0){
          box_stat_visit2[yyy,5] <- box_stat_visit2[yyy,5] - 1
          box_stat_visit2[yyy,7] <- box_stat_visit2[yyy,7] - 1
          
          if(log$events[gggg] == "Single"){
            
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
            
          }
          
          if(log$events[gggg] == "Double"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
            }
          
          if(log$events[gggg] == "Triple"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Walk"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "HBP"){
            
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
          }
          
        }
        
        if(box_stat_visit2[yyy,4] > 0){
          box_stat_visit2[yyy,4] <- box_stat_visit2[yyy,4] - 1
          box_stat_visit2[yyy,7] <- box_stat_visit2[yyy,7] - 1
          
          if(log$events[gggg] == "Single"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Double"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Triple"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Walk"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "HBP"){
            log$on[gggg] <- log$on[gggg] + 3
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
          }
          
        }
        
        if(box_stat_visit2[yyy,3] > 0){
          
          box_stat_visit2[yyy,3] <- box_stat_visit2[yyy,3] - 1
          box_stat_visit2[yyy,7] <- box_stat_visit2[yyy,7] - 1
          
          if(log$events[gggg] == "Single"){
            log$on[gggg] <- log$on[gggg] + 2
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Double"){
            log$on[gggg] <- log$on[gggg] + 2
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Triple"){
            log$on[gggg] <- log$on[gggg] + 2
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "Walk"){
            log$on[gggg] <- log$on[gggg] + 2
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
          }
          
          if(log$events[gggg] == "HBP"){
            log$on[gggg] <- log$on[gggg] + 2
            
            if(log$on[gggg] > 3){
              
              master_spd_visit$run_bonus[y] <- master_spd_visit$run_bonus[y] + (master_spd_visit$spd[y])
              master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] <- master_spd_visit$rbi_bonus[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])] + (master_spd_visit$arbw[which(master_spd_visit$MLBId %in% box_stat_visit2$MLBId[yyy])])
              break;
            }
          }
          
        }
        
      }
      
      if(box_stat_visit2$total[yyy] == 0){
        next;
      }
      
    }
  }
  
  
    
    
  }
  

