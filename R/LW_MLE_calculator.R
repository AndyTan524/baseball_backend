LW_MLE <- read.csv("LW_MLE.csv")

for(i in 1:ncol(LW_MLE)){LW_MLE[,i] <- as.double(LW_MLE[,i])}

single <- -0.46
walk <- -0.33
out <- 0.25

for(j in 1:nrow(LW_MLE))
{
  Best_LW_value <- 20
  
  for(k in 40:0)
  {
    for(l in 40:0)
    {
  
      raw_value <- (k * single) + (l * walk) + (LW_MLE$OUT[j] * out)
      
      if(raw_value < 0)
      {
        current_LW_value <- abs(raw_value) - 0.5
      }
      
      if(raw_value > 0)
      {
        current_LW_value <- abs(raw_value) + 0.5
      }
      
      if(abs(current_LW_value) < abs(Best_LW_value))
      {
        Best_LW_value <- current_LW_value
        
        LW_MLE$X1B[j] <- k;
        LW_MLE$BB[j] <- l;
        
        LW_MLE$LW[j] <- raw_value
      }
      
      if(abs(current_LW_value) > abs(Best_LW_value))
      {
        Best_LW_value <- Best_LW_value
      }
      
      if(current_LW_value == 0)
      {
        next;
      }
      
    }
  }
}

write.csv(LW_MLE,"LW_MLE.csv",row.names=FALSE)