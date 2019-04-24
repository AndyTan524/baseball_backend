library(XML)
library(tidyr)
library(plyr)

master_field <- read.csv("master_field_stat_mle.csv")

aaa <- read.csv("2016_aaa.csv")

aaa$date <- substr(aaa$game,5,14)

aaa$date <- as.Date(aaa$date,"%Y_%m_%d")

aaa <- aaa[aaa$date == as.Date("2016-07-10"),]

master1 <- data.frame(matrix(NA,nrow=1,ncol=9))

colnames(master1) <- c("id","name_display_first_last","pos","po","a","e","gameday","date","used")

for(k in 1:nrow(aaa)){
  
  print(paste0(k," of ",nrow(aaa)))
  
  url <- paste0("http://gd2.mlb.com/components/game/aaa/year_2016/","month_",substr(aaa$game[k],10,11),"/day_",substr(aaa$game[k],13,14),"/",aaa$game[k],"boxscore.xml")

  t <- try(xmlTreeParse(url))
  
  if(class(t) == "try-error"){
    next
  }
  
  xmlfile <- xmlTreeParse(url)
  
  xmlfile <- xmlfile[[1]]
  
  xmltop = xmlRoot(xmlfile)
  
  xmltop <- xmltop[[3]]
  
  xmltop2 <- xmlToList(xmltop)
  
  master <- data.frame(matrix(NA,nrow=6,ncol=1))
  
  rownames(master) <- c("id","name_display_first_last","pos","po","a","e")
  colnames(master) <- c("no_name")
  
  for(i in 1:length(xmltop2)){
    
    data <- as.data.frame(xmltop2[i])
    
    if(length(data) == 0){
      next;
    }
    
    if("team_flag" %in% row.names(data)){
      next;
    }
    
    data <- data[c("id","name_display_first_last","pos","po","a","e"),]
    
    master <- cbind(master,data)
    colnames(master)[i+1] <- as.character(data[1])
  }
  
  master2 <- data.frame(matrix(NA,nrow=ncol(master),ncol=6))
  
  colnames(master2) <- c("id","name_display_first_last","pos","po","a","e")
  
  master2$id <- t(master[1,])
  master2$name_display_first_last <- t(master[2,])
  master2$pos <- t(master[3,])
  master2$po <- t(master[4,])
  master2$a <- t(master[5,])
  master2$e <- t(master[6,])
  
  master2 <- master2[!master2$id %in% NA,]
  
  master2 <- master2[!master2$pos %in% "PH",]
  
  master2$gameday <- ""
  
  master2$gameday <- substr(x = url,start = 66,stop=95)
  
  master2$date <- substr(x=master2$gameday,start =5 ,stop=14)
  
  master2$date <- as.character(as.Date(master2$date,"%Y_%m_%d"))
  
  master2$used <- ""
  
  master1 <- rbind(master1,master2)

}

master1 <- master1[!master1$pos %in% c("PR","DH","PH","NA","",NA),]

dash <- grep(pattern = "-",x = master1$pos)

for(j in 1:length(dash)){
  
print(paste0(j," of ",length(dash)))


all <- length(unlist(strsplit(x = master1$pos[dash[j]],split = "-")))

if(unlist(strsplit(x = master1$pos[dash[j]],split = "-"))[1] %in% c("PR","DH","PH")){
  
for(k in 1:all){
  
if(!unlist(strsplit(x = master1$pos[dash[j]],split = "-"))[k] %in% c("PH","PR","DH")){
  
master1$pos[dash[j]] <- unlist(strsplit(x = master1$pos[dash[j]],split = "-"))[k]

break;

}
  
if(unlist(strsplit(x = master1$pos[dash[j]],split = "-"))[k] %in% c("PH","PR","DH")){
  
  if(k == all){
    master1$pos[dash[j]] <- unlist(strsplit(x = master1$pos[dash[j]],split = "-"))[k]
  }
  
next;
  
}
  
}
  
}


if(!unlist(strsplit(x = master1$pos[dash[j]],split = "-"))[1] %in% c("PR","DH","PH")){
  
master1$pos[dash[j]] <- unlist(strsplit(x = master1$pos[dash[j]],split = "-"))[1]

}

}

master1 <- master1[!master1$pos %in% c("PH","PR","DH"),]

id_exist <- as.character(unique(master_field$gameday))

master1 <- master1[!master1$gameday %in% id_exist,]

write.csv(master1,"master_field_stat_mle.csv",row.names = FALSE)

