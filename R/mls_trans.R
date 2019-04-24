crunch <- read.csv("crunchoct.csv")

trans <- read.csv("16transaction.csv")

trans$one <- ""
trans$two <- ""
trans$three <- ""
trans$four <- ""

trans$Transaction <- paste("<ca>",trans$Transaction,"<ca>",sep="")

for(i in 1:nrow(trans))
{
  trans$one[i] <- unlist(strsplit(as.character(trans$Transaction[i]),"<ca>"))[2]
  trans$two[i] <- unlist(strsplit(as.character(trans$Transaction[i]),"<ca>"))[3]
  trans$three[i] <- unlist(strsplit(as.character(trans$Transaction[i]),"<ca>"))[4]
  trans$four[i] <- unlist(strsplit(as.character(trans$Transaction[i]),"<ca>"))[5]
  
}

trans <- trans[order(trans$Date),]

trans$Transaction <- NULL

trans$Team <- ""
trans$Name <- ""

for(j in 1:nrow(trans))
{
  ifelse(substr(trans$one[j],nchar(trans$one[j]),nchar(trans$one[j])) == ".",sub("[.]","",trans$one[j]),next)
}

for(j in 1:nrow(trans))
{
  ifelse(substr(trans$two[j],nchar(trans$two[j]),nchar(trans$two[j])) == ".",sub("[.]","",trans$two[j]),next)
}

for(j in 1:nrow(trans))
{
  ifelse(substr(trans$three[j],nchar(trans$three[j]),nchar(trans$three[j])) == ".",sub("[.]","",trans$three[j]),next)
}

for(j in 1:nrow(trans))
{
  ifelse(substr(trans$four[j],nchar(trans$four[j]),nchar(trans$four[j])) == ".",sub("[.]","",trans$four[j]),next)
}


  