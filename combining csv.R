library(plyr)
library(dplyr)
#path - path to the directory
#recur- set true to include subfolders
#head- set true if files contains the coloumn names
#removefiles-  files which you do not want to include
path="./"
recur=F
head=T


test<-list.files(path,pattern="*.csv$",recursive = recur)%>%as.data.frame()

removefiles=c("cities.csv")

for(i in 1:length(removefiles)){
  test<-subset(test,test!=removefiles[i])
}

data<-data.frame()
i=1
for(i in 1:nrow(test)){
  d1<-read.csv(paste(path,test[i,1],sep="")%>%as.character(),header = head,stringsAsFactors = F)
  data<-rbind.fill(data,d1)
}

write.csv(data,"combined.csv")
