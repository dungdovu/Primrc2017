#set the working folder
setwd("/home/student/WORK/Pimrc2017/Primrc2017/ver1")
getwd()
# install the needed packages

libs <- c("sp", "classInt", "RColorBrewer", "ggplot2", "hexbin", "ggmap", "dplyr","e1071","rworldmap","mapdata")

for (x in libs){
  if(x %in% rownames(installed.packages()) == FALSE) {
    print(paste0("installing ", x, "..."))
    install.packages(x)
  }
  else{
    print (paste0(x, " is already installed "))
  }
  library(x, character.only = TRUE)
}
#install.packages("MASS")
#distance between two point
library(GA)
library(MASS)

#install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)

distance_two_points<- function(lat1,lon1,lat2,lon2)
{
  d<- acos( sin(NISTdegTOradian(lat1))*sin(NISTdegTOradian(lat2)) + cos(NISTdegTOradian(lat1))*cos(NISTdegTOradian(lat2))*cos(NISTdegTOradian(lon2)-NISTdegTOradian(lon1)) ) * 6371000
  
}

#read data from the local and process it
data<-read.csv("trashbin.csv",header=TRUE,row.names=NULL,sep=';')
head(data,5)
data$level.status<-data$level.status*0.1
data<-data.frame(data)
data$X.1<-NULL
data$X.2<-NULL
names(data)[5]<-paste("lat")
names(data)[6]<-paste("lon")
data$lat<-as.numeric(gsub('\\(', '', data$lat))
data$lon<-as.numeric(gsub('\\)', '', data$lon))
data$timestamp<-gsub('\\PM', '', data$timestamp)
data$timestamp<-gsub('\\AM', '', data$timestamp)
data$timestamp<-trimws(data$timestamp)
nchar(data$timestamp[996])
retemp<-substr(data$timestamp,1,10)
setemp<-substr(data$timestamp,12,nchar(data$timestamp))
i<-1
for (i in 1:length(setemp))
{
  if(nchar(setemp[i])<5)
  {
    setemp[i]<-paste("0",setemp[i],":00",sep="")
  }
  else
    if(nchar(setemp[i])<6)
    {
      setemp[i]<-paste(setemp[i],":00",sep="")
    }
}
data$timestamp<-paste(retemp,setemp,sep=" ")
#convert string to date
data$timestamp<-strptime(data$timestamp, "%m/%d/%Y %H:%M:%S")
#write.csv(data,"data.csv")
summary(data)
#take the sample of data to get the 80% training data and 20% test data 
set.seed(123)
#training and test data
sample <- sample.int(n = nrow(data), size = floor(.8*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]
head(train,5)
head(test,5)
set.seed(124)
# 
# for (i in 1:nrow(data))
# {
#   if (data$level[i] =="GREEN")
#   {
#     data$level.status[i]<-sample(0:39, 1,replace=T)/100
#   }
#   if (data$level[i] =="YELLOW")
#   {
#     data$level.status[i]<-sample(40:69, 1,replace=T)/100
#   }
# 
#   if (data$level[i] =="RED")
#   {
#     data$level.status[i]<-sample(70:100, 1,replace=T)/100
#   }
#   print(i)
# 
# 
# }



for (i in 1:nrow(data))
{
  if (data$level[i] =="GREEN")
  {
    data$level.status[i]<-0
  }
  if (data$level[i] =="YELLOW")
  {
    data$level.status[i]<-1
  }
  
  if (data$level[i] =="RED")
  {
    data$level.status[i]<-1
  }
  print(i)
  
  
}
ggplot(data,aes(data$lat, data$lon))+geom_point()


set.seed(20)

mydata <- cbind(data$lat,data$lon)

library(GMD)

dist.obj <- dist(mydata[,1:2])
hclust.obj <- hclust(dist.obj)
css.obj <- css.hclust(dist.obj,hclust.obj)
elbow.obj <- elbow.batch(css.obj)
print(elbow.obj)

