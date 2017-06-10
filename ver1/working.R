#set the working folder
setwd("/home/student/Downloads/ver1")
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
distance_two_points<- function(lat1,lon1,lat2,lon2)
{
  d<- acos( sin(NISTdegTOradian(lat1))*sin(NISTdegTOradian(lat2)) + cos(NISTdegTOradian(lat1))*cos(NISTdegTOradian(lat2))*cos(NISTdegTOradian(lon2)-NISTdegTOradian(lon1)) ) * 6371000
  
}

#read data from the local and process it
data<-read.csv("trashbin.csv",header=TRUE,row.names=NULL,sep=';')
head(data,5)
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

ggplot(data,aes(data$lat, data$lon))+geom_point()


set.seed(20)

mydata <- cbind(train$lat,train$lon)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)
#k=5

dataCluster <- kmeans(train[, 5:6], 5, nstart = 20)


dataCluster
table(dataCluster$cluster)
#install.packages("ggplot2")
library(ggplot2)
dataCluster$cluster <- as.factor(dataCluster$cluster)
ggplot(train, aes(train$lat, train$lon, color = dataCluster$cluster)) + geom_point()


#plot the high level of each bin in the cluster


cluster8<-train[which(dataCluster$cluster==1),]

#test in a certain date 06/16/2014 and in a certain cluster (cluster 8)
cluster8_temp<-cluster8[which(substr(cluster8$timestamp,1,10)=="2014-06-16"),]
cluster8_temp_red<-cluster8_temp[which(cluster8_temp$level=="RED"),]
plot(cluster8_temp_red[,5],cluster8_temp_red[,6],col="red",main= 1)


#plot cluser 1



#optimize


#install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
data_temp_dup<-cluster8_temp_red[!duplicated(cluster8_temp_red),]
l<-nrow(data_temp_dup)
d1<-matrix(NA, nrow = l, ncol = l)


dim(d1)


i<-1
j<-1
for(i in 1:l)
{ for(j in 1:l)
{
  d1[i,j]<-distance_two_points(data_temp_dup[i,5],data_temp_dup[i,6],data_temp_dup[j,5],data_temp_dup[j,6])
  
}
  print(i) 
}




tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[,2:1]
  sum(distMatrix[route])
}

D <- as.matrix(d1)
head(D)

tspFitness <- function(tour, ...) 1/tourLength(tour, ...)
GA <- ga(type = "permutation", fitness = tspFitness, distMatrix = D,
         min = 1, max = nrow(D), popSize = 50, maxiter = 5000,
         run = 500, pmutation = 0.2)

summary(GA)


mds <- cmdscale(D)
x <- mds[, 1]
y <- -mds[, 2]
#plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
abline(h = pretty(range(x), 10), v = pretty(range(y), 10),
       col = "light gray")
tour <- GA@solution[1, ]
tour <- c(tour, tour[1])
n <- length(tour)
#arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]],
 #     length = 0.15, angle = 36, col = "steelblue", lwd = 2)

#text(x,y,labels(D[,1]))
#print tour of verhicle
tour
plot(cluster8_temp_red[,5],cluster8_temp_red[,6],col="red",xlab="longitude",ylab="latitude",pch=19,main="Optimization verhicle route at 06/16/2014")
text(cluster8_temp_red[,5],cluster8_temp_red[,6]+0.001,labels(D[,1]), cex=1.5)
arrows(cluster8_temp_red[tour[-n],5],cluster8_temp_red[tour[-n],6],cluster8_temp_red[tour[-1],5],cluster8_temp_red[tour[-1],6],
       length = 0.15, angle = 36, col = "steelblue", lwd = 2)

####################################################################################################################

# count duplicate 
count.duplicates <- function(DF){
  x <- do.call('paste', c(DF, sep = '\r'))
  ox <- order(x)
  rl <- rle(x[ox])
  cbind(DF[ox[cumsum(rl$lengths)],,drop=FALSE],count = rl$lengths)
  
}
DF <-data.frame(cluster8$sn)
DF_count<-count.duplicates(DF)
temp_predict<-cluster8[which(cluster8$sn==DF[which(DF_count$count==max(DF_count$count)),]),]




plot(temp_predict$timestamp,temp_predict$level.status,cex=0.5,pch=19)

temp_predict$timestamp<-as.numeric(temp_predict$timestamp)
#linear regression
#relation <- lm(temp_predict$level.status~temp_predict$timestamp)
#logic regression

#install.packages('caTools')
library(caTools)




