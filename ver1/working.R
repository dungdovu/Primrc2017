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

#dist.obj <- dist(mydata[,1:2])
#hclust.obj <- hclust(dist.obj)
#css.obj <- css.hclust(dist.obj,hclust.obj)
#elbow.obj <- elbow.batch(css.obj)
#print(elbow.obj)






















wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)
#k=5


set.seed(123)
# # Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- mydata 
wss <- sapply(1:k.max, function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
#wssplot(1:k.max, wss,  type="b",  frame = TRUE,  xlab="Number of clusters K", ylab="Total within-clusters sum of squares (WSS)", main="Assessing the Optimal Number of Clusters with the Elbow Method",
 #   pch=20, cex=2)
# 
# 
bss <- sapply(1:k.max, function(k){kmeans(data, k, nstart=50,iter.max = 15 )}$betweenss)
tss <- sapply(1:k.max, function(k){kmeans(data, k, nstart=50,iter.max = 15 )}$totss) 

ratio<-bss/tss
y<-ratio
x<-c(1:k.max)
plot(x,y, type="b",  frame = TRUE,  xlab="Number of clusters k", ylab="The ratio of BSS/WSS", main="Assessing the Optimal Number of Clusters with the Elbow Method",
      pch=20, cex=2)


nrow(data)
set.seed(170)
dataCluster <- kmeans(train[, 5:6], 5, nstart = 20)



dataCluster
set.seed(179)
table(dataCluster$cluster)
#install.packages("ggplot2")
library(ggplot2)
dataCluster$cluster <- as.factor(dataCluster$cluster)
p<-ggplot(train, aes(train$lat, train$lon, color = dataCluster$cluster)) + geom_point()


p + labs(colour = "Cluster",x = "Latitude", y = "Longitude")
#plot the high level of each bin in the cluster

#choose the working cluster
cluster8<-data[which(dataCluster$cluster==3),]
head(cluster8,5)
#test in a certain date 06/16/2014 and in a certain cluster (cluster 8)
cluster8_temp<-cluster8[which(substr(cluster8$timestamp,1,10)=="2014-06-16"),]
cluster8_temp_red<-cluster8_temp[which(cluster8_temp$level.status > 0),]
plot(cluster8_temp_red[,5],cluster8_temp_red[,6],col="red",main= 1)


#plot cluser 1



#optimize


#install.packages("NISTunits", dependencies = TRUE)
#library(NISTunits)
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
set.seed(888)
tspFitness <- function(tour, ...) 1/tourLength(tour, ...)
GA <- ga(type = "permutation", fitness = tspFitness, distMatrix = D,
         min = 1, max = nrow(D), popSize = 50, maxiter = 5000,
         run = 500, pmutation = 0.2)

summary(GA)
#install.packages("polysat")
#install.packages("ape")

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
plot(cluster8_temp_red[,5],cluster8_temp_red[,6],col="red",xlab="Longitude",ylab="Latitude",pch=19)
#text(cluster8_temp_red[,5],cluster8_temp_red[,6]+0.001,labels(D[,1]), cex=1.5)
arrows(cluster8_temp_red[tour[-n],5],cluster8_temp_red[tour[-n],6],cluster8_temp_red[tour[-1],5],cluster8_temp_red[tour[-1],6],
       length = 0.15, angle = 36, col = "steelblue", lwd = 2)

####################################################################################################################


#insert value for the data set



# count duplicate 
count.duplicates <- function(DF){
  x <- do.call('paste', c(DF, sep = '\r'))
  ox <- order(x)
  rl <- rle(x[ox])
  cbind(DF[ox[cumsum(rl$lengths)],,drop=FALSE],count = rl$lengths)
 
}
DF <-data.frame(cluster8$sn)
DF_count<-count.duplicates(DF)


candidate<-DF_count[which(DF_count$count==max(DF_count$count)),]
temp_predict<-cluster8[which(cluster8$sn==candidate$cluster8.sn),]



temp_predict$timestamp<-as.numeric(temp_predict$timestamp)
attach(temp_predict)
#plot(temp_predict$timestamp,temp_predict$level.status,cex=0.5,pch=19,co;="green")


#linear regression
# relation <- lm(temp_predict$level.status~temp_predict$timestamp)
# summary(relation)
# predictlinear <- predict(relation, type = 'response')
# ggplot(temp_predict, aes(x=temp_predict$timestamp, y=temp_predict$level.status)) + geom_point() + 
#   stat_smooth(method="lm", se=FALSE)







#install.packages('caTools')
library(caTools)

status<-temp_predict$level.status
timest<-temp_predict$timestamp

model <- glm(status~timest,binomial)


plot(timest,status,col="darkgreen",ylab="The weight of trash bin", xlab = "The time stamp of collecting data after converting to the numeric")
summary(model)

xv<-seq(min(timest),max(timest),10)
yv<-predict(model,list(timest=xv),type="response")
lines(xv,yv,col="red")
# predict <- predict(model, type = 'response')
# 
# table(temp_predict$level.status, predict > 0.5)
# 
# 
 library(ROCR)
 ROCRpred <- prediction(timest,status)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))


