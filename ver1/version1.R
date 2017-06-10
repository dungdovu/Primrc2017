#install.packages("netgen")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("MASS")
#install.packages("e1071")
#install.packages("dismo")
#install.packages("XML")
#install.packages("rworldmap")
#install.packages("mapdata")
#install.packages("sp")
#install.packages("forecast")

library(sp)
library(rworldmap)
library(ggplot2)
library(grid)
library(maps)
library(mapdata)

library(netgen)
library(ggplot2)
library(gridExtra)
library(e1071)
library(MASS)

library(dismo)
library(scales)
library("forecast")
#install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)

distance_two_points<- function(lat1,lon1,lat2,lon2)
{
  d<- acos( sin(NISTdegTOradian(lat1))*sin(NISTdegTOradian(lat2)) + cos(NISTdegTOradian(lat1))*cos(NISTdegTOradian(lat2))*cos(NISTdegTOradian(lon2)-NISTdegTOradian(lon1)) ) * 6371000
  
}


data<-read.csv("trashbin.csv",header=TRUE,row.names=NULL,sep=';')
#plot(data)

data_temp<-data.frame(cbind(data[,1],data[,7],data[,8],data[,9],data[,10]))







# #map on the google
# bb<-duplicated(data_temp)
# data_temp_dup<-data_temp[!duplicated(data_temp),]
# write.csv(data_temp_dup,"cc.csv",row.names=FALSE)
# l<-nrow(data_temp_dup)
# head(data_temp_dup)
# #time series
# #tm<-data[which(data[,1]==171783),]
# #head(tm)
# #plot.ts(tm[,3],tm[,9],yax.flip = FALSE)
# #distance between all the node
# head(data_temp_dup)
d1<-matrix(NA, nrow = l, ncol = l)

dim(d1)

head(d1)
i<-1
j<-1
for(i in 1:l)
 { for(j in 1:l)
  {
    d1[i,j]<-distance_two_points(data_temp_dup[i,2],data_temp_dup[i,3],data_temp_dup[j,2],data_temp_dup[j,3])

 }
  print(i) 
}


D <- as.matrix(d1)
head(D)
tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[,2:1]
  sum(distMatrix[route])
}

tspFitness <- function(tour, ...) 1/tourLength(tour, ...)
GA <- ga(type = "permutation", fitness = tspFitness, distMatrix = D,
         min = 1, max = attr(eurodist, "Size"), popSize = 50, maxiter = 5000,
         run = 500, pmutation = 0.2)

summary(GA)



