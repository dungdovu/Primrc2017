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

library(MASS)

#install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
pointv <- data.frame(x=c(1,1.5,3,5,3.5,4.5,3.5),
                     y=c(1,2,4,7,5,5,4.5))

plot(pointv)
data<-pointv
k.max = 5


library(GMD)

k=1

dist.obj <- dist(data[,1:2])
d_m<-mean(dist.obj)
s<-rep()
dist_<-as.vector(dist.obj)
for(i in 1:21)
  s[i] <-(dist_[i]-d_m)^2



bss <- sapply(1:k.max, function(k){kmeans(data, k, nstart=50,iter.max = 15 )}$betweenss)
tss <- sapply(1:k.max, function(k){kmeans(data, k, nstart=50,iter.max = 15 )}$totss) 
plot(1:k.max, bss/tss)
