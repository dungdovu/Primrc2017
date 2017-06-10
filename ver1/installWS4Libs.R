### this script checks if libraries for Workshop 4 are available, 
### if not it installs, then loads them all
### cengel - Feb 2015

libs <- c("sp", "rgdal", "classInt", "RColorBrewer", "ggplot2", "hexbin", "ggmap", "XML", "dplyr")

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
library(ggmap)

# get the basemap
phBasemap <- get_map(location="Philadelphia, PA", zoom=12, maptype = 'roadmap')
waste=read.csv("coordinate.csv",header=TRUE,row.names=NULL,sep=';')
write.csv(waste,"a.csv")
colnames(waste) = c("lat","lon")
mapPoints <- ggmap(phBasemap)   +   geom_point(aes(x = lon, y = lat,cex=0.05), data = waste, alpha = .5)

