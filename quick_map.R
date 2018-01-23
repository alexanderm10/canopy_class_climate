# Quick map

library(raster); library(maps)
library(ggplot2)


itrdb.loc <- read.csv("processed_data/ITRDB_site_locations.csv", header=T)
eco.loc <- read.csv("processed_data/ecology_site_locations.csv", header=T)

itrdb.loc$type <- as.factor("ITRDB")
eco.loc$type <- as.factor("Ecology")

itrdb.loc <- SpatialPointsDataFrame(coords=itrdb.loc[,c("longitude", "latitude")], itrdb.loc, proj4string=CRS("+proj=longlat"))
eco.loc <- SpatialPointsDataFrame(coords=eco.loc[,c("longitude", "latitude")], eco.loc, proj4string=CRS("+proj=longlat"))

map("state", plot=T,lty="solid", col="gray30", lwd=1.5)
plot(itrdb.loc, pch=19, add=T, cex=0.5, col="blue")
plot(eco.loc, pch=19, add=T, cex=0.5, col="orange")

