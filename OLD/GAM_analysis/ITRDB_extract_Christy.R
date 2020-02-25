library(FedData)
library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(dplR)
library(maps)


# Get all US state Bondaries
us <- getData("GADM", country="USA", level=1)
us$NAME_1 <- as.factor(us$NAME_1)
summary(us)
#plot(us)

# The states we want to extract
states <- c("Alabama", "Arkansas","Connecticut","Delaware","Florida","Georgia","Illinois","Indiana", "Iowa","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan",  "Minnesota","Mississippi","Missouri","New Hampshire","New Jersey","New York", "North Carolina","Ohio","Pennsylvania","Rhode Island","South Carolina","Tennessee", "Vermont","Virginia", "West Virginia","Wisconsin")
#states.short <- c("New York")
# Extract the states we want
borders <- us[us$NAME_1 %in% states,]
#plot(borders)

borders2 <- spPolygons(borders)
projection(borders2) <- projection(borders)
class(borders2)


# Source Christy's ITRDB script that can be found here:
# https://github.com/crollinson/R_Functions/blob/master/extract_itrdb.R
source("~/Desktop/extract_itrdb.R")
extract.itrdb(area.extract=borders, download.types=)

