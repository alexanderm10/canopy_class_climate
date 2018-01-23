# library(FedData)
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
states <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio", "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Vermont", "Virginia", "West Virginia", "Wisconsin")
#states.short <- c("New York")
# Extract the states we want
borders <- us[us$NAME_1 %in% states,]
#plot(borders)

# borders2 <- spPolygons(borders)
# projection(borders2) <- projection(borders)
# class(borders2)

# Source Christy's extract.ITRDB function that can be downloaded from here:
# https://github.com/crollinson/R_Functions/blob/master/extract_itrdb.R
# Note: this repo is the one that also contains the gam helper functions
source("extract_itrdb.R")

itrdb.meta <- extract.itrdb(area.extract=borders, download.types=c("Chronology", "Raw Measurements", "Correlation Stats"), dir.out="ITRDB_data/ITRDB_US_east/")
summary(itrdb.meta)


# Using feddata package
# itrdb <- get_itrdb(template = borders2,
#                    label = "canopy_ITRDB",
#                    measurement.type = "Ring Width",
#                    chronology.type = c("Measurements Only"),
#                    raw.dir = "./input_files/ITRDB",
#                    extraction.dir = "./input_files/ITRDB_Extractions")

# summary(itrdb)
# summary(itrdb$metadata)

# meta <- itrdb$metadata

# names(meta)
unique(itrdb.meta$species.code); 
unique(itrdb.meta$species.name)

write.csv(itrdb.meta, "processed_data/eastern_ITRDB_metadata.csv", row.names=F)
