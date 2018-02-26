# ------------------------------------
# Extracting Monthly Met from PRISM for all of Ross's Sites
# C. Rollinson 10 Feb 2016
# ------------------------------------

# ------------------------------------
# Load libraries
# ------------------------------------
library(raster); library(maps)
# ------------------------------------

# ------------------------------------
# Load file with site names & locations & turn it into a spatial file
# ------------------------------------
plot.dat <- read.csv("input_data/NE_site_locations.csv", header=T)
summary(plot.dat)


# Aggregating the location of plots up to the site level so that:
#  1) We have lat/lon for missing plots
#  2) We're saving ourselves a lot of time in the extraction
#  -- Note: this makes the assumption that there are no microclimatic differences among sites
site.dat <- aggregate(plot.dat[,c("latitude", "longitude")],
                      by=plot.dat[,c("site.name", "Site.Code")], 
                      FUN=mean, na.rm=T)

# Make sure Longitude is negative
site.dat$longitude <- ifelse(site.dat$longitude>0, site.dat$longitude*-1, site.dat$longitude)
summary(site.dat)

# Get rid of sites without any lat/lon info
site.dat <- site.dat[!is.na(site.dat$longitude),]
summary(site.dat)

# Making site a spatial file & graphing to make sure everything looks okay
site.loc <- SpatialPointsDataFrame(coords=site.dat[,c("longitude", "latitude")], site.dat, proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# A quick & dirty plot to double check things 
#  (ggplot can make it a lot prettier but requires extra steps)
map("state", plot=T,lty="solid", col="gray30", lwd=1.5, 
    xlim=range(site.loc$longitude)+c(-2,2), 
    ylim=range(site.loc$latitude)+c(-2,2.25))
plot(site.loc, pch=19, add=T, cex=1, col="blue")

# saving ecology site locations for a quick map
write.csv(site.loc, file = "processed_data/NE_site_locations.csv", row.names=F)
# ------------------------------------




# ------------------------------------
# Set up & extract the PRISM data
# ------------------------------------
# Directory containing PRISM data & what variables we have
path.out <- "/Volumes/GoogleDrive/My Drive/canopy_and_climate/met/"
dir.prism <- "~/Desktop/SpatialData/PRISM/monthly/"
var.prism <- c("ppt", "tmax", "tmin", "tmean", "vpdmax", "vpdmin")
mo.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
mo.num <- stringr::str_pad(1:12, 2, "left", "0")

# Simplifying our lives by figuring out which directories we have for one variable
for(VAR in var.prism){
  print("")
  print(VAR)
  
  dir.var  <- dir(file.path(dir.prism, VAR))
  
  # Looping through and doing the extraction of all years of met
  met.all <- data.frame()
  pb <- txtProgressBar(min=0, max=length(dir.var), style=3)
  for(j in 1:length(dir.var)){
    files.var  <- dir(file.path(dir.prism, VAR, dir.var[j]), "_bil.bil")
    
    # remove the .xml and annual mean files from these lists
    files.var <- files.var[!substr(files.var, nchar(files.var)-3, nchar(files.var))==".xml"]
    files.var <- files.var[nchar(files.var)==max(nchar(files.var))] # Annual mean is missing a month field, so it's name is shorter
    
    met.rast <- stack(file.path(dir.prism, VAR, dir.var[j], files.var))
    
    met.extract <- data.frame(extract(met.rast, site.loc, method = "bilinear"))
    names(met.extract) <- mo.num
    
    met.long <- stack(met.extract)
    names(met.long) <- c("value", "month")
    met.long$year <- as.numeric(dir.var[j])
    met.long$met  <- VAR
    met.long[,names(site.dat)] <- site.dat
    # summary(met.long)
    
    met.all <- rbind(met.all, met.long)
    rm(files.var, met.rast, met.extract, met.long)
    
    setTxtProgressBar(pb, j)
  }
  
  # Setting up the lags
  met.lag <- met.all
  met.lag$year <- met.lag$year+1
  met.lag$month <- paste0("p",met.lag$month)
  
  met.all <- rbind(met.lag, met.all)
  met.all$month <- factor(met.all$month, levels=c(paste0("p", mo.num), mo.num))
  
  write.csv(met.all, file.path(path.out, paste0("prism_met_NE_sites_long_", VAR,".csv")), row.names=F)
  
  # ---------------------
  # reshaping into wide format
  # ---------------------
  met.all$year <- as.ordered(met.all$year)
  # summary(met.all)
  
  met.wide <- reshape2::recast(met.all[,c("Site.Code", "year", "month", "value")], Site.Code+year~month)
  names(met.wide)[3:ncol(met.wide)] <- c(paste0("p", mo.names), mo.names) # Need to be non-numeric for R to be happy
  # summary(met.wide)
  
  write.csv(met.wide, file.path(path.out, paste0("prism_met_NE_sites_wide_", VAR,".csv")), row.names=F)
  # ---------------------
}
# ------------------------------------
