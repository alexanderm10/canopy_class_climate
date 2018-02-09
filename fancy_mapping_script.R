
## A slighly fancier map showing experimental & observational sites locations ##
# Christy Rollinson 
# 25 July 2016


## housekeeping
rm(list=ls()) 

# Key libraries
library(ggplot2); library(maps)
# # library(rworldmap)

# Define our base directory
# Load in the locations for experimental and observational sites
load("processed_data/east_ITRDB.Rdata")
sites.ITRDB <- itrdb.metadata
#sites.obs <- read.csv("../obssiteinfo.csv")
summary(sites.ITRDB)
sites.ITRDB[,c("ITRDB.id", "lat", "lon")]

# Trimming to just the sites for ch. 2
#ch2.sites.exp <- sites.exp[sites.exp$site.name%in% c("Harvard Forest", "Howland Forest", "Morgan Monroe State Forest","Missouri Ozark","Ohio Oak Openings"),]
#ch2.sites.exp$state<- recode(ch2.sites.exp$site.name, "'Harvard Forest'='MA';'Howland Forest'='ME';'Morgan Monroe State Forest'='IN';'Missouri Ozark'='MO';'Ohio Oak Openings'='OH'")
#sites.obs[,c("Site.code", "Lat", "Long")]

#itrdb.map <- sites.exp[!sites.exp$site.name %in% c("Flagstaff control site", "Flagstaf managed forest", "Wind River Crane Site"),]

# -------------
# Note that in Sierra Nevadas, there appear to be two sites & these must be two separate rows
# -------------
# siernev <- sites.obs[sites.obs$Site.cod=="siernev", ]
# sn.lat <- as.numeric(strsplit(paste(siernev[, "Lat"]), split=",")[[1]])
# sn.lon <- as.numeric(strsplit(paste(siernev[, "Long"]), split=",")[[1]])
# 
# cols <- 1:ncol(siernev)
# cols.excl <- which(names(siernev) %in% c("Lat", "Long"))
# cols[!cols %in% cols.excl]
# siernev <- siernev[,cols[!cols %in% cols.excl] ]
# 
# siernev <- merge(siernev, data.frame(Lat=sn.lat, Long=sn.lon), all=T)
# siernev$Site.code <- c(paste0("siernev", 1:nrow(siernev)))
# 
# # Merging sierra nevadas into the origin site.obs file
# #  -- Note: need to first drop siernev & then convert lat/lon to numeric
# sites.obs <- sites.obs[sites.obs$Site.code!="siernev", ]
# sites.obs$Lat <- as.numeric(paste(sites.obs$Lat))
# sites.obs$Long <- as.numeric(paste(sites.obs$Long))
# summary(sites.obs)
# sites.obs <- merge(sites.obs, siernev, all=T)
# summary(sites.obs)
# -------------

ch2.sites.exp[,c("site.name", "lat", "long")]
summary(ch2.sites.exp[,c("site.name", "lat", "long")])
#summary(sites.obs[,c("Site.code", "Lat", "Long")])

#sites.obs[,c("Site.code", "Site", "Lat", "Long")]


dat.map <- data.frame(Spp= c(paste(sites.ITRDB$species)),
                      Lat = c(sites.ITRDB$lat),
                      Lon = c(sites.ITRDB$lon),
                      ID = sites.ITRDB$ID,
                      type= c(rep("ITRDB", nrow(sites.ITRDB))))
summary(dat.map)

# Setting bounding box for mapping
lat.min <- 25.7617
lat.max <- 46.8640
lon.min <- -96.4003
lon.max <- -67.9980

library(raster)
# 10m land cover from Natural Earth http://www.naturalearthdata.com/downloads/10m-raster-data/10m-natural-earth-1/
nat.earth <- stack("~malexander10/Dropbox/Research/mapping_data/base_layers/NE1_HR_LC_SR_W_DR/NE1_HR_LC_SR_W_DR.tif")

# USFS forest cover http://www.mrlc.gov/nlcd11_data.php
#nat.earth <- stack("base_layers/CONUSCartographic_2_8_16/Cartographic/nlcd2011_usfs_conus_canopy_cartographic.img")
nat.crop <- crop(nat.earth, y=c(lon.min-5, lon.max+5, lat.min-5, lat.max+5))
nat.crop <- aggregate(nat.crop, fact=2, fun=mean)


rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))

rast.table$rgb <- with(rast.table, rgb(NE1_HR_LC_SR_W_DR.1,
                                       NE1_HR_LC_SR_W_DR.2,
                                       NE1_HR_LC_SR_W_DR.3,
                                       1))

states <- map_data("state")
names(states)
summary(states)
names(states) <- c("Lon", "Lat", paste(names(states[,3:ncol(states)])))

# states.crop <- states[states$Lon %in% range(dat.map$Lon) & states$Lat %in% range(dat.map$Lat),]
dim(states)
# Note: the natural earth data takes quite a while to plot!`

# png("figures/ITRDB.png", width=10, height=5, units="in", res=220)
ggplot(data=dat.map) +
  guides(fill="none") +
  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  geom_path(data=states,aes(x = Lon, y = Lat, group=group), color = "black", size=0.1) +
  geom_point(aes(x=Lon, y=Lat, color=type), size=2.5, alpha=0.75) +
  #geom_text(data=ch2.sites.exp,aes(x=long+0.75, y=lat+0.75, label = paste("",as.character(state), sep="")), color="black", size=5,fontface="bold") +
  scale_color_manual(values="red", name="Data Type") +
  theme_bw() +
  theme(legend.position="none") +
  scale_x_continuous(expand=c(0,0), name="Degrees Longitude", limits =range(rast.table$x)) +
  scale_y_continuous(expand=c(0,0), name="Degrees Latitude", limits=range(rast.table$y)) +
  coord_equal()


dev.off()

#######################################################
# Making map of neil's expanded network
#######################################################
sites.neil <- read.csv("input_data/neil_ross_combo_update.csv")
summary(sites.neil)

# Removing the climate targeted sampling
sites.neil <- sites.neil[!sites.neil$Method %in% c("Targeted", "Climate", "Target"),]
unique(sites.neil$Method)
dat.map.neil <- data.frame(Spp= c(paste(sites.neil$species)),
                      Lat = c(sites.neil$LAT),
                      Lon = c(sites.neil$LONG),
                      type= c(rep("Expanded", nrow(sites.neil)))
)
summary(dat.map.neil)

ggplot(data=dat.map.neil) +
  guides(fill="none") +
  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  geom_path(data=states,aes(x = Lon, y = Lat, group=group), color = "black", size=0.1) +
  geom_point(aes(x=Lon, y=Lat, color=type), size=2.5, alpha=0.75) +
  #geom_text(data=ch2.sites.exp,aes(x=long+0.75, y=lat+0.75, label = paste("",as.character(state), sep="")), color="black", size=5,fontface="bold") +
  scale_color_manual(values="red", name="Data Type") +
  theme_bw() +
  theme(legend.position="none") +
  scale_x_continuous(expand=c(0,0), name="Degrees Longitude", limits =range(rast.table$x)) +
  scale_y_continuous(expand=c(0,0), name="Degrees Latitude", limits=range(rast.table$y)) +
  coord_equal()



# Combining maps together together
dat.map.all <- rbind(dat.map, dat.map.neil)

png("figures/grant_map.png", width=10, height=5, units="in", res=300)
ggplot(data=dat.map.all) +
  guides(fill="none") +
  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  geom_path(data=states,aes(x = Lon, y = Lat, group=group), color = "black", size=0.1) +
  geom_point(aes(x=Lon, y=Lat, color=type), size=2.5, alpha=0.75) +
  #geom_text(data=ch2.sites.exp,aes(x=long+0.75, y=lat+0.75, label = paste("",as.character(state), sep="")), color="black", size=5,fontface="bold") +
  scale_color_manual(values= c("red", "blue"), name="Network") +
  theme_bw() +
  theme(legend.position="top") +
  scale_x_continuous(expand=c(0,0), name="Degrees Longitude", limits =range(rast.table$x)) +
  scale_y_continuous(expand=c(0,0), name="Degrees Latitude", limits=range(rast.table$y)) +
  coord_equal()
dev.off()



################################
# Tryign to map the european sites
################################

library(ggplot2)
library(car)
require(plyr)
require(ggplot2)
require(RColorBrewer)
require(reshape)
require(scales)
require(zoo)
require(gridExtra)
require(grid)

load("itrdb_europe.Rdata")

# Setting bounding box for mapping
lat.min <- 34
lat.max <- 70
lon.min <- -9
lon.max <- 32

library(raster)
# 10m land cover from Natural Earth http://www.naturalearthdata.com/downloads/10m-raster-data/10m-natural-earth-1/
nat.earth <- stack("~malexander10/Dropbox/Research/mapping_data/base_layers/NE1_HR_LC_SR_W_DR/NE1_HR_LC_SR_W_DR.tif")

# USFS forest cover http://www.mrlc.gov/nlcd11_data.php
#nat.earth <- stack("base_layers/CONUSCartographic_2_8_16/Cartographic/nlcd2011_usfs_conus_canopy_cartographic.img")
nat.crop <- crop(nat.earth, y=c(lon.min-5, lon.max+5, lat.min-5, lat.max+5))
nat.crop <- aggregate(nat.crop, fact=2, fun=mean)


rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))

rast.table$rgb <- with(rast.table, rgb(NE1_HR_LC_SR_W_DR.1,
                                       NE1_HR_LC_SR_W_DR.2,
                                       NE1_HR_LC_SR_W_DR.3,
                                       1))


worldmap2 <- map_data("world")
worldmap2$region <- as.factor(worldmap2$region)

europe.sites <- read.csv("input_data/FunDiv_european_sites.csv")
load("europe_outlines.Rdata")

summary(itrdb.out)
summary(europe.sites)
europe.sites$type <- as.factor("Expanded")

itrdb.out2 <- itrdb.out[,c("studyCode", "Latitude", "Longitude")]
names(itrdb.out2) <- c("site", "lat", "lon")

itrdb.out2$type <- as.factor("ITRDB")
europe.combo <- rbind(itrdb.out2,europe.sites)

europe.map <- ggplot(data=europe.combo) +
                  guides(fill="none") +
                  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
                  #geom_path(data=europe,aes(x = Lon, y = Lat, group=group), color = "black", size=0.1) +
                  geom_point(aes(x=lon, y=lat, color=type, size = type)) +
                  scale_color_manual(values = c("#E69F00","#0072B2"), guide = "legend", name="Network") +
                  scale_size_manual(values = c(1, 2.5)) +
                  guides(colour = guide_legend(override.aes = list(alpha = 1, size=4))) +                
                  guides(size=F) +
                  #scale_color_manual(values="red", name="Data Type") +
                  theme_bw() +
                  theme(axis.title=element_blank(),
                        axis.text=element_blank(),
                        axis.ticks=element_blank(),
                        panel.grid.major=element_blank(),
                        panel.border=element_blank(),
                        panel.grid.minor=element_blank())+
                  theme(legend.position="none", legend.text= element_text(size=22),legend.title = element_text(size = 22, face = "bold")) +
                                  coord_cartesian(xlim=range(europe.sites$lon), ylim=range(europe.sites$lat)) + coord_equal()+
                                  scale_x_continuous(limits=range(europe.sites$lon, na.rm=T) + c(-2, 2),expand=c(0,0),name="Degrees Longitude")+ 
                                  scale_y_continuous(limits=range(europe.sites$lat, na.rm=T) + c(-2,2), expand=c(0,0), name="Degrees Latitude")
                 

png("figures/grant_europe_map.png", height=5, width=5, units="in", res=300)
europe.map
dev.off()