# Running EOF analysis on the eastern portion of the network
library(dplR)
library(car)
library(reshape)
library(ggplot2)
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(maps)
library(rworldmap)
library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(plyr)
library(sp)
library(geosphere)
library(stringr)

# Loading in site data
sites.exp <- read.csv("~/PhD/Carbon Research/Calloc_generalscripts/DOE_sites_ross_sampled.csv")

summary(sites.exp)
sites.exp[,c("site.name", "lat", "long")]
# Trimming to just the sites for ch. 2
ch2.sites.exp <- sites.exp[sites.exp$site.name%in% c("Harvard Forest", "Howland Forest", "Morgan Monroe State Forest","Missouri Ozark","Ohio Oak Openings"),]
ch2.sites.exp$state<- recode(ch2.sites.exp$site.name, "'Harvard Forest'='MA';'Howland Forest'='ME';'Morgan Monroe State Forest'='IN';'Missouri Ozark'='MO';'Ohio Oak Openings'='OH'")
#sites.obs[,c("Site.code", "Lat", "Long")]


ch2.sites.exp[,c("site.name", "lat", "long")]
summary(ch2.sites.exp[,c("site.name", "lat", "long")])
#summary(sites.obs[,c("Site.code", "Lat", "Long")])

#sites.obs[,c("Site.code", "Site", "Lat", "Long")]


dat.map <- data.frame(Site= c(ch2.sites.exp$site.name),
                      Lat = c(ch2.sites.exp$lat),
                      Lon = c(ch2.sites.exp$long),
                      type= c(rep("Experiment", nrow(ch2.sites.exp)))
)
summary(dat.map)


# Making some pretty maps to see how temp and precip vary across space
load("~/PhD/Carbon Research/ch3_spatial_analysis/processed_data/annual_tmean_2d.Rdata")
load("~/PhD/Carbon Research/ch3_spatial_analysis/processed_data/grow_seas_tmean_2d.Rdata")
load("~/PhD/Carbon Research/ch3_spatial_analysis/processed_data/annual_ppt_2d.Rdata")
load("~/PhD/Carbon Research/ch3_spatial_analysis/processed_data/grow_seas_ppt_2d.Rdata")

# loading in the original PRISM dataframe so I can pull the lat lon
load("processed_data/PRISM_tmean_annual_field.Rdata")

lat.m <- dimnames(tmean.yr)[[1]]
lon.m <- dimnames(tmean.yr)[[2]]


# Mean annual temperature
head(tmean.yr.2d[,1:10])
tmean.yr.short <- tmean.yr.2d[row.names(tmean.yr.2d) >= 1950 & row.names(tmean.yr.2d) <= 2012,] 

tmean.yr.mean <- as.data.frame(apply(tmean.yr.short, 2, FUN=mean, na.rm=T))

summary(tmean.yr.mean)
names(tmean.yr.mean) <- "mat"
tmean.yr.mean$lat <- as.numeric(rep(lat.m, each=length(lon.m)))
tmean.yr.mean$lon <-  as.numeric(paste(lon.m))



# Plotting out MAT

worldmap2 <- map_data("world")
worldmap2$region <- as.factor(worldmap2$region)

ggplot(data=tmean.yr.mean) + 
  geom_raster(aes(x = lon, y = lat, fill = mat),interpolate = TRUE) +
  scale_fill_gradientn(colours = rev(rainbow(10)), na.value = NA) +
  geom_point(data=dat.map, aes(x=Lon, y=Lat), color="red", size=2.5, alpha=0.75) +
  geom_text(data=ch2.sites.exp,aes(x=long+0.65, y=lat+0.65, label = paste("",as.character(state), sep="")), color="white", size=5,fontface="bold") +
  labs(title="Mean Annual Temperature (1950-2012)", x = "Latitude", y= "Longitude") +
  geom_path(data=worldmap2[worldmap2$region %in%c("USA"),],aes(x=long, y=lat, group=group), color="white")+
  theme(panel.background = element_rect(fill = "black"), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.x = element_blank(), 
        legend.title=element_blank(),axis.text.y = element_blank()) +
  coord_cartesian(xlim=range(tmean.yr.mean$lon), ylim=range(tmean.yr.mean$lat)) + coord_equal()+
  scale_x_continuous(limits=range(tmean.yr.mean$lon) ,expand=c(0,0) )+ 
  scale_y_continuous(limits=range(tmean.yr.mean$lat) + c(-5,5), expand=c(0,0))


# Growing season temperature
# Mean annual temperature
head(tmean.gs.2d[,1:10])
tmean.gs.short <- tmean.gs.2d[row.names(tmean.gs.2d) >= 1950 & row.names(tmean.gs.2d) <= 2012,] 

tmean.gs.mean <- as.data.frame(apply(tmean.gs.short, 2, FUN=mean, na.rm=T))

summary(tmean.gs.mean)
names(tmean.gs.mean) <- "gs.temp"
tmean.gs.mean$lat <- as.numeric(rep(lat.m, each=length(lon.m)))
tmean.gs.mean$lon <-  as.numeric(paste(lon.m))



# Plotting out GS temp

worldmap2 <- map_data("world")
worldmap2$region <- as.factor(worldmap2$region)

ggplot(data=tmean.gs.mean) + 
  geom_raster(aes(x = lon, y = lat, fill = gs.temp),interpolate = TRUE) +
  scale_fill_gradientn(colours = rev(rainbow(10)), na.value = NA) +
  geom_point(data=dat.map, aes(x=Lon, y=Lat), color="red", size=2.5, alpha=0.75) +
  geom_text(data=ch2.sites.exp,aes(x=long+0.65, y=lat+0.65, label = paste("",as.character(state), sep="")), color="white", size=5,fontface="bold") +
  labs(title="Mean GS Temperature (1950-2012)", x = "Latitude", y= "Longitude") +
  geom_path(data=worldmap2[worldmap2$region %in%c("USA"),],aes(x=long, y=lat, group=group), color="white")+
  theme(panel.background = element_rect(fill = "black"), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.x = element_blank(), 
        legend.title=element_blank(),axis.text.y = element_blank()) +
  coord_cartesian(xlim=range(tmean.gs.mean$lon), ylim=range(tmean.gs.mean$lat)) + coord_equal()+
  scale_x_continuous(limits=range(tmean.gs.mean$lon) ,expand=c(0,0) )+ 
  scale_y_continuous(limits=range(tmean.gs.mean$lat) + c(-5,5), expand=c(0,0))


#################################################################
# Precipitation
##################################################
# Mean annual Precip
head(ppt.yr.2d[,1:10])
ppt.yr.short <- ppt.yr.2d[row.names(ppt.yr.2d) >= 1950 & row.names(ppt.yr.2d) <= 2012,] 

ppt.yr.mean <- as.data.frame(apply(ppt.yr.short, 2, FUN=mean, na.rm=T))

summary(ppt.yr.mean)
names(ppt.yr.mean) <- "map"
ppt.yr.mean$lat <- as.numeric(rep(lat.m, each=length(lon.m)))
ppt.yr.mean$lon <-  as.numeric(paste(lon.m))



# Plotting out MAP

worldmap2 <- map_data("world")
worldmap2$region <- as.factor(worldmap2$region)

ggplot(data=ppt.yr.mean) + 
  geom_raster(aes(x = lon, y = lat, fill = map),interpolate = TRUE) +
  scale_fill_gradient2(low="brown", high="darkgreen",na.value = NA) +
  geom_point(data=dat.map, aes(x=Lon, y=Lat), color="red", size=2.5, alpha=0.75) +
  geom_text(data=ch2.sites.exp,aes(x=long+0.65, y=lat+0.65, label = paste("",as.character(state), sep="")), color="white", size=5,fontface="bold") +
  labs(title="Mean Annual Precipitation (1950-2012)", x = "Latitude", y= "Longitude") +
  geom_path(data=worldmap2[worldmap2$region %in%c("USA"),],aes(x=long, y=lat, group=group), color="white")+
  theme(panel.background = element_rect(fill = "black"), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.x = element_blank(), 
        legend.title=element_blank(),axis.text.y = element_blank()) +
  coord_cartesian(xlim=range(ppt.yr.mean$lon), ylim=range(ppt.yr.mean$lat)) + coord_equal()+
  scale_x_continuous(limits=range(ppt.yr.mean$lon) ,expand=c(0,0) )+ 
  scale_y_continuous(limits=range(ppt.yr.mean$lat) + c(-5,5), expand=c(0,0))
  
  


# Growing season precip

head(ppt.gs.2d[,1:10])
ppt.gs.short <- ppt.gs.2d[row.names(ppt.gs.2d) >= 1950 & row.names(ppt.gs.2d) <= 2012,] 

ppt.gs.mean <- as.data.frame(apply(ppt.gs.short, 2, FUN=mean, na.rm=T))

summary(ppt.gs.mean)
names(ppt.gs.mean) <- "gs.ppt"
ppt.gs.mean$lat <- as.numeric(rep(lat.m, each=length(lon.m)))
ppt.gs.mean$lon <-  as.numeric(paste(lon.m))



# Plotting out GS temp

worldmap2 <- map_data("world")
worldmap2$region <- as.factor(worldmap2$region)

ggplot(data=ppt.gs.mean) + 
  geom_raster(aes(x = lon, y = lat, fill = gs.ppt),interpolate = TRUE) +
  scale_fill_gradient2(low="brown", high="darkgreen",na.value = NA) +
  geom_point(data=dat.map, aes(x=Lon, y=Lat), color="red", size=2.5, alpha=0.75) +
  geom_text(data=ch2.sites.exp,aes(x=long+0.65, y=lat+0.65, label = paste("",as.character(state), sep="")), color="white", size=5,fontface="bold") +
  labs(title="Mean GS Precip (1950-2012)", x = "Latitude", y= "Longitude") +
  geom_path(data=worldmap2[worldmap2$region %in%c("USA"),],aes(x=long, y=lat, group=group), color="white")+
  theme(panel.background = element_rect(fill = "black"), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.ticks = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.x = element_blank(), 
        legend.title=element_blank(),axis.text.y = element_blank()) +
  coord_cartesian(xlim=range(ppt.gs.mean$lon), ylim=range(ppt.gs.mean$lat)) + coord_equal()+
  scale_x_continuous(limits=range(ppt.gs.mean$lon) ,expand=c(0,0) )+ 
  scale_y_continuous(limits=range(ppt.gs.mean$lat) + c(-5,5), expand=c(0,0))