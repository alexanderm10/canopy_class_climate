# setwd("~/Desktop/ITRDB Map/")
rm(list=ls())
library(ggplot2)
library(raster)
library(maps)

ggplot(data=usa) +
  geom_polygon(aes(x=long, y=lat)) + coord_equal()

nbcd <- raster("~/Dropbox/research/NBCD_countrywide_biomass_240m_raster/NBCD_countrywide_biomass_mosaic.tif")
nbcd

# Aggregating nbcd just to make things a little faster for testing
nbcd2 <- aggregate(nbcd, fac=8) # This should make it approx 1 km res.
nbcd2
plot(nbcd2)
# map("usa", add=T)

# Getting the USA map so we can mask data
# map('usa')
usa <- map_data("usa")
# usa <- map("usa", plot=F)
# summary(usa)
# 
usa2 <- SpatialPointsDataFrame(usa[,c("long", "lat")], data=usa, proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
usa2 <- spTransform(usa2, projection(nbcd2))
# mask(nbcd2)

# Making it a data frame
# plot(usa2, pch=16)

usa3 <- data.frame(usa2)
names(usa3)[names(usa3) %in% c("long.1", "lat.1")] <- c("x", "y")
summary(usa3)


png("NBCD_AEA_1km.png", height=8, width=10, units="in", res=120)
plot(nbcd2)
dev.off()

# Read in the ITRDB
load("processed_data/noaa_meta.Rdata")
itrdb <- noaa.meta
summary(itrdb)

# Read in expanded dataset
sites.neil <- read.csv("input_data/neil_ross_combo_update.csv")
summary(sites.neil)
sites.neil <- sites.neil[!sites.neil$Method %in% c("Targeted", "Climate", "Target"),]
sites.neil$Latitude <- sites.neil$LAT
sites.neil$Longitude <- sites.neil$LONG

# Make it a spatial points data frame
itrdb <- SpatialPointsDataFrame(itrdb[,c("Longitude", "Latitude")], data=itrdb, proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
plot(itrdb) # 

sites.neil <- SpatialPointsDataFrame(sites.neil[,c("Longitude", "Latitude")], data=sites.neil, proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Convert it to AEA projection to match itrdb
itrdb <- spTransform(itrdb, projection(nbcd))
plot(itrdb) # YES this will look funky

sites.neil <- spTransform(sites.neil, projection(nbcd))
plot(sites.neil) # YES this will look funky
plot(nbcd2)
points(itrdb, pch=16, cex=0.5)
points(sites.neil, pch=16, cex=0.5)

# Converting nbcd & itrdb back into normal data frames so we can use ggplot
itrdb.df <- data.frame(itrdb)
names(itrdb.df)[names(itrdb.df) %in% c("Longitude.1", "Latitude.1")] <- c("x", "y")
summary(itrdb.df)

neil.df <- data.frame(sites.neil)
names(neil.df)[names(neil.df) %in% c("Longitude.1", "Latitude.1")] <- c("x", "y")
summary(neil.df)


nbcd.df <- data.frame(coordinates(nbcd2))
nbcd.df$Biomass <- as.data.frame(nbcd2)[,1]
summary(nbcd.df)
dim(nbcd.df)



# Crop itrdb.df to what's in the nbcd
itrdb.df <- itrdb.df[itrdb.df$x>=min(nbcd.df$x) & itrdb.df$x<=max(nbcd.df$x) &
                 itrdb.df$y>=min(nbcd.df$y) & itrdb.df$y<=max(nbcd.df$y),]
summary(itrdb.df)
itrdb.df <- itrdb.df[,c("species.code", "x", "y")]
names(itrdb.df) <- c("species", "x", "y")
itrdb.df$type <- as.factor("ITRDB")

neil.df <- neil.df[neil.df$x>=min(nbcd.df$x) & neil.df$x<=max(nbcd.df$x) &
                       neil.df$y>=min(nbcd.df$y) & neil.df$y<=max(nbcd.df$y),]
summary(neil.df)
neil.df <- neil.df[,c("species", "x", "y")]
neil.df$type <- as.factor("Expanded")
# usa <- map_data("usa")

combo.df <- rbind(itrdb.df, neil.df)

png("figures/collections_biomass_map.png", height=5, width=5, units="in", res=300)
ggplot(data=nbcd.df[nbcd.df$Biomass>0,]) +
  coord_equal() +
  geom_polygon(data=usa3, aes(x=x, y=y, group=group), fill="white") +
  geom_raster(aes(x=x, y=y, fill=Biomass)) +
  guides(fill=F) +
  # geom_path(data=usa3, aes(x=x, y=y, group=group)) +
  geom_point(data=combo.df, aes(x=x, y=y, color=type), size=2, alpha=0.8) +
 # geom_point(data=neil.df, aes(x=x, y=y), size=2, alpha=0.6) +
  scale_color_manual(values = c("#E69F00","#0072B2"), guide = "legend", name="Network") +
  #annotate("text", x=-1800000, y=-1300000, label="© Christine Rollinson 2016", color="white", size=8) +
  scale_fill_gradientn(colors=c("darkolivegreen2", "darkolivegreen4", "darkolivegreen", "darkgreen", "red")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=4))) +
  theme_bw() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major=element_blank(),
        panel.border=element_blank(),
        panel.grid.minor=element_blank())+
  theme(legend.position="top", legend.text= element_text(size=22),legend.title = element_text(size = 22, face = "bold")) +
  scale_x_continuous(limits=range(itrdb.df$x, na.rm=T) + c(-100000, 100000),expand=c(0,0) )+ 
  scale_y_continuous(limits=range(itrdb.df$y, na.rm=T) + c(-500000,100000), expand=c(0,0))
        #panel.background=element_rect(fill="black"),
        #plot.background=element_rect(fill="black"))
dev.off()

png("ITRDB_NBCD_1km_small.png", height=6, width=9.5, units="in", res=140)
ggplot(data=nbcd.df[nbcd.df$Biomass>0,]) +
  coord_equal() +
  geom_polygon(data=usa3, aes(x=x, y=y, group=group), fill="white") +
  geom_raster(aes(x=x, y=y, fill=Biomass)) +
  # geom_path(data=usa3, aes(x=x, y=y, group=group)) +
  geom_point(data=itrdb.df, aes(x=x, y=y), size=1.3, color="black") +
  annotate("text", x=-1800000, y=-1300000, label="© Christine Rollinson 2016", color="white", size=6) +
  scale_fill_gradientn(colors=c("darkolivegreen2", "darkolivegreen4", "darkolivegreen", "darkgreen", "red")) +
  guides(fill=F) +
  theme_bw() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major=element_blank(),
        panel.border=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_rect(fill="black"),
        plot.background=element_rect(fill="black"))
dev.off()
