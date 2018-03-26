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


# Read in cc data
cc.sites <- as.data.frame(read.csv("processed_data/NE_site_locations.csv", header=T))


cc.sites$Latitude <- cc.sites$latitude
cc.sites$Longitude <- cc.sites$longitude

# Make it a spatial points data frame
cc.sites <- SpatialPointsDataFrame(cc.sites[,c("Longitude", "Latitude")], data=cc.sites, proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Convert it to AEA projection to match itrdb
cc.sites <- spTransform(cc.sites, projection(nbcd))
plot(cc.sites) # YES this will look funky
plot(nbcd2)

points(cc.sites, pch=16, cex=0.5)

# Converting nbcd & itrdb back into normal data frames so we can use ggplot

cc.df <- data.frame(cc.sites)
names(cc.df)[names(cc.df) %in% c("Longitude.1", "Latitude.1")] <- c("x", "y")
summary(cc.df)


nbcd.df <- data.frame(coordinates(nbcd2))
nbcd.df$Biomass <- as.data.frame(nbcd2)[,1]
summary(nbcd.df)
dim(nbcd.df)



# Crop itrdb.df to what's in the nbcd

cc.df <- cc.df[cc.df$x>=min(nbcd.df$x) & cc.df$x<=max(nbcd.df$x) &
                 cc.df$y>=min(nbcd.df$y) & cc.df$y<=max(nbcd.df$y),]
summary(cc.df)
cc.df <- cc.df[,c("x", "y")]
cc.df$Site.Code <- cc.sites$Site.Code
# usa <- map_data("usa")


png("figures/canopy_class_biomass_map.png", height=5, width=5, units="in", res=300)
cc.bm <- ggplot(data=nbcd.df[nbcd.df$Biomass>0,]) +
                coord_equal() +
                geom_polygon(data=usa3, aes(x=x, y=y, group=group), fill="white") +
                geom_raster(aes(x=x, y=y, fill=Biomass)) +
                guides(fill=F) +
                # geom_path(data=usa3, aes(x=x, y=y, group=group)) +
                geom_point(data=cc.df, aes(x=x, y=y, color="#0072B2"), size=5, alpha=0.8) +
               # geom_point(data=neil.df, aes(x=x, y=y), size=2, alpha=0.6) +
                #scale_color_manual(values = c("#E69F00","#0072B2"), guide = NULL, name="Network") +
                #annotate("text", x=-1800000, y=-1300000, label="© Christine Rollinson 2016", color="white", size=8) +
                scale_fill_gradientn(colors=c("darkolivegreen2", "darkolivegreen4", "darkolivegreen", "darkgreen", "red")) +
                guides(colour = guide_legend(override.aes = list(alpha = 1, size=4))) +
                geom_text(data = cc.df[!cc.df$Site.Code %in% c("NR", "HF", "LF"),], aes(x=x+30000, y=y+35000, label = paste("",as.character(Site.Code), sep="")), color="black", size=10,fontface="bold") +
                geom_text(data = cc.df[cc.df$Site.Code %in% "NR",], aes(x=x-30000, y=y-40000, label = paste("",as.character(Site.Code), sep="")), color="black", size=10,fontface="bold") +
                geom_text(data = cc.df[cc.df$Site.Code %in% "HF",], aes(x=x+30000, y=y-40000, label = paste("",as.character(Site.Code), sep="")), color="black", size=10,fontface="bold") +
                geom_text(data = cc.df[cc.df$Site.Code %in% "LF",], aes(x=x+30000, y=y+25000, label = paste("",as.character(Site.Code), sep="")), color="black", size=10,fontface="bold") +
                theme_bw() +
                theme(axis.title=element_blank(),
                      axis.text=element_blank(),
                      axis.ticks=element_blank(),
                      panel.grid.major=element_blank(),
                      panel.border=element_blank(),
                      panel.grid.minor=element_blank())+
                theme(legend.position="none", legend.text= element_text(size=22),legend.title = element_text(size = 22, face = "bold")) +
                scale_x_continuous(limits=range(cc.df$x, na.rm=T) + c(-300000, 200000),expand=c(0,0) )+ 
                scale_y_continuous(limits=range(cc.df$y, na.rm=T) + c(-200000,400000), expand=c(0,0))
                      #panel.background=element_rect(fill="black"),
                      #plot.background=element_rect(fill="black"))
dev.off()
