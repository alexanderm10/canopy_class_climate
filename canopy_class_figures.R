library(ggplot2); library(maps)


#################################################
# Figure 1
#################################################
# making a figure that has a map of the Sites used in teh New England area
# Pair this with stacked barplots of species and canopy classes at each site

# Barplots first
load("overstory_understory_combined_data_use.Rdata")
summary(test)

# Need to transform back into tree-level data
# Right now there is duplicate information for each tree in each year at each site

cc.tree.data <- data.frame(TreeID = unique(test$TreeID))
  
for(i in cc.tree.data$TreeID){
  cc.tree.data[cc.tree.data$TreeID==i, "Site"] <- unique(test[test$TreeID==i, "Site.Code"])
  cc.tree.data[cc.tree.data$TreeID==i, "Canopy.Class"] <- unique(test[test$TreeID==i, "Canopy.Class"])
  cc.tree.data[cc.tree.data$TreeID==i, "Species"] <- unique(test[test$TreeID==i, "Species"])
  cc.tree.data[cc.tree.data$TreeID==i, "DBH"] <- unique(test[test$TreeID==i, "DBH"])
}

summary(cc.tree.data)
pdf("figures/prelim_figures/site_spp_cc_breakdown.pdf ", width= 13, height = 8.5)
for(i in unique(cc.tree.data$Site)){
  
  print(ggplot(data=cc.tree.data[cc.tree.data$Site==i,]) +
          geom_bar(aes(x=Species, fill=Canopy.Class)) +
          labs(title=paste(i)) +
    theme(axis.line=element_line(color="black"), 
          panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank(), 
          panel.border=element_blank(),  
          panel.background=element_blank(), 
          axis.text.x=element_text(angle=0, color="black", size=16, vjust= 0.5), 
          axis.text.y=element_text(angle=0, color="black", size=16), 
          strip.text=element_text(face="bold", size=22),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          legend.position="top") +
    #legend.key.size = unit(0.75, "cm"),
    #legend.text = element_text(size=22),
    #legend.key = element_rect(fill = "white")) + 
    #guides(fill=guide_legend(nrow=1, title="")) +
    theme(axis.title.y= element_text(size=24, face="bold")) +
    theme(axis.title.x= element_text(size=24, face="bold")))
}
dev.off()

######################################################
# Making maps of sites
######################################################

## A slighly fancier map showing experimental & observational sites locations ##
# Christy Rollinson 
# 25 July 2016


## housekeeping
# rm(list=ls()) 

# Key libraries
library(ggplot2); library(maps)
# # library(rworldmap)

# Define our base directory
# Load in the locations for experimental and observational sites
cc.sites <- read.csv("processed_data/NE_site_locations.csv", header=T)

summary(cc.sites)

# -------------
dat.map <- data.frame(Site = cc.sites$site.name,
                      Site.Code = cc.sites$Site.Code,
                      Lat = cc.sites$latitude,
                      Lon = cc.sites$longitude)
summary(dat.map)

# Setting bounding box for mapping
lat.min <- 42
lat.max <- 47
lon.min <- -76
lon.max <- -70

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

png("figures/ITRDB.png", width=10, height=5, units="in", res=220)
ggplot(data=dat.map) +
  guides(fill="none") +
  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  geom_path(data=states,aes(x = Lon, y = Lat, group=group), color = "black", size=0.1) +
  geom_point(aes(x=Lon, y=Lat), color = "red", size=2.5, alpha=0.75) +
  geom_text(aes(x=Lon+0.25, y=Lat+0.25, label = paste("",as.character(Site.Code), sep="")), color="black", size=5,fontface="bold") +
  # scale_color_manual(values="red", name="Data Type") +
  theme_bw() +
  theme(legend.position="none") +
  scale_x_continuous(expand=c(0,0), name="Degrees Longitude", limits =c(lon.min,lon.max)) +
  scale_y_continuous(expand=c(0,0), name="Degrees Latitude", limits= c(lat.min,lat.max)) +
  coord_equal()


dev.off()
