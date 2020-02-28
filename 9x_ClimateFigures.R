library(ggplot2); library(maps)
library(car)
library(gridExtra)
library(grid)

path.google <- "/Volumes/GoogleDrive/My Drive/Manuscripts/Alexander_CanopyClimateResponse/canopy_and_climate/manuscript/Ecology (submit 2019-10)/Revision 1 2019-12"
dir.figs <- file.path(path.google, "figures")
dir.create(dir.figs, recursive = T, showWarnings = F)

#################################################
# Figure 1
# Map and canopy break down
#################################################
# making a figure that has a map of the Sites used in teh New England area
# Pair this with stacked barplots of species and canopy classes at each site

# Barplots first
load("processed_data/overstory_understory_combined_data_use.Rdata")
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
cc.tree.data$Canopy.Class <- recode(cc.tree.data$Canopy.Class, "'Canopy'='Overstory'; 'I'='Middle'; 'U'='Understory'")

cc.tree.data.all <- cc.tree.data
cc.tree.data.all$Site <- as.factor("All") 

cc.tree.data2 <- rbind(cc.tree.data, cc.tree.data.all)
summary(cc.tree.data2)

cc.tree.data2$Canopy.Class <- factor(cc.tree.data2$Canopy.Class, levels=c("Overstory", "Middle", "Understory"))
cc.tree.data2$Species <- factor(cc.tree.data2$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))



# Pulling out outlier datapoint from the analysis
# HUGE QURU labeled as suppressed
# Pulling LF2029

cc.tree.data2 <- cc.tree.data2[!cc.tree.data2$TreeID=="LF2029",]

write.csv(cc.tree.data2, "processed_data/canopy_class_tree_data.csv", row.names=F)

for(i in unique(cc.tree.data$Site)){
  
  print(ggplot(data=cc.tree.data[cc.tree.data$Site==i,]) +
          geom_bar(aes(x=Species, fill=Canopy.Class)) +
          labs(title=paste(i)) +
          scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2")) +
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


pdf("figures/site_spp_cc_breakdown.pdf ", width= 13, height = 8.5)
ggplot(data=cc.tree.data2) + facet_wrap(~Site) +
  geom_bar(aes(x=Species, fill=Canopy.Class)) +
  scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2")) +
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
  theme(axis.title.x= element_text(size=24, face="bold"))
dev.off()


fig1b <- ggplot(data=cc.tree.data2[cc.tree.data2$Site %in% "All",]) + 
  facet_grid(Species~.) +
  geom_histogram(aes(x=DBH, fill=Canopy.Class), binwidth = 5) +
  labs(x="DBH (cm)", y="Count") +
  scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"), guide = guide_legend(title = "")) +
  scale_y_continuous(expand=c(0,0), limits=c(0,87), breaks=seq(0, 75, by=25)) +
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=10, vjust= 0.5), 
        axis.text.y=element_text(angle=0, color="black", size=10), 
        strip.text=element_text(face="bold", size=10),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.text = element_text(size=10),
        legend.key.size = unit(0.75, "lines")) +
  theme(axis.title.y= element_text(size=12, face="bold")) +
  theme(axis.title.x= element_text(size=12, face="bold")) +
  theme(panel.spacing.y = unit(0.25,"lines"),
        plot.margin = unit(c(1, 0.5, 0.5, 1), "lines"))

pdf("figures/Fig1b.pdf ", width= 13, height = 8.5)
fig1b
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
# nat.earth <- "~/Dropbox/canopy_class_climate/map"
# nat.earth <- stack("~malexander10/Dropbox/Research/mapping_data/base_layers/NE1_HR_LC_SR_W_DR/NE1_HR_LC_SR_W_DR.tif")
nat.earth <- stack("~/Desktop/SpatialData/NaturalEarth/NE1_HR_LC_SR_W_DR/NE1_HR_LC_SR_W_DR.tif")

# USFS forest cover http://www.mrlc.gov/nlcd11_data.php
#nat.earth <- stack("base_layers/CONUSCartographic_2_8_16/Cartographic/nlcd2011_usfs_conus_canopy_cartographic.img")
nat.crop <- crop(nat.earth, y=c(lon.min-5, lon.max+5, lat.min-5, lat.max+5))
#nat.crop <- aggregate(nat.crop, fact=2, fun=mean)


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


fig1a <- ggplot(data=dat.map) +
  guides(fill="none") +
  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  geom_path(data=states,aes(x = Lon, y = Lat, group=group), color = "black", size=0.1) +
  geom_point(aes(x=Lon, y=Lat), color = "red", size=2.5, alpha=0.75) +
  geom_text(data = dat.map[!dat.map$Site.Code %in% c("NR", "HF"),], aes(x=Lon+0.25, y=Lat+0.25, label = paste("",as.character(Site.Code), sep="")), color="black", size=3,fontface="bold") +
  geom_text(data = dat.map[dat.map$Site.Code %in% "NR",], aes(x=Lon-0.25, y=Lat-0.25, label = paste("",as.character(Site.Code), sep="")), color="black", size=3,fontface="bold") +
  geom_text(data = dat.map[dat.map$Site.Code %in% "HF",], aes(x=Lon-0.25, y=Lat-0.25, label = paste("",as.character(Site.Code), sep="")), color="black", size=3,fontface="bold") +
  # scale_color_manual(values="red", name="Data Type") +
  theme_bw() +
  theme(legend.position="none",
        axis.text.x=element_text(angle=0, color="black", size=8, vjust= 0.5), 
        axis.text.y=element_text(angle=0, color="black", size=8)) +
  scale_x_continuous(expand=c(0,0), name="Degrees Longitude", limits =c(lon.min - 0.25,lon.max +2.25)) +
  scale_y_continuous(expand=c(0,0), name="Degrees Latitude", limits= c(lat.min - 1,lat.max + 1)) +
  coord_equal() +
  theme(axis.title.y= element_text(size=10, face="bold"),
        axis.title.x= element_text(size=10, face="bold"),
        plot.margin = unit(c(1, 0.5, 0.5, 1), "lines"))

png("figures/fig1a.png", width= 13, height = 8.5, unit="in", res = 300)
fig1a
dev.off()

# png("figures/pub_figs/Figure1.png", width= 13, height = 8.5, unit="in", res = 300)
# grid.newpage()
# pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2, widths=c(1.3,1,2))))
# print(fig1a, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
# print(fig1b, vp = viewport(layout.pos.row = 1, layout.pos.col=2))	
# 
# dev.off()

library(cowplot)
tiff(file=file.path(dir.figs, "Figure1.tiff"), width=3, height=6, res=600, unit="in")
cowplot::plot_grid(fig1a, fig1b, align = c("h"), ncol = 1, rel_heights=c(0.75, 1), labels = c("A)", "B)"))
dev.off()

pdf(file=file.path(dir.figs, "Figure1.pdf"), width=3, height=6)
cowplot::plot_grid(fig1a, fig1b, align = c("h"), ncol = 1, rel_heights=c(0.75, 1), labels = c("A)", "B)"))
dev.off()


# png("figures/pub_figs/Figure1_presentation.png", width= 15, height = 9, unit="in", res = 300)
# grid.newpage()
# pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2, widths=c(1.3,1,2))))
# print(fig1a, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
# print(fig1b, vp = viewport(layout.pos.row = 1, layout.pos.col=2))	
# 
# dev.off()


######################################################
# Figure 2
# PDF's of climate space
######################################################
data.use <- read.csv("processed_data/NESites_tree_plus_climate_and_BA.csv", header=T)
summary(data.use)

cc.tmean.data <- data.frame(Year = 1895:2015)
cc.precip.data <- data.frame(Year = 1895:2015)
cc.vpdmax.data <- data.frame(Year = 1895:2015)
for(i in unique(data.use$Site.Code)){
  for(t in cc.tmean.data$Year){
    if(length(which(data.use$Site.Code==i & data.use$Year==t))==0) next
    cc.tmean.data[cc.tmean.data$Year==t,i] <- unique(data.use[data.use$Site.Code==i & data.use$Year==t , "tmean"])
    cc.precip.data[cc.precip.data$Year==t,i] <- unique(data.use[data.use$Site.Code==i & data.use$Year==t, "precip"])
    cc.vpdmax.data[cc.vpdmax.data$Year==t,i] <- unique(data.use[data.use$Site.Code==i & data.use$Year==t, "vpd.max"])
  }
}



row.names(cc.tmean.data) <- cc.tmean.data$Year
cc.tmean.stack <- stack(cc.tmean.data[,!names(cc.tmean.data) %in% "Year"])
names(cc.tmean.stack) <- c("climate.var", "Site.Code")
cc.tmean.stack$type <- as.factor("Tmean")
cc.tmean.stack$Year <- as.numeric(row.names(cc.tmean.data))
summary(cc.tmean.stack)


row.names(cc.precip.data) <- cc.precip.data$Year
cc.precip.stack <- stack(cc.precip.data[,!names(cc.precip.data) %in% "Year"])
names(cc.precip.stack) <- c("climate.var", "Site.Code")
cc.precip.stack$type <- as.factor("Precip")
cc.precip.stack$Year <- as.numeric(row.names(cc.precip.data))
summary(cc.precip.stack)

row.names(cc.vpdmax.data) <- cc.vpdmax.data$Year
cc.vpdmax.stack <- stack(cc.vpdmax.data[,!names(cc.vpdmax.data) %in% "Year"])
names(cc.vpdmax.stack) <- c("climate.var", "Site.Code")
cc.vpdmax.stack$climate.var <- cc.vpdmax.stack$climate.var/100 # converting to kPa
cc.vpdmax.stack$type <- as.factor("VPDmax")
cc.vpdmax.stack$Year <- as.numeric(row.names(cc.tmean.data))
summary(cc.vpdmax.stack)

cc.climate.stack <- rbind(cc.tmean.stack, cc.precip.stack, cc.vpdmax.stack)
summary(cc.climate.stack)
# sorting out sites so that they go from north at the top to south at the bottom
cc.climate.stack$Site.Code <- factor(cc.climate.stack$Site.Code, levels = c("HO", "GB", "RH", "GE", "PS", "NR", "HF", "LF"))

site.palette <- c("grey30", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

avg.climate <- data.frame(Site.Code = unique(cc.climate.stack$Site.Code),
                          Tmean = NA,
                          Precip = NA,
                          VPDmax = NA)
# Adding mean temperature line
for(i in unique(cc.climate.stack$Site.Code)){
  avg.climate[avg.climate$Site==i, "Tmean"] <- mean(cc.climate.stack[cc.climate.stack$Site.Code==i & cc.climate.stack$type=="Tmean","climate.var"], na.rm=T)
  avg.climate[avg.climate$Site==i, "Precip"] <- mean(cc.climate.stack[cc.climate.stack$Site.Code==i & cc.climate.stack$type=="Precip","climate.var"], na.rm=T)
  avg.climate[avg.climate$Site==i, "VPDmax"] <- mean(cc.climate.stack[cc.climate.stack$Site.Code==i & cc.climate.stack$type=="VPDmax","climate.var"], na.rm=T)
}
avg.climate
avg.climate.stack <- stack(avg.climate)
names(avg.climate.stack) <- c("mean.values", "type")
avg.climate.stack$Site.Code <- avg.climate$Site.Code

fig2 <- ggplot(data=cc.climate.stack) + facet_grid(Site.Code~type, scales="free") +
  geom_density(aes(x=climate.var, y = ..scaled.., fill=type, color=type), alpha=0.4) +
  geom_vline(data= avg.climate.stack, aes(xintercept=mean.values, color=type)) +    
  scale_y_continuous(limits=c(0, 1), expand=c(0,0), breaks=c(0, 0.5, 1)) +
  scale_fill_manual(values = c("#D55E00", "#0072B2", "#009E73"), guide = guide_legend(title="Clim. Var.")) +
  scale_color_manual(values = c("#D55E00", "#0072B2", "#009E73"), guide = guide_legend(title="Clim. Var.")) +
  labs(x = "Growing Season Conditions", y = "Scaled") +
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=12, vjust= 0.5), 
        axis.text.y=element_text(angle=0, color="black", size=8), 
        strip.text=element_text(face="bold", size=14),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="none") +
  #legend.key.size = unit(0.75, "cm"),
  #legend.text = element_text(size=22),
  #legend.key = element_rect(fill = "white")) + 
  #guides(fill=guide_legend(nrow=1, title="")) +
  theme(axis.title.y= element_text(size=14, face="bold")) +
  theme(axis.title.x= element_text(size=14, face="bold")) +
  theme(panel.spacing.x = unit(1,"lines"),
        panel.spacing.y = unit(1,"lines"))


# Looking at precip and temp data for the overall domain
# ggplot(data  = data.use) +
#   geom_point(aes(x=tmean, y = precip)) +
#   stat_smooth(aes(x=tmean, y=precip), method="lm")
# 

tiff(file.path(dir.figs, "SupplementalFigure02.tiff"), width=6, height=6, unit="in", res=600)
fig2
dev.off()


pdf(file.path(dir.figs, "SupplementalFigure02.pdf"), width=6, height=6)
fig2
dev.off()
 