library(ggplot2); library(maps)
library(car)
library(gridExtra)
library(grid)
#################################################
# Figure 1
# Map and canopy break down
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
cc.tree.data$Canopy.Class <- recode(cc.tree.data$Canopy.Class, "'I'='Intermediate'; 'U'='Understory'")

cc.tree.data.all <- cc.tree.data
cc.tree.data.all$Site <- as.factor("All") 

cc.tree.data2 <- rbind(cc.tree.data, cc.tree.data.all)

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


pdf("figures/prelim_figures/site_spp_cc_breakdown.pdf ", width= 13, height = 8.5)
ggplot(data=cc.tree.data) + facet_wrap(~Site) +
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


fig1b <- ggplot(data=cc.tree.data2[cc.tree.data2$Site %in% "All",]) + #facet_wrap(~Site) +
            geom_bar(aes(x=Species, fill=Canopy.Class)) +
            labs(x="Species", y="Count") +
            scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"), guide = guide_legend(title = "Canopy Class")) +
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

pdf("figures/pub_figs/Fig1b.pdf ", width= 13, height = 8.5)
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


fig1a <- ggplot(data=dat.map) +
            guides(fill="none") +
            geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
            geom_path(data=states,aes(x = Lon, y = Lat, group=group), color = "black", size=0.1) +
            geom_point(aes(x=Lon, y=Lat), color = "red", size=2.5, alpha=0.75) +
            geom_text(data = dat.map[!dat.map$Site.Code %in% c("NR", "HF"),], aes(x=Lon+0.25, y=Lat+0.25, label = paste("",as.character(Site.Code), sep="")), color="black", size=5,fontface="bold") +
            geom_text(data = dat.map[dat.map$Site.Code %in% "NR",], aes(x=Lon-0.25, y=Lat-0.25, label = paste("",as.character(Site.Code), sep="")), color="black", size=5,fontface="bold") +
            geom_text(data = dat.map[dat.map$Site.Code %in% "HF",], aes(x=Lon-0.25, y=Lat-0.25, label = paste("",as.character(Site.Code), sep="")), color="black", size=5,fontface="bold") +
            # scale_color_manual(values="red", name="Data Type") +
            theme_bw() +
            theme(legend.position="none") +
            scale_x_continuous(expand=c(0,0), name="Degrees Longitude", limits =c(lon.min - 2,lon.max +2)) +
            scale_y_continuous(expand=c(0,0), name="Degrees Latitude", limits= c(lat.min - 2,lat.max + 2)) +
            coord_equal()

pdf("figures/pub_figs/fig1a.pdf", width=5, height=5)
fig1a
dev.off()

png("figures/pub_figs/Figure1.png", width= 13, height = 8.5, unit="in", res = 300)
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2, widths=c(1.3,1,2))))
print(fig1a, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
print(fig1b, vp = viewport(layout.pos.row = 1, layout.pos.col=2))	

dev.off()


######################################################
# Figure 2
# PDF's of climate space
######################################################
load("processed_data/gam_input_dataset.Rdata")
summary(data.use)

cc.tmean.data <- data.frame(Year = 1895:2015)
cc.precip.data <- data.frame(Year = 1895:2015)

for(i in unique(data.use$Site.Code)){
 for(t in cc.tmean.data$Year){
   if(length(which(data.use$Site.Code==i & data.use$Year==t))==0) next
   cc.tmean.data[cc.tmean.data$Year==t,i] <- unique(data.use[data.use$Site.Code==i & data.use$Year==t , "tmean"])
   cc.precip.data[cc.precip.data$Year==t,i] <- unique(data.use[data.use$Site.Code==i & data.use$Year==t, "precip"])
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
cc.precip.stack$type <- as.factor("Precip.")
cc.precip.stack$Year <- as.numeric(row.names(cc.precip.data))
summary(cc.precip.stack)

cc.climate.stack <- rbind(cc.tmean.stack, cc.precip.stack)

# sorting out sites so that they go from north at the top to south at the bottom
cc.climate.stack$Site.Code <- factor(cc.climate.stack$Site.Code, levels = c("HO", "GB", "RH", "GE", "PS", "NR", "HF", "LF"))

site.palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

fig2 <- ggplot(data=cc.climate.stack) + facet_grid(Site.Code~type, scales="free") +
          geom_density(aes(x=climate.var, y = ..scaled.., fill=Site.Code, color=Site.Code), alpha=0.6) +
          scale_fill_manual(values=site.palette, guide = guide_legend(title = "Site")) +
          scale_color_manual(values=site.palette, guide = guide_legend(title = "Site")) +
          labs(x = "Climate Variable", y = "Scaled") +
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
                legend.position="right") +
          #legend.key.size = unit(0.75, "cm"),
          #legend.text = element_text(size=22),
          #legend.key = element_rect(fill = "white")) + 
          #guides(fill=guide_legend(nrow=1, title="")) +
          theme(axis.title.y= element_text(size=24, face="bold")) +
          theme(axis.title.x= element_text(size=24, face="bold"))




png("figures/pub_figs/Figure2.png", width=8, height=11, units="in", res=300)
fig2
dev.off()

pdf("figures/pub_figs/Figure2.pdf", width=8, height=11)
fig2
dev.off()

########################################################################
# Figure 3 
# Sensitivity curves for Tmean, Precip, Size
########################################################################

load("processed_data/gam6_response_graph.Rdata")

ci.terms.graph$Canopy.Class <- recode(ci.terms.graph$Canopy.Class, "'I'='Intermediate'; 'U'='Understory'")
# DBH dwarfs Tmean and Precip
# will make 2 separate graphs and stitch together like ch2 of diss.


# Temperature
fig3.t <-  ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "tmean", ]) + 
              facet_wrap(~Species) +
              geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=Canopy.Class), alpha=0.5) +
              geom_line(aes(x=x, y=exp(mean), color=Canopy.Class))+
              scale_fill_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
              scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
              geom_hline(yintercept=1, linetype="dashed") +
              coord_cartesian(ylim=c(0.5, 1.5)) +
              # scale_colour_manual("", values = cbbPalette) +
              # scale_fill_manual("", values = cbbPalette) +
              labs(x = expression(bold(paste("Temperature ("^"o", "C)"))), y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=22), 
        axis.text.y=element_text(angle=0, color="black", size=22), 
        strip.text=element_text(face="bold", size=18),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="none",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=24),
        legend.key = element_rect(fill = "white")) + 
  guides(color=guide_legend(nrow=1))+
  theme(axis.title.x = element_text(size=22, face="bold"),
        axis.title.y= element_text(size=22, face="bold"))	


fig3.p <-  ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "precip", ]) + 
  facet_wrap(~Species) +
  geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=Canopy.Class), alpha=0.5) +
  geom_line(aes(x=x, y=exp(mean), color=Canopy.Class))+
  scale_fill_manual(values=c("#0072B2", "#009E73", "#E69F00"), guide = guide_legend(title = ""))+
  scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00"), guide = guide_legend(title = ""))+
  geom_hline(yintercept=1, linetype="dashed") +
  coord_cartesian(ylim=c(0.5, 1.5)) +
  # scale_colour_manual("", values = cbbPalette) +
  # scale_fill_manual("", values = cbbPalette) +
  labs(x = "Precipitation (mm)", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=22), 
        axis.text.y=element_text(angle=0, color="black", size=22), 
        strip.text=element_text(face="bold", size=18),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=18),
        legend.key = element_rect(fill = "white")) + 
  #guides(color=guide_legend(nrow=1),)+
  theme(axis.title.x = element_text(size=22, face="bold"),
        axis.title.y= element_text(size=22, face="bold"))	


# DBH fill by species
spp.palette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00")
fig3.dbh <- ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "dbh.recon", ]) + 
              # facet_wrap(~Species) +
              geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=Species), alpha=0.5) +
              geom_line(aes(x=x, y=exp(mean), color=Species))+
              scale_fill_manual(values=spp.palette)+
              scale_color_manual(values=spp.palette)+
              geom_hline(yintercept=1, linetype="dashed") +
              coord_cartesian(ylim=c(0, 15)) +
              labs(x = "DBH (cm)", y = NULL)+
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=22), 
        axis.text.y=element_text(angle=0, color="black", size=22), 
        #axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
        strip.text=element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="right",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=24),
        legend.key = element_rect(fill = "white")) + 
  guides(color=guide_legend(ncol=1, title=""), fill=guide_legend(title="")) +
  theme(axis.title.x = element_text(size=22, face="bold"),
        axis.title.y= element_text(size=22, face="bold"))


# Stitching together
pdf("figures/submission1_figs/Figure4_combined.pdf", height = 8, width = 13)
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3, widths=c(1.3,1.3,2))))
print(fig4.t, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
print(fig4.p + theme(plot.margin=unit(c(0.5,0,0.7,0),"lines")), vp = viewport(layout.pos.row = 1, layout.pos.col=2))
print(fig3.dbh + theme(plot.margin=unit(c(0.5,0,0.7,0),"lines")), vp = viewport(layout.pos.row = 1, layout.pos.col=3))	
dev.off()
           


fig3.clim <- ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean", "precip"), ]) + 
                facet_grid(Species~Effect, scales="free_x") +
                geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=Canopy.Class), alpha=0.5) +
                geom_line(aes(x=x, y=exp(mean), color=Canopy.Class))+
                scale_fill_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
                scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
                geom_hline(yintercept=1, linetype="dashed") +
                coord_cartesian(ylim=c(0.5, 1.5)) +
                theme_bw()+
                labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
                theme(axis.line.x = element_line(color="black", size = 0.5),
                      axis.line.y = element_line(color="black", size = 0.5))
              # ylim(-0.1,2.1)
