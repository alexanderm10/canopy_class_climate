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

write.csv(cc.tree.data2, "canopy_class_tree_data.csv", row.names=F)
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


fig1b <- ggplot(data=cc.tree.data2[cc.tree.data2$Site %in% "All",]) + facet_grid(Species~.) +
            geom_histogram(aes(x=DBH, fill=Canopy.Class), binwidth = 5) +
            labs(x="DBH (cm)", y="Count") +
            scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"), guide = guide_legend(title = "")) +
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
                  legend.position="top",
                  legend.text = element_text(size=18)) +
            #legend.key.size = unit(0.75, "cm"),
            #legend.text = element_text(size=22),
            #legend.key = element_rect(fill = "white")) + 
            #guides(fill=guide_legend(nrow=1, title="")) +
            theme(axis.title.y= element_text(size=24, face="bold")) +
            theme(axis.title.x= element_text(size=24, face="bold")) +
            theme(panel.spacing.y = unit(1.55,"lines"))

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
            geom_text(data = dat.map[!dat.map$Site.Code %in% c("NR", "HF"),], aes(x=Lon+0.25, y=Lat+0.25, label = paste("",as.character(Site.Code), sep="")), color="black", size=5,fontface="bold") +
            geom_text(data = dat.map[dat.map$Site.Code %in% "NR",], aes(x=Lon-0.25, y=Lat-0.25, label = paste("",as.character(Site.Code), sep="")), color="black", size=5,fontface="bold") +
            geom_text(data = dat.map[dat.map$Site.Code %in% "HF",], aes(x=Lon-0.25, y=Lat-0.25, label = paste("",as.character(Site.Code), sep="")), color="black", size=5,fontface="bold") +
            # scale_color_manual(values="red", name="Data Type") +
            theme_bw() +
            theme(legend.position="none",
                  axis.text.x=element_text(angle=0, color="black", size=16, vjust= 0.5), 
                  axis.text.y=element_text(angle=0, color="black", size=16)) +
            scale_x_continuous(expand=c(0,0), name="Degrees Longitude", limits =c(lon.min - 0.25,lon.max +2.25)) +
            scale_y_continuous(expand=c(0,0), name="Degrees Latitude", limits= c(lat.min - 1,lat.max + 1)) +
            coord_equal() +
            theme(axis.title.y= element_text(size=24, face="bold")) +
            theme(axis.title.x= element_text(size=24, face="bold"))

png("figures/pub_figs/fig1a.png", width= 13, height = 8.5, unit="in", res = 300)
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
png(file="figures/pub_figs/Figure1.png", width=15, height= 10, res=300, unit="in")
plot_grid(fig1a, fig1b, align = c("v","h"), nrow = 1, rel_widths = c(0.4, 0.6), labels = c("A)", "B)"))
dev.off()


png("figures/pub_figs/Figure1_presentation.png", width= 15, height = 9, unit="in", res = 300)
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2, widths=c(1.3,1,2))))
print(fig1a, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
print(fig1b, vp = viewport(layout.pos.row = 1, layout.pos.col=2))	

dev.off()


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
  # scale_fill_manual(values=site.palette, guide = guide_legend(title = "Site")) +
  #         scale_color_manual(values=site.palette, guide = guide_legend(title = "Site")) +
          scale_fill_manual(values = c("#D55E00", "#0072B2", "#009E73"), guide = guide_legend(title="Clim. Var.")) +
          scale_color_manual(values = c("#D55E00", "#0072B2", "#009E73"), guide = guide_legend(title="Clim. Var.")) +
          labs(x = expression(bold(paste("Temperature ("^"o", "C)"))), y = "Scaled") +
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
                legend.position="none") +
          #legend.key.size = unit(0.75, "cm"),
          #legend.text = element_text(size=22),
          #legend.key = element_rect(fill = "white")) + 
          #guides(fill=guide_legend(nrow=1, title="")) +
          theme(axis.title.y= element_text(size=24, face="bold")) +
          theme(axis.title.x= element_text(size=24, face="bold")) +
          theme(panel.spacing.x = unit(1.25,"lines"),
                panel.spacing.y = unit(1.75,"lines"))


# Looking at precip and temp data for the overall domain
ggplot(data  = data.use) +
  geom_point(aes(x=tmean, y = precip)) +
  stat_smooth(aes(x=tmean, y=precip), method="lm")


png("figures/pub_figs/Figure2.png", width=13, height=13, units="in", res=300)
fig2
dev.off()

pdf("figures/pub_figs/Figure2.pdf", width=13, height=13)
fig2
dev.off()

########################
# Figure 3 Species Responses
########################
# Loading in species gam data
load("processed_data/gam4_response_graph.Rdata")
ci.terms.graph$Canopy.Class <- recode(ci.terms.graph$Canopy.Class, "'Canopy'='Overstory'; 'I'='Middle'; 'U'='Understory'")
ci.terms.graph$Effect <- recode(ci.terms.graph$Effect, "'tmean'='Tmean';'precip'='Precip'")

ci.terms.graph$Canopy.Class <- factor(ci.terms.graph$Canopy.Class, levels= c("Overstory", "Middle", "Understory"))
ci.terms.graph$Effect <- factor(ci.terms.graph$Effect, levels= c("Tmean", "Precip", "dbh.recon"))
ci.terms.graph$Species <- factor(ci.terms.graph$Species, levels = c("TSCA", "FAGR", "ACRU", "QURU"))
cbbPalette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00")



fig3.combo <- ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("Tmean", "Precip"), ]) + 
  facet_grid(Species~Effect, scales = "free_x") +
  geom_ribbon(aes(x=x, ymin=exp(lwr)*100, ymax=exp(upr)*100), fill= "black", alpha=0.25) +
  geom_line(aes(x=x, y=exp(mean)*100), color="black")+
  scale_fill_manual(values=cbbPalette, guide = guide_legend(title = ""))+
  scale_color_manual(values=cbbPalette, guide = guide_legend(title = ""))+
  geom_hline(yintercept=100, linetype="dashed") +
  coord_cartesian(ylim=c(50, 150)) +
  # scale_colour_manual("", values = cbbPalette) +
  # scale_fill_manual("", values = cbbPalette) +
  labs(x = expression(bold(paste("Temperature ("^"o", "C)"))), y = expression(bold(paste("Effect on BAI (%)"))))+
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=18), 
        axis.text.y=element_text(angle=0, color="black", size=18), 
        strip.text=element_text(face="bold", size=18),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=18),
        legend.key = element_rect(fill = "white")) + 
  #guides(color=guide_legend(nrow=1),)+
  theme(axis.title.x = element_text(size=22, face="bold"),
        axis.title.y= element_text(size=22, face="bold"))+
  theme(panel.spacing.x = unit(1.25,"lines"),
        panel.spacing.y = unit(1.75,"lines"))

png("figures/pub_figs/Figure3_nosize_change_name.png", width=13, height=13, units="in", res=300)
fig3.combo
dev.off()

pdf("figures/pub_figs/Figure3_nosize_changename.pdf", width=13, height=13)
fig3.combo
dev.off()



########################################################################
# Figure 4 
# Sensitivity curves for Tmean, Precip, Size
########################################################################

load("processed_data/gam6_response_graph.Rdata")

ci.terms.graph$Canopy.Class <- recode(ci.terms.graph$Canopy.Class, "'Canopy'='Overstory'; 'I'='Middle'; 'U'='Understory'")
ci.terms.graph$Canopy.Class <- factor(ci.terms.graph$Canopy.Class, levels= c("Overstory", "Middle", "Understory"))

ci.terms.graph$Effect <- recode(ci.terms.graph$Effect, "'tmean'='Tmean';'precip'='Precip'")

ci.terms.graph$Effect <- factor(ci.terms.graph$Effect, levels= c("Tmean", "Precip", "dbh.recon"))
# DBH dwarfs Tmean and Precip
# will make 2 separate graphs and stitch together like ch2 of diss.
ci.terms.graph$Species <- factor(ci.terms.graph$Species, levels = c("TSCA", "FAGR", "ACRU", "QURU"))

fig4.combo <- ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("Tmean", "Precip"), ]) + 
                  facet_grid(Species~Effect, scales = "free_x") +
                  #geom_vline(aes(x=ci.terms.graph$x[which(ci.terms.graph$Effect=="Tmean" & ci.terms.graph$mean==0)])) +
                  geom_ribbon(aes(x=x, ymin=exp(lwr)*100, ymax=exp(upr)*100, fill=Canopy.Class), alpha=0.5) +
                  geom_line(aes(x=x, y=exp(mean)*100, color=Canopy.Class))+
                  scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"), guide = guide_legend(title = ""))+
                  scale_color_manual(values=c("#E69F00","#009E73", "#0072B2"), guide = guide_legend(title = ""))+
                  geom_hline(yintercept=100, linetype="dashed") +
                  coord_cartesian(ylim=c(50, 150)) +
                  # scale_colour_manual("", values = cbbPalette) +
                  # scale_fill_manual("", values = cbbPalette) +
                  labs(x = expression(bold(paste("Temperature ("^"o", "C)"))), y = expression(bold(paste("Effect on BAI (%)"))))+
                  theme(axis.line=element_line(color="black"), 
                        panel.grid.major=element_blank(), 
                        panel.grid.minor=element_blank(), 
                        panel.border=element_blank(),  
                        panel.background=element_blank(), 
                        axis.text.x=element_text(angle=0, color="black", size=18), 
                        axis.text.y=element_text(angle=0, color="black", size=18), 
                        strip.text=element_text(face="bold", size=18),
                        axis.line.x = element_line(color="black", size = 0.5),
                        axis.line.y = element_line(color="black", size = 0.5),
                        legend.position="top",
                        legend.key.size = unit(0.75, "cm"),
                        legend.text = element_text(size=18),
                        legend.key = element_rect(fill = "white")) + 
                  #guides(color=guide_legend(nrow=1),)+
                  theme(axis.title.x = element_text(size=22, face="bold"),
                        axis.title.y= element_text(size=22, face="bold")) +
                  theme(panel.spacing.x = unit(1.25,"lines"),
                        panel.spacing.y = unit(1.75,"lines"))
png("figures/pub_figs/Figure4_nosize_change_name.png", width=13, height=8, units="in", res=300)
fig4.combo
dev.off()

pdf("figures/pub_figs/Figure4_nosize_changename.pdf", width=13, height=8)
fig4.combo
dev.off()

# Temperature
fig4.t <-  ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "tmean", ]) + 
              facet_wrap(~Species) +
              geom_ribbon(aes(x=x, ymin=exp(lwr)*100, ymax=exp(upr)*100, fill=Canopy.Class), alpha=0.5) +
              geom_line(aes(x=x, y=exp(mean)*100, color=Canopy.Class))+
              scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"))+
              scale_color_manual(values=c("#E69F00","#009E73", "#0072B2"))+
              geom_hline(yintercept=100, linetype="dashed") +
              coord_cartesian(ylim=c(50, 150)) +
              # scale_colour_manual("", values = cbbPalette) +
              # scale_fill_manual("", values = cbbPalette) +
              labs(x = expression(bold(paste("Temperature ("^"o", "C)"))), y = expression(bold(paste("Effect on BAI (%)"))))+
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
        legend.text = element_text(size=24),
        legend.key = element_rect(fill = "white")) + 
  guides(color=guide_legend(nrow=1))+
  theme(axis.title.x = element_text(size=22, face="bold"),
        axis.title.y= element_text(size=22, face="bold")) +
  theme(panel.spacing.x = unit(1.25,"lines"),
        panel.spacing.y = unit(1.75,"lines"))


fig4.p <-  ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "precip", ]) + 
  facet_wrap(~Species) +
  geom_ribbon(aes(x=x, ymin=exp(lwr)*100, ymax=exp(upr)*100, fill=Canopy.Class), alpha=0.5) +
  geom_line(aes(x=x, y=exp(mean)*100, color=Canopy.Class))+
  scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"), guide = guide_legend(title = ""))+
  scale_color_manual(values=c("#E69F00","#009E73", "#0072B2"), guide = guide_legend(title = ""))+
  geom_hline(yintercept=100, linetype="dashed") +
  coord_cartesian(ylim=c(50, 150)) +
  # scale_colour_manual("", values = cbbPalette) +
  # scale_fill_manual("", values = cbbPalette) +
  labs(x = "Precipitation (mm)", y=element_blank())+
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
        axis.title.y= element_text(size=22, face="bold"))	+
  theme(axis.text.y=element_blank()) +
  theme(panel.spacing.x = unit(1.25,"lines"),
        panel.spacing.y = unit(1.75,"lines"))


# DBH fill by species-- turning into Figure 5
spp.palette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00")
figS1.dbh <- ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "dbh.recon", ]) + 
              # facet_wrap(~Species) +
              geom_ribbon(aes(x=x, ymin=exp(lwr)*100, ymax=exp(upr)*100, fill=Species), alpha=0.5) +
              geom_line(aes(x=x, y=exp(mean)*100, color=Species))+
              scale_fill_manual(values=spp.palette)+
              scale_color_manual(values=spp.palette)+
              geom_hline(yintercept=100, linetype="dashed") +
              coord_cartesian(ylim=c(0, 1500)) +
              labs(x = "DBH (cm)", y = "Effect on BAI (%)")+
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
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=24),
        legend.key = element_rect(fill = "white")) + 
  guides(color=guide_legend(ncol=1, title=""), fill=guide_legend(title="")) +
  theme(axis.title.x = element_text(size=22, face="bold"),
        axis.title.y= element_text(size=22, face="bold")) +
  theme(panel.spacing.x = unit(1.25,"lines"),
        panel.spacing.y = unit(1.75,"lines"))


png("figures/pub_figs/FigureS1.png", width=13, height=8, units="in", res=300)
figS1.dbh
dev.off()

pdf("figures/pub_figs/FigureS1.pdf", width=13, height=8)
figS1.dbh
dev.off()


# # Stitching together
# pdf("figures/submission1_figs/Figure3_combined.pdf", height = 8, width = 13)
# grid.newpage()
# pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2, widths=c(1.3,1.3))))
# print(fig3.t, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
# print(fig3.p, vp = viewport(layout.pos.row = 1, layout.pos.col=2))
# #print(fig3.dbh + theme(plot.margin=unit(c(0.5,0,0.7,0),"lines")), vp = viewport(layout.pos.row = 1, layout.pos.col=3))
# dev.off()
           


fig4.clim <- ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean", "precip"), ]) + 
                facet_grid(Species~Effect, scales="free_x") +
                geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=Canopy.Class), alpha=0.5) +
                geom_line(aes(x=x, y=exp(mean), color=Canopy.Class))+
                scale_fill_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
                scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
                geom_hline(yintercept=1, linetype="dashed") +
                coord_cartesian(ylim=c(0.5, 1.5)) +
                theme_bw()+
                labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (%)"))))+
                theme(axis.line.x = element_line(color="black", size = 0.5),
                      axis.line.y = element_line(color="black", size = 0.5)) +
                theme(panel.spacing.x = unit(1.25,"lines"),
                      panel.spacing.y = unit(1.75,"lines"))
              # ylim(-0.1,2.1)


##################
# Figure 5 VPD sensitivity Curves
###################
library(car)
load("processed_data/gam_vpdmax_response_graph.Rdata")
vpd.graph <- ci.terms.graph

summary(vpd.graph)

vpd.graph$Canopy.Class <- recode(vpd.graph$Canopy.Class, "'Canopy'='Overstory'; 'I'='Middle';'U'='Understory'")
vpd.graph$Canopy.Class <- factor(vpd.graph$Canopy.Class, levels= c("Overstory", "Middle", "Understory"))
vpd.graph$Species <- factor(vpd.graph$Species, levels = c("TSCA", "FAGR", "ACRU", "QURU"))

fig5 <- ggplot(data=vpd.graph[vpd.graph$Effect %in% "vpd.max", ]) + 
            facet_wrap(~Species) +
            geom_ribbon(aes(x=x/100, ymin=exp(lwr)*100, ymax=exp(upr)*100, fill=Canopy.Class), alpha=0.5) +
            geom_line(aes(x=x/100, y=exp(mean)*100, color=Canopy.Class))+
            scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"),guide = guide_legend(title = ""))+
            scale_color_manual(values=c("#E69F00","#009E73", "#0072B2"),guide = guide_legend(title = ""))+
            geom_hline(yintercept=100, linetype="dashed") +
            coord_cartesian(ylim=c(50, 150)) +
            # scale_colour_manual("", values = cbbPalette) +
            # scale_fill_manual("", values = cbbPalette) +
            labs(x = "VPDmax (kPa)", y = expression(bold(paste("Effect on BAI (%)"))))+
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
                  axis.title.y= element_text(size=22, face="bold")) +
            theme(panel.spacing.x = unit(1.25,"lines"),
                  panel.spacing.y = unit(1.75,"lines"))

png("figures/pub_figs/FigureS2.png", width=13, height=8, units="in", res=300)
fig5
dev.off()

pdf("figures/pub_figs/FigureS2a.pdf", width=13, height=8)
fig5
dev.off()

# Try merging temp precip and VPD into one figure
vpd.graph2 <- vpd.graph
vpd.graph2$x <- vpd.graph2$x/100

franken.fig <- rbind(ci.terms.graph[ci.terms.graph$Effect %in% c("Tmean", "Precip"),], vpd.graph2[vpd.graph2$Effect %in% "vpd.max",])
summary(franken.fig)

franken.fig$Effect <- recode(franken.fig$Effect, "'vpd.max'='VPDmax'")

franken.fig$Effect <- factor(franken.fig$Effect, levels=c("Tmean", "Precip", "VPDmax"))

franken.graph <- ggplot(data=franken.fig[franken.fig$Effect %in% c("VPDmax", "Tmean", "Precip"), ]) + 
                    facet_grid(Species~Effect, scales="free_x") +
                    geom_ribbon(aes(x=x, ymin=exp(lwr)*100, ymax=exp(upr)*100, fill=Canopy.Class), alpha=0.5) +
                    geom_line(aes(x=x, y=exp(mean)*100, color=Canopy.Class))+
                    scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"),guide = guide_legend(title = ""))+
                    scale_color_manual(values=c("#E69F00","#009E73", "#0072B2"),guide = guide_legend(title = ""))+
                    geom_hline(yintercept=100, linetype="dashed") +
                    coord_cartesian(ylim=c(50, 150)) +
                    # scale_colour_manual("", values = cbbPalette) +
                    # scale_fill_manual("", values = cbbPalette) +
                    labs(x = "VPDmax (kPa)", y = expression(bold(paste("Effect on BAI (%)"))))+
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
                          axis.title.y= element_text(size=22, face="bold")) +
                    theme(panel.spacing.x = unit(1.25,"lines"),
                          panel.spacing.y = unit(1.75,"lines"))

pdf(file="figures/pub_figs/franken_graph.pdf", width=13, height=13)
franken.graph
dev.off()

