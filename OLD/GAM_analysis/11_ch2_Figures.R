# Scripts for figures to be used in Chapter 2

#############################################################################
#Figure1
# Map of Study sites
#############################################################################

library(ggplot2)
library(maps)
library(raster)
library(car)

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

# 10m land cover from Natural Earth http://www.naturalearthdata.com/downloads/10m-raster-data/10m-natural-earth-1/
nat.earth <- stack("~/PhD/Carbon Research/Calloc_generalscripts/base_layers/NE1_HR_LC_SR_W_DR/NE1_HR_LC_SR_W_DR.tif")

nat.crop <- crop(nat.earth, y=c(min(dat.map$Lon, na.rm=T)-5, max(dat.map$Lon, na.rm=T)+5, min(dat.map$Lat, na.rm=T)-5, max(dat.map$Lat, na.rm=T)+5))
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
#png("figures/Prelim_Figures/pub_figs/Figure1.png", width=10, height=5, units="in", res=220)
png("figures/submission1_figs/Figure1.png", width=10, height=8, units="in", res=220)
	ggplot(data=dat.map) +
  		guides(fill="none") +
  		geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  		geom_path(data=states,aes(x = Lon, y = Lat, group=group), color = "black", size=0.1) +
  		geom_point(aes(x=Lon, y=Lat, color=type), size=2.5, alpha=0.75) +
  		geom_text(data=ch2.sites.exp,aes(x=long+0.65, y=lat+0.65, label = paste("",as.character(state),
  					 sep="")), color="black", size=5,fontface="bold") +
  		scale_color_manual(values="red", name="Data Type") +
  		theme_bw() +
  		theme(legend.position="none") +
  		scale_x_continuous(expand=c(0,0), name="Degrees Longitude", limits =range(rast.table$x)) +
 		scale_y_continuous(expand=c(0,0), name="Degrees Latitude", limits=range(rast.table$y)) +
  		coord_equal()

dev.off()

pdf("figures/submission1_figs/Figure1.pdf", width=10, height=8)
ggplot(data=dat.map) +
  guides(fill="none") +
  geom_tile(data=rast.table, aes(x=x, y=y), fill=rast.table$rgb) + # NOTE: fill MUST be outside of the aes otherwise it converts it to ggcolors
  geom_path(data=states,aes(x = Lon, y = Lat, group=group), color = "black", size=0.1) +
  geom_point(aes(x=Lon, y=Lat, color=type), size=2.5, alpha=0.75) +
  geom_text(data=ch2.sites.exp,aes(x=long+0.65, y=lat+0.65, label = paste("",as.character(state),
                                                                          sep="")), color="black", size=5,fontface="bold") +
  scale_color_manual(values="red", name="Data Type") +
  theme_bw() +
  theme(legend.position="none") +
  scale_x_continuous(expand=c(0,0), name="Degrees Longitude", limits =range(rast.table$x)) +
  scale_y_continuous(expand=c(0,0), name="Degrees Latitude", limits=range(rast.table$y)) +
  coord_equal()

dev.off()


#############################################################################
# Figure2
# Combined figure of climate space and climate time series
#############################################################################
library(ggplot2)
library(car)
require(plyr)
require(ggplot2)
require(RColorBrewer)
require(reshape)
require(scales)
require(zoo)
require(gridExtra)
cbbPalette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00")


# Setting up the Scatter plot of temp vs. precip climate space
cols <- brewer.pal(8,"Set1")
cols <- c(cols[2], cols[1], cols[3:8]) # inverts red/blue order

source("poster_theme.R")
# Doing some data exploration with the cliamte data.

data.use <- read.csv("processed_data/AllSites_tree_plus_climate_and_BA.csv", header=T)
summary(data.use)

sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

data.use <- data.use[data.use$Site %in% sites.use,]

summary(data.use[data.use$Site=="Howland","tmean"])

data.use$Site <- factor(data.use$Site, levels = c("Missouri Ozark", "Morgan Monroe State Park", "Oak Openings Toledo", "Harvard", "Howland"))


summary(data.use)
data.use <- data.use[data.use$Year >=1950 & data.use$Year<= 2012,]

# data.use <- na.omit(data.use)
# data.use <- droplevels(data.use)

# Restructuring for legend
data.use$Site <- factor(data.use$Site, levels=c("Howland", "Harvard", "Oak Openings Toledo", "Morgan Monroe State Park", "Missouri Ozark"))
data.use$State <- recode(data.use$Site,"'Howland'='ME';'Harvard'='MA';'Oak Openings Toledo'='OH';'Morgan Monroe State Park'='IN';'Missouri Ozark'='MO'")
data.use$State <- factor(data.use$State, levels=c("ME", "MA", "OH", "IN", "MO"))

# Script that allows polygons to be drawn around the scatter cloud to show site conditions better.
# Remember to change to State or Site as necessary for proper presentation
df <- data.use
find_hull <- function(data.use) data.use[chull(data.use$tmean, data.use$precip),]
hulls <- ddply(df, "State", find_hull)
summary(hulls)

	
fig2 <- ggplot(data=data.use, aes(tmean, precip, colour=State, fill=State)) + 
  geom_point() + 
  geom_hline(yintercept=0, colour="darkgrey") + 
  geom_vline(xintercept=0, colour="darkgrey") +
  scale_colour_manual("", values = cbbPalette) +
  scale_fill_manual("", values = cbbPalette) +
  #labs(x = "Temperature", y = "Precip")+
  # labs(title = "Site Climate 1950-2012", x = "Mean GS Temp (˚C)", y = expression(bold(paste("GS Precip (mm yr"^"-1",")"))))+
		 theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines"))+
		 theme(legend.position=c(0.2,0.85))+
		 poster.theme2+
		 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        scale_x_continuous(limits=c(10, 30))+
        scale_y_continuous(limits=c(100, 1200))
  
  
fig2

# Fig. 2A
# Density lines- not quite there yet.  
fig2a <- fig2 + geom_density2d(alpha=.5); fig2a

# Fig.2 Scatterplot by itself--Climate dataf rom 1950-2014


figure2a <- fig2 + geom_polygon(data=hulls, alpha=.2)+
	
		 theme(axis.line=element_line(color="black", size=0.5), 
		 	panel.grid.major=element_blank(), 
		 	panel.grid.minor= element_blank(), 
		 	panel.border= element_blank(), 
		 	panel.background= element_blank(), 
		 	axis.text.x=element_text(angle=0, color="black", size=22),
		 	axis.text.y=element_text(color="black", size=22), 
		 	axis.title.x=element_text(size=22, vjust=-0.5),
		 	axis.title.y=element_text(size=22, vjust=1),
		 	plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines"))+
		 theme(legend.position="top")+
		 theme(axis.line.x = element_line(color="black", size = 0.5),
        	axis.line.y = element_line(color="black", size = 0.5))+
        	scale_x_continuous(name=expression(bold(paste("Temperature ("^"o", "C)"))), limits=c(10, 30))+
        	scale_y_continuous(name= expression(bold(paste("Precipitation (mm yr"^"-1",")"))),
        		limits=c(100, 1100))




#pdf("figures/Prelim_Figures/pub_figs/Figure2a.pdf", height = 8, width = 13)
pdf("figures/submission1_figs/Figure2a.pdf", height = 8, width = 13)
	figure2a
dev.off()

png("figures/submission1_figs/Figure2a.png", res=300, unit="in", height = 9, width = 15)
  figure2a
dev.off()

#----------------------------------------------------------------------
# Creating Figure 2B which is the timeseries of climate with the extreme years marked
#----------------------------------------------------------------------
load("processed_data/climate_markeryears.Rdata") # climate.markers
load("processed_data/climate_ts.Rdata")
source("poster_theme.R")

summary(climate.combo)
climate.combo$State <- recode(climate.combo$Site,"'Howland'='ME';'Harvard'='MA';'Oak Openings Toledo'='OH';'Morgan Monroe State Park'='IN';'Missouri Ozark'='MO'")
climate.combo$State <- factor(climate.combo$State, levels=c("ME", "MA", "OH", "IN", "MO"))
climate.combo$type <- recode(climate.combo$type, "'tmean'='Mean Temperature';'precip'='Precipitation'")

summary(climate.markers)

climate.markers$type <- recode(climate.markers$type, "'tmean'='Mean Temperature';'precip'='Precipitation'")
climate.markers$marker <- recode(climate.markers$marker, "'hot'='Hot';'cold'='Cool';'wet'='Wet';'dry'='Dry'")

climate.markers$State <- recode(climate.markers$Site,"'Howland'='ME';'Harvard'='MA';'Oak Openings Toledo'='OH';'Morgan Monroe State Park'='IN';'Missouri Ozark'='MO'")
climate.markers$State <- factor(climate.markers$State, levels=c("ME", "MA", "OH", "IN", "MO"))


figure2b_temp <- ggplot(data=climate.combo[climate.combo$type %in%"Mean Temperature",]) + 
	facet_grid(~State, scales="free")+
	
	geom_vline(data=climate.markers[climate.markers$type %in% "Mean Temperature",],aes(xintercept=marker.year, color=marker), alpha=0.75)+
	geom_line(aes(x=Year, y=value), color="red",size=0.75) +
	geom_line(aes(x=Year, y=mean), size=2) +
	scale_color_manual(values=c("lightblue", "orange")) +
	scale_x_continuous(breaks = seq(1950, 2015, by=10),labels = c("1950","1960","1970", "1980","1990", "2000","2010")) +
	# scale_x_continuous(breaks = seq(1950, 2015, by=10)) +
	scale_y_continuous(expand=c(0,0), name=expression(bold(paste("Temperature ("^"o", "C)")))) +
	
	labs(x="Year", y="Mean Temperature") +
		
	theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(nrow=1)) +
	theme(axis.title.y= element_text(size=rel(1.1), face="bold"))+
	theme(axis.title.x=element_blank())+
  guides(color=guide_legend(title=""))
	
	
#pdf("figures/Prelim_Figures/pub_figs/Figure2b_temp.pdf", height = 8, width = 13)
pdf("figures/submission1_figs/Figure2b_temp.pdf", height = 8, width = 13)
	figure2b_temp
dev.off()

figure2b_precip <- ggplot(data=climate.combo[climate.combo$type %in% "Precipitation",]) + 
	facet_grid(~Site, scales="free")+
	
	geom_vline(data=climate.markers[climate.markers$type %in% "Precipitation",],aes(xintercept=marker.year,
		 color=marker), alpha=0.50)+
	geom_line(aes(x=Year, y=value), size=0.75, color="blue") +
	geom_line(aes(x=Year, y=mean), size=2) +
	scale_color_manual(values=c("brown","darkgreen"))+
	scale_x_continuous(breaks = seq(1950, 2015, by=10),labels = c("1950","1960","1970", "1980","1990", "2000","2010")) +
	labs(x="Year", y=expression(bold(paste("Precipitation (mm yr"^"-1",")")))) +
		
	theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_blank(),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="bottom",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(nrow=1)) +
	theme(axis.title.x = element_text(size=rel(1.1), face="bold"),
        axis.title.y= element_text(size=rel(1.1), face="bold"))	+
  guides(color=guide_legend(title=""))
	
pdf("figures/submission1_figs/Figure2b_precip.pdf", height = 8, width = 13)
	figure2b_precip
dev.off()

# making a combined figure of the time series for a presentation

load("processed_data/climate_markeryears.Rdata") # climate.markers
load("processed_data/climate_ts.Rdata")
source("poster_theme.R")

summary(climate.combo)
climate.combo$State <- recode(climate.combo$Site,"'Howland'='ME';'Harvard'='MA';'Oak Openings Toledo'='OH';'Morgan Monroe State Park'='IN';'Missouri Ozark'='MO'")
climate.combo$State <- factor(climate.combo$State, levels=c("ME", "MA", "OH", "IN", "MO"))
climate.combo$type <- recode(climate.combo$type, "'tmean'='Mean Temperature';'precip'='Precipitation'")

summary(climate.markers)

climate.markers$type <- recode(climate.markers$type, "'tmean'='Mean Temperature';'precip'='Precipitation'")
climate.markers$marker <- recode(climate.markers$marker, "'hot'='Hot';'cold'='Cool';'wet'='Wet';'dry'='Dry'")

climate.markers$State <- recode(climate.markers$Site,"'Howland'='ME';'Harvard'='MA';'Oak Openings Toledo'='OH';'Morgan Monroe State Park'='IN';'Missouri Ozark'='MO'")
climate.markers$State <- factor(climate.markers$State, levels=c("ME", "MA", "OH", "IN", "MO"))


figure2b_temp.agu <- ggplot(data=climate.combo[climate.combo$type %in%"Mean Temperature",]) + 
  facet_grid(~State, scales="free")+
  
  #geom_vline(data=climate.markers[climate.markers$type %in% "Mean Temperature",],aes(xintercept=marker.year, color=marker), alpha=0.75)+
  geom_line(aes(x=Year, y=value), color="red",size=0.75) +
  geom_line(aes(x=Year, y=mean), size=2) +
  #scale_color_manual(values=c("lightblue", "orange")) +
  scale_x_continuous(breaks = seq(1950, 2015, by=10),labels = c("1950","1960","1970", "1980","1990", "2000","2010")) +
  # scale_x_continuous(breaks = seq(1950, 2015, by=10)) +
  scale_y_continuous(expand=c(0,0), name=expression(bold(paste("Temperature ("^"o", "C)")))) +
  
  labs(x="Year", y="Mean Temperature") +
  
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
        axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
        strip.text=element_text(face="bold", size=rel(1.0)),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
  guides(color=guide_legend(nrow=1)) +
  theme(axis.title.y= element_text(size=rel(2.1), face="bold"))+
  theme(axis.title.x=element_blank())+
  guides(color=guide_legend(title=""))


figure2b_precip.agu <- ggplot(data=climate.combo[climate.combo$type %in% "Precipitation",]) + 
  facet_grid(~Site, scales="free")+
  
  #geom_vline(data=climate.markers[climate.markers$type %in% "Precipitation",],aes(xintercept=marker.year, color=marker), alpha=0.50)+
  geom_line(aes(x=Year, y=value), size=0.75, color="blue") +
  geom_line(aes(x=Year, y=mean), size=2) +
  #scale_color_manual(values=c("brown","darkgreen"))+
  scale_x_continuous(breaks = seq(1950, 2015, by=10),labels = c("1950","1960","1970", "1980","1990", "2000","2010")) +
  labs(x="Year", y=expression(bold(paste("Precipitation (mm yr"^"-1",")")))) +
  
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
        axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
        strip.text=element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="bottom",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
  guides(color=guide_legend(nrow=1)) +
  theme(axis.title.x = element_text(size=rel(2.1), face="bold"),
        axis.title.y= element_text(size=rel(2.1), face="bold"))	+
  guides(color=guide_legend(title=""))







png(filename="figures/submission1_figs/AGU_timeseries.png", width=13, height=8.5, units="in", res=300)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1, widths=c(1.3,1,2))))
  print(figure2b_temp, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
  print(figure2b_precip, vp = viewport(layout.pos.row = 2, layout.pos.col=1))	
dev.off()

png(filename="figures/submission1_figs/AGU_timeseries_nolines.png", width=13, height=8.5, units="in", res=300)
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=2,ncol=1, widths=c(1.3,1,2))))
print(figure2b_temp.agu, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
print(figure2b_precip.agu, vp = viewport(layout.pos.row = 2, layout.pos.col=1))	
dev.off()
	 
# Figure 2 Combined
# combining figures with Christy's Script
pdf("figures/submission1_figs/Figure2_combined.pdf", height = 13, width = 13)
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(nrow=3,ncol=1, widths=c(1.3,1,2))))
	print(figure2a  , vp = viewport(layout.pos.row = 1, layout.pos.col=1))
  	print(figure2b_temp, vp = viewport(layout.pos.row = 2, layout.pos.col=1))
  	print(figure2b_precip, vp = viewport(layout.pos.row = 3, layout.pos.col=1))	
dev.off()

png(file.path("figures/submission1_figs/", "Figure2.png"), width=13, height=8.5, units="in", res=300)
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(nrow=3,ncol=1, widths=c(1.3,1,2))))
	print(figure2a  , vp = viewport(layout.pos.row = 1, layout.pos.col=1))
  	print(figure2b_temp, vp = viewport(layout.pos.row = 2, layout.pos.col=1))
  	print(figure2b_precip, vp = viewport(layout.pos.row = 3, layout.pos.col=1))
dev.off()

#############################################################################
# Figure 3
# Competition indices
#############################################################################
load("processed_data/comp_index_graph.Rdata")
summary(comp.ind.graph3)

comp.ind.graph3$State <- recode(comp.ind.graph3$Site,"'Howland'='ME';'Harvard'='MA';'Oak Openings Toledo'='OH';'Morgan Monroe State Park'='IN';'Missouri Ozark'='MO'")
comp.ind.graph3$State <- factor(comp.ind.graph3$State, levels=c("MO", "IN", "OH", "MA", "ME", "All"))


comp.vio <- ggplot(data=comp.ind.graph3) +# facet_wrap(~State)+
  #geom_boxplot(aes(x=State, y= comp.index, fill=NULL, color=Canopy.Class)) +
  geom_violin(aes(x=State, y=comp.index, fill=Canopy.Class), scale="width", alpha=0.75)+
  stat_summary(aes(x=State, y=comp.index, mapping =Canopy.Class), fun.y="quantile",fun.args=list(probs=0.5), geom="point", shape="-", size=17, position=position_dodge(width = 0.9)) +
  stat_summary(aes(x=State, y=comp.index, mapping =Canopy.Class), fun.y="quantile",fun.args=list(probs=0.95), geom="point", shape="_", size=8, position=position_dodge(width = 0.9)) +
  stat_summary(aes(x=State, y=comp.index, mapping =Canopy.Class), fun.y="quantile",fun.args=list(probs=0.05), geom="point", shape="_", size=8, position=position_dodge(width = 0.9)) +              
  labs(x="Canopy Class", y="Competition Index") +
  scale_fill_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
  scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
        axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
        strip.text=element_text(face="bold", size=rel(1.0)),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
  guides(fill=guide_legend(nrow=1, title="")) +
  theme(axis.title.y= element_text(size=rel(1.1), face="bold")) +
  theme(axis.title.x= element_text(size=rel(1.1), face="bold"))

pdf("figures/submission1_figs/Figure3.pdf", width=13, height=8.5)
comp.vio
dev.off()



#############################################################################
# Figure 4
# Sensitivity Curves fro the Canopy.Class Model
#############################################################################
# Getting just temp and precip curves here then will combine with size for later

load("processed_data/gam2_response_graph.R")

g2.ci.out2$Canopy.Class <- recode(g2.ci.out2$Canopy.Class, "'D'='Dominant';'I'='Intermediate';'S'='Understory'")

fig4.t<- 	ggplot(data=g2.ci.out2[g2.ci.out2$Effect %in% "tmean", ]) + 
  		#facet_wrap(~Effect, scales="free_x") +
  		geom_hline(aes(yintercept=0), linetype="dashed")+
  		geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Canopy.Class), alpha=0.5) +
  		geom_line(aes(x=x, y=mean.bai, color=Canopy.Class))+
  		scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00")) +
  		scale_fill_manual(values=c("#0072B2", "#009E73",  "#E69F00")) +
      scale_x_continuous(breaks=c(15,20,25)) +
		labs(x = expression(bold(paste("Temperature ("^"o", "C)"))), y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
  		 ylim(0,2) +
  		theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=22), 
		axis.text.y=element_text(angle=0, color="black", size=22), 
		strip.text=element_blank(),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="none",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=24),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(nrow=1))+
       	theme(axis.title.x = element_text(size=22, face="bold"),
        axis.title.y= element_text(size=22, face="bold"))	

#pdf("figures/Prelim_Figures/pub_figs/Figure4_temp.pdf", width= 13, height = 8.5)
pdf("figures/submission1_figs/Figure4_temp.pdf", width= 13, height = 8.5)
	fig4.t
dev.off()
#---------------------------------------
# Precip
fig4.p<- 	ggplot(data=g2.ci.out2[g2.ci.out2$Effect %in% "precip", ]) + 
  		#facet_wrap(~Effect, scales="free_x") +
      geom_hline(aes(yintercept=0), linetype="dashed")+
  		geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Canopy.Class), alpha=0.5) +
  		geom_line(aes(x=x, y=mean.bai, color=Canopy.Class))+
  		scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00")) +
  		scale_fill_manual(values=c("#0072B2", "#009E73",  "#E69F00")) +
  		labs(x = "Precipitation (mm)", y=element_blank())+
  		 ylim(0,2) +
  		theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=22), 
		#axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_blank(),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="none",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=24),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(nrow=1)) +
    theme(axis.title.x = element_text(size=22, face="bold"),
        axis.title.y= element_text(size=22, face="bold"))+
        theme(axis.text.y=element_blank())
  
        
  

#pdf("figures/Prelim_Figures/pub_figs/Figure4_precip.pdf", width= 13, height = 8.5)
pdf("figures/submission1_figs/Figure4_precip.pdf", width= 13, height = 8.5)
	fig4.p
dev.off()


#---------------------------------------
# Size
fig4.size<- 	ggplot(data=g2.ci.out2[g2.ci.out2$Effect %in% "dbh.recon", ]) + 
  		#facet_wrap(~Effect, scales="free_x") +
      geom_hline(aes(yintercept=0), linetype="dashed")+
  		geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Canopy.Class), alpha=0.5) +
  		geom_line(aes(x=x, y=mean.bai, color=Canopy.Class))+
  		scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00")) +
  		scale_fill_manual(values=c("#0072B2", "#009E73",  "#E69F00")) +
  		labs(x = "DBH (cm)", y=element_blank())+
  		 #ylim(0,2) +
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
        axis.title.y= element_text(size=22, face="bold"))#+
        #theme(axis.text.y=element_blank())	

#pdf("figures/Prelim_Figures/pub_figs/Figure4_DBH.pdf", width= 13, height = 8.5)
pdf("figures/submission1_figs/Figure4_DBH.pdf", width= 13, height = 8.5)
	fig4.size
dev.off()

# Figure 4 Combined
# combining figures with Christy's Script
#pdf("figures/Prelim_Figures/pub_figs/Figure4_combined.pdf", height = 8, width = 13)
pdf("figures/submission1_figs/Figure4_combined.pdf", height = 8, width = 13)
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3, widths=c(1.3,1.3,2))))
	print(fig4.t, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
  	print(fig4.p + theme(plot.margin=unit(c(0.5,0,0.7,0),"lines")), vp = viewport(layout.pos.row = 1, layout.pos.col=2))
  	print(fig4.size + theme(plot.margin=unit(c(0.5,0,0.7,0),"lines")), vp = viewport(layout.pos.row = 1, layout.pos.col=3))	
dev.off()

png("figures/submission1_figs/Figure4_combined.png", height = 9, width = 15, res=300, unit="in")
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3, widths=c(1.5,1.3,2))))
print(fig4.t, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
print(fig4.p + theme(plot.margin=unit(c(0.5,0,0.7,0),"lines")), vp = viewport(layout.pos.row = 1, layout.pos.col=2))
print(fig4.size + theme(plot.margin=unit(c(0.5,0,0.7,0),"lines")), vp = viewport(layout.pos.row = 1, layout.pos.col=3))	
dev.off()


# png(file.path("figures/Prelim_Figures/pub_figs/", "Figure4.png"), width=13, height=8.5, units="in", res=180)
# 	grid.newpage()
# 	pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3, widths=c(1.3,1,2))))
# 	print(fig4.t, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
#   	print(fig4.p, vp = viewport(layout.pos.row = 1, layout.pos.col=2))
#   	print(fig4.size, vp = viewport(layout.pos.row = 1, layout.pos.col=3))
# dev.off()
#############################################################################
# Figure 5
# Weights of CC Model graphed without site separation
#############################################################################
load("processed_data/gam2_weights_graph.R")
summary(data.graph)
data.graph$Canopy.Class <- recode(data.graph$Canopy.Class, "'Suppressed'='Understory'")


fig5 <-	ggplot(data.graph) + facet_grid(~Canopy.Class) +
		
	geom_ribbon(aes(x=Year, ymin=weight.tmean2 - weight.tmean2.SE, ymax=weight.tmean2 + weight.tmean2.SE),
	 alpha=0.25, fill="red") +
	geom_ribbon(aes(x=Year, ymin=weight.precip2 - weight.precip2.SE, ymax=weight.precip2 +
	 weight.precip2.SE), alpha=0.25, fill="blue") +
	geom_ribbon(aes(x=Year, ymin=weight.dbh.recon2 - weight.dbh.recon2.SE, ymax=weight.dbh.recon2 +
	 weight.dbh.recon2.SE), alpha=0.25, fill="darkgreen") +
		
	geom_line(aes(x=Year, y=weight.tmean2), size=1, color="red") +
	geom_line(aes(x=Year, y=weight.precip2), size=1, color="blue") +
	geom_line(aes(x=Year, y=weight.dbh.recon2), size=1, color="darkgreen")+
  scale_x_continuous(breaks=c(1900,1925,1950,1975,2000)) +
	labs(y="Fraction Growth Limitation", x="Year") +
	
	theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=22),
		axis.text.y=element_text(angle=0, color="black", size=22), 
		#axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=22),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="right",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=24),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(ncol=1)) +
        theme(axis.title.x = element_text(size=24, face="bold"),
        axis.title.y= element_text(size=24, face="bold")) #+
        #theme(axis.text.y=element_blank())	
	

fig5.agu <-	ggplot(data.graph) + facet_grid(~Canopy.Class) +
  
  geom_ribbon(aes(x=Year, ymin=weight.tmean2 - weight.tmean2.SE, ymax=weight.tmean2 + weight.tmean2.SE),
              alpha=0.25, fill="red") +
  geom_ribbon(aes(x=Year, ymin=weight.precip2 - weight.precip2.SE, ymax=weight.precip2 +
                    weight.precip2.SE), alpha=0.25, fill="blue") +
  geom_ribbon(aes(x=Year, ymin=weight.dbh.recon2 - weight.dbh.recon2.SE, ymax=weight.dbh.recon2 +
                    weight.dbh.recon2.SE), alpha=0.25, fill="darkgreen") +
  
  geom_line(aes(x=Year, y=weight.tmean2), size=1, color="red") +
  geom_line(aes(x=Year, y=weight.precip2), size=1, color="blue") +
  geom_line(aes(x=Year, y=weight.dbh.recon2), size=1, color="darkgreen")+
  
  labs(y="Fraction Growth Limitation", x="Year") +
  
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), 
        #axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
        strip.text=element_text(face="bold", size=rel(1.0)),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="right",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
  guides(color=guide_legend(ncol=1)) +
  theme(axis.title.x = element_text(size=rel(2.1), face="bold"),
        axis.title.y= element_text(size=rel(2.1), face="bold")) #+
#theme(axis.text.y=element_blank())	


#pdf("figures/Prelim_Figures/pub_figs/Figure5.pdf", width= 13, height = 8.5)
pdf("figures/submission1_figs/Figure5.pdf", width= 13, height = 8.5)
	fig5
dev.off()

# png(file.path("figures/Prelim_Figures/pub_figs/", "Figure5.png"), width=13, height=8.5, units="in", res=180)
# 	fig4
# dev.off()

png(file.path("figures/Prelim_Figures/pub_figs/", "Figure5_defense.png"), width=15, height=9, units="in", res=300)
  fig5
dev.off()

#############################################################################
# Figure 6
# Probability density functions for diameter
#############################################################################

load("processed_data/gam2_weights_graph.Rdata")
summary(gam2.weights)

load("processed_data/median_temp.Rdata")
load("processed_data/median_precip.Rdata")


median.temp$clim.type <- as.factor("Temperature")
median.precip$clim.type <- as.factor("Precipitation")

median.clim <- rbind(median.temp, median.precip)



gam2.weights1 <- gam2.weights
gam2.weights1$clim.type <- as.factor("Temperature")
gam2.weights1$clim.mark <- gam2.weights1$Temp.Mark

gam2.weights2 <- gam2.weights
gam2.weights2$clim.type <- as.factor("Precipitation")
gam2.weights2$clim.mark <- gam2.weights2$Precip.Mark

gam2.graph <- rbind(gam2.weights1, gam2.weights2)
summary(gam2.graph)
gam2.graph$clim.mark <- recode(gam2.graph$clim.mark, "'A'='Ambient';'cold'='Cool';'hot'='Hot';'dry'='Dry';'wet'='Wet'")
gam2.graph$Canopy.Class <- recode(gam2.graph$Canopy.Class, "'D'='Dominant'; 'I'='Intermediate';'S'='Understory'")

summary(median.clim)
median.clim$type <- recode(median.clim$type, "'A'='Ambient';'cold'='Cool';'hot'='Hot';'dry'='Dry';'wet'='Wet'")
median.clim$Canopy.Class <- recode(median.clim$Canopy.Class, "'D'='Dominant'; 'I'='Intermediate';'S'='Understory'")

#interact <- c("A-A", "hot-wet", "A-wet", "A-dry")


# median-centering the data so that the ambient line is *always* on 0
summary(gam2.graph)
summary(median.clim)
for(i in unique(gam2.graph$Canopy.Class)){
  # Find the median temp & precip adjustment values
  temp.adj   <- median.clim[median.clim$Canopy.Class==i & median.clim$type=="Ambient" & median.clim$clim.type=="Temperature","median"]
  precip.adj <- median.clim[median.clim$Canopy.Class==i & median.clim$type=="Ambient" & median.clim$clim.type=="Precipitation","median"]
  
  # Adjust the Standardized BAI
  gam2.graph[gam2.graph$Canopy.Class==i & gam2.graph$clim.type=="Temperature", "Clim.Dev"] <- gam2.graph[gam2.graph$Canopy.Class==i & gam2.graph$clim.type=="Temperature", "BA.inc.Clim"]-temp.adj
  gam2.graph[gam2.graph$Canopy.Class==i & gam2.graph$clim.type=="Precipitation", "Clim.Dev"] <- gam2.graph[gam2.graph$Canopy.Class==i & gam2.graph$clim.type=="Temperature", "BA.inc.Clim"]-precip.adj
  
  # Adjust our median lines
  median.clim[median.clim$Canopy.Class==i & median.clim$clim.type=="Temperature","median.dev"] <- median.clim[median.clim$Canopy.Class==i & median.clim$clim.type=="Temperature","median"] - temp.adj
  median.clim[median.clim$Canopy.Class==i & median.clim$clim.type=="Precipitation","median.dev"] <- median.clim[median.clim$Canopy.Class==i & median.clim$clim.type=="Precipitation","median"] - precip.adj
}

save(gam2.graph, file="processed_data/gam2_density_graph_data.Rdata")

gam2.graph$mix.mark <- recode(gam2.graph$mix.mark, "'A-A'='Ambient';'A-wet'='Wet';'A-dry'='Dry';
                                                    'hot-wet'='Hot-Wet'; 'hot-dry' = 'Hot-Dry'; 'hot-A'='Hot'; 'cold-A'='Cool'")

gam2.graph2 <- gam2.graph
gam2.graph2$BA.inc.Clim.log <- log(gam2.graph$BA.inc.Clim)


gam2.graph2$BA.inc.Clim.log <- ifelse(gam2.graph2$BA.inc.Clim.log < -2, -2, gam2.graph2$BA.inc.Clim.log)
gam2.graph2$BA.inc.Clim.log <- ifelse(gam2.graph2$BA.inc.Clim.log > 2, 2, gam2.graph2$BA.inc.Clim.log)
# Set interactions to graph
interact.T <- c("Ambient", "Hot", "Cool")
interact.P <- c("Ambient", "Wet", "Dry")
interact.mix <- c("Ambient", "Hot-Wet", "Hot-Dry")

summary(gam2.graph)
temp.dens.plot <- ggplot(gam2.graph2[gam2.graph2$mix.mark %in% interact.T,]) + facet_grid(Canopy.Class~.) +
                geom_density(aes(x=BA.inc.Clim.log,color=mix.mark, fill=mix.mark), alpha=0.1) +
                # geom_vline(data=median.clim, aes(xintercept=median, color=type)) +
                # geom_density(aes(x=log(Clim.Dev),color=clim.mark, fill=clim.mark), alpha=0.1) +
                #geom_vline(data=median.clim, aes(xintercept=median.dev, color=type)) +
                #scale_x_continuous(limits = c(-20,20), expand=c(0,0), breaks = seq(-15,15, by=5)) +
                scale_color_manual(values=c("grey50","blue", "red")) +
                scale_fill_manual(values=c("grey50","blue", "red")) +
                labs(x= "", y="Density") +
                coord_cartesian(xlim=c(-2, 2)) +
                theme(axis.line=element_line(color="black"), 
                      panel.grid.major=element_blank(), 
                      panel.grid.minor=element_blank(), 
                      panel.border=element_blank(),  
                      panel.background=element_blank(), 
                      axis.text.x=element_text(angle=0, color="black"), 
                      axis.text.y=element_text(angle=0, color="black"), 
                      strip.text=element_blank(),
                      axis.line.x = element_line(color="black", size = 0.5),
                      axis.line.y = element_line(color="black", size = 0.5),
                      legend.position="top",
                      legend.key.size = unit(0.75, "cm"),
                      legend.text = element_text(size=18),
                      legend.key = element_rect(fill = "white")) + 
                guides(color=guide_legend(nrow=1, title=""), fill=guide_legend(title="")) +
                theme(axis.title.y= element_text(size=24, face="bold"))+
                theme(axis.title.x= element_text(size=24, face="bold")) +
                scale_x_continuous(expand=c(0,0))


precip.dens.plot <- ggplot(gam2.graph2[gam2.graph2$mix.mark %in% interact.P,]) + facet_grid(Canopy.Class~.) +
                        geom_density(aes(x=BA.inc.Clim.log,color=mix.mark, fill=mix.mark), alpha=0.1) +
                        # geom_vline(data=median.clim, aes(xintercept=median, color=type)) +
                        # geom_density(aes(x=log(Clim.Dev),color=clim.mark, fill=clim.mark), alpha=0.1) +
                        #geom_vline(data=median.clim, aes(xintercept=median.dev, color=type)) +
                        #scale_x_continuous(limits = c(-20,20), expand=c(0,0), breaks = seq(-15,15, by=5)) +
                        scale_color_manual(values=c("grey50","burlywood", "darkgreen")) +
                        scale_fill_manual(values=c("grey50","burlywood", "darkgreen")) +
                        labs(x= "log(BAI-Index)", y="") +
                        coord_cartesian(xlim=c(-2, 2)) +
                        theme(axis.line=element_line(color="black"), 
                              panel.grid.major=element_blank(), 
                              panel.grid.minor=element_blank(), 
                              panel.border=element_blank(),  
                              panel.background=element_blank(), 
                              axis.text.x=element_text(angle=0, color="black"), 
                              axis.text.y=element_text(angle=0, color="black"), 
                              strip.text=element_blank(),
                              axis.line.x = element_line(color="black", size = 0.5),
                              axis.line.y = element_line(color="black", size = 0.5),
                              legend.position="top",
                              legend.key.size = unit(0.75, "cm"),
                              legend.text = element_text(size=18),
                              legend.key = element_rect(fill = "white")) + 
                        guides(color=guide_legend(nrow=1, title=""), fill=guide_legend(title="")) +
                        theme(axis.title.y= element_text(size=24, face="bold"))+
                        theme(axis.title.x= element_text(size=24, face="bold")) +
                        theme(axis.text.y=element_blank()) +
                        scale_x_continuous(expand=c(0,0))

int.dens.plot <- ggplot(gam2.graph2[gam2.graph2$mix.mark %in% interact.mix,]) + facet_grid(Canopy.Class~.) +
                        geom_density(aes(x=BA.inc.Clim.log,color=mix.mark, fill=mix.mark), alpha=0.1) +
                        # geom_vline(data=median.clim, aes(xintercept=median, color=type)) +
                        # geom_density(aes(x=log(Clim.Dev),color=clim.mark, fill=clim.mark), alpha=0.1) +
                        #geom_vline(data=median.clim, aes(xintercept=median.dev, color=type)) +
                        #scale_x_continuous(limits = c(-20,20), expand=c(0,0), breaks = seq(-15,15, by=5)) +
                        scale_color_manual(values=c("grey50","darkorange", "darkorchid")) +
                        scale_fill_manual(values=c("grey50","darkorange", "darkorchid")) +
                        labs(x= "", y="") +
                        coord_cartesian(xlim=c(-2, 2)) +
                        theme(axis.line=element_line(color="black"), 
                              panel.grid.major=element_blank(), 
                              panel.grid.minor=element_blank(), 
                              panel.border=element_blank(),  
                              panel.background=element_blank(), 
                              axis.text.x=element_text(angle=0, color="black"), 
                              axis.text.y=element_text(angle=0, color="black"), 
                              strip.text=element_text(face="bold", size=22),
                              axis.line.x = element_line(color="black", size = 0.5),
                              axis.line.y = element_line(color="black", size = 0.5),
                              legend.position="top",
                              legend.key.size = unit(0.75, "cm"),
                              legend.text = element_text(size=18),
                              legend.key = element_rect(fill = "white")) + 
                        guides(color=guide_legend(nrow=1, title=""), fill=guide_legend(title="")) +
                        theme(axis.title.y= element_text(size=24, face="bold"))+
                        theme(axis.title.x= element_text(size=24, face="bold")) +
                          theme(axis.text.y=element_blank()) +
                        scale_x_continuous(expand=c(0,0))
#dens.plot
# pdf("figures/Prelim_Figures/pub_figs/Figure6_density.pdf", height = 8, width = 13)
pdf("figures/submission1_figs/Figure6_temp.pdf", height = 8, width = 13)
  temp.dens.plot
dev.off()

pdf("figures/submission1_figs/Figure6_precip.pdf", height = 8, width = 13)
precip.dens.plot
dev.off()

pdf("figures/submission1_figs/Figure6_mix.pdf", height = 8, width = 13)
int.dens.plot
dev.off()


png(file.path("figures/submission1_figs/", "Figure6_combined2.png"), width=13, height=8.5, units="in", res=300)
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3, widths=c(1.3,1.3,1.3))))
	print(temp.dens.plot, vp = viewport(layout.pos.row = 1, layout.pos.col=1))
  	print(precip.dens.plot, vp = viewport(layout.pos.row = 1, layout.pos.col=2))
  	print(int.dens.plot, vp = viewport(layout.pos.row = 1, layout.pos.col=3))
dev.off()



# Figure 6 sub A--AGU
dens.plot.agu <- ggplot(gam2.graph) + facet_grid(Canopy.Class~clim.type) +
  # geom_density(aes(x=BA.inc.Clim,color=clim.mark, fill=clim.mark), alpha=0.1) +
  # geom_vline(data=median.clim, aes(xintercept=median, color=type)) +
  geom_density(aes(x=Clim.Dev,color=clim.mark, fill=clim.mark), alpha=0.1) +
  geom_vline(data=median.clim, aes(xintercept=median.dev, color=type)) +
  #scale_x_continuous(limits = c(-20,20), expand=c(0,0), breaks = seq(-15,15, by=5)) +
  coord_cartesian(xlim=c(-10, 10)) +
  scale_color_manual(values=c("grey50", "blue", "burlywood3", "red", "darkgreen")) +
  scale_fill_manual(values=c("grey50", "blue", "burlywood3", "red", "darkgreen")) +
  labs(x= "BAI-Index", y="Density") +
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), 
        axis.text.y=element_text(angle=0, color="black", size=rel(1.5)), 
        strip.text=element_text(face="bold", size=rel(1.0)),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
  guides(color=guide_legend(nrow=1, title=""), fill=guide_legend(title="")) +
  theme(axis.title.y= element_text(size=rel(2.1), face="bold"))+
  theme(axis.title.x= element_text(size=rel(2.1), face="bold"))

dens.plot.agu
# pdf("figures/Prelim_Figures/pub_figs/AGU_Figure6_density.pdf", height = 8, width = 13)
png(filename="figures/submission1_figs/AGU_Figure6.png", height = 8, width = 13, unit="in", res=300)
dens.plot.agu
dev.off()
