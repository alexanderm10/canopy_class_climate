library(ggplot2)
library(car)
require(plyr)
require(ggplot2)
require(RColorBrewer)
require(reshape)
require(scales)
require(zoo)

cbbPalette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00")

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
data.use <- data.use[data.use$Year >=1950 & data.use$Year <= 2012,]

# data.use <- na.omit(data.use)
# data.use <- droplevels(data.use)

# Script that allows polygons to be drawn around the scatter cloud to show site conditions better.
df <- data.use
find_hull <- function(data.use) data.use[chull(data.use$tmean, data.use$precip),]
hulls <- ddply(df, "Site", find_hull)
summary(hulls)

	
	
fig2 <- ggplot(data=data.use, aes(tmean, precip, colour=Site, fill=Site)) + 
  geom_point() + 
  geom_hline(yintercept=0, colour="darkgrey") + 
  geom_vline(xintercept=0, colour="darkgrey") +
  scale_colour_manual("", values = cbbPalette) +
  scale_fill_manual("", values = cbbPalette) +
  #labs(x = "Temperature", y = "Precip")+
  labs(title = "Site Climate 1950-2012", x = "Mean GS Temp (˚C)", y = expression(bold(paste("Cumulative GS Precip (mm yr"^"-1",")"))))+
		 theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines"))+
		 theme(legend.position=c(0.2,0.85))+
		 poster.theme2+
		 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        scale_x_continuous(limits=c(10, 30))+
        scale_y_continuous(limits=c(100, 1200))
  
  
fig2

# Fig. 2A
# Density lines
fig2a <- fig2 + geom_density2d(alpha=.5); fig2a

# Fig. 2B
# Convex hulls
# fig2b <- 

pdf("figures/Prelim_Figures/climate_space2.pdf", height = 8, width = 13)
fig2 + geom_polygon(data=hulls, alpha=.2)+
	labs(title = "Site Climate 1950-2014", x = "Mean GS Temp (˚C)", y = expression(bold(paste("Cumulative GS Precip (mm yr"^"-1",")"))))+
		 theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines"))+
		 theme(legend.position=c(0.2,0.85))+
		 poster.theme2+
		 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        scale_x_continuous(limits=c(10, 30))+
        scale_y_continuous(limits=c(100, 1200))
dev.off()



pdf("figures/Prelim_Figures/climate_space.pdf", height = 8, width = 13)
ggplot(data=data.use) +
	geom_point(aes(x=tmean, y=precip, color=Site, shape=Site), size=4)+
	scale_colour_manual("", values = cbbPalette) +
  scale_fill_manual("", values = cbbPalette) +
		
	labs(title = "Site Climate 1950-2012", x = "Mean GS Temp (˚C)", y = expression(bold(paste("Cumulative GS Precip (mm yr"^"-1",")"))))+
		 theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines"))+
		 theme(legend.position=c(0.85,0.85))+
		 poster.theme2+
		 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))
dev.off()




# Want to look at the hottest and driest years in the record

clim.data <- read.csv("processed_data/climate_growing_season.csv", header=T)
summary(clim.data)

clim.data <- clim.data[clim.data$Site %in% data.use$Site & clim.data$Year >= 1950 & clim.data$Year<=2012,]
clim.data$Site <- droplevels(clim.data$Site)

# Hottest Years
hot.years <- data.frame(array(dim=c(round(length(unique(clim.data$Year))*.07,0), length(unique(clim.data$Site)))))
names(hot.years) <- unique(clim.data$Site)

for(s in names(hot.years)){
  site.data <- clim.data[clim.data$Site==s,]
  
  magic.number <- quantile(site.data[,"tmean"], 0.95, na.rm=T)
  
  rows <- which(site.data[,"tmean"]>=magic.number)
  years <- site.data[rows,"Year"]
  hot.years[,s] <- c(years, rep(NA, nrow(hot.years)-length(years)))
  
}
hot.years

hot.stack <- stack(hot.years)
names(hot.stack) <- c("marker.year", "Site")
hot.stack$type <- as.factor("tmean")
hot.stack$marker <- as.factor("hot")
summary(hot.stack)

# Coolest Years


cold.years <- data.frame(array(dim=c(round(length(unique(clim.data$Year))*.07,0), length(unique(clim.data$Site)))))
names(cold.years) <- unique(clim.data$Site)

for(s in names(cold.years)){
  site.data <- clim.data[clim.data$Site==s,]
  
  magic.number <- quantile(site.data[,"tmean"], 0.05, na.rm=T)
  
  rows <- which(site.data[,"tmean"]<=magic.number)
  years <- site.data[rows,"Year"]
  cold.years[,s] <- c(years, rep(NA, nrow(cold.years)-length(years)))
  
}
cold.years
cold.stack <- stack(cold.years)
names(cold.stack) <- c("marker.year", "Site")
cold.stack$type <- as.factor("tmean")
cold.stack$marker <- as.factor("cold")
summary(cold.stack)


# Wettest Years

wet.years <- data.frame(array(dim=c(round(length(unique(clim.data$Year))*.07,0), length(unique(clim.data$Site)))))
names(wet.years) <- unique(clim.data$Site)

for(s in names(wet.years)){
  site.data <- clim.data[clim.data$Site==s,]
  
  magic.number <- quantile(site.data[,"precip"], 0.95, na.rm=T)
  
  rows <- which(site.data[,"precip"]>=magic.number)
  years <- site.data[rows,"Year"]
  wet.years[,s] <- c(years, rep(NA, nrow(wet.years)-length(years)))
  
}
wet.years

wet.stack <- stack(wet.years)
names(wet.stack) <- c("marker.year", "Site")
wet.stack$type <- as.factor("precip")
wet.stack$marker <- as.factor("wet")
summary(wet.stack)

# Driest years

dry.years <- data.frame(array(dim=c(round(length(unique(clim.data$Year))*.07,0), length(unique(clim.data$Site)))))
names(dry.years) <- unique(clim.data$Site)

for(s in names(dry.years)){
  site.data <- clim.data[clim.data$Site==s,]
  
  magic.number <- quantile(site.data[,"precip"], 0.05, na.rm=T)
  
  rows <- which(site.data[,"precip"]<=magic.number)
  years <- site.data[rows,"Year"]
  dry.years[,s] <- c(years, rep(NA, nrow(dry.years)-length(years)))
  
}
dry.years

dry.stack <- stack(dry.years)
names(dry.stack) <- c("marker.year", "Site")
dry.stack$type <- as.factor("precip")
dry.stack$marker <- as.factor("dry")
summary(dry.stack)

# Combining everything together for graphing

climate.markers <- rbind(hot.stack, cold.stack, wet.stack, dry.stack)
summary(climate.markers)

save(climate.markers, file="processed_data/climate_markeryears.Rdata")

# Plotting time series to temperature and precipitation

# Creating moving averages for temp and precip

temp <- unique(data.use[, c("Site", "Year","tmean")])

sites <- unique(temp$Site)

dim(temp)
temp$mean <- NA
require(zoo)

for(s in sites){
	temp[temp$Site==s,"mean"] <- rollmean(temp[temp$Site==s,"tmean"],10, fill=list(NA,NULL,NA))
	
}

summary(temp)
names(temp) <- c("Site", "Year", "value", "mean")
temp$type <- as.factor("tmean")



precip <- unique(data.use[, c("Site", "Year","precip")])
precip$mean <- NA

for(s in sites){
	precip[precip$Site==s,"mean"] <- rollmean(precip[precip$Site==s,"precip"],10, fill=list(NA,NULL,NA))
	
}
summary(precip )
names(precip) <- c("Site", "Year", "value", "mean")
precip$type<- as.factor("precip")

climate.combo <- rbind(temp, precip)

# Tmean
ggplot(data=temp) + facet_grid(Site~.)+
	geom_line(aes(x=Year, y=value), color="red")+
	geom_line(aes(x=Year, y=mean), linetype="dashed")+
	theme_bw()
# Precip	
ggplot(data=precip) + facet_grid(Site~.)+
	geom_line(aes(x=Year, y=value), color="blue") +
	geom_line(aes(x=Year, y=mean), linetype="dashed") +
	theme_bw()	
# Combo
save(climate.combo, file="processed_data/climate_ts.Rdata")
pdf("figures/Prelim_Figures/climate_time_series.pdf", height = 8, width = 13)
ggplot(data=climate.combo) + facet_grid(type~Site, scales="free")+
	geom_vline(data=climate.markers,aes(xintercept=marker.year, color=marker), alpha=0.50)+
	geom_line(aes(x=Year, y=value, color=type)) +
	geom_line(aes(x=Year, y=mean), size=2) +
	scale_color_manual(values=c("lightblue", "brown", "orange", "blue","red", "darkgreen"))+
	scale_x_continuous(breaks = seq(1950, 2015, by=10),labels = c("1950","1960","1970", "1980","1990", "2000","2010")) +
	theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), panel.background=element_blank(),axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))
dev.off()
