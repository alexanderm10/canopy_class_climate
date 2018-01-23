library(ggplot2)
library(car)
source("poster_theme.R")


#########################################################
# Publication Figures
#########################################################

# Cliamte Space figure

data.use <- read.csv("processed_data/AllSites_tree_plus_climate_and_BA.csv", header=T)
summary(data.use)

sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

data.use <- data.use[data.use$Site %in% sites.use,]

summary(data.use[data.use$Site=="Howland","tmean"])

data.use$Site <- factor(data.use$Site, levels = c("Missouri Ozark", "Morgan Monroe State Park", "Oak Openings Toledo", "Harvard", "Howland"))

pdf("figures/Prelim Figures/climate_space.pdf", height = 8, width = 13)
ggplot(data=data.use[data.use$Year >=1935,]) +
	geom_point(aes(x=tmean, y=precip, color=Site, shape=Site), size=4)+
		labs(title = "Site Climate 1935-2012", x = "Mean GS Temp (˚C)", y = expression(bold(paste("Cumulative GS Precip (mm yr"^"-1",")"))))+
		 theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.5)), axis.text.y=element_text(color="black", size=rel(1.5)), axis.title.x=element_text(face="bold", size=rel(1.5), vjust=-0.5),  axis.title.y=element_text(face="bold", size=rel(1.5), vjust=1), plot.margin=unit(c(0.1,0.5,0.5,0.1), "lines"))+
		 theme(legend.position=c(0.85,0.85))+
		 poster.theme2
dev.off()