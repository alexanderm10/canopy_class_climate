library(ggplot2)
library(gameofthrones)

path.google <- "/Volumes/GoogleDrive/My Drive/Manuscripts/Alexander_CanopyClimateResponse/canopy_and_climate/manuscript/Ecology (submit 2019-10)/Revision 3 - 2020-10/"
dir.figs <- file.path(path.google, "figures")
dir.create(dir.figs, recursive = T)

# Summarizing the range of climatic conditions used to for each site
met <- read.csv("processed_data/climate_growing_season.csv", stringsAsFactors = T)
summary(met$Site.Code)

tmean.all <- range(met$tmean, na.rm=T)
tmean.trim <- range(met$tmean[!met$Site.Code %in% c("RH", "GB")], na.rm=T)
tmean.trim2 <- range(met$tmean[met$Site.Code %in% c("RH", "GB")], na.rm=T)

diff(tmean.all)
diff(tmean.trim)
diff(tmean.trim2)

diff(tmean.trim)/diff(tmean.all)
diff(tmean.trim2)/diff(tmean.all)


vpd.all <- range(met$vpd.max, na.rm=T)
vpd.trim <- range(met$vpd.max[!met$Site.Code %in% c("RH", "GB")], na.rm=T)
vpd.trim2 <- range(met$vpd.max[met$Site.Code %in% c("RH", "GB")], na.rm=T)

diff(vpd.all)
diff(vpd.trim)
diff(vpd.trim2)

diff(vpd.trim)/diff(vpd.all)
diff(vpd.trim2)/diff(vpd.all)


# Looking at the distribution of data points in climate space
data.raw <- read.csv("processed_data/NESites_tree_plus_climate_and_BA.csv", header=T, stringsAsFactors = T)
data.raw$Canopy.Class <- car::recode(data.raw$Canopy.Class, "'C' = 'Canopy'; 'D'='Canopy'")
data.raw[!is.na(data.raw$BA.inc) & data.raw$BA.inc > 140, "BA.inc"] <- NA
dat.filter <- data.raw$Live.Dead=="LIVE" & !is.na(data.raw$Live.Dead) & !data.raw$Canopy.Class=="F" & !is.na(data.raw$Canopy.Class) & !is.na(data.raw$RW) & !is.na(data.raw$BA.inc) & data.raw$Species %in% c("QURU", "TSCA", "FAGR", "ACRU")
data.use <- data.raw[dat.filter,]
data.use <- droplevels(data.use)
summary(data.use)

# summary(data.use)
# Checking Temperature ranges for specific species
tmean.quru <- range(data.use$tmean[data.use$Species=="QURU"], na.rm=T)
tmean.quru2 <- range(data.use$tmean[data.use$Species=="QURU" & !data.use$Site.Code %in% c("RH", "GB")], na.rm=T)
tmean.quru3 <- range(data.use$tmean[data.use$Species=="QURU" & data.use$Site.Code %in% c("RH", "GB")], na.rm=T)

diff(tmean.quru)
diff(tmean.quru2)
diff(tmean.quru3)

diff(tmean.quru2)/diff(tmean.quru)
diff(tmean.quru3)/diff(tmean.quru)

vpd.quru <- range(data.use$vpd.max[data.use$Species=="QURU"], na.rm=T)
vpd.quru2 <- range(data.use$vpd.max[data.use$Species=="QURU" & !data.use$Site.Code %in% c("RH", "GB")], na.rm=T)
vpd.quru3 <- range(data.use$vpd.max[data.use$Species=="QURU" & data.use$Site.Code %in% c("RH", "GB")], na.rm=T)

diff(vpd.quru)
diff(vpd.quru2)
diff(vpd.quru3)

diff(vpd.quru2)/diff(vpd.quru)
diff(vpd.quru3)/diff(vpd.quru)



data.graph <- stack(data.use[,c("tmean", "precip", "vpd.max")])
names(data.graph)[2] <- "Effect"
summary(data.graph)
data.graph[,c("Year", "Site.Code", "Species", "Canopy.Class")] <- data.use[,c("Year", "Site.Code", "Species", "Canopy.Class")]
# data.graph <- droplevels(data.graph)
summary(data.graph)

data.use$Site.Code <- factor(data.use$Site.Code, levels=c("HO", "GB", "RH", "GE", "PS", "NR", "HF", "LF"))
data.graph$Site.Code <- factor(data.graph$Site.Code, levels=c("HO", "GB", "RH", "GE", "PS", "NR", "HF", "LF"))

hex.sites <- gameofthrones::got(length(unique(data.use$Site.Code)), option="Daenerys")


plot.base <-  ggplot(data=data.graph[,], aes(x=values, fill=Site.Code)) +
  facet_grid(Species ~ Effect)  +
  geom_density(position="stack")+
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_manual(values=c("HO"="#792427FF", "GB"="#633D43FF", "RH"="#4E565FFF", "GE"="#36727EFF", "PS"="#438990FF", "NR"="#739B96FF", "HF"="#A2AC9CFF", "LF"="#D1BDA2FF"))+
  scale_color_manual(values=c("HO"="#792427FF", "GB"="#633D43FF", "RH"="#4E565FFF", "GE"="#36727EFF", "PS"="#438990FF", "NR"="#739B96FF", "HF"="#A2AC9CFF", "LF"="#D1BDA2FF"))+
  theme(legend.title = element_blank()) +
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_rect(fill=NA, color="black"), 
        axis.ticks.length = unit(-0.5, "lines"),
        axis.text.x = element_text(margin=unit(c(1,1,1,1), "lines"), color="black", size=10),
        axis.text.y = element_text(margin=unit(c(1,1,1,1), "lines"), color="black", size=10),
        strip.text=element_text(face="bold", size=18),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        # legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=12),
        legend.key = element_rect(fill = "white")) + 
  #guides(color=guide_legend(nrow=1),)+
  theme(axis.title.x = element_text(size=12, face="bold"),
        axis.title.y= element_text(size=12, face="bold")) +
  theme(panel.spacing.x = unit(0.5,"lines"),
        panel.spacing.y = unit(0.5,"lines"),
        strip.text.x = element_blank(),
        plot.background = element_rect(fill=NA, color=NA))

plot.tmean <- plot.base %+% subset(data.graph, Effect=="tmean") + 
  labs(x = expression(bold(paste("Temperature ("^"o", "C)"))), y = expression(bold(paste("Relativized BAI (%)")))) +
  guides(fill=F, color=F) +
  theme(strip.text.y = element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(margin=unit(c(1,1,1,1), "lines"), color="black", size=10),
        axis.title.x = element_text(margin=unit(c(0,0,0,0), "lines"), color="black", size=12),
        plot.margin = unit(c(4.75,0.5, 0.5, 1), "lines"))

plot.precip <- plot.base %+% subset(data.graph, Effect=="precip") + 
  labs(x = expression(bold(paste("Precipitation (mm)"))), y = element_blank()) +
  theme(axis.text.y=element_blank(), 
        axis.ticks.y=element_line(unit(-0.5, units="lines")),
        strip.text.y = element_blank(),
        plot.margin = unit(c(0.8,0.5, 0.5, 0.5), "lines"))

plot.vpd <- plot.base %+% subset(data.graph, Effect=="vpd.max") + 
  labs(x = expression(bold(paste("VPD (kPa)"))), y = element_blank()) +
  guides(fill=F, color=F) +
  theme(axis.text.y=element_blank(), 
        axis.ticks.y=element_line(unit(-0.5, units="lines")),
        plot.margin = unit(c(4.75,1, 0.5, 0.5), "lines"))

tiff(file.path(dir.figs,  paste0("SupplementalFigure07_ClimateDataDensity.tiff")), height=6, width=6, unit="in", res=600)
cowplot::plot_grid(plot.tmean, plot.precip, plot.vpd, nrow=1, rel_widths = c(1, 1, 1.1))
dev.off()

pdf(file.path(dir.figs,  paste0("SupplementalFigure07_ClimateDataDensity.pdf")), height=6, width=6)
cowplot::plot_grid(plot.tmean, plot.precip, plot.vpd, nrow=1, rel_widths = c(1, 1, 1.1))
dev.off()