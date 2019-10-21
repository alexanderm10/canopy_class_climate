# Plotting all of the supplemental figures hsowing effects estimated at individual sites
library(ggplot2)
source("0_GraphEffects.R")


path.google <- "/Volumes/GoogleDrive/My Drive/Manuscripts/Alexander_CanopyClimateResponse/canopy_and_climate/manuscript/Revisions_Round2/"
dir.figs <- file.path(path.google, "figures")


pred.out <- read.csv("processed_data/gam_results_SiteSpecific/Post-Process_AllSites.csv")
pred.out$Site.Code <- factor(pred.out$Site.Code, levels=c("GB", "RH", "HO", "GE", "LF", "HF", "NR", "PS"))
pred.out$Species <- factor(pred.out$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
pred.out$Canopy.Class <- car::recode(pred.out$Canopy.Class, "'Canopy'='Overstory'; 'I'='Middle'; 'U'='Understory'")
pred.out$Canopy.Class <- factor(pred.out$Canopy.Class, levels= c("Overstory", "Middle", "Understory"))
summary(pred.out)


# -----------------
# Graphing size effects
# -----------------
site.size <- ggplot(data=pred.out[pred.out$Effect=="dbh.recon",]) +
  # ggtitle("Null Model") +
  facet_grid(Species~.) +
  geom_ribbon(aes(x=x, ymin=lwr.bai*100, ymax=upr.bai*100, fill=Site.Code), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai*100, color=Site.Code)) +
  geom_hline(yintercept=100, linetype="dashed") +
  scale_x_continuous(expand=c(0,0)) +
  # scale_color_manual(name="Site Code") +
  # scale_fill_manual(name="Site Code") +
  coord_cartesian(ylim=c(0, 80*100)) +
  labs(x = expression(bold(paste("DBH (cm)"))), y = expression(bold(paste("Effect on BAI (%)"))),
       fill="Site Code", color="Site Code") +
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_rect(fill=NA, color="black"), 
        axis.text.x=element_text(angle=0, color="black", size=10), 
        axis.text.y=element_text(angle=0, color="black", size=8), 
        strip.text=element_text(face="bold", size=10),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.33, "cm"),
        legend.text = element_text(size=8),
        legend.title=element_text(size=8, face="bold"),
        legend.key = element_rect(fill = "white")) + 
  # guides(color="Site Code", fill="Site Code")+
  theme(axis.title.x = element_text(size=12, face="bold"),
        axis.title.y= element_text(size=12, face="bold"))+
  theme(panel.spacing.x = unit(1.0,"lines"),
        panel.spacing.y = unit(1.0,"lines"))

png(file.path(dir.figs, "SupplementalFigure17_SiteModels_SizeEffect.png"), height=90, width=80, unit="mm", res=600)
site.size
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure17_SiteModels_SizeEffect.pdf"), height=90/25.4, width=80/25.4)
site.size
dev.off()

# -----------------

# -----------------
# Plot-based year effects
# -----------------
site.year <- ggplot(data=pred.out[pred.out$Effect=="Year",]) +
  # ggtitle("Null Model") +
  facet_wrap(~PlotID, scales="free_y") +
  geom_ribbon(aes(x=x, ymin=lwr.bai*100, ymax=upr.bai*100, fill=Site.Code), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai*100, color=Site.Code)) +
  geom_hline(yintercept=100, linetype="dashed") +
  scale_x_continuous(expand=c(0,0)) +
  # coord_cartesian(ylim=c(0, 1750)) +
  labs(x = expression(bold(paste("Year"))), y = expression(bold(paste("Effect on BAI (%)"))),
       fill="Site Code", color="Site Code") +
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_rect(fill=NA, color="black"), 
        axis.text.y=element_text(angle=0, color="black", size=10), 
        axis.text.x=element_text(angle=-45, hjust=0, color="black", size=10), 
        strip.text=element_text(face="bold", size=10),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=10),
        legend.key = element_rect(fill = "white")) + 
  #guides(color=guide_legend(nrow=1),)+
  theme(axis.title.x = element_text(size=12, face="bold"),
        axis.title.y= element_text(size=12, face="bold"))+
  theme(panel.spacing.x = unit(0.5,"lines"),
        panel.spacing.y = unit(0.5,"lines"))

png(file.path(dir.figs, "SupplementalFigure18_SiteModels_YearEffect.png"), height=180, width=180, unit="mm", res=600)
site.year
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure18_SiteModels_YearEffect.pdf"), height=180/25.4, width=180/25.4)
site.year
dev.off()

# -----------------

# -----------------
# Climate effects
# -----------------
png(file.path(dir.figs, "SupplementalFigure19_SiteModels_ClimateEffect_TSCA.png"), height=180, width=180, unit="mm", res=600)
plot.climate.site(dat.plot=pred.out[pred.out$Species=="TSCA",], canopy=T, species=F)
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure19_SiteModels_ClimateEffect_TSCA.pdf"), height=180/25.4, width=180/25.4)
plot.climate.site(dat.plot=pred.out[pred.out$Species=="TSCA",], canopy=T, species=F)
dev.off()


png(file.path(dir.figs, "SupplementalFigure20_SiteModels_ClimateEffect_FAGR.png"), height=180, width=180, unit="mm", res=600)
plot.climate.site(dat.plot=pred.out[pred.out$Species=="FAGR",], canopy=T, species=F)
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure20_SiteModels_ClimateEffect_FAGR.pdf"), height=180/25.4, width=180/25.4)
plot.climate.site(dat.plot=pred.out[pred.out$Species=="FAGR",], canopy=T, species=F)
dev.off()


png(file.path(dir.figs, "SupplementalFigure21_SiteModels_ClimateEffect_ACRU.png"), height=180, width=180, unit="mm", res=600)
plot.climate.site(dat.plot=pred.out[pred.out$Species=="ACRU",], canopy=T, species=F)
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure21_SiteModels_ClimateEffect_ACRU.pdf"), height=180/25.4, width=180/25.4)
plot.climate.site(dat.plot=pred.out[pred.out$Species=="ACRU",], canopy=T, species=F)
dev.off()



png(file.path(dir.figs, "SupplementalFigure22_SiteModels_ClimateEffect_QURU.png"), height=180, width=180, unit="mm", res=600)
plot.climate.site(dat.plot=pred.out[pred.out$Species=="QURU",], canopy=T, species=F)
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure22_SiteModels_ClimateEffect_QURU.pdf"), height=180/25.4, width=180/25.4)
plot.climate.site(dat.plot=pred.out[pred.out$Species=="QURU",], canopy=T, species=F)
dev.off()

# -----------------