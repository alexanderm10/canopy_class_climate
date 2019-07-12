library(mgcv)
library(ggplot2)
new.dat <- read.csv("processed_data/sensitivity_extaction_dataframe.csv")
summary(new.dat)


# Figure out which vars are numeric vs. factor
predictors.all <- c("vpd.min", "vpd.max", "tmean", "precip", "Species", "dbh.recon", "Canopy.Class", "spp.plot", "Site.Code", "Year", "PlotID", "spp.cc") 
vars.num <- vector()
for(v in predictors.all){
  if(class(new.dat[,v]) %in% c("numeric", "integer")) vars.num <- c(vars.num, v)
}


load("processed_data/gam_results/gam6_canopy_species_add.Rdata")		
n=100
new.dat6 <- new.dat
vars.fac <- c("Site.Code", "PlotID", "TreeID", "Canopy.Class", "Species", "spp.cc")
var.smooth <- c("Canopy.Class", "Species", "dbh.recon", "Year", "PlotID")
for(v in vars.fac){
  if(v %in% var.smooth) next # keep all levels for our "by" variable
  # Get rid of unimportant levels for everything else
  l1 <- unique(new.dat6[,v])[1]
  new.dat6 <- new.dat6[new.dat6[,v]==l1,]
}
new.dat6$spp.cc <- as.factor(paste(new.dat6$Species, new.dat6$Canopy.Class, sep="."))
summary(new.dat6)

source("0_Calculate_GAMM_Posteriors.R")
g6.ci.terms.pred <- post.distns(model.gam=gam6, model.name="can_spp_Additive", n=n, newdata=new.dat6, vars=predictors.all, terms=T)

g6.ci.out <- g6.ci.terms.pred$ci # separting out the confidence interval 
g6.ci.out[,predictors.all[!predictors.all %in% vars.num]] <- new.dat6[,predictors.all[!predictors.all %in% vars.num]] # copying over our factor labels
g6.ci.out$x <- as.numeric(g6.ci.out$x) # making x numeric; will make factors NA; NA's are ok here
summary(g6.ci.out)

# Convert mean, lwr, upr to BAI units
g6.ci.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(g6.ci.out[,c("mean", "lwr", "upr")])
summary(g6.ci.out)

ci.terms.graph <- g6.ci.out

cbbPalette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00")

# ci.terms.graph$Site <- factor(ci.terms.graph$Site, levels = c("Missouri Ozark", "Morgan Monroe State Park", "Oak Openings Toledo", "Harvard", "Howland"))

# Limiting graph to observational range

summary(ci.terms.graph)
# Year
for(PLOT in unique(test$PlotID)){
  yr.min <- min(test[test$PlotID==PLOT, "Year"])
  yr.max <- max(test[test$PlotID==PLOT, "Year"])
  
  print(paste0(PLOT, ": ", yr.min, "-", yr.max))
  
  ci.terms.graph$x <- ifelse(ci.terms.graph$PlotID!=PLOT | ci.terms.graph$Effect!="Year" | (ci.terms.graph$x>=yr.min & ci.terms.graph$x<=yr.max), ci.terms.graph$x, NA)
  
}


# DBH
for(s in unique(test$spp.cc)){
  dbh.min <- min(test[test$spp.cc==s, "dbh.recon"])
  dbh.max <- max(test[test$spp.cc==s, "dbh.recon"])
  
  ci.terms.graph$x <- ifelse(ci.terms.graph$spp.cc!=s | ci.terms.graph$Effect!="dbh.recon" | (ci.terms.graph$x>=dbh.min & ci.terms.graph$x<=dbh.max), ci.terms.graph$x, NA)
  
}

# Temp
for(s in unique(test$spp.cc)){
  temp.min <- min(test[test$spp.cc==s, "tmean"])
  temp.max <- max(test[test$spp.cc==s, "tmean"])
  
  ci.terms.graph$x <- ifelse(ci.terms.graph$spp.cc!=s | ci.terms.graph$Effect!="tmean" | (ci.terms.graph$x>=temp.min & ci.terms.graph$x<=temp.max), ci.terms.graph$x, NA)
  
}

# Precip	
for(s in unique(test$spp.cc)){
  precip.min <- min(test[test$spp.cc==s, "precip"])
  precip.max <- max(test[test$spp.cc==s, "precip"])
  
  ci.terms.graph$x <- ifelse(ci.terms.graph$spp.cc!=s | ci.terms.graph$Effect!="precip" | (ci.terms.graph$x>=precip.min & ci.terms.graph$x<=precip.max), ci.terms.graph$x, NA)
  
}

summary(ci.terms.graph)

ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "tmean", ]) + 
  facet_wrap(~Species) +
  geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=Canopy.Class), alpha=0.5) +
  geom_line(aes(x=x, y=exp(mean), color=Canopy.Class))+
  scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"))+
  scale_color_manual(values=c("#E69F00","#009E73", "#0072B2"))+
  geom_hline(yintercept=1, linetype="dashed") +
  coord_cartesian(ylim=c(0.5, 1.5)) +
  # scale_colour_manual("", values = cbbPalette) +
  # scale_fill_manual("", values = cbbPalette) +
  theme_bw()+
  labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

# updating plots to resemble the nomenclature used in the manuscript
# in code: TP = Harvard Forest Ameriflux (T)ower (P)lot

summary(ci.terms.graph)

ci.terms.graph$PlotID <- car::recode(ci.terms.graph$PlotID, "'HOW1'='HO1';'HOW2'='HO2';'HOW3'='HO3';
                                     'NRP1'='NR1';'NRP2'='NR2';'NRP3'='NR3';'NRP4'='NR4';'TP1'='HF1';'TP2'='HF2'")

png("figures/pub_figs/SUPPLEMENT_gam6_sensitivities_year.png", height=10, width=8.5, units="in", res=120)
ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "Year" & ci.terms.graph$Species==paste(unique(ci.terms.graph$Species)[1]) &
                             ci.terms.graph$Canopy.Class==paste(unique(ci.terms.graph$Canopy.Class)[1]), ]) + 
  facet_wrap(~PlotID, scales="free_y", ncol=3) +
  geom_ribbon(aes(x=x, ymin=exp(lwr)*100, ymax=exp(upr)*100), alpha=0.5) +
  geom_line(aes(x=x, y=exp(mean)*100))+
  # scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"))+
  # scale_color_manual(values=c("#E69F00","#009E73", "#0072B2"))+
  geom_hline(yintercept=100, linetype="dashed") +
  # coord_cartesian(ylim=c(0.5, 1.5)) +
  # scale_colour_manual("", values = cbbPalette) +
  # scale_fill_manual("", values = cbbPalette) +
  labs(x = "Year", y = "Effect on BAI (%)") +
  scale_x_continuous(expand=c(0,0)) +
  theme(legend.position=c(0.2, 0.8),
        legend.text = element_text(size=rel(2)),
        legend.title = element_text(size=rel(2), face="bold"),
        legend.key.size = unit(2, "lines"),
        axis.text = element_text(size=rel(1), color="black"),
        axis.title = element_text(size=rel(2), face="bold"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        strip.text = element_text(size=rel(1.25), face="bold"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill=NA, color="black"))
dev.off()



png("figures/pub_figs/SUPPLEMENT_gam6_sensitivities_size.png", height=8, width=8.5, units="in", res=120)
ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "dbh.recon" & ci.terms.graph$Canopy.Class==paste(unique(ci.terms.graph$Canopy.Class)[1]), ]) + 
  # facet_wrap(~PlotID, scales="free_y") +
  geom_ribbon(aes(x=x, ymin=exp(lwr)*100, ymax=exp(upr)*100, fill=Species), alpha=0.5) +
  geom_line(aes(x=x, y=exp(mean)*100, color=Species), size=1.5)+
  # scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"))+
  # scale_color_manual(values=c("#E69F00","#009E73", "#0072B2"))+
  geom_hline(yintercept=100, linetype="dashed") +
  coord_cartesian(ylim=c(0, 1500)) +
  # scale_colour_manual("", values = cbbPalette) +
  # scale_fill_manual("", values = cbbPalette) +
  labs(x = expression(bold("DBH (cm)")), y = expression(bold("Effect on BAI (%)")))+
  scale_x_continuous(expand=c(0,0)) +
  theme(legend.position=c(0.2, 0.8),
        legend.text = element_text(size=rel(2)),
        legend.title = element_text(size=rel(2), face="bold"),
        legend.key.size = unit(2, "lines"),
        axis.text = element_text(size=rel(1.5), color="black"),
        axis.title = element_text(size=rel(2), face="bold"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        panel.grid = element_blank(),
        panel.background = element_rect(fill=NA, color="black"))
dev.off()


# png("figures/prelim_figures/SUPPLEMENT_gam6_sensitivities_size.png", height=8, width=8.5, units="in", res=120)
# ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "dbh.recon" & ci.terms.graph$Canopy.Class==paste(unique(ci.terms.graph$Canopy.Class)[1]), ]) + 
#   # facet_wrap(~PlotID, scales="free_y") +
#   geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=Species), alpha=0.5) +
#   geom_line(aes(x=x, y=exp(mean), color=Species))+
#   # scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"))+
#   # scale_color_manual(values=c("#E69F00","#009E73", "#0072B2"))+
#   geom_hline(yintercept=1, linetype="dashed") +
#   # coord_cartesian(ylim=c(0.5, 1.5)) +
#   # scale_colour_manual("", values = cbbPalette) +
#   # scale_fill_manual("", values = cbbPalette) +
#   labs(x = expression(bold("Diameter at Breast Height (cm)")), y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
#   theme(axis.line.x = element_line(color="black", size = 0.5),
#         axis.line.y = element_line(color="black", size = 0.5),
#         panel.grid = element_blank(),
#         panel.background = element_rect(fill=NA, color="black"))
# dev.off()
