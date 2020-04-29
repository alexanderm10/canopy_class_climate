# --------------------------------
# Header & Description
# --------------------------------
# Compare outputs of species and canopy-based models; now fit independtly
# Steps:
# 1. Format Data (lines 21-60)
# 2. Run & Save inidividual models; save stats to table: (lines 60-193)
#    - spp.clim = null + temp + precip + vpd (lines 91-108)
#    - cc.clim = null + (base clim)*canopy class (lines 130-147)
# 4. Generate posterior predictions (mean & 95 CI) & save data frames?
# --------------------------------

library(mgcv)
library(ggplot2)
library(car)

# Set up a directory where to save things & not overwrite past efforts
dir.out <- "processed_data/gam_results_VarDecomp"
dir.create(dir.out, recursive=T, showWarnings = F)

# --------------------------------
# 1. Read in & format data
# --------------------------------
data.raw <- read.csv("processed_data/NESites_tree_plus_climate_and_BA.csv", header=T)

# CR 4 Feb 2018 -- Rather than remove this value, lets just make it NA
# data.use[data.use$BA.inc > 140 & !is.na(data.use$BA.inc),]
data.raw[!is.na(data.raw$BA.inc) & data.raw$BA.inc > 140, "BA.inc"] <- NA
summary(data.raw)
data.raw$Canopy.Class <- car::recode(data.raw$Canopy.Class, "'C' = 'Canopy'; 'D'='Canopy'")
data.raw$spp.cc <- as.factor(paste(data.raw$Species, data.raw$Canopy.Class, sep="."))
data.raw$spp.plot <- as.factor(paste(data.raw$Species, data.raw$PlotID, sep="."))

# trimming down the data used to be only living trees, and not any goofs or snags
dat.filter <- data.raw$Live.Dead=="LIVE" & !is.na(data.raw$Live.Dead) & !data.raw$Canopy.Class=="F" & !is.na(data.raw$Canopy.Class) & !is.na(data.raw$RW) & !is.na(data.raw$BA.inc)
data.use <- data.raw[dat.filter,]
summary(data.use)



# Getting rid of observations that have NAs in the important variables we want to use as predictors
predictors.all <- c("vpd.min", "vpd.max", "tmean", "precip", "Species", "dbh.recon", "Canopy.Class", "spp.plot", "Site.Code", "Year", "PlotID", "spp.cc") 
predictors.all[!predictors.all %in% names(data.use)]
data.use <- data.use[complete.cases(data.use[,predictors.all]),]

# Narrow it down to just the species we want
spp.use <- c("TSCA", "ACRU", "QURU", "FAGR")
data.use <- data.use[data.use$Species %in% spp.use,]
summary(data.use)

# Because we're going to do a log-transformation, we can't have a true 0, so lets just make it TINY
data.use[data.use$BA.inc==0, "BA.inc"] <- 1e-6
data.use$log.dbh <- log(data.use$dbh.recon)

summary(data.use)
# --------------------------------

# --------------------------------
# 4. Calculating the posterior estimates for the effects for graphing
# --------------------------------
source("0_Calculate_GAMM_Posteriors_Updated.R")
n=100

# ----------
# Species Climate Model: Species-based climatic effects
# ----------
yrs = min(data.use$Year):max(data.use$Year)
# Create a data frame with the factors that we care about (the "by" terms)
# Create a data frame with the numeric predictors we care about (the spline terms)
dat.clim.spp <- data.frame(Year=yrs,
                           dbh.recon=seq(min(data.use$dbh.recon), 
                                         max(data.use$dbh.recon), 
                                         length.out=length(yrs)),
                           tmean=seq(min(data.use$tmean), 
                                     max(data.use$tmean), 
                                     length.out=length(yrs)),
                           precip=seq(min(data.use$precip), 
                                      max(data.use$precip), 
                                      length.out=length(yrs)),
                           vpd.max=seq(min(data.use$vpd.max), 
                                       max(data.use$vpd.max), 
                                       length.out=length(yrs))
)
# dat.clim.spp <- merge(dat.clim.spp, fac.df, all=T)

# Add in dummy levels for factors we don't care about for this model
# dat.clim.spp$Species <- factor(dat.clim.spp$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
# dat.clim.spp$Site.Code <- substr(dat.clim.spp$PlotID, 1, 2)
# dat.clim.spp$Site.Code <- car::recode(dat.clim.spp$Site.Code, "'TP'='HF'")
# dat.clim.spp$TreeID <- data.use$TreeID[1]
# dat.clim.spp$Canopy.Class <- data.use$Canopy.Class[1]
# summary(dat.clim.spp)
# dim(dat.clim.spp)
clim.spp.out <- data.frame()
for(SPP in spp.use){
  print(paste("Processing species:", SPP))
  fac.df <- data.frame(Species= SPP,
                       PlotID = unique(data.use$PlotID[data.use$Species==SPP])
                       )
  dat.spp <- merge(dat.clim.spp, fac.df, all=T)
  dat.spp$Site.Code <- substr(dat.spp$PlotID, 1, 2)
  dat.spp$Site.Code <- car::recode(dat.spp$Site.Code, "'TP'='HF'")
  dat.spp$TreeID <- data.use$TreeID[data.use$Species==SPP][1]
  dat.spp$Canopy.Class <- data.use$Canopy.Class[data.use$Species==SPP][1]
  summary(dat.spp)
  
  
  # Load gam.clim.spp
  load(file.path(dir.out, paste0("gam_clim_spp_", SPP, ".Rdata")))
  
  # Create a data frame with just what we need for the clim.spp model
  gam.clim.spp$formula
  
  # Do the posterior predictions
  pred.clim.spp <- post.distns(model.gam=gam.clim.spp, n=n, newdata=dat.spp, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)
  pred.clim.spp$Species <- as.factor(SPP)
  # clim.spp.out$x <- as.numeric(clim.spp.out$x) # making x numeric; will make factors NA
  summary(pred.clim.spp)
  
  clim.spp.out <- rbind(clim.spp.out, pred.clim.spp)
}
  
clim.spp.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.spp.out[,c("mean", "lwr", "upr")])
summary(clim.spp.out)

png(file.path(dir.out, "YearResponse_Spp.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.spp.out[clim.spp.out$Effect=="Year",]) +
  ggtitle("Species Climate Model, Year Response") +
  facet_wrap(~PlotID, scales="free_y") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Species), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Species))
dev.off()

png(file.path(dir.out, "SizeResponse_Spp.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.spp.out[clim.spp.out$Effect=="dbh.recon",]) +
  ggtitle("Species Climate Model, Size Response") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Species), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Species))
dev.off()

png(file.path(dir.out, "ClimateResponse_Spp.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.spp.out[clim.spp.out$Effect%in% c("tmean", "precip", "vpd.max"),]) +
  ggtitle("Species Climate Model, Climate Response") +
  facet_wrap(~Effect, scales="free_x") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Species), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Species))
dev.off()
# ----------

# ----------
# Canopy Class Climate Model: Canopy-based climatic effects
# ----------
yrs = min(data.use$Year):max(data.use$Year)
# Create a data frame with the numeric predictors we care about (the spline terms)
dat.clim.cc <- data.frame(Year=yrs,
                          dbh.recon=seq(min(data.use$dbh.recon), 
                                        max(data.use$dbh.recon), 
                                        length.out=length(yrs)),
                          tmean=seq(min(data.use$tmean), 
                                    max(data.use$tmean), 
                                    length.out=length(yrs)),
                          precip=seq(min(data.use$precip), 
                                     max(data.use$precip), 
                                     length.out=length(yrs)),
                          vpd.max=seq(min(data.use$vpd.max), 
                                      max(data.use$vpd.max), 
                                      length.out=length(yrs))
)
# summary(dat.clim.cc)
# dim(dat.clim.cc)

# Do the posterior predictions
clim.cc.out <- data.frame()
for(SPP in spp.use){
  print(paste("Processing species:", SPP))
  fac.df <- data.frame(Species= SPP,
                       Canopy.Class=unique(data.use$Canopy.Class[data.use$Species==SPP]),
                       PlotID = rep(unique(data.use$PlotID[data.use$Species==SPP]), each=length(unique(data.use$Canopy.Class[data.use$Species==SPP])))
  )
  dat.spp <- merge(dat.clim.cc, fac.df, all=T)
  dat.spp$Site.Code <- substr(dat.spp$PlotID, 1, 2)
  dat.spp$Site.Code <- car::recode(dat.spp$Site.Code, "'TP'='HF'")
  dat.spp$TreeID <- data.use$TreeID[data.use$Species==SPP][1]
  # dat.spp$Canopy.Class <- data.use$Canopy.Class[data.use$Species==SPP][1]
  summary(dat.spp)
  
  
  # Load gam.clim.cc
  load(file.path(dir.out, paste0("gam_clim_cc_", SPP, ".Rdata")))
  
  # Create a data frame with just what we need for the clim.cc model
  gam.clim.cc$formula
  
  pred.clim.cc <- post.distns(model.gam=gam.clim.cc, n=n, newdata=dat.spp, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)
  pred.clim.cc[,c("Species", "Canopy.Class")] <- dat.spp[,c("Species", "Canopy.Class")]
  # pred.clim.cc$x <- as.numeric(pred.clim.cc$x) # making x numeric; will make factors NA
  # summary(pred.clim.cc)
  
  clim.cc.out <- rbind(clim.cc.out, pred.clim.cc)
  # summary(clim.cc.out)
}  
clim.cc.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.cc.out[,c("mean", "lwr", "upr")])
summary(clim.cc.out)

png(file.path(dir.out, "YearResponse_Canopy.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.cc.out[clim.cc.out$Effect=="Year",]) +
  ggtitle("Canopy Climate Mode, Year Effect") +
  facet_wrap(~PlotID, scales="free_y") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Species), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Species))
dev.off()

png(file.path(dir.out, "SizeResponse_Canopy.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.cc.out[clim.cc.out$Effect=="dbh.recon",]) +
  ggtitle("Canopy Climate Model") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Species), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Species))
dev.off()

png(file.path(dir.out, "CliamteResponse_Canopy.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.cc.out[clim.cc.out$Effect%in% c("tmean", "precip", "vpd.max"),]) +
  ggtitle("Canopy Climate Model") +
  facet_grid(Species~Effect, scales="free_x") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Canopy.Class), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Canopy.Class)) +
  coord_cartesian(ylim=c(0.5, 2))
dev.off()
# ----------

# --------------------------------

load(file.path(dir.out, "gam_clim_cc_ACRU.Rdata"))
mod.acru <- gam.clim.cc
summary(mod.acru)

load(file.path(dir.out, "gam_clim_cc_FAGR.Rdata"))
mod.fagr <- gam.clim.cc
summary(mod.fagr)

load(file.path(dir.out, "gam_clim_cc_QURU.Rdata"))
mod.quru <- gam.clim.cc
summary(mod.quru)

load(file.path(dir.out, "gam_clim_cc_TSCA.Rdata"))
mod.tsca <- gam.clim.cc
summary(mod.tsca)

