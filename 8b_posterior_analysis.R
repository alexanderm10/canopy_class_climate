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
source("0_Calculate_GAMM_Posteriors.R")
n=100

# ----------
# Null Model: No climatic effects
# ----------
# Load gam.null
load(file.path(dir.out, "gam_null.Rdata"))

# Create a data frame with just what we need for the null model
gam.null$formula
yrs = min(data.use$Year):max(data.use$Year)
# Create a data frame with the factors that we care about (the "by" terms)
fac.df <- data.frame(PlotID = rep(unique(data.use$PlotID), 
                                  each=length(unique(data.use$Species))),
                     Species= rep(unique(data.use$Species)))

# Create a data frame with the numeric predictors we care about (the spline terms)
dat.null <- data.frame(Year=yrs,
                       dbh.recon=seq(min(data.use$dbh.recon), 
                                     max(data.use$dbh.recon), 
                                     length.out=length(yrs)))
dat.null <- merge(dat.null, fac.df, all=T)

# Add in dummy levels for factors we don't care about for this model
dat.null$Species <- factor(dat.null$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
dat.null$Site.Code <- substr(dat.null$PlotID, 1, 2)
dat.null$Site.Code <- car::recode(dat.null$Site.Code, "'TP'='HF'")
dat.null$TreeID <- data.use$TreeID[1]
dat.null$Canopy.Class <- data.use$Canopy.Class[1]
summary(dat.null)
dim(dat.null)

# Do the posterior predictions
pred.null <- post.distns(model.gam=gam.null, model.name="null", n=n, newdata=dat.null, vars=c("dbh.recon", "Year"), terms=T)
null.out <- pred.null$ci
null.out[,c("Species")] <- dat.null[,c("Species")]
null.out$x <- as.numeric(null.out$x) # making x numeric; will make factors NA
summary(null.out)

null.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(null.out[,c("mean", "lwr", "upr")])
summary(null.out)

summary(null.out[null.out$Effect=="dbh.recon" & null.out$PlotID==null.out$PlotID[1],])

png(file.path(dir.out, "SizeResponse_0Null.png"), height=8, width=8, unit="in", res=120)
ggplot(data=null.out[null.out$Effect=="dbh.recon" & null.out$PlotID==null.out$PlotID[1],]) +
  ggtitle("Null Model") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Species), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Species))
dev.off()

png(file.path(dir.out, "YearResponse_0Null.png"), height=8, width=8, unit="in", res=120)
ggplot(data=null.out[null.out$Effect=="Year" & null.out$Species==null.out$Species[1],]) +
  ggtitle("Null Model") +
  facet_wrap(~PlotID, scales="free_y") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai))
dev.off()
# ----------



# ----------
# Base Climate Model: Global climatic effects
# ----------
# Load gam.clim.base
load(file.path(dir.out, "gam_clim_base.Rdata"))

# Create a data frame with just what we need for the clim.base model
gam.clim.base$formula
yrs = min(data.use$Year):max(data.use$Year)
# Create a data frame with the factors that we care about (the "by" terms)
fac.df <- data.frame(PlotID = rep(unique(data.use$PlotID), 
                                  each=length(unique(data.use$Species))),
                     Species= rep(unique(data.use$Species)))

# Create a data frame with the numeric predictors we care about (the spline terms)
dat.clim.base <- data.frame(Year=yrs,
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
dat.clim.base <- merge(dat.clim.base, fac.df, all=T)

# Add in dummy levels for factors we don't care about for this model
dat.clim.base$Species <- factor(dat.clim.base$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
dat.clim.base$Site.Code <- substr(dat.clim.base$PlotID, 1, 2)
dat.clim.base$Site.Code <- car::recode(dat.clim.base$Site.Code, "'TP'='HF'")
dat.clim.base$TreeID <- data.use$TreeID[1]
dat.clim.base$Canopy.Class <- data.use$Canopy.Class[1]
summary(dat.clim.base)
dim(dat.clim.base)

# Do the posterior predictions
pred.clim.base <- post.distns(model.gam=gam.clim.base, model.name="clim.base", n=n, newdata=dat.clim.base, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)
clim.base.out <- pred.clim.base$ci
clim.base.out[,c("Species")] <- dat.clim.base[,c("Species")]
clim.base.out$x <- as.numeric(clim.base.out$x) # making x numeric; will make factors NA
summary(clim.base.out)

clim.base.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.base.out[,c("mean", "lwr", "upr")])
summary(clim.base.out)

png(file.path(dir.out, "YearResponse_BaseClim.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.base.out[clim.base.out$Effect=="Year",]) +
  ggtitle("Base Climate Model") +
  facet_wrap(~PlotID, scales="free_y") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai))
dev.off()

png(file.path(dir.out, "SizeResponse_BaseClim.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.base.out[clim.base.out$Effect=="dbh.recon" & clim.base.out$PlotID==clim.base.out$PlotID[1],]) +
  ggtitle("Base Climate Model") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Species), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Species))
dev.off()

png(file.path(dir.out, "ClimateResponse_BaseClim.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.base.out[clim.base.out$Effect%in% c("tmean", "precip", "vpd.max") & clim.base.out$PlotID==clim.base.out$PlotID[1] & clim.base.out$Species==clim.base.out$Species[1],]) +
  ggtitle("Base Climate Model") +
  facet_wrap(~Effect, scales="free_x") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai))
dev.off()
# ----------

# ----------
# Species Climate Model: Species-based climatic effects
# ----------
# Load gam.clim.spp
load(file.path(dir.out, "gam_clim_spp.Rdata"))

# Create a data frame with just what we need for the clim.spp model
gam.clim.spp$formula
yrs = min(data.use$Year):max(data.use$Year)
# Create a data frame with the factors that we care about (the "by" terms)
fac.df <- data.frame(PlotID = rep(unique(data.use$PlotID), 
                                  each=length(unique(data.use$Species))),
                     Species= rep(unique(data.use$Species)))

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
dat.clim.spp <- merge(dat.clim.spp, fac.df, all=T)

# Add in dummy levels for factors we don't care about for this model
dat.clim.spp$Species <- factor(dat.clim.spp$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
dat.clim.spp$Site.Code <- substr(dat.clim.spp$PlotID, 1, 2)
dat.clim.spp$Site.Code <- car::recode(dat.clim.spp$Site.Code, "'TP'='HF'")
dat.clim.spp$TreeID <- data.use$TreeID[1]
dat.clim.spp$Canopy.Class <- data.use$Canopy.Class[1]
summary(dat.clim.spp)
dim(dat.clim.spp)

# Do the posterior predictions
pred.clim.spp <- post.distns(model.gam=gam.clim.spp, model.name="clim.spp", n=n, newdata=dat.clim.spp, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)
clim.spp.out <- pred.clim.spp$ci
clim.spp.out[,c("Species")] <- dat.clim.spp[,c("Species")]
clim.spp.out$x <- as.numeric(clim.spp.out$x) # making x numeric; will make factors NA
summary(clim.spp.out)

clim.spp.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.spp.out[,c("mean", "lwr", "upr")])
summary(clim.spp.out)

png(file.path(dir.out, "YearResponse_Spp.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.spp.out[clim.spp.out$Effect=="Year" & clim.spp.out$Species==clim.spp.out$Species[1],]) +
  ggtitle("Species Climate Model") +
  facet_wrap(~PlotID, scales="free_y") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai))
dev.off()

png(file.path(dir.out, "SizeResponse_Spp.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.spp.out[clim.spp.out$Effect=="dbh.recon" & clim.spp.out$PlotID==clim.spp.out$PlotID[1],]) +
  ggtitle("Species Climate Model") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Species), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Species))
dev.off()

png(file.path(dir.out, "ClimateResponse_Spp.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.spp.out[clim.spp.out$Effect%in% c("tmean", "precip", "vpd.max") & clim.spp.out$PlotID==clim.spp.out$PlotID[1],]) +
  ggtitle("Species Climate Model") +
  facet_wrap(~Effect, scales="free_x") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Species), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Species))
dev.off()
# ----------

# ----------
# Canopy Class Climate Model: Canopy-based climatic effects
# ----------
# Load gam.clim.cc
load(file.path(dir.out, "gam_clim_cc.Rdata"))

# Create a data frame with just what we need for the clim.cc model
gam.clim.cc$formula
yrs = min(data.use$Year):max(data.use$Year)
# Create a data frame with the factors that we care about (the "by" terms)
fac.df <- data.frame(PlotID = rep(unique(data.use$PlotID), 
                                  each=length(unique(data.use$Species))*length(unique(data.use$Canopy.Class))),
                     Species= rep(unique(data.use$Species)),
                     Canopy.Class = rep(unique(data.use$Canopy.Class), each=length(unique(data.use$Species))))

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
dat.clim.cc <- merge(dat.clim.cc, fac.df, all=T)

# Add in dummy levels for factors we don't care about for this model
dat.clim.cc$Species <- factor(dat.clim.cc$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
dat.clim.cc$Site.Code <- substr(dat.clim.cc$PlotID, 1, 2)
dat.clim.cc$Site.Code <- car::recode(dat.clim.cc$Site.Code, "'TP'='HF'")
dat.clim.cc$TreeID <- data.use$TreeID[1]
summary(dat.clim.cc)
dim(dat.clim.cc)

# Do the posterior predictions
pred.clim.cc <- post.distns(model.gam=gam.clim.cc, model.name="clim.cc", n=n, newdata=dat.clim.cc, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)
clim.cc.out <- pred.clim.cc$ci
clim.cc.out[,c("Species", "Canopy.Class")] <- dat.clim.cc[,c("Species", "Canopy.Class")]
clim.cc.out$x <- as.numeric(clim.cc.out$x) # making x numeric; will make factors NA
summary(clim.cc.out)

clim.cc.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.cc.out[,c("mean", "lwr", "upr")])
summary(clim.cc.out)

png(file.path(dir.out, "YearResponse_Canopy.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.cc.out[clim.cc.out$Effect=="Year" & clim.cc.out$Canopy.Class==clim.cc.out$Canopy.Class[1] & clim.cc.out$Species==clim.cc.out$Species[1],]) +
  ggtitle("Canopy Climate Model") +
  facet_wrap(~PlotID, scales="free_y") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai))
dev.off()

png(file.path(dir.out, "SizeResponse_Canopy.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.cc.out[clim.cc.out$Effect=="dbh.recon" & clim.cc.out$PlotID==clim.cc.out$PlotID[1],]) +
  ggtitle("Canopy Climate Model") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Species), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Species))
dev.off()

png(file.path(dir.out, "CliamteResponse_Canopy.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.cc.out[clim.cc.out$Effect%in% c("tmean", "precip", "vpd.max") & clim.cc.out$PlotID==clim.cc.out$PlotID[1],]) +
  ggtitle("Canopy Climate Model") +
  facet_wrap(~Effect, scales="free_x") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Canopy.Class), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Canopy.Class))
dev.off()
# ----------

# ----------
# Species x Canopy Climate Model: Species & Canopy-based climatic effects: single term
# ----------
# Load gam.clim.spp.cc
load(file.path(dir.out, "gam_clim_spp.cc.Rdata"))

# Create a data frame with just what we need for the clim.spp.cc model
gam.clim.spp.cc$formula
yrs = min(data.use$Year):max(data.use$Year)
# Create a data frame with the factors that we care about (the "by" terms)
fac.df <- data.frame(PlotID = rep(unique(data.use$PlotID), 
                                  each=length(unique(data.use$Species))*length(unique(data.use$Canopy.Class))),
                     Species= rep(unique(data.use$Species)),
                     Canopy.Class = rep(unique(data.use$Canopy.Class), each=length(unique(data.use$Species))))
fac.df$spp.cc <- paste(fac.df$Species, fac.df$Canopy.Class, sep=".")

# Create a data frame with the numeric predictors we care about (the spline terms)
dat.clim.spp.cc <- data.frame(Year=yrs,
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
dat.clim.spp.cc <- merge(dat.clim.spp.cc, fac.df, all=T)

# Add in dummy levels for factors we don't care about for this model
dat.clim.spp.cc$Species <- factor(dat.clim.spp.cc$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
dat.clim.spp.cc$Site.Code <- substr(dat.clim.spp.cc$PlotID, 1, 2)
dat.clim.spp.cc$Site.Code <- car::recode(dat.clim.spp.cc$Site.Code, "'TP'='HF'")
dat.clim.spp.cc$TreeID <- data.use$TreeID[1]
summary(dat.clim.spp.cc)
dim(dat.clim.spp.cc)

# Do the posterior predictions
pred.clim.spp.cc <- post.distns(model.gam=gam.clim.spp.cc, model.name="clim.spp.cc", n=n, newdata=dat.clim.spp.cc, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)
clim.spp.cc.out <- pred.clim.spp.cc$ci
clim.spp.cc.out[,c("Species", "Canopy.Class", "spp.cc")] <- dat.clim.spp.cc[,c("Species", "Canopy.Class", "spp.cc")]
clim.spp.cc.out$x <- as.numeric(clim.spp.cc.out$x) # making x numeric; will make factors NA
summary(clim.spp.cc.out)

clim.spp.cc.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.spp.cc.out[,c("mean", "lwr", "upr")])
summary(clim.spp.cc.out)

png(file.path(dir.out, "YearResponse_Spp.Canopy.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.spp.cc.out[clim.spp.cc.out$Effect=="Year" & clim.spp.cc.out$Canopy.Class==clim.spp.cc.out$Canopy.Class[1] & clim.spp.cc.out$Species==clim.spp.cc.out$Species[1],]) +
  ggtitle("Species-Canopy Climate Model") +
  facet_wrap(~PlotID, scales="free_y") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai))
dev.off()

png(file.path(dir.out, "SizeResponse_Spp.Canopy.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.spp.cc.out[clim.spp.cc.out$Effect=="dbh.recon" & clim.spp.cc.out$PlotID==clim.spp.cc.out$PlotID[1],]) +
  ggtitle("Species-Canopy Climate Model") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Species), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Species))
dev.off()

png(file.path(dir.out, "ClimateResponse_Spp.Canopy.png"), height=8, width=8, unit="in", res=120)
ggplot(data=clim.spp.cc.out[clim.spp.cc.out$Effect%in% c("tmean", "precip", "vpd.max") & clim.spp.cc.out$PlotID==clim.spp.cc.out$PlotID[1],]) +
  ggtitle("Species-Canopy Climate Model") +
  facet_grid(Species~Effect, scales="free_x") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Canopy.Class), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Canopy.Class))
dev.off()
# ----------


# ----------
# Species + Canopy Climate Model: Species & Canopy-based climatic effects: additive terms
# ----------
# Load gam.all.var
load(file.path(dir.out, "gam_all_variables.Rdata"))

# Create a data frame with just what we need for the all.var model
gam.all.var$formula
yrs = min(data.use$Year):max(data.use$Year)
# Create a data frame with the factors that we care about (the "by" terms)
fac.df <- data.frame(PlotID = rep(unique(data.use$PlotID), 
                                  each=length(unique(data.use$Species))*length(unique(data.use$Canopy.Class))),
                     Species= rep(unique(data.use$Species)),
                     Canopy.Class = rep(unique(data.use$Canopy.Class), each=length(unique(data.use$Species))))
fac.df$spp.cc <- paste(fac.df$Species, fac.df$Canopy.Class, sep=".")

# Create a data frame with the numeric predictors we care about (the spline terms)
dat.all.var <- data.frame(Year=yrs,
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
dat.all.var <- merge(dat.all.var, fac.df, all=T)

# Add in dummy levels for factors we don't care about for this model
dat.all.var$Species <- factor(dat.all.var$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
dat.all.var$Site.Code <- substr(dat.all.var$PlotID, 1, 2)
dat.all.var$Site.Code <- car::recode(dat.all.var$Site.Code, "'TP'='HF'")
dat.all.var$TreeID <- data.use$TreeID[1]
summary(dat.all.var)
dim(dat.all.var)

# Do the posterior predictions
pred.all.var <- post.distns(model.gam=gam.all.var, model.name="all.var", n=n, newdata=dat.all.var, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)
all.var.out <- pred.all.var$ci
all.var.out[,c("Species", "Canopy.Class", "spp.cc")] <- dat.all.var[,c("Species", "Canopy.Class", "spp.cc")]
all.var.out$x <- as.numeric(all.var.out$x) # making x numeric; will make factors NA
summary(all.var.out)

all.var.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(all.var.out[,c("mean", "lwr", "upr")])
summary(all.var.out)

png(file.path(dir.out, "YearResponse_Spp_Canopy_All.png"), height=8, width=8, unit="in", res=120)
ggplot(data=all.var.out[all.var.out$Effect=="Year" & all.var.out$Canopy.Class==all.var.out$Canopy.Class[1] & all.var.out$Species==all.var.out$Species[1],]) +
  ggtitle("Species + Canopy ('All') Climate Model") +
  facet_wrap(~PlotID, scales="free_y") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai))
dev.off()

png(file.path(dir.out, "SizeResponse_Spp_Canopy_All.png"), height=8, width=8, unit="in", res=120)
ggplot(data=all.var.out[all.var.out$Effect=="dbh.recon" & all.var.out$PlotID==all.var.out$PlotID[1],]) +
  ggtitle("Species + Canopy ('All') Climate Model") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Species), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Species))
dev.off()

png(file.path(dir.out, "ClimateResponse_Spp_Canopy_All.png"), height=8, width=8, unit="in", res=120)
ggplot(data=all.var.out[all.var.out$Effect%in% c("tmean", "precip", "vpd.max") & all.var.out$PlotID==all.var.out$PlotID[1],]) +
  ggtitle("Species + Canopy ('All') Climate Model") +
  facet_grid(Species~Effect, scales="free_x") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Canopy.Class), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Canopy.Class))
dev.off()
# ----------

# --------------------------------
