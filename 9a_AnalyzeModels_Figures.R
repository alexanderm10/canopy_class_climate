# --------------------------------
# Header & Description
# --------------------------------
# Analyses of ALL models generated in script 8a
# Steps:
# 1. Format Data (lines 21-64)
# 2. Generate Figures & do posterior analyses on models
# --------------------------------

library(mgcv)
library(ggplot2)
library(car)

# Set up a directory where to save things & not overwrite past efforts
dir.out <- "processed_data/gam_results_VarDecomp"
dir.create(dir.out, recursive=T, showWarnings = F)

path.google <- "/Volumes/GoogleDrive/My Drive/Manuscripts/Alexander_CanopyClimateResponse/canopy_and_climate/manuscript/Ecology (submit 2019-10)/Revision 2 2020-04/"
dir.figs <- file.path(path.google, "figures")
dir.create(dir.figs, recursive = T)

source("0_GraphEffects.R")
source("0_Calculate_GAMM_Posteriors_Updated.R")
source("0_Calculate_GAMM_Derivs.R")

n=100

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

# --------------------------------
# --------------------------------


# --------------------------------
# A few quick hard-coded summaries
# --------------------------------
model.comp <- read.csv(file.path(dir.out, "ModComparison_Full.csv"))
# model.comp$Description <- c("null", "naive", "species", "canopy", "pseudo-interactive", "hypothesis")

# mean(model.comp$dev.exp); sd(model.comp$dev.exp)
# round(range(model.comp$dev.exp), 3)
# --------------------------------



# --------------------------------
# 4. Calculating the posterior estimates for the effects for graphing
# --------------------------------

# ----------
# Naive Climate Model: Global climatic effects
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
clim.base.out <- post.distns(model.gam=gam.clim.base, n=n, newdata=dat.clim.base, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)
# clim.base.out <- pred.clim.base$ci
clim.base.out[,c("Species")] <- dat.clim.base[,c("Species")]
clim.base.out$x <- as.numeric(clim.base.out$x) # making x numeric; will make factors NA
summary(clim.base.out)

clim.base.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.base.out[,c("mean", "lwr", "upr")])
summary(clim.base.out)

# Trim down to just the areas present for each species/plot etc:
# Species DBH range
for(SPP in unique(dat.clim.base$Species)){
  dbh.min <- min(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  dbh.max <- max(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  
  clim.base.out[clim.base.out$Effect=="dbh.recon" & clim.base.out$Species==paste(SPP) & (clim.base.out$x<dbh.min | clim.base.out$x>dbh.max),c("mean.bai", "lwr.bai", "upr.bai")] <- NA
}


# Site
for(SITE in unique(dat.clim.base$Site.Code)){
  yr.min <- min(data.use[data.use$Site.Code==SITE,"Year"])
  yr.max <- max(data.use[data.use$Site.Code==SITE,"Year"])
  
  clim.base.out[clim.base.out$Effect=="Year" & clim.base.out$Site.Code==SITE & (clim.base.out$x<yr.min | clim.base.out$x>yr.max),c("mean.bai", "lwr.bai", "upr.bai")] <- NA
}


tiff(file.path(dir.figs, "SupplementalFigure05_NaiveClim_SizeEffect.tiff"), height=4.5, width=3, unit="in", res=600)
plot.size(dat.plot = clim.base.out[clim.base.out$PlotID==clim.base.out$PlotID[1],])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure05_NaiveClim_SizeEffect.pdf"), height=4.5, width=3)
plot.size(dat.plot = clim.base.out[clim.base.out$PlotID==clim.base.out$PlotID[1],])
dev.off()


tiff(file.path(dir.figs, "SupplementalFigure06_NaiveClim_YearEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.year(dat.plot=clim.base.out[clim.base.out$Species==clim.base.out$Species[1],])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure06_NaiveClim_YearEffect.pdf"), height=6, width=6)
plot.year(dat.plot=clim.base.out[clim.base.out$Species==clim.base.out$Species[1],])
dev.off()

tiff(file.path(dir.figs, "SupplementalFigure07_NaiveClim_ClimateEffects.tiff"), height=6, width=6, unit="in", res=600)
plot.climate(dat.plot=clim.base.out[clim.base.out$Effect%in% c("tmean", "precip", "vpd.max") & clim.base.out$PlotID==clim.base.out$PlotID[1] & clim.base.out$Species==clim.base.out$Species[1],], canopy=F, species=F)
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure07_NaiveClim_ClimateEffects.pdf"), height=6, width=6)
plot.climate(dat.plot=clim.base.out[clim.base.out$Effect%in% c("tmean", "precip", "vpd.max") & clim.base.out$PlotID==clim.base.out$PlotID[1] & clim.base.out$Species==clim.base.out$Species[1],], canopy=F, species=F)
dev.off()

# ----------

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
deriv.spp.out <- list(ci=data.frame(),
                      sims=data.frame())
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
  deriv.clim.spp <- calc.derivs(model.gam=gam.clim.spp, newdata=dat.spp, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), return.sims=T)
  
  pred.clim.spp$Species <- as.factor(SPP)
  # clim.spp.out$x <- as.numeric(clim.spp.out$x) # making x numeric; will make factors NA
  summary(pred.clim.spp)
  
  deriv.clim.spp$sims <- deriv.clim.spp$sims[,1:100]
  deriv.clim.spp$ci$Species <- as.factor(SPP)
  
  clim.spp.out <- rbind(clim.spp.out, pred.clim.spp)
  deriv.spp.out$sims <- rbind(deriv.spp.out$sims, deriv.clim.spp$sims)
  deriv.spp.out$ci <- rbind(deriv.spp.out$ci, deriv.clim.spp$ci)
}
clim.spp.out$Species <- factor(clim.spp.out$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
deriv.spp.out$ci$Species <- factor(deriv.spp.out$ci$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))

clim.spp.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.spp.out[,c("mean", "lwr", "upr")])
summary(clim.spp.out)

# Trim down to just the areas present for each species/plot etc:
# Species DBH range
for(SPP in unique(dat.clim.spp$Species)){
  dbh.min <- min(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  dbh.max <- max(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  
  rows.na <- which(clim.spp.out$Effect=="dbh.recon" & clim.spp.out$Species==paste(SPP) & (clim.spp.out$x<dbh.min | clim.spp.out$x>dbh.max))
  clim.spp.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.spp.out$ci[rows.na,c("dbh.recon", "mean", "lwr", "upr", "sig")] <- NA
  deriv.spp.out$sims[rows.na,] <- NA
}


# Site
for(SITE in unique(dat.clim.spp$Site.Code)){
  yr.min <- min(data.use[data.use$Site.Code==SITE,"Year"])
  yr.max <- max(data.use[data.use$Site.Code==SITE,"Year"])
  
  rows.na <- which(clim.spp.out$Effect=="Year" & clim.spp.out$Site.Code==SITE & (clim.spp.out$x<yr.min | clim.spp.out$x>yr.max))
  clim.spp.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.spp.out$ci[rows.na,c("Year", "mean", "lwr", "upr", "sig")] <- NA
  deriv.spp.out$sims[rows.na,] <- NA
  
}

# Climate
for(CC in unique(clim.spp.out$Species)){
  rows.cc <- which(data.use$Species==paste(CC))
  tmean.min <- min(data.use[rows.cc,"tmean"])
  tmean.max <- max(data.use[rows.cc,"tmean"])
  precip.min <- min(data.use[rows.cc,"precip"])
  precip.max <- max(data.use[rows.cc,"precip"])
  vpd.min <- min(data.use[rows.cc,"vpd.max"])
  vpd.max <- max(data.use[rows.cc,"vpd.max"])
  
  rows.na <- which(clim.spp.out$Effect=="tmean" & clim.spp.out$Species==CC & (clim.spp.out$x<tmean.min | clim.spp.out$x>tmean.max))
  clim.spp.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.spp.out$ci[rows.na,c("tmean", "mean", "lwr", "upr", "sig")] <- NA
  deriv.spp.out$sims[rows.na,] <- NA
  
  
  rows.na <- which(clim.spp.out$Effect=="precip" & clim.spp.out$Species==CC & (clim.spp.out$x<precip.min | clim.spp.out$x>precip.max))
  clim.spp.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.spp.out$ci[rows.na,c("precip", "mean", "lwr", "upr", "sig")] <- NA
  deriv.spp.out$sims[rows.na,] <- NA
  
  rows.na <- which(clim.spp.out$Effect=="vpd.max" & clim.spp.out$Species==CC & (clim.spp.out$x<vpd.min | clim.spp.out$x>vpd.max))
  clim.spp.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.spp.out$ci[rows.na,c("vpd.max", "mean", "lwr", "upr", "sig")] <- NA
  deriv.spp.out$sims[rows.na,] <- NA
  
}
summary(clim.spp.out)
summary(deriv.spp.out)


tiff(file.path(dir.figs, "SupplementalFigure01_SppClim_SizeEffect.tiff"), height=4.5, width=3, unit="in", res=600)
plot.size(dat.plot=clim.spp.out)
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure01_SppClim_SizeEffect.pdf"), height=4.5, width=3)
plot.size(dat.plot=clim.spp.out[,])
dev.off()


tiff(file.path(dir.figs, "SupplementalFigure02_SppClim_YearEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.year(dat.plot=clim.spp.out[,])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure02_SppClim_YearEffect.pdf"), height=6, width=6)
plot.year(dat.plot=clim.spp.out[,])
dev.off()


tiff(file.path(dir.figs, "Figure2_SppClim_ClimateEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.climate(dat.plot=clim.spp.out[clim.spp.out$Effect%in% c("tmean", "precip", "vpd.max"),], canopy=F, species=T)
dev.off()

pdf(file.path(dir.figs, "Figure2_SppClim_ClimateEffect.pdf"), height=6, width=6)
plot.climate(dat.plot=clim.spp.out[clim.spp.out$Effect%in% c("tmean", "precip", "vpd.max"),], canopy=F, species=T)
dev.off()

# Getting numbers for the manuscript
summary(deriv.spp.out)


# "The two northern-distributed and late-successional species Tsuga canadensis and Fagus grandifolia increased growth with high precipitation (0.03 ± 0.01 and 0.05 ± 0.01 %BAI/mm)."
mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T)

mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T)



# "Both species also so overall reduced growth at warmer temperatures, with Tsuga canadensis sensitive to temperatures below 16.3˚C (-7.88 ± 2.18) and Fagus grandifolia showing a mean sensitivity of -4.26 ± 1.97 %BAI/˚C across the full temperature range. "
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig),"tmean"])

mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T)

summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig),"tmean"])

mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="tmean",1:100], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="tmean",1:100], 2, mean, na.rm=T)*100, na.rm=T)




# "In contrast, Quercus rubra displays nearly opposite responses with higher growth under warm conditions (mean slope = XX±XX) and reduced growth in wet conditions (mean slope = XX±XX).  "
mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="tmean",1:100], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="tmean",1:100], 2, mean, na.rm=T)*100, na.rm=T)

mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T)


# "Acer rubrum shows optimal growth in moderate conditions with peak growth at XX˚C and XX mm precipitation"
mean(clim.spp.out[!is.na(clim.spp.out$mean.bai) & clim.spp.out$Species=="ACRU" & clim.spp.out$Effect=="tmean" & clim.spp.out$mean.bai==max(clim.spp.out$mean.bai[clim.spp.out$Species=="ACRU" & clim.spp.out$Effect=="tmean"], na.rm=T), "x"])
mean(clim.spp.out[!is.na(clim.spp.out$mean.bai) & clim.spp.out$Species=="ACRU" & clim.spp.out$Effect=="precip" & clim.spp.out$mean.bai==max(clim.spp.out$mean.bai[clim.spp.out$Species=="ACRU" & clim.spp.out$Effect=="precip"], na.rm=T), "x"])

# "VPDmax responses among species ranged from insensitive in Fagus grandifolia (mean slope = XX±XX) to highly sensitive in Quercus rubra (mean slope = XX±XX)"
mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="vpd.max",], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="vpd.max",], 2, mean, na.rm=T)*100, na.rm=T)


mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="vpd.max",], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="vpd.max",], 2, mean, na.rm=T)*100, na.rm=T)

# ----------


# ----------
# Pseudo-Interactive model: Species x Canopy Climate Model: Species & Canopy-based climatic effects: single term
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
deriv.cc.out <- list(ci=data.frame(),
                     sims=data.frame())
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
  deriv.clim.cc <- calc.derivs(model.gam=gam.clim.cc, newdata=dat.spp, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), return.sims = T)
  
  pred.clim.cc[,c("Species", "Canopy.Class")] <- dat.spp[,c("Species", "Canopy.Class")]
  deriv.clim.cc$ci[,c("Species", "Canopy.Class")] <- dat.spp[,c("Species", "Canopy.Class")]
  deriv.clim.cc$sims <- deriv.clim.cc$sims[,1:100]

  clim.cc.out <- rbind(clim.cc.out, pred.clim.cc)
  deriv.cc.out$ci <- rbind(deriv.cc.out$ci, deriv.clim.cc$ci)
  deriv.cc.out$sims <- rbind(deriv.cc.out$sims, deriv.clim.cc$sims)
  # summary(clim.cc.out)
}  
clim.cc.out$Species <- factor(clim.cc.out$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
deriv.cc.out$ci$Species <- factor(deriv.cc.out$ci$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
clim.cc.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.cc.out[,c("mean", "lwr", "upr")])
summary(clim.cc.out)


# Trim down to just the areas present for each species/plot etc:
# Species DBH range
for(SPP in unique(clim.cc.out$Species)){
  dbh.min <- min(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  dbh.max <- max(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  
  rows.na <- which(clim.cc.out$Effect=="dbh.recon" & clim.cc.out$Species==paste(SPP) & (clim.cc.out$x<dbh.min | clim.cc.out$x>dbh.max))
  clim.cc.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.cc.out$sims[rows.na,] <- NA
  deriv.cc.out$ci[rows.na,c("dbh.recon", "mean", "lwr", "upr", "sig")] <- NA
  deriv.cc.out$sims[rows.na,] <- NA
  
}

# Site
for(SITE in unique(clim.cc.out$Site.Code)){
  yr.min <- min(data.use[data.use$Site.Code==SITE,"Year"])
  yr.max <- max(data.use[data.use$Site.Code==SITE,"Year"])
  
  rows.na <- which(clim.cc.out$Effect=="Year" & clim.cc.out$Site.Code==SITE & (clim.cc.out$x<yr.min | clim.cc.out$x>yr.max))
  clim.cc.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  pred.clim.cc$sims[rows.na,] <- NA
  deriv.cc.out$ci[rows.na,c("Year", "mean", "lwr", "upr", "sig")] <- NA
  deriv.cc.out$sims[rows.na,] <- NA
  
}

# Climate
for(SPP in unique(clim.cc.out$Species)){
  for(CC in unique(clim.cc.out$Canopy.Class)){
    rows.cc <- which(data.use$Species==SPP & data.use$Canopy.Class==paste(CC))
    tmean.min <- min(data.use[rows.cc,"tmean"])
    tmean.max <- max(data.use[rows.cc,"tmean"])
    precip.min <- min(data.use[rows.cc,"precip"])
    precip.max <- max(data.use[rows.cc,"precip"])
    vpd.min <- min(data.use[rows.cc,"vpd.max"])
    vpd.max <- max(data.use[rows.cc,"vpd.max"])
    
    rows.na <- which(clim.cc.out$Effect=="tmean" & clim.cc.out$Species==SPP & clim.cc.out$Canopy.Class==CC & (clim.cc.out$x<tmean.min | clim.cc.out$x>tmean.max))
    if(length(rows.na)>0){
      clim.cc.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
      deriv.cc.out$sims[rows.na,] <- NA
      deriv.cc.out$ci[rows.na,c("tmean", "mean", "lwr", "upr", "sig")] <- NA
      deriv.cc.out$sims[rows.na,] <- NA
    }
    
    rows.na <- which(clim.cc.out$Effect=="precip" & clim.cc.out$Species==SPP & clim.cc.out$Canopy.Class==CC & (clim.cc.out$x<precip.min | clim.cc.out$x>precip.max))
    if(length(rows.na)>0){
      clim.cc.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
      deriv.cc.out$sims[rows.na,] <- NA
      deriv.cc.out$ci[rows.na,c("tmean", "mean", "lwr", "upr", "sig")] <- NA
      deriv.cc.out$sims[rows.na,] <- NA
    }
    
    rows.na <- which(clim.cc.out$Effect=="vpd.max" & clim.cc.out$Species==SPP & clim.cc.out$Canopy.Class==CC & (clim.cc.out$x<vpd.min | clim.cc.out$x>vpd.max))
    if(length(rows.na)>0){
      clim.cc.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
      deriv.cc.out$sims[rows.na,] <- NA
      deriv.cc.out$ci[rows.na,c("tmean", "mean", "lwr", "upr", "sig")] <- NA
      deriv.cc.out$sims[rows.na,] <- NA
    }
    
  }
}
clim.cc.out$Canopy.Class <- recode(clim.cc.out$Canopy.Class, "'Canopy'='Overstory'; 'I'='Middle'; 'U'='Understory'")
clim.cc.out$Canopy.Class <- factor(clim.cc.out$Canopy.Class, levels= c("Overstory", "Middle", "Understory"))
summary(clim.cc.out)
summary(deriv.cc.out)

tiff(file.path(dir.figs, "SupplementalFigure03_HypothesisClim_SizeEffect.tiff"), height=4.5, width=3, unit="in", res=600)
plot.size(clim.cc.out[,])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure03_HypothesisClim_SizeEffect.pdf"), height=4.5, width=3)
plot.size(clim.cc.out)
dev.off()


tiff(file.path(dir.figs, "SupplementalFigure04_HypothesisClim_YearEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.year(clim.cc.out)
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure04_HypothesisClim_YearEffect.pdf"), height=6, width=6)
plot.year(clim.cc.out)
dev.off()



tiff(file.path(dir.figs, "Figure3_CanopyClassClim_ClimateEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.climate(clim.cc.out[,], canopy=T, species=T)
dev.off()

pdf(file.path(dir.figs, "Figure3_CanopyClassClim_ClimateEffect.pdf"), height=6, width=6)
plot.climate(clim.cc.out[,], canopy=T, species=T)
dev.off()

#Now pasting in the results
#"For example, Tsuga canadensis continues to display the most reduced growth with warming temperatures below 16˚C than other species across overstory (mean slope = XX±XX), middle (mean slope = XX±XX), and understory trees (mean slope = XX±XX)."
mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean<16,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean<16,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean<16,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean<16,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean<16,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean<16,]*100, 2, mean, na.rm=T), na.rm=T)


# "Similarly, overstory Fagus grandifolia had a linear, positve effect of precipitation on growth that was more sensitive than observed in the species-only model (mean slope = XX±XX), which is balanced by the understory trees of the same species have a mostly non-significant precipitation effect (mean slope = XX±XX)."
mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T) 

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T) 


# "Quercus rubra continued to show the strongest VPDmax responses across species with a mean effect across canopy strata of XX±XX for the observed VPD range."
mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU"  & deriv.cc.out$ci$var=="vpd.max" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU"  & deriv.cc.out$ci$var=="vpd.max" ,]*100, 2, mean, na.rm=T), na.rm=T)


#######################
# Temperature
#######################
# "Within individual species, understory trees had temperature sensitivities that ranged from farily similar to overstory in Quercus rubra to over 17 times more sensitive in Fagus grandifolia (Figure 4). "
mean(abs(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean","mean"])/abs(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean","mean"]), na.rm=T)

mean(abs(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean","mean"])/abs(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean","mean"]), na.rm=T)

mean(abs(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean","mean"])/abs(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean","mean"]), na.rm=T)

mean(abs(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean","mean"])/abs(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean","mean"]), na.rm=T)

# "Overall, understory trees benefited more from cooler conditions with observed relative growth of XX ± XX %BAI at temperatures less than 16 ˚C compared to XX ± XX %BAI for overstory trees over the same range." 
mean(apply(pred.clim.cc$sims[clim.cc.out$Species=="QURU" & clim.cc.out$Canopy.Class=="Understory" & clim.cc.out$Effect=="tmean" & clim.cc.out$x<16 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.clim.cc$sims[clim.cc.out$Species=="QURU" & clim.cc.out$Canopy.Class=="Understory" & clim.cc.out$Effect=="tmean" & clim.cc.out$x<16 ,]*100, 2, mean, na.rm=T), na.rm=T)


mean(apply(pred.clim.cc$sims[clim.cc.out$Species=="QURU" & clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="tmean" & clim.cc.out$x<16 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.clim.cc$sims[clim.cc.out$Species=="QURU" & clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="tmean" & clim.cc.out$x<16 ,]*100, 2, mean, na.rm=T), na.rm=T)


# "Shapes of temperature sensitivity was most similar in Acer rubrum where overstory and middle canopy strata had similar optimal temperatures of 18.8˚C and 18.5˚C, respecitvely, and understory trees had their optimal growth at 15.4."
mean(clim.cc.out[clim.cc.out$Species=="ACRU" & clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="tmean" &  !is.na(clim.cc.out$mean.bai) & clim.cc.out$mean.bai==max(clim.cc.out$mean.bai[clim.cc.out$Species=="ACRU" & clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="tmean" ], na.rm=T),"x"], na.rm=T)

mean(clim.cc.out[clim.cc.out$Species=="ACRU" & clim.cc.out$Canopy.Class=="Middle" & clim.cc.out$Effect=="tmean" &  !is.na(clim.cc.out$mean.bai) & clim.cc.out$mean.bai==max(clim.cc.out$mean.bai[clim.cc.out$Species=="ACRU" & clim.cc.out$Canopy.Class=="Middle" & clim.cc.out$Effect=="tmean" ], na.rm=T),"x"], na.rm=T)

mean(clim.cc.out[clim.cc.out$Species=="ACRU" & clim.cc.out$Canopy.Class=="Understory" & clim.cc.out$Effect=="tmean" &  !is.na(clim.cc.out$mean.bai) & clim.cc.out$mean.bai==max(clim.cc.out$mean.bai[clim.cc.out$Species=="ACRU" & clim.cc.out$Canopy.Class=="Understory" & clim.cc.out$Effect=="tmean" ], na.rm=T),"x"], na.rm=T)

# "In contrast, the lack of temperature sensitivity in overstory Fagus grandifolia (XX ± XXX %BAI/˚C) coincided with a mean temperature sensitivity of XX ± XX %BAI/˚C in the understory."
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean",])


mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T)


# "Overstory Quercus rubra trees displayed a nearly linear, positive effects of temperature on growth (mean slope = XX±XX) that was not reflected in the understory."
mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T)

# "Understory Quercus rubra were insensitive to temperature below X˚C, above which growth was reduced as a rate of XX±XX %BAI/˚C."
temp.min <- max(deriv.cc.out$ci$tmean[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$var=="tmean" & is.na(deriv.cc.out$ci$sig)], na.rm=T)
summary(clim.cc.out[clim.cc.out$Species=="QURU" & clim.cc.out$Canopy.Class=="Understory" & clim.cc.out$Effect=="tmean" & clim.cc.out$x>temp.min & !is.na(clim.cc.out$mean.bai),], na.rm=T)

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean>temp.min & !is.na(deriv.cc.out$ci$tmean),]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean>temp.min & !is.na(deriv.cc.out$ci$tmean),]*100, 2, mean, na.rm=T), na.rm=T)

# "Overstory Tsuga canadensis trees displayed weakly negative temperature sensitivity below XXXX ˚C (XX±XX %BAI/˚C) and slightly positive sensitivity above XX ˚C (XX ± XX %BAI/˚C)"
temp.min <- min(deriv.cc.out$ci$tmean[deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$var=="tmean" & is.na(deriv.cc.out$ci$sig)], na.rm=T)
temp.max <- max(deriv.cc.out$ci$tmean[deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$var=="tmean" & is.na(deriv.cc.out$ci$sig)], na.rm=T)


mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean<temp.min & !is.na(deriv.cc.out$ci$tmean),]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean<temp.min & !is.na(deriv.cc.out$ci$tmean),]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean>temp.max & !is.na(deriv.cc.out$ci$tmean),]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean>temp.max & !is.na(deriv.cc.out$ci$tmean),]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$var=="tmean",]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T)

temp.u <- range(deriv.cc.out$ci$tmean[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$var=="tmean" ], na.rm=T)

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean>=temp.u[1] & deriv.cc.out$ci$tmean<=temp.u[2],]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$var=="tmean" & deriv.cc.out$ci$tmean>=temp.u[1] & deriv.cc.out$ci$tmean<=temp.u[2],]*100, 2, mean, na.rm=T), na.rm=T)
#######################



#######################
# Precipitation
#######################
# "Consistent with the species-only model, overstory Fagus grandifolia and Tsuga canadensis showed had positive precipitation effects with mean sensitivities of XX±XX %BA/mm and XX±XX %BAI/mm, respectively."
mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T)

# ".  Both understory trees of these same species saw increase growth with more precipitation for the dry end of observed conditions (0.05 ± 0.2 %BAI/mm for Fagus below 523 mm; 0.04 ± 0.02 %BAI/mm for Tsuga below 439 mm). However, both species showed decreased growth in the understory at higher levels of precipitation (-0.06 ± 0.02 %BAI/mm for Fagus; -0.08 ± 0.02 for Tsuga)."  
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" ,])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" ,])

ns.tsca.min <- min(deriv.cc.out$ci$precip[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$var=="precip"  & is.na(deriv.cc.out$ci$sig)], na.rm=T)
ns.fagr.min <- min(deriv.cc.out$ci$precip[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$var=="precip"  & is.na(deriv.cc.out$ci$sig)], na.rm=T)
ns.tsca.max <- max(deriv.cc.out$ci$precip[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$var=="precip"  & is.na(deriv.cc.out$ci$sig)], na.rm=T)
ns.fagr.max <- max(deriv.cc.out$ci$precip[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$var=="precip"  & is.na(deriv.cc.out$ci$sig)], na.rm=T)

ns.fagr.min
mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" &  deriv.cc.out$ci$precip<ns.fagr.min,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" &  deriv.cc.out$ci$precip<ns.fagr.min ,]*100, 2, mean, na.rm=T), na.rm=T)

ns.tsca.min
mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" &  deriv.cc.out$ci$precip<ns.tsca.min,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" &  deriv.cc.out$ci$precip<ns.tsca.min ,]*100, 2, mean, na.rm=T), na.rm=T)

ns.fagr.max
mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" &  deriv.cc.out$ci$precip>ns.fagr.max,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" &  deriv.cc.out$ci$precip>ns.fagr.max ,]*100, 2, mean, na.rm=T), na.rm=T)
ns.tsca.max
mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" &  deriv.cc.out$ci$precip>ns.tsca.max,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" &  deriv.cc.out$ci$precip>ns.tsca.max ,]*100, 2, mean, na.rm=T), na.rm=T)

"All canopy strata of Quercus rubra showed similar patterns in precipitation responses, but understory trees were X times more sensitive than the overstory at precipitation greater than 567 mm (XX±XX %BAI/mm and XX±XX %BAI/mm, respectively)"
mean(abs(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip","mean"])/abs(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip","mean"]), na.rm=T)

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T)


mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T)


# "Above XXX mm precipitation, growth of overstory Acer rubrum was insensitive to precipitation (mean slope = XX±XX) even though understory trees displayed strong negative responses (mean slope = XX±XX). "
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" ,])
ns.acru.min <- min(deriv.cc.out$ci$precip[deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$var=="precip"  & is.na(deriv.cc.out$ci$sig)], na.rm=T)
ns.acru.max <- max(deriv.cc.out$ci$precip[deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$var=="precip"  & is.na(deriv.cc.out$ci$sig)], na.rm=T)

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & deriv.cc.out$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & deriv.cc.out$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T)


# "Mean growth rates of overstory trees reviving less than 400 mm precipitation ranged from -15.2 ± 3.7 %BAI in Fagus grandifolia to -6.9 ± 3.2 %BAI in Quercus rubra."
mean(apply(pred.clim.cc$sims[clim.cc.out$Species=="TSCA" & clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="precip" & clim.cc.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.clim.cc$sims[clim.cc.out$Species=="TSCA" & clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="precip" & clim.cc.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(pred.clim.cc$sims[clim.cc.out$Species=="FAGR" & clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="precip" & clim.cc.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.clim.cc$sims[clim.cc.out$Species=="FAGR" & clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="precip" & clim.cc.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(pred.clim.cc$sims[clim.cc.out$Species=="ACRU" & clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="precip" & clim.cc.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.clim.cc$sims[clim.cc.out$Species=="ACRU" & clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="precip" & clim.cc.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(pred.clim.cc$sims[clim.cc.out$Species=="QURU" & clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="precip" & clim.cc.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.clim.cc$sims[clim.cc.out$Species=="QURU" & clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="precip" & clim.cc.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T)


# "Across all species, understory trees showed a mean increase in growth relative to the overstory counterparts of XXX±XXX %BAI. "
mean(apply(pred.clim.cc$sims[clim.cc.out$Canopy.Class=="Understory" & clim.cc.out$Effect=="precip" & clim.cc.out$x<400 ,]*100 - pred.clim.cc$sims[clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="precip" & clim.cc.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.clim.cc$sims[clim.cc.out$Canopy.Class=="Understory" & clim.cc.out$Effect=="precip" & clim.cc.out$x<400 ,]*100 - pred.clim.cc$sims[clim.cc.out$Canopy.Class=="Overstory" & clim.cc.out$Effect=="precip" & clim.cc.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T); 
#######################


#######################
# VPD
#######################
# "For all species, overstory and middle-canopy trees had similar responses to VPDmax (Fig. 4) with trends ranging from mostly insensitive in overstory Fagus grandiolia (0.06 ± 0.24 %BAI/kPa) strongly negative in overstory Quercus rubra (-0.83 ± 0.16 %BAI/kPa). " 
mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & deriv.cc.out$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & deriv.cc.out$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & deriv.cc.out$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & deriv.cc.out$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T)
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" ,])


mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & deriv.cc.out$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & deriv.cc.out$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & deriv.cc.out$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & deriv.cc.out$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T)



# "Understory Tsuga canadensis and Acer rubrum were both insensitive to VPDmax across the full range of observed conditions. "
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max" ,])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max" ,])




# "Understory Quercus rubra is half as sensitive to VPDmax as its overstory counterparts below 72 kPa, above which point growth of the understory is not affected by variability in VPDmax."
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max" ,])
ns.quru <- range(deriv.cc.out$ci$vpd.max[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$var=="vpd.max"  & is.na(deriv.cc.out$ci$sig)], na.rm=T)
uquru <- range(deriv.cc.out$ci$vpd.max[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$var=="vpd.max"], na.rm=T)

mean(abs(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max" & deriv.cc.out$ci$vpd.max<ns.quru[1],"mean"]), na.rm=T)/mean(abs(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max"  & deriv.cc.out$ci$vpd.max<ns.quru[1] & deriv.cc.out$ci$vpd.max>=uquru[1],"mean"]), na.rm=T)


mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max" & deriv.cc.out$ci$vpd.max<ns.quru[1] ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max" & deriv.cc.out$ci$vpd.max<ns.quru[1] ,]*100, 2, mean, na.rm=T), na.rm=T)

# "In contrast, understory Fagus grandifolia is only sensitive to the higher range of VPDmax (XX ± XX %BAI/kPA above XXX kPa)."
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max" ,])
ns.fagr <- range(deriv.cc.out$ci$vpd.max[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$var=="vpd.max"  & is.na(deriv.cc.out$ci$sig)], na.rm=T)
ufagr <- range(deriv.cc.out$ci$vpd.max[deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$var=="vpd.max"], na.rm=T)

mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max" & deriv.cc.out$ci$vpd.max>ns.fagr[2] ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max" & deriv.cc.out$ci$vpd.max>ns.fagr[2],]*100, 2, mean, na.rm=T), na.rm=T)

#######################

# ----------

# --------------------------------
