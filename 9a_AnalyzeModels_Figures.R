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

path.google <- "/Volumes/GoogleDrive/My Drive/Manuscripts/Alexander_CanopyClimateResponse/canopy_and_climate/manuscript/Ecology (submit 2019-10)/Revision 1 2019-12/"
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
model.comp$Description <- c("null", "naive", "species", "canopy", "pseudo-interactive", "hypothesis")

mean(model.comp$dev.exp); sd(model.comp$dev.exp)
round(range(model.comp$dev.exp), 3)
# --------------------------------



# --------------------------------
# 4. Calculating the posterior estimates for the effects for graphing
# --------------------------------

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
null.out <- post.distns(model.gam=gam.null, n=n, newdata=dat.null, vars=c("dbh.recon", "Year"), terms=T)
# null.out <- pred.null$ci
null.out[,c("Species")] <- dat.null[,c("Species")]
null.out$x <- as.numeric(null.out$x) # making x numeric; will make factors NA
summary(null.out)

null.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(null.out[,c("mean", "lwr", "upr")])
summary(null.out)

# Trim down to just the areas present for each species/plot etc:
# Species DBH range
for(SPP in unique(null.out$Species)){
  dbh.min <- min(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  dbh.max <- max(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  
  null.out[null.out$Effect=="dbh.recon" & null.out$Species==paste(SPP) & (null.out$x<dbh.min | null.out$x>dbh.max),c("mean.bai", "lwr.bai", "upr.bai")] <- NA
}


# Site
for(SITE in unique(null.out$Site.Code)){
  yr.min <- min(data.use[data.use$Site.Code==SITE,"Year"])
  yr.max <- max(data.use[data.use$Site.Code==SITE,"Year"])
  
  null.out[null.out$Effect=="Year" & null.out$Site.Code==SITE & (null.out$x<yr.min | null.out$x>yr.max),c("mean.bai", "lwr.bai", "upr.bai")] <- NA
}

summary(null.out[null.out$Effect=="dbh.recon" & null.out$PlotID==null.out$PlotID[1],])

tiff(file.path(dir.figs, "SupplementalFigure03_0Null_SizeEffects.tiff"), height=4.5, width=3, unit="in", res=600)
plot.size(dat.plot= null.out[null.out$PlotID==null.out$PlotID[1],])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure03_0Null_SizeEffects.pdf"), height=4.5, width=3)
plot.size(dat.plot= null.out[null.out$PlotID==null.out$PlotID[1],])
dev.off()


tiff(file.path(dir.figs, "SupplementalFigure04_0Null_YearEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.year(dat.plot= null.out[null.out$Species==null.out$Species[1],])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure04_0Null_YearEffect.pdf"), height=6, width=6)
plot.year(dat.plot= null.out[null.out$Species==null.out$Species[1],])
dev.off()
# ----------



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
pred.clim.spp <- post.distns(model.gam=gam.clim.spp, n=n, newdata=dat.clim.spp, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)
deriv.clim.spp <- calc.derivs(model.gam=gam.clim.spp, newdata=dat.clim.spp, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), return.sims=T)

deriv.clim.spp$sims <- deriv.clim.spp$sims[,1:100]

clim.spp.out <- pred.clim.spp
clim.spp.out[,c("Species")] <- dat.clim.spp[,c("Species")]
clim.spp.out$x <- as.numeric(clim.spp.out$x) # making x numeric; will make factors NA
summary(clim.spp.out)

clim.spp.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.spp.out[,c("mean", "lwr", "upr")])
summary(clim.spp.out)

# Trim down to just the areas present for each species/plot etc:
# Species DBH range
for(SPP in unique(dat.clim.spp$Species)){
  dbh.min <- min(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  dbh.max <- max(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  
  rows.na <- which(clim.spp.out$Effect=="dbh.recon" & clim.spp.out$Species==paste(SPP) & (clim.spp.out$x<dbh.min | clim.spp.out$x>dbh.max))
  clim.spp.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.clim.spp$ci[rows.na,c("dbh.recon", "mean", "lwr", "upr", "sig")] <- NA
  deriv.clim.spp$sims[rows.na,] <- NA
}


# Site
for(SITE in unique(dat.clim.spp$Site.Code)){
  yr.min <- min(data.use[data.use$Site.Code==SITE,"Year"])
  yr.max <- max(data.use[data.use$Site.Code==SITE,"Year"])
  
  rows.na <- which(clim.spp.out$Effect=="Year" & clim.spp.out$Site.Code==SITE & (clim.spp.out$x<yr.min | clim.spp.out$x>yr.max))
  clim.spp.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.clim.spp$ci[rows.na,c("Year", "mean", "lwr", "upr", "sig")] <- NA
  deriv.clim.spp$sims[rows.na,] <- NA
  
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
  deriv.clim.spp$ci[rows.na,c("tmean", "mean", "lwr", "upr", "sig")] <- NA
  deriv.clim.spp$sims[rows.na,] <- NA
  
  
  rows.na <- which(clim.spp.out$Effect=="precip" & clim.spp.out$Species==CC & (clim.spp.out$x<precip.min | clim.spp.out$x>precip.max))
  clim.spp.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.clim.spp$ci[rows.na,c("precip", "mean", "lwr", "upr", "sig")] <- NA
  deriv.clim.spp$sims[rows.na,] <- NA
  
  rows.na <- which(clim.spp.out$Effect=="vpd.max" & clim.spp.out$Species==CC & (clim.spp.out$x<vpd.min | clim.spp.out$x>vpd.max))
  clim.spp.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.clim.spp$ci[rows.na,c("vpd.max", "mean", "lwr", "upr", "sig")] <- NA
  deriv.clim.spp$sims[rows.na,] <- NA
  
}
summary(clim.spp.out)
summary(deriv.clim.spp$ci)

tiff(file.path(dir.figs, "SupplementalFigure08_SppClim_SizeEffect.tiff"), height=4.5, width=3, unit="in", res=600)
plot.size(dat.plot=clim.spp.out[ clim.spp.out$PlotID==clim.spp.out$PlotID[1],])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure08_SppClim_SizeEffect.pdf"), height=4.5, width=3)
plot.size(dat.plot=clim.spp.out[ clim.spp.out$PlotID==clim.spp.out$PlotID[1],])
dev.off()


tiff(file.path(dir.figs, "SupplementalFigure09_SppClim_YearEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.year(dat.plot=clim.spp.out[ clim.spp.out$Species==clim.spp.out$Species[1],])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure09_SppClim_YearEffect.pdf"), height=6, width=6)
plot.year(dat.plot=clim.spp.out[ clim.spp.out$Species==clim.spp.out$Species[1],])
dev.off()


tiff(file.path(dir.figs, "Figure2_SppClim_ClimateEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.climate(dat.plot=clim.spp.out[clim.spp.out$Effect%in% c("tmean", "precip", "vpd.max")  & clim.spp.out$PlotID==clim.spp.out$PlotID[1],], canopy=F, species=T)
dev.off()

pdf(file.path(dir.figs, "Figure2_SppClim_ClimateEffect.pdf"), height=6, width=6)
plot.climate(dat.plot=clim.spp.out[clim.spp.out$Effect%in% c("tmean", "precip", "vpd.max")  & clim.spp.out$PlotID==clim.spp.out$PlotID[1],], canopy=F, species=T)
dev.off()

# Getting numbers for the manuscript
summary(deriv.clim.spp)


# "The two northern-distributed and late-successional species Tsuga canadensis and Fagus grandifolia increased growth with high precipitation (0.03 ± 0.01 and 0.05 ± 0.01 %BAI/mm)."
mean(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="TSCA" & deriv.clim.spp$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="TSCA" & deriv.clim.spp$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T)

mean(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="FAGR" & deriv.clim.spp$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="FAGR" & deriv.clim.spp$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T)



# "Both species also so overall reduced growth at warmer temperatures, with Tsuga canadensis sensitive to temperatures below 16.3˚C (-7.88 ± 2.18) and Fagus grandifolia showing a mean sensitivity of -4.26 ± 1.97 %BAI/˚C across the full temperature range. "
summary(deriv.clim.spp$ci[deriv.clim.spp$ci$Species=="TSCA" & deriv.clim.spp$ci$var=="tmean" & !is.na(deriv.clim.spp$ci$sig),"tmean"])

mean(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="TSCA" & deriv.clim.spp$ci$var=="tmean" & !is.na(deriv.clim.spp$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="TSCA" & deriv.clim.spp$ci$var=="tmean" & !is.na(deriv.clim.spp$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T)

summary(deriv.clim.spp$ci[deriv.clim.spp$ci$Species=="FAGR" & deriv.clim.spp$ci$var=="tmean" & !is.na(deriv.clim.spp$ci$sig),"tmean"])

mean(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="FAGR" & deriv.clim.spp$ci$var=="tmean",1:100], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="FAGR" & deriv.clim.spp$ci$var=="tmean",1:100], 2, mean, na.rm=T)*100, na.rm=T)




# "In contrast, Quercus rubra displays nearly opposite responses with higher growth under warm conditions (mean slope = XX±XX) and reduced growth in wet conditions (mean slope = XX±XX).  "
mean(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="QURU" & deriv.clim.spp$ci$var=="tmean",1:100], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="QURU" & deriv.clim.spp$ci$var=="tmean",1:100], 2, mean, na.rm=T)*100, na.rm=T)

mean(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="QURU" & deriv.clim.spp$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="QURU" & deriv.clim.spp$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T)


# "Acer rubrum shows optimal growth in moderate conditions with peak growth at XX˚C and XX mm precipitation"
mean(clim.spp.out[!is.na(clim.spp.out$mean.bai) & clim.spp.out$Species=="ACRU" & clim.spp.out$Effect=="tmean" & clim.spp.out$mean.bai==max(clim.spp.out$mean.bai[clim.spp.out$Species=="ACRU" & clim.spp.out$Effect=="tmean"], na.rm=T), "x"])
mean(clim.spp.out[!is.na(clim.spp.out$mean.bai) & clim.spp.out$Species=="ACRU" & clim.spp.out$Effect=="precip" & clim.spp.out$mean.bai==max(clim.spp.out$mean.bai[clim.spp.out$Species=="ACRU" & clim.spp.out$Effect=="precip"], na.rm=T), "x"])

# "VPDmax responses among species ranged from insensitive in Fagus grandifolia (mean slope = XX±XX) to highly sensitive in Quercus rubra (mean slope = XX±XX)"
mean(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="FAGR" & deriv.clim.spp$ci$var=="vpd.max",], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="FAGR" & deriv.clim.spp$ci$var=="vpd.max",], 2, mean, na.rm=T)*100, na.rm=T)


mean(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="QURU" & deriv.clim.spp$ci$var=="vpd.max",], 2, mean, na.rm=T)*100, na.rm=T); sd(apply(deriv.clim.spp$sims[deriv.clim.spp$ci$Species=="QURU" & deriv.clim.spp$ci$var=="vpd.max",], 2, mean, na.rm=T)*100, na.rm=T)

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
pred.clim.cc <- post.distns(model.gam=gam.clim.cc, n=n, newdata=dat.clim.cc, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)
deriv.clim.cc <- calc.derivs(model.gam=gam.clim.cc, newdata=dat.clim.cc, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), return.sims=T)

deriv.clim.cc$sims <- deriv.clim.cc$sims[,1:100]
clim.cc.out <- pred.clim.cc
clim.cc.out[,c("Species", "Canopy.Class")] <- dat.clim.cc[,c("Species", "Canopy.Class")]
clim.cc.out$x <- as.numeric(clim.cc.out$x) # making x numeric; will make factors NA
summary(clim.cc.out)

clim.cc.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.cc.out[,c("mean", "lwr", "upr")])
summary(clim.cc.out)

# Trim down to just the areas present for each species/plot etc:
# Species DBH range
for(SPP in unique(dat.clim.cc$Species)){
  dbh.min <- min(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  dbh.max <- max(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  
  rows.na <- which(clim.cc.out$Effect=="dbh.recon" & clim.cc.out$Species==paste(SPP) & (clim.cc.out$x<dbh.min | clim.cc.out$x>dbh.max))
  clim.cc.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.clim.cc$ci[rows.na,c("dbh.recon", "mean", "lwr", "upr", "sig")] <- NA
  deriv.clim.cc$sims[rows.na,] <- NA
  
}



# Site
for(SITE in unique(dat.clim.cc$Site.Code)){
  yr.min <- min(data.use[data.use$Site.Code==SITE,"Year"])
  yr.max <- max(data.use[data.use$Site.Code==SITE,"Year"])
  
  rows.na <- which(clim.cc.out$Effect=="Year" & clim.cc.out$Site.Code==SITE & (clim.cc.out$x<yr.min | clim.cc.out$x>yr.max))
  clim.cc.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.clim.cc$ci[rows.na,c("Year", "mean", "lwr", "upr", "sig")] <- NA
  deriv.clim.cc$sims[rows.na,] <- NA
  
}

# Climate
for(CC in unique(clim.cc.out$Canopy.Class)){
  rows.cc <- which(data.use$Canopy.Class==paste(CC))
  tmean.min <- min(data.use[rows.cc,"tmean"])
  tmean.max <- max(data.use[rows.cc,"tmean"])
  precip.min <- min(data.use[rows.cc,"precip"])
  precip.max <- max(data.use[rows.cc,"precip"])
  vpd.min <- min(data.use[rows.cc,"vpd.max"])
  vpd.max <- max(data.use[rows.cc,"vpd.max"])
  
  rows.na <- which(clim.cc.out$Effect=="tmean" & clim.cc.out$Canopy.Class==CC & (clim.cc.out$x<tmean.min | clim.cc.out$x>tmean.max))
  clim.cc.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.clim.cc$ci[rows.na,c("Year", "mean", "lwr", "upr", "sig")] <- NA
  deriv.clim.cc$sims[rows.na,] <- NA
  
  rows.na <- which(clim.cc.out$Effect=="precip" & clim.cc.out$Canopy.Class==CC & (clim.cc.out$x<precip.min | clim.cc.out$x>precip.max))
  clim.cc.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.clim.cc$ci[rows.na,c("Year", "mean", "lwr", "upr", "sig")] <- NA
  deriv.clim.cc$sims[rows.na,] <- NA
  
  rows.na <- which(clim.cc.out$Effect=="vpd.max" & clim.cc.out$Canopy.Class==CC & (clim.cc.out$x<vpd.min | clim.cc.out$x>vpd.max))
  clim.cc.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.clim.cc$ci[rows.na,c("Year", "mean", "lwr", "upr", "sig")] <- NA
  deriv.clim.cc$sims[rows.na,] <- NA
  
  
}
clim.cc.out$Canopy.Class <- recode(clim.cc.out$Canopy.Class, "'Canopy'='Overstory'; 'I'='Middle'; 'U'='Understory'")
clim.cc.out$Canopy.Class <- factor(clim.cc.out$Canopy.Class, levels= c("Overstory", "Middle", "Understory"))
summary(clim.cc.out)
summary(deriv.clim.cc)

tiff(file.path(dir.figs, "SupplementalFigure10_CanopyClim_SizeEffect.tiff"), height=4.5, width=3, unit="in", res=600)
plot.size(dat.plot=clim.cc.out[clim.cc.out$PlotID==clim.cc.out$PlotID[1] & clim.cc.out$Canopy.Class==clim.cc.out$Canopy.Class[1],])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure10_CanopyClim_SizeEffect.pdf"), height=4.5, width=3)
plot.size(dat.plot=clim.cc.out[clim.cc.out$PlotID==clim.cc.out$PlotID[1] & clim.cc.out$Canopy.Class==clim.cc.out$Canopy.Class[1],])
dev.off()


tiff(file.path(dir.figs, "SupplementalFigure11_CanopyClim_YearEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.year(dat.plot=clim.cc.out[clim.cc.out$Canopy.Class==clim.cc.out$Canopy.Class[1] & clim.cc.out$Species==clim.cc.out$Species[1],])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure11_CanopyClim_YearEffect.pdf"), height=6, width=6)
plot.year(dat.plot=clim.cc.out[clim.cc.out$Canopy.Class==clim.cc.out$Canopy.Class[1] & clim.cc.out$Species==clim.cc.out$Species[1],])
dev.off()


tiff(file.path(dir.figs, "Figure3_CanopyClimate_ClimateEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.climate(dat.plot=clim.cc.out[clim.cc.out$Species==clim.cc.out$Species[1] & clim.cc.out$PlotID==clim.cc.out$PlotID[1],], species=F, canopy=T)
dev.off()

pdf(file.path(dir.figs, "Figure3_CanopyClimate_ClimateEffect.pdf"), height=6, width=6)
plot.climate(dat.plot=clim.cc.out[clim.cc.out$Species==clim.cc.out$Species[1] & clim.cc.out$PlotID==clim.cc.out$PlotID[1],], species=F, canopy=T)
dev.off()

# Getting numbers for results: 
# "Overstory trees display increased growth with warm summer temperatures (mean slope = XX±XX), understory trees show the opposite pattern with strongly reduced growth under the same conditions (mean slope = XX±XX)."
mean(apply(deriv.clim.cc$sims[deriv.clim.cc$ci$Canopy.Class=="Canopy" & deriv.clim.cc$ci$var=="tmean",]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.clim.cc$sims[deriv.clim.cc$ci$Canopy.Class=="Canopy" & deriv.clim.cc$ci$var=="tmean",]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.clim.cc$sims[deriv.clim.cc$ci$Canopy.Class=="U" & deriv.clim.cc$ci$var=="tmean",]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.clim.cc$sims[deriv.clim.cc$ci$Canopy.Class=="U" & deriv.clim.cc$ci$var=="tmean",]*100, 2, mean, na.rm=T), na.rm=T)

# "For precipitation, overstory trees show concerted growth increases (mean slope = XX±XX) up to XXX mm."
max.canopy <- max(deriv.clim.cc$ci$precip[deriv.clim.cc$ci$Canopy.Class=="Canopy" & deriv.clim.cc$ci$var=="precip" & !is.na(deriv.clim.cc$ci$sig) & deriv.clim.cc$ci$mean>0])
summary(deriv.clim.cc$ci[deriv.clim.cc$ci$Canopy.Class=="Canopy" & deriv.clim.cc$ci$var=="precip" & deriv.clim.cc$ci$precip>max.canopy & !is.na(deriv.clim.cc$ci$precip) & !is.na(deriv.clim.cc$ci$sig),])

mean(apply(deriv.clim.cc$sims[deriv.clim.cc$ci$Canopy.Class=="Canopy" & deriv.clim.cc$ci$var=="precip" & deriv.clim.cc$ci$precip<=max.canopy,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.clim.cc$sims[deriv.clim.cc$ci$Canopy.Class=="Canopy" & deriv.clim.cc$ci$var=="precip" & deriv.clim.cc$ci$precip<=max.canopy,]*100, 2, mean, na.rm=T), na.rm=T)




# "In contrast, understory trees display an overall negative growth response to increased precipitation (mean slope = XX±XX)."
mean(apply(deriv.clim.cc$sims[deriv.clim.cc$ci$Canopy.Class=="U" & deriv.clim.cc$ci$var=="precip",]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.clim.cc$sims[deriv.clim.cc$ci$Canopy.Class=="U" & deriv.clim.cc$ci$var=="precip",]*100, 2, mean, na.rm=T), na.rm=T)

# "Overstory trees display consistently reduced growth with increasing VPDmax (mean slope = XX±XX)."
summary(deriv.clim.cc$ci[deriv.clim.cc$ci$Canopy.Class=="Canopy" & deriv.clim.cc$ci$var=="vpd.max",])

mean(apply(deriv.clim.cc$sims[deriv.clim.cc$ci$Canopy.Class=="Canopy" & deriv.clim.cc$ci$var=="vpd.max",]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.clim.cc$sims[deriv.clim.cc$ci$Canopy.Class=="Canopy" & deriv.clim.cc$ci$var=="vpd.max",]*100, 2, mean, na.rm=T), na.rm=T)

summary(deriv.clim.cc[deriv.clim.cc$Canopy.Class=="U" & deriv.clim.cc$var=="vpd.max",])

# ----------

# ----------
# Pseudo-Interactive model: Species x Canopy Climate Model: Species & Canopy-based climatic effects: single term
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
pred.clim.spp.cc <- post.distns(model.gam=gam.clim.spp.cc, n=n, newdata=dat.clim.spp.cc, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)

clim.spp.cc.out <- pred.clim.spp.cc
clim.spp.cc.out[,c("Species", "Canopy.Class", "spp.cc")] <- dat.clim.spp.cc[,c("Species", "Canopy.Class", "spp.cc")]
clim.spp.cc.out$x <- as.numeric(clim.spp.cc.out$x) # making x numeric; will make factors NA
summary(clim.spp.cc.out)

clim.spp.cc.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.spp.cc.out[,c("mean", "lwr", "upr")])
summary(clim.spp.cc.out)

# Trim down to just the areas present for each species/plot etc:
# Species DBH range
for(SPP in unique(dat.clim.spp.cc$Species)){
  dbh.min <- min(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  dbh.max <- max(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  
  clim.spp.cc.out[clim.spp.cc.out$Effect=="dbh.recon" & clim.spp.cc.out$Species==paste(SPP) & (clim.spp.cc.out$x<dbh.min | clim.spp.cc.out$x>dbh.max),c("mean.bai", "lwr.bai", "upr.bai")] <- NA
}



# Site
for(SITE in unique(dat.clim.spp.cc$Site.Code)){
  yr.min <- min(data.use[data.use$Site.Code==SITE,"Year"])
  yr.max <- max(data.use[data.use$Site.Code==SITE,"Year"])
  
  clim.spp.cc.out[clim.spp.cc.out$Effect=="Year" & clim.spp.cc.out$Site.Code==SITE & (clim.spp.cc.out$x<yr.min | clim.spp.cc.out$x>yr.max),c("mean.bai", "lwr.bai", "upr.bai")] <- NA
}

# Climate
for(CC in unique(clim.spp.cc.out$spp.cc)){
  rows.cc <- which(data.use$spp.cc==paste(CC))
  tmean.min <- min(data.use[rows.cc,"tmean"])
  tmean.max <- max(data.use[rows.cc,"tmean"])
  precip.min <- min(data.use[rows.cc,"precip"])
  precip.max <- max(data.use[rows.cc,"precip"])
  vpd.min <- min(data.use[rows.cc,"vpd.max"])
  vpd.max <- max(data.use[rows.cc,"vpd.max"])
  
  clim.spp.cc.out[clim.spp.cc.out$Effect=="tmean" & clim.spp.cc.out$spp.cc==CC & (clim.spp.cc.out$x<tmean.min | clim.spp.cc.out$x>tmean.max),c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  clim.spp.cc.out[clim.spp.cc.out$Effect=="precip" & clim.spp.cc.out$spp.cc==CC & (clim.spp.cc.out$x<precip.min | clim.spp.cc.out$x>precip.max),c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  clim.spp.cc.out[clim.spp.cc.out$Effect=="vpd.max" & clim.spp.cc.out$spp.cc==CC & (clim.spp.cc.out$x<vpd.min | clim.spp.cc.out$x>vpd.max),c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  
}
clim.spp.cc.out$Canopy.Class <- car::recode(clim.spp.cc.out$Canopy.Class, "'Canopy'='Overstory'; 'I'='Middle'; 'U'='Understory'")
clim.spp.cc.out$Canopy.Class <- factor(clim.spp.cc.out$Canopy.Class, levels= c("Overstory", "Middle", "Understory"))
summary(clim.spp.cc.out)

tiff(file.path(dir.figs, "SupplementalFigure12_Pseudo-InteractiveClim_SizeEffect.tiff"), height=4.5, width=3, unit="in", res=600)
plot.size(dat.plot=clim.spp.cc.out[clim.spp.cc.out$PlotID==clim.spp.cc.out$PlotID[1],])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure12_Pseudo-InteractiveClim_SizeEffect.pdf"), height=4.5, width=3)
plot.size(dat.plot=clim.spp.cc.out[clim.spp.cc.out$PlotID==clim.spp.cc.out$PlotID[1],])
dev.off()


tiff(file.path(dir.figs, "SupplementalFigure13_Pseudo-InteractiveClim_YearEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.year(dat.plot=clim.spp.cc.out[clim.spp.cc.out$Canopy.Class==clim.spp.cc.out$Canopy.Class[1] & clim.spp.cc.out$Species==clim.spp.cc.out$Species[1],])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure13_Pseudo-InteractiveClim_YearEffect.pdf"), height=6, width=6)
plot.year(dat.plot=clim.spp.cc.out[clim.spp.cc.out$Canopy.Class==clim.spp.cc.out$Canopy.Class[1] & clim.spp.cc.out$Species==clim.spp.cc.out$Species[1],])
dev.off()


tiff(file.path(dir.figs, "SupplementalFigure14_Pseudo-InteractiveClim_ClimateEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.climate(dat.plot=clim.spp.cc.out[clim.spp.cc.out$PlotID==clim.spp.cc.out$PlotID[1],], species = T, canopy = T)
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure14_Pseudo-InteractiveClim_ClimateEffect.pdf"), height=6, width=6)
plot.climate(dat.plot=clim.spp.cc.out[clim.spp.cc.out$PlotID==clim.spp.cc.out$PlotID[1],], species = T, canopy = T)
dev.off()

# ----------


# ----------
# Hypothesis-driven model: Species + Canopy Climate Model: Species & Canopy-based climatic effects: additive terms
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
pred.all.var <- post.distns(model.gam=gam.all.var, n=n, newdata=dat.all.var, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T, return.sims = T)
deriv.all.var <- calc.derivs(model.gam=gam.all.var, newdata=dat.all.var, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), return.sims = T)

deriv.all.var$sims <- deriv.all.var$sims[,1:100]
pred.all.var$sims <- pred.all.var$sims[,5:ncol(pred.all.var$sims)]
all.var.out <- pred.all.var$ci
all.var.out[,c("Species", "Canopy.Class", "spp.cc")] <- dat.all.var[,c("Species", "Canopy.Class", "spp.cc")]
all.var.out$x <- as.numeric(all.var.out$x) # making x numeric; will make factors NA
summary(all.var.out)

all.var.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(all.var.out[,c("mean", "lwr", "upr")])
pred.all.var$sims <- exp(pred.all.var$sims)
summary(all.var.out)

# Trim down to just the areas present for each species/plot etc:
# Species DBH range
for(SPP in unique(all.var.out$Species)){
  dbh.min <- min(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  dbh.max <- max(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  
  rows.na <- which(all.var.out$Effect=="dbh.recon" & all.var.out$Species==paste(SPP) & (all.var.out$x<dbh.min | all.var.out$x>dbh.max))
  all.var.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  pred.all.var$sims[rows.na,] <- NA
  deriv.all.var$ci[rows.na,c("dbh.recon", "mean", "lwr", "upr", "sig")] <- NA
  deriv.all.var$sims[rows.na,] <- NA
  
}

# Site
for(SITE in unique(all.var.out$Site.Code)){
  yr.min <- min(data.use[data.use$Site.Code==SITE,"Year"])
  yr.max <- max(data.use[data.use$Site.Code==SITE,"Year"])
  
  rows.na <- which(all.var.out$Effect=="Year" & all.var.out$Site.Code==SITE & (all.var.out$x<yr.min | all.var.out$x>yr.max))
  all.var.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  pred.all.var$sims[rows.na,] <- NA
  deriv.all.var$ci[rows.na,c("Year", "mean", "lwr", "upr", "sig")] <- NA
  deriv.all.var$sims[rows.na,] <- NA
  
}

# Climate
for(CC in unique(all.var.out$spp.cc)){
  rows.cc <- which(data.use$spp.cc==paste(CC))
  tmean.min <- min(data.use[rows.cc,"tmean"])
  tmean.max <- max(data.use[rows.cc,"tmean"])
  precip.min <- min(data.use[rows.cc,"precip"])
  precip.max <- max(data.use[rows.cc,"precip"])
  vpd.min <- min(data.use[rows.cc,"vpd.max"])
  vpd.max <- max(data.use[rows.cc,"vpd.max"])
  
  rows.na <- which(all.var.out$Effect=="tmean" & all.var.out$spp.cc==CC & (all.var.out$x<tmean.min | all.var.out$x>tmean.max))
  all.var.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  pred.all.var$sims[rows.na,] <- NA
  deriv.all.var$ci[rows.na,c("tmean", "mean", "lwr", "upr", "sig")] <- NA
  deriv.all.var$sims[rows.na,] <- NA
  
  rows.na <- which(all.var.out$Effect=="precip" & all.var.out$spp.cc==CC & (all.var.out$x<precip.min | all.var.out$x>precip.max))
  all.var.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  pred.all.var$sims[rows.na,] <- NA
  deriv.all.var$ci[rows.na,c("precip", "mean", "lwr", "upr", "sig")] <- NA
  deriv.all.var$sims[rows.na,] <- NA
  
  rows.na <- which(all.var.out$Effect=="vpd.max" & all.var.out$spp.cc==CC & (all.var.out$x<vpd.min | all.var.out$x>vpd.max))
  all.var.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  pred.all.var$sims[rows.na,] <- NA
  deriv.all.var$ci[rows.na,c("vpd.max", "mean", "lwr", "upr", "sig")] <- NA
  deriv.all.var$sims[rows.na,] <- NA
  
}
all.var.out$Canopy.Class <- recode(all.var.out$Canopy.Class, "'Canopy'='Overstory'; 'I'='Middle'; 'U'='Understory'")
all.var.out$Canopy.Class <- factor(all.var.out$Canopy.Class, levels= c("Overstory", "Middle", "Understory"))
summary(all.var.out)
summary(deriv.all.var)

tiff(file.path(dir.figs, "SupplementalFigure15_HypothesisClim_SizeEffect.tiff"), height=4.5, width=3, unit="in", res=600)
plot.size(all.var.out[all.var.out$PlotID==all.var.out$PlotID[1],])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure15_HypothesisClim_SizeEffect.pdf"), height=4.5, width=3)
plot.size(all.var.out[all.var.out$PlotID==all.var.out$PlotID[1],])
dev.off()


tiff(file.path(dir.figs, "SupplementalFigure16_HypothesisClim_YearEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.year(all.var.out[all.var.out$Species==all.var.out$Species[1],])
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure16_HypothesisClim_YearEffect.pdf"), height=6, width=6)
plot.year(all.var.out[all.var.out$Species==all.var.out$Species[1],])
dev.off()



tiff(file.path(dir.figs, "Figure4_HypothesisClim_ClimateEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.climate(all.var.out[all.var.out$PlotID==all.var.out$PlotID[1],], canopy=T, species=T)
dev.off()

pdf(file.path(dir.figs, "Figure4_HypothesisClim_ClimateEffect.pdf"), height=6, width=6)
plot.climate(all.var.out[all.var.out$PlotID==all.var.out$PlotID[1],], canopy=T, species=T)
dev.off()

#Now pasting in the results
#"For example, Tsuga canadensis continues to display the most reduced growth with warming temperatures below 16˚C than other species across overstory (mean slope = XX±XX), middle (mean slope = XX±XX), and understory trees (mean slope = XX±XX)."
mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean<16,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean<16,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="I" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean<16,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="I" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean<16,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean<16,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean<16,]*100, 2, mean, na.rm=T), na.rm=T)


# "Similarly, overstory Fagus grandifolia had a linear, positve effect of precipitation on growth that was more sensitive than observed in the species-only model (mean slope = XX±XX), which is balanced by the understory trees of the same species have a mostly non-significant precipitation effect (mean slope = XX±XX)."
mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T) 

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T) 


# "Quercus rubra continued to show the strongest VPDmax responses across species with a mean effect across canopy strata of XX±XX for the observed VPD range."
mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="QURU"  & deriv.all.var$ci$var=="vpd.max" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="QURU"  & deriv.all.var$ci$var=="vpd.max" ,]*100, 2, mean, na.rm=T), na.rm=T)


#######################
# Temperature
#######################
# "Within individual species, understory trees had temperature sensitivities that ranged from farily similar to overstory in Quercus rubra to over 17 times more sensitive in Fagus grandifolia (Figure 4). "
mean(abs(deriv.all.var$ci[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="tmean","mean"])/abs(deriv.all.var$ci[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="tmean","mean"]), na.rm=T)

mean(abs(deriv.all.var$ci[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="tmean","mean"])/abs(deriv.all.var$ci[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="tmean","mean"]), na.rm=T)

mean(abs(deriv.all.var$ci[deriv.all.var$ci$Species=="ACRU" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="tmean","mean"])/abs(deriv.all.var$ci[deriv.all.var$ci$Species=="ACRU" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="tmean","mean"]), na.rm=T)

mean(abs(deriv.all.var$ci[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="tmean","mean"])/abs(deriv.all.var$ci[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="tmean","mean"]), na.rm=T)

# "Overall, understory trees benefited more from cooler conditions with observed relative growth of XX ± XX %BAI at temperatures less than 16 ˚C compared to XX ± XX %BAI for overstory trees over the same range." 
mean(apply(pred.all.var$sims[all.var.out$Species=="QURU" & all.var.out$Canopy.Class=="Understory" & all.var.out$Effect=="tmean" & all.var.out$x<16 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.all.var$sims[all.var.out$Species=="QURU" & all.var.out$Canopy.Class=="Understory" & all.var.out$Effect=="tmean" & all.var.out$x<16 ,]*100, 2, mean, na.rm=T), na.rm=T)


mean(apply(pred.all.var$sims[all.var.out$Species=="QURU" & all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="tmean" & all.var.out$x<16 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.all.var$sims[all.var.out$Species=="QURU" & all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="tmean" & all.var.out$x<16 ,]*100, 2, mean, na.rm=T), na.rm=T)


# "Shapes of temperature sensitivity was most similar in Acer rubrum where overstory and middle canopy strata had similar optimal temperatures of 18.8˚C and 18.5˚C, respecitvely, and understory trees had their optimal growth at 15.4."
mean(all.var.out[all.var.out$Species=="ACRU" & all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="tmean" &  !is.na(all.var.out$mean.bai) & all.var.out$mean.bai==max(all.var.out$mean.bai[all.var.out$Species=="ACRU" & all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="tmean" ], na.rm=T),"x"], na.rm=T)

mean(all.var.out[all.var.out$Species=="ACRU" & all.var.out$Canopy.Class=="Middle" & all.var.out$Effect=="tmean" &  !is.na(all.var.out$mean.bai) & all.var.out$mean.bai==max(all.var.out$mean.bai[all.var.out$Species=="ACRU" & all.var.out$Canopy.Class=="Middle" & all.var.out$Effect=="tmean" ], na.rm=T),"x"], na.rm=T)

mean(all.var.out[all.var.out$Species=="ACRU" & all.var.out$Canopy.Class=="Understory" & all.var.out$Effect=="tmean" &  !is.na(all.var.out$mean.bai) & all.var.out$mean.bai==max(all.var.out$mean.bai[all.var.out$Species=="ACRU" & all.var.out$Canopy.Class=="Understory" & all.var.out$Effect=="tmean" ], na.rm=T),"x"], na.rm=T)

# "In contrast, the lack of temperature sensitivity in overstory Fagus grandifolia (XX ± XXX %BAI/˚C) coincided with a mean temperature sensitivity of XX ± XX %BAI/˚C in the understory."
summary(deriv.all.var$ci[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="tmean",])


mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T)


# "Overstory Quercus rubra trees displayed a nearly linear, positive effects of temperature on growth (mean slope = XX±XX) that was not reflected in the understory."
mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T)

# "Understory Quercus rubra were insensitive to temperature below X˚C, above which growth was reduced as a rate of XX±XX %BAI/˚C."
temp.min <- max(deriv.all.var$ci$tmean[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$var=="tmean" & is.na(deriv.all.var$ci$sig)], na.rm=T)
summary(all.var.out[all.var.out$Species=="QURU" & all.var.out$Canopy.Class=="Understory" & all.var.out$Effect=="tmean" & all.var.out$x>temp.min & !is.na(all.var.out$mean.bai),], na.rm=T)

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean>temp.min & !is.na(deriv.all.var$ci$tmean),]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean>temp.min & !is.na(deriv.all.var$ci$tmean),]*100, 2, mean, na.rm=T), na.rm=T)

# "Overstory Tsuga canadensis trees displayed weakly negative temperature sensitivity below XXXX ˚C (XX±XX %BAI/˚C) and slightly positive sensitivity above XX ˚C (XX ± XX %BAI/˚C)"
temp.min <- min(deriv.all.var$ci$tmean[deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$var=="tmean" & is.na(deriv.all.var$ci$sig)], na.rm=T)
temp.max <- max(deriv.all.var$ci$tmean[deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$var=="tmean" & is.na(deriv.all.var$ci$sig)], na.rm=T)


mean(apply(deriv.all.var$sims[deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean<temp.min & !is.na(deriv.all.var$ci$tmean),]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean<temp.min & !is.na(deriv.all.var$ci$tmean),]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean>temp.max & !is.na(deriv.all.var$ci$tmean),]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean>temp.max & !is.na(deriv.all.var$ci$tmean),]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$var=="tmean",]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$var=="tmean" ,]*100, 2, mean, na.rm=T), na.rm=T)

temp.u <- range(deriv.all.var$ci$tmean[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$var=="tmean" ], na.rm=T)

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean>=temp.u[1] & deriv.all.var$ci$tmean<=temp.u[2],]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$var=="tmean" & deriv.all.var$ci$tmean>=temp.u[1] & deriv.all.var$ci$tmean<=temp.u[2],]*100, 2, mean, na.rm=T), na.rm=T)
#######################



#######################
# Precipitation
#######################
# "Consistent with the species-only model, overstory Fagus grandifolia and Tsuga canadensis showed had positive precipitation effects with mean sensitivities of XX±XX %BA/mm and XX±XX %BAI/mm, respectively."
mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T)

# ".  Both understory trees of these same species saw increase growth with more precipitation for the dry end of observed conditions (0.05 ± 0.2 %BAI/mm for Fagus below 523 mm; 0.04 ± 0.02 %BAI/mm for Tsuga below 439 mm). However, both species showed decreased growth in the understory at higher levels of precipitation (-0.06 ± 0.02 %BAI/mm for Fagus; -0.08 ± 0.02 for Tsuga)."  
summary(deriv.all.var$ci[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" ,])
summary(deriv.all.var$ci[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" ,])

ns.tsca.min <- min(deriv.all.var$ci$precip[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$var=="precip"  & is.na(deriv.all.var$ci$sig)], na.rm=T)
ns.fagr.min <- min(deriv.all.var$ci$precip[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$var=="precip"  & is.na(deriv.all.var$ci$sig)], na.rm=T)
ns.tsca.max <- max(deriv.all.var$ci$precip[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$var=="precip"  & is.na(deriv.all.var$ci$sig)], na.rm=T)
ns.fagr.max <- max(deriv.all.var$ci$precip[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$var=="precip"  & is.na(deriv.all.var$ci$sig)], na.rm=T)

ns.fagr.min
mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" &  deriv.all.var$ci$precip<ns.fagr.min,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" &  deriv.all.var$ci$precip<ns.fagr.min ,]*100, 2, mean, na.rm=T), na.rm=T)

ns.tsca.min
mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" &  deriv.all.var$ci$precip<ns.tsca.min,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" &  deriv.all.var$ci$precip<ns.tsca.min ,]*100, 2, mean, na.rm=T), na.rm=T)

ns.fagr.max
mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" &  deriv.all.var$ci$precip>ns.fagr.max,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" &  deriv.all.var$ci$precip>ns.fagr.max ,]*100, 2, mean, na.rm=T), na.rm=T)
ns.tsca.max
mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" &  deriv.all.var$ci$precip>ns.tsca.max,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" &  deriv.all.var$ci$precip>ns.tsca.max ,]*100, 2, mean, na.rm=T), na.rm=T)

"All canopy strata of Quercus rubra showed similar patterns in precipitation responses, but understory trees were X times more sensitive than the overstory at precipitation greater than 567 mm (XX±XX %BAI/mm and XX±XX %BAI/mm, respectively)"
mean(abs(deriv.all.var$ci[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip","mean"])/abs(deriv.all.var$ci[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="precip","mean"]), na.rm=T)

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T)


mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="precip" ,]*100, 2, mean, na.rm=T), na.rm=T)


# "Above XXX mm precipitation, growth of overstory Acer rubrum was insensitive to precipitation (mean slope = XX±XX) even though understory trees displayed strong negative responses (mean slope = XX±XX). "
summary(deriv.all.var$ci[deriv.all.var$ci$Species=="ACRU" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="precip" ,])
ns.acru.min <- min(deriv.all.var$ci$precip[deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$Species=="ACRU" & deriv.all.var$ci$var=="precip"  & is.na(deriv.all.var$ci$sig)], na.rm=T)
ns.acru.max <- max(deriv.all.var$ci$precip[deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$Species=="ACRU" & deriv.all.var$ci$var=="precip"  & is.na(deriv.all.var$ci$sig)], na.rm=T)

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="ACRU" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" & deriv.all.var$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="ACRU" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="precip" & deriv.all.var$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T)


# "Mean growth rates of overstory trees reviving less than 400 mm precipitation ranged from -15.2 ± 3.7 %BAI in Fagus grandifolia to -6.9 ± 3.2 %BAI in Quercus rubra."
mean(apply(pred.all.var$sims[all.var.out$Species=="TSCA" & all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="precip" & all.var.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.all.var$sims[all.var.out$Species=="TSCA" & all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="precip" & all.var.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(pred.all.var$sims[all.var.out$Species=="FAGR" & all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="precip" & all.var.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.all.var$sims[all.var.out$Species=="FAGR" & all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="precip" & all.var.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(pred.all.var$sims[all.var.out$Species=="ACRU" & all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="precip" & all.var.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.all.var$sims[all.var.out$Species=="ACRU" & all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="precip" & all.var.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(pred.all.var$sims[all.var.out$Species=="QURU" & all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="precip" & all.var.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.all.var$sims[all.var.out$Species=="QURU" & all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="precip" & all.var.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T)


# "Across all species, understory trees showed a mean increase in growth relative to the overstory counterparts of XXX±XXX %BAI. "
mean(apply(pred.all.var$sims[all.var.out$Canopy.Class=="Understory" & all.var.out$Effect=="precip" & all.var.out$x<400 ,]*100 - pred.all.var$sims[all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="precip" & all.var.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(pred.all.var$sims[all.var.out$Canopy.Class=="Understory" & all.var.out$Effect=="precip" & all.var.out$x<400 ,]*100 - pred.all.var$sims[all.var.out$Canopy.Class=="Overstory" & all.var.out$Effect=="precip" & all.var.out$x<400 ,]*100, 2, mean, na.rm=T), na.rm=T); 
#######################


#######################
# VPD
#######################
# "For all species, overstory and middle-canopy trees had similar responses to VPDmax (Fig. 4) with trends ranging from mostly insensitive in overstory Fagus grandiolia (0.06 ± 0.24 %BAI/kPa) strongly negative in overstory Quercus rubra (-0.83 ± 0.16 %BAI/kPa). " 
mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="vpd.max" & deriv.all.var$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="vpd.max" & deriv.all.var$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="vpd.max" & deriv.all.var$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="vpd.max" & deriv.all.var$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T)
summary(deriv.all.var$ci[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="vpd.max" ,])


mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="ACRU" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="vpd.max" & deriv.all.var$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="ACRU" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="vpd.max" & deriv.all.var$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T)

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="vpd.max" & deriv.all.var$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="vpd.max" & deriv.all.var$ci$precip>=ns.acru.min ,]*100, 2, mean, na.rm=T), na.rm=T)



# "Understory Tsuga canadensis and Acer rubrum were both insensitive to VPDmax across the full range of observed conditions. "
summary(deriv.all.var$ci[deriv.all.var$ci$Species=="TSCA" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="vpd.max" ,])
summary(deriv.all.var$ci[deriv.all.var$ci$Species=="ACRU" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="vpd.max" ,])




# "Understory Quercus rubra is half as sensitive to VPDmax as its overstory counterparts below 72 kPa, above which point growth of the understory is not affected by variability in VPDmax."
summary(deriv.all.var$ci[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="vpd.max" ,])
ns.quru <- range(deriv.all.var$ci$vpd.max[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$var=="vpd.max"  & is.na(deriv.all.var$ci$sig)], na.rm=T)
uquru <- range(deriv.all.var$ci$vpd.max[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$var=="vpd.max"], na.rm=T)

mean(abs(deriv.all.var$ci[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="vpd.max" & deriv.all.var$ci$vpd.max<ns.quru[1],"mean"]), na.rm=T)/mean(abs(deriv.all.var$ci[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="Canopy" & deriv.all.var$ci$var=="vpd.max"  & deriv.all.var$ci$vpd.max<ns.quru[1] & deriv.all.var$ci$vpd.max>=uquru[1],"mean"]), na.rm=T)


mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="vpd.max" & deriv.all.var$ci$vpd.max<ns.quru[1] ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="QURU" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="vpd.max" & deriv.all.var$ci$vpd.max<ns.quru[1] ,]*100, 2, mean, na.rm=T), na.rm=T)

# "In contrast, understory Fagus grandifolia is only sensitive to the higher range of VPDmax (XX ± XX %BAI/kPA above XXX kPa)."
summary(deriv.all.var$ci[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="vpd.max" ,])
ns.fagr <- range(deriv.all.var$ci$vpd.max[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$var=="vpd.max"  & is.na(deriv.all.var$ci$sig)], na.rm=T)
ufagr <- range(deriv.all.var$ci$vpd.max[deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$var=="vpd.max"], na.rm=T)

mean(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="vpd.max" & deriv.all.var$ci$vpd.max>ns.fagr[2] ,]*100, 2, mean, na.rm=T), na.rm=T); sd(apply(deriv.all.var$sims[deriv.all.var$ci$Species=="FAGR" & deriv.all.var$ci$Canopy.Class=="U" & deriv.all.var$ci$var=="vpd.max" & deriv.all.var$ci$vpd.max>ns.fagr[2],]*100, 2, mean, na.rm=T), na.rm=T)

#######################

# ----------

# --------------------------------
