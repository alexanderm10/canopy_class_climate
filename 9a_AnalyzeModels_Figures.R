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
if(!dir.exists(dir.out)) dir.create(dir.out, recursive=T, showWarnings = F)

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
names(model.comp)[names(model.comp)=="r.sq"] <- "R2"
model.comp$Model <- car::recode(model.comp$Model, "'climate.spp'='spp'; 'climate.cc'='cc'")
model.comp

# Reshapign the table a bit
# test <- reshape2::recast(r.sq ~ Species, data=model.comp)
suff <- unique(model.comp$Model)
table2 <- reshape(model.comp[,c("Model", "Species", "R2", "AIC", "RMSE")], idvar = "Species", timevar="Model", direction = "wide")
table2$dR2 <- table2$R2.cc - table2$R2.spp
table2$dAIC <- table2$AIC.cc - table2$AIC.spp
table2$dRMSE <- table2$RMSE.cc - table2$RMSE.spp
table2$n[table2$Species=="TSCA"] <- 496
table2$n[table2$Species=="FAGR"] <- 148
table2$n[table2$Species=="ACRU"] <- 165
table2$n[table2$Species=="QURU"] <- 275

table2 <- table2[,c("Species", "n", paste("R2", suff, sep="."), "dR2", paste("AIC", suff, sep="."), "dAIC", paste("RMSE", suff, sep="."), "dRMSE")]

table2$Species <- factor(table2$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
table2 <- table2[order(table2$Species),]


write.csv(table2, file.path(path.google, "Table2_ModelComparison.csv"), row.names=F)
# mean(model.comp$dev.exp); sd(model.comp$dev.exp)
# round(range(model.comp$dev.exp), 3)
# --------------------------------



# --------------------------------
# 4. Calculating the posterior estimates for the effects for graphing
# --------------------------------

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
  gam.clim.spp$gam$formula
  
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


for(SPP in unique(clim.cc.out$Species)){
  dbh.min <- min(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  dbh.max <- max(data.use[data.use$Species==paste(SPP),"dbh.recon"])
  
  rows.na <- which(clim.spp.out$Effect=="dbh.recon" & clim.spp.out$Species==paste(SPP) & (clim.spp.out$x<dbh.min | clim.spp.out$x>dbh.max))
  clim.spp.out[rows.na,c("mean.bai", "lwr.bai", "upr.bai")] <- NA
  deriv.spp.out$sims[rows.na,] <- NA
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
  
  rows.na <- which(clim.spp.out$Effect=="tmean" & clim.spp.out$Species==paste(CC) & (clim.spp.out$x<tmean.min | clim.spp.out$x>tmean.max))
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
summary(deriv.spp.out$ci)

range(deriv.spp.out$ci$tmean, na.rm=T)
range(deriv.spp.out$ci$precip, na.rm=T)
range(deriv.spp.out$ci$vpd.max, na.rm=T)
# -------------------
# Fagus grandifolia:
# With the exception of Fagus grandifolia precipitaiton response, species displayed highly non-linear sensitivities to climate.  Fagus grandifolia showed a linear sensitivity of XXX ± XXX %BAI/mm for precipitiaton.  However, Fagus grandifolia was insensitive to VPD at the species-level and only sensitive to temperatures below X˚C (XX ± XX %/BAI/˚C). 
# -------------------
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="vpd.max",])
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="precip",])
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="tmean",])
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig),])

# Average precip stats
round(mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="precip" & !is.na(deriv.spp.out$ci$sig) ,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)


# Minimum temperature for sensitivity
round(min(deriv.spp.out$ci[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig),"tmean"]), 1)

round(mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="FAGR" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig) ,1:100], 2, mean, na.rm=T)*100, na.rm=T),2 )
# -------------------

# -------------------
# Tsuga canadensis:
# In contrast, Tsuga canadensis only showed growth sensitivity to changes in temperatures below XX ˚C where the mean sensitivity was XX ± XX %BAI/˚C.  This paralleled the observed Tsuga canadensis response to VPD, where it only showed reduced growth at VPD below XX kPA (XX ± XX %BAI/kPa).  Although Tsuga canadensis showed overall positive relationships to precipitation, it was only significantly sensitive to annual summer precipitation below XX mm (XX ± XX %BAI/mm).  
# -------------------
# summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="TSCA",])
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="vpd.max",])
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="precip",])
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="tmean",])

# Temperature
round(max(deriv.spp.out$ci[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig),"tmean"]), 1)
round(mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig) ,1:100], 2, mean, na.rm=T)*100, na.rm=T),2 )

#VPD
round(max(deriv.spp.out$ci[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="vpd.max" & !is.na(deriv.spp.out$ci$sig),"vpd.max"]), 1)
round(mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="vpd.max" & !is.na(deriv.spp.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="vpd.max" & !is.na(deriv.spp.out$ci$sig) ,1:100], 2, mean, na.rm=T)*100, na.rm=T),2 )

# Precip
round(max(deriv.spp.out$ci[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="precip" & !is.na(deriv.spp.out$ci$sig),"precip"]), 0)
round(mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="precip" & !is.na(deriv.spp.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="TSCA" & deriv.spp.out$ci$var=="precip" & !is.na(deriv.spp.out$ci$sig) ,1:100], 2, mean, na.rm=T)*100, na.rm=T),2 )
# -------------------


# -------------------
# Quercus rubra
# Quercus rubra showed responses to temperature and preciptiation nearly opposite that of Tsuga canadensis with increasing growth rates at temperatures below X ˚C (XX ± XX %BAI/˚C) and reduced growth with summer precipitation above XX mm (XX ± XX %BAI/mm).  Without consideration of canopy position, Quercus rubra showed the greatest sensitivity to VPD with strong sensitivity below XX kPA (XX ± XX %BAI/VPD).  
# -------------------
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="vpd.max",])
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="precip",])
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="tmean",])

# Temperature
round(max(deriv.spp.out$ci[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig),"tmean"]), 1)
round(mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig) ,1:100], 2, mean, na.rm=T)*100, na.rm=T),2 )

# Precip
round(min(deriv.spp.out$ci[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="precip" & !is.na(deriv.spp.out$ci$sig),"precip"]), 0)
round(mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="precip" & !is.na(deriv.spp.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="precip" & !is.na(deriv.spp.out$ci$sig) ,1:100], 2, mean, na.rm=T)*100, na.rm=T),2 )

#VPD
round(max(deriv.spp.out$ci[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="vpd.max" & !is.na(deriv.spp.out$ci$sig),"vpd.max"]), 1)
round(mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="vpd.max" & !is.na(deriv.spp.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="QURU" & deriv.spp.out$ci$var=="vpd.max" & !is.na(deriv.spp.out$ci$sig) ,1:100], 2, mean, na.rm=T)*100, na.rm=T),2 )
# -------------------


# -------------------
# Acer rubrum
# Acer rubrum showed the lowest sensitivity to temperature and preciptiation, with temperature sensitivity only significant [UNDER X Conditions] (XX ± XX %BAI/˚C) and preciptiation sensitivity above XX mm (XX ± XX %BAI/mm).  Similar to Quercus rubra and Tsuga canadensis, Acer rubrum only showed sensitivity to low VPD (XX ± XX %BAI/kPa below XX kPA)
# -------------------
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="vpd.max",])
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="precip",])
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="tmean",])

# Temperature
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig),])

round(max(deriv.spp.out$ci[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig),"tmean"]), 1)
round(max(deriv.spp.out$ci[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig) & deriv.spp.out$ci$mean>0,"tmean"]), 1)
round(mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig)& deriv.spp.out$ci$mean>0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig) & deriv.spp.out$ci$mean>0,1:100], 2, mean, na.rm=T)*100, na.rm=T),2 )

round(min(deriv.spp.out$ci[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig) & deriv.spp.out$ci$mean<0,"tmean"]), 1)
round(mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig)& deriv.spp.out$ci$mean<0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="tmean" & !is.na(deriv.spp.out$ci$sig) & deriv.spp.out$ci$mean<0,1:100], 2, mean, na.rm=T)*100, na.rm=T),2 )

# Precip
summary(deriv.spp.out$ci[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="precip" & !is.na(deriv.spp.out$ci$sig),])

round(min(deriv.spp.out$ci[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="precip" & !is.na(deriv.spp.out$ci$sig),"precip"]), 0)
round(mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="precip" & !is.na(deriv.spp.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="precip" & !is.na(deriv.spp.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T),2 )

#VPD
round(max(deriv.spp.out$ci[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="vpd.max" & !is.na(deriv.spp.out$ci$sig),"vpd.max"]), 1)
round(mean(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="vpd.max" & !is.na(deriv.spp.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.spp.out$sims[deriv.spp.out$ci$Species=="ACRU" & deriv.spp.out$ci$var=="vpd.max" & !is.na(deriv.spp.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T),2 )
# -------------------
# ---------------------------------------------


# ---------------------------------------------
# Canopy Class model
# ---------------------------------------------
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
  # gam.clim.cc$gam$formula
  # summary(gam.clim.cc$gam)
  
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


#######################
# Temperature
#######################
# -----------
# Tsuga canadensis
# Growth of overstory Tsuga canadensis decreased XX ± XX %BAI/˚C at temperatures below X ˚C, but increased XX ± XX %BAI/˚C above X˚C.  In contrast, understory Tsuga canadensis showed consistent negative growth sensitivity of XXX ± XX %BAI/˚C at temperatures above X˚C.  
# -----------
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="tmean",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean",])

#Canopy
round(max(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,"tmean"]), 1)

round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0 ,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0 ,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)

round(min(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,"tmean"]), 1)
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0 ,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0 ,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)

# Understory
round(min(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),"tmean"]), 1)

round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)

# -----------

# -----------
# Fagus grandifolia
# Fagus grandifolia displayed the greatest similarity in growth sensitivity between the understory and overstory with mean sensitivity of XXX ± XXX %BAI/˚C and XXX±XXX %BAI/˚C above X˚C and X˚C, respectively.  Middle-canopy trees of Fagus grandifolia were insensitive to temperature.  
# -----------
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="tmean",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean",])

round(min(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),"tmean"]), 1)
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)

round(min(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),"tmean"]), 1)
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)
# -----------


# -----------
# ACRU
# Acer rubrum was not sensitive to temperature in the overstory (Appendix S1 Table S3), negative temperature sensitivity with warm temeratures was seen in both the middle-canopy  (XX ± XX %BAI/˚C above X˚C) and understory (XX ± XX %BAI/˚C above X˚C).
# -----------
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="tmean",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean",])

summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="tmean"& !is.na(deriv.cc.out$ci$sig),])
round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,"tmean"]), 1)
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)

round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,"tmean"]), 1)
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)


# round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,"tmean"]), 1)
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean"& !is.na(deriv.cc.out$ci$sig),])
round(min(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),"tmean"]), 1)
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)
# -----------

# -----------
# QURU
# Despite overall positive temperature sensitivity of overstory Quercus rubra (XX ± XX %BAI/˚C below X˚C), growth of understory trees was insensitive to temperatures in our study.
# -----------
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="tmean",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean",])

summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean"& !is.na(deriv.cc.out$ci$sig),])
round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),"tmean"]), 1)
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)


summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean"& !is.na(deriv.cc.out$ci$sig),])
round(min(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),"tmean"]), 1)
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="tmean" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)

# -----------
#######################

#######################
# Precipitation
#######################
# -----------
# Tsuga canadensis
# When climate sensitivity is analyzed by canopy position, overstory Tsuga canadensis and Fagus grandifolia display nearly linear, positive sensitivity to precipitation (XX ± XX %BAI/mm and XX ± XX %BAI/mm, respectively). 
# In the case of Tsuga canadensis, growth declined in the understory with increasing preciptiation at levels above XX mm (XX ± XX %BAI/mm).  
# -----------
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="precip",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip",])

round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)

summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig),])
round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,"precip"]))
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)

round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,"precip"]))
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)

# -----------

# -----------
# Fagus grandifolia
# When climate sensitivity is analyzed by canopy position, overstory Tsuga canadensis and Fagus grandifolia display nearly linear, positive sensitivity to precipitation (XX ± XX %BAI/mm and XX ± XX %BAI/mm, respectively). 
# -----------
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="precip",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip",])

round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip",1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)


# -----------

# -----------
# Acer rubrum
# -----------
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="precip",]) # Intermediate NOT snsitive
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip",])

# Comparing paired sensitivities
d.acru <- deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip",] - deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip",]
quantile(d.acru, c(0.025,0.975), na.rm=T)


round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,"precip"]))

round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,"precip"]))
round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,"precip"]))
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)

round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)

round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,"precip"]))
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)
# -----------

# -----------
# Quercus rubra
# For Quercus rubra trees in all canopy positions showed similar trends as observed in the species-only model with a cross-group sensisitivity of XXX ± XXX %BAI/mm above X mm.
# -----------
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="precip",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip",])

d.quru <- deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip",] - deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip",]
quantile(d.quru, c(0.025,0.975), na.rm=T)


round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig),"precip"]))
round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig),"precip"]))

round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,"precip"]))
round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,"precip"]))

round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,"precip"]))
round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean<0,"precip"]))
round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig),"precip"]))

round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$var=="precip" & deriv.cc.out$ci$precip>500,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" & deriv.cc.out$ci$precip>500,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)


round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,"precip"]))
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="precip" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$mean>0,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)

# -----------

#######################

#######################
# VPD
#######################
# -----------
# Tsuga canadensis
# -----------
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="vpd.max",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max",])

round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig),"vpd.max"]), 1)
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$vpd.max<81,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="TSCA" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$vpd.max<81,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2)

# -----------

# -----------
# Fagus grandifolia
# -----------
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="vpd.max",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max",])

round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig),"vpd.max"]), 1)
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="FAGR" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2) 

# -----------

# -----------
# Acer rubrum
# -----------
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="vpd.max",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max",])

round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig),"vpd.max"]), 1)

round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$vpd.max<81,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="ACRU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$vpd.max<81,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2) 
# -----------

# -----------
# Quercus rubra
# -----------
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="I" & deriv.cc.out$ci$var=="vpd.max",])
summary(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max",])

round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig),"vpd.max"]), 1)
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$vpd.max<81,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="Canopy" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig) & deriv.cc.out$ci$vpd.max<81,1:100], 2, mean, na.rm=T)*100, na.rm=T), 2) 


round(range(deriv.cc.out$ci[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig),"vpd.max"]), 1)
round(mean(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2); round(sd(apply(deriv.cc.out$sims[deriv.cc.out$ci$Species=="QURU" & deriv.cc.out$ci$Canopy.Class=="U" & deriv.cc.out$ci$var=="vpd.max" & !is.na(deriv.cc.out$ci$sig),1:100], 2, mean, na.rm=T)*100, na.rm=T), 2) 

# -----------


#######################

# ---------------------------------------------

# --------------------------------


load(file.path(dir.out, "gam_clim_cc_TSCA.Rdata"))
cc.tsca <- gam.clim.cc

load(file.path(dir.out, "gam_clim_cc_FAGR.Rdata"))
cc.fagr <- gam.clim.cc

load(file.path(dir.out, "gam_clim_cc_ACRU.Rdata"))
cc.acru <- gam.clim.cc

load(file.path(dir.out, "gam_clim_cc_QURU.Rdata"))
cc.quru <- gam.clim.cc


summary(cc.tsca$gam)$s.table[1:9,]

clim.sig <- data.frame(TSCA=round(summary(cc.tsca$gam)$s.table[1:9,"p-value"], 3),
                       FAGR=round(summary(cc.fagr$gam)$s.table[1:9,"p-value"], 3),
                       ACRU=round(summary(cc.acru$gam)$s.table[1:9,"p-value"], 3),
                       QURU=round(summary(cc.quru$gam)$s.table[1:9,"p-value"], 3))

clim.sig