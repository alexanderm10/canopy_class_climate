# --------------------------------
# Header & Description
# --------------------------------
# Full model variance decomposition & model selection to appease picky reviewer 3
# Steps:
# 1. Format Data (lines 21-60)
# 2. Run & Save inidividual models; save stats to table: (lines 60-193)
#    - null = size + year effects (+ intercepts) only (lines 75-89)
#    - base clim = null + temp + precip + vpd (lines 91-108)
#    - spp.clim = null + (base clim)*species (lines 110-128)
#    - cc.clim = null + (base clim)*canopy class (lines 130-147)
#    - spp.cc.clim = null + (base clim)*canopy classxspecies (lines 150-167) 
#    - full = null + (base clim)*species + (base clim)*canopy (lines 169-190)
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
# 2. Run & Save inidivudal models:
#    - null = size + year effects (+ intercepts) only
#    - base clim = null + temp + precip + vpd
#    - spp.clim = null + (base clim)*species
#    - cc.clim = null + (base clim)*canopy class
#    - spp.cc.clim = null + (base clim)*speciesxcanopy class
#    - full = null + (base clim)*species + (base clim)*canopy
# --------------------------------
mod.comp <- data.frame(Model=rep(c("climate.spp", "climate.cc"), each=length(spp.use)),
                       Species=rep(spp.use),
                       r.sq=NA, dev.expl=NA, AIC=NA, RMSE=NA)

for(SPP in spp.use){
  print(paste("working on speices:", SPP) )
  gam.clim.spp <- gam(log(BA.inc)~
                         s(tmean, k=3) +
                         s(precip, k=3) +
                         s(vpd.max, k=3) +
                         s(dbh.recon, k=3, by=Species) +
                         s(Year, k=4, by=PlotID)+
                         Canopy.Class,
                       random=list(Site=~1, PlotID=~1, TreeID=~1),
                       data=data.use[data.use$Species==SPP,])
  
  mod.comp[mod.comp$Species==SPP & mod.comp$Model=="climate.spp", "r.sq"] <- summary(gam.clim.spp)$r.sq # R-squared
  mod.comp[mod.comp$Species==SPP & mod.comp$Model=="climate.spp", "dev.expl"] <- summary(gam.clim.spp)$dev.expl # explained deviance
  mod.comp[mod.comp$Species==SPP & mod.comp$Model=="climate.spp", "AIC"] <- AIC(gam.clim.spp)
  
  pred.comp <- predict(gam.clim.spp, data.use[data.use$Species==SPP,])
  mod.comp[mod.comp$Species==SPP & mod.comp$Model=="climate.spp", "RMSE"] <- sqrt(mean((log(data.use$BA.inc[data.use$Species==SPP])-pred.comp)^2))
  
  # anova(gam.clim.base) 
  
  save(gam.clim.spp, file=file.path(dir.out, paste0("gam_clim_spp_", SPP, ".Rdata")))
  # ----------
  

  # ----------
  # Canopy-based climate model
  # ----------
  gam.clim.cc <- gam(log(BA.inc)~ 
                        s(tmean, k=3, by=Canopy.Class) +
                        s(precip, k=3, by=Canopy.Class) +
                        s(vpd.max, k=3, by=Canopy.Class) +
                        s(dbh.recon, k=3, by=Species) +
                        s(Year, k=4, by=PlotID)+
                        Canopy.Class,
                      # random=list(Site=~1, PlotID=~1, TreeID=~1),
                      data=data.use[data.use$Species==SPP,])
  mod.comp[mod.comp$Species==SPP & mod.comp$Model=="climate.cc", "r.sq"] <- summary(gam.clim.cc)$r.sq # R-squared
  mod.comp[mod.comp$Species==SPP & mod.comp$Model=="climate.cc", "dev.expl"] <- summary(gam.clim.cc)$dev.expl # explained deviance
  mod.comp[mod.comp$Species==SPP & mod.comp$Model=="climate.cc", "AIC"] <- AIC(gam.clim.cc)
  # anova(gam.clim.cc) 
  
  pred.cc <- predict(gam.clim.cc, data.use[data.use$Species==SPP,])
  mod.comp[mod.comp$Species==SPP & mod.comp$Model=="climate.cc", "RMSE"] <- sqrt(mean((log(data.use$BA.inc[data.use$Species==SPP])-pred.cc)^2))
  
  save(gam.clim.cc, file=file.path(dir.out, paste0("gam_clim_cc_", SPP, ".Rdata")))
  # ----------
}
# ----------

# Save Table 2
mod.comp
mod.comp$r.sq <- round(mod.comp$r.sq, 3)
mod.comp$dev.expl <- round(mod.comp$dev.expl, 3)
mod.comp$AIC <- round(mod.comp$AIC, 0)
mod.comp$RMSE <- round(mod.comp$RMSE, 3)

write.csv(mod.comp, file.path(dir.out, "ModComparison_Full.csv"), row.names=F)
# --------------------------------

