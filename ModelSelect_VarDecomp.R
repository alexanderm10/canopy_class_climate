# --------------------------------
# Header & Description
# --------------------------------
# Full model variance decomposition & model selection to appease picky reviewer 3
# Steps:
# 1. Format Data
# 2. Run & Save inidivudal models:
#    - null = size + year effects (+ intercepts) only
#    - base clim = null + temp + precip + vpd
#    - spp.clim = null + (base clim)*species
#    - cc.clim = null + (base clim)*canopy class
#    - full = null + (base clim)*species + (base clim)*canopy
# 3. Build AIC/Exp Dev Table
# --------------------------------

library(mgcv)
library(ggplot2)
library(car)


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

# Set up a directory where to save things & not overwrite past efforts
dir.out <- "processed_data/gam_results_VarDecomp"
dir.create(dir.out, recursive=T, showWarnings = F)

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
mod.comp <- data.frame(Model=c("null", "climate.base", "climate.spp", "climate.cc", "climate.spp.cc", "full"),
                       r.sq=NA, dev.expl=NA, AIC=NA)

# ----------
# Null Model: No climatic effects
# ----------
gam.null <- gam(log(BA.inc)~ s(dbh.recon, k=3, by=Species) +
                  s(Year, k=4, by=PlotID)+
                  Site.Code + PlotID  + TreeID + Canopy.Class + Species,
                   # random=list(Site=~1, PlotID=~1, TreeID=~1),
                  data=data.use)
mod.comp[mod.comp$Model=="null", "r.sq"] <- summary(gam.null)$r.sq # R-squared
mod.comp[mod.comp$Model=="null", "dev.expl"] <- summary(gam.null)$dev.expl # explained deviance
mod.comp[mod.comp$Model=="null", "AIC"] <- AIC(gam.null)
# anova(gam.null) 

save(gam.null, file=file.path(dir.out, "gam_null.Rdata"))
# ----------

# ----------
# Base Climate Model: No species/canopy-based climate effects
# ----------
gam.clim.base <- gam(log(BA.inc)~ 
                       s(tmean, k=3) +
                       s(precip, k=3) +
                       s(vpd.max, k=3) +
                       s(dbh.recon, k=3, by=Species) +
                       s(Year, k=4, by=PlotID)+
                       Site.Code + PlotID  + TreeID + Canopy.Class + Species,
                     # random=list(Site=~1, PlotID=~1, TreeID=~1),
                     data=data.use)
mod.comp[mod.comp$Model=="climate.base", "r.sq"] <- summary(gam.clim.base)$r.sq # R-squared
mod.comp[mod.comp$Model=="climate.base", "dev.expl"] <- summary(gam.clim.base)$dev.expl # explained deviance
mod.comp[mod.comp$Model=="climate.base", "AIC"] <- AIC(gam.clim.base)
# anova(gam.clim.base) 
save(gam.clim.base, file=file.path(dir.out, "gam_clim_base.Rdata"))
# ----------

# ----------
# Species-based Climate Model
# ----------
gam.clim.spp <- gam(log(BA.inc)~ 
                       s(tmean, k=3, by=Species) +
                       s(precip, k=3, by=Species) +
                       s(vpd.max, k=3, by=Species) +
                       s(dbh.recon, k=3, by=Species) +
                       s(Year, k=4, by=PlotID)+
                       Site.Code + PlotID  + TreeID + Canopy.Class + Species,
                     # random=list(Site=~1, PlotID=~1, TreeID=~1),
                     data=data.use)
mod.comp[mod.comp$Model=="climate.spp", "r.sq"] <- summary(gam.clim.spp)$r.sq # R-squared
mod.comp[mod.comp$Model=="climate.spp", "dev.expl"] <- summary(gam.clim.spp)$dev.expl # explained deviance
mod.comp[mod.comp$Model=="climate.spp", "AIC"] <- AIC(gam.clim.spp)
# anova(gam.clim.spp) 

save(gam.clim.spp, file=file.path(dir.out, "gam_clim_spp.Rdata"))
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
                      Site.Code + PlotID  + TreeID + Canopy.Class + Species,
                    # random=list(Site=~1, PlotID=~1, TreeID=~1),
                    data=data.use)
mod.comp[mod.comp$Model=="climate.cc", "r.sq"] <- summary(gam.clim.cc)$r.sq # R-squared
mod.comp[mod.comp$Model=="climate.cc", "r.sq"] <- summary(gam.clim.cc)$dev.expl # explained deviance
mod.comp[mod.comp$Model=="climate.cc", "r.sq"] <- AIC(gam.clim.cc)
# anova(gam.clim.cc) 
save(gam.clim.cc, file=file.path(dir.out, "gam_clim_cc.Rdata"))
# ----------

# ----------
# SpeciesxCanopy model
# ----------
gam.clim.spp.cc <- gam(log(BA.inc)~ 
                     s(tmean, k=3, by=spp.cc) +
                     s(precip, k=3, by=spp.cc) +
                     s(vpd.max, k=3, by=spp.cc) +
                     s(dbh.recon, k=3, by=Species) +
                     s(Year, k=4, by=PlotID)+
                     Site.Code + PlotID  + TreeID + Canopy.Class + Species,
                   # random=list(Site=~1, PlotID=~1, TreeID=~1),
                   data=data.use)
mod.comp[mod.comp$Model=="climate.spp.cc", "r.sq"] <- summary(gam.clim.spp.cc)$r.sq # R-squared
mod.comp[mod.comp$Model=="climate.spp.cc", "dev.expl"] <- summary(gam.clim.spp.cc)$dev.expl # explained deviance
mod.comp[mod.comp$Model=="climate.spp.cc", "AIC"] <- AIC(gam.clim.spp.cc)
# anova(gam.clim.spp.cc) 
save(gam.clim.spp.cc, file=file.path(dir.out, "gam_clim_spp.cc.Rdata"))
# ----------

# ----------
# Species + Canopy model
# ----------
gam.all.var <- gam(log(BA.inc)~ 
                     s(tmean, k=3, by=Canopy.Class) +
                     s(precip, k=3, by=Canopy.Class) +
                     s(vpd.max, k=3, by=Canopy.Class) +
                     s(tmean, k=3, by=Species) +
                     s(precip, k=3, by=Species) +
                     s(vpd.max, k=3, by=Species) +
                     s(dbh.recon, k=3, by=Species) +
                     s(Year, k=4, by=PlotID)+
                     Site.Code + PlotID  + TreeID + Canopy.Class + Species,
                   # random=list(Site=~1, PlotID=~1, TreeID=~1),
                   data=data.use)
mod.comp[mod.comp$Model=="full", "r.sq"] <- summary(gam.all.var)$r.sq # R-squared
mod.comp[mod.comp$Model=="full", "dev.expl"] <- summary(gam.all.var)$dev.expl # explained deviance
mod.comp[mod.comp$Model=="full", "AIC"] <- AIC(gam.all.var)
# anova(gam.all.var) 

save(gam.all.var, file=file.path(dir.out, "gam_all_variables.Rdata"))
# ----------
# --------------------------------
