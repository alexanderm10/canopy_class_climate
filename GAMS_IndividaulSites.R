# --------------------------------
# Header & Description
# --------------------------------
# Doing site-specific models just to appease picky reviewer 3 who doesn't like macroecology
# Steps:
# 1. Format Data
# 2. Create our prediction data frame to make our graphs and do analyses on the fly
# 3. Run & Save models for individual sites
#    - predict as we go as well
# 4. Graph & look at the dang data
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
dir.out <- "processed_data/gam_results_SiteSpecific"
dir.create(dir.out, recursive=T, showWarnings=F)
# --------------------------------

# --------------------------------
# Setting up the base prediction data frame
# --------------------------------

# --------------------------------

# --------------------------------
# setting up a loop to automatically go through each site
# --------------------------------
summary(data.use$Site.Code)

mod.stats <- data.frame(Site.Code=unique(data.use$Site.Code), r.sq=NA, dev.expl=NA, AIC=NA)

for(SITE in unique(data.use$Site.Code)){
  gam.site <- gam(log(BA.inc)~ 
                    s(tmean, k=3, by=spp.cc) +
                    s(precip, k=3, by=spp.cc) +
                    s(vpd.max, k=3, by=spp.cc) +
                    s(dbh.recon, k=3, by=Species) +
                    s(Year, k=4, by=PlotID)+
                    PlotID  + TreeID + Canopy.Class + Species,
                  # random=list(Site=~1, PlotID=~1, TreeID=~1),
                  data=data.use[data.use$Site.Code==SITE,])
  mod.stats[mod.stats$Site.Code==SITE,"r.sq"] <- summary(gam.site)$r.sq # R-squared
  mod.stats[mod.stats$Site.Code==SITE,"dev.expl"] <- summary(gam.site)$dev.expl # explained deviance
  mod.stats[mod.stats$Site.Code==SITE,"AIC"] <- AIC(gam.site)
  anova(gam.site) 
  save(gam.site, file=file.path(dir.out, paste0("gam_site_", SITE, ".Rdata")))
}
mod.stats

# --------------------------------
