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

# --------------------------------
# setting up a loop to automatically go through each site
# --------------------------------
source("0_Calculate_GAMM_Posteriors_Updated.R")
n=100

mod.stats <- data.frame(Site.Code=unique(data.use$Site.Code), r.sq=NA, dev.expl=NA, AIC=NA)
pred.out <- data.frame() # Where the posteriors can end up

unique(data.use$Site.Code)
for(SITE in unique(data.use$Site.Code)){
  print(SITE)
  # -------
  # train the model
  # -------
  gam.site <- gam(log(BA.inc)~ 
                    s(tmean, k=3, by=spp.cc) +
                    s(precip, k=3, by=spp.cc) +
                    s(vpd.max, k=3, by=spp.cc) +
                    s(dbh.recon, k=3, by=Species) +
                    s(Year, k=4, by=PlotID)+
                    PlotID  + TreeID + Canopy.Class + Species + spp.cc,
                  # random=list(Site=~1, PlotID=~1, TreeID=~1),
                  data=data.use[data.use$Site.Code==SITE,])
  mod.stats[mod.stats$Site.Code==SITE,"r.sq"] <- summary(gam.site)$r.sq # R-squared
  mod.stats[mod.stats$Site.Code==SITE,"dev.expl"] <- summary(gam.site)$dev.expl # explained deviance
  mod.stats[mod.stats$Site.Code==SITE,"AIC"] <- AIC(gam.site)
  # anova(gam.site) 
  save(gam.site, file=file.path(dir.out, paste0("gam_site_", SITE, ".Rdata")))
  # -------
  
  
  # -------
  # Setting up the base prediction data frame
  # -------
  yrs = min(data.use$Year[data.use$Site.Code==SITE]):max(data.use$Year[data.use$Site.Code==SITE])
  # Create a data frame with the factors that we care about (the "by" terms)
  fac.df <- data.frame(PlotID = rep(unique(data.use$PlotID[data.use$Site.Code==SITE]), 
                                    each=length(unique(data.use$Species[data.use$Site.Code==SITE]))*length(unique(data.use$Canopy.Class[data.use$Site.Code==SITE]))),
                       Species= rep(unique(data.use$Species[data.use$Site.Code==SITE])),
                       Canopy.Class = rep(unique(data.use$Canopy.Class[data.use$Site.Code==SITE]), each=length(unique(data.use$Species[data.use$Site.Code==SITE]))))
  fac.df$spp.cc <- paste(fac.df$Species, fac.df$Canopy.Class, sep=".")
  
  # Create a data frame with the numeric predictors we care about (the spline terms)
  dat.site <- data.frame(Year=yrs,
                                dbh.recon=seq(min(data.use$dbh.recon[data.use$Site.Code==SITE]), 
                                              max(data.use$dbh.recon[data.use$Site.Code==SITE]), 
                                              length.out=length(yrs)),
                                tmean=seq(min(data.use$tmean[data.use$Site.Code==SITE]), 
                                          max(data.use$tmean[data.use$Site.Code==SITE]), 
                                          length.out=length(yrs)),
                                precip=seq(min(data.use$precip[data.use$Site.Code==SITE]), 
                                           max(data.use$precip[data.use$Site.Code==SITE]), 
                                           length.out=length(yrs)),
                                vpd.max=seq(min(data.use$vpd.max[data.use$Site.Code==SITE]), 
                                            max(data.use$vpd.max[data.use$Site.Code==SITE]), 
                                            length.out=length(yrs))
  )
  dat.site <- merge(dat.site, fac.df, all=T)
  
  # Add in dummy levels for factors we don't care about for this model
  dat.site$Species <- factor(dat.site$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
  dat.site$Site.Code <- SITE
  dat.site$TreeID <- data.use$TreeID[data.use$Site.Code==SITE][1]
  summary(dat.site)
  dim(dat.site)
  
  # Get rid of site-species combos not in the original data
  dat.site <- dat.site[dat.site$spp.cc %in% unique(data.use[data.use$Site.Code==SITE,"spp.cc"]),]
  summary(dat.site)
  dim(dat.site)
  # -------

  # -------
  # Doing the effects posterior predictions and saving it
  # -------
  pred.site <- post.distns(model.gam=gam.site, model.name=SITE, n=n, newdata=dat.site, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)
  site.out <- pred.site$ci
  site.out[,c("Site.Code", "Species", "Canopy.Class", "spp.cc")] <- dat.site[,c("Site.Code", "Species", "Canopy.Class", "spp.cc")]
  site.out$x <- as.numeric(site.out$x) # making x numeric; will make factors NA
  summary(site.out)
  
  site.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(site.out[,c("mean", "lwr", "upr")])
  summary(site.out)
  
  pred.out <- rbind(pred.out, site.out)
  # -------
  
}
mod.stats
pred.out$Site.Code <- factor(pred.out$Site.Code, levels=c("GB", "RH", "HO", "GE", "LF", "HF", "NR", "PS"))
summary(pred.out)
write.csv(pred.out, file.path(dir.out, "Post-Process_AllSites.csv"), row.names=F)
# --------------------------------


# --------------------------------
# Plotting posterior comparisons
# --------------------------------
ggplot(data=pred.out[pred.out$Effect=="tmean",]) +
  facet_grid(Site.Code ~ Species) +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Canopy.Class), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Canopy.Class)) +
  coord_cartesian(ylim=c(0.5, 2.0))


for(SPP in unique(pred.out$Species)){
  png(file.path(dir.out, paste0("ClimateResponse_", SPP, ".png")), height=10, width=8, unit="in", res=120)
  print(
  ggplot(data=pred.out[pred.out$Effect %in% c("tmean", "precip", "vpd.max") & pred.out$Species==SPP,]) +
    ggtitle(paste0("Climate Effects: ", SPP)) + 
    facet_grid(Site.Code ~ Effect, scales="free_x") +
    geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Canopy.Class), alpha=0.5) +
    geom_line(aes(x=x, y=mean.bai, color=Canopy.Class)) +
    coord_cartesian(ylim=c(0.5, 2.0))
  )
  dev.off()
}

png(file.path(dir.out, "SizeResponse_SiteModels.png"), height=8, width=8, unit="in", res=120)
ggplot(data=pred.out[pred.out$Effect=="dbh.recon" ,]) +
  facet_wrap(~Species) +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Site.Code), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Site.Code)) +
  coord_cartesian(ylim=c(0,75))
dev.off()

png(file.path(dir.out, "YearResponse_SiteModels.png"), height=8, width=8, unit="in", res=120)
ggplot(data=pred.out[pred.out$Effect=="Year" ,]) +
  facet_wrap(~PlotID, scales="free_y") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Site.Code), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Site.Code)) 
dev.off()
# --------------------------------
