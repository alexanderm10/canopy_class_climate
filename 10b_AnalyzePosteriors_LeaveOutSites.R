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
path.google <- "/Volumes/GoogleDrive/My Drive/Manuscripts/Alexander_CanopyClimateResponse/canopy_and_climate/manuscript/Ecology (submit 2019-10)/Revision 3 - 2020-10/"
dir.figs <- file.path(path.google, "figures")
dir.create(dir.figs, recursive = T, showWarnings = F)

dir.out <- "processed_data/gam_results_SiteSensitivity/"
dir.create(dir.out, recursive=T, showWarnings = F)

source("0_GraphEffects.R")

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
  print(paste("working on species:", SPP) )
  spp.raw <- data.use[data.use$Species==SPP,]
  
  for(SITE in unique(spp.raw$Site.Code)){
    print(paste("     - site:", SITE) )
    dat.site <- spp.raw[spp.raw$Site.Code!=SITE,]
    
    fac.df <- data.frame(Species= SPP,
                         SiteOut = SITE,
                         PlotID = unique(dat.site$PlotID)
                         )
    dat.spp <- merge(dat.clim.spp, fac.df, all=T)
    dat.spp$Site.Code <- substr(dat.spp$PlotID, 1, 2)
    dat.spp$Site.Code <- car::recode(dat.spp$Site.Code, "'TP'='HF'")
    dat.spp$TreeID <- data.use$TreeID[data.use$Species==SPP][1]
    dat.spp$Canopy.Class <- data.use$Canopy.Class[data.use$Species==SPP][1]
    summary(dat.spp)
    
    
    # Load gam.clim.spp
    load(file.path(dir.out, paste0("gam_clim_spp_", SPP, "_", SITE, ".Rdata")))
    
    # Create a data frame with just what we need for the clim.spp model
    gam.clim.spp$gam$formula
    
    # Do the posterior predictions
    pred.clim.spp <- post.distns(model.gam=gam.clim.spp, n=n, newdata=dat.spp, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)
    pred.clim.spp$Species <- as.factor(SPP)
    pred.clim.spp$Site.Code <- as.factor(SITE)
    # clim.spp.out$x <- as.numeric(clim.spp.out$x) # making x numeric; will make factors NA
    summary(pred.clim.spp)
    
    clim.spp.out <- rbind(clim.spp.out, pred.clim.spp)
  } # end site loop
} # End species loop

clim.spp.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.spp.out[,c("mean", "lwr", "upr")])
summary(clim.spp.out)

# Trimming out size fits
for(SPP in unique(clim.spp.out$Species)){
  spp.raw <- data.use[data.use$Species==SPP,]
  
  for(SITE in unique(spp.raw$Site.Code)){
    dat.site <- spp.raw[spp.raw$Site.Code!=SITE,]
    
    yr.now <- range(dat.site$Year)
    dbh.now <- range(dat.site$dbh.recon)
    tmp.now <- range(dat.site$tmean)
    pcp.now <- range(dat.site$precip)
    vpd.now <- range(dat.site$vpd.max)
    
    dbh.max <- max(data.use[data.use$Species==paste(SPP),"dbh.recon"])
    
    dbh.na <- which(clim.spp.out$Effect=="dbh.recon" & clim.spp.out$Site.Code==paste(SITE) & clim.spp.out$Species==paste(SPP) & (clim.spp.out$x<dbh.now[1] | clim.spp.out$x>dbh.now[2]))
    tmp.na <- which(clim.spp.out$Effect=="tmean" & clim.spp.out$Site.Code==paste(SITE) & clim.spp.out$Species==paste(SPP) & (clim.spp.out$x<tmp.now[1] | clim.spp.out$x>tmp.now[2]))
    prcp.na <- which(clim.spp.out$Effect=="precip" & clim.spp.out$Site.Code==paste(SITE) & clim.spp.out$Species==paste(SPP) & (clim.spp.out$x<pcp.now[1] | clim.spp.out$x>pcp.now[2]))
    vpd.na <- which(clim.spp.out$Effect=="vpd.max" & clim.spp.out$Site.Code==paste(SITE) & clim.spp.out$Species==paste(SPP) & (clim.spp.out$x<vpd.now[1] | clim.spp.out$x>vpd.now[2]))
    
    clim.spp.out[c(dbh.na, tmp.na, prcp.na, vpd.na),c("mean.bai", "lwr.bai", "upr.bai")] <- NA

  } # End site loop

} # End species

clim.spp.out$Site <- factor(clim.spp.out$Site.Code, levels=c("HO", "GB", "RH", "GE", "PS", "NR", "HF", "LF"))
clim.spp.out$Species <- factor(clim.spp.out$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))

png(file.path(dir.figs, "SupplementalFigure08_SiteOut-Species_ClimateEffect.png"), height=6, width=6, unit="in", res=600)
plot.climate.sites(dat.plot=clim.spp.out, canopy=F, panel="sites")
dev.off()


tiff(file.path(dir.figs, "SupplementalFigure08_SiteOut-Species_ClimateEffect.tiff"), height=6, width=6, unit="in", res=600)
plot.climate.sites(dat.plot=clim.spp.out, canopy=F, panel="sites")
dev.off()

pdf(file.path(dir.figs, "SupplementalFigure08_SiteOut-Species_ClimateEffect.pdf"), height=6, width=6)
plot.climate.sites(dat.plot=clim.spp.out, canopy=F, panel="sites")
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
  print(paste("working on species:", SPP) )
  spp.raw <- data.use[data.use$Species==SPP,]
  
  for(SITE in unique(spp.raw$Site.Code)){
    print(paste("     - site:", SITE) )
    dat.site <- spp.raw[spp.raw$Site.Code!=SITE,]
    
    fac.df <- data.frame(Species= SPP,
                         SiteOut = SITE,
                         Canopy.Class=unique(dat.site$Canopy.Class),
                         PlotID = rep(unique(dat.site$PlotID), 
                                      each=length(unique(dat.site$Canopy.Class))))    
    dat.spp <- merge(dat.clim.cc, fac.df, all=T)
    dat.spp$Site.Code <- substr(dat.spp$PlotID, 1, 2)
    dat.spp$Site.Code <- car::recode(dat.spp$Site.Code, "'TP'='HF'")
    dat.spp$TreeID <- data.use$TreeID[data.use$Species==SPP][1]
    # dat.spp$Canopy.Class <- data.use$Canopy.Class[data.use$Species==SPP][1]
    summary(dat.spp)
    
    
    # Load gam.clim.cc
    load(file.path(dir.out, paste0("gam_clim_cc_", SPP, "_", SITE, ".Rdata")))
    
    # Create a data frame with just what we need for the clim.cc model
    gam.clim.cc$gam$formula
    
    pred.clim.cc <- post.distns(model.gam=gam.clim.cc, n=n, newdata=dat.spp, vars=c("dbh.recon", "Year", "tmean", "precip", "vpd.max"), terms=T)
    pred.clim.cc[,c("Species", "SiteOut", "Canopy.Class")] <- dat.spp[,c("Species", "SiteOut", "Canopy.Class")]
    # pred.clim.cc$x <- as.numeric(pred.clim.cc$x) # making x numeric; will make factors NA
    # summary(pred.clim.cc)
    
    clim.cc.out <- rbind(clim.cc.out, pred.clim.cc)
    # summary(clim.cc.out)
  }
}  
clim.cc.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(clim.cc.out[,c("mean", "lwr", "upr")])


for(SPP in unique(clim.cc.out$Species)){
  spp.raw <- data.use[data.use$Species==SPP,]
  
  for(SITE in unique(spp.raw$Site.Code)){
    dat.site <- spp.raw[spp.raw$Site.Code!=SITE,]
    
    for(CC in unique(dat.site$Canopy.Class)){
      dat.cc <- dat.site[dat.site$Canopy.Class==CC,]
      yr.now <- range(dat.cc$Year)
      dbh.now <- range(dat.cc$dbh.recon)
      tmp.now <- range(dat.cc$tmean)
      pcp.now <- range(dat.cc$precip)
      vpd.now <- range(dat.cc$vpd.max)
      

      dbh.na <- which(clim.cc.out$Effect=="dbh.recon" & clim.cc.out$SiteOut==paste(SITE) & clim.cc.out$Species==paste(SPP) & clim.cc.out$Canopy.Class==CC & (clim.cc.out$x<dbh.now[1] | clim.cc.out$x>dbh.now[2]))
      tmp.na <- which(clim.cc.out$Effect=="tmean" & clim.cc.out$SiteOut==paste(SITE) & clim.cc.out$Species==paste(SPP) & clim.cc.out$Canopy.Class==CC & (clim.cc.out$x<tmp.now[1] | clim.cc.out$x>tmp.now[2]))
      prcp.na <- which(clim.cc.out$Effect=="precip" & clim.cc.out$SiteOut==paste(SITE) & clim.cc.out$Species==paste(SPP) & clim.cc.out$Canopy.Class==CC & (clim.cc.out$x<pcp.now[1] | clim.cc.out$x>pcp.now[2]))
      vpd.na <- which(clim.cc.out$Effect=="vpd.max" & clim.cc.out$SiteOut==paste(SITE) & clim.cc.out$Species==paste(SPP) & clim.cc.out$Canopy.Class==CC & (clim.cc.out$x<vpd.now[1] | clim.cc.out$x>vpd.now[2]))
      
      clim.cc.out[c(dbh.na, tmp.na, prcp.na, vpd.na),c("mean.bai", "lwr.bai", "upr.bai")] <- NA
    } # End Canopy Class Loop

  } # End site loop
  
} # End species

summary(clim.cc.out)

clim.cc.out$Species <- factor(clim.cc.out$Species, levels=c("TSCA", "FAGR", "ACRU", "QURU"))
clim.cc.out$Site <- factor(clim.cc.out$SiteOut, levels=c("HO", "GB", "RH", "GE", "PS", "NR", "HF", "LF"))
clim.cc.out$Canopy.Class <- car::recode(clim.cc.out$Canopy.Class, "'Canopy'='Overstory'; 'I'='Middle'; 'U'='Understory'")
clim.cc.out$Canopy.Class <- factor(clim.cc.out$Canopy.Class, levels=c("Overstory", "Middle", "Understory"))
summary(clim.cc.out)


# Broken by Canopy 
fig.num = 9
for(SPP in unique(clim.cc.out$Species)){
  
  png(file.path(dir.figs, paste0("SupplementalFigure", stringr::str_pad(fig.num, "0", width=2, side="left"),"_SiteOut-CanopyClass_", SPP, "_ClimateEffect_groupSites.png")), height=6, width=6, unit="in", res=600)
  print(plot.climate.sites(dat.plot=clim.cc.out, canopy=T, panel="sites", species = SPP))
  dev.off()
  
  tiff(file.path(dir.figs, paste0("SupplementalFigure", stringr::str_pad(fig.num, "0", width=2, side="left"),"_SiteOut-CanopyClass_", SPP, "_ClimateEffect_groupSites.tiff")), height=6, width=6, unit="in", res=600)
  print(plot.climate.sites(dat.plot=clim.cc.out, canopy=T, panel="sites", species = SPP))
  dev.off()
  
  pdf(file.path(dir.figs, paste0("SupplementalFigure", stringr::str_pad(fig.num, "0", width=2, side="left"),"_SiteOut-CanopyClass_", SPP, "_ClimateEffect_groupSites.pdf")), height=6, width=6)
  print(plot.climate.sites(dat.plot=clim.cc.out, canopy=T, panel="sites", species = SPP))
  dev.off()
  
  fig.num <- fig.num+1 # Go to the next number 
  
  png(file.path(dir.figs, paste0("SupplementalFigure", stringr::str_pad(fig.num, "0", width=2, side="left"),"_SiteOut-CanopyClass_", SPP, "_ClimateEffect_groupCanopy.png")), height=6, width=6, unit="in", res=600)
  print(plot.climate.sites(dat.plot=clim.cc.out, canopy=T, panel="canopy", species = SPP))
  dev.off()
  
  tiff(file.path(dir.figs, paste0("SupplementalFigure", stringr::str_pad(fig.num, "0", width=2, side="left"),"_SiteOut-CanopyClass_", SPP, "_ClimateEffect_groupCanopy.tiff")), height=6, width=6, unit="in", res=600)
  print(plot.climate.sites(dat.plot=clim.cc.out, canopy=T, panel="canopy", species = SPP))
  dev.off()
  
  pdf(file.path(dir.figs, paste0("SupplementalFigure", stringr::str_pad(fig.num, "0", width=2, side="left"),"_SiteOut-CanopyClass_", SPP, "_ClimateEffect_groupCanopy.pdf")), height=6, width=6)
  print(plot.climate.sites(dat.plot=clim.cc.out, canopy=T, panel="canopy", species = SPP))
  dev.off()
  
  fig.num <- fig.num+1 # Go to the next number
}



# ----------
