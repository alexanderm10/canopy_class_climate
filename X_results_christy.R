# Results script for the NEUS canopy_class paper
# Loading in data frames

# Loading in all of the base stuff we need
library(mgcv)
source("0_Calculate_GAMM_Posteriors_Updated.R") # This is a slightly updated version from what you were using earlier
source("0_Calculate_GAMM_Derivs.R")

# Setting up the base dataframe we need
new.dat <- read.csv("processed_data/sensitivity_extaction_dataframe.csv")
#n <- 1000

predictors.all <- c("vpd.max", "tmean", "precip", "Species", "dbh.recon", "Canopy.Class", "spp.plot", "Site.Code", "Year", "PlotID", "spp.cc", "TreeID") 




# ----------------------------------------
# Species level gam
# ----------------------------------------
load("processed_data/gam4_response_graph.Rdata")
load("processed_data/gam_results/gam4_species_only.Rdata") 
# processed_data/gam_results/gam0_null_model_tempprecip.Rdata
spp.gam <- gam4; rm(gam4)
# spp.graph <- ci.terms.graph; rm(ci.terms.graph)


df.spp <- new.dat
vars.fac <- c("Site.Code", "PlotID", "TreeID", "Canopy.Class", "Species", "spp.cc")
var.smooth <- "Species"
for(v in vars.fac){
  if(v %in% var.smooth) next # keep all levels for our "by" variable
  # Get rid of unimportant levels for everything else
  l1 <- unique(df.spp[,v])[1]
  df.spp <- df.spp[df.spp[,v]==l1,]
}
summary(df.spp)


pred.spp <- c("tmean", "precip")
# spp.post <- post.distns(model.gam=spp.gam, newdata=df.spp, vars=pred.spp, terms=T)
# spp.post$x <- as.numeric(spp.post$x) # making x numeric; will make factors NA; NA's are ok here
# summary(spp.post)

spp.deriv <- calc.derivs(model.gam=spp.gam, n=n, newdata=df.spp, vars=pred.spp)
summary(spp.deriv)

# ----------------
# ACRU
# ----------------
# ------
# Temperature
# ------
summary(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean", ])

# Slope mean & SD (whole range)
mean(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean", "mean"])
sd(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean", "mean"])

# Non-significant temperature range
summary(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean" & is.na(spp.deriv$sig), "tmean"])

# Strong negative slope area
mean(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean" & spp.deriv$tmean>15.5, "mean"])
sd(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean" & spp.deriv$tmean>15.5, "mean"])

# Slgiht positive area
mean(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean" & spp.deriv$tmean<14.9, "mean"])
sd(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean" & spp.deriv$tmean<14.9, "mean"])
# ------

# ------
# Precipitation
# ------
summary(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip", ])

# Slope mean & SD (whole range)
mean(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip", "mean"])
sd(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip", "mean"])

# Non-significant precipitation range
summary(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip" & is.na(spp.deriv$sig), "precip"])


# positive slope area
mean(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip" & spp.deriv$precip<405, "mean"])
sd(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip" & spp.deriv$precip<405, "mean"])

# negative slope area
mean(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip" & spp.deriv$precip>609.1, "mean"])
sd(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip" & spp.deriv$precip>609.1, "mean"])
# ------

# ----------------

# ----------------
# FAGR
# ----------------
# ------
# Temperature
# ------
summary(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="tmean", ])

# Slope mean & SD (whole range)
mean(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="tmean", "mean"])
sd(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="tmean", "mean"])

# Non-significant temperature range
summary(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="tmean" & is.na(spp.deriv$sig), "tmean"])
# ------

# ------
# Precipitation
# ------
summary(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="precip", ])

# Slope mean & SD (whole range)
mean(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="precip", "mean"])
sd(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="precip", "mean"])

# Non-significant precipitation range
summary(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="precip" & is.na(spp.deriv$sig), "precip"])


# positive slope area
mean(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="precip" & spp.deriv$precip<405, "mean"])
sd(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="precip" & spp.deriv$precip<405, "mean"])

# negative slope area
mean(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="precip" & spp.deriv$precip>609.1, "mean"])
sd(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="precip" & spp.deriv$precip>609.1, "mean"])
# ------
# ----------------

# ----------------
# QURU
# ----------------
# ------
# Temperature
# ------
summary(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="tmean", ])

# Slope mean & SD (whole range)
mean(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="tmean", "mean"])
sd(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="tmean", "mean"])

# Non-significant temperature range
summary(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="tmean" & is.na(spp.deriv$sig), "tmean"])

# Area of significant change
mean(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="tmean" & spp.deriv$tmean>15.84, "mean"])
sd(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="tmean" & spp.deriv$tmean>15.84, "mean"])

# ------

# ------
# Precipitation
# ------
summary(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip", ])

# Slope mean & SD (whole range)
mean(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip", "mean"])
sd(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip", "mean"])

# Non-significant precipitation range
summary(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip" & is.na(spp.deriv$sig), "precip"])


# positive slope area
mean(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip" & spp.deriv$precip<498, "mean"])
sd(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip" & spp.deriv$precip<498, "mean"])

# negative slope area
mean(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip" & spp.deriv$precip>550, "mean"])
sd(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip" & spp.deriv$precip>550, "mean"])
# ------
# ----------------

# ----------------
# TSCA
# ----------------
# ------
# Temperature
# ------
summary(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="tmean", ])

# Slope mean & SD (whole range)
mean(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="tmean", "mean"])
sd(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="tmean", "mean"])

# Non-significant temperature range
summary(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="tmean" & is.na(spp.deriv$sig), "tmean"])

# Significant effect range
mean(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="tmean" & spp.deriv$tmean<17.1, "mean"])
sd(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="tmean" & spp.deriv$tmean<17.1, "mean"])
# ------

# ------
# Precipitation
# ------
summary(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip", ])

# Slope mean & SD (whole range)
mean(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip", "mean"])
sd(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip", "mean"])

# Non-significant precipitation range
summary(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip" & is.na(spp.deriv$sig), "precip"])


# significant slope area
mean(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip" & spp.deriv$precip<685.7, "mean"])
sd(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip" & spp.deriv$precip<685.7, "mean"])
# ------
# ----------------

# ----------------------------------------


# ----------------------------------------
# Canopy class Temp/Precip gam
# ----------------------------------------
library(nlme)
# load("processed_data/gam6_response_graph.Rdata")
load("processed_data/gam_results/gam6_canopy_species_add.Rdata") 
cc.gam <- gam6; rm(gam6)
# cc.graph <- ci.terms.graph; rm(ci.terms.graph)

df.cc <- new.dat
vars.fac <- c("Site.Code", "PlotID", "TreeID", "Canopy.Class", "Species", "spp.cc")
var.smooth <- c("Canopy.Class", "Species")
for(v in vars.fac){
  if(v %in% var.smooth) next # keep all levels for our "by" variable
  # Get rid of unimportant levels for everything else
  l1 <- unique(df.cc[,v])[1]
  df.cc <- df.cc[df.cc[,v]==l1,]
}
df.cc$spp.cc <- as.factor(paste(df.cc$Species, df.cc$Canopy.Class, sep="."))
summary(df.cc)

pred.cc <- c("tmean", "precip")
# cc.post <- post.distns(model.gam=cc.gam, newdata=df.cc, vars=pred.cc, terms=T)
# cc.post$x <- as.numeric(cc.post$x) # making x numeric; will make factors NA; NA's are ok here
# summary(cc.post)

cc.deriv <- calc.derivs(model.gam=cc.gam, n=n, newdata=df.cc, vars=pred.cc)
summary(cc.deriv)

# ----------------
# Comparing canopy class effects given species slopes
# ----------------
cc.temp <- lme(mean ~ tmean*Canopy.Class -1 -tmean, random = ~1 + Canopy.Class|Species, data=cc.deriv[cc.deriv$var=="tmean", ])
summary(cc.temp)
# anova(cc.temp)

cc.precip <- lme(mean ~ precip*Canopy.Class -1 -precip, random = ~1 + Canopy.Class|Species, data=cc.deriv[cc.deriv$var=="precip", ])
summary(cc.precip)
# anova(cc.precip)
# ----------------

# ----------------
# Looking at canopy effects within species
# ----------------
# ------------
# ACRU
# ------------
# -----
# Temperature
# -----
range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU","tmean"])

# Looking at the range at which ACRU canopy plateaus
acru.temp.ns <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","tmean"]

# <14.53 (where canopy turns negative)
acru.temp.lo <- lm(mean ~ tmean*Canopy.Class - Canopy.Class - tmean -1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$tmean<min(acru.temp.ns), ])
summary(acru.temp.lo)

# >15.72˚C (where canopy turns negative)
acru.temp.hi <- lm(mean ~ tmean*Canopy.Class - Canopy.Class - tmean -1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU"  & cc.deriv$tmean>max(acru.temp.ns), ])
summary(acru.temp.hi)
# -----

# -----
# Precipitation
# -----
range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU","precip"])

# Looking at the range at which ACRU canopy plateaus
acru.precip.ns <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","precip"]

# whole range
acru.precip <- lm(mean ~ precip*Canopy.Class - Canopy.Class - precip -1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU", ])
summary(acru.precip)

# low precip (where there is significant precip signal)
acru.precip.lo <- lm(mean ~ precip*Canopy.Class - Canopy.Class - precip -1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$precip<min(acru.precip.ns), ])
summary(acru.precip.lo)
# -----
# ------------

# ------------
# FAGR
# ------------
# -----
# Temperature
# -----
range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR","tmean"])

# Looking at the range at which FAGR canopy plateaus
fagr.temp.ns <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","tmean"]

# whole range (dominant is non-temp significant)
fagr.temp <- lm(mean ~ tmean*Canopy.Class - Canopy.Class - tmean -1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR", ])
summary(fagr.temp)
# -----

# -----
# Precipitation
# -----
range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR","precip"])

# Looking at the range at which FAGR canopy plateaus
fagr.precip.ns <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","precip"]
summary(fagr.precip.ns)

# whole range
fagr.precip <- lm(mean ~ precip*Canopy.Class - Canopy.Class - precip -1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR", ])
summary(fagr.precip)
# -----
# ------------

# ------------
# QURU
# ------------
# -----
# Temperature
# -----
range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU","tmean"])

# Looking at the range at which QURU canopy plateaus
quru.temp.ns <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","tmean"]
summary(quru.temp.ns)

# whole range (dominant is non-temp significant)
quru.temp <- lm(mean ~ tmean*Canopy.Class - Canopy.Class - tmean -1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU", ])
summary(quru.temp)

# high temp range (where canopy is signif)
quru.temp.hi <- lm(mean ~ tmean*Canopy.Class - Canopy.Class - tmean -1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$tmean>max(quru.temp.ns), ])
summary(quru.temp.hi)
# -----

# -----
# Precipitation
# -----
range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU","precip"])

# Looking at the range at which QURU canopy plateaus
quru.precip.ns <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","precip"]
summary(quru.precip.ns)

# whole range
quru.precip <- lm(mean ~ precip*Canopy.Class - Canopy.Class - precip -1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU", ])
summary(quru.precip)

# High precip (where QURU shows decline)
quru.precip.hi <- lm(mean ~ precip*Canopy.Class - Canopy.Class - precip -1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$precip>max(quru.precip.ns), ])
summary(quru.precip.hi)
# -----
# ------------


# ------------
# TSCA
# ------------
# -----
# Temperature
# -----
range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA","tmean"])

# Looking at the range at which TSCA canopy plateaus
tsca.temp.ns <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","tmean"]
summary(tsca.temp.ns)

# whole range (dominant is non-temp significant)
tsca.temp <- lm(mean ~ tmean*Canopy.Class - Canopy.Class - tmean -1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA", ])
summary(tsca.temp)

tsca.temp.abs <- lm(abs(mean) ~ tmean*Canopy.Class - Canopy.Class - tmean -1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA", ])
summary(tsca.temp.abs)
# -----

# -----
# Precipitation
# -----
range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA","precip"])

# Looking at the range at which TSCA canopy plateaus
tsca.precip.ns <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","precip"]
summary(tsca.precip.ns)

# whole range
tsca.precip <- lm(mean ~ precip*Canopy.Class - Canopy.Class - precip -1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA", ])
summary(tsca.precip)

# Lo precip where Canopy is sensitive
tsca.precip.lo <- lm(mean ~ precip*Canopy.Class - Canopy.Class - precip -1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$precip<min(tsca.precip.ns), ])
summary(tsca.precip.lo)

# Hi precip where Canopy is NOT sensitive
tsca.precip.hi <- lm(mean ~ precip*Canopy.Class - Canopy.Class - precip -1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$precip>min(tsca.precip.ns), ])
summary(tsca.precip.hi)

# -----
# ------------

# ----------------

# ----------------------------------------


# ----------------------------------------
# Canopy Class VPD gam
# ----------------------------------------
load("processed_data/gam_vpdmax_response_graph.Rdata")
load("processed_data/gam_results/gam_vpdmax.Rdata")		
vpd.gam <- gam.vpdmax; rm(gam.vpdmax)
vpd.graph <- ci.terms.graph; rm(ci.terms.graph)

# ----------------------------------------
