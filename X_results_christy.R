# Results script for the NEUS canopy_class paper
# Loading in data frames

# Loading in all of the base stuff we need
library(mgcv)
source("0_Calculate_GAMM_Posteriors_Updated.R") # This is a slightly updated version from what you were using earlier
source("0_Calculate_GAMM_Derivs.R")

# Setting up the base dataframe we need
new.dat <- read.csv("processed_data/sensitivity_extaction_dataframe.csv")
n <- 1000

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

spp.deriv <- calc.derivs(model.gam=spp.gam, newdata=df.spp, vars=pred.spp)
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

cc.deriv <- calc.derivs(model.gam=cc.gam, newdata=df.cc, vars=pred.cc)
summary(cc.deriv)

# ----------------
# Comparing canopy class effects given species slopes
# ----------------
# Means parameterization
cc.temp1 <- lme(mean ~ Canopy.Class -1, random = ~tmean + Canopy.Class|Species, data=cc.deriv[cc.deriv$var=="tmean", ])
summary(cc.temp1)

cc.precip1 <- lme(mean ~ Canopy.Class-1, random = ~precip + Canopy.Class|Species, data=cc.deriv[cc.deriv$var=="precip", ])
summary(cc.precip1)

# Effects Parameterization (compare to canopy trees)
cc.temp2 <- lme(mean ~ Canopy.Class , random = ~tmean + Canopy.Class|Species, data=cc.deriv[cc.deriv$var=="tmean", ])
summary(cc.temp2)
# anova(cc.temp)

cc.precip2 <- lme(mean ~ Canopy.Class, random = ~precip + Canopy.Class|Species, data=cc.deriv[cc.deriv$var=="precip", ])
summary(cc.precip2)
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

# whole range
acru.temp.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="Canopy", "mean"]
acru.temp.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="I", "mean"]
acru.temp.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="U", "mean"]
mean(acru.temp.c); sd(acru.temp.c)
mean(acru.temp.i); sd(acru.temp.i)
mean(acru.temp.u); sd(acru.temp.u)

acru.temp <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU", ])
summary(acru.temp)

acru.temp2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU", ])
summary(acru.temp2)


# <14.53 (where canopy turns negative)
acru.temp.lo.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$tmean<min(acru.temp.ns), "mean"]
acru.temp.lo.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="I" & cc.deriv$tmean<min(acru.temp.ns), "mean"]
acru.temp.lo.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="U" & cc.deriv$tmean<min(acru.temp.ns), "mean"]
mean(acru.temp.lo.c); sd(acru.temp.lo.c)
mean(acru.temp.lo.i); sd(acru.temp.lo.i)
mean(acru.temp.lo.u); sd(acru.temp.lo.u)

acru.temp.lo <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$tmean<min(acru.temp.ns), ])
summary(acru.temp.lo)

# >15.72˚C (where canopy turns negative)
acru.temp.hi.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$tmean>max(acru.temp.ns), "mean"]
acru.temp.hi.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="I" & cc.deriv$tmean>max(acru.temp.ns), "mean"]
acru.temp.hi.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="U" & cc.deriv$tmean>max(acru.temp.ns), "mean"]
mean(acru.temp.hi.c); sd(acru.temp.hi.c)
mean(acru.temp.hi.i); sd(acru.temp.hi.i)
mean(acru.temp.hi.u); sd(acru.temp.hi.u)

acru.temp.hi <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU"  & cc.deriv$tmean>max(acru.temp.ns), ])
summary(acru.temp.hi)
# -----

# -----
# Precipitation
# -----
range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU","precip"])

# Looking at the range at which ACRU canopy plateaus
acru.precip.ns <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","precip"]

# whole range
acru.precip.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="Canopy", "mean"]
acru.precip.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="I", "mean"]
acru.precip.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="U", "mean"]
mean(acru.precip.c); sd(acru.precip.c)
mean(acru.precip.i); sd(acru.precip.i)
mean(acru.precip.u); sd(acru.precip.u)

acru.precip <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU", ])
summary(acru.precip)

acru.precip2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU", ])
summary(acru.precip2)

# low precip (where there is significant precip signal)
acru.precip.lo.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$precip<min(acru.precip.ns), "mean"]
acru.precip.lo.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="I" & cc.deriv$precip<min(acru.precip.ns), "mean"]
acru.precip.lo.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="U" & cc.deriv$precip<min(acru.precip.ns), "mean"]
mean(acru.precip.lo.c); sd(acru.precip.lo.c)
mean(acru.precip.lo.i); sd(acru.precip.lo.i)
mean(acru.precip.lo.u); sd(acru.precip.lo.u)

acru.precip.lo <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$precip<min(acru.precip.ns), ])
summary(acru.precip.lo)

acru.precip.lo2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$precip<min(acru.precip.ns), ])
summary(acru.precip.lo2)

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
fagr.temp.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="Canopy", "mean"]
fagr.temp.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="I", "mean"]
fagr.temp.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="U", "mean"]
mean(fagr.temp.c); sd(fagr.temp.c)
mean(fagr.temp.i); sd(fagr.temp.i)
mean(fagr.temp.u); sd(fagr.temp.u)

fagr.temp <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR", ])
summary(fagr.temp)

fagr.temp2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR", ])
summary(fagr.temp2)
# -----

# -----
# Precipitation
# -----
range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR","precip"])

# Looking at the range at which FAGR canopy plateaus
fagr.precip.ns <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","precip"]
summary(fagr.precip.ns)

# whole range
fagr.precip.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="Canopy", "mean"]
fagr.precip.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="I", "mean"]
fagr.precip.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="U", "mean"]
mean(fagr.precip.c); sd(fagr.precip.c)
mean(fagr.precip.i); sd(fagr.precip.i)
mean(fagr.precip.u); sd(fagr.precip.u)

fagr.precip <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR", ])
summary(fagr.precip)

fagr.precip2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR", ])
summary(fagr.precip2)
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

# whole range (dominant is non-temp significant for most of the range)
quru.temp.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="Canopy", "mean"]
quru.temp.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="I", "mean"]
quru.temp.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="U", "mean"]
mean(quru.temp.c); sd(quru.temp.c)
mean(quru.temp.i); sd(quru.temp.i)
mean(quru.temp.u); sd(quru.temp.u)

quru.temp <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU", ])
summary(quru.temp)

quru.temp2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU", ])
summary(quru.temp2)

# high temp range (where canopy is signif)
quru.temp.hi.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$tmean>max(quru.temp.ns), "mean"]
quru.temp.hi.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="I" & cc.deriv$tmean>max(quru.temp.ns), "mean"]
quru.temp.hi.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="U" & cc.deriv$tmean>max(quru.temp.ns), "mean"]
mean(quru.temp.hi.c); sd(quru.temp.hi.c)
mean(quru.temp.hi.i); sd(quru.temp.hi.i)
mean(quru.temp.hi.u); sd(quru.temp.hi.u)

quru.temp.hi <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$tmean>max(quru.temp.ns), ])
summary(quru.temp.hi)

quru.temp.hi2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$tmean>max(quru.temp.ns), ])
summary(quru.temp.hi2)
# -----

# -----
# Precipitation
# -----
range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU","precip"])

# Looking at the range at which QURU canopy plateaus
quru.precip.ns <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","precip"]
summary(quru.precip.ns)

# whole range
quru.precip.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="Canopy", "mean"]
quru.precip.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="I", "mean"]
quru.precip.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="U", "mean"]
mean(quru.precip.c); sd(quru.precip.c)
mean(quru.precip.i); sd(quru.precip.i)
mean(quru.precip.u); sd(quru.precip.u)

quru.precip <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU", ])
summary(quru.precip)

quru.precip2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU", ])
summary(quru.precip2)

# High precip (where QURU shows decline)
quru.precip.hi <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$precip>max(quru.precip.ns), ])
summary(quru.precip.hi)

quru.precip.hi2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$precip>max(quru.precip.ns), ])
summary(quru.precip.hi2)
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
tsca.temp.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="Canopy", "mean"]
tsca.temp.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="I", "mean"]
tsca.temp.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="U", "mean"]
mean(tsca.temp.c); sd(tsca.temp.c)
mean(tsca.temp.i); sd(tsca.temp.i)
mean(tsca.temp.u); sd(tsca.temp.u)

tsca.temp <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA", ])
summary(tsca.temp)

tsca.temp2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA", ])
summary(tsca.temp2)


mean(abs(tsca.temp.c)); sd(abs(tsca.temp.c))
mean(abs(tsca.temp.i)); sd(abs(tsca.temp.i))
mean(abs(tsca.temp.u)); sd(abs(tsca.temp.u))

tsca.temp.abs <- lm(abs(mean) ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA", ])
summary(tsca.temp.abs)

tsca.temp.abs2 <- lm(abs(mean) ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA", ])
summary(tsca.temp.abs2)
# -----

# -----
# Precipitation
# -----
range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA","precip"])

# Looking at the range at which TSCA canopy plateaus
tsca.precip.ns <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","precip"]
summary(tsca.precip.ns)

# whole range
tsca.precip.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="Canopy", "mean"]
tsca.precip.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="I", "mean"]
tsca.precip.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="U", "mean"]
mean(tsca.precip.c); sd(tsca.precip.c)
mean(tsca.precip.i); sd(tsca.precip.i)
mean(tsca.precip.u); sd(tsca.precip.u)


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
# load("processed_data/gam_vpdmax_response_graph.Rdata")
load("processed_data/gam_results/gam_vpdmax.Rdata")		
vpd.gam <- gam.vpdmax; rm(gam.vpdmax)
# vpd.graph <- ci.terms.graph; rm(ci.terms.graph)

df.vpd <- new.dat
vars.fac <- c("Site.Code", "PlotID", "TreeID", "Canopy.Class", "Species", "spp.cc")
var.smooth <- c("Canopy.Class", "Species")
for(v in vars.fac){
  if(v %in% var.smooth) next # keep all levels for our "by" variable
  # Get rid of unimportant levels for everything else
  l1 <- unique(df.vpd[,v])[1]
  df.vpd <- df.vpd[df.vpd[,v]==l1,]
}
df.vpd$spp.cc <- as.factor(paste(df.vpd$Species, df.vpd$Canopy.Class, sep="."))
summary(df.vpd)

pred.vpd <- c("vpd.max")
# vpd.post <- post.distns(model.gam=vpd.gam, newdata=df.vpd, vars=pred.cc, terms=T)
# vpd.post$x <- as.numeric(vpd.post$x) # making x numeric; will make factors NA; NA's are ok here
# summary(vpd.post)

vpd.deriv <- calc.derivs(model.gam=vpd.gam, newdata=df.vpd, vars=pred.vpd)
summary(vpd.deriv)

# ----------------
# Comparing canopy class effects given species slopes
# ----------------
vpd.vpd <- lme(mean ~ Canopy.Class, random = ~vpd.max + Canopy.Class|Species, data=vpd.deriv[vpd.deriv$var=="vpd.max", ])
summary(vpd.vpd)
anova(vpd.vpd)

vpd.vpd2 <- lme(mean ~ Canopy.Class-1, random = ~vpd.max + Canopy.Class|Species, data=vpd.deriv[vpd.deriv$var=="vpd.max", ])
summary(vpd.vpd2)
anova(vpd.vpd2)
# ----------------

# ----------------
# Looking at canopy effects within species
# ----------------
# ------------
# ACRU
# ------------
summary(vpd.deriv[vpd.deriv$Species=="ACRU",])
range(vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="ACRU","vpd.max"])

# Looking at mean sensitivity
acru.vpd.c <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="ACRU" & vpd.deriv$Canopy.Class=="Canopy", "mean"]
acru.vpd.i <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="ACRU" & vpd.deriv$Canopy.Class=="I", "mean"]
acru.vpd.u <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="ACRU" & vpd.deriv$Canopy.Class=="U", "mean"]
mean(acru.vpd.c); sd(acru.vpd.c)
mean(acru.vpd.i); sd(acru.vpd.i)
mean(acru.vpd.u); sd(acru.vpd.u)


# Looking at the range at which ACRU canopy plateaus
acru.vpd.ns <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="ACRU" & is.na(vpd.deriv$sig) & vpd.deriv$Canopy.Class=="Canopy","vpd.max"]
summary(acru.vpd.ns)

# Whole range b/c canopy is significantly negative the whole way
acru.vpd <- lm(mean ~ Canopy.Class, data=vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="ACRU", ])
summary(acru.vpd)

acru.vpd2 <- lm(mean ~ Canopy.Class-1, data=vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="ACRU", ])
summary(acru.vpd2)
# ------------

# ------------
# FAGR
# ------------
summary(vpd.deriv[vpd.deriv$Species=="FAGR",])
range(vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="FAGR","vpd.max"])


# Looking at the range at which FAGR canopy plateausfagr.vpd.ns <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="FAGR" & is.na(vpd.deriv$sig) & vpd.deriv$Canopy.Class=="Canopy","vpd.max"]
summary(fagr.vpd.ns)

# Looking at mean sensitivity
fagr.vpd.c <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="FAGR" & vpd.deriv$Canopy.Class=="Canopy", "mean"]
fagr.vpd.i <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="FAGR" & vpd.deriv$Canopy.Class=="I", "mean"]
fagr.vpd.u <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="FAGR" & vpd.deriv$Canopy.Class=="U", "mean"]
mean(fagr.vpd.c); sd(fagr.vpd.c)
mean(fagr.vpd.i); sd(fagr.vpd.i)
mean(fagr.vpd.u); sd(fagr.vpd.u)

# Whole range
fagr.vpd <- lm(mean ~ Canopy.Class, data=vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="FAGR", ])
summary(fagr.vpd)

fagr.vpd2 <- lm(mean ~ Canopy.Class-1, data=vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="FAGR", ])
summary(fagr.vpd2)
# ------------

# ------------
# QURU
# ------------
summary(vpd.deriv[vpd.deriv$Species=="QURU",])
range(vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="QURU","vpd.max"])


# Looking at the range at which QURU canopy plateaus
quru.vpd.ns <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="QURU" & is.na(vpd.deriv$sig) & vpd.deriv$Canopy.Class=="Canopy","vpd.max"]
summary(quru.vpd.ns)

# Whole range
quru.vpd.c <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="QURU" & vpd.deriv$Canopy.Class=="Canopy", "mean"]
quru.vpd.i <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="QURU" & vpd.deriv$Canopy.Class=="I", "mean"]
quru.vpd.u <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="QURU" & vpd.deriv$Canopy.Class=="U", "mean"]
mean(quru.vpd.c); sd(quru.vpd.c)
mean(quru.vpd.i); sd(quru.vpd.i)
mean(quru.vpd.u); sd(quru.vpd.u)

quru.vpd <- lm(mean ~ Canopy.Class, data=vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="QURU", ])
summary(quru.vpd)

quru.vpd2 <- lm(mean ~ Canopy.Class-1, data=vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="QURU", ])
summary(quru.vpd2)

quru.vpd.abs <- lm(abs(mean) ~ Canopy.Class, data=vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="QURU", ])
summary(quru.vpd.abs)
# ------------


# ------------
# TSCA
# ------------
summary(vpd.deriv[vpd.deriv$Species=="TSCA",])
range(vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="TSCA","vpd.max"])

# Looking at mean sensitivity
tsca.vpd.c <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="TSCA" & vpd.deriv$Canopy.Class=="Canopy", "mean"]
tsca.vpd.i <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="TSCA" & vpd.deriv$Canopy.Class=="I", "mean"]
tsca.vpd.u <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="TSCA" & vpd.deriv$Canopy.Class=="U", "mean"]
mean(tsca.vpd.c); sd(tsca.vpd.c)
mean(tsca.vpd.i); sd(tsca.vpd.i)
mean(tsca.vpd.u); sd(tsca.vpd.u)


# Looking at the range at which TSCA canopy plateaus
tsca.vpd.ns <- vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="TSCA" & is.na(vpd.deriv$sig) & vpd.deriv$Canopy.Class=="Canopy","vpd.max"]
summary(tsca.vpd.ns)

# Whole range b/c canopy is significantly negative the whole way
tsca.vpd <- lm(mean ~ Canopy.Class, data=vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="TSCA", ])
summary(tsca.vpd)

tsca.vpd2 <- lm(mean ~ Canopy.Class-1, data=vpd.deriv[vpd.deriv$var=="vpd.max" & vpd.deriv$Species=="TSCA", ])
summary(tsca.vpd2)
# ----------------


# ----------------------------------------
