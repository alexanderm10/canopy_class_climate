# Results script for the NEUS canopy_class paper
# Loading in data frames

# Loading in all of the base stuff we need
library(mgcv)
source("0_Calculate_GAMM_Posteriors_Updated.R") # This is a slightly updated version from what you were using earlier
source("0_Calculate_GAMM_Derivs.R")

# Setting up the base dataframe we need
data.use <- read.csv("processed_data/NESites_tree_plus_climate_and_BA.csv", header=T)

new.dat <- read.csv("processed_data/sensitivity_extaction_dataframe.csv")
n <- 1000

predictors.all <- c("vpd.max", "tmean", "precip", "Species", "dbh.recon", "Canopy.Class", "spp.plot", "Site.Code", "Year", "PlotID", "spp.cc", "TreeID") 

summary(dat.use)
summary(new.dat)


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
spp.post <- post.distns(model.gam=spp.gam, newdata=df.spp, vars=pred.spp, terms=T)
spp.post[,c("Species", "Canopy.Class")] <- spp.deriv[,c("Species", "Canopy.Class")]
summary(spp.deriv)
summary(spp.post)


# # Truncate out ranges we didn't observe in the data
for(VAR in var.smooth){
  for(LEV in unique(df.spp[,VAR])){
    for(PRED in pred.spp){
      prange <- range(data.use[data.use[,VAR]==paste(LEV),PRED], na.rm=T)
      
      # Insert NAs for predictor values outside the range of observed
      # NOTE: Assumes things are in teh same order (which they should be)
      rows.na <- which(spp.deriv[,VAR]==paste(LEV) & spp.deriv$var==PRED & 
                         !is.na(spp.deriv[spp.deriv$var==PRED,PRED]) & 
                         (spp.deriv[,PRED]<prange[1] | spp.deriv[,PRED]>prange[2]))
      spp.deriv[rows.na, PRED] <- NA
      spp.post[rows.na, "x"] <- NA
    }
  }
}
spp.deriv[is.na(spp.deriv$tmean),c("mean", "lwr", "upr", "sig")] <- NA
spp.deriv[is.na(spp.deriv$precip),c("mean", "lwr", "upr", "sig")] <- NA
spp.post[is.na(spp.post$x),c("mean", "lwr", "upr")] <- NA
summary(spp.deriv)
summary(spp.post)

# ----------------
# ACRU
# ----------------
# ------
# Temperature
# ------
summary(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean", ])

# Slope mean & SD (whole range)
mean(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean", "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean", "mean"], na.rm=T)

# Find range of ACRU response
acru.range  <- range(spp.post[spp.post$Species=="ACRU" & spp.post$Effect=="tmean", "mean"],na.rm=T)
spp.post[spp.post$Species=="ACRU" & spp.post$Effect=="tmean" & !is.na(spp.post$x) & spp.post$mean==acru.range[1],"x",]
spp.post[spp.post$Species=="ACRU" & spp.post$Effect=="tmean" & !is.na(spp.post$x) & spp.post$mean==acru.range[2],"x",]
sd(spp.post[spp.post$Species=="ACRU" & spp.post$Effect=="tmean", "mean"], na.rm=T)

# Non-significant temperature range
summary(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean" & is.na(spp.deriv$sig), "tmean"], na.rm=T)
acru.ns <- range(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean" & is.na(spp.deriv$sig), "tmean"], na.rm=T)
acru.ns

# Strong negative slope area
mean(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean" & spp.deriv$tmean>acru.ns[2], "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean" & spp.deriv$tmean>acru.ns[2], "mean"], na.rm=T)

# Slgiht positive area
mean(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean" & spp.deriv$tmean<acru.ns[1], "mean"])
sd(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="tmean" & spp.deriv$tmean<acru.ns[1], "mean"])
# ------

# ------
# Precipitation
# ------
summary(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip", ])

# Slope mean & SD (whole range)
mean(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip", "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip", "mean"], na.rm=T)

# Non-significant precipitation range
summary(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip" & is.na(spp.deriv$sig), "precip"])
range(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip", "precip"], na.rm=T)
acru.ns <- range(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip" & is.na(spp.deriv$sig), "precip"], na.rm=T)
acru.ns


# positive slope area
mean(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip" & spp.deriv$precip<acru.ns[1], "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip" & spp.deriv$precip<acru.ns[1], "mean"], na.rm=T)

# negative slope area
mean(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip" & spp.deriv$precip>acru.ns[2], "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="ACRU" & spp.deriv$var=="precip" & spp.deriv$precip>acru.ns[2], "mean"], na.rm=T)
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
mean(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="tmean", "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="tmean", "mean"], na.rm=T)


# Non-significant temperature range
summary(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="tmean" & is.na(spp.deriv$sig), "tmean"])
# ------

# ------
# Precipitation
# ------
summary(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="precip", ])

# Slope mean & SD (whole range)
mean(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="precip", "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="precip", "mean"], na.rm=T)

fagr.range  <- range(spp.post[spp.post$Species=="FAGR" & spp.post$Effect=="precip", "mean"], na.rm=T)
spp.post[spp.post$Species=="FAGR" & spp.post$Effect=="precip" & spp.post$mean==fagr.range[1],"x"]
spp.post[spp.post$Species=="FAGR" & spp.post$Effect=="precip" & spp.post$mean==fagr.range[2],"x"]
mean(spp.post[spp.post$Species=="FAGR" & spp.post$Effect=="precip", "mean"], na.rm=T)
sd(spp.post[spp.post$Species=="FAGR" & spp.post$Effect=="precip", "mean"], na.rm=T)



# Non-significant precipitation range
summary(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="precip" & is.na(spp.deriv$sig), "precip"])
fagr.ns <- range(spp.deriv[spp.deriv$Species=="FAGR" & spp.deriv$var=="precip" & is.na(spp.deriv$sig), "precip"])


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
mean(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="tmean", "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="tmean", "mean"], na.rm=T)

# Non-significant temperature range
summary(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="tmean" & is.na(spp.deriv$sig), "tmean"])
range(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="tmean", "tmean"], na.rm=T)
quru.ns <- range(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="tmean" & is.na(spp.deriv$sig), "tmean"], na.rm=T)
quru.ns

# Area of significant change
mean(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="tmean" & spp.deriv$tmean>quru.ns[2], "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="tmean" & spp.deriv$tmean>quru.ns[2], "mean"], na.rm=T)

mean(spp.post[spp.post$Species=="QURU" & spp.post$Effect=="tmean" & spp.post$x>quru.ns[2], "mean"], na.rm=T)
sd(spp.post[spp.post$Species=="QURU" & spp.post$Effect=="tmean" & spp.post$x>quru.ns[2], "mean"], na.rm=T)

# ------

# ------
# Precipitation
# ------
summary(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip", ])

# Slope mean & SD (whole range)
mean(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip", "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip", "mean"], na.rm=T)

# Non-significant precipitation range
summary(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip" & is.na(spp.deriv$sig), "precip"])
range(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip", "precip"], na.rm=T)
quru.ns <- range(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip" & is.na(spp.deriv$sig), "precip"], na.rm=T)
quru.ns


# positive slope area
mean(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip" & spp.deriv$precip<quru.ns[1], "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip" & spp.deriv$precip<quru.ns[1], "mean"], na.rm=T)

# negative slope area
mean(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip" & spp.deriv$precip>quru.ns[2], "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="QURU" & spp.deriv$var=="precip" & spp.deriv$precip>quru.ns[2], "mean"], na.rm=T)
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
mean(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="tmean", "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="tmean", "mean"], na.rm=T)

# Non-significant temperature range
summary(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="tmean" & is.na(spp.deriv$sig), "tmean"])
range(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="tmean", "tmean"], na.rm=T)
tsca.ns <- range(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="tmean" & is.na(spp.deriv$sig), "tmean"], na.rm=T)
tsca.ns

# Significant effect range
mean(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="tmean" & spp.deriv$tmean<tsca.ns[1], "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="tmean" & spp.deriv$tmean<tsca.ns[1], "mean"], na.rm=T)
# ------

# ------
# Precipitation
# ------
summary(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip", ])

# Slope mean & SD (whole range)
mean(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip", "mean"], na.rm=T)
sd(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip", "mean"], na.rm-T)

# Non-significant precipitation range
summary(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip" & is.na(spp.deriv$sig), "precip"])
range(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip", "precip"], na.rm=T)
tsca.ns <- range(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip" & is.na(spp.deriv$sig), "precip"], na.rm=T)
tsca.ns


# significant slope area
mean(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip" & spp.deriv$precip<tsca.ns[1], "mean"])
sd(spp.deriv[spp.deriv$Species=="TSCA" & spp.deriv$var=="precip" & spp.deriv$precip<tsca.ns[1], "mean"])
# ------
# ----------------

# ----------------------------------------


# ----------------------------------------
# Canopy class Temp/Precip gam
# ----------------------------------------
library(nlme)
load("processed_data/gam6_response_graph.Rdata")
load("processed_data/gam_results/gam6_canopy_species_add.Rdata") 
cc.gam <- gam6; rm(gam6)
cc.graph <- ci.terms.graph; rm(ci.terms.graph)

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
cc.post <- post.distns(model.gam=cc.gam, newdata=df.cc, vars=pred.cc, terms=T)
cc.post[,c("Species", "Canopy.Class")] <- cc.deriv[,c("Species", "Canopy.Class")]
summary(cc.deriv)
summary(cc.post)


# # Truncate out ranges we didn't observe in the data
for(SPP in unique(df.cc$Species)){
  for(CC in unique(df.cc$Canopy.Class)){
    for(PRED in pred.cc){
      cc.orig <- ifelse(CC=="Canopy", "C", paste(CC)) 
      prange <- range(data.use[data.use$Species==paste(SPP) & data.use$Canopy.Class==paste(cc.orig),PRED], na.rm=T)
      
      # Insert NAs for predictor values outside the range of observed
      # NOTE: Assumes things are in teh same order (which they should be)
      rows.na <- which(cc.deriv$Species==SPP & cc.deriv$Canopy.Class==CC & cc.deriv$var==PRED & 
                         !is.na(cc.deriv[cc.deriv$var==PRED,PRED]) & 
                         (cc.deriv[,PRED]<prange[1] | cc.deriv[,PRED]>prange[2]))
      cc.deriv[rows.na, PRED] <- NA
      cc.post[rows.na, "x"] <- NA
    }
  }
}
cc.deriv[is.na(cc.deriv$tmean),c("mean", "lwr", "upr", "sig")] <- NA
cc.deriv[is.na(cc.deriv$precip),c("mean", "lwr", "upr", "sig")] <- NA
cc.post[is.na(cc.post$x),c("mean", "lwr", "upr")] <- NA
summary(cc.deriv)
summary(cc.post)


# ----------------
# Comparing canopy class effects given species slopes
# ----------------
# Means parameterization
mean(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Canopy.Class=="Canopy", "mean"], na.rm=T); 
sd(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Canopy.Class=="Canopy", "mean"], na.rm=T) 

mean(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Canopy.Class=="I", "mean"], na.rm=T); 
sd(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Canopy.Class=="I", "mean"], na.rm=T) 

mean(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Canopy.Class=="U", "mean"], na.rm=T); 
sd(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Canopy.Class=="U", "mean"], na.rm=T) 

summary(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Canopy.Class=="U" & cc.deriv$tmean>16, "mean"]); 
mean(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Canopy.Class=="U" & cc.deriv$tmean>16, "mean"], na.rm=T); 
sd(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Canopy.Class=="U" & cc.deriv$tmean>16, "mean"], na.rm=T); 

mean(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Canopy.Class=="I" & cc.deriv$tmean>16, "mean"], na.rm=T); 
sd(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Canopy.Class=="I" & cc.deriv$tmean>16, "mean"], na.rm=T); 

mean(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$tmean>16, "mean"], na.rm=T); 
sd(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$tmean>16, "mean"], na.rm=T); 


mean(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Canopy.Class=="Canopy", "mean"]); 
sd(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Canopy.Class=="Canopy", "mean"]) 

mean(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Canopy.Class=="I", "mean"]); 
sd(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Canopy.Class=="I", "mean"]) 

mean(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Canopy.Class=="U", "mean"]); 
sd(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Canopy.Class=="U", "mean"]) 

mean(cc.post[cc.post$Effect=="precip" & cc.post$Canopy.Class=="Canopy" & cc.post$x>600 & cc.post$x<900, "mean"], na.rm=T); 
sd(cc.post[cc.post$Effect=="precip" & cc.post$Canopy.Class=="Canopy" & cc.post$x>600 & cc.post$x<900, "mean"], na.rm=T) 

mean(cc.post[cc.post$Effect=="precip" & cc.post$Canopy.Class=="I" & cc.post$x>600 & cc.post$x<900, "mean"], na.rm=T); 
sd(cc.post[cc.post$Effect=="precip" & cc.post$Canopy.Class=="I" & cc.post$x>600 & cc.post$x<900, "mean"], na.rm=T) 

range(cc.post[cc.post$Effect=="precip" & cc.post$Canopy.Class=="U" & cc.post$x>600 & cc.post$x<900, "mean"],  na.rm=T); 
mean(cc.post[cc.post$Effect=="precip" & cc.post$Canopy.Class=="U" & cc.post$x>600 & cc.post$x<900, "mean"], na.rm=T); 
sd(cc.post[cc.post$Effect=="precip" & cc.post$Canopy.Class=="U" & cc.post$x>600 & cc.post$x<900, "mean"], na.rm=T) 


# Looking at response, not sensitivity
mean(cc.post[cc.post$Effect=="tmean" & cc.post$Canopy.Class=="U" & cc.post$x<16, "mean"], na.rm=T); 
sd(cc.post[cc.post$Effect=="tmean" & cc.post$Canopy.Class=="U" & cc.post$x<16, "mean"], na.rm=T); 

mean(cc.post[cc.post$Effect=="tmean" & cc.post$Canopy.Class=="Canopy" & cc.post$x<16, "mean"], na.rm=T); 
sd(cc.post[cc.post$Effect=="tmean" & cc.post$Canopy.Class=="Canopy" & cc.post$x<16, "mean"], na.rm=T); 


cc.temp1 <- lme(mean ~ Canopy.Class -1, random = ~tmean + Canopy.Class|Species, data=cc.deriv[cc.deriv$var=="tmean", ], na.action=na.omit)
summary(cc.temp1)

cc.precip1 <- lme(mean ~ Canopy.Class-1, random = ~precip + Canopy.Class|Species, data=cc.deriv[cc.deriv$var=="precip", ])
summary(cc.precip1)

# Effects Parameterization (compare to canopy trees)
cc.temp2 <- lme(mean ~ Canopy.Class , random = ~tmean + Canopy.Class|Species, data=cc.deriv[cc.deriv$var=="tmean", ], na.action=na.omit)
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

# whole range
acru.temp.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="Canopy", "mean"]
acru.temp.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="I", "mean"]
acru.temp.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="U", "mean"]
mean(acru.temp.c, na.rm=T); sd(acru.temp.c, na.rm=T)
mean(acru.temp.i, na.rm=T); sd(acru.temp.i, na.rm=T)
mean(acru.temp.u, na.rm=T); sd(acru.temp.u, na.rm=T)

acru.temp <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU", ])
summary(acru.temp)

acru.temp2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU", ])
summary(acru.temp2)


# Looking at the range at which ACRU canopy plateaus
acru.temp.ns <- range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","tmean"], na.rm=T)


# <15.1 (where canopy turns negative)
acru.temp.lo.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$tmean<min(acru.temp.ns), "mean"]
acru.temp.lo.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="I" & cc.deriv$tmean<min(acru.temp.ns), "mean"]
acru.temp.lo.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="U" & cc.deriv$tmean<min(acru.temp.ns), "mean"]
mean(acru.temp.lo.c, na.rm=T); sd(acru.temp.lo.c, na.rm=T)
mean(acru.temp.lo.i, na.rm=T); sd(acru.temp.lo.i, na.rm=T)
mean(acru.temp.lo.u, na.rm=T); sd(acru.temp.lo.u, na.rm=T)

acru.temp.lo <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$tmean<min(acru.temp.ns), ])
summary(acru.temp.lo)

# >15.8˚C (where canopy turns negative)
acru.temp.hi.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$tmean>max(acru.temp.ns), "mean"]
acru.temp.hi.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="I" & cc.deriv$tmean>max(acru.temp.ns), "mean"]
acru.temp.hi.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="U" & cc.deriv$tmean>max(acru.temp.ns), "mean"]
mean(acru.temp.hi.c, na.rm=T); sd(acru.temp.hi.c, na.rm=T)
mean(acru.temp.hi.i, na.rm=T); sd(acru.temp.hi.i, na.rm=T)
mean(acru.temp.hi.u, na.rm=T); sd(acru.temp.hi.u, na.rm=T)

acru.temp.hi <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="ACRU"  & cc.deriv$tmean>max(acru.temp.ns), ])
summary(acru.temp.hi)
# -----

# -----
# Precipitation
# -----
range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU","precip"])

# Looking at the range at which ACRU canopy plateaus
range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="Canopy","precip"], na.rm=T)
acru.precip.ns <- range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","precip"], na.rm=T)
acru.precip.ns

# whole range
acru.precip.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="Canopy", "mean"]
acru.precip.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="I", "mean"]
acru.precip.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="U", "mean"]
mean(acru.precip.c, na.rm=T); sd(acru.precip.c, na.rm=T)
mean(acru.precip.i, na.rm=T); sd(acru.precip.i, na.rm=T)
mean(acru.precip.u, na.rm=T); sd(acru.precip.u, na.rm=T)

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


# ---
# low precip2 -- below 400 mm (~20 percentile; consistent comparison across species) (where there is significant precip signal)
# ---
# SLOPES Comparison
acru.precip.lo2.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$precip<400, "mean"]
acru.precip.lo2.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="I" & cc.deriv$precip<400, "mean"]
acru.precip.lo2.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$Canopy.Class=="U" & cc.deriv$precip<400, "mean"]
mean(acru.precip.lo2.c); sd(acru.precip.lo2.c)
mean(acru.precip.lo2.i); sd(acru.precip.lo2.i)
mean(acru.precip.lo2.u); sd(acru.precip.lo2.u)

acru.precip.lo.2 <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$precip<400, ])
summary(acru.precip.lo.2)

acru.precip.lo2.2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="ACRU" & cc.deriv$precip<400, ])
summary(acru.precip.lo2.2)

# EFFECTS comparison
acru.precip.lo2.c.eff <- cc.post[cc.post$Effect=="precip" & cc.post$Species=="ACRU" & cc.post$Canopy.Class=="Canopy" & cc.post$x<400, "mean"]
acru.precip.lo2.i.eff <- cc.post[cc.post$Effect=="precip" & cc.post$Species=="ACRU" & cc.post$Canopy.Class=="I" & cc.post$x<400, "mean"]
acru.precip.lo2.u.eff <- cc.post[cc.post$Effect=="precip" & cc.post$Species=="ACRU" & cc.post$Canopy.Class=="U" & cc.post$x<400, "mean"]
mean(acru.precip.lo2.c.eff, na.rm=T); sd(acru.precip.lo2.c.eff, na.rm=T)
mean(acru.precip.lo2.i.eff, na.rm=T); sd(acru.precip.lo2.i.eff, na.rm=T)
mean(acru.precip.lo2.u.eff, na.rm=T); sd(acru.precip.lo2.u.eff, na.rm=T)

acru.precip.lo.2.eff <- lm(mean ~ Canopy.Class, data=cc.graph[cc.graph$Effect=="precip" & cc.graph$Species=="ACRU" & cc.graph$x<400, ])
summary(acru.precip.lo.2.eff)

# ---

# -----
# ------------

# ------------
# FAGR
# ------------
# -----
# Temperature
# -----
range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR","tmean"])

# whole range (dominant is non-temp significant)
fagr.temp.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="Canopy", "mean"]
fagr.temp.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="I", "mean"]
fagr.temp.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="U", "mean"]
mean(fagr.temp.c, na.rm=T); sd(fagr.temp.c, na.rm=T)
mean(fagr.temp.i, na.rm=T); sd(fagr.temp.i, na.rm=T)
mean(fagr.temp.u, na.rm=T); sd(fagr.temp.u, na.rm=T)


# Looking at the range at which FAGR canopy plateaus
range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="Canopy","tmean"], na.rm=T)
fagr.temp.ns <- range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","tmean"], na.rm=T)

fagr.temp.ns.u <- range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="FAGR" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="U","tmean"], na.rm=T)


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
range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="Canopy","precip"], na.rm=T)
fagr.precip.ns <- range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","precip"], na.rm=T)
fagr.precip.ns

# whole range (no portion of N.S. change)
fagr.precip.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="Canopy", "mean"]
fagr.precip.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="I", "mean"]
fagr.precip.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="U", "mean"]
mean(fagr.precip.c, na.rm=T); sd(fagr.precip.c, na.rm=T)
mean(fagr.precip.i, na.rm=T); sd(fagr.precip.i, na.rm=T)
mean(fagr.precip.u, na.rm=T); sd(fagr.precip.u, na.rm=T)

fagr.precip <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR", ])
summary(fagr.precip)

fagr.precip2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR", ])
summary(fagr.precip2)

# ---
# low precip2 -- below 400 mm (~20 percentile; consistent comparison across species) (where there is significant precip signal)
# ---
# SLOPES Comparison
fagr.precip.lo2.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$precip<400, "mean"]
fagr.precip.lo2.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="I" & cc.deriv$precip<400, "mean"]
fagr.precip.lo2.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & cc.deriv$Canopy.Class=="U" & cc.deriv$precip<400, "mean"]
mean(fagr.precip.lo2.c, na.rm=T); sd(fagr.precip.lo2.c, na.rm=T)
mean(fagr.precip.lo2.i, na.rm=T); sd(fagr.precip.lo2.i, na.rm=T)
mean(fagr.precip.lo2.u, na.rm=T); sd(fagr.precip.lo2.u, na.rm=T)

fagr.precip.lo.2 <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & cc.deriv$precip<400, ])
summary(fagr.precip.lo.2)

fagr.precip.lo2.2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="FAGR" & cc.deriv$precip<400, ])
summary(fagr.precip.lo2.2)

# EFFECTS comparison
fagr.precip.lo2.c.eff <- cc.post[cc.post$Effect=="precip" & cc.post$Species=="FAGR" & cc.post$Canopy.Class=="Canopy" & cc.post$x<400, "mean"]
fagr.precip.lo2.i.eff <- cc.post[cc.post$Effect=="precip" & cc.post$Species=="FAGR" & cc.post$Canopy.Class=="I" & cc.post$x<400, "mean"]
fagr.precip.lo2.u.eff <- cc.post[cc.post$Effect=="precip" & cc.post$Species=="FAGR" & cc.post$Canopy.Class=="U" & cc.post$x<400, "mean"]
mean(fagr.precip.lo2.c.eff, na.rm=T); sd(fagr.precip.lo2.c.eff, na.rm=T)
mean(fagr.precip.lo2.i.eff, na.rm=T); sd(fagr.precip.lo2.i.eff, na.rm=T)
mean(fagr.precip.lo2.u.eff, na.rm=T); sd(fagr.precip.lo2.u.eff, na.rm=T)

fagr.precip.lo.2.eff <- lm(mean ~ Canopy.Class, data=cc.graph[cc.graph$Effect=="precip" & cc.graph$Species=="FAGR" & cc.graph$x<400, ])
summary(fagr.precip.lo.2.eff)

# ---

# -----
# ------------

# ------------
# QURU
# ------------
# -----
# Temperature
# -----
range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU","tmean"])

# whole range (dominant is non-temp significant for most of the range)
quru.temp.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="Canopy", "mean"]
quru.temp.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="I", "mean"]
quru.temp.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="U", "mean"]
mean(quru.temp.c); sd(quru.temp.c)
mean(quru.temp.i); sd(quru.temp.i)
mean(quru.temp.u); sd(quru.temp.u)

# Looking at the range at which QURU canopy plateaus
range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="Canopy","tmean"], na.rm=T)
quru.temp.ns <- range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","tmean"], na.rm=T)
quru.temp.ns

# whole range (dominant is non-temp significant for most of the range)
quru.temp.c.hi <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$tmean>max(quru.temp.ns), "mean"]
quru.temp.i.hi <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="I" & cc.deriv$tmean>max(quru.temp.ns), "mean"]
quru.temp.u.hi <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="U" & cc.deriv$tmean>max(quru.temp.ns), "mean"]
mean(quru.temp.c.hi, na.rm=T); sd(quru.temp.c.hi, na.rm=T)
mean(quru.temp.i.hi, na.rm=T); sd(quru.temp.i.hi, na.rm=T)
mean(quru.temp.u.hi, na.rm=T); sd(quru.temp.u.hi, na.rm=T)


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
range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="Canopy","precip"], na.rm=T)
quru.precip.ns <- range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","precip"], na.rm=T)
quru.precip.ns

# whole range
quru.precip.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="Canopy", "mean"]
quru.precip.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="I", "mean"]
quru.precip.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="U", "mean"]
mean(quru.precip.c, na.rm=T); sd(quru.precip.c, na.rm=T)
mean(quru.precip.i, na.rm=T); sd(quru.precip.i, na.rm=T)
mean(quru.precip.u, na.rm=T); sd(quru.precip.u, na.rm=T)

# high precip range (where canopy is signif)
quru.precip.hi.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$precip>max(quru.precip.ns), "mean"]
quru.precip.hi.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="I" & cc.deriv$precip>max(quru.precip.ns), "mean"]
quru.precip.hi.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="U" & cc.deriv$precip>max(quru.precip.ns), "mean"]
mean(quru.precip.hi.c, na.rm=T); sd(quru.precip.hi.c, na.rm=T)
mean(quru.precip.hi.i, na.rm=T); sd(quru.precip.hi.i, na.rm=T)
mean(quru.precip.hi.u, na.rm=T); sd(quru.precip.hi.u, na.rm=T)
mean(quru.precip.hi.u, na.rm=T)/mean(quru.precip.hi.c, na.rm=T)

quru.precip <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU", ])
summary(quru.precip)

quru.precip2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU", ])
summary(quru.precip2)

# High precip (where QURU shows decline)
quru.precip.hi <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$precip>max(quru.precip.ns), ])
summary(quru.precip.hi)

quru.precip.hi2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$precip>max(quru.precip.ns), ])
summary(quru.precip.hi2)

# ---
# low precip2 -- below 400 mm (~20 percentile; consistent comparison across species) (where there is significant precip signal)
# ---
# SLOPES Comparison
quru.precip.lo2.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$precip<400, "mean"]
quru.precip.lo2.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="I" & cc.deriv$precip<400, "mean"]
quru.precip.lo2.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$Canopy.Class=="U" & cc.deriv$precip<400, "mean"]
mean(quru.precip.lo2.c); sd(quru.precip.lo2.c)
mean(quru.precip.lo2.i); sd(quru.precip.lo2.i)
mean(quru.precip.lo2.u); sd(quru.precip.lo2.u)

quru.precip.lo.2 <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$precip<400, ])
summary(quru.precip.lo.2)

quru.precip.lo2.2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="QURU" & cc.deriv$precip<400, ])
summary(quru.precip.lo2.2)

# EFFECTS comparison
quru.precip.lo2.c.eff <- cc.post[cc.post$Effect=="precip" & cc.post$Species=="QURU" & cc.post$Canopy.Class=="Canopy" & cc.post$x<400, "mean"]
quru.precip.lo2.i.eff <- cc.post[cc.post$Effect=="precip" & cc.post$Species=="QURU" & cc.post$Canopy.Class=="I" & cc.post$x<400, "mean"]
quru.precip.lo2.u.eff <- cc.post[cc.post$Effect=="precip" & cc.post$Species=="QURU" & cc.post$Canopy.Class=="U" & cc.post$x<400, "mean"]
mean(quru.precip.lo2.c.eff, na.rm=T); sd(quru.precip.lo2.c.eff, na.rm=T)
mean(quru.precip.lo2.i.eff, na.rm=T); sd(quru.precip.lo2.i.eff, na.rm=T)
mean(quru.precip.lo2.u.eff, na.rm=T); sd(quru.precip.lo2.u.eff, na.rm=T)


quru.precip.lo.2.eff <- lm(mean ~ Canopy.Class, data=cc.graph[cc.graph$Effect=="precip" & cc.graph$Species=="QURU" & cc.graph$x<400, ])
summary(quru.precip.lo.2.eff)

# ---

# -----
# ------------


# ------------
# TSCA
# ------------
# -----
# Temperature
# -----

# whole range (dominant is non-temp significant)
tsca.temp.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="Canopy", "mean"]
tsca.temp.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="I", "mean"]
tsca.temp.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="U", "mean"]
mean(tsca.temp.c, na.rm=T); sd(tsca.temp.c, na.rm=T)
mean(tsca.temp.i, na.rm=T); sd(tsca.temp.i, na.rm=T)
mean(tsca.temp.u, na.rm=T); sd(tsca.temp.u, na.rm=T)


trange.u <- range(data.use[data.use$Species=="TSCA" & data.use$Canopy.Class=="U","tmean"], na.rm=T)
tsca.temp.u2 <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="U" & cc.deriv$tmean>=trange.u[1], "mean"]
tsca.temp.c2 <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$tmean>=trange.u[1], "mean"]
mean(tsca.temp.u2, na.rm=T); sd(tsca.temp.u2, na.rm=T)
mean(tsca.temp.c2, na.rm=T); sd(tsca.temp.c2, na.rm=T)
mean(tsca.temp.u2, na.rm=T)/mean(tsca.temp.c2, na.rm=T)

# Looking at the range at which TSCA canopy plateaus
range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA","tmean"], na.rm=T)
tsca.temp.ns <- range(cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","tmean"], na.rm=T)
tsca.temp.ns

# high temp range (where canopy is signif)
TSCA.temp.hi.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$tmean>max(tsca.temp.ns), "mean"]
TSCA.temp.hi.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="I" & cc.deriv$tmean>max(tsca.temp.ns), "mean"]
TSCA.temp.hi.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="U" & cc.deriv$tmean>max(tsca.temp.ns), "mean"]
mean(TSCA.temp.hi.c, na.rm=T); sd(TSCA.temp.hi.c, na.rm=T)
mean(TSCA.temp.hi.i, na.rm=T); sd(TSCA.temp.hi.i, na.rm=T)
mean(TSCA.temp.hi.u, na.rm=T); sd(TSCA.temp.hi.u, na.rm=T)

# lo temp range (where canopy is signif)
TSCA.temp.lo.c <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$tmean<min(tsca.temp.ns), "mean"]
TSCA.temp.lo.i <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="I" & cc.deriv$tmean<min(tsca.temp.ns), "mean"]
TSCA.temp.lo.u <- cc.deriv[cc.deriv$var=="tmean" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="U" & cc.deriv$tmean<min(tsca.temp.ns), "mean"]
mean(TSCA.temp.lo.c, na.rm=T); sd(TSCA.temp.lo.c, na.rm=T)
mean(TSCA.temp.lo.i, na.rm=T); sd(TSCA.temp.lo.i, na.rm=T)
mean(TSCA.temp.lo.u, na.rm=T); sd(TSCA.temp.lo.u, na.rm=T)


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
range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="Canopy","precip"], na.rm=T)
tsca.precip.ns <- range(cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & is.na(cc.deriv$sig) & cc.deriv$Canopy.Class=="Canopy","precip"], na.rm=T)
tsca.precip.ns

# whole range
tsca.precip.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="Canopy", "mean"]
tsca.precip.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="I", "mean"]
tsca.precip.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="U", "mean"]
mean(tsca.precip.c, na.rm=T); sd(tsca.precip.c, na.rm=T)
mean(tsca.precip.i, na.rm=T); sd(tsca.precip.i, na.rm=T)
mean(tsca.precip.u, na.rm=T); sd(tsca.precip.u, na.rm=T)


tsca.precip <- lm(mean ~ precip*Canopy.Class - Canopy.Class - precip -1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA", ])
summary(tsca.precip)

# Lo precip where Canopy is sensitive
tsca.precip.lo <- lm(mean ~ precip*Canopy.Class - Canopy.Class - precip -1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$precip<min(tsca.precip.ns), ])
summary(tsca.precip.lo)

# Hi precip where Canopy is NOT sensitive
tsca.precip.hi <- lm(mean ~ precip*Canopy.Class - Canopy.Class - precip -1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$precip>min(tsca.precip.ns), ])
summary(tsca.precip.hi)

# ---
# low precip2 -- below 400 mm (~20 percentile; consistent comparison across species) (where there is significant precip signal)
# ---
# SLOPES Comparison
tsca.precip.lo2.c <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="Canopy" & cc.deriv$precip<400, "mean"]
tsca.precip.lo2.i <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="I" & cc.deriv$precip<400, "mean"]
tsca.precip.lo2.u <- cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$Canopy.Class=="U" & cc.deriv$precip<400, "mean"]
mean(tsca.precip.lo2.c); sd(tsca.precip.lo2.c)
mean(tsca.precip.lo2.i); sd(tsca.precip.lo2.i)
mean(tsca.precip.lo2.u); sd(tsca.precip.lo2.u)

tsca.precip.lo.2 <- lm(mean ~ Canopy.Class, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$precip<400, ])
summary(tsca.precip.lo.2)

tsca.precip.lo2.2 <- lm(mean ~ Canopy.Class-1, data=cc.deriv[cc.deriv$var=="precip" & cc.deriv$Species=="TSCA" & cc.deriv$precip<400, ])
summary(tsca.precip.lo2.2)

# EFFECTS comparison
tsca.precip.lo2.c.eff <- cc.post[cc.post$Effect=="precip" & cc.post$Species=="TSCA" & cc.post$Canopy.Class=="Canopy" & cc.post$x<400, "mean"]
tsca.precip.lo2.i.eff <- cc.post[cc.post$Effect=="precip" & cc.post$Species=="TSCA" & cc.post$Canopy.Class=="I" & cc.post$x<400, "mean"]
tsca.precip.lo2.u.eff <- cc.post[cc.post$Effect=="precip" & cc.post$Species=="TSCA" & cc.post$Canopy.Class=="U" & cc.post$x<400, "mean"]
mean(tsca.precip.lo2.c.eff, na.rm=T); sd(tsca.precip.lo2.c.eff, na.rm=T)
mean(tsca.precip.lo2.i.eff, na.rm=T); sd(tsca.precip.lo2.i.eff, na.rm=T)
mean(tsca.precip.lo2.u.eff, na.rm=T); sd(tsca.precip.lo2.u.eff, na.rm=T)


tsca.precip.lo.2.eff <- lm(mean ~ Canopy.Class, data=cc.graph[cc.graph$Effect=="precip" & cc.graph$Species=="TSCA" & cc.graph$x<400, ])
summary(tsca.precip.lo.2.eff)

# ---

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
