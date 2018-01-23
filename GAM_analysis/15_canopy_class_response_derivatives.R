# After generating gam 2 in script "3_gamm.R" and sourcing the file "0_Calculate_GAMM_Derivs.R"

# This will calculate the slope of the line for the factors you're interested in
mod.derivs <- calc.derivs(gam2, new.dat, vars=c("tmean", "precip", "dbh.recon"))

# Analyzing the mean slopes for different factors/canopy classes
# Tmean 

summary(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="D" & mod.derivs$PlotID=="HOW1" ,])
# mean(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="D" & mod.derivs$PlotID=="HOW1" ,"mean"]); sd(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="D" & mod.derivs$PlotID=="HOW1" ,"mean"])
mean(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="D" ,"mean"]); sd(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="D","mean"])
mean(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="I" ,"mean"]); sd(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="I","mean"])
mean(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="S" ,"mean"]); sd(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="S","mean"])

abs(mean(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="I" ,"mean"]))/abs(mean(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="D" ,"mean"]))
abs(mean(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="S" ,"mean"]))/abs(mean(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="D" ,"mean"]))
abs(mean(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="S" ,"mean"]))/abs(mean(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="I" ,"mean"]))
# getting summary stats & seeing whether the slope of the line is ever significantly different from 0 or not
summary(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="D" ,])
summary(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="I" ,])
summary(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="S" ,])

# Looking at only the portion of the supressed tree that is significantly changing
summary(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="S" & !is.na(mod.derivs$sig),])
mean(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="S" & !is.na(mod.derivs$sig),"mean"]); sd(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="S" & !is.na(mod.derivs$sig),"mean"])

summary(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="S" & is.na(mod.derivs$sig),]) # Portion where there is no significant change


# Precip



summary(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="D" & mod.derivs$PlotID=="HOW1" ,])
# mean(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="D" & mod.derivs$PlotID=="HOW1" ,"mean"]); sd(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="D" & mod.derivs$PlotID=="HOW1" ,"mean"])
mean(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="D" ,"mean"]); sd(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="D","mean"])
mean(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="I" ,"mean"]); sd(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="I","mean"])
mean(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="S" ,"mean"]); sd(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="S","mean"])

abs(mean(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="I" ,"mean"]))/abs(mean(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="D" ,"mean"]))
abs(mean(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="S" ,"mean"]))/abs(mean(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="D" ,"mean"]))
abs(mean(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="S" ,"mean"]))/abs(mean(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="I" ,"mean"]))

# getting summary stats & seeing whether the slope of the line is ever significantly different from 0 or not
summary(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="D" ,])
summary(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="I" ,])
summary(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="S" ,])

# Looking at only the portion of the supressed tree that is significantly changing
summary(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$sig),])
summary(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="D" & is.na(mod.derivs$sig),]) # Portion where there is no significant change

mean(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$sig),"mean"]); sd(mod.derivs[mod.derivs$var=="precip" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$sig),"mean"]) 

# Size
 
# Truncating the size effects to the range of observations for each canpoy class
mod.derivs.orig <- mod.derivs
mod.derivs <- mod.derivs.orig
summary(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$mean),])
summary(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$mean),])
for(s in unique(test$Canopy.Class)){
  dbh.min <- min(test[test$Canopy.Class==s, "dbh.recon"])
  dbh.max <- max(test[test$Canopy.Class==s, "dbh.recon"])
  
  # g2.ci.out2$x <- ifelse(g2.ci.out2$Canopy.Class!=s | g2.ci.out2$Effect!="dbh.recon" | (g2.ci.out2$x>=dbh.min & g2.ci.out2$x<=dbh.max), g2.ci.out2$x, NA)
  # summary(mod.derivs[mod.derivs$Canopy.Class==s & mod.derivs$var=="dbh.recon" , ])
  # summary(mod.derivs[mod.derivs$Canopy.Class==s & mod.derivs$var=="dbh.recon" & mod.derivs$dbh.recon<dbh.min | mod.derivs$dbh.recon>dbh.max, ])
  # dim(mod.derivs[mod.derivs$Canopy.Class==s & mod.derivs$var=="dbh.recon" & mod.derivs$dbh.recon<dbh.min | mod.derivs$dbh.recon>dbh.max, ])
  mod.derivs[mod.derivs$Canopy.Class==s & mod.derivs$var=="dbh.recon" & (mod.derivs$dbh.recon<dbh.min | mod.derivs$dbh.recon>dbh.max), c("mean", "lwr", "upr")] <- NA
  
}
summary(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D",])
summary(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="D",])


summary(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="D" & mod.derivs$PlotID=="HOW1" ,])
# mean(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="D" & mod.derivs$PlotID=="HOW1" ,"mean"]); sd(mod.derivs[mod.derivs$var=="tmean" & mod.derivs$Canopy.Class=="D" & mod.derivs$PlotID=="HOW1" ,"mean"])
mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" ,"mean"],na.rm=T); sd(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D","mean"],na.rm=T)
mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="I" ,"mean"], na.rm=T); sd(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="I","mean"], na.rm=T)
mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="S" ,"mean"], na.rm=T); sd(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="S","mean"], na.rm=T)

abs(mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="I" ,"mean"], na.rm=T))/abs(mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" ,"mean"], na.rm=T))
abs(mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="S" ,"mean"], na.rm=T))/abs(mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" ,"mean"], na.rm=T))
abs(mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="S" ,"mean"], na.rm=T))/abs(mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="I" ,"mean"], na.rm=T))

# getting summary stats & seeing whether the slope of the line is ever significantly different from 0 or not
summary(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" ,])
summary(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="I" ,])
summary(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="S" ,])

# Looking at only the portion of the supressed tree that is significantly changing
summary(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$mean),])
summary(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean),])
summary(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & is.na(mod.derivs$sig) & !is.na(mod.derivs$mean),]) # Portion where there is no significant change

# looking at the mean slope of the significant portions; two sig regions
# 1-54.9
mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon < 55, "mean"]) ; sd(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon < 55, "mean"])

# greater than 88
mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon > 88, "mean"]) ; sd(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon > 88, "mean"])


# intermediate
summary(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="I" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean),])
summary(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="I" & is.na(mod.derivs$sig) & !is.na(mod.derivs$mean),]) # Portion where there is no significant change

# Shows one significant range
# 1-46
mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="I" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon < 46, "mean"]) ; sd(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="I" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon < 46, "mean"])

# Suppressed
summary(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="S" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean),])
summary(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="S" & is.na(mod.derivs$sig) & !is.na(mod.derivs$mean),]) # Portion where there is no significant change

# Show two ranges of significant size

# 0-28cm
mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="S" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon < 28, "mean"]) ; sd(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="I" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon < 28, "mean"])


# 30.1-36cm
mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="S" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon > 30, "mean"]) ; sd(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="I" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon > 30, "mean"])


# Comparing positive effects

#I vs D
abs(mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="I" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon < 46, "mean"])) /abs(mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon < 55, "mean"]))

# S vs D
abs(mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="S" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon > 30, "mean"]))/abs(mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon < 55, "mean"]))

# Comparing negative effects
# S vs D

abs(mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="S" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon > 30, "mean"])) / abs(mean(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & !is.na(mod.derivs$sig) & !is.na(mod.derivs$mean) & mod.derivs$dbh.recon > 88, "mean"]))


summary(as.factor(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & mod.derivs$PlotID=="HOW1" ,"dbh.recon"]))
length(unique(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & mod.derivs$PlotID=="HOW1","dbh.recon"]))
length(unique(mod.derivs[mod.derivs$var=="dbh.recon" & mod.derivs$Canopy.Class=="D" & mod.derivs$PlotID=="HOW1","Year"]))
length(unique(mod.derivs$dbh.recon))
