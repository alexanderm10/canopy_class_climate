comp.data <- read.csv("sensitivity_comparisons.csv", header = T)

summary(comp.data)

range(comp.data[comp.data$clim.var=="temp" & comp.data$canopy.class=="mid","value"])
range(comp.data[comp.data$clim.var=="precip" & comp.data$canopy.class=="mid","value"])

range(comp.data[comp.data$clim.var=="temp" & comp.data$canopy.class=="low","value"])
range(comp.data[comp.data$clim.var=="precip" & comp.data$canopy.class=="low","value"])


mean.comp <- read.csv("mean_sensitivity_comparisons.csv", header=T)
summary(mean.comp)

# looking at the differences between teh spp level response and each canopy class
sens.diff <- data.frame(spp = rep(c("ACRU", "FAGR", "QURU", "TSCA"),each=3))
sens.diff$canopy.class <- rep(c("upper", "mid", "low"), 4)
sens.diff$diff <- NA

sens.diff <- rbind(sens.diff, sens.diff)
sens.diff$clim.var <- as.factor(rep(c("temp", "precip"), each=12))
summary(sens.diff)

summary(mean.comp)

for(i in unique(sens.diff$spp)){
  for(x in unique(sens.diff$clim.var)){
    spp.mean <- mean.comp[mean.comp$spp==i & mean.comp$clim.var==x & mean.comp$canopy.class=="all","mean"]
  for(j in unique(sens.diff$canopy.class)){
    mean.temp <- mean.comp[mean.comp$spp==i & mean.comp$canopy.class==j & mean.comp$clim.var==x,"mean"]
    
      sens.diff[sens.diff$spp==i & sens.diff$canopy.class==j & sens.diff$clim.var==x, "diff"] <- abs(spp.mean-mean.temp)
    }
  }
}

sens.diff

diff.cc <- data.frame(spp = unique(sens.diff$spp))

for(i in sens.diff$spp){
  for(x in sens.diff$clim.var){
    min.value <- min(sens.diff[sens.diff$spp==i & sens.diff$clim.var==x,"diff"])
    diff.cc[diff.cc$spp==i,"small.diff"] <- sens.diff$canopy.class[which(sens.diff[sens.diff$spp==i & sens.diff$clim.var==x,"diff"]==min.value)]
  }
}

diff.cc
