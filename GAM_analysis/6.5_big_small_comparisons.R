# script to look at biggest and smallest quantiles of Ch. 2 eastern chronologies

load("processed_data/res_site_chronologies.Rdata")

summary(sites.crn)

# Pulling the 10% largest years from each site
big.years <- sites.crn[, 2:ncol(sites.crn)]
summary(big.years)
sites <- c("Harvard", "Howland", "Missouri", "Morgan_Monroe", "Oak_openings")

big.years <- data.frame(array(dim=c(round(nrow(sites.crn)*.1,0), length(sites))))
names(big.years) <- sites

for(s in names(big.years)){
  magic.number <- quantile(sites.crn[,s], 0.9, na.rm=T)
  
  years <- sites.crn$Year[which(sites.crn[,s]>=magic.number)]
  big.years[,s] <- years
}
big.years

# Pulling the 10% smallest years from each site

small.years <- data.frame(array(dim=c(round(nrow(sites.crn)*.1,0), length(sites))))
names(small.years) <- sites

for(s in names(small.years)){
  magic.number <- quantile(sites.crn[,s], 0.1, na.rm=T)
  
  years <- sites.crn$Year[which(sites.crn[,s]<=magic.number)]
  small.years[,s] <- years
}
small.years

# Pulling the middle 10% of years from each site

medium.years <- data.frame(array(dim=c(round(nrow(sites.crn)*.1,0), length(sites))))
names(medium.years) <- sites

for(s in names(medium.years)){
  magic.number1 <- quantile(sites.crn[,s], 0.45, na.rm=T)
  magic.number2 <- quantile(sites.crn[,s], 0.55, na.rm=T)
  
  
  years <- sites.crn$Year[which(!is.na(sites.crn[,s]) & sites.crn[,s]>=magic.number1 & sites.crn[,s]<=magic.number2 & !is.na(sites.crn[,s]))]
  medium.years[,s] <- c(years, rep(NA, nrow(medium.years)-length(years)))
}
medium.years

load("processed_data/sites_use_climate.Rdata")
summary(climate.site[[1]])


big.temp <- big.years

names(climate.site) <- recode(names(climate.site), "'Missouri Ozark'='Missouri'; 'Morgan Monroe State Park'='Morgan_Monroe'; 'Oak Openings Toledo'='Oak_openings'")
summary(climate.site)

# Pulling Temperatures
# Big Years
big.temp <- big.years
for(s in names(climate.site)){
  big.temp[,s] <- climate.site[[s]][climate.site[[s]]$Year %in% big.years[,s],"tmean"]
}
big.temp
big.temp.stack <- stack(big.temp)
names(big.temp.stack) <- c("value", "Site")
big.temp.stack$size <- as.factor("big")
big.temp.stack$factor <- as.factor("tmean")
summary(big.temp.stack)  


# Med Years

medium.temp <- medium.years
for(s in names(climate.site)){
  temp <- climate.site[[s]][climate.site[[s]]$Year %in% medium.years[,s],"tmean"]
  medium.temp[,s] <- c(temp, rep(NA, nrow(medium.temp)-length(temp)))
}
medium.temp
medium.temp.stack <- stack(medium.temp)
names(medium.temp.stack) <- c("value", "Site")
medium.temp.stack$size <- as.factor("medium")
medium.temp.stack$factor <- as.factor("tmean")
summary(medium.temp.stack)  


# Small Years
small.temp <- small.years
for(s in names(climate.site)){
  small.temp[,s] <- climate.site[[s]][climate.site[[s]]$Year %in% small.years[,s],"tmean"]
}
small.temp
small.temp.stack <- stack(small.temp)
names(small.temp.stack) <- c("value", "Site")
small.temp.stack$size <- as.factor("small")
small.temp.stack$factor <- as.factor("tmean")
summary(small.temp.stack)  


# Pulling Precipitation

# Big Years
big.precip <- big.years
for(s in names(climate.site)){
  big.precip[,s] <- climate.site[[s]][climate.site[[s]]$Year %in% big.years[,s],"precip"]
}
big.precip
big.precip.stack <- stack(big.precip)
names(big.precip.stack) <- c("value", "Site")
big.precip.stack$size <- as.factor("big")
big.precip.stack$factor <- as.factor("precip")
summary(big.precip.stack)  


# Med Years

medium.precip <- medium.years
for(s in names(climate.site)){
  precip <- climate.site[[s]][climate.site[[s]]$Year %in% medium.years[,s],"precip"]
  medium.precip[,s] <- c(precip, rep(NA, nrow(medium.precip)-length(precip)))
}
medium.precip
medium.precip.stack <- stack(medium.precip)
names(medium.precip.stack) <- c("value", "Site")
medium.precip.stack$size <- as.factor("medium")
medium.precip.stack$factor <- as.factor("precip")
summary(medium.precip.stack)  


# Small Years
small.precip <- small.years
for(s in names(climate.site)){
  small.precip[,s] <- climate.site[[s]][climate.site[[s]]$Year %in% small.years[,s],"precip"]
}
small.precip
small.precip.stack <- stack(small.precip)
names(small.precip.stack) <- c("value", "Site")
small.precip.stack$size <- as.factor("small")
small.precip.stack$factor <- as.factor("precip")
summary(small.precip.stack)  



# Combining everything together

climate.compare <- rbind(big.temp.stack, medium.temp.stack, small.temp.stack, big.precip.stack, medium.precip.stack, small.precip.stack)
summary(climate.compare)

climate.compare$Site <- factor(climate.compare$Site, levels=c("Missouri", "Morgan_Monroe", "Oak_openings", "Harvard", "Howland"))

#----------------------------------------------------------------------------------------
# Running ANOVA to see if differences exist between big, med and small years for Tmean
----------------------------------------------------------------------------------------
  
anova.temp <-list()

for(s in sites){
  anova.temp[[s]] <- aov(value ~ size, data=climate.compare[climate.compare$Site==s & climate.compare$factor=="tmean",])  
}

summary(anova.temp)

summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests

# Tukey Post Hoc
tukey.temp <- list()

for(s in sites){
  tukey.temp[[s]] <- TukeyHSD(anova.temp[[s]])
}


# Bonferoni Post hoc
bonf.temp <- list()

for(s in sites){
  bonf.temp[[s]] <- with(climate.compare[climate.compare$Site==s & climate.compare$factor=="tmean",], pairwise.t.test(value,size, p.adj="bonferroni", paired=F))  
}

#--------------------------------------------------------------------------------
# Precipitation
#--------------------------------------------------------------------------------

# Running ANOVA to see if differences exist between big, med and small years

anova.precip <-list()

for(s in sites){
  anova.precip[[s]] <- aov(value ~ size, data=climate.compare[climate.compare$Site==s & climate.compare$factor=="precip",])  
}

summary(anova.precip)

summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests

# Tukey Post Hoc
tukey.precip <- list()

for(s in sites){
  tukey.precip[[s]] <- TukeyHSD(anova.precip[[s]])
}


# Bonferoni Post hoc
bonf.precip <- list()

for(s in sites){
  bonf.precip[[s]] <- with(climate.compare[climate.compare$Site==s & climate.compare$factor=="precip",], pairwise.t.test(value,size, p.adj="bonferroni", paired=F))  
}


climate.compare$State <- recode(climate.compare$Site, "'Missouri'='MO';'Morgan_Monroe'='IN';'Oak_openings'='OH';'Harvard'='MA';'Howland'='ME'")
climate.compare$State <- factor(climate.compare$State, levels=c("MO", "IN", "OH", "MA", "ME"))

climate.compare$size <- recode(climate.compare$size, "'big'='Large';'medium'='Med.';'small'='Small'")

pdf(file="figures/Prelim_Figures/big_small_climate_boxplot.pdf", height=8, width=13.5)
ggplot(data=climate.compare) + facet_grid(factor~State, scales="free_y")+
  geom_boxplot(aes(x=size, y=value, fill=size))+
  poster.theme2+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))
dev.off()  


