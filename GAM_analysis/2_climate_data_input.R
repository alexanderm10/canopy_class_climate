library(car)
library(dplR)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}

# Loading in climate data from the PRISM extracts
t.mean <- read.csv("climate_data/prism_met_sites_wide_tmean.csv", header=T)
precip <- read.csv("climate_data/prism_met_sites_wide_ppt.csv", header=T)
t.min <- read.csv("climate_data/prism_met_sites_wide_tmin.csv", header=T)
t.max <- read.csv("climate_data/prism_met_sites_wide_tmax.csv", header=T)



summary(t.mean)

# We have Harvard P1 and Harvard P2 as separate sites

harvard <- c("Harvard Forest (Tower Plot 1)", "Harvard Forest (Tower Plot 2)")

t.mean$Site.Name <- as.factor(ifelse(t.mean$Site.Name %in% harvard, "Harvard", paste(t.mean$Site.Name)))
summary(t.mean)

t.mean2 <- aggregate(t.mean[,3:ncol(t.mean)], by=t.mean[,c("Site.Name", "Year")], FUN=mean)
# summary(t.mean2)

t.mean2$grow.seas <- rowMeans(t.mean2[,c("May", "Jun", "Jul", "Aug", "Sep")])
t.mean2$pfall <- rowMeans(t.mean2[,c("pSep", "pOct", "pNov")])
t.mean2$winter <- rowMeans(t.mean2[,c("pDec", "Jan", "Feb")])
t.mean2$spring <- rowMeans(t.mean2[,c("Mar", "Apr", "May")])
t.mean2$summer <- rowMeans(t.mean2[,c("Jun", "Jul", "Aug")])


summary(t.mean2)
write.csv(t.mean2, file="processed_data/ch2_tmean.csv", row.names=F)

# Precip
precip$Site.Name <- as.factor(ifelse(precip$Site.Name %in% harvard, "Harvard", paste(precip$Site.Name)))
summary(precip)

precip2 <- aggregate(precip[,3:ncol(precip)], by=precip[,c("Site.Name", "Year")], FUN=mean)
summary(precip2)

precip2$grow.seas <- rowSums(precip2[,c("May", "Jun", "Jul", "Aug", "Sep")])
precip2$pfall <- rowMeans(precip2[,c("pSep", "pOct", "pNov")])
precip2$winter <- rowMeans(precip2[,c("pDec", "Jan", "Feb")])
precip2$spring <- rowMeans(precip2[,c("Mar", "Apr", "May")])
precip2$summer <- rowMeans(precip2[,c("Jun", "Jul", "Aug")])




summary(precip2)
write.csv(precip2, file="processed_data/ch2_precip.csv", row.names=F)
####################################################
# making a file with just the growing season data
####################################################
climate.use <- data.frame(Site = t.mean2$Site.Name,
						  tmean = t.mean2$grow.seas,
						  precip = precip2$grow.seas,
						  Year = t.mean2$Year)
summary(climate.use)

write.csv(climate.use, file="processed_data/climate_growing_season.csv", row.names=F)						  


####################################################
# Combining my data.use from script #1 to this set of climate data
####################################################

data.use <- read.csv("processed_data/tree_data_use.csv", header=T)
summary(data.use)
dim(data.use)
summary(climate.use)
dim(climate.use)

unique(data.use$Site)
unique(climate.use$Site)

data.use2 <- merge(data.use, climate.use, all.x=T, all.y=T)
dim(data.use2)
summary(data.use2)

# taking onlt the trees that dated so that we don't have any gapfilled data in the analysis
data.use2 <- data.use2[data.use2$Dated=="Y" & !is.na(data.use2$Dated),]
summary(data.use2)

write.csv(data.use2, file="processed_data/AllSites_tree_plus_climate.csv", row.names=F)


####################################################
# Making a data frame that has months and seasons broken down so we can determine the critical climate period for each site
####################################################
summary(t.mean2)
summary(precip2)

# Minimum temp
harvard <- c("Harvard Forest (Tower Plot 1)", "Harvard Forest (Tower Plot 2)")

t.min$Site.Name <- as.factor(ifelse(t.min$Site.Name %in% harvard, "Harvard", paste(t.min$Site.Name)))
summary(t.min)


t.min2 <- aggregate(t.min[,3:ncol(t.min)], by=t.min[,c("Site.Name", "Year")], FUN=mean)
# summary(t.min2)

t.min2$grow.seas <- rowMeans(t.min2[,c("May", "Jun", "Jul", "Aug", "Sep")])
t.min2$pfall <- rowMeans(t.min2[,c("pSep", "pOct", "pNov")])
t.min2$winter <- rowMeans(t.min2[,c("pDec", "Jan", "Feb")])
t.min2$spring <- rowMeans(t.min2[,c("Mar", "Apr", "May")])
t.min2$summer <- rowMeans(t.min2[,c("Jun", "Jul", "Aug")])
summary(t.min2)
write.csv(t.min2, file="processed_data/ch2_tmin.csv", row.names=F)

# Maximum temp

harvard <- c("Harvard Forest (Tower Plot 1)", "Harvard Forest (Tower Plot 2)")

t.max$Site.Name <- as.factor(ifelse(t.max$Site.Name %in% harvard, "Harvard", paste(t.max$Site.Name)))
summary(t.max)

t.max2 <- aggregate(t.max[,3:ncol(t.max)], by=t.max[,c("Site.Name", "Year")], FUN=mean)
# summary(t.max2)

t.max2$grow.seas <- rowMeans(t.max2[,c("May", "Jun", "Jul", "Aug", "Sep")])
t.max2$pfall <- rowMeans(t.max2[,c("pSep", "pOct", "pNov")])
t.max2$winter <- rowMeans(t.max2[,c("pDec", "Jan", "Feb")])
t.max2$spring <- rowMeans(t.max2[,c("Mar", "Apr", "May")])
t.max2$summer <- rowMeans(t.max2[,c("Jun", "Jul", "Aug")])
write.csv(t.max2, file="processed_data/ch2_tmax.csv", row.names=F)

