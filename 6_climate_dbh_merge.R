library(car)
library(dplR)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}


path.met <- "met_data/"

# Loading in climate data from the PRISM extracts
precip  <- read.csv(file.path(path.met, "prism_met_NE_sites_wide_ppt.csv"), header=T)
t.mean  <- read.csv(file.path(path.met, "prism_met_NE_sites_wide_tmean.csv" ), header=T)
t.min   <- read.csv(file.path(path.met, "prism_met_NE_sites_wide_tmin.csv"  ), header=T)
t.max   <- read.csv(file.path(path.met, "prism_met_NE_sites_wide_tmax.csv"  ), header=T)
vpd.min <- read.csv(file.path(path.met, "prism_met_NE_sites_wide_vpdmin.csv"), header=T)
vpd.max <- read.csv(file.path(path.met, "prism_met_NE_sites_wide_vpdmax.csv"), header=T)



# -----------------------------
# CR Hasn't edited past here! (3/2018)
# -----------------------------

t.max$Site.Code <- recode(t.max$Site.Name, "'gillbrook'='GB';'goose_egg'='GE';'lyford'='LF';'north_round_pond'='NR';'pisgah'='PS';'rooster_hill'='RH'")


summary(t.mean)

t.mean2 <- aggregate(t.mean[,!names(t.mean) %in% c("year", "Site.Code")], by=t.mean[,c("Site.Code", "year")], FUN=mean)
# summary(t.mean2)

t.mean2$grow.seas <- rowMeans(t.mean2[,c("May", "Jun", "Jul", "Aug", "Sep")])
t.mean2$pfall <- rowMeans(t.mean2[,c("pSep", "pOct", "pNov")])
t.mean2$winter <- rowMeans(t.mean2[,c("pDec", "Jan", "Feb")])
t.mean2$spring <- rowMeans(t.mean2[,c("Mar", "Apr", "May")])
t.mean2$summer <- rowMeans(t.mean2[,c("Jun", "Jul", "Aug")])


summary(t.mean2)
write.csv(t.mean2, file="processed_data/ch2_tmean.csv", row.names=F)

# Precip
#precip$Site.Name <- as.factor(ifelse(precip$Site.Name %in% harvard, "Harvard", paste(precip$Site.Name)))
summary(precip)

precip2 <- aggregate(precip[,!names(precip) %in% c("year", "Site.Code")], by=precip[,c("Site.Code", "year")], FUN=mean)
summary(precip2)

precip2$grow.seas <- rowSums(precip2[,c("May", "Jun", "Jul", "Aug", "Sep")])
precip2$pfall <- rowMeans(precip2[,c("pSep", "pOct", "pNov")])
precip2$winter <- rowMeans(precip2[,c("pDec", "Jan", "Feb")])
precip2$spring <- rowMeans(precip2[,c("Mar", "Apr", "May")])
precip2$summer <- rowMeans(precip2[,c("Jun", "Jul", "Aug")])




summary(precip2)
write.csv(precip2, file="processed_data/ch2_precip.csv", row.names=F)

# VPDmax
#vpd.max$Site.Name <- as.factor(ifelse(vpd.max$Site.Name %in% harvard, "Harvard", paste(vpd.max$Site.Name)))
summary(vpd.max)

vpd.max2 <- aggregate(vpd.max[,!names(vpd.max) %in% c("year", "Site.Code")], by=vpd.max[,c("Site.Code", "year")], FUN=mean)
summary(vpd.max2)

vpd.max2$grow.seas <- rowSums(vpd.max2[,c("May", "Jun", "Jul", "Aug", "Sep")])
vpd.max2$pfall <- rowMeans(vpd.max2[,c("pSep", "pOct", "pNov")])
vpd.max2$winter <- rowMeans(vpd.max2[,c("pDec", "Jan", "Feb")])
vpd.max2$spring <- rowMeans(vpd.max2[,c("Mar", "Apr", "May")])
vpd.max2$summer <- rowMeans(vpd.max2[,c("Jun", "Jul", "Aug")])

# VPDmin
#vpd.min$Site.Name <- as.factor(ifelse(vpd.min$Site.Name %in% harvard, "Harvard", paste(vpd.min$Site.Name)))
summary(vpd.min)

vpd.min2 <- aggregate(vpd.min[,!names(vpd.min) %in% c("year", "Site.Code")], by=vpd.min[,c("Site.Code", "year")], FUN=mean)
summary(vpd.min2)

vpd.min2$grow.seas <- rowSums(vpd.min2[,c("May", "Jun", "Jul", "Aug", "Sep")])
vpd.min2$pfall <- rowMeans(vpd.min2[,c("pSep", "pOct", "pNov")])
vpd.min2$winter <- rowMeans(vpd.min2[,c("pDec", "Jan", "Feb")])
vpd.min2$spring <- rowMeans(vpd.min2[,c("Mar", "Apr", "May")])
vpd.min2$summer <- rowMeans(vpd.min2[,c("Jun", "Jul", "Aug")])
####################################################
# making a file with just the growing season data
####################################################
climate.use <- data.frame(Site.Code = t.mean2$Site.Code,
                          tmean = t.mean2$grow.seas,
                          precip = precip2$grow.seas,
                          vpd.min = vpd.min2$grow.seas,
                          vpd.max = vpd.max2$grow.seas,
                          Year = t.mean2$year)
summary(climate.use)

write.csv(climate.use, file="processed_data/climate_growing_season.csv", row.names=F)						  


####################################################
# Combining my data.use from script #1 to this set of climate data
####################################################

data.use <- read.csv("processed_data/tree_data_use.csv", header=T)
summary(data.use)
dim(data.use)

data.use$Live.Dead <- recode(data.use$Live.Dead, "'Li'='LIVE'")
summary(data.use)
data.use2 <- data.use[,c("Year", "Site.Code", "TreeID", "RW", "Species", "Canopy.Class", "Live.Dead", "DBH", "dbh.recon", "Plot")]
names(data.use2) <- c("Year", "Site.Code", "TreeID", "RW", "Species", "Canopy.Class", "Live.Dead", "DBH", "dbh.recon", "PlotID")


# Loading in Harvard and Howland sites from PHDwork
phd.data <- read.csv("processed_data/AllSites_tree_plus_climate_and_BA.csv", header=T)
summary(phd.data)

ne.phd.data <- phd.data[phd.data$Site %in% c("Howland", "Harvard"),]
ne.phd.data$DBH <- ne.phd.data$DBH..cm.
ne.phd.data$Canopy.Class <- recode(ne.phd.data$Canopy.Class, "'S' = 'U'")
ne.phd.data$Site.Code <- recode(ne.phd.data$Site, "'Howland'='HO';'Harvard'='HF'")

ne.phd.data2 <- ne.phd.data[,c("Year", "Site.Code", "TreeID", "RW", "Species", "Canopy.Class", "Live.Dead", "DBH", "dbh.recon", "PlotID")]
# ne.phd.data2 <- ne.phd.data[,names(ne.phd.data) %in% names(data.use)]

names(ne.phd.data2)
names(data.use2)

data.use <- rbind(data.use2, ne.phd.data2)
summary(data.use)

# Limiting ring-width data to the period that we have overlap in climate data (1895-2015)
data.use <- data.use[data.use$Year >=1895 & data.use$Year <=2015,]
summary(climate.use)
dim(climate.use)

#climate.use$Site.Code <- recode(climate.use$Site, "'gillbrook'='GB';'goose_egg'='GE';'lyford'='LF';'north_round_pond'='NR';'pisgah'='PS';'rooster_hill'='RH'")
# climate.use <- climate.use[,!names(climate.use) %in% "Site"]

unique(data.use$Site.Code)
unique(climate.use$Site.Code)

data.use2 <- merge(data.use, climate.use, all.x=T, all.y=F)
dim(data.use2)
summary(data.use2)


write.csv(data.use2, file="processed_data/NESites_tree_plus_climate.csv", row.names=F)


