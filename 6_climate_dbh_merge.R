library(car)
library(dplR)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}


path.met <- "/Volumes/GoogleDrive/My Drive/canopy_and_climate/met/"

# Loading in climate data from the PRISM extracts
precip  <- read.csv(file.path(path.met, "prism_met_NE_sites_wide_ppt.csv".  ), header=T)
t.mean  <- read.csv(file.path(path.met, "prism_met_NE_sites_wide_tmean.csv" ), header=T)
t.min   <- read.csv(file.path(path.met, "prism_met_NE_sites_wide_tmin.csv"  ), header=T)
t.max   <- read.csv(file.path(path.met, "prism_met_NE_sites_wide_tmax.csv"  ), header=T)
vpd.min <- read.csv(file.path(path.met, "prism_met_NE_sites_wide_vpdmin.csv"), header=T)
vpd.max <- read.csv(file.path(path.met, "prism_met_NE_sites_wide_vpdmax.csv"), header=T)



# -----------------------------
# CR Hasn't edited past here!)
# -----------------------------

t.max$Site.Code <- recode(t.max$Site.Name, "'gillbrook'='GB';'goose_egg'='GE';'lyford'='LF';'north_round_pond'='NR';'pisgah'='PS';'rooster_hill'='RH'")


summary(t.mean)

t.mean2 <- aggregate(t.mean[,!names(t.mean) %in% c("Site.Name", "Year", "Site.Code")], by=t.mean[,c("Site.Name", "Year")], FUN=mean)
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

precip2 <- aggregate(precip[,!names(precip) %in% c("Site.Name", "Year", "Site.Code")], by=precip[,c("Site.Name", "Year")], FUN=mean)
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

# Limiting ring-width data to the period that we have overlap in climate data (1895-2015)
data.use <- data.use[data.use$Year >=1895 & data.use$Year <=2015,]
summary(climate.use)
dim(climate.use)

climate.use$Site.Code <- recode(climate.use$Site, "'gillbrook'='GB';'goose_egg'='GE';'lyford'='LF';'north_round_pond'='NR';'pisgah'='PS';'rooster_hill'='RH'")
climate.use <- climate.use[,!names(climate.use) %in% "Site"]

unique(data.use$Site.Code)
unique(climate.use$Site.Code)

data.use2 <- merge(data.use, climate.use, all.x=T, all.y=T)
dim(data.use2)
summary(data.use2)


write.csv(data.use2, file="processed_data/NESites_tree_plus_climate.csv", row.names=F)


