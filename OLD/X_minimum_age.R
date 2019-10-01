# Needing the mean age of understory trees for each species
# Loaded the data of the 6_climate_dbh_merge.R script
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


# Minimum age calculations

tsca.u <- unique(data.use[data.use$Species=="TSCA" & data.use$Canopy.Class=="U" & data.use$Live.Dead=="LIVE","TreeID"])
acru.u <- unique(data.use[data.use$Species=="ACRU" & data.use$Canopy.Class=="U" & data.use$Live.Dead=="LIVE","TreeID"])
fagr.u <- unique(data.use[data.use$Species=="FAGR" & data.use$Canopy.Class=="U" & data.use$Live.Dead=="LIVE","TreeID"])
quru.u <- unique(data.use[data.use$Species=="QURU" & data.use$Canopy.Class=="U" & data.use$Live.Dead=="LIVE","TreeID"])


summary(data.use)

tsca <- as.data.frame(tsca.u[!is.na(tsca.u)])
names(tsca) <- "TreeID"

for(i in tsca.u[!is.na(tsca.u)]){
tsca[tsca$TreeID==i, "Min.Year"] <- min(data.use[data.use$TreeID==i & !is.na(data.use$RW), c("Year")])
tsca[tsca$TreeID==i, "Max.Year"] <- max(data.use[data.use$TreeID==i & !is.na(data.use$RW), c("Year")])
tsca[tsca$TreeID==i, "age"] <- tsca[tsca$TreeID==i, "Max.Year"] - tsca[tsca$TreeID==i, "Min.Year"] +1
}

tsca$spp <- as.factor("TSCA")


fagr <- as.data.frame(fagr.u[!is.na(fagr.u)])
names(fagr) <- "TreeID"

for(i in fagr.u[!is.na(fagr.u)]){
  fagr[fagr$TreeID==i, "Min.Year"] <- min(data.use[data.use$TreeID==i & !is.na(data.use$RW), c("Year")])
  fagr[fagr$TreeID==i, "Max.Year"] <- max(data.use[data.use$TreeID==i & !is.na(data.use$RW), c("Year")])
  fagr[fagr$TreeID==i, "age"] <- fagr[fagr$TreeID==i, "Max.Year"] - fagr[fagr$TreeID==i, "Min.Year"] +1
  
  
}

fagr$spp <- as.factor("FAGR")


quru <- as.data.frame(quru.u[!is.na(quru.u)])
names(quru) <- "TreeID"

for(i in quru.u[!is.na(quru.u)]){
  quru[quru$TreeID==i, "Min.Year"] <- min(data.use[data.use$TreeID==i & !is.na(data.use$RW), c("Year")])
  quru[quru$TreeID==i, "Max.Year"] <- max(data.use[data.use$TreeID==i & !is.na(data.use$RW), c("Year")])
  quru[quru$TreeID==i, "age"] <- quru[quru$TreeID==i, "Max.Year"] - quru[quru$TreeID==i, "Min.Year"] +1
  

}

quru$spp <- as.factor("QURU")


acru <- as.data.frame(acru.u[!is.na(acru.u)])
names(acru) <- "TreeID"

for(i in acru.u[!is.na(acru.u)]){
  acru[acru$TreeID==i, "Min.Year"] <- min(data.use[data.use$TreeID==i & !is.na(data.use$RW), c("Year")])
  acru[acru$TreeID==i, "Max.Year"] <- max(data.use[data.use$TreeID==i & !is.na(data.use$RW), c("Year")])
  acru[acru$TreeID==i, "age"] <- acru[acru$TreeID==i, "Max.Year"] - acru[acru$TreeID==i, "Min.Year"] +1
  
}

acru$spp <- as.factor("ACRU")


under.min <- rbind(tsca, quru, fagr, acru)
summary(under.min)

ggplot(data=under.min) + facet_grid(spp~.) +
  geom_density(aes(x=age, col=spp, fill=spp), alpha=0.5)

ggplot(data=under.min) + #facet_grid(spp~.) +
  geom_boxplot(aes(x=spp, y=age))


test <- aov(under.min$age~under.min$spp)
anova(test)

meow <- anova(test)
TukeyHSD(test)


cc.pub.trees <- data.use[data.use$Live.Dead=="LIVE" & data.use$Species%in% c("ACRU", "TSCA", "QURU", "FAGR") ,]
cc.pub.trees <- cc.pub.trees[!is.na(cc.pub.trees$Year),]
summary(cc.pub.trees)

ggplot(data=cc.pub.trees) +
  facet_grid(Species~.) +
  geom_density(aes(x=DBH, col=Species, fill=Species), alpha=0.5)

ggplot(data=cc.pub.trees) +
  facet_grid(Species~.) +
  geom_boxplot(aes(x=Canopy.Class, y=DBH))

# one weird oak
summary(data.use[data.use$Canopy.Class=="U" & data.use$DBH > 55,])
