library(mgcv)
library(ggplot2)
library(car)
library(reshape)
library(dplR)
library(DescTools)
# Christy's GAMM code

# gam1 <- gamm(Y ~  s(Biomass, bs="cr", k=3, by=PFT) + s(tair, k=k, by=PFT) + s(precipf, k=k, by=PFT) + s(CO2, k=k, by=PFT), random=list(PlotID=~1), data=data) 

# Loading in my data

data.use <- read.csv("processed_data/AllSites_tree_plus_climate_and_BA.csv", header=T)

data.use$group <- data.use$Species
data.use$group <- recode(data.use$group, "'CAOV' = 'CARYA'; 'CACO' = 'CARYA'; 
  							'CATE' = 'CARYA'; 'ACSAC' = 'ACSA'; 'BEAL' = 'BETULA'; 'BELE' = 'BETULA'; 'QUMU' = 'QUAL'")

data.use$group.plot <- as.factor(paste(data.use$group, data.use$PlotID, sep="."))

data.use$Canopy.Class <- recode(data.use$Canopy.Class, "'C' = 'D'")
summary(data.use)


# reducing the amount of data for the test runs
sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

test <- data.use[data.use$Site %in% sites.use & !is.na(data.use$RW) & !is.na(data.use$BA.inc),]
summary(test)

test$group.cc <- as.factor(paste(test$group, test$Canopy.Class, sep="."))
summary(test)


# Get a list of what predictors & responses I'm using
predictors.all <- c("tmean", "precip", "Species", "dbh.recon", "Canopy.Class", "spp.plot", "group", "group.plot", "group.cc")

# Getting rid of observations that have NAs in the important variables
test <- test[complete.cases(test[,predictors.all]),]
test <- test[test$Live.Dead=="LIVE" & !test$Canopy.Class=="F",]

# Subsetting to a set of species that we have enough data to feel good about
#species.use <- c("TSCA", "QURU", "ACRU", "BEAL", "ACSA", "LITU", "QUAL", "CAOV", "CACO", "CATE", "JUVI", "QUVE", "PCRU", "THOC", "PIST")
group.use <- c("ACRU", "ACSA", "BETULA", "CARYA", "FAGR", "FRAX", "PIST", "QUAL", "QURU", "QUVE", "SAAL", "TSCA", "ULRU")

test <- test[test$group %in% group.use,]

summary(test)

# Right now just looking at the dominant trees.  Not sure how to show this yet
test <- test[test$Canopy.Class=="S",]

par(new=F)
plot(test[test$TreeID=="MMA003", "BA.inc"]~ test[test$TreeID=="MMA003","Year"], type="l")


summary(test)


# Truncatign at 1950; at 1950 the sample depth for the oak openings is only 1
test <- test[test$Year >= 1950 & test$Year <= 2012,]

tree.rw <- test
summary(tree.rw)

summary(test)



save(test, file="processed_data/test_tree_data_1950_2012.Rdata")

sites.rw <- list()

tree.rw$Year <- as.factor(tree.rw$Year)

for(s in unique(tree.rw$Site)){
	sites.rw[[s]] <- recast(tree.rw[tree.rw$Site==s,c("Year", "TreeID", "RW")], Year ~ TreeID)	
}
summary(sites.rw)

save(sites.rw, file="processed_data/sites_rw.Rdata")


summary(sites.rw[[4]])
sites.rw
# Making Indices of all the different sites using a 30 yr cubic smoothing spline
sites.i <- list()

for(s in names(sites.rw)){
	sites.i[[s]] <- detrend(sites.rw[[s]][,2:ncol(sites.rw[[s]])], method="Spline", nyr=30)
}
summary(sites.i[[1]])

# making chronologies for each site

site.chron <- list()

for(s in names(sites.i)){
	
	site.chron[[s]] <- chron(sites.i[[s]][,2:ncol(sites.i[[s]])], prefix = substr(s, 1, 3), prewhiten = T) 
	site.chron[[s]][,"Year"]<- c(1950:2012) 
}

summary(site.chron[[5]])


# Making each site it's own dataframe
harvard.crn <- as.data.frame(site.chron[[1]])

howland.crn <- as.data.frame(site.chron[[2]])

misso.crn <- as.data.frame(site.chron[[3]])

mmf.crn <- as.data.frame(site.chron[[4]])

oakop.crn <- as.data.frame(site.chron[[5]])


# Plotting thigns for presentation
par(cex=2 )
plot(mmf.crn$Morres ~ mmf.crn$Year, type="l", lwd=5, axes=F, cex=6, col="white")
abline(h=1,lty="dashed", col="white")
axis(side =1,tck= 0.05) 
axis(side=2,tck=0.05)

#Plotting out mmfsites
summary(test)

mmf <- test[test$Site %in% "Morgan Monroe State Park",]

summary(mmf)

for(i in unique(mmf$Year)){
  mmf[mmf$Year==i,"Mean"] <- mean(mmf[mmf$Year==i,"RW"])
  mmf[mmf$Year==i,"UB"] <- quantile(mmf[mmf$Year==i,"RW"], 0.975)
  mmf[mmf$Year==i,"LB"] <- quantile(mmf[mmf$Year==i,"RW"], 0.025)
}

summary(mmf)

ggplot(data=mmf)+
  # geom_line(aes(x=Year, y=RW))
  geom_ribbon(aes(x=Year, ymin=LB, ymax=UB), alpha=0.5, fill="white")+
  geom_line(aes(x=Year, y=Mean), size=2, color="white")+
  poster.theme2
  



# Merging the residual chronologies back together
sites.crn <- data.frame(Harvard = harvard.crn[, "Harres"], 
						Howland = howland.crn[,"Howres"], 
						Missouri = misso.crn[,"Misres"],
						Morgan_Monroe = mmf.crn[,"Morres"], 
						Oak_openings = oakop.crn[,"Oakres"]
						)

summary(sites.crn)








# Load in climate data

climate.use <- read.csv("processed_data/climate_growing_season.csv")

summary(climate.use)

climate.use <- climate.use[climate.use$Site %in% sites.use,]
climate.use <- climate.use[climate.use$Year>=1950 & climate.use$Year <=2012,]

climate.site <- list()

for(s in unique(climate.use$Site)){
	climate.site[[s]] <- climate.use[climate.use$Site==s,]
}

summary(climate.site)
save(climate.site, file="processed_data/sites_use_climate.Rdata")


class(climate.site[[1]][,"Year"])

head(climate.site[[1]])

summary(climate.site[[1]])

harv.clim <- climate.site[["Harvard"]][,c("tmean", "precip")]

how.clim <- climate.site[["Howland"]][,c("tmean", "precip")]

miss.clim <- climate.site[["Missouri Ozark"]][,c("tmean", "precip")]

mmf.clim <- climate.site[["Morgan Monroe State Park"]][,c("tmean", "precip")]

oak.clim <- climate.site[["Oak Openings Toledo"]][,c("tmean", "precip")]


dim(harv.clim)
dim(harvard.crn)

# harvard
harv.cor<- cor(harvard.crn, harv.clim[,c("tmean", "precip")], method="pearson")
harvard <- data.frame(cor = c(harv.cor[1:2,"tmean"],  harv.cor[1:2,"precip"]),
					  var = c("tmean", "tmean", "precip", "precip"))
					  
harvard$type <- c("std", "res")
harvard$site <- "Harvard" 


# Howland
how.cor <- cor(howland.crn, how.clim[,c("tmean", "precip")], method="pearson")
how.cor

howland <- data.frame(cor = c(how.cor[1:2,"tmean"],  how.cor[1:2,"precip"]),
					  var = c("tmean", "tmean", "precip", "precip"))

					  
howland$type <- c("std", "res")
howland$site <- "Howland" 



# Missouri
mo.cor <- cor(misso.crn, miss.clim[,c("tmean", "precip")], method="pearson")
mo.cor

missouri <- data.frame(cor = c(mo.cor[1:2,"tmean"],  mo.cor[1:2,"precip"]),
					  var = c("tmean", "tmean", "precip", "precip"))

					  
missouri$type <- c("std", "res")
missouri$site <- "Missouri" 

# Morgan Monroe
mmf.cor <- cor(mmf.crn, mmf.clim[,c("tmean", "precip")], method="pearson")
mmf.cor

morgan <- data.frame(cor = c(mmf.cor[1:2,"tmean"],  mmf.cor[1:2,"precip"]),
					  var = c("tmean", "tmean", "precip", "precip"))

					  
morgan$type <- c("std", "res")
morgan$site <- "Morgan-Monroe" 

# Oak openings
oakop.cor <- cor(oakop.crn, oak.clim[,c("tmean", "precip")], method="pearson")
oakop.cor

oakop <- data.frame(cor = c(oakop.cor[1:2,"tmean"],  oakop.cor[1:2,"precip"]),
					  var = c("tmean", "tmean", "precip", "precip"))

					  
oakop$type <- c("std", "res")
oakop$site <- "Oak-Openings" 

# Merging standard chronologies into one file

clim.cor <- merge(harvard, howland, all.x=T, all.y=T)
clim.cor2 <- merge(clim.cor, missouri,all.x=T, all.y=T)
clim.cor3 <- merge(clim.cor2, morgan,all.x=T, all.y=T)
clim.cor4 <- merge(clim.cor3, oakop,all.x=T, all.y=T)

summary(clim.cor4)
clim.cor4$site <- as.factor(clim.cor4$site)
clim.cor4$type <- as.factor(clim.cor4$type)
clim.cor4$cor <- as.numeric(clim.cor4$cor)

# Sig value for 81 df = 0.209
clim.cor4$sig <- ifelse(clim.cor4$cor < -0.2245 | clim.cor4$cor > 0.2245, "Y", "N")
clim.cor4$sig <- factor(clim.cor4$sig, levels = c("Y", "N"))

clim.cor4$site <- factor(clim.cor4$site, levels = c("Missouri", "Morgan-Monroe", "Oak-Openings", "Harvard", "Howland"))

clim.cor4$Site <- clim.cor4$site
clim.cor4$Site <- recode(clim.cor4$Site, "'Missouri' = 'MO'; 'Morgan-Monroe' = 'IN'; 'Oak-Openings' = 'OH'; 'Harvard' = 'MA'; 'Howland' = 'ME'")
clim.cor4$Site <- factor(clim.cor4$Site, levels = c("MO", "IN", "OH", "MA", "ME"))


pdf("figures/site_correlations.pdf", width= 13, height = 8.5)
ggplot(data=clim.cor4[clim.cor4$type=="std",]) + facet_grid(var~. , scales="free_x") +
	geom_bar(aes(x=Site, y=cor, fill=sig), stat="identity", position="dodge", colour="black") +
	geom_hline(yintercept=0.2245, linetype="dashed") + 
	geom_hline(yintercept=-0.2245, linetype="dashed") + 	
	geom_hline(yintercept=0, linetype="solid") +
	scale_fill_manual(values= c("green", "grey50")) + 
	poster.theme2

	 
dev.off()

######################################################################
# Gettign correlations for all for all of the available climate data data
######################################################################


# Loading in climate data
t.mean <- read.csv("processed_data/ch2_tmean.csv", header=T)
t.min <- read.csv("processed_data/ch2_tmin.csv", header=T)
t.max <- read.csv("processed_data/ch2_tmax.csv", header=T)
precip <- read.csv("processed_data/ch2_precip.csv", header=T)


summary(t.mean)

# Limiting time frame--1950-2012
t.mean <- t.mean[t.mean$Year >= 1951 & t.mean$Year <= 2012,]
t.min <- t.min[t.min$Year >= 1951 & t.min$Year <= 2012,]
t.max <- t.max[t.max$Year >= 1951 & t.max$Year <= 2012,]
precip <- precip[precip$Year >= 1951 & precip$Year <= 2012,]
summary(t.mean)
summary(t.min)
summary(t.max)
summary(precip)

head(t.mean)
unique(t.mean$Site.Name)

t.mean$Site.Name <- recode(t.mean$Site.Name, "'Missouri Ozark' = 'Missouri'; 'Morgan Monroe State Park'='Morgan_Monroe'; 'Oak Openings Toledo' = 'Oak_openings'")

t.min$Site.Name <- recode(t.min$Site.Name, "'Missouri Ozark' = 'Missouri'; 'Morgan Monroe State Park'='Morgan_Monroe'; 'Oak Openings Toledo' = 'Oak_openings'")


t.max$Site.Name <- recode(t.max$Site.Name, "'Missouri Ozark' = 'Missouri'; 'Morgan Monroe State Park'='Morgan_Monroe'; 'Oak Openings Toledo' = 'Oak_openings'")


precip$Site.Name <- recode(precip$Site.Name, "'Missouri Ozark' = 'Missouri'; 'Morgan Monroe State Park'='Morgan_Monroe'; 'Oak Openings Toledo' = 'Oak_openings'")



# getting the ring widths in a data frame
summary(sites.crn)
sites.crn$Year <- site.chron[[1]][,"Year"]
head(sites.crn)
sites.crn <- sites.crn[sites.crn$Year > 1950,]


#--------------------------------------------
# Tmean correlation
#--------------------------------------------

sites <- c("Harvard", "Howland", "Missouri", "Morgan_Monroe", "Oak_openings")

tmean.corr<-list()

for(i in sites){
	tmean.corr[[i]] <- cor(sites.crn[,i], t.mean[t.mean$Site.Name %in% i,!names(t.mean) %in% c("Site.Name", "Year")], method="pearson")
}
summary(tmean.corr)

# Making each site it's own dataframe
harvard.tmean <- as.data.frame(tmean.corr[[1]])

harvard.tmean.stack <- stack(harvard.tmean)
names(harvard.tmean.stack)<- c("cor", "month")
harvard.tmean.stack$Site <- as.factor("Harvard")
harvard.tmean.stack$type <- as.factor("tmean")

howland.tmean <- as.data.frame(tmean.corr[[2]])

howland.tmean.stack <- stack(howland.tmean)
names(howland.tmean.stack)<- c("cor", "month")
howland.tmean.stack$Site <- as.factor("Howland")
howland.tmean.stack$type <- as.factor("tmean")

mo.tmean <- as.data.frame(tmean.corr[[3]])

mo.tmean.stack <- stack(mo.tmean)
names(mo.tmean.stack)<- c("cor", "month")
mo.tmean.stack$Site <- as.factor("Missouri")
mo.tmean.stack$type <- as.factor("tmean")

mmf.tmean <- as.data.frame(tmean.corr[[4]])

mmf.tmean.stack <- stack(mmf.tmean)
names(mmf.tmean.stack)<- c("cor", "month")
mmf.tmean.stack$Site <- as.factor("MMF")
mmf.tmean.stack$type <- as.factor("tmean")

oakop.tmean <- as.data.frame(tmean.corr[[5]])

oakop.tmean.stack <- stack(oakop.tmean)
names(oakop.tmean.stack)<- c("cor", "month")
oakop.tmean.stack$Site <- as.factor("Oak_openings")
oakop.tmean.stack$type <- as.factor("tmean")

all.sites.tmean <- rbind(harvard.tmean.stack, howland.tmean.stack, mo.tmean.stack, mmf.tmean.stack, oakop.tmean.stack)

summary(all.sites.tmean)

#--------------------------------------------
# Tmin correlation
#--------------------------------------------

sites <- c("Harvard", "Howland", "Missouri", "Morgan_Monroe", "Oak_openings")

tmin.corr<-list()

for(i in sites){
	tmin.corr[[i]] <- cor(sites.crn[,i], t.min[t.min$Site.Name %in% i,!names(t.min) %in% c("Site.Name", "Year")], method="pearson")
}
summary(tmin.corr)

# Making each site it's own dataframe
harvard.tmin <- as.data.frame(tmin.corr[[1]])

harvard.tmin.stack <- stack(harvard.tmin)
names(harvard.tmin.stack)<- c("cor", "month")
harvard.tmin.stack$Site <- as.factor("Harvard")
harvard.tmin.stack$type <- as.factor("tmin")

howland.tmin <- as.data.frame(tmin.corr[[2]])

howland.tmin.stack <- stack(howland.tmin)
names(howland.tmin.stack)<- c("cor", "month")
howland.tmin.stack$Site <- as.factor("Howland")
howland.tmin.stack$type <- as.factor("tmin")

mo.tmin <- as.data.frame(tmin.corr[[3]])

mo.tmin.stack <- stack(mo.tmin)
names(mo.tmin.stack)<- c("cor", "month")
mo.tmin.stack$Site <- as.factor("Missouri")
mo.tmin.stack$type <- as.factor("tmin")

mmf.tmin <- as.data.frame(tmin.corr[[4]])

mmf.tmin.stack <- stack(mmf.tmin)
names(mmf.tmin.stack)<- c("cor", "month")
mmf.tmin.stack$Site <- as.factor("MMF")
mmf.tmin.stack$type <- as.factor("tmin")

oakop.tmin <- as.data.frame(tmin.corr[[5]])

oakop.tmin.stack <- stack(oakop.tmin)
names(oakop.tmin.stack)<- c("cor", "month")
oakop.tmin.stack$Site <- as.factor("Oak_openings")
oakop.tmin.stack$type <- as.factor("tmin")

all.sites.tmin <- rbind(harvard.tmin.stack, howland.tmin.stack, mo.tmin.stack, mmf.tmin.stack, oakop.tmin.stack)

summary(all.sites.tmin)



#--------------------------------------------
# Tmax correlation
#--------------------------------------------

sites <- c("Harvard", "Howland", "Missouri", "Morgan_Monroe", "Oak_openings")

tmax.corr<-list()

for(i in sites){
	tmax.corr[[i]] <- cor(sites.crn[,i], t.max[t.max$Site.Name %in% i,!names(t.max) %in% c("Site.Name", "Year")], method="pearson")
}
summary(tmax.corr)

# Making each site it's own dataframe
harvard.tmax <- as.data.frame(tmax.corr[[1]])

harvard.tmax.stack <- stack(harvard.tmax)
names(harvard.tmax.stack)<- c("cor", "month")
harvard.tmax.stack$Site <- as.factor("Harvard")
harvard.tmax.stack$type <- as.factor("tmax")

howland.tmax <- as.data.frame(tmax.corr[[2]])

howland.tmax.stack <- stack(howland.tmax)
names(howland.tmax.stack)<- c("cor", "month")
howland.tmax.stack$Site <- as.factor("Howland")
howland.tmax.stack$type <- as.factor("tmax")

mo.tmax <- as.data.frame(tmax.corr[[3]])

mo.tmax.stack <- stack(mo.tmax)
names(mo.tmax.stack)<- c("cor", "month")
mo.tmax.stack$Site <- as.factor("Missouri")
mo.tmax.stack$type <- as.factor("tmax")

mmf.tmax <- as.data.frame(tmax.corr[[4]])

mmf.tmax.stack <- stack(mmf.tmax)
names(mmf.tmax.stack)<- c("cor", "month")
mmf.tmax.stack$Site <- as.factor("MMF")
mmf.tmax.stack$type <- as.factor("tmax")

oakop.tmax <- as.data.frame(tmax.corr[[5]])

oakop.tmax.stack <- stack(oakop.tmax)
names(oakop.tmax.stack)<- c("cor", "month")
oakop.tmax.stack$Site <- as.factor("Oak_openings")
oakop.tmax.stack$type <- as.factor("tmax")

all.sites.tmax <- rbind(harvard.tmax.stack, howland.tmax.stack, mo.tmax.stack, mmf.tmax.stack, oakop.tmax.stack)

summary(all.sites.tmax)




#--------------------------------------------
# Precip correlation
#--------------------------------------------
sites <- c("Harvard", "Howland", "Missouri", "Morgan_Monroe", "Oak_openings")

precip.corr<-list()

for(i in sites){
	precip.corr[[i]] <- cor(sites.crn[,i], precip[precip$Site.Name %in% i,!names(precip) %in% c("Site.Name", "Year")], method="pearson")
}
summary(precip.corr)

# Making each site it's own dataframe
harvard.precip <- as.data.frame(precip.corr[[1]])

harvard.precip.stack <- stack(harvard.precip)
names(harvard.precip.stack)<- c("cor", "month")
harvard.precip.stack$Site <- as.factor("Harvard")
harvard.precip.stack$type <- as.factor("precip")

howland.precip <- as.data.frame(precip.corr[[2]])

howland.precip.stack <- stack(howland.precip)
names(howland.precip.stack)<- c("cor", "month")
howland.precip.stack$Site <- as.factor("Howland")
howland.precip.stack$type <- as.factor("precip")

mo.precip <- as.data.frame(precip.corr[[3]])

mo.precip.stack <- stack(mo.precip)
names(mo.precip.stack)<- c("cor", "month")
mo.precip.stack$Site <- as.factor("Missouri")
mo.precip.stack$type <- as.factor("precip")

mmf.precip <- as.data.frame(precip.corr[[4]])

mmf.precip.stack <- stack(mmf.precip)
names(mmf.precip.stack)<- c("cor", "month")
mmf.precip.stack$Site <- as.factor("MMF")
mmf.precip.stack$type <- as.factor("precip")

oakop.precip <- as.data.frame(precip.corr[[5]])

oakop.precip.stack <- stack(oakop.precip)
names(oakop.precip.stack)<- c("cor", "month")
oakop.precip.stack$Site <- as.factor("Oak_openings")
oakop.precip.stack$type <- as.factor("precip")

all.sites.precip <- rbind(harvard.precip.stack, howland.precip.stack, mo.precip.stack, mmf.precip.stack, oakop.precip.stack)

summary(all.sites.precip)

# Merging all stacked dataframes together to make plotting easier

all.sites.climate <- rbind(all.sites.precip, all.sites.tmax, all.sites.tmin, all.sites.tmean)
summary(all.sites.climate)

# Sig value for 75 df = 0.2245
all.sites.climate$sig <- ifelse(all.sites.climate$cor < -0.2245 | all.sites.climate$cor > 0.2245, "Y", "N")
all.sites.climate$sig <- factor(all.sites.climate$sig, levels = c("Y", "N"))


all.sites.climate$month <- factor(all.sites.climate$month, levels = c("pJan", "pFeb", "pMar", "pApr", "pMay", "pJun", "pJul", "pAug", "pSep", "pOct", "pNov", "pDec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "pfall", "winter", "spring", "summer", "grow.seas"))

all.sites.climate$Site <- factor(all.sites.climate$Site, levels = c("Missouri", "MMF", "Oak_openings", "Harvard", "Howland"))

pdf("figures/site_correlations_allmonths_short.pdf", width= 13, height = 8.5)
ggplot(data=all.sites.climate[all.sites.climate$month %in% c("pOct", "pNov", "pDec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "spring", "summer", "pfall", "winter", "grow.seas"),]) + facet_grid(Site ~ type ) +
	geom_bar(aes(x=month, y=cor, fill=sig), stat="identity", position="dodge", colour="black") +
	geom_hline(yintercept=0.2245, linetype="dashed") + 
	geom_hline(yintercept=-0.2245, linetype="dashed") + 	
	geom_hline(yintercept=0, linetype="solid") +
	scale_fill_manual(values= c("green", "grey50"))+
	theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

summary(all.sites.climate$month)

all.sites.gs <- all.sites.climate[all.sites.climate$month=="grow.seas",]
summary(all.sites.gs)

cbbPalette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00")

all.sites.climate$type <- factor(all.sites.climate$type, levels=c("tmean", "precip", "tmin", "tmax"))
pdf("figures/site_correlations_growing season.pdf", width= 13, height = 8.5)
ggplot(data=all.sites.climate[all.sites.climate$month %in% "grow.seas" & all.sites.climate$type %in% c("tmean", "precip"),]) + facet_grid(~type) +
  geom_bar(aes(x=month, y=cor, color=Site), stat="identity", position="dodge", fill=NA) + ylim(-0.6,0.6)+
  geom_bar(aes(x=month, y=cor, color=Site), stat="identity", position="dodge", fill=NA) +ylim(-0.6,0.6)+
  geom_bar(aes(x=month, y=cor, fill=Site, alpha=sig), stat="identity", position="dodge") +ylim(-0.6,0.6)+
  geom_hline(yintercept=0.2245, linetype="dashed") + 
  geom_hline(yintercept=-0.2245, linetype="dashed") + 	
  geom_hline(yintercept=0, linetype="solid") +
 # scale_fill_manual(values= cbbPalette)+
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_alpha_manual(values = c(1, 0.2))+
  poster.theme2 +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank()) +
  labs(title= "TR Site Climate Correlations", x="Seasons", y=expression(bold(paste("Correlation Value (r)"))))

dev.off()

################################################################################
# Running correlations on the individual indices to get the full spread of the 
################################################################################

all.sites.gs <- all.sites.climate[all.sites.climate$month=="grow.seas",]
summary(all.sites.gs)

all.sites.gs$State <- recode(all.sites.gs$Site, "'Missouri'='MO';'MMF'='IN';'Oak_openings'='OH';'Harvard'='MA';'Howland'='ME'")
all.sites.gs$series <- as.factor("chron")
summary(all.sites.gs)

summary(climate.site$Howland[,])
head(sites.i$Howland)

summary(sites.i)
index.cor.temp <- sites.i
index.cor.precip <- sites.i

test <- sites.i
meow <- test

# Running correlations fro Tmean and Precip on individual indices
for(s in names(sites.i)){
	for(i in names(sites.i[[s]])){
		index.cor.temp[[s]][,i] <- cor(sites.i[[s]][,i], climate.site[[s]][,"tmean"], method="pearson")
	}
}	

test <- cor(sites.i[["Harvard"]][,c(1:10)], climate.site[["Harvard"]][,"tmean"], method="pearson")



for(s in names(sites.i)){
	for(i in names(sites.i[[s]])){
		index.cor.precip[[s]][,i] <- cor(sites.i[[s]][,i], climate.site[[s]][,"precip"], method="pearson") 
	}
}	
summary(index.cor.precip)

# Now need to merge everything into one stacked file to be used with ggplot 2

test <- data.frame()

# Need to break it down by site

# Harvard
har.precip.stack <- stack(index.cor.precip[["Harvard"]])
names(har.precip.stack)<- c("cor", "TreeID")
har.precip.stack$State <- as.factor("MA")
har.precip.stack$type <- as.factor("precip")
summary(har.precip.stack)

har.temp.stack <- stack(index.cor.temp[["Harvard"]])
names(har.temp.stack)<- c("cor", "TreeID")
har.temp.stack$State <- as.factor("MA")
har.temp.stack$type <- as.factor("tmean")
summary(har.temp.stack)

har.stack <- merge(har.temp.stack, har.precip.stack, all.x=T, all.y=T)
summary(har.stack)

# Howland

how.precip.stack <- stack(index.cor.precip[["Howland"]])
names(how.precip.stack)<- c("cor", "TreeID")
how.precip.stack$State <- as.factor("ME")
how.precip.stack$type <- as.factor("precip")
summary(how.precip.stack)

how.temp.stack <- stack(index.cor.temp[["Howland"]])
names(how.temp.stack)<- c("cor", "TreeID")
how.temp.stack$State <- as.factor("ME")
how.temp.stack$type <- as.factor("tmean")
summary(how.temp.stack)

how.stack <- merge(how.temp.stack, how.precip.stack, all.x=T, all.y=T)
summary(how.stack)

# Missouri

mo.precip.stack <- stack(index.cor.precip[["Missouri Ozark"]])
names(mo.precip.stack)<- c("cor", "TreeID")
mo.precip.stack$State <- as.factor("MO")
mo.precip.stack$type <- as.factor("precip")
summary(mo.precip.stack)

mo.temp.stack <- stack(index.cor.temp[["Missouri Ozark"]])
names(mo.temp.stack)<- c("cor", "TreeID")
mo.temp.stack$State <- as.factor("MO")
mo.temp.stack$type <- as.factor("tmean")
summary(mo.temp.stack)

mo.stack <- merge(mo.temp.stack, mo.precip.stack, all.x=T, all.y=T)
summary(mo.stack)




# MMF

mmf.precip.stack <- stack(index.cor.precip[["Morgan Monroe State Park"]])
names(mmf.precip.stack)<- c("cor", "TreeID")
mmf.precip.stack$State <- as.factor("IN")
mmf.precip.stack$type <- as.factor("precip")
summary(mmf.precip.stack)

mmf.temp.stack <- stack(index.cor.temp[["Morgan Monroe State Park"]])
names(mmf.temp.stack)<- c("cor", "TreeID")
mmf.temp.stack$State <- as.factor("IN")
mmf.temp.stack$type <- as.factor("tmean")
summary(mmf.temp.stack)

mmf.stack <- merge(mmf.temp.stack, mmf.precip.stack, all.x=T, all.y=T)
summary(mmf.stack)



# Oak Openings

oak.precip.stack <- stack(index.cor.precip[["Oak Openings Toledo"]])
names(oak.precip.stack)<- c("cor", "TreeID")
oak.precip.stack$State <- as.factor("OH")
oak.precip.stack$type <- as.factor("precip")
summary(oak.precip.stack)

oak.temp.stack <- stack(index.cor.temp[["Oak Openings Toledo"]])
names(oak.temp.stack)<- c("cor", "TreeID")
oak.temp.stack$State <- as.factor("OH")
oak.temp.stack$type <- as.factor("tmean")
summary(oak.temp.stack)

oak.stack <- merge(oak.temp.stack, oak.precip.stack, all.x=T, all.y=T)
summary(oak.stack)

cor.all.stack <- rbind(har.stack, how.stack, mmf.stack, oak.stack, mo.stack)
summary(cor.all.stack)

# Calculating Tukey Biweight Robust mean

for(s in unique(cor.all.stack$State)){
	for(t in unique(cor.all.stack$type)){
	cor.all.stack[cor.all.stack$State==s & cor.all.stack$type==t,"biweight.mean"] <- TukeyBiweight(cor.all.stack[cor.all.stack$State==s & cor.all.stack$type==t,"cor"], na.rm=T)
	}
}
summary(cor.all.stack)

# creating a significance column
cor.all.stack$sig <- ifelse(cor.all.stack$cor < -0.2245 | cor.all.stack$cor > 0.2245, "Y", "N")
cor.all.stack$sig <- factor(cor.all.stack$sig, levels = c("Y", "N"))

cor.all.stack$State <- factor(cor.all.stack$State, levels=c("MO", "IN", "OH", "MA", "ME"))
summary(cor.all.stack)

cor.all.stack$type <- factor(cor.all.stack$type, levels = c("precip", "tmean"))
cor.all.stack$series <- as.factor("index")

summary(all.sites.gs)
summary(cor.all.stack)

summary(cor.all.stack[is.na(cor.all.stack$cor),])

pdf("figures/violin_climate_cor.pdf", height=8, width=13)
ggplot(data=cor.all.stack) + facet_grid(~type)+

	geom_violin(aes(x=State, y=cor, fill=State), adjust = 2, trim=T) + ylim(-0.6,0.6)+
	
	geom_point(data=all.sites.gs[all.sites.gs$type %in% c("precip", "tmean"),],aes(x=State, y=cor), shape=126, size=25)+ ylim(-0.6,0.6)+
	
	geom_point(aes(x=State, y=biweight.mean), shape=95, size=25)+ ylim(-0.6,0.6)+
			
	geom_hline(yintercept=0.2245, linetype="dashed") + 
  	geom_hline(yintercept=-0.2245, linetype="dashed") + 	
  	geom_hline(yintercept=0, linetype="solid") +

	scale_color_manual(values=cbbPalette) +
  	scale_fill_manual(values=cbbPalette) +
  	poster.theme2 +
  	theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major=element_blank(), panel.grid.minor= 	element_blank(), panel.border= element_blank(), panel.background= element_blank()) +
  	labs(title= "TR Index Climate Correlations", x="State", y=expression(bold(paste("Correlation Value (r)"))))
dev.off()


summary(cor.all.stack)
cor.all.stack$Canopy.Class <- as.factor("S")

sup.cor.stack <- cor.all.stack
save(sup.cor.stack, file="processed_data/Suppressed_climate_corrs.Rdata")

summary(all.sites.gs)
all.sites.gs$Canopy.Class <- as.factor("S")

sup.sites.gs <- all.sites.gs
save(sup.sites.gs, file="processed_data/Suppressed_gs_chron_corrs.Rdata")