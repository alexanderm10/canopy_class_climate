require(ggplot2)
require(mgcv)
require(lsmeans)
require(car)


load("processed_data/climate_markeryears.Rdata") # Loading in Climate Marker Years
load("ch2_combined_data_use.Rdata") # Loading in Raw data used in GAMs
summary(climate.markers)
load(file="processed_data/gamm_weights/gam1_weights.Rdata") # Loading in Modeled weights from GAM1
load(file="processed_data/gamm_weights/gam2_weights.Rdata") # Loading in Modeled weights from GAM2
load(file="processed_data/gamm_weights/gam4_weights.Rdata") # Loading in Modeled weights from GAM4

# Recoding Site as State for simplicity later
gam1.weights$State <- recode(gam1.weights$Site, "'Missouri Ozark'='MO'; 'Morgan Monroe State Park'='IN'; 'Oak Openings Toledo'='OH'; 'Harvard'= 'MA'; 'Howland'='ME'")

gam2.weights$State <- recode(gam2.weights$Site, "'Missouri Ozark'='MO'; 'Morgan Monroe State Park'='IN'; 'Oak Openings Toledo'='OH'; 'Harvard'= 'MA'; 'Howland'='ME'")

gam4.weights$State <- recode(gam4.weights$Site, "'Missouri Ozark'='MO'; 'Morgan Monroe State Park'='IN'; 'Oak Openings Toledo'='OH'; 'Harvard'= 'MA'; 'Howland'='ME'")


# Looking at the non-Size & Time BAI
summary(gam1.weights)
#gam1.weights$BA.inc.Clim <- gam1.weights$BA.inc - exp(gam1.weights$fit.dbh.recon) - exp(gam1.weights$fit.Year)
# # After you re-run script 3.1, change to the following
gam1.weights$BA.inc.Clim <- gam1.weights$BA.inc / (exp(gam1.weights$fit.dbh.recon) * exp(gam1.weights$fit.Year) * exp(gam1.weights$fit.intercept))
summary(gam1.weights)


# Species Level Model--GAM1

# Simple mean-centering on what's left after removing size & year trends
# Note: this didn't work because the trend in the data starts post-1950 for many sites
for(s in unique(gam1.weights$Site)){
	for(c in unique(gam1.weights[gam1.weights$Site==s, "group"])){
		x.inc <- mean(gam1.weights[gam1.weights$Site==s & gam1.weights$group==c, "BA.inc.Clim"], na.rm=T)
		gam1.weights[gam1.weights$Site==s & gam1.weights$group==c, "Clim.Rel"] <- gam1.weights[gam1.weights$Site==s & gam1.weights$group==c, "BA.inc.Clim"]/x.inc-1
	}
}

# gam1.weights$Clim.Rel <- gam1.weights

gam1.dat.summary <- aggregate(gam1.weights[,c("BA.inc", "BA.inc.Clim", "Clim.Rel")],
                         by=gam1.weights[,c("Year", "Site", "group")],
                         FUN=mean)
gam1.dat.summary$Site <- factor(gam1.dat.summary$Site, levels=c("Missouri Ozark", "Morgan Monroe State Park", "Oak Openings Toledo", "Harvard", "Howland"))






gam1.weights$Temp.Mark <- NA
gam1.weights$Precip.Mark <- NA
for(s in unique(gam1.weights$Site)){
	gam1.weights[gam1.weights$Site==s,"Temp.Mark"] <- ifelse(gam1.weights[gam1.weights$Site==s, "Year"] %in% unique(climate.markers[climate.markers$Site==s & climate.markers$marker=="hot", "marker.year"]), "hot", ifelse(gam1.weights[gam1.weights$Site==s, "Year"] %in% unique(climate.markers[climate.markers$Site==s & climate.markers$marker=="cold", "marker.year"]), "cold", "NONE"))
	gam1.weights[gam1.weights$Site==s,"Precip.Mark"] <- ifelse(gam1.weights[gam1.weights$Site==s, "Year"] %in% unique(climate.markers[climate.markers$Site==s & climate.markers$marker=="dry", "marker.year"]), "dry", ifelse(gam1.weights[gam1.weights$Site==s, "Year"] %in% unique(climate.markers[climate.markers$Site==s & climate.markers$marker=="wet", "marker.year"]), "wet","NONE"))
}
gam1.weights$Temp.Mark <- as.factor(gam1.weights$Temp.Mark)
gam1.weights$Precip.Mark <- as.factor(gam1.weights$Precip.Mark)


cols.use <- c("BA.inc", "BA.inc.Clim", "Clim.Rel")
gam1.dat.summary <- aggregate(gam1.weights[,cols.use],
                         by=gam1.weights[,c("Year", "Site", "group", "Temp.Mark", "Precip.Mark")],
                         FUN=mean)

gam1.dat.summary[,paste0(cols.use, ".LB")] <- aggregate(gam1.weights[,cols.use],
                         by=gam1.weights[,c("Year", "Site", "group", "Temp.Mark", "Precip.Mark")],
                         FUN=quantile, 0.025, na.rm=T)[,cols.use]
gam1.dat.summary[,paste0(cols.use, ".UB")] <- aggregate(gam1.weights[,cols.use],
                         by=gam1.weights[,c("Year", "Site", "group", "Temp.Mark", "Precip.Mark")],
                         FUN=quantile, 0.975, na.rm=T)[,cols.use]
summary(gam1.dat.summary)



gam1.dat.summary$Site <- factor(gam1.dat.summary$Site, levels=c("Missouri Ozark", "Morgan Monroe State Park", "Oak Openings Toledo", "Harvard", "Howland"))



# Fixed Scales
save(gam1.dat.summary, file="processed_data/gam1_relative_growth.Rdata")
# Temp
pdf("figures/Prelim_Figures/gam1_rel_growth_hot_dry.pdf", width= 13, height = 8.5)
ggplot(gam1.dat.summary) +
	facet_grid(Site~group, scales="free") +
	
	geom_ribbon(aes(x=Year, ymin=Clim.Rel.LB, ymax=Clim.Rel.UB, fill=group), alpha=0.35) +
	geom_line(aes(x=Year, y=Clim.Rel,color=group),size=0.75) +
		geom_hline(yintercept=0, color="gray50", linetype="dashed") +
	
	geom_vline(data=gam1.dat.summary[gam1.dat.summary$Temp.Mark=="hot",], aes(xintercept=Year), alpha=0.3, color="red") +
	geom_vline(data=gam1.dat.summary[gam1.dat.summary$Precip.Mark=="dry",], aes(xintercept=Year), alpha=0.3, color="brown", linetype="dashed") +

	
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())+
	theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))
dev.off()

pdf("figures/Prelim_Figures/gam1_rel_growth_cold_wet.pdf", width= 13, height = 8.5)
ggplot(gam1.dat.summary) +
	facet_grid(Site~group, scales="free") +
	
	geom_ribbon(aes(x=Year, ymin=Clim.Rel.LB, ymax=Clim.Rel.UB, fill=group), alpha=0.35) +
	geom_line(aes(x=Year, y=Clim.Rel,color=group),size=0.75) +
		geom_hline(yintercept=0, color="gray50", linetype="dashed") +
	
	geom_vline(data=gam1.dat.summary[gam1.dat.summary$Temp.Mark=="cold",], aes(xintercept=Year), alpha=0.3, color="blue") +
	geom_vline(data=gam1.dat.summary[gam1.dat.summary$Precip.Mark=="wet",], aes(xintercept=Year), alpha=0.3, color="darkgreen", linetype="dashed") +

	
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())+
	theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        scale_x_continuous(expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))
dev.off()


#########################################################
# Canopy Class Model--GAM2
#########################################################
# Out Model: log(BAI) ~ A + B + C
# BAI ~ exp(A + B + C)
# BAI ~ exp(A) * exp(B) * exp(C)
# BAI/exp(B) = exp(A + C)

# Looking at the non-Size & Time BAI
summary(gam2.weights)
#gam2.weights$BA.inc.Clim <- gam2.weights$BA.inc - exp(gam2.weights$fit.dbh.recon) - exp(gam2.weights$fit.Year)
# # After you re-run script 3.2, change to the following
gam2.weights$BA.inc.Clim <- gam2.weights$BA.inc / (exp(gam2.weights$fit.dbh.recon) * exp(gam2.weights$fit.Year) * exp(gam2.weights$fit.intercept))
# gam2.weights$BA.inc.Clim.log <- log(gam2.weights$BA.inc) - gam2.weights$fit.dbh.recon - gam2.weights$fit.Year - gam2.weights$fit.intercept
# gam2.weights$BA.inc.Clim2 <- exp(gam2.weights$BA.inc.Clim.log)
# gam2.weights$BA.inc.Clim3 <- exp(log(gam2.weights$BA.inc) - gam2.weights$fit.dbh.recon - gam2.weights$fit.Year - gam2.weights$fit.intercept)
summary(gam2.weights)



# Simple mean-centering on what's left after removing size & year trends
# Note: this didn't work because the trend in the data starts post-1950 for many sites
for(s in unique(gam2.weights$Site)){
	for(c in unique(gam2.weights[gam2.weights$Site==s, "Canopy.Class"])){
		x.inc <- mean(gam2.weights[gam2.weights$Site==s & gam2.weights$Canopy.Class==c, "BA.inc.Clim"], na.rm=T)
		x.inc.log <- mean(gam2.weights[gam2.weights$Site==s & gam2.weights$Canopy.Class==c, "BA.inc.Clim.log"], na.rm=T)
		gam2.weights[gam2.weights$Site==s & gam2.weights$Canopy.Class==c, "Clim.Rel"] <- gam2.weights[gam2.weights$Site==s & gam2.weights$Canopy.Class==c, "BA.inc.Clim"]/x.inc-1
		gam2.weights[gam2.weights$Site==s & gam2.weights$Canopy.Class==c, "Clim.Rel.log"] <- gam2.weights[gam2.weights$Site==s & gam2.weights$Canopy.Class==c, "BA.inc.Clim"]/x.inc.log-1
	}
}

# gam2.weights$Clim.Rel <- gam2.weights

gam2.dat.summary <- aggregate(gam2.weights[,c("BA.inc", "BA.inc.Clim", "BA.inc.Clim.log", "Clim.Rel", "Clim.Rel.log")],
                         by=gam2.weights[,c("Year", "Site", "Canopy.Class")],
                         FUN=mean)

ggplot(gam2.dat.summary[,]) +
	facet_grid(Site~Canopy.Class) +
	geom_line(aes(x=Year, y=BA.inc), color="black") 

ggplot(gam2.dat.summary[,]) +
	facet_grid(Site~Canopy.Class) +
	geom_line(aes(x=Year, y=Clim.Rel), color="black") +
	geom_hline(yintercept=0, color="red", linetype="dashed")

ggplot(gam2.dat.summary[,]) +
  facet_grid(Site~Canopy.Class) +
  geom_line(aes(x=Year, y=Clim.Rel.log), color="black") +
  geom_hline(yintercept=0, color="red", linetype="dashed")


gam2.weights$Temp.Mark <- NA
gam2.weights$Precip.Mark <- NA
for(s in unique(gam2.weights$Site)){
	gam2.weights[gam2.weights$Site==s,"Temp.Mark"] <- ifelse(gam2.weights[gam2.weights$Site==s, "Year"] %in% unique(climate.markers[climate.markers$Site==s & climate.markers$marker=="hot", "marker.year"]), "hot", ifelse(gam2.weights[gam2.weights$Site==s, "Year"] %in% unique(climate.markers[climate.markers$Site==s & climate.markers$marker=="cold", "marker.year"]), "cold", "NONE"))
	gam2.weights[gam2.weights$Site==s,"Precip.Mark"] <- ifelse(gam2.weights[gam2.weights$Site==s, "Year"] %in% unique(climate.markers[climate.markers$Site==s & climate.markers$marker=="dry", "marker.year"]), "dry", ifelse(gam2.weights[gam2.weights$Site==s, "Year"] %in% unique(climate.markers[climate.markers$Site==s & climate.markers$marker=="wet", "marker.year"]), "wet","NONE"))
}
gam2.weights$Temp.Mark <- as.factor(gam2.weights$Temp.Mark)
gam2.weights$Precip.Mark <- as.factor(gam2.weights$Precip.Mark)

cols.use <- c("BA.inc", "BA.inc.Clim", "BA.inc.Clim.log", "Clim.Rel", "Clim.Rel.log")
gam2.dat.summary <- aggregate(gam2.weights[,cols.use],
                         by=gam2.weights[,c("Year", "Site", "Canopy.Class", "Temp.Mark", "Precip.Mark")],
                         FUN=mean)

gam2.dat.summary[,paste0(cols.use, ".LB")] <- aggregate(gam2.weights[,cols.use],
                         by=gam2.weights[,c("Year", "Site", "Canopy.Class", "Temp.Mark", "Precip.Mark")],
                         FUN=quantile, 0.025, na.rm=T)[,cols.use]
                         
gam2.dat.summary[,paste0(cols.use, ".UB")] <- aggregate(gam2.weights[,cols.use],
                         by=gam2.weights[,c("Year", "Site", "Canopy.Class", "Temp.Mark", "Precip.Mark")],
                         FUN=quantile, 0.975, na.rm=T)[,cols.use]
summary(gam2.dat.summary)


gam2.dat.summary$Site <- factor(gam2.dat.summary$Site, levels=c("Missouri Ozark", "Morgan Monroe State Park", "Oak Openings Toledo", "Harvard", "Howland"))


pdf("figures/Prelim_Figures/gam2_rel_growth.pdf", width= 13, height = 8.5)
# Hot and Dry
ggplot(gam2.dat.summary) +
	facet_grid(Canopy.Class~Site, scales="fixed") +
	
	geom_ribbon(aes(x=Year, ymin=Clim.Rel.LB, ymax=Clim.Rel.UB, fill=Canopy.Class), alpha=0.35) +
	geom_line(aes(x=Year, y=Clim.Rel,color=Canopy.Class),size=0.75) +
		geom_hline(yintercept=0, color="gray50", linetype="dashed") +
	
	# geom_vline(data=gam2.dat.summary[gam2.dat.summary$Temp.Mark=="hot",], aes(xintercept=Year), alpha=0.3, color="red") +
	# geom_vline(data=gam2.dat.summary[gam2.dat.summary$Precip.Mark=="dry",], aes(xintercept=Year), alpha=0.5, color="brown", linetype="dashed") +
	
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())+
	theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0))
dev.off()

pdf("figures/Prelim_Figures/gam2_rel_growth_hot_dry.pdf", width= 13, height = 8.5)
# Hot and Dry
ggplot(gam2.dat.summary) +
	facet_grid(Site~., scales="free") +
	
	geom_ribbon(aes(x=Year, ymin=Clim.Rel.LB, ymax=Clim.Rel.UB, fill=Canopy.Class), alpha=0.35) +
	geom_line(aes(x=Year, y=Clim.Rel,color=Canopy.Class),size=0.75) +
		geom_hline(yintercept=0, color="gray50", linetype="dashed") +
	
	geom_vline(data=gam2.dat.summary[gam2.dat.summary$Temp.Mark=="hot",], aes(xintercept=Year), alpha=0.3, color="red") +
	geom_vline(data=gam2.dat.summary[gam2.dat.summary$Precip.Mark=="dry",], aes(xintercept=Year), alpha=0.5, color="brown", linetype="dashed") +
	
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())+
	theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0))
dev.off()

# Cool and Wet

pdf("figures/Prelim_Figures/gam2_rel_growth_cool_wet.pdf", width= 13, height = 8.5)
ggplot(gam2.dat.summary) +
	facet_grid(Site~., scales="free") +
	
	geom_ribbon(aes(x=Year, ymin=Clim.Rel.LB, ymax=Clim.Rel.UB, fill=Canopy.Class), alpha=0.35) +
	geom_line(aes(x=Year, y=Clim.Rel,color=Canopy.Class),size=0.75) +
		geom_hline(yintercept=0, color="gray50", linetype="dashed") +
	
	
	geom_vline(data=gam2.dat.summary[gam2.dat.summary$Temp.Mark=="cold",], aes(xintercept=Year), alpha=0.3, color="blue") +

	geom_vline(data=gam2.dat.summary[gam2.dat.summary$Precip.Mark=="wet",], aes(xintercept=Year), alpha=0.5, color="darkgreen", linetype="dashed") +
	
	
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())+
	theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0))
dev.off()


#########################################################
# Site Model--GAM4
#########################################################

# Looking at the non-Size & Time BAI
summary(gam4.weights)
# gam4.weights$BA.inc.Clim <- gam4.weights$BA.inc - exp(gam4.weights$fit.dbh.recon) - exp(gam4.weights$fit.Year)
# # After you re-run script 3.4, change to the following
gam4.weights$BA.inc.Clim <- gam4.weights$BA.inc / (exp(gam4.weights$fit.dbh.recon) * exp(gam4.weights$fit.Year) * exp(gam4.weights$fit.intercept))
summary(gam4.weights)

# Simple mean-centering on what's left after removing size & year trends
# Note: this didn't work because the trend in the data starts post-1950 for many sites
for(s in unique(gam4.weights$Site)){
		x.inc <- mean(gam4.weights[gam4.weights$Site==s, "BA.inc.Clim"], na.rm=T)
		gam4.weights[gam4.weights$Site==s,"Clim.Rel"] <- gam4.weights[gam4.weights$Site==s, "BA.inc.Clim"]/x.inc-1
	
}

# gam4.weights$Clim.Rel <- gam4.weights

gam4.dat.summary <- aggregate(gam4.weights[,c("BA.inc", "BA.inc.Clim", "Clim.Rel")],
                         by=gam4.weights[,c("Year", "Site")],
                         FUN=mean)

ggplot(gam4.dat.summary[,]) +
	facet_grid(Site~Canopy.Class) +
	geom_line(aes(x=Year, y=BA.inc), color="black") 

ggplot(gam4.dat.summary[,]) +
	facet_grid(Site~Canopy.Class) +
	geom_line(aes(x=Year, y=Clim.Rel), color="black") +
	geom_hline(yintercept=0, color="red", linetype="dashed")

gam4.weights$Temp.Mark <- NA
gam4.weights$Precip.Mark <- NA
for(s in unique(gam4.weights$Site)){
	gam4.weights[gam4.weights$Site==s,"Temp.Mark"] <- ifelse(gam4.weights[gam4.weights$Site==s, "Year"] %in% unique(climate.markers[climate.markers$Site==s & climate.markers$marker=="hot", "marker.year"]), "hot", ifelse(gam4.weights[gam4.weights$Site==s, "Year"] %in% unique(climate.markers[climate.markers$Site==s & climate.markers$marker=="cold", "marker.year"]), "cold", "NONE"))
	gam4.weights[gam4.weights$Site==s,"Precip.Mark"] <- ifelse(gam4.weights[gam4.weights$Site==s, "Year"] %in% unique(climate.markers[climate.markers$Site==s & climate.markers$marker=="dry", "marker.year"]), "dry", ifelse(gam4.weights[gam4.weights$Site==s, "Year"] %in% unique(climate.markers[climate.markers$Site==s & climate.markers$marker=="wet", "marker.year"]), "wet","NONE"))
}
gam4.weights$Temp.Mark <- as.factor(gam4.weights$Temp.Mark)
gam4.weights$Precip.Mark <- as.factor(gam4.weights$Precip.Mark)

cols.use <- c("BA.inc", "BA.inc.Clim", "Clim.Rel")
gam4.dat.summary <- aggregate(gam4.weights[,cols.use],
                         by=gam4.weights[,c("Year", "Site", "Temp.Mark", "Precip.Mark")],
                         FUN=mean)

gam4.dat.summary[,paste0(cols.use, ".LB")] <- aggregate(gam4.weights[,cols.use],
                         by=gam4.weights[,c("Year", "Site", "Temp.Mark", "Precip.Mark")],
                         FUN=quantile, 0.025, na.rm=T)[,cols.use]
                         
gam4.dat.summary[,paste0(cols.use, ".UB")] <- aggregate(gam4.weights[,cols.use],
                         by=gam4.weights[,c("Year", "Site", "Temp.Mark", "Precip.Mark")],
                         FUN=quantile, 0.975, na.rm=T)[,cols.use]
summary(gam4.dat.summary)





##########################################################################################
# Setting up an ANOVA to determine differences between marker years between canopy strata and species at each site
##########################################################################################

# GAM1--Species Model

# Setting non-extreme marker years as 'A' for ambient.
gam1.weights$Temp.Mark <- recode(gam1.weights$Temp.Mark, "'NONE'='A'")
gam1.weights$Precip.Mark <- recode(gam1.weights$Precip.Mark, "'NONE'='A'")
summary(gam1.weights)
save(gam1.weights, file="processed_data/gam1_weights_processed.Rdata")




# Precip Models
gam1.lme.precip <- lme(BA.inc ~ Precip.Mark*group-Precip.Mark, 
			random=list(PlotID=~1, TreeID=~1), data=gam1.weights) 
anova(gam1.lme.precip)
summary(gam1.lme.precip)
lsmeans(gam1.lme.precip, pairwise~Precip.Mark*group, adjust="tukey")



gam1.wt.stack <- stack(gam1.weights[,c("weight.precip", "weight.tmean", "weight.dbh.recon")])
names(gam1.wt.stack) <- c("Weight", "Factor")
gam1.wt.stack$Factor <- as.factor(substr(gam1.wt.stack$Factor, 8, nchar(paste(gam1.wt.stack$Factor))))
gam1.wt.stack[,c("State", "PlotID", "TreeID", "Temp.Mark", "Precip.Mark", "group", "BA.inc")] <- gam1.weights[,c("State", "PlotID", "TreeID", "Temp.Mark", "Precip.Mark", "group", "BA.inc")]
summary(gam1.wt.stack)

gam1.lme.wts.temp <- lme(Weight ~ Factor*Temp.Mark*group-Temp.Mark, random=list(State=~1, PlotID=~1, TreeID=~1), data=gam1.wt.stack)
anova(gam1.lme.wts.temp)
summary(gam1.lme.wts.temp)
lsmeans(gam1.lme.wts.temp, pairwise~Factor*Temp.Mark*group, adjust="tukey")

gam1.lme.wts.precip <- lme(Weight ~ Factor*Precip.Mark*group-Temp.Mark, random=list(State=~1, PlotID=~1, TreeID=~1), data=gam1.wt.stack)
anova(gam1.lme.wts.precip)
summary(gam1.lme.wts.precip)
lsmeans(gam1.lme.wts.precip, pairwise~Factor*Precip.Mark*group, adjust="tukey")


############################################################
# GAM2--Canopy.Class Model
############################################################
# Recoding Non-extreme years to 'A' for ambient
gam2.weights$Temp.Mark <- recode(gam2.weights$Temp.Mark, "'NONE'='A'")
gam2.weights$Precip.Mark <- recode(gam2.weights$Precip.Mark, "'NONE'='A'")
save(gam2.weights,file="processed_data/gam2_weights_processed.Rdata")

summary(gam2.weights)
gam2.weights <- gam2.weights[gam2.weights$Year >=1950]

head(gam2.weights)
gam2.lme.temp <- lme(BA.inc ~ Temp.Mark*Canopy.Class-Temp.Mark, 
			random=list(State=~1,PlotID=~1, TreeID=~1), data=gam2.weights)
anova(gam2.lme.temp)
summary(gam2.lme.temp)
lsmeans(gam2.lme.temp, pairwise~Temp.Mark*Canopy.Class, adjust="tukey")	

gam2.lme.precip <- lme(BA.inc ~ Precip.Mark*Canopy.Class-Temp.Mark, 
			random=list(State=~1,PlotID=~1, TreeID=~1), data=gam2.weights)
anova(gam2.lme.precip)
summary(gam2.lme.precip)
lsmeans(gam2.lme.precip, pairwise~Precip.Mark*Canopy.Class, adjust="tukey")	


gam2.wt.stack <- stack(gam2.weights[,c("weight.precip", "weight.tmean", "weight.dbh.recon")])
names(gam2.wt.stack) <- c("Weight", "Factor")
gam2.wt.stack$Factor <- as.factor(substr(gam2.wt.stack$Factor, 8, nchar(paste(gam2.wt.stack$Factor))))
gam2.wt.stack[,c("State", "PlotID", "TreeID", "Temp.Mark", "Precip.Mark", "Canopy.Class", "BA.inc")] <- gam2.weights[,c("State", "PlotID", "TreeID", "Temp.Mark", "Precip.Mark", "Canopy.Class", "BA.inc")]
summary(gam2.wt.stack)

gam2.lme.wt.temp <- lme(Weight~Factor*Temp.Mark*Canopy.Class-Temp.Mark,
						random=list(State=~1,PlotID=~1, TreeID=~1), data=gam2.wt.stack); anova(gam2.lme.wt.temp)
summary(gam2.lme.wt.temp)
lsmeans(gam2.lme.wt.temp, pairwise~Factor*Temp.Mark*Canopy.Class, adjust="tukey")



# Setting up a site by site runs to just compare the growth between canopy Classes
gam2.lme.sites <- lme(BA.inc~Temp.Mark*State*Canopy.Class-Temp.Mark,	
						random=list(State=~1, PlotID=~1, TreeID=~1),
						data=gam2.weights)
						
anova(gam2.lme.sites)
summary(gam2.lme.sites)

lsmeans(gam2.lme.sites, pairwise~State*Canopy.Class, adjust="tukey")





############################################################
# GAM4--Site Model
############################################################
# Recoding Non-extreme years to 'A' for ambient
gam4.weights$Temp.Mark <- recode(gam4.weights$Temp.Mark, "'NONE'='A'")
gam4.weights$Precip.Mark <- recode(gam4.weights$Precip.Mark, "'NONE'='A'")

summary(gam4.weights)
save(gam4.weights, file="processed_data/gam4_weights_processed.Rdata")



# Temp
gam4.lme.temp <- lme(BA.inc ~ Temp.Mark*State-Temp.Mark, 
			random=list(TreeID=~1), data=gam4.weights); anova(gam4.lme.temp)
summary(gam4.lme.temp)	
lsmeans(gam4.lme.temp, pairwise~Temp.Mark*State, adjust="tukey")		

# Precip
gam4.lme.precip <- lme(BA.inc ~ Precip.Mark*State-Precip.Mark, 
			random=list(PlotID=~1, TreeID=~1), data=gam4.weights); anova(gam4.lme.precip)
summary(gam4.lme.precip)	
lsmeans(gam4.lme.precip, pairwise~Precip.Mark*State, adjust="tukey")		



gam4.wt.stack <- stack(gam4.weights[,c("weight.precip", "weight.tmean", "weight.dbh.recon")])
names(gam4.wt.stack) <- c("Weight", "Factor")
gam4.wt.stack$Factor <- as.factor(substr(gam4.wt.stack$Factor, 8, nchar(paste(gam4.wt.stack$Factor))))
gam4.wt.stack[,c("State", "PlotID", "TreeID", "Temp.Mark", "Precip.Mark", "group", "BA.inc")] <- gam4.weights[,c("State", "PlotID", "TreeID", "Temp.Mark", "Precip.Mark", "group", "BA.inc")]
summary(gam4.wt.stack)

gam4.lme.wts.temp <- lme(Weight ~ Temp.Mark*Factor*State, 
			random=list(PlotID=~1, TreeID=~1), data=gam4.wt.stack); anova(gam4.lme.wts.temp)
summary(gam4.lme.wts.temp)	
lsmeans(gam4.lme.wts.temp, pairwise~Temp.Mark*Factor*State, adjust="tukey")		

gam4.lme.wts.precip <- lme(Weight ~ Precip.Mark*Factor*State, 
			random=list(PlotID=~1, TreeID=~1), data=gam4.wt.stack); anova(gam4.lme.wts.precip)
summary(gam4.lme.wts.precip)	
lsmeans(gam4.lme.wts.precip, pairwise~Precip.Mark*Factor*State, adjust="tukey")	


######################################################################################################
# Setting Up Comparisons between the different factors
# Getting Hard numbers for growth fluctuations
######################################################################################################



# Missouri Ozark
#Hot
mean(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

# Dry

mean(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)



# Cold
mean(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)


# Wet
mean(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Missouri Ozark" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)



# Morgan Monroe
#Hot
mean(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

# Dry

mean(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)



# Cold
mean(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)


# Wet
mean(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Morgan Monroe State Park" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)


# Oak Openings
#Hot
mean(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

# Dry

mean(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)



# Cold
mean(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)


# Wet
mean(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Oak Openings Toledo" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)

# Harvard
#Hot
mean(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

# Dry

mean(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)



# Cold
mean(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)


# Wet
mean(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Harvard" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)

# Howland
#Hot
mean(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="hot", "Clim.Rel"], na.rm=T)

# Dry

mean(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="dry", "Clim.Rel"], na.rm=T)



# Cold
mean(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Temp.Mark=="cold", "Clim.Rel"], na.rm=T)


# Wet
mean(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="D" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="I" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)

mean(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T); sd(gam2.dat.summary[gam2.dat.summary$Site=="Howland" & gam2.dat.summary$Canopy.Class=="S" & gam2.dat.summary$Precip.Mark=="wet", "Clim.Rel"], na.rm=T)


