library(mgcv)
library(ggplot2)
library(car)
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
predictors.all <- c("tmean", "precip", "Species", "dbh.recon", "Canopy.Class", "spp.plot", "group", "group.plot", "group.cc", "Site", "Year", "PlotID") 

# Getting rid of observations that have NAs in the important variables
test <- test[complete.cases(test[,predictors.all]),]
test <- test[test$Live.Dead=="LIVE" & !test$Canopy.Class=="F",]

# Subsetting to a set of species that we have enough data to feel good about
#species.use <- c("TSCA", "QURU", "ACRU", "BEAL", "ACSA", "LITU", "QUAL", "CAOV", "CACO", "CATE", "JUVI", "QUVE", "PCRU", "THOC", "PIST")
group.use <- c("ACRU", "ACSA", "BETULA", "CARYA", "FAGR", "FRAX", "PIST", "QUAL", "QURU", "QUVE", "SAAL", "TSCA", "ULRU", "PCRU", "THOC", "LITU", "JUVI")

test <- test[test$group %in% group.use,]

summary(test)

par(new=F)
plot(test[test$TreeID=="MMA003", "BA.inc"]~ test[test$TreeID=="MMA003","Year"], type="l")


test$log.dbh <- log(test$dbh.recon)
summary(test)

summary(test)
summary(test$Live.Dead)
summary(test$Canopy.Class)
# RW <- test$RW
# temp <- test$tmean
# precip <- test$precip
# canopy <- test$Canopy.Class
# size <- test$DBH..cm.
# species <- unique(test$Species)
# library(ggplot2)
# ggplot(data=test) +
   # facet_wrap(~PlotID) +
   # geom_histogram(aes(x=dbh.recon))
# ggplot(data=test) +
   # facet_wrap(~Species) +
   # geom_histogram(aes(x=dbh.recon))


# hist(test$dbh.recon)


# test2 <- test[test$group %in% c("QURU", "ACRU") & test$Year>=1980,]
# test2 <- test[test$group %in% c("QURU", "ACRU"),]
test2 <- test[test$Site %in% c("Morgan Monroe State Park", "Harvard"), ]
# test2$log.dbh <- log(test2$dbh.recon)
# summary(test2)


summary(test)
test[test$BA.inc==0, "BA.inc"] <- 1e-6
save(test, file="ch2_combined_data_use.Rdata")
# test.gam3 <- test
# test.gam3$Canopy.Class <- recode(test.gam3$Canopy.Class, "'C' = 'D'")
# summary(test.gam3)
################################################### 
# HERE'S THE GAMM!!!
################################################### 
# RW ~ CLIMATE(Species) + Size
                  

gam1 <- gam(log(BA.inc)~ s(tmean, k=3, by=group) +
                  s(precip, k=3, by=group) +
                  s(dbh.recon, k=3, by=group) +
                  s(Year, k=4, by=PlotID)+
                   Site + PlotID  + TreeID + Canopy.Class + group,
                  # random=list(Site=~1, PlotID=~1, TreeID=~1),
                  data=test)

summary(gam1)$r.sq # R-squared
summary(gam1)$dev.expl # explained deviance
anova(gam1)
AIC(gam1)
# Results 10 Aug 2016
# Only a few species are significantly sensitive to temp (p<0.05): ACRU, ACSA, PIST, TSCA
# Only a few species are significantly sensitive to precip (p<0.05): ACSA, TSCA, (FRAX is close)
                  
# # gam1.test <- gamm(BA.inc ~ s(tmean, k=3, by=group) + # tmean*Species 
                  # s(precip, k=3, by=group) +
                  # s(log.dbh, k=3, by=group.plot) +
                  # Canopy.Class, 
                  # random=list(Site =~1, PlotID=~1),
                  # data=test2)
             

save(gam1, file="processed_data/gam_results/gam1_climate_by_species.Rdata")

# s(tmean, by=Spp.Can) + s(tmean, by=Canopy)
# s(tmean, by=Spp) + s(tmean, by=Canopy) + s(tmean, by=Spp.Can)

                  
gam2 <- gam(log(BA.inc)~ s(tmean, k=3, by=Canopy.Class) +
                  s(precip, k=3, by=Canopy.Class) +
                  s(dbh.recon, k=3, by=Canopy.Class) +
                  s(Year, k=4, by=PlotID)+
                   Site + PlotID  + TreeID + Canopy.Class + group,
                  # random=list(Site=~1, PlotID=~1, TreeID=~1),
                  data=test)
# Look at the R-squared and explained deviance
summary(gam2)$r.sq # R-squared
summary(gam2)$dev.expl # explained deviance
anova(gam2) 
AIC(gam2)
# Results 10 Aug 2016
# Only Dominant trees have a significant trend with precip
# Intermediate trees (as a whole) are insensitive to temperature 
#  -- suppressed trees still have the U-shape
                  
# # gam2.test <- gamm(BA.inc~ s(tmean, k=3, by=Canopy.Class) +
                  # s(precip, k=3, by=Canopy.Class) +
                  # s(log.dbh, k=3, by=group) +
                  # group,
                  # random=list(Site=~1, PlotID=~1),
                  # data=test2)  
 
# gam3 <- gamm(log(BA.inc)~ s(tmean, k=3, by=group.cc) +
                  # s(precip, k=3, by=group.cc) +
                  # s(dbh.recon, k=3, by=group.cc) +
                  # Canopy.Class + group,
                  # random=list(Site=~1, PlotID=~1, TreeID=~1),
                  # data=test, control=list(niterEM=0, sing.tol=1e-20, opt="optim"))

gam4 <- gam(log(BA.inc)~ s(tmean, k=3, by=Site) +
                  s(precip, k=3, by=Site) +
                  s(dbh.recon, k=3, by=Site) +
                  s(Year, k=4, by=PlotID)+
                  Site + PlotID  + TreeID + Canopy.Class + group,
                  # random=list(Site=~1, PlotID=~1, TreeID=~1),
                  data=test)
summary(gam4)$r.sq # R-squared
summary(gam4)$dev.expl # explained deviance
anova(gam4) 
AIC(gam4)
# Results 10 Aug 2016
# Only Dominant trees have a significant trend with precip
# Intermediate trees (as a whole) are insensitive to temperature 
#  -- suppressed trees still have the U-shape



save(gam2, file="processed_data/gam_results/gam2_climate_by_canopyclass.Rdata") 
# save(gam3, file="processed_data/gam_results/gam3_climate_by_canopyclass_interactions.Rdata")
save(gam4, file="processed_data/gam_results/gam4_Site_level_model.Rdata") 



 par(mfrow=c(4,2)); plot(gam1$gam, ylim=c(-0.025, 0.025))
 par(mfrow=c(4,2)); plot(gam2$gam, ylim=c(-0.025, 0.025))
 
# MAKE gam2 by= canopy class
# make sure to when adding multiple site to list Site in teh random list.
# Random effects are hierarchical 
 
 

#----------------------------------------------
# Gam1 graphs
n <- 100
data <- test
 

		n.out = n

		new.dat <- data.frame(Model="species_response",
							  Extent=as.factor(paste(min(data$Year), max(data$Year), sep="-")))
		
		# Figure out which vars are numeric vs. factor
		vars.num <- vector()
		
		for(v in predictors.all){
			if(class(data[,v]) %in% c("numeric", "integer")) vars.num <- c(vars.num, v)
		}
		
		# Getting the unique values of our factor variables and adding them to the data frame
		# need to skip group & group.cc so we aren't trying to match Carya & Quercus etc
		# predictors.all <- predictors.all[!predictors.all %in% c("Species", "spp.plot")]
		predictors.all <- c("tmean", "precip", "dbh.recon", "Year", "Site", "PlotID", "TreeID", "Canopy.Class", "group")
		spline.by=c("group", "Canopy.Class", "Site", "PlotID") # the "by" terms in the models you're running
		for(v in predictors.all[!predictors.all %in% vars.num & !(predictors.all %in% c("Site"))]){
			# if v is a factor, merge all unique values into the dataframe
			if(!(v %in% spline.by)){ # Only pull the full range of values for whatever the "by" term was by, otherwise everythign should have the same shape, just different intercepts
				var.temp <- data.frame(x=unique(data[,v])[1]) 
			}else {
				var.temp <- data.frame(x=unique(data[,v])) 
			}
			names(var.temp) <- v
			new.dat <- merge(new.dat, var.temp, all.x=T, all.y=T)
		}
		# Separate out Plot & Site
		# new.dat$PlotID <- as.factor(ifelse(substr(new.dat$TreeID, 1, 3)=="HOW", substr(new.dat$TreeID, 1, 4), substr(new.dat$TreeID, 1, 3)))
		
		# Matching the site for the plot
		for(p in unique(new.dat$PlotID)){
			new.dat[new.dat$PlotID==p, "Site"] <- unique(test[test$PlotID==p, "Site"])
		}
		new.dat$Site <- as.factor(new.dat$Site)
		summary(new.dat)
		
		# getting species from species.plot
		# new.dat$group <- as.factor(ifelse(substr(new.dat$group.plot, 1, 4)=="BETU", "BETULA", ifelse(substr(new.dat$group.plot, 1,4)=="CARY", "CARYA", substr(new.dat$group.plot, 1, 4))))
		
		# # Adding in the group.canopy class then getting rid of combinations we don't actually have in our data
		# new.dat$group.cc <- as.factor(paste(new.dat$group, new.dat$Canopy.Class, sep="."))
		# new.dat <- new.dat[new.dat$group.cc %in% unique(test$group.cc),]
		# summary(new.dat)
		
		# Putting the numerical variables into an array and adding it in 
		var.temp <- data.frame(array(dim=c(n.out, length(vars.num))))
		names(var.temp) <- vars.num
		for(v in vars.num){
			var.temp[,v] <- seq(min(data[,v], na.rm=T), max(data[,v], na.rm=T), length.out=n.out)
		}								
		new.dat <- merge(new.dat, var.temp, all.x=T, all.y=T)
		summary(new.dat)
								
write.csv(new.dat, file="processed_data/sensitivity_extaction_dataframe.csv", row.names=F)								

# Change which gamm you look at here!

load("processed_data/gam_results/gam1_climate_by_species.Rdata")
n <- 100						
		# SOurce & run the function
		source("0_Calculate_GAMM_Posteriors.R")
		# Make things run faster by reducing dimensions
		new.dat2 <- new.dat
		vars.fac <- c("Site", "PlotID", "TreeID", "Canopy.Class", "group")
		var.smooth <- "group"
		for(v in vars.fac){
			if(v == var.smooth) next # keep all levels for our "by" variable
			# Get rid of unimportant levels for everything else
			l1 <- unique(new.dat2[,v])[1]
			new.dat2 <- new.dat2[new.dat2[,v]==l1,]
		}
		g1.ci.terms.pred <- post.distns(model.gam=gam1, model.name="species_response", n=n, newdata=new.dat2, vars=predictors.all, terms=T)
		
		g1.ci.out <- g1.ci.terms.pred$ci # separting out the confidence interval 
		g1.ci.out[,predictors.all[!predictors.all %in% vars.num]] <- new.dat2[,predictors.all[!predictors.all %in% vars.num]] # copying over our factor labels
		g1.ci.out$x <- as.numeric(g1.ci.out$x) # making x numeric; will make factors NA
		summary(g1.ci.out)
		
		g1.ci.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(g1.ci.out[,c("mean", "lwr", "upr")])
		
spp.colors <- read.csv("spp.Colors.csv", header=T)	
summary(spp.colors)	

group.fig <- unique(g1.ci.out$group)
group.fig <- group.fig[order(group.fig)]
colors.use <- as.vector(c(paste(spp.colors[spp.colors$Species %in% group.fig, "color"])))

ci.terms.graph <- g1.ci.out
ci.terms.graph[ci.terms.graph$mean<(-3),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr<(-3),"lwr"] <- -3
ci.terms.graph[ci.terms.graph$upr<(-3),"upr"] <- -3 
ci.terms.graph[which(ci.terms.graph$mean>3),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr>(3),"lwr"] <- 3
ci.terms.graph[ci.terms.graph$upr>(3),"upr"] <- 3 

ci.terms.graph[ci.terms.graph$mean.bai<(0),"mean.bai"] <- NA 
ci.terms.graph[ci.terms.graph$lwr.bai<(0),"lwr.bai"] <- 0
ci.terms.graph[ci.terms.graph$upr.bai<(0),"upr.bai"] <- 0 
ci.terms.graph[which(ci.terms.graph$mean.bai>30),"mean.bai"] <- NA 
ci.terms.graph[ci.terms.graph$lwr.bai>(30),"lwr.bai"] <- 30
ci.terms.graph[ci.terms.graph$upr.bai>(30),"upr.bai"] <- 30 
	

# Truncating to observed range
# DBH		
for(s in unique(test$group)){
	dbh.min <- min(test[test$group==s, "dbh.recon"])
	dbh.max <- max(test[test$group==s, "dbh.recon"])
	
	ci.terms.graph$x <- ifelse(ci.terms.graph$group!=s | ci.terms.graph$Effect!="dbh.recon" | (ci.terms.graph$x>=dbh.min & ci.terms.graph$x<=dbh.max), ci.terms.graph$x, NA)
	
	}

# Temp
for(s in unique(test$group)){
	temp.min <- min(test[test$group==s, "tmean"])
	temp.max <- max(test[test$group==s, "tmean"])
	
		ci.terms.graph$x <- ifelse(ci.terms.graph$group!=s | ci.terms.graph$Effect!="tmean" | (ci.terms.graph$x>=temp.min & ci.terms.graph$x<=temp.max), ci.terms.graph$x, NA)
	
	}
	
# Precip	
for(s in unique(test$group)){
	precip.min <- min(test[test$group==s, "precip"])
	precip.max <- max(test[test$group==s, "precip"])
	
		ci.terms.graph$x <- ifelse(ci.terms.graph$group!=s | ci.terms.graph$Effect!="precip" | (ci.terms.graph$x>=precip.min & ci.terms.graph$x<=precip.max), ci.terms.graph$x, NA)
	
	}


save(ci.terms.graph,file="processed_data/gam1_response_graph.Rdata")

pdf("figures/prelim_figures/gam1_sensitivities_truncated_tmean.pdf", width= 13, height = 8.5)		
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean"), ]) + 
			facet_wrap(group~Effect) +
			geom_line(aes(x=x, y=0), linetype="dotted")+
			geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=exp(mean), color=group)) +
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)+
			theme_bw()+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", 							"y"^"-1",")")))) +
			ylim(0,5)
dev.off()

pdf("figures/prelim_figures/gam1_sensitivities_truncated_precip.pdf", width= 13, height = 8.5)		
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("precip"), ]) + 
			facet_wrap(group~Effect) +
			geom_line(aes(x=x, y=0), linetype="dashed")+
			geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=exp(mean), color=group)) +
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)+
			theme_bw()+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", 							"y"^"-1",")"))))+
			scale_y_continuous(expand=c(0,0))
dev.off()

pdf("figures/prelim_figures/gam1_sensitivities_truncated_size.pdf", width= 13, height = 8.5)		
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("dbh.recon") & ci.terms.graph$group %in% c("JUVI", "ULRU"), ]) + 
			facet_wrap(~Effect) +
			geom_line(aes(x=x, y=0), linetype="dashed")+
			geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=exp(mean), color=group)) +
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)+
			theme_bw()+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", 							"y"^"-1",")"))))+
			scale_y_continuous(expand=c(0,0))
dev.off()




pdf("figures/prelim_figures/gam1_sensitivities_truncated_combo.pdf", width= 13, height = 8.5)		
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean", "precip", "dbh.recon"),]) + 
			facet_grid(group~Effect, scales="free_x") +
			geom_line(aes(x=x, y=0), linetype="dashed")+
			geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean.bai, color=group)) +
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)+
			theme_bw()+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
			 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))

        
dev.off()

pdf("figures/prelim_figures/gam1_sensitivities_truncated_combo2.pdf", width= 13, height = 8.5)		
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean", "precip", "dbh.recon"),]) + 
			facet_grid(~Effect, scales = "free") +
			geom_hline(yintercept=0, linetype="dashed")+
			geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean.bai, color=group)) +
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)+
			theme_bw()+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
			 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))

dev.off()


# pdf("figures/gam1_sensitivities_ACRU.pdf", width= 13, height = 8.5)					
	# ggplot(data=ci.terms.graph[ci.terms.graph$group %in% "ACRU" & (ci.terms.graph$Effect %in% c("tmean", "precip") | (ci.terms.graph$Effect=="dbh.recon" & ci.terms.graph$x<=50)),]) + 
			# facet_grid(~Effect, scales="free_x") +
			# geom_line(aes(x=x, y=0), linetype="dashed")+
			# geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=Effect), alpha=0.4) +
			# geom_line(aes(x=x, y=mean, color=Effect)) +
			# scale_color_manual(values= c("red", "blue", "green")) +
			# scale_fill_manual(values=c("red", "blue", "green"))+
			# poster.theme2+
			# labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", 							"y"^"-1",")"))))
# dev.off()





# ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "tmean", ]) + 
			# facet_grid(~Effect, scales="free_x") +
			# geom_line(aes(x=x, y=0), linetype="dashed")+
			# geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			# geom_line(aes(x=x, y=mean, color=group)) +
			# scale_color_manual(values=colors.use) +
			# scale_fill_manual(values=colors.use)
			
			# ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "precip", ]) + 
			# facet_grid(Canopy.Class~Effect, scales="free_x") +
			# geom_line(aes(x=x, y=0), linetype="dashed")+
			# geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			# geom_line(aes(x=x, y=mean, color=group)) +
			# scale_color_manual(values=colors.use) +
			# scale_fill_manual(values=colors.use)

			
		
				
		
		
		# g1.ci.out$PlotID <- as.factor(substr(g1.ci.out$group.plot, 6, nchar(paste(g1.ci.out$group.plot)))) # adding a plotID factor
		# summary(g1.ci.out)
		
		# ggplot(data=g1.ci.out[g1.ci.out$Effect == "dbh.recon", ]) + 
			# facet_wrap(~PlotID) +
			# geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			# geom_line(aes(x=x, y=mean, color=group)) +
			# scale_color_manual(values=colors.use) +
			# scale_fill_manual(values=colors.use)

#----------------------------------------------
# GAM 2								
load("processed_data/gam_results/gam2_climate_by_canopyclass.Rdata")

# SOurce & run the function
		source("0_Calculate_GAMM_Posteriors.R")
		g2.ci.terms.pred2 <- post.distns(model.gam=gam2, model.name="harv_mmf_cc.test", n=n, newdata=new.dat, vars=predictors.all, terms=T)
		
		g2.ci.out2 <- g2.ci.terms.pred2$ci # separting out the confidence interval 
		g2.ci.out2[,predictors.all[!predictors.all %in% vars.num]] <- new.dat[,predictors.all[!predictors.all %in% vars.num]] # copying over our factor labels
		g2.ci.out2$x <- as.numeric(g2.ci.out2$x) # making x numeric; will make factors NA
		summary(g2.ci.out2)
		
		g2.ci.out2[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(g2.ci.out2[,c("mean", "lwr", "upr")])
		
		
# Truncating to observed range
# DBH		
for(s in unique(test$Canopy.Class)){
	dbh.min <- min(test[test$Canopy.Class==s, "dbh.recon"])
	dbh.max <- max(test[test$Canopy.Class==s, "dbh.recon"])
	
	g2.ci.out2$x <- ifelse(g2.ci.out2$Canopy.Class!=s | g2.ci.out2$Effect!="dbh.recon" | (g2.ci.out2$x>=dbh.min & g2.ci.out2$x<=dbh.max), g2.ci.out2$x, NA)
	
	}

# Temp
for(s in unique(test$Canopy.Class)){
	temp.min <- min(test[test$Canopy.Class==s, "tmean"])
	temp.max <- max(test[test$Canopy.Class==s, "tmean"])
	
		g2.ci.out2$x <- ifelse(g2.ci.out2$Canopy.Class!=s | g2.ci.out2$Effect!="tmean" | (g2.ci.out2$x>=temp.min & g2.ci.out2$x<=temp.max), g2.ci.out2$x, NA)
	
	}
	
# Precip	
for(s in unique(test$Canopy.Class)){
	precip.min <- min(test[test$Canopy.Class==s, "precip"])
	precip.max <- max(test[test$Canopy.Class==s, "precip"])
	
		g2.ci.out2$x <- ifelse(g2.ci.out2$Canopy.Class!=s | g2.ci.out2$Effect!="precip" | (g2.ci.out2$x>=precip.min & g2.ci.out2$x<=precip.max), g2.ci.out2$x, NA)
	
	}		

# pdf("figures/Prelim_Figures/gam2_sensitivities.pdf", width= 13, height = 8.5)		
# 		ggplot(data=g2.ci.out2[g2.ci.out2$Effect %in% c("tmean", "precip", "dbh.recon"), ]) + 
# 			facet_wrap(~Effect, scales="free_x") +
# 			geom_line(aes(x=x, y=0), linetype="dashed")+
# 			geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=Canopy.Class), alpha=0.5) +
# 			geom_line(aes(x=x, y=exp(mean), color=Canopy.Class))+
# 			scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00")) +
# 			scale_fill_manual(values=c("#0072B2", "#009E73",  "#E69F00")) +
# 			theme_bw()+
# 			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
# 			 theme(axis.line.x = element_line(color="black", size = 0.5),
#         axis.line.y = element_line(color="black", size = 0.5))+
#         ylim(0,2.5)
# dev.off()		
save(g2.ci.out2, file="processed_data/gam2_response_graph.R")

pdf("figures/Prelim_Figures/gam2_sensitivities.pdf", width= 13, height = 8.5)		
ggplot(data=g2.ci.out2[g2.ci.out2$Effect %in% c("tmean", "precip", "dbh.recon"), ]) + 
  facet_wrap(~Effect, scales="free_x") +
  geom_line(aes(x=x, y=0), linetype="dashed")+
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Canopy.Class), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Canopy.Class))+
  scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00")) +
  scale_fill_manual(values=c("#0072B2", "#009E73",  "#E69F00")) +
  theme_bw()+
  labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
  scale_y_continuous(expand=c(0,0))
dev.off()		

pdf("figures/Prelim_Figures/gam2_sensitivities_no_size.pdf", width= 13, height = 8.5)		
ggplot(data=g2.ci.out2[g2.ci.out2$Effect %in% c("tmean", "precip"), ]) + 
  facet_wrap(~Effect, scales="free_x") +
  geom_line(aes(x=x, y=0), linetype="dashed")+
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Canopy.Class), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Canopy.Class))+
  scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00")) +
  scale_fill_manual(values=c("#0072B2", "#009E73",  "#E69F00")) +
  theme_bw()+
  labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
  scale_y_continuous(expand=c(0,0))
dev.off()



		g2.ci.out2$PlotID <- as.factor(substr(g2.ci.out2$group.plot, 6, nchar(paste(g2.ci.out2$group.plot)))) # adding a plotID factor
		summary(g2.ci.out2)
# pdf("figures/gam2_sensitivities_size.pdf", width = 13, height= 8.5)		
		# ggplot(data=g2.ci.out2[g2.ci.out2$Effect == "dbh.recon", ]) + 
			# facet_wrap(~group) +
			# geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=group), alpha=0.5) +
			# geom_line(aes(x=x, y=exp(mean), color=group)) +
			# scale_color_manual(values=colors.use) +
			# scale_fill_manual(values=colors.use)+
			# theme_bw()
# dev.off()

#----------------------------------------------
# GAM 3								
		# SOurce & run the function
# "processed_data/gam_results/gam3_climate_by_canopyclass_interactions.Rdata"
load("processed_data/gam_results/gam3_climate_by_canopyclass_interactions.Rdata")		
		source("0_Calculate_GAMM_Posteriors.R")
		g3.ci.terms.pred <- post.distns(model.gam=gam3, model.name="harv_mmf_cc.test", n=n, newdata=new.dat, vars=predictors.all, terms=T)
		
		g3.ci.out <- g3.ci.terms.pred$ci # separting out the confidence interval 
		g3.ci.out[,predictors.all[!predictors.all %in% vars.num]] <- new.dat[,predictors.all[!predictors.all %in% vars.num]] # copying over our factor labels
		g3.ci.out$x <- as.numeric(g3.ci.out$x) # making x numeric; will make factors NA
		summary(g3.ci.out)
		
ci.terms.graph <- g3.ci.out
ci.terms.graph[ci.terms.graph$mean<(-2.5),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr<(-2.5),"lwr"] <- -2.5
ci.terms.graph[ci.terms.graph$upr<(-2.5),"upr"] <- -2.5 
ci.terms.graph[which(ci.terms.graph$mean>2.5),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr>(2.5),"lwr"] <- 2.5 
ci.terms.graph[ci.terms.graph$upr>(2.5),"upr"] <- 2.5 

pdf("figures/gam3_sensitivities.pdf", width= 13, height = 8.5)
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean", "precip"), ]) + 
			facet_grid(Canopy.Class~Effect, scales="free_x") +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group))+
			poster.theme2+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", 							"y"^"-1",")"))))
dev.off()

pdf("figures/gam3_sensitivities_size.pdf", width= 13, height = 8.5)
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "dbh.recon" & ci.terms.graph$x<=50, ]) + 
			facet_wrap(~group.cc, scales="free_x") +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group))+
			#poster.theme2+
			scale_color_manual(values=colors.use) +
			scale_fill_manual(values=colors.use)+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", 							"y"^"-1",")"))))
dev.off()



ci.terms.graph <- g3.ci.out
ci.terms.graph[ci.terms.graph$mean<(-13),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr<(-13),"lwr"] <- -13
ci.terms.graph[ci.terms.graph$upr<(-13),"upr"] <- -13 
ci.terms.graph[which(ci.terms.graph$mean>8),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr>(8),"lwr"] <- 8 
ci.terms.graph[ci.terms.graph$upr>(8),"upr"] <- 8 

			
# Oaks only
			
pdf("figures/oak_climate_effects.pdf", width= 13, height = 8.5)		
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean", "precip") & substr(ci.terms.graph$group.cc, 1,2)=="QU", ]) + 
			facet_grid(Canopy.Class~Effect, scales="free_x") +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group))+
			scale_color_manual(values=c("#E69F00", "#0072B2", "#009E73")) +
			scale_fill_manual(values=c("#E69F00", "#0072B2", "#009E73")) +
			poster.theme2+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2", "y"^"-1",")"))))
dev.off()			
			
		
		g3.ci.out$PlotID <- as.factor(substr(g3.ci.out$group.plot, 6, nchar(paste(g3.ci.out$group.plot)))) # adding a plotID factor
		summary(g3.ci.out)
		
		ggplot(data=g3.ci.out[g3.ci.out$Effect == "dbh.recon", ]) + 
			facet_wrap(~PlotID) +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group))

pdf("figures/oak_size_effects.pdf", width= 13, height = 8.5)		
	ggplot(data=g3.ci.out[g3.ci.out$Effect == "dbh.recon" & substr(g3.ci.out$group.cc, 1,2)=="QU", ]) + 
			facet_grid(~Canopy.Class) +
			geom_ribbon(aes(x=x, ymin=lwr, ymax=upr, fill=group), alpha=0.5) +
			geom_line(aes(x=x, y=mean, color=group)) +
			scale_color_manual(values=c("#E69F00", "#0072B2", "#009E73")) +
			scale_fill_manual(values=c("#E69F00", "#0072B2", "#009E73")) +
			poster.theme2+
			labs(x = "DBH (cm)", y = expression(bold(paste("Effect on BAI (mm"^"2", "y"^"-1",")"))))
 dev.off()
 
#----------------------------------------------
# GAM 4								
# SOurce & run the function

load("processed_data/gam_results/gam4_Site_level_model.Rdata")		

source("0_Calculate_GAMM_Posteriors.R")
g4.ci.terms.pred <- post.distns(model.gam=gam4, model.name="Site_level", n=n, newdata=new.dat, vars=predictors.all, terms=T)
		
		g4.ci.out <- g4.ci.terms.pred$ci # separting out the confidence interval 
		g4.ci.out[,predictors.all[!predictors.all %in% vars.num]] <- new.dat[,predictors.all[!predictors.all %in% vars.num]] # copying over our factor labels
		g4.ci.out$x <- as.numeric(g4.ci.out$x) # making x numeric; will make factors NA; NA's are ok here
		summary(g4.ci.out)

		# Convert mean, lwr, upr to BAI units
		g4.ci.out[,c("mean.bai", "lwr.bai", "upr.bai")] <- exp(g4.ci.out[,c("mean", "lwr", "upr")])
		summary(g4.ci.out)
		
ci.terms.graph <- g4.ci.out
ci.terms.graph[ci.terms.graph$mean<(-2.5),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr<(-2.5),"lwr"] <- -2.5
ci.terms.graph[ci.terms.graph$upr<(-2.5),"upr"] <- -2.5 
ci.terms.graph[which(ci.terms.graph$mean>2.5),"mean"] <- NA 
ci.terms.graph[ci.terms.graph$lwr>(100),"lwr"] <- 100
ci.terms.graph[ci.terms.graph$upr>(100),"upr"] <- 100 

# # Also truncating the BAI units
# ci.terms.graph[ci.terms.graph$mean.bai<(exp(-2.5)),"mean.bai"] <- NA 
# ci.terms.graph[ci.terms.graph$lwr.bai<(exp(-2.5)),"lwr.bai"] <- exp(-2.5)
# ci.terms.graph[ci.terms.graph$upr.bai<(exp(-2.5)),"upr.bai"] <- exp(-2.5) 
# ci.terms.graph[which(ci.terms.graph$mean.bai>exp(2.5)),"mean.bai"] <- NA 
# ci.terms.graph[ci.terms.graph$lwr.bai>(exp(2.5)),"lwr.bai"] <- exp(2.5) 
# ci.terms.graph[ci.terms.graph$upr.bai>(exp(2.5)),"upr.bai"] <- exp(2.5) 

cbbPalette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00")

ci.terms.graph$Site <- factor(ci.terms.graph$Site, levels = c("Missouri Ozark", "Morgan Monroe State Park", "Oak Openings Toledo", "Harvard", "Howland"))

# Limiting graph to observational range

summary(ci.terms.graph)
# DBH
for(s in unique(test$Site)){
	dbh.min <- min(test[test$Site==s, "dbh.recon"])
	dbh.max <- max(test[test$Site==s, "dbh.recon"])
	
		ci.terms.graph$x <- ifelse(ci.terms.graph$Site!=s | ci.terms.graph$Effect!="dbh.recon" | (ci.terms.graph$x>=dbh.min & ci.terms.graph$x<=dbh.max), ci.terms.graph$x, NA)
	
	}

# Temp
for(s in unique(test$Site)){
	temp.min <- min(test[test$Site==s, "tmean"])
	temp.max <- max(test[test$Site==s, "tmean"])
	
		ci.terms.graph$x <- ifelse(ci.terms.graph$Site!=s | ci.terms.graph$Effect!="tmean" | (ci.terms.graph$x>=temp.min & ci.terms.graph$x<=temp.max), ci.terms.graph$x, NA)
	
	}
	
# Precip	
for(s in unique(test$Site)){
	precip.min <- min(test[test$Site==s, "precip"])
	precip.max <- max(test[test$Site==s, "precip"])
	
		ci.terms.graph$x <- ifelse(ci.terms.graph$Site!=s | ci.terms.graph$Effect!="precip" | (ci.terms.graph$x>=precip.min & ci.terms.graph$x<=precip.max), ci.terms.graph$x, NA)
	
	}

summary(ci.terms.graph)
save(ci.terms.graph, file="processed_data/gam4_response_graph.Rdata")

pdf("figures/prelim_figures/gam4_sensitivities_observed_tmean.pdf", width= 13, height = 8.5)
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "tmean", ]) + 
			facet_grid(Site*Effect~.) +
			geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=Site), alpha=0.5) +
			geom_line(aes(x=x, y=exp(mean), color=Site))+
			geom_hline(yintercept=0, linetype="dashed") +
			scale_colour_manual("", values = cbbPalette) +
			scale_fill_manual("", values = cbbPalette) +
			theme_bw()+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
			 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        ylim(0,7)
dev.off()

pdf("figures/prelim_figures/gam4_sensitivities_observed_precip.pdf", width= 13, height = 8.5)
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "precip", ]) + 
			facet_grid(Site*Effect~.) +
			geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=Site), alpha=0.5) +
			geom_line(aes(x=x, y=exp(mean), color=Site))+
			geom_hline(yintercept=0, linetype="dashed") +
			scale_colour_manual("", values = cbbPalette) +
			scale_fill_manual("", values = cbbPalette) +
			theme_bw()+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
			 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        ylim(0,7)
dev.off()

pdf("figures/prelim_figures/gam4_sensitivities_dbh_recon_observed.pdf", width= 13, height = 8.5)
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% "dbh.recon", ]) + 
			
			geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=Site), alpha=0.5) +
			geom_line(aes(x=x, y=exp(mean), color=Site))+
			geom_hline(yintercept=0, linetype="dashed") +
			scale_colour_manual("", values = cbbPalette) +
			scale_fill_manual("", values = cbbPalette) +
			theme_bw()+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
			 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        ylim(0,15)
dev.off()

pdf("figures/prelim_figures/gam4_sensitivities_observed_combo.pdf", width= 13, height = 8.5)
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean", "precip","dbh.recon"), ]) + 
			facet_wrap(~Effect, scales="free") +
			geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=Site), alpha=0.5) +
			geom_line(aes(x=x, y=exp(mean), color=Site))+
			geom_hline(yintercept=0, linetype="dashed") +
			scale_colour_manual("", values = cbbPalette) +
			scale_fill_manual("", values = cbbPalette) +
			theme_bw()+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
			 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        ylim(0,15)
dev.off()
 
pdf("figures/prelim_figures/gam4_sensitivities_observed_combo_no_size.pdf", width= 13, height = 8.5)
		ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean", "precip"), ]) + 
			facet_wrap(~Effect, scales="free_x") +
			geom_ribbon(aes(x=x, ymin=exp(lwr), ymax=exp(upr), fill=Site), alpha=0.5) +
			geom_line(aes(x=x, y=exp(mean), color=Site))+
			geom_hline(yintercept=0, linetype="dashed") +
			scale_colour_manual("", values = cbbPalette) +
			scale_fill_manual("", values = cbbPalette) +
			theme_bw()+
			labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
			 theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
         ylim(-0.1,2.1)
dev.off()


ggplot(data=ci.terms.graph[ci.terms.graph$Effect %in% c("tmean", "precip","dbh.recon"), ]) + 
  facet_grid(~Effect, scales="free") +
  geom_ribbon(aes(x=x, ymin=lwr.bai, ymax=upr.bai, fill=Site), alpha=0.5) +
  geom_line(aes(x=x, y=mean.bai, color=Site))+
  geom_hline(yintercept=0, linetype="dashed") +
  scale_colour_manual("", values = cbbPalette) +
  scale_fill_manual("", values = cbbPalette) +
  theme_bw()+
  labs(x = "Climate Variable", y = expression(bold(paste("Effect on BAI (mm"^"2","y"^"-1",")"))))+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
  ylim(0,7)
  
  
################################################################################
# Some stats on the models
################################################################################

test$gam1.pred <- predict(gam1, newdata=test)
test$gam2.pred <- predict(gam2, newdata=test)
test$gam4.pred <- predict(gam4, newdata=test)

# Looking at the obs vs. full predicted fixed and mixed effects
meow1 <- lm(log(BA.inc)~gam1.pred, data=test)
plot(log(BA.inc)~gam1.pred, data=test)
	abline(meow1, col="red", lwd=3)

meow2 <- lm(log(BA.inc)~gam2.pred, data=test)
plot(log(BA.inc)~gam2.pred, data=test)
	abline(meow2, col="red", lwd=3)

meow4 <- lm(log(BA.inc)~gam4.pred, data=test)
plot(log(BA.inc)~gam4.pred, data=test)
	abline(meow4, col="red", lwd=3)

