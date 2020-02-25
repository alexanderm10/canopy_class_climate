################################################### 
# Copied over from 0_process_gamm.R
# This will give us the sensitivities of RW in a pretty format.

# Set up a dummy dataset for the script to run correctly
# number of simulations to run

load("processed_data/gam_results/gam3.1_NeilTest.Rdata")


source("0_Calculate_GAMM_Posteriors.R")
# Fitting our model to the data to see if we're doing a pasable job of capturing the variance
# If things don't match, we shoudl take our sensitiivty curves with a grain of salt

n <- 100
model.pred <- post.distns(model.gam=gam3.1, model.name="species_response", newdata=test2, vars=predictors.all, n=n, terms=F)
summary(model.pred$ci)

# Need help dealing with the list that is set up here.  Need help with the aggregation
model.pred2 <- model.pred$ci
summary(model.pred2)
dim(model.pred2)


# Aggregating to the group level in teh same way we do below with the ring widths
# We can then compare our modeled RW to our measured RW and see how things look
# Sanity Check #1

mean.model <- aggregate(model.pred2$mean, by = model.pred2[, c("group.cc", "Site", "Year")], FUN=mean, na.rm=T)
names(mean.model)[names(mean.model)=="x"] <- c("BAI.mean") 
mean.model[,"BAI.lwr"] <- aggregate(model.pred2$mean, by=model.pred2[,c("group.cc", "Site", "Year")], FUN=quantile, probs=0.025, na.rm=T)[,"x"]
mean.model[,"BAI.upr"] <- aggregate(model.pred2$mean, by=model.pred2[,c("group.cc", "Site", "Year")], FUN=quantile, probs=0.975, na.rm=T)[,"x"]
head(mean.model)



# aggregating the raw data for graphing
mean.rw <- aggregate(test2$BA.inc, by=test2[, c("group.cc", "Site", "Year")], FUN=mean, na.rm=T)
names(mean.rw)[names(mean.rw)=="x"]<- c("BAI.mean")                  
# mean.rw[,"rw.lwr"] <- aggregate(test$RW, by=test[,c("group", "Site", "Year")], FUN=quantile, probs=0.025, na.rm=T)[,"x"]
# mean.rw[,"rw.upr"] <- aggregate(test$RW, by=test[,c("group", "Site", "Year")], FUN=quantile, probs=0.975, na.rm=T)[,"x"]


# mean.rw[,"BAI.mean"] <- aggregate(test$BA.inc, by=test[,c("group", "Site", "Year")], FUN=mean, na.rm=T)[,"x"]
mean.rw[,"BAI.lwr"] <- aggregate(test2$BA.inc, by=test2[,c("group.cc", "Site", "Year")], FUN=quantile, probs=0.025, na.rm=T)[,"x"]
mean.rw[,"BAI.upr"] <- aggregate(test2$BA.inc, by=test2[,c("group.cc", "Site", "Year")], FUN=quantile, probs=0.975, na.rm=T)[,"x"]


head(mean.rw)

mean.rw <- mean.rw[mean.rw$Site %in% sites.use,]

#mean.rw$Species <- mean.rw$group

# Setting up lm between the modeled values and the observed	
dim(mean.rw)
dim(mean.model)

# mean.rw <- mean.rw[mean.rw$group %in% mean.model$group,]
# mean.rw <- mean.rw[mean.rw$Year %in% mean.model$Year,]

summary(mean.rw); 
summary(mean.model)

mean.model2 <- mean.model
names(mean.model2)[4:6] <- c("mod.mean", "mod.lwr", "mod.up")

sanity.gam1.df <- merge(mean.model2, mean.rw, all.x=T, all.y=T)
summary(sanity.gam1.df)
summary(sanity.gam1.df[is.na(sanity.gam1.df$mod.mean),])
head(sanity.gam1.df[is.na(sanity.gam1.df$mod.mean),])

# LM on aggregated BAI
sanity.lm1 <- lm(mod.mean ~ BAI.mean, data=sanity.gam1.df)
summary(sanity.lm1)


# Sanity Check #1 graph
pdf("figures/gam_sanity_check/gam3.1_sanitycheck1.pdf", width= 13, height = 8.5)
ggplot(data=mean.rw) + facet_wrap(group.cc ~ Site, scales="fixed") + theme_bw() +
	# plot the data
	geom_ribbon(aes(x=Year, ymin=BAI.lwr, ymax=BAI.upr), alpha=0.5) +
	geom_line(aes(x=Year, y=BAI.mean), size=1) +
	# Plot our model
	geom_ribbon(data=mean.model, aes(x=Year, ymin=BAI.lwr, ymax=BAI.upr), fill="red3", alpha=0.3) +
	geom_line(data=mean.model, aes(x=Year, y=BAI.mean), color="red3", alpha=0.8, size=1) +
	labs(title="Gamm Model vs. Data", x="Year", y="BAI")
dev.off()


# Sanity Check #2
# Pulling random trees from both the data.use and the model.pred2 to see how they compare

n <- 10
data.use2 <- data.use[data.use$Site %in% sites.use,]
data.use2 <- data.use2[data.use2$group %in% group.use,]

sanity2.trees <- sample(test2$TreeID, size=n, replace=F) 
summary(sanity2.trees)

summary(mean.rw)
summary(data.use)
summary(model.pred2)

# Sanity Check #2 graph
pdf("figures/gam_sanity_check/gam3.1_sanitycheck2.pdf", width= 13, height = 8.5)
ggplot(data=test2[test2$TreeID %in% sanity2.trees,]) + facet_wrap(TreeID~ Site, scales="fixed") + theme_bw() +
	# plot the data
	#geom_ribbon(aes(x=Year, ymin=rw.lwr, ymax=rw.upr), alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=1) +
	# Plot our model
	#geom_ribbon(data=model.pred2[model.pred2$TreeID %in% sanity2.trees,], aes(x=Year, ymin=rw.lwr, ymax=rw.upr), fill="red3", alpha=0.3) +
	geom_line(data=model.pred2[model.pred2$TreeID %in% sanity2.trees,], aes(x=Year, y=mean), color="red3", alpha=0.8, size=1) +
	labs(title="Gamm Model vs. Data Indiv. Trees", x="Year", y="RW")
dev.off()


# lm2 for sanity check 2
summary(model.pred2)
summary(data.use)
summary(test2)

model.pred2$RW <- test2$RW
model.pred2$BAI <- test2$BA.inc

# LM for indiv. trees
sanity.lm2 <- lm(BAI ~ mean, data=model.pred2)
summary(sanity.lm2)

# sanity.lm2.quru <- lm(RW ~ mean, data=model.pred2[model.pred2$group=="quru",])
# summary(sanity.lm2)


# running scripts to get the weights
source("0_Calculate_GAMM_Weights.R")


# test2 <- test[test$group %in% c("QURU", "ACRU") & test$Year>=1980,]
# test2 <- test[test$group %in% c("QURU") & test$Year>=1980,]
# summary(test2)
# gam1.test <- gamm(RW ~ s(tmean, k=3, by=group) + # tmean*Species 
#                   s(precip, k=3, by=group) +
#                   s(dbh.recon, k=3, by=group.plot) +
#                   Canopy.Class, 
#                   random=list(Site=~1, PlotID=~1),
#                   data=test2)

predictors.all
vars <- c("tmean", "precip", "dbh.recon", "Canopy.Class", "group.plot", "group", "group.cc")
gam3.1.weights <- factor.weights(model.gam = gam3.1, model.name = "species_response", newdata = test2, extent = "", vars = vars, limiting=T)

summary(gam3.1.weights)
summary(test2)
gam3.1.weights[,c("BA.inc", "group", "group.cc")] <- test2[,c("BA.inc", "group", "group.cc")] # Adding in factors we forgot


# Just the weights of tmean and Precip, ignoring size
vars2 <- c("fit.tmean", "fit.precip")
fit.spline2 <- rowSums(abs(gam3.1.weights[,vars2]), na.rm=T)
for(v in vars2){
	gam3.1.weights[,paste("weight", v, "2", sep=".")] <- gam3.1.weights[,v]/fit.spline2
}
summary(gam3.1.weights)

cols.weights <- c("weight.fit.tmean.2", "weight.fit.precip.2")
for(i in 1:nrow(gam3.1.weights)){
	fweight <- abs(gam3.1.weights[i,cols.weights])
	gam3.1.weights[i,"max2"] <- max(fweight, na.rm=T)
	gam3.1.weights[i,"factor.max2"] <- c("tmean", "precip")[which(fweight==max(fweight))]
}
gam3.1.weights$factor.max2 <- as.factor(gam3.1.weights$factor.max2)
summary(gam3.1.weights)

save(gam3.1.weights, file="processed_data/gamm_weights/gam3.1_weights.Rdata")
