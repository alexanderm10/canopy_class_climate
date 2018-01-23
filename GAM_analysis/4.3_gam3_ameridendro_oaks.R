library(ggplot2)
library(stringr)
source("poster_theme.R")


load(file="processed_data/gamm_weights/gam3_weights.Rdata")

summary(gam3.weights)
factors.fits <- c("fit.tmean", "fit.precip", "fit.dbh.recon", "fit.full", "BA.inc")
factors.weights <- c("weight.tmean", "weight.dbh.recon", "weight.precip")

# Transforming things back to BA.inc rather than log
gam3.weights[,which(substr(names(gam3.weights),1,3)=="fit")] <- exp(gam3.weights[,which(substr(names(gam3.weights),1,3)=="fit")] )




othervars <- c("Year", "Site", "group.cc", "Model")

data.graph1 <- aggregate(gam3.weights[,factors.fits], by = gam3.weights[,othervars], FUN= mean, na.rm=T)

data.graph1[,paste(factors.fits, "upr", sep=".")] <- aggregate(gam3.weights[,factors.fits], by = gam3.weights[,othervars], FUN= quantile, prob= 0.975, na.rm=T)[,factors.fits]

data.graph1[,paste(factors.fits, "lwr", sep=".")] <- aggregate(gam3.weights[,factors.fits], by = gam3.weights[,othervars], FUN= quantile, prob= 0.025, na.rm=T)[,factors.fits]

summary(data.graph1)

data.graph2 <- aggregate(abs(gam3.weights[,factors.weights]), by = gam3.weights[,othervars], FUN= mean, na.rm=T)

data.graph2[,paste(factors.weights, "upr", sep=".")] <- aggregate(abs(gam3.weights[,factors.weights]), by = gam3.weights[,othervars], FUN= quantile, prob= 0.975, na.rm=T)[,factors.weights]

data.graph2[,paste(factors.weights, "lwr", sep=".")] <- aggregate(abs(gam3.weights[,factors.weights]), by = gam3.weights[,othervars], FUN= quantile, prob= 0.025, na.rm=T)[,factors.weights]

summary(data.graph2)

data.graph <- merge(data.graph1, data.graph2, all.x=T, all.y=T)

# data.graph <- gam3.weights[gam3.weights$TreeID== "MMA014",]
summary(data.graph)
gam3.weights$wts.check <- rowSums(abs(gam3.weights[,c("weight.tmean", "weight.precip", "weight.dbh.recon")]))
data.graph$wts.check <- rowSums(abs(data.graph[,c("weight.tmean", "weight.precip", "weight.dbh.recon")]))

summary(gam3.weights)
summary(data.graph)

# Ordering the data for graphing

data.graph<- data.graph[order(data.graph$Year, data.graph$group.cc, data.graph$Site, decreasing=F),]


plot.rgb <- function(STATE, GC, SIZE){	geom_line(data=data.graph[data.graph$State==STATE & data.graph$group.cc==GC,],aes(x=Year, y=fit.full), size=SIZE,
  		        color=rgb(abs(data.graph[data.graph$State==STATE & data.graph$group.cc==GC,"weight.tmean"     ]), # red
                        abs(data.graph[data.graph$State==STATE & data.graph$group.cc==GC,"weight.dbh.recon"     ]), # green
                        abs(data.graph[data.graph$State==STATE & data.graph$group.cc==GC,"weight.precip"   ]))) }   # blue


# Plotting the Obs and modeled with influence coloring
data.graph$State <- recode(data.graph$Site, "'Howland' = 'ME';'Harvard' = 'MA';'Morgan Monroe State Park' = 'IN';'Missouri Ozark' = 'MO';'Oak Openings Toledo' = 'OH'")
data.graph$State <- factor(data.graph$State, levels=c("MO", "IN", "OH", "MA", "ME"))

for(i in unique(data.graph$group.cc)){
	data.graph[data.graph$group.cc==i,"Canopy.Class"] <- str_sub(data.graph[data.graph$group.cc==i,"group.cc"], -1)
	data.graph[data.graph$group.cc==i,"group"] <- substr(data.graph[data.graph$group.cc==i,"group.cc"], 1,4)
}
data.graph$Canopy.Class <- as.factor(data.graph$Canopy.Class)
data.graph$group <- as.factor(data.graph$group)
summary(data.graph)





########################################################################
########################################################################
# Oaks only for Ameridendro
pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_All_oaks.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[substr(data.graph$group, 1,2)=="QU",]) + facet_grid(group.cc ~ State) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="BAI") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
	# Harvard
		
	plot.rgb("MA", "QURU.C", 4) +
	plot.rgb("MA", "QURU.D", 4) +
	plot.rgb("MA", "QURU.I", 4) +
	plot.rgb("MA", "QURU.S", 4) +
	
	plot.rgb("MA", "QUVE.C", 4) +
	plot.rgb("MA", "QUVE.D", 4) +
	plot.rgb("MA", "QUVE.I", 4) +
	plot.rgb("MA", "QUVE.S", 4) +
	
		
	plot.rgb("MA", "QUAL.C", 4) +
	plot.rgb("MA", "QUAL.D", 4) +
	plot.rgb("MA", "QUAL.I", 4) +
	plot.rgb("MA", "QUAL.S", 4) +
	
	
	# Howland
		
	plot.rgb("ME", "QURU.C", 4) +
	plot.rgb("ME", "QURU.D", 4) +
	plot.rgb("ME", "QURU.I", 4) +
	plot.rgb("ME", "QURU.S", 4) +
	
	plot.rgb("ME", "QUVE.C", 4) +
	plot.rgb("ME", "QUVE.D", 4) +
	plot.rgb("ME", "QUVE.I", 4) +
	plot.rgb("ME", "QUVE.S", 4) +
		
	plot.rgb("ME", "QUAL.C", 4) +
	plot.rgb("ME", "QUAL.D", 4) +
	plot.rgb("ME", "QUAL.I", 4) +
	plot.rgb("ME", "QUAL.S", 4) +
	
		
	# Morgan Monroe
		
	plot.rgb("IN", "QURU.C", 4) +
	plot.rgb("IN", "QURU.D", 4) +
	plot.rgb("IN", "QURU.I", 4) +
	plot.rgb("IN", "QURU.S", 4) +
	
	plot.rgb("IN", "QUVE.C", 4) +
	plot.rgb("IN", "QUVE.D", 4) +
	plot.rgb("IN", "QUVE.I", 4) +
	plot.rgb("IN", "QUVE.S", 4) +
			
	plot.rgb("IN", "QUAL.C", 4) +
	plot.rgb("IN", "QUAL.D", 4) +
	plot.rgb("IN", "QUAL.I", 4) +
	plot.rgb("IN", "QUAL.S", 4) +
	
		
	# Missouri Ozark
	
		
	plot.rgb("MO", "QURU.C", 4) +
	plot.rgb("MO", "QURU.D", 4) +
	plot.rgb("MO", "QURU.I", 4) +
	plot.rgb("MO", "QURU.S", 4) +
	
	plot.rgb("MO", "QUVE.C", 4) +
	plot.rgb("MO", "QUVE.D", 4) +
	plot.rgb("MO", "QUVE.I", 4) +
	plot.rgb("MO", "QUVE.S", 4) +
	
		
	plot.rgb("MO", "QUAL.C", 4) +
	plot.rgb("MO", "QUAL.D", 4) +
	plot.rgb("MO", "QUAL.I", 4) +
	plot.rgb("MO", "QUAL.S", 4) +
	
		
	# Oak Openings
	
		
	plot.rgb("OH", "QURU.C", 4) +
	plot.rgb("OH", "QURU.D", 4) +
	plot.rgb("OH", "QURU.I", 4) +
	plot.rgb("OH", "QURU.S", 4) +
	
	plot.rgb("OH", "QUVE.C", 4) +
	plot.rgb("OH", "QUVE.D", 4) +
	plot.rgb("OH", "QUVE.I", 4) +
	plot.rgb("OH", "QUVE.S", 4) +
			
	plot.rgb("OH", "QUAL.C", 4) +
	plot.rgb("OH", "QUAL.D", 4) +
	plot.rgb("OH", "QUAL.I", 4) +
	plot.rgb("OH", "QUAL.S", 4) 
	
		
dev.off()	
	
		
# Just plotting the BAI fits
summary(data.graph)
ggplot(data.graph) + facet_wrap(group.cc~Site) +
  scale_x_continuous(expand=c(0,0), name="Year") +
  scale_y_continuous(expand=c(0,0), name="BAI") +
  # facet_wrap(~TreeID, scales="free_y", space="free") +
  # geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  geom_line(aes(x=Year, y=fit.full), size=2, alpha=0.5) +
  geom_ribbon(aes(x=Year, ymin=fit.full.lwr, ymax=fit.full.upr), alpha=0.3)

# Plotting the Effects
pdf("figures/gam3/gam3_influence_in_time_all_oaks.pdf", width= 13, height = 8.5)
ggplot(data.graph[substr(data.graph$group, 1,2)=="QU",]) + facet_grid(group.cc~State) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="Effect on RW (in mm)") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=fit.tmean), size=2, color="red") +
	geom_line(aes(x=Year, y=fit.precip), size=2, color="blue") +
	geom_line(aes(x=Year, y=fit.dbh.recon), size=2, color="green")
dev.off()



# Separating things out By canopy class to see things better
########################################################
########################################################
# Dominant
########################################################
########################################################
pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_D_oaks.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$Canopy.Class=="D" & substr(data.graph$group,1,2)=="QU",]) + facet_grid(group ~ State) +
	labs(title= "Dominant Trees", x="Year", y = expression(bold(paste("BAI (mm"^"2", "y"^"-1",")")))) +
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
	# Harvard
		
	plot.rgb("MA", "QURU.D", 4) +
	plot.rgb("MA", "QUVE.D", 4) +
	plot.rgb("MA", "QUAL.D", 4) +
	
	# Howland
	plot.rgb("ME", "QURU.D", 4) +
	plot.rgb("ME", "QUVE.D", 4) +
	plot.rgb("ME", "QUAL.D", 4) +
	
	# Morgan Monroe
	plot.rgb("IN", "QURU.D", 4) +
	plot.rgb("IN", "QUVE.D", 4) +
	plot.rgb("IN", "QUAL.D", 4) +
	
	# Missouri Ozark
	
	plot.rgb("MO", "QURU.D", 4) +
	plot.rgb("MO", "QUVE.D", 4) +
	plot.rgb("MO", "QUAL.D", 4) +
	
	# Oak Openings
	
	plot.rgb("OH", "QURU.D", 4) +
	plot.rgb("OH", "QUVE.D", 4) +
	plot.rgb("OH", "QUAL.D", 4) +
	poster.theme2 
	
dev.off()	
	
pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_D_oaks_MO.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$Canopy.Class=="D" & substr(data.graph$group,1,2)=="QU" & data.graph$State == "MO",]) + facet_grid(group ~ State) +
	labs(title= "Dominant Trees", x="Year", y = expression(bold(paste("BAI (mm"^"2", "y"^"-1",")")))) +
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
	# Harvard
		
	# plot.rgb("MA", "QURU.D", 4) +
	# plot.rgb("MA", "QUVE.D", 4) +
	# plot.rgb("MA", "QUAL.D", 4) +
	
	# # Howland
	# plot.rgb("ME", "QURU.D", 4) +
	# plot.rgb("ME", "QUVE.D", 4) +
	# plot.rgb("ME", "QUAL.D", 4) +
	
	# # Morgan Monroe
	# plot.rgb("IN", "QURU.D", 4) +
	# plot.rgb("IN", "QUVE.D", 4) +
	# plot.rgb("IN", "QUAL.D", 4) +
	
	# Missouri Ozark
	
	plot.rgb("MO", "QURU.D", 4) +
	plot.rgb("MO", "QUVE.D", 4) +
	plot.rgb("MO", "QUAL.D", 4) +
	
	# # Oak Openings
	
	# plot.rgb("OH", "QURU.D", 4) +
	# plot.rgb("OH", "QUVE.D", 4) +
	# plot.rgb("OH", "QUAL.D", 4) +
	poster.theme2 
	
dev.off()	
	
	
		
# Just plotting the BAI fits
summary(data.graph)
ggplot(data.graph) + facet_wrap(group.cc~Site) +
  scale_x_continuous(expand=c(0,0), name="Year") +
  scale_y_continuous(expand=c(0,0), name="BAI") +
  # facet_wrap(~TreeID, scales="free_y", space="free") +
  # geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  geom_line(aes(x=Year, y=fit.full), size=2, alpha=0.5) +
  geom_ribbon(aes(x=Year, ymin=fit.full.lwr, ymax=fit.full.upr), alpha=0.3)

# Plotting the Effects
pdf("figures/gam3/gam3_influence_in_time_D_oaks.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$Canopy.Class=="D" & substr(data.graph$group,1,2)=="QU",]) + facet_grid(group~State) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name=expression(bold(paste("Effect on BAI (mm"^"2", "y"^"-1",")")))) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=fit.tmean), size=2, color="red") +
	geom_line(aes(x=Year, y=fit.precip), size=2, color="blue") +
	geom_line(aes(x=Year, y=fit.dbh.recon), size=2, color="green")+
	labs(title= "Dominant Effects")+
	poster.theme2
dev.off()

################################################################
################################################################
# Intermediate Trees
################################################################
################################################################

pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_I_oaks.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$Canopy.Class=="I" & substr(data.graph$group,1,2)=="QU",]) + facet_grid(group ~ State) +
	labs(title= "Intermediate Trees", x="Year", y = expression(bold(paste("BAI (mm"^"2", "y"^"-1",")")))) +

	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
	# Harvard
		
	plot.rgb("MA", "QURU.I", 3) +
	plot.rgb("MA", "QUVE.I", 3) +
	plot.rgb("MA", "QUAL.I", 3) +
	
	# Howland
	plot.rgb("ME", "QURU.I", 3) +
	plot.rgb("ME", "QUVE.I", 3) +
	plot.rgb("ME", "QUAL.I", 3) +
	
	# Morgan Monroe
	plot.rgb("IN", "QURU.I", 3) +
	plot.rgb("IN", "QUVE.I", 3) +
	plot.rgb("IN", "QUAL.I", 3) +
	
	# Missouri Ozark
	
	plot.rgb("MO", "QURU.I", 3) +
	plot.rgb("MO", "QUVE.I", 3) +
	plot.rgb("MO", "QUAL.I", 3) +
	
	# Oak Openings
	
	plot.rgb("OH", "QURU.I", 3) +
	plot.rgb("OH", "QUVE.I", 3) +
	plot.rgb("OH", "QUAL.I", 3) +
	poster.theme2 
	
	
dev.off()	
	
		
# Just plotting the BAI fits
summary(data.graph)
ggplot(data.graph) + facet_wrap(group.cc~Site) +
  scale_x_continuous(expand=c(0,0), name="Year") +
  scale_y_continuous(expand=c(0,0), name="BAI") +
  # facet_wrap(~TreeID, scales="free_y", space="free") +
  # geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  geom_line(aes(x=Year, y=fit.full), size=2, alpha=0.5) +
  geom_ribbon(aes(x=Year, ymin=fit.full.lwr, ymax=fit.full.upr), alpha=0.3)

# Plotting the Effects
pdf("figures/gam3/gam3_influence_in_time_I_oaks.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$Canopy.Class=="I" & substr(data.graph$group,1,2)=="QU",]) + facet_grid(group~State) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name=expression(bold(paste("Effect on BAI (mm"^"2", "y"^"-1",")")))) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=fit.tmean), size=2, color="red") +
	geom_line(aes(x=Year, y=fit.precip), size=2, color="blue") +
	geom_line(aes(x=Year, y=fit.dbh.recon), size=2, color="green")+
	labs(title= "Intermediate Effects")+
	poster.theme2
dev.off()


################################################################
################################################################
# Suppressed Trees
################################################################
################################################################

pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_S_oaks.pdf", width= 13, height = 8.5)
ggplot(data =data.graph[data.graph$Canopy.Class=="S" & substr(data.graph$group,1,2)=="QU",]) + facet_grid(group ~ State)+
	labs(title= "Suppressed Trees", x="Year", y = expression(bold(paste("BAI (mm"^"2", "y"^"-1",")"))))+
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
	# Harvard
		
	plot.rgb("MA", "QURU.S", 3) +
	plot.rgb("MA", "QUVE.S", 3) +
	plot.rgb("MA", "QUAL.S", 3) +
	
	# Howland
	plot.rgb("ME", "QURU.S", 3) +
	plot.rgb("ME", "QUVE.S", 3) +
	plot.rgb("ME", "QUAL.S", 3) +
	
	# Morgan Monroe
	plot.rgb("IN", "QURU.S", 3) +
	plot.rgb("IN", "QUVE.S", 3) +
	plot.rgb("IN", "QUAL.S", 3) +
	
	# Missouri Ozark
	
	plot.rgb("MO", "QURU.S", 3) +
	plot.rgb("MO", "QUVE.S", 3) +
	plot.rgb("MO", "QUAL.S", 3) +
	
	# Oak Openings
	
	plot.rgb("OH", "QURU.S", 3) +
	plot.rgb("OH", "QUVE.S", 3) +
	plot.rgb("OH", "QUAL.S", 3) +
	poster.theme2
	

	
dev.off()	
	
		
# Just plotting the BAI fits
summary(data.graph)
ggplot(data.graph) + facet_wrap(group.cc~Site) +
  scale_x_continuous(expand=c(0,0), name="Year") +
  scale_y_continuous(expand=c(0,0), name="BAI") +
  # facet_wrap(~TreeID, scales="free_y", space="free") +
  # geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  geom_line(aes(x=Year, y=fit.full), size=2, alpha=0.5) +
  geom_ribbon(aes(x=Year, ymin=fit.full.lwr, ymax=fit.full.upr), alpha=0.3)

# Plotting the Effects
pdf("figures/gam3/gam3_influence_in_time_S_oaks.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$Canopy.Class=="S" & substr(data.graph$group,1,2)=="QU",]) + facet_grid(group~State) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name=expression(bold(paste("Effect on BAI (mm"^"2", "y"^"-1",")")))) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=fit.tmean), size=2, color="red") +
	geom_line(aes(x=Year, y=fit.precip), size=2, color="blue") +
	geom_line(aes(x=Year, y=fit.dbh.recon), size=2, color="green")+
	labs(title= "Suppressed Effects")+
	poster.theme2
dev.off()


