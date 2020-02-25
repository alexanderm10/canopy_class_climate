library(ggplot2)
library(stringr)



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


plot.rgb <- function(STATE, GC, SIZE){	geom_point(data=data.graph[data.graph$State==STATE & data.graph$group.cc==GC,],aes(x=Year, y=fit.full), size=SIZE,
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


# Plotting the Obs and modeled with influence coloring
pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_All.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[!data.graph$group.cc %in% c("BETULA.D", "CARYA.D", "FAGR.D", "FRAX.D", "SAAL.D",
	"BETULA.I", "CARYA.I", "FAGR.I", "FRAX.I", "SAAL.I",
	"BETULA.S", "CARYA.S", "FAGR.S", "FRAX.S", "SAAL.S"),]) + 
	facet_grid(group.cc ~ State) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="BAI") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
	# Harvard
	plot.rgb("MA", "ACRU.C", 3) +
	plot.rgb("MA", "ACRU.D", 3) +
	plot.rgb("MA", "ACRU.I", 3) +
	plot.rgb("MA", "ACRU.S", 3) +
	
	plot.rgb("MA", "PIST.C", 3) +
	plot.rgb("MA", "PIST.D", 3) +
	plot.rgb("MA", "PIST.I", 3) +
	plot.rgb("MA", "PIST.S", 3) +

	plot.rgb("MA", "TSCA.C", 3) +
	plot.rgb("MA", "TSCA.D", 3) +
	plot.rgb("MA", "TSCA.I", 3) +
	plot.rgb("MA", "TSCA.S", 3) +
	
	plot.rgb("MA", "QURU.C", 3) +
	plot.rgb("MA", "QURU.D", 3) +
	plot.rgb("MA", "QURU.I", 3) +
	plot.rgb("MA", "QURU.S", 3) +
	
	plot.rgb("MA", "QUVE.C", 3) +
	plot.rgb("MA", "QUVE.D", 3) +
	plot.rgb("MA", "QUVE.I", 3) +
	plot.rgb("MA", "QUVE.S", 3) +
	
	# plot.rgb("MA", "FRAX.C", 3) +
	# plot.rgb("MA", "FRAX.D", 3) +
	# plot.rgb("MA", "FRAX.I", 3) +
	# plot.rgb("MA", "FRAX.S", 3) +

	# plot.rgb("MA", "BETULA.C", 3) +
	# plot.rgb("MA", "BETULA.D", 3) +
	# plot.rgb("MA", "BETULA.I", 3) +
	# plot.rgb("MA", "BETULA.S", 3) +
	
	plot.rgb("MA", "ACSA.C", 3) +
	plot.rgb("MA", "ACSA.D", 3) +
	plot.rgb("MA", "ACSA.I", 3) +
	plot.rgb("MA", "ACSA.S", 3) +
	
	plot.rgb("MA", "QUAL.C", 3) +
	plot.rgb("MA", "QUAL.D", 3) +
	plot.rgb("MA", "QUAL.I", 3) +
	plot.rgb("MA", "QUAL.S", 3) +
	
	# plot.rgb("MA", "FAGR.C", 3) +
	# plot.rgb("MA", "FAGR.D", 3) +
	# plot.rgb("MA", "FAGR.I", 3) +
	# plot.rgb("MA", "FAGR.S", 3) +
	
	plot.rgb("MA", "ULRU.C", 3) +
	plot.rgb("MA", "ULRU.D", 3) +
	plot.rgb("MA", "ULRU.I", 3) +
	plot.rgb("MA", "ULRU.S", 3) +
	
	# plot.rgb("MA", "CARYA.C", 3) +
	# plot.rgb("MA", "CARYA.D", 3) +
	# plot.rgb("MA", "CARYA.I", 3) +
	# plot.rgb("MA", "CARYA.S", 3) +

	# plot.rgb("MA", "SAAL.C", 3) +
	# plot.rgb("MA", "SAAL.D", 3) +
	# plot.rgb("MA", "SAAL.I", 3) +
	# plot.rgb("MA", "SAAL.S", 3) +

	# Howland
		plot.rgb("ME", "ACRU.C", 3) +
	plot.rgb("ME", "ACRU.D", 3) +
	plot.rgb("ME", "ACRU.I", 3) +
	plot.rgb("ME", "ACRU.S", 3) +
	
	plot.rgb("ME", "PIST.C", 3) +
	plot.rgb("ME", "PIST.D", 3) +
	plot.rgb("ME", "PIST.I", 3) +
	plot.rgb("ME", "PIST.S", 3) +

	plot.rgb("ME", "TSCA.C", 3) +
	plot.rgb("ME", "TSCA.D", 3) +
	plot.rgb("ME", "TSCA.I", 3) +
	plot.rgb("ME", "TSCA.S", 3) +
	
	plot.rgb("ME", "QURU.C", 3) +
	plot.rgb("ME", "QURU.D", 3) +
	plot.rgb("ME", "QURU.I", 3) +
	plot.rgb("ME", "QURU.S", 3) +
	
	plot.rgb("ME", "QUVE.C", 3) +
	plot.rgb("ME", "QUVE.D", 3) +
	plot.rgb("ME", "QUVE.I", 3) +
	plot.rgb("ME", "QUVE.S", 3) +
	
	# plot.rgb("ME", "FRAX.C", 3) +
	# plot.rgb("ME", "FRAX.D", 3) +
	# plot.rgb("ME", "FRAX.I", 3) +
	# plot.rgb("ME", "FRAX.S", 3) +

	# plot.rgb("ME", "BETULA.C", 3) +
	# plot.rgb("ME", "BETULA.D", 3) +
	# plot.rgb("ME", "BETULA.I", 3) +
	# plot.rgb("ME", "BETULA.S", 3) +
	
	plot.rgb("ME", "ACSA.C", 3) +
	plot.rgb("ME", "ACSA.D", 3) +
	plot.rgb("ME", "ACSA.I", 3) +
	plot.rgb("ME", "ACSA.S", 3) +
	
	plot.rgb("ME", "QUAL.C", 3) +
	plot.rgb("ME", "QUAL.D", 3) +
	plot.rgb("ME", "QUAL.I", 3) +
	plot.rgb("ME", "QUAL.S", 3) +
	
	# plot.rgb("ME", "FAGR.C", 3) +
	# plot.rgb("ME", "FAGR.D", 3) +
	# plot.rgb("ME", "FAGR.I", 3) +
	# plot.rgb("ME", "FAGR.S", 3) +
	
	plot.rgb("ME", "ULRU.C", 3) +
	plot.rgb("ME", "ULRU.D", 3) +
	plot.rgb("ME", "ULRU.I", 3) +
	plot.rgb("ME", "ULRU.S", 3) +
	
	# plot.rgb("ME", "CARYA.C", 3) +
	# plot.rgb("ME", "CARYA.D", 3) +
	# plot.rgb("ME", "CARYA.I", 3) +
	# plot.rgb("ME", "CARYA.S", 3) +

	# plot.rgb("ME", "SAAL.C", 3) +
	# plot.rgb("ME", "SAAL.D", 3) +
	# plot.rgb("ME", "SAAL.I", 3) +
	# plot.rgb("ME", "SAAL.S", 3) +
	
	
	# Morgan Monroe
	plot.rgb("IN", "ACRU.C", 3) +
	plot.rgb("IN", "ACRU.D", 3) +
	plot.rgb("IN", "ACRU.I", 3) +
	plot.rgb("IN", "ACRU.S", 3) +
	
	plot.rgb("IN", "PIST.C", 3) +
	plot.rgb("IN", "PIST.D", 3) +
	plot.rgb("IN", "PIST.I", 3) +
	plot.rgb("IN", "PIST.S", 3) +

	plot.rgb("IN", "TSCA.C", 3) +
	plot.rgb("IN", "TSCA.D", 3) +
	plot.rgb("IN", "TSCA.I", 3) +
	plot.rgb("IN", "TSCA.S", 3) +
	
	plot.rgb("IN", "QURU.C", 3) +
	plot.rgb("IN", "QURU.D", 3) +
	plot.rgb("IN", "QURU.I", 3) +
	plot.rgb("IN", "QURU.S", 3) +
	
	plot.rgb("IN", "QUVE.C", 3) +
	plot.rgb("IN", "QUVE.D", 3) +
	plot.rgb("IN", "QUVE.I", 3) +
	plot.rgb("IN", "QUVE.S", 3) +
	
	# plot.rgb("IN", "FRAX.C", 3) +
	# plot.rgb("IN", "FRAX.D", 3) +
	# plot.rgb("IN", "FRAX.I", 3) +
	# plot.rgb("IN", "FRAX.S", 3) +

	# plot.rgb("IN", "BETULA.C", 3) +
	# plot.rgb("IN", "BETULA.D", 3) +
	# plot.rgb("IN", "BETULA.I", 3) +
	# plot.rgb("IN", "BETULA.S", 3) +
	
	plot.rgb("IN", "ACSA.C", 3) +
	plot.rgb("IN", "ACSA.D", 3) +
	plot.rgb("IN", "ACSA.I", 3) +
	plot.rgb("IN", "ACSA.S", 3) +
	
	plot.rgb("IN", "QUAL.C", 3) +
	plot.rgb("IN", "QUAL.D", 3) +
	plot.rgb("IN", "QUAL.I", 3) +
	plot.rgb("IN", "QUAL.S", 3) +
	
	# plot.rgb("IN", "FAGR.C", 3) +
	# plot.rgb("IN", "FAGR.D", 3) +
	# plot.rgb("IN", "FAGR.I", 3) +
	# plot.rgb("IN", "FAGR.S", 3) +
	
	plot.rgb("IN", "ULRU.C", 3) +
	plot.rgb("IN", "ULRU.D", 3) +
	plot.rgb("IN", "ULRU.I", 3) +
	plot.rgb("IN", "ULRU.S", 3) +
	
	# plot.rgb("IN", "CARYA.C", 3) +
	# plot.rgb("IN", "CARYA.D", 3) +
	# plot.rgb("IN", "CARYA.I", 3) +
	# plot.rgb("IN", "CARYA.S", 3) +

	# plot.rgb("IN", "SAAL.C", 3) +
	# plot.rgb("IN", "SAAL.D", 3) +
	# plot.rgb("IN", "SAAL.I", 3) +
	# plot.rgb("IN", "SAAL.S", 3) +
	
	# Missouri Ozark
	
		plot.rgb("MO", "ACRU.C", 3) +
	plot.rgb("MO", "ACRU.D", 3) +
	plot.rgb("MO", "ACRU.I", 3) +
	plot.rgb("MO", "ACRU.S", 3) +
	
	plot.rgb("MO", "PIST.C", 3) +
	plot.rgb("MO", "PIST.D", 3) +
	plot.rgb("MO", "PIST.I", 3) +
	plot.rgb("MO", "PIST.S", 3) +

	plot.rgb("MO", "TSCA.C", 3) +
	plot.rgb("MO", "TSCA.D", 3) +
	plot.rgb("MO", "TSCA.I", 3) +
	plot.rgb("MO", "TSCA.S", 3) +
	
	plot.rgb("MO", "QURU.C", 3) +
	plot.rgb("MO", "QURU.D", 3) +
	plot.rgb("MO", "QURU.I", 3) +
	plot.rgb("MO", "QURU.S", 3) +
	
	plot.rgb("MO", "QUVE.C", 3) +
	plot.rgb("MO", "QUVE.D", 3) +
	plot.rgb("MO", "QUVE.I", 3) +
	plot.rgb("MO", "QUVE.S", 3) +
	
	# plot.rgb("MO", "FRAX.C", 3) +
	# plot.rgb("MO", "FRAX.D", 3) +
	# plot.rgb("MO", "FRAX.I", 3) +
	# plot.rgb("MO", "FRAX.S", 3) +

	# plot.rgb("MO", "BETULA.C", 3) +
	# plot.rgb("MO", "BETULA.D", 3) +
	# plot.rgb("MO", "BETULA.I", 3) +
	# plot.rgb("MO", "BETULA.S", 3) +
	
	plot.rgb("MO", "ACSA.C", 3) +
	plot.rgb("MO", "ACSA.D", 3) +
	plot.rgb("MO", "ACSA.I", 3) +
	plot.rgb("MO", "ACSA.S", 3) +
	
	plot.rgb("MO", "QUAL.C", 3) +
	plot.rgb("MO", "QUAL.D", 3) +
	plot.rgb("MO", "QUAL.I", 3) +
	plot.rgb("MO", "QUAL.S", 3) +
	
	# plot.rgb("MO", "FAGR.C", 3) +
	# plot.rgb("MO", "FAGR.D", 3) +
	# plot.rgb("MO", "FAGR.I", 3) +
	# plot.rgb("MO", "FAGR.S", 3) +
	
	plot.rgb("MO", "ULRU.C", 3) +
	plot.rgb("MO", "ULRU.D", 3) +
	plot.rgb("MO", "ULRU.I", 3) +
	plot.rgb("MO", "ULRU.S", 3) +
	
	# plot.rgb("MO", "CARYA.C", 3) +
	# plot.rgb("MO", "CARYA.D", 3) +
	# plot.rgb("MO", "CARYA.I", 3) +
	# plot.rgb("MO", "CARYA.S", 3) +

	# plot.rgb("MO", "SAAL.C", 3) +
	# plot.rgb("MO", "SAAL.D", 3) +
	# plot.rgb("MO", "SAAL.I", 3) +
	# plot.rgb("MO", "SAAL.S", 3) +
	
	# Oak Openings
	
			plot.rgb("OH", "ACRU.C", 3) +
	plot.rgb("OH", "ACRU.D", 3) +
	plot.rgb("OH", "ACRU.I", 3) +
	plot.rgb("OH", "ACRU.S", 3) +
	
	plot.rgb("OH", "PIST.C", 3) +
	plot.rgb("OH", "PIST.D", 3) +
	plot.rgb("OH", "PIST.I", 3) +
	plot.rgb("OH", "PIST.S", 3) +

	plot.rgb("OH", "TSCA.C", 3) +
	plot.rgb("OH", "TSCA.D", 3) +
	plot.rgb("OH", "TSCA.I", 3) +
	plot.rgb("OH", "TSCA.S", 3) +
	
	plot.rgb("OH", "QURU.C", 3) +
	plot.rgb("OH", "QURU.D", 3) +
	plot.rgb("OH", "QURU.I", 3) +
	plot.rgb("OH", "QURU.S", 3) +
	
	plot.rgb("OH", "QUVE.C", 3) +
	plot.rgb("OH", "QUVE.D", 3) +
	plot.rgb("OH", "QUVE.I", 3) +
	plot.rgb("OH", "QUVE.S", 3) +
	
	# plot.rgb("OH", "FRAX.C", 3) +
	# plot.rgb("OH", "FRAX.D", 3) +
	# plot.rgb("OH", "FRAX.I", 3) +
	# plot.rgb("OH", "FRAX.S", 3) +

	# plot.rgb("OH", "BETULA.C", 3) +
	# plot.rgb("OH", "BETULA.D", 3) +
	# plot.rgb("OH", "BETULA.I", 3) +
	# plot.rgb("OH", "BETULA.S", 3) +
	
	plot.rgb("OH", "ACSA.C", 3) +
	plot.rgb("OH", "ACSA.D", 3) +
	plot.rgb("OH", "ACSA.I", 3) +
	plot.rgb("OH", "ACSA.S", 3) +
	
	plot.rgb("OH", "QUAL.C", 3) +
	plot.rgb("OH", "QUAL.D", 3) +
	plot.rgb("OH", "QUAL.I", 3) +
	plot.rgb("OH", "QUAL.S", 3) +
	
	# plot.rgb("OH", "FAGR.C", 3) +
	# plot.rgb("OH", "FAGR.D", 3) +
	# plot.rgb("OH", "FAGR.I", 3) +
	# plot.rgb("OH", "FAGR.S", 3) +
	
	plot.rgb("OH", "ULRU.C", 3) +
	plot.rgb("OH", "ULRU.D", 3) +
	plot.rgb("OH", "ULRU.I", 3) +
	plot.rgb("OH", "ULRU.S", 3) #+
	
	# plot.rgb("OH", "CARYA.C", 3) +
	# plot.rgb("OH", "CARYA.D", 3) +
	# plot.rgb("OH", "CARYA.I", 3) +
	# plot.rgb("OH", "CARYA.S", 3) +

	# plot.rgb("OH", "SAAL.C", 3) +
	# plot.rgb("OH", "SAAL.D", 3) +
	# plot.rgb("OH", "SAAL.I", 3) +
	# plot.rgb("OH", "SAAL.S", 3)
	
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
pdf("figures/gam3/gam3_influence_in_time_all.pdf", width= 13, height = 8.5)
ggplot(data.graph) + facet_grid(group.cc~State) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="Effect on RW (in mm)") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=fit.tmean), size=1, color="red") +
	geom_line(aes(x=Year, y=fit.precip), size=1, color="blue") +
	geom_line(aes(x=Year, y=fit.dbh.recon), size=1, color="green")
dev.off()


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
		
	plot.rgb("MA", "QURU.C", 3) +
	plot.rgb("MA", "QURU.D", 3) +
	plot.rgb("MA", "QURU.I", 3) +
	plot.rgb("MA", "QURU.S", 3) +
	
	plot.rgb("MA", "QUVE.C", 3) +
	plot.rgb("MA", "QUVE.D", 3) +
	plot.rgb("MA", "QUVE.I", 3) +
	plot.rgb("MA", "QUVE.S", 3) +
	
		
	plot.rgb("MA", "QUAL.C", 3) +
	plot.rgb("MA", "QUAL.D", 3) +
	plot.rgb("MA", "QUAL.I", 3) +
	plot.rgb("MA", "QUAL.S", 3) +
	
	
	# Howland
		
	plot.rgb("ME", "QURU.C", 3) +
	plot.rgb("ME", "QURU.D", 3) +
	plot.rgb("ME", "QURU.I", 3) +
	plot.rgb("ME", "QURU.S", 3) +
	
	plot.rgb("ME", "QUVE.C", 3) +
	plot.rgb("ME", "QUVE.D", 3) +
	plot.rgb("ME", "QUVE.I", 3) +
	plot.rgb("ME", "QUVE.S", 3) +
		
	plot.rgb("ME", "QUAL.C", 3) +
	plot.rgb("ME", "QUAL.D", 3) +
	plot.rgb("ME", "QUAL.I", 3) +
	plot.rgb("ME", "QUAL.S", 3) +
	
		
	# Morgan Monroe
		
	plot.rgb("IN", "QURU.C", 3) +
	plot.rgb("IN", "QURU.D", 3) +
	plot.rgb("IN", "QURU.I", 3) +
	plot.rgb("IN", "QURU.S", 3) +
	
	plot.rgb("IN", "QUVE.C", 3) +
	plot.rgb("IN", "QUVE.D", 3) +
	plot.rgb("IN", "QUVE.I", 3) +
	plot.rgb("IN", "QUVE.S", 3) +
			
	plot.rgb("IN", "QUAL.C", 3) +
	plot.rgb("IN", "QUAL.D", 3) +
	plot.rgb("IN", "QUAL.I", 3) +
	plot.rgb("IN", "QUAL.S", 3) +
	
		
	# Missouri Ozark
	
		
	plot.rgb("MO", "QURU.C", 3) +
	plot.rgb("MO", "QURU.D", 3) +
	plot.rgb("MO", "QURU.I", 3) +
	plot.rgb("MO", "QURU.S", 3) +
	
	plot.rgb("MO", "QUVE.C", 3) +
	plot.rgb("MO", "QUVE.D", 3) +
	plot.rgb("MO", "QUVE.I", 3) +
	plot.rgb("MO", "QUVE.S", 3) +
	
		
	plot.rgb("MO", "QUAL.C", 3) +
	plot.rgb("MO", "QUAL.D", 3) +
	plot.rgb("MO", "QUAL.I", 3) +
	plot.rgb("MO", "QUAL.S", 3) +
	
		
	# Oak Openings
	
		
	plot.rgb("OH", "QURU.C", 3) +
	plot.rgb("OH", "QURU.D", 3) +
	plot.rgb("OH", "QURU.I", 3) +
	plot.rgb("OH", "QURU.S", 3) +
	
	plot.rgb("OH", "QUVE.C", 3) +
	plot.rgb("OH", "QUVE.D", 3) +
	plot.rgb("OH", "QUVE.I", 3) +
	plot.rgb("OH", "QUVE.S", 3) +
			
	plot.rgb("OH", "QUAL.C", 3) +
	plot.rgb("OH", "QUAL.D", 3) +
	plot.rgb("OH", "QUAL.I", 3) +
	plot.rgb("OH", "QUAL.S", 3) 
	
		
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
pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_D.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$Canopy.Class=="D" & !data.graph$group.cc %in% c("BETULA.D", "CARYA.D", "FAGR.D", "FRAX.D", "SAAL.D", "ULRU.D", "ACSA.D"),]) + facet_grid(group ~ State) +
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
	# Harvard
	
	plot.rgb("MA", "ACRU.D", 3) +
	plot.rgb("MA", "PIST.D", 3) +
	plot.rgb("MA", "TSCA.D", 3) +
	plot.rgb("MA", "QURU.D", 3) +
	plot.rgb("MA", "QUVE.D", 3) +
	# plot.rgb("MA", "FRAX.D", 3) +
	# plot.rgb("MA", "BETULA.D", 3) +
	#plot.rgb("MA", "ACSA.D", 3) +
	plot.rgb("MA", "QUAL.D", 3) +
	# plot.rgb("MA", "FAGR.D", 3) +
	#plot.rgb("MA", "ULRU.D", 3) +
	# plot.rgb("MA", "CARYA.D", 3) +
	# plot.rgb("MA", "SAAL.D", 3) +
	
	# Howland
	plot.rgb("ME", "ACRU.D", 3) +
	plot.rgb("ME", "PIST.D", 3) +
	plot.rgb("ME", "TSCA.D", 3) +
	plot.rgb("ME", "QURU.D", 3) +
	plot.rgb("ME", "QUVE.D", 3) +
	# plot.rgb("ME", "FRAX.D", 3) +
	# plot.rgb("ME", "BETULA.D", 3) +
	#plot.rgb("ME", "ACSA.D", 3) +
	plot.rgb("ME", "QUAL.D", 3) +
	# plot.rgb("ME", "FAGR.D", 3) +
	#plot.rgb("ME", "ULRU.D", 3) +
	# plot.rgb("ME", "CARYA.D", 3) +
	# plot.rgb("ME", "SAAL.D", 3) +
	
	
	# Morgan Monroe
	plot.rgb("IN", "ACRU.D", 3) +
	plot.rgb("IN", "PIST.D", 3) +
	plot.rgb("IN", "TSCA.D", 3) +
	plot.rgb("IN", "QURU.D", 3) +
	plot.rgb("IN", "QUVE.D", 3) +
	# plot.rgb("IN", "FRAX.D", 3) +
	# plot.rgb("IN", "BETULA.D", 3) +
	#plot.rgb("IN", "ACSA.D", 3) +
	plot.rgb("IN", "QUAL.D", 3) +
	# plot.rgb("IN", "FAGR.D", 3) +
	#plot.rgb("IN", "ULRU.D", 3) +
	# plot.rgb("IN", "CARYA.D", 3) +
	# plot.rgb("IN", "SAAL.D", 3) +
	
	# Missouri Ozark
	
	plot.rgb("MO", "ACRU.D", 3) +
	plot.rgb("MO", "PIST.D", 3) +
	plot.rgb("MO", "TSCA.D", 3) +
	plot.rgb("MO", "QURU.D", 3) +
	plot.rgb("MO", "QUVE.D", 3) +
	# plot.rgb("MO", "FRAX.D", 3) +
	# plot.rgb("MO", "BETULA.D", 3) +
	#plot.rgb("MO", "ACSA.D", 3) +
	plot.rgb("MO", "QUAL.D", 3) +
	# plot.rgb("MO", "FAGR.D", 3) +
	#plot.rgb("MO", "ULRU.D", 3) +
	# plot.rgb("MO", "CARYA.D", 3) +
	# plot.rgb("MO", "SAAL.D", 3) +
	
	# Oak Openings
	
	plot.rgb("OH", "ACRU.D", 3) +
	plot.rgb("OH", "PIST.D", 3) +
	plot.rgb("OH", "TSCA.D", 3) +
	plot.rgb("OH", "QURU.D", 3) +
	plot.rgb("OH", "QUVE.D", 3) +
	# plot.rgb("OH", "FRAX.D", 3) +
	# plot.rgb("OH", "BETULA.D", 3) +
	#plot.rgb("OH", "ACSA.D", 3) +
	plot.rgb("OH", "QUAL.D", 3) +
	# plot.rgb("OH", "FAGR.D", 3) +
	#plot.rgb("OH", "ULRU.D", 3) +
	# plot.rgb("OH", "CARYA.D", 3) +
	# plot.rgb("OH", "SAAL.D", 3) +
	poster.theme2 +
	labs(title= "Dominant Trees", x="Year", y = expression(bold(paste("BAI (mm"^"2", "y"^"-1",")"))))
	
dev.off()	

data.graph$State <- factor(data.graph$State, levels=c("MO", "IN", "OH", "MA", "ME"))	
		
# Just plotting the BAI fits
summary(data.graph)
ggplot(data.graph[data.graph$Canopy.Class=="D" & !data.graph$group.cc %in% c("BETULA.D", "CARYA.D", "FAGR.D", "FRAX.D", "SAAL.D"),]) + facet_grid(group.cc~State) +
  scale_x_continuous(expand=c(0,0), name="Year") +
  scale_y_continuous(expand=c(0,0), name="BAI") +
  # facet_wrap(~TreeID, scales="free_y", space="free") +
  # geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  geom_line(aes(x=Year, y=fit.full), size=2, alpha=0.5) +
  geom_ribbon(aes(x=Year, ymin=fit.full.lwr, ymax=fit.full.upr), alpha=0.3)

# Plotting the Effects
pdf("figures/gam3/gam3_influence_in_time_D.pdf", width= 13, height = 8.5)
ggplot(data.graph[data.graph$Canopy.Class=="D" & !data.graph$group.cc %in% c("BETULA.D", "CARYA.D", "FAGR.D", "FRAX.D", "SAAL.D"),]) + facet_grid(group~State) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="Effect on RW (in mm)") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	
	#geom_ribbon(aes(x=Year, ymin=fit.dbh.recon.lwr, ymax=fit.dbh.recon.upr), alpha=0.4, fill="green")+
	
	geom_line(aes(x=Year, y=fit.tmean), size=1, color="red") +
	geom_line(aes(x=Year, y=fit.precip), size=1, color="blue") +
	geom_line(aes(x=Year, y=fit.dbh.recon), size=1, color="green")+
	ylim(c(0,3)) +
	labs(title= "Dominant Effects (group.cc)")
dev.off()

################################################################
################################################################
# Intermediate Trees
################################################################
################################################################

pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_I.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$Canopy.Class=="I" & !data.graph$group.cc %in% c("BETULA.I", "CARYA.I", "FAGR.I", "FRAX.I", "SAAL.I"),]) + facet_grid(group ~ State) +
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
	# Harvard
	
	plot.rgb("MA", "ACRU.I", 3) +
	plot.rgb("MA", "PIST.I", 3) +
	plot.rgb("MA", "TSCA.I", 3) +
	plot.rgb("MA", "QURU.I", 3) +
	plot.rgb("MA", "QUVE.I", 3) +
	# plot.rgb("MA", "FRAX.I", 3) +
	# plot.rgb("MA", "BETULA.I", 3) +
	plot.rgb("MA", "ACSA.I", 3) +
	plot.rgb("MA", "QUAL.I", 3) +
	# plot.rgb("MA", "FAGR.I", 3) +
	plot.rgb("MA", "ULRU.I", 3) +
	# plot.rgb("MA", "CARYA.I", 3) +
	# plot.rgb("MA", "SAAL.I", 3) +
	
	# Howland
	plot.rgb("ME", "ACRU.I", 3) +
	plot.rgb("ME", "PIST.I", 3) +
	plot.rgb("ME", "TSCA.I", 3) +
	plot.rgb("ME", "QURU.I", 3) +
	plot.rgb("ME", "QUVE.I", 3) +
	# plot.rgb("ME", "FRAX.I", 3) +
	# plot.rgb("ME", "BETULA.I", 3) +
	plot.rgb("ME", "ACSA.I", 3) +
	plot.rgb("ME", "QUAL.I", 3) +
	# plot.rgb("ME", "FAGR.I", 3) +
	plot.rgb("ME", "ULRU.I", 3) +
	# plot.rgb("ME", "CARYA.I", 3) +
	# plot.rgb("ME", "SAAL.I", 3) +
	
	
	# Morgan Monroe
	plot.rgb("IN", "ACRU.I", 3) +
	plot.rgb("IN", "PIST.I", 3) +
	plot.rgb("IN", "TSCA.I", 3) +
	plot.rgb("IN", "QURU.I", 3) +
	plot.rgb("IN", "QUVE.I", 3) +
	# plot.rgb("IN", "FRAX.I", 3) +
	# plot.rgb("IN", "BETULA.I", 3) +
	plot.rgb("IN", "ACSA.I", 3) +
	plot.rgb("IN", "QUAL.I", 3) +
	# plot.rgb("IN", "FAGR.I", 3) +
	plot.rgb("IN", "ULRU.I", 3) +
	# plot.rgb("IN", "CARYA.I", 3) +
	# plot.rgb("IN", "SAAL.I", 3) +
	
	# Missouri Ozark
	
	plot.rgb("MO", "ACRU.I", 3) +
	plot.rgb("MO", "PIST.I", 3) +
	plot.rgb("MO", "TSCA.I", 3) +
	plot.rgb("MO", "QURU.I", 3) +
	plot.rgb("MO", "QUVE.I", 3) +
	# plot.rgb("MO", "FRAX.I", 3) +
	# plot.rgb("MO", "BETULA.I", 3) +
	plot.rgb("MO", "ACSA.I", 3) +
	plot.rgb("MO", "QUAL.I", 3) +
	# plot.rgb("MO", "FAGR.I", 3) +
	plot.rgb("MO", "ULRU.I", 3) +
	# plot.rgb("MO", "CARYA.I", 3) +
	# plot.rgb("MO", "SAAL.I", 3) +
	
	# Oak Openings
	
	plot.rgb("OH", "ACRU.I", 3) +
	plot.rgb("OH", "PIST.I", 3) +
	plot.rgb("OH", "TSCA.I", 3) +
	plot.rgb("OH", "QURU.I", 3) +
	plot.rgb("OH", "QUVE.I", 3) +
	# plot.rgb("OH", "FRAX.I", 3) +
	# plot.rgb("OH", "BETULA.I", 3) +
	plot.rgb("OH", "ACSA.I", 3) +
	plot.rgb("OH", "QUAL.I", 3) +
	# plot.rgb("OH", "FAGR.I", 3) +
	plot.rgb("OH", "ULRU.I", 3) +
	# plot.rgb("OH", "CARYA.I", 3) +
	# plot.rgb("OH", "SAAL.I", 3) +
	poster.theme2 +
	labs(title= "Intermediate Trees (group.cc)", x="Year", y=expression(bold(paste("BAI (mm2 / year)"))))
	
dev.off()	
	
		
# Just plotting the BAI fits
summary(data.graph)
ggplot(data.graph[data.graph$Canopy.Class=="I" & !data.graph$group.cc %in% c("BETULA.I", "CARYA.I", "FAGR.I", "FRAX.I", "SAAL.I"),]) + facet_grid(group.cc~State) +
  scale_x_continuous(expand=c(0,0), name="Year") +
  scale_y_continuous(expand=c(0,0), name="BAI") +
  # facet_wrap(~TreeID, scales="free_y", space="free") +
  # geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  geom_line(aes(x=Year, y=fit.full), size=2, alpha=0.5) +
  geom_ribbon(aes(x=Year, ymin=fit.full.lwr, ymax=fit.full.upr), alpha=0.3)

# Plotting the Effects
pdf("figures/gam3/gam3_influence_in_time_I.pdf", width= 13, height = 8.5)
ggplot(data.graph[data.graph$Canopy.Class=="I" & !data.graph$group.cc %in% c("BETULA.I", "CARYA.I", "FAGR.I", "FRAX.I", "SAAL.I"),]) + facet_grid(group~State) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="Effect on RW (in mm)") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=fit.tmean), size=1, color="red") +
	geom_line(aes(x=Year, y=fit.precip), size=1, color="blue") +
	geom_line(aes(x=Year, y=fit.dbh.recon), size=1, color="green")+
	ylim(c(0,3)) +
	labs(title= "Intermediate Effects (group.cc)")
dev.off()


################################################################
################################################################
# Suppressed Trees
################################################################
################################################################

pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_S.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$Canopy.Class=="S" & !data.graph$group.cc %in% c("BETULA.S", "CARYA.S", "FAGR.S", "FRAX.S", "SAAL.S"),]) + facet_grid(group ~ State) +
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
	# Harvard
	
	plot.rgb("MA", "ACRU.S", 3) +
	plot.rgb("MA", "PIST.S", 3) +
	plot.rgb("MA", "TSCA.S", 3) +
	plot.rgb("MA", "QURU.S", 3) +
	plot.rgb("MA", "QUVE.S", 3) +
	# plot.rgb("MA", "FRAX.S", 3) +
	# plot.rgb("MA", "BETULA.S", 3) +
	plot.rgb("MA", "ACSA.S", 3) +
	plot.rgb("MA", "QUAL.S", 3) +
	# plot.rgb("MA", "FAGR.S", 3) +
	plot.rgb("MA", "ULRU.S", 3) +
	# plot.rgb("MA", "CARYA.S", 3) +
	# plot.rgb("MA", "SAAL.S", 3) +
	
	# Howland
	plot.rgb("ME", "ACRU.S", 3) +
	plot.rgb("ME", "PIST.S", 3) +
	plot.rgb("ME", "TSCA.S", 3) +
	plot.rgb("ME", "QURU.S", 3) +
	plot.rgb("ME", "QUVE.S", 3) +
	# plot.rgb("ME", "FRAX.S", 3) +
	# plot.rgb("ME", "BETULA.S", 3) +
	plot.rgb("ME", "ACSA.S", 3) +
	plot.rgb("ME", "QUAL.S", 3) +
	# plot.rgb("ME", "FAGR.S", 3) +
	plot.rgb("ME", "ULRU.S", 3) +
	# plot.rgb("ME", "CARYA.S", 3) +
	# plot.rgb("ME", "SAAL.S", 3) +
	
	
	# Morgan Monroe
	plot.rgb("IN", "ACRU.S", 3) +
	plot.rgb("IN", "PIST.S", 3) +
	plot.rgb("IN", "TSCA.S", 3) +
	plot.rgb("IN", "QURU.S", 3) +
	plot.rgb("IN", "QUVE.S", 3) +
	# plot.rgb("IN", "FRAX.S", 3) +
	# plot.rgb("IN", "BETULA.S", 3) +
	plot.rgb("IN", "ACSA.S", 3) +
	plot.rgb("IN", "QUAL.S", 3) +
	# plot.rgb("IN", "FAGR.S", 3) +
	plot.rgb("IN", "ULRU.S", 3) +
	# plot.rgb("IN", "CARYA.S", 3) +
	# plot.rgb("IN", "SAAL.S", 3) +
	
	# Missouri Ozark
	
	plot.rgb("MO", "ACRU.S", 3) +
	plot.rgb("MO", "PIST.S", 3) +
	plot.rgb("MO", "TSCA.S", 3) +
	plot.rgb("MO", "QURU.S", 3) +
	plot.rgb("MO", "QUVE.S", 3) +
	# plot.rgb("MO", "FRAX.S", 3) +
	# plot.rgb("MO", "BETULA.S", 3) +
	plot.rgb("MO", "ACSA.S", 3) +
	plot.rgb("MO", "QUAL.S", 3) +
	# plot.rgb("MO", "FAGR.S", 3) +
	plot.rgb("MO", "ULRU.S", 3) +
	# plot.rgb("MO", "CARYA.S", 3) +
	# plot.rgb("MO", "SAAL.S", 3) +
	
	# Oak Openings
	
	plot.rgb("OH", "ACRU.S", 3) +
	plot.rgb("OH", "PIST.S", 3) +
	plot.rgb("OH", "TSCA.S", 3) +
	plot.rgb("OH", "QURU.S", 3) +
	plot.rgb("OH", "QUVE.S", 3) +
	# plot.rgb("OH", "FRAX.S", 3) +
	# plot.rgb("OH", "BETULA.S", 3) +
	plot.rgb("OH", "ACSA.S", 3) +
	plot.rgb("OH", "QUAL.S", 3) +
	# plot.rgb("OH", "FAGR.S", 3) +
	plot.rgb("OH", "ULRU.S", 3) +
	# plot.rgb("OH", "CARYA.S", 3) +
	# plot.rgb("OH", "SAAL.S", 3) +
	poster.theme2 +
	labs(title= "Suppressed Trees (group.cc)", x="Year", y=expression(bold(paste("BAI (mm2 / year)")))) 
	
	
dev.off()	
	
		
# Just plotting the BAI fits
summary(data.graph)
ggplot(data.graph[data.graph$Canopy.Class=="S" & !data.graph$group.cc %in% c("BETULA.S", "CARYA.S", "FAGR.S", "FRAX.S", "SAAL.S"),]) + facet_grid(group.cc~State) +
  scale_x_continuous(expand=c(0,0), name="Year") +
  scale_y_continuous(expand=c(0,0), name="BAI") +
  # facet_wrap(~TreeID, scales="free_y", space="free") +
  # geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  geom_line(aes(x=Year, y=fit.full), size=2, alpha=0.5) +
  geom_ribbon(aes(x=Year, ymin=fit.full.lwr, ymax=fit.full.upr), alpha=0.3)+
  ylim(c(-0.5,20))

# Plotting the Effects
pdf("figures/gam3/gam3_influence_in_time_S.pdf", width= 13, height = 8.5)
ggplot(data.graph[data.graph$Canopy.Class=="S" & !data.graph$group.cc %in% c("BETULA.S", "CARYA.S", "FAGR.S", "FRAX.S", "SAAL.S"),]) + facet_grid(group~State) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="Effect on RW (in mm)") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=fit.tmean), size=1, color="red") +
	geom_line(aes(x=Year, y=fit.precip), size=1, color="blue") +
	geom_line(aes(x=Year, y=fit.dbh.recon), size=1, color="green")+
	ylim(c(0,3)) +
	labs(title= "Suppressed Effects (group.cc)")
dev.off()


########################################################
########################################################
# Plotting out sites individually
########################################################
########################################################


# Missouri
# Plotting the Obs and modeled with influence coloring
pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_MO.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$State %in% "MO",]) + facet_grid(Canopy.Class ~ group) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="BAI") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
	
	# Missouri Ozark
	
		plot.rgb("MO", "ACRU.C", 3) +
	plot.rgb("MO", "ACRU.D", 3) +
	plot.rgb("MO", "ACRU.I", 3) +
	plot.rgb("MO", "ACRU.S", 3) +
	
	plot.rgb("MO", "PIST.C", 3) +
	plot.rgb("MO", "PIST.D", 3) +
	plot.rgb("MO", "PIST.I", 3) +
	plot.rgb("MO", "PIST.S", 3) +

	plot.rgb("MO", "TSCA.C", 3) +
	plot.rgb("MO", "TSCA.D", 3) +
	plot.rgb("MO", "TSCA.I", 3) +
	plot.rgb("MO", "TSCA.S", 3) +
	
	plot.rgb("MO", "QURU.C", 3) +
	plot.rgb("MO", "QURU.D", 3) +
	plot.rgb("MO", "QURU.I", 3) +
	plot.rgb("MO", "QURU.S", 3) +
	
	plot.rgb("MO", "QUVE.C", 3) +
	plot.rgb("MO", "QUVE.D", 3) +
	plot.rgb("MO", "QUVE.I", 3) +
	plot.rgb("MO", "QUVE.S", 3) +
	
	plot.rgb("MO", "FRAX.C", 3) +
	plot.rgb("MO", "FRAX.D", 3) +
	plot.rgb("MO", "FRAX.I", 3) +
	plot.rgb("MO", "FRAX.S", 3) +

	plot.rgb("MO", "BETULA.C", 3) +
	plot.rgb("MO", "BETULA.D", 3) +
	plot.rgb("MO", "BETULA.I", 3) +
	plot.rgb("MO", "BETULA.S", 3) +
	
	plot.rgb("MO", "ACSA.C", 3) +
	plot.rgb("MO", "ACSA.D", 3) +
	plot.rgb("MO", "ACSA.I", 3) +
	plot.rgb("MO", "ACSA.S", 3) +
	
	plot.rgb("MO", "QUAL.C", 3) +
	plot.rgb("MO", "QUAL.D", 3) +
	plot.rgb("MO", "QUAL.I", 3) +
	plot.rgb("MO", "QUAL.S", 3) +
	
	plot.rgb("MO", "FAGR.C", 3) +
	plot.rgb("MO", "FAGR.D", 3) +
	plot.rgb("MO", "FAGR.I", 3) +
	plot.rgb("MO", "FAGR.S", 3) +
	
	plot.rgb("MO", "ULRU.C", 3) +
	plot.rgb("MO", "ULRU.D", 3) +
	plot.rgb("MO", "ULRU.I", 3) +
	plot.rgb("MO", "ULRU.S", 3) +
	
	plot.rgb("MO", "CARYA.C", 3) +
	plot.rgb("MO", "CARYA.D", 3) +
	plot.rgb("MO", "CARYA.I", 3) +
	plot.rgb("MO", "CARYA.S", 3) +

	plot.rgb("MO", "SAAL.C", 3) +
	plot.rgb("MO", "SAAL.D", 3) +
	plot.rgb("MO", "SAAL.I", 3) +
	plot.rgb("MO", "SAAL.S", 3) +
	poster.theme2
	
	
dev.off()	

#----------------------------------------
# Indiana

# Plotting the Obs and modeled with influence coloring
pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_IN.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$State %in% "IN" & !,]) + facet_grid(Canopy.Class ~ group) +
	
	labs(title= "Dominant Trees", x="Year", y = expression(bold(paste("BAI (mm"^"2", "y"^"-1",")")))) +
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
		
	# Morgan Monroe
	plot.rgb("IN", "ACRU.C", 3) +
	plot.rgb("IN", "ACRU.D", 3) +
	plot.rgb("IN", "ACRU.I", 3) +
	plot.rgb("IN", "ACRU.S", 3) +
	
	plot.rgb("IN", "PIST.C", 3) +
	plot.rgb("IN", "PIST.D", 3) +
	plot.rgb("IN", "PIST.I", 3) +
	plot.rgb("IN", "PIST.S", 3) +

	plot.rgb("IN", "TSCA.C", 3) +
	plot.rgb("IN", "TSCA.D", 3) +
	plot.rgb("IN", "TSCA.I", 3) +
	plot.rgb("IN", "TSCA.S", 3) +
	
	plot.rgb("IN", "QURU.C", 3) +
	plot.rgb("IN", "QURU.D", 3) +
	plot.rgb("IN", "QURU.I", 3) +
	plot.rgb("IN", "QURU.S", 3) +
	
	plot.rgb("IN", "QUVE.C", 3) +
	plot.rgb("IN", "QUVE.D", 3) +
	plot.rgb("IN", "QUVE.I", 3) +
	plot.rgb("IN", "QUVE.S", 3) +
	
	plot.rgb("IN", "FRAX.C", 3) +
	plot.rgb("IN", "FRAX.D", 3) +
	plot.rgb("IN", "FRAX.I", 3) +
	plot.rgb("IN", "FRAX.S", 3) +

	plot.rgb("IN", "BETULA.C", 3) +
	plot.rgb("IN", "BETULA.D", 3) +
	plot.rgb("IN", "BETULA.I", 3) +
	plot.rgb("IN", "BETULA.S", 3) +
	
	plot.rgb("IN", "ACSA.C", 3) +
	plot.rgb("IN", "ACSA.D", 3) +
	plot.rgb("IN", "ACSA.I", 3) +
	plot.rgb("IN", "ACSA.S", 3) +
	
	plot.rgb("IN", "QUAL.C", 3) +
	plot.rgb("IN", "QUAL.D", 3) +
	plot.rgb("IN", "QUAL.I", 3) +
	plot.rgb("IN", "QUAL.S", 3) +
	
	plot.rgb("IN", "FAGR.C", 3) +
	plot.rgb("IN", "FAGR.D", 3) +
	plot.rgb("IN", "FAGR.I", 3) +
	plot.rgb("IN", "FAGR.S", 3) +
	
	plot.rgb("IN", "ULRU.C", 3) +
	plot.rgb("IN", "ULRU.D", 3) +
	plot.rgb("IN", "ULRU.I", 3) +
	plot.rgb("IN", "ULRU.S", 3) +
	
	plot.rgb("IN", "CARYA.C", 3) +
	plot.rgb("IN", "CARYA.D", 3) +
	plot.rgb("IN", "CARYA.I", 3) +
	plot.rgb("IN", "CARYA.S", 3) +

	plot.rgb("IN", "SAAL.C", 3) +
	plot.rgb("IN", "SAAL.D", 3) +
	plot.rgb("IN", "SAAL.I", 3) +
	plot.rgb("IN", "SAAL.S", 3) +
	poster.theme2
	

	
dev.off()	

# Indiana Dominant

# Plotting the Obs and modeled with influence coloring
pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_IN_dom.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$State %in% "IN" & data.graph$Canopy.Class=="D" & !data.graph$group.cc %in% c("BETULA.D", "CARYA.D", "FAGR.D", "FRAX.D", "SAAL.D"),]) + facet_grid(group ~.) +
	labs(title= " IN Dominant Trees", x="Year", y = expression(bold(paste("BAI (mm"^"2", "y"^"-1",")")))) +
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
		
	# Morgan Monroe
	
	plot.rgb("IN", "ACRU.D", 3) +
	plot.rgb("IN", "PIST.D", 3) +
	plot.rgb("IN", "TSCA.D", 3) +
	plot.rgb("IN", "QURU.D", 3) +
	plot.rgb("IN", "QUVE.D", 3) +
	plot.rgb("IN", "ACSA.D", 3) +
	plot.rgb("IN", "QUAL.D", 3) +
	plot.rgb("IN", "ULRU.D", 3) +
	poster.theme2
	

	

	
dev.off()	



#-------------------------------------------
# Ohio

# Plotting the Obs and modeled with influence coloring
pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_OH.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$State %in% "OH",]) + facet_grid(Canopy.Class ~ group) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="BAI") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
	# Oak Openings
	
			plot.rgb("OH", "ACRU.C", 3) +
	plot.rgb("OH", "ACRU.D", 3) +
	plot.rgb("OH", "ACRU.I", 3) +
	plot.rgb("OH", "ACRU.S", 3) +
	
	plot.rgb("OH", "PIST.C", 3) +
	plot.rgb("OH", "PIST.D", 3) +
	plot.rgb("OH", "PIST.I", 3) +
	plot.rgb("OH", "PIST.S", 3) +

	plot.rgb("OH", "TSCA.C", 3) +
	plot.rgb("OH", "TSCA.D", 3) +
	plot.rgb("OH", "TSCA.I", 3) +
	plot.rgb("OH", "TSCA.S", 3) +
	
	plot.rgb("OH", "QURU.C", 3) +
	plot.rgb("OH", "QURU.D", 3) +
	plot.rgb("OH", "QURU.I", 3) +
	plot.rgb("OH", "QURU.S", 3) +
	
	plot.rgb("OH", "QUVE.C", 3) +
	plot.rgb("OH", "QUVE.D", 3) +
	plot.rgb("OH", "QUVE.I", 3) +
	plot.rgb("OH", "QUVE.S", 3) +
	
	plot.rgb("OH", "FRAX.C", 3) +
	plot.rgb("OH", "FRAX.D", 3) +
	plot.rgb("OH", "FRAX.I", 3) +
	plot.rgb("OH", "FRAX.S", 3) +

	plot.rgb("OH", "BETULA.C", 3) +
	plot.rgb("OH", "BETULA.D", 3) +
	plot.rgb("OH", "BETULA.I", 3) +
	plot.rgb("OH", "BETULA.S", 3) +
	
	plot.rgb("OH", "ACSA.C", 3) +
	plot.rgb("OH", "ACSA.D", 3) +
	plot.rgb("OH", "ACSA.I", 3) +
	plot.rgb("OH", "ACSA.S", 3) +
	
	plot.rgb("OH", "QUAL.C", 3) +
	plot.rgb("OH", "QUAL.D", 3) +
	plot.rgb("OH", "QUAL.I", 3) +
	plot.rgb("OH", "QUAL.S", 3) +
	
	plot.rgb("OH", "FAGR.C", 3) +
	plot.rgb("OH", "FAGR.D", 3) +
	plot.rgb("OH", "FAGR.I", 3) +
	plot.rgb("OH", "FAGR.S", 3) +
	
	plot.rgb("OH", "ULRU.C", 3) +
	plot.rgb("OH", "ULRU.D", 3) +
	plot.rgb("OH", "ULRU.I", 3) +
	plot.rgb("OH", "ULRU.S", 3) +
	
	plot.rgb("OH", "CARYA.C", 3) +
	plot.rgb("OH", "CARYA.D", 3) +
	plot.rgb("OH", "CARYA.I", 3) +
	plot.rgb("OH", "CARYA.S", 3) +

	plot.rgb("OH", "SAAL.C", 3) +
	plot.rgb("OH", "SAAL.D", 3) +
	plot.rgb("OH", "SAAL.I", 3) +
	plot.rgb("OH", "SAAL.S", 3)+
	poster.theme2
	
dev.off()	

#-----------------------------------------------
# Massachusetts

# Plotting the Obs and modeled with influence coloring
pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_MA.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$State %in% "MA",]) + facet_grid(Canopy.Class~group) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="BAI") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
	# Harvard
	plot.rgb("MA", "ACRU.C", 3) +
	plot.rgb("MA", "ACRU.D", 3) +
	plot.rgb("MA", "ACRU.I", 3) +
	plot.rgb("MA", "ACRU.S", 3) +
	
	plot.rgb("MA", "PIST.C", 3) +
	plot.rgb("MA", "PIST.D", 3) +
	plot.rgb("MA", "PIST.I", 3) +
	plot.rgb("MA", "PIST.S", 3) +

	plot.rgb("MA", "TSCA.C", 3) +
	plot.rgb("MA", "TSCA.D", 3) +
	plot.rgb("MA", "TSCA.I", 3) +
	plot.rgb("MA", "TSCA.S", 3) +
	
	plot.rgb("MA", "QURU.C", 3) +
	plot.rgb("MA", "QURU.D", 3) +
	plot.rgb("MA", "QURU.I", 3) +
	plot.rgb("MA", "QURU.S", 3) +
	
	plot.rgb("MA", "QUVE.C", 3) +
	plot.rgb("MA", "QUVE.D", 3) +
	plot.rgb("MA", "QUVE.I", 3) +
	plot.rgb("MA", "QUVE.S", 3) +
	
	plot.rgb("MA", "FRAX.C", 3) +
	plot.rgb("MA", "FRAX.D", 3) +
	plot.rgb("MA", "FRAX.I", 3) +
	plot.rgb("MA", "FRAX.S", 3) +

	plot.rgb("MA", "BETULA.C", 3) +
	plot.rgb("MA", "BETULA.D", 3) +
	plot.rgb("MA", "BETULA.I", 3) +
	plot.rgb("MA", "BETULA.S", 3) +
	
	plot.rgb("MA", "ACSA.C", 3) +
	plot.rgb("MA", "ACSA.D", 3) +
	plot.rgb("MA", "ACSA.I", 3) +
	plot.rgb("MA", "ACSA.S", 3) +
	
	plot.rgb("MA", "QUAL.C", 3) +
	plot.rgb("MA", "QUAL.D", 3) +
	plot.rgb("MA", "QUAL.I", 3) +
	plot.rgb("MA", "QUAL.S", 3) +
	
	plot.rgb("MA", "FAGR.C", 3) +
	plot.rgb("MA", "FAGR.D", 3) +
	plot.rgb("MA", "FAGR.I", 3) +
	plot.rgb("MA", "FAGR.S", 3) +
	
	plot.rgb("MA", "ULRU.C", 3) +
	plot.rgb("MA", "ULRU.D", 3) +
	plot.rgb("MA", "ULRU.I", 3) +
	plot.rgb("MA", "ULRU.S", 3) +
	
	plot.rgb("MA", "CARYA.C", 3) +
	plot.rgb("MA", "CARYA.D", 3) +
	plot.rgb("MA", "CARYA.I", 3) +
	plot.rgb("MA", "CARYA.S", 3) +

	plot.rgb("MA", "SAAL.C", 3) +
	plot.rgb("MA", "SAAL.D", 3) +
	plot.rgb("MA", "SAAL.I", 3) +
	plot.rgb("MA", "SAAL.S", 3) +
	poster.theme2
	
dev.off()	
	
	
#-----------------------------------------------------
		
# Plotting the Obs and modeled with influence coloring
pdf("figures/gam3/gam3_SPP_CC_BAI_limiting_factors_ME.pdf", width= 13, height = 8.5)
ggplot(data = data.graph[data.graph$State %in% "ME",]) + facet_grid(Canopy.Class ~ group) +
	scale_x_continuous(expand=c(0,0), name="Year") +
	scale_y_continuous(expand=c(0,0), name="BAI") +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam1.weights[gam1.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	
	# Howland
		plot.rgb("ME", "ACRU.C", 3) +
	plot.rgb("ME", "ACRU.D", 3) +
	plot.rgb("ME", "ACRU.I", 3) +
	plot.rgb("ME", "ACRU.S", 3) +
	
	plot.rgb("ME", "PIST.C", 3) +
	plot.rgb("ME", "PIST.D", 3) +
	plot.rgb("ME", "PIST.I", 3) +
	plot.rgb("ME", "PIST.S", 3) +

	plot.rgb("ME", "TSCA.C", 3) +
	plot.rgb("ME", "TSCA.D", 3) +
	plot.rgb("ME", "TSCA.I", 3) +
	plot.rgb("ME", "TSCA.S", 3) +
	
	plot.rgb("ME", "QURU.C", 3) +
	plot.rgb("ME", "QURU.D", 3) +
	plot.rgb("ME", "QURU.I", 3) +
	plot.rgb("ME", "QURU.S", 3) +
	
	plot.rgb("ME", "QUVE.C", 3) +
	plot.rgb("ME", "QUVE.D", 3) +
	plot.rgb("ME", "QUVE.I", 3) +
	plot.rgb("ME", "QUVE.S", 3) +
	
	plot.rgb("ME", "FRAX.C", 3) +
	plot.rgb("ME", "FRAX.D", 3) +
	plot.rgb("ME", "FRAX.I", 3) +
	plot.rgb("ME", "FRAX.S", 3) +

	plot.rgb("ME", "BETULA.C", 3) +
	plot.rgb("ME", "BETULA.D", 3) +
	plot.rgb("ME", "BETULA.I", 3) +
	plot.rgb("ME", "BETULA.S", 3) +
	
	plot.rgb("ME", "ACSA.C", 3) +
	plot.rgb("ME", "ACSA.D", 3) +
	plot.rgb("ME", "ACSA.I", 3) +
	plot.rgb("ME", "ACSA.S", 3) +
	
	plot.rgb("ME", "QUAL.C", 3) +
	plot.rgb("ME", "QUAL.D", 3) +
	plot.rgb("ME", "QUAL.I", 3) +
	plot.rgb("ME", "QUAL.S", 3) +
	
	plot.rgb("ME", "FAGR.C", 3) +
	plot.rgb("ME", "FAGR.D", 3) +
	plot.rgb("ME", "FAGR.I", 3) +
	plot.rgb("ME", "FAGR.S", 3) +
	
	plot.rgb("ME", "ULRU.C", 3) +
	plot.rgb("ME", "ULRU.D", 3) +
	plot.rgb("ME", "ULRU.I", 3) +
	plot.rgb("ME", "ULRU.S", 3) +
	
	plot.rgb("ME", "CARYA.C", 3) +
	plot.rgb("ME", "CARYA.D", 3) +
	plot.rgb("ME", "CARYA.I", 3) +
	plot.rgb("ME", "CARYA.S", 3) +

	plot.rgb("ME", "SAAL.C", 3) +
	plot.rgb("ME", "SAAL.D", 3) +
	plot.rgb("ME", "SAAL.I", 3) +
	plot.rgb("ME", "SAAL.S", 3)+
	poster.theme2
	
dev.off()	
			
			
gam3.data.graph <- data.graph
save(gam3.data.graph, file="processed_data/gam3_graph_data.Rdata")