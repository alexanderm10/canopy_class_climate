library(ggplot2)
require(car)

se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}



load(file="processed_data/gamm_weights/gam2_weights.Rdata")

summary(gam2.weights)
factors.fits <- c("fit.tmean", "fit.precip", "fit.dbh.recon", "fit.full", "BA.inc")
# factors.weights <- c("weight.tmean", "weight.dbh.recon", "weight.precip")
# factors.weights <- c("weight.tmean.bai.2", "weight.dbh.bai.2", "weight.precip.bai.2")
factors.weights <- c("weight.tmean2", "weight.dbh.recon2", "weight.precip2")
gam2.weights[,c("weight.tmean2", "weight.precip2", "weight.dbh.recon2")] <- gam2.weights[,c("weight.tmean", "weight.precip", "weight.dbh.recon")]/rowSums(gam2.weights[,c("weight.tmean", "weight.precip", "weight.dbh.recon")],na.rm=T)

# Transforming things back to BA.inc rather than log
gam2.weights[,which(substr(names(gam2.weights),1,3)=="fit")] <- exp(gam2.weights[,which(substr(names(gam2.weights),1,3)=="fit")] )

# othervars <- c("Year", "Site", "Canopy.Class", "Model")
othervars <- c("Year", "Canopy.Class", "Model")

data.graph1 <- aggregate(gam2.weights[,factors.fits], by = gam2.weights[,othervars], FUN= mean, na.rm=T)

data.graph1[,paste(factors.fits, "upr", sep=".")] <- aggregate(gam2.weights[,factors.fits], by = gam2.weights[,othervars], FUN= quantile, prob= 0.975, na.rm=T)[,factors.fits]

data.graph1[,paste(factors.fits, "lwr", sep=".")] <- aggregate(gam2.weights[,factors.fits], by = gam2.weights[,othervars], FUN= quantile, prob= 0.025, na.rm=T)[,factors.fits]

summary(data.graph1)

data.graph2 <- aggregate(abs(gam2.weights[,factors.weights]), by = gam2.weights[,othervars], FUN= mean, na.rm=T)

data.graph2[,paste(factors.weights, "upr", sep=".")] <- aggregate(abs(gam2.weights[,factors.weights]), by = gam2.weights[,othervars], FUN= quantile, prob= 0.975, na.rm=T)[,factors.weights]

data.graph2[,paste(factors.weights, "lwr", sep=".")] <- aggregate(abs(gam2.weights[,factors.weights]), by = gam2.weights[,othervars], FUN= quantile, prob= 0.025, na.rm=T)[,factors.weights]

data.graph2[,paste(factors.weights, "SD", sep=".")] <- aggregate(abs(gam2.weights[,factors.weights]), by = gam2.weights[,othervars], FUN= sd, na.rm=T)[,factors.weights]

data.graph2[,paste(factors.weights, "SE", sep=".")] <- aggregate(abs(gam2.weights[,factors.weights]), by = gam2.weights[,othervars], FUN= se)[,factors.weights]

summary(data.graph2)

data.graph <- merge(data.graph1, data.graph2, all.x=T, all.y=T)

# data.graph <- gam2.weights[gam2.weights$TreeID== "MMA014",]
summary(data.graph)
gam2.weights$wts.check <- rowSums(abs(gam2.weights[,factors.weights]))
data.graph$wts.check <- rowSums(abs(data.graph[,factors.weights]))

summary(gam2.weights)
summary(data.graph)

# Ordering the data for graphing

# Removing the site aggregation for now
# data.graph<- data.graph[order(data.graph$Year, data.graph$Canopy.Class, data.graph$Site, decreasing=F),]
data.graph<- data.graph[order(data.graph$Year, data.graph$Canopy.Class,decreasing=F),]

save(data.graph, file="processed_data/gam2_growth_limitations_weight_data.Rdata")

plot.rgb <- function(STATE, CC, SIZE){	geom_point(data=data.graph[data.graph$State==STATE & data.graph$Canopy.Class==CC,],aes(x=Year, y=fit.full), size=SIZE,
  		        color=rgb(abs(data.graph[data.graph$State==STATE & data.graph$Canopy.Class==CC,"weight.tmean2"     ]), # red
                        abs(data.graph[data.graph$State==STATE & data.graph$Canopy.Class==CC,"weight.dbh.recon2"     ]), # green
                        abs(data.graph[data.graph$State==STATE & data.graph$Canopy.Class==CC,"weight.precip2"   ]))) }   # blue


# Plotting the Obs and modeled with influence coloring
data.graph$State <- recode(data.graph$Site, "'Howland' = 'ME';'Harvard' = 'MA';'Morgan Monroe State Park' = 'IN';'Missouri Ozark' = 'MO';'Oak Openings Toledo' = 'OH'")
data.graph$State <- factor(data.graph$State, levels=c("MO", "IN", "OH", "MA", "ME"))
# save(data.graph, file="processed_data/gam2_weights_graph_withSites.Rdata")

# Plotting the Obs and modeled with influence coloring
pdf("figures/Prelim_Figures/gam2_canopyclass_BAI_limiting_factors.pdf", width= 13, height = 8.5)
ggplot(data.graph) + facet_grid(Canopy.Class~State, scale="free") +
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	# facet_wrap(~TreeID, scales="free_y", space="free") +
	# geom_ribbon(data=gam2.weights[gam2.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3) +
	
	geom_line(aes(x=Year, y = 0), linetype="dashed") +
	plot.rgb("MA", "C", 2) +
	plot.rgb("MA", "D", 2) +
	plot.rgb("MA", "I", 2) +
	plot.rgb("MA", "S", 2) +
		
	plot.rgb("ME", "C", 2) +
	plot.rgb("ME", "D", 2) +
	plot.rgb("ME", "I", 2) +
	plot.rgb("ME", "S", 2) +
		
	plot.rgb("MO", "C", 2) +
	plot.rgb("MO", "D", 2) +
	plot.rgb("MO", "I", 2) +
	plot.rgb("MO", "S", 2) +
	
	plot.rgb("IN", "C", 2) +
	plot.rgb("IN", "D", 2) +
	plot.rgb("IN", "I", 2) +
	plot.rgb("IN", "S", 2) +
	

	plot.rgb("OH", "C", 2) +
	plot.rgb("OH", "D", 2) +
	plot.rgb("OH", "I", 2) +
	plot.rgb("OH", "S", 2) +
	labs(title= "Canopy Class", x="Year", y = expression(bold(paste("BAI (mm"^"2", "y"^"-1",")")))) +
	theme_bw()
dev.off()		
# Just plotting the BAI fits
summary(data.graph)

ggplot(data.graph) + facet_grid(State~Canopy.Class) +
  scale_x_continuous(expand=c(0,0), name="Year") +
  scale_y_continuous(expand=c(0,0), name="BAI") +
  # facet_wrap(~TreeID, scales="free_y", space="free") +
  # geom_ribbon(data=gam2.weights[gam2.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  geom_line(aes(x=Year, y=fit.full), size=2, alpha=0.5) +
  geom_ribbon(aes(x=Year, ymin=fit.full.lwr, ymax=fit.full.upr), alpha=0.3)

load("processed_data/climate_markeryears.Rdata")
summary(climate.markers)
climate.markers$State <- recode(climate.markers$Site, "'Howland' = 'ME';'Harvard' = 'MA';'Morgan Monroe State Park' = 'IN';'Missouri Ozark' = 'MO';'Oak Openings Toledo' = 'OH'")


############################################################################
# Looking at the BAI for each canopy class in reference
############################################################################
# Missouri Ozark

pdf("figures/Prelim_Figures/bai_climatemarkers_canopy.pdf", width= 13, height = 8.5)
ggplot(data.graph[data.graph$State=="MO",]) + facet_grid(Canopy.Class~., scales="free") + 
	scale_x_continuous(expand=c(0,0), name="Year") +
  	scale_y_continuous(expand=c(0,0), name="BAI") +
  	# facet_wrap(~TreeID, scales="free_y", space="free") +
  	# geom_ribbon(data=gam2.weights[gam2.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3)+
  	geom_vline(data=climate.markers[climate.markers$State=="MO" & climate.markers$type=="tmean",],aes(xintercept=marker.year, color=marker), alpha=0.35)+
  	scale_color_manual(values=c("red", "blue"))+
  	xlim(1900,2015)+
  	theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), panel.background=element_blank(),axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +labs(title="Missouri Canopy temp years")


ggplot(data.graph[data.graph$State=="MO",]) + facet_grid(Canopy.Class~., scales="free") +
	scale_x_continuous(expand=c(0,0), name="Year") +
  	scale_y_continuous(expand=c(0,0), name="BAI") +
  	# facet_wrap(~TreeID, scales="free_y", space="free") +
  	# geom_ribbon(data=gam2.weights[gam2.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3)+
  	geom_vline(data=climate.markers[climate.markers$State=="MO" & climate.markers$type=="precip",],aes(xintercept=marker.year, color=marker), alpha=0.35)+
  	xlim(1900,2015)+
  	scale_color_manual(values=c("dodgerblue", "brown"))+
  	theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), panel.background=element_blank(),axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) + labs(title="Missouri Canopy precip years")


# Morgan Monroe

ggplot(data.graph[data.graph$State=="IN",]) + facet_grid(Canopy.Class~., scales="free") + 
	scale_x_continuous(expand=c(0,0), name="Year") +
  	scale_y_continuous(expand=c(0,0), name="BAI") +
  	# facet_wrap(~TreeID, scales="free_y", space="free") +
  	# geom_ribbon(data=gam2.weights[gam2.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3)+
  	geom_vline(data=climate.markers[climate.markers$State=="IN" & climate.markers$type=="tmean",],aes(xintercept=marker.year, color=marker), alpha=0.35)+
  	scale_color_manual(values=c("red", "blue"))+
  	xlim(1900,2015)+
  	theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), panel.background=element_blank(),axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +labs(title="MMF Canopy temp years")


ggplot(data.graph[data.graph$State=="IN",]) + facet_grid(Canopy.Class~., scales="free") +
	scale_x_continuous(expand=c(0,0), name="Year") +
  	scale_y_continuous(expand=c(0,0), name="BAI") +
  	# facet_wrap(~TreeID, scales="free_y", space="free") +
  	# geom_ribbon(data=gam2.weights[gam2.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3)+
  	geom_vline(data=climate.markers[climate.markers$State=="IN" & climate.markers$type=="precip",],aes(xintercept=marker.year, color=marker), alpha=0.35)+
  	xlim(1900,2015)+
  	scale_color_manual(values=c("dodgerblue", "brown"))+
  	theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), panel.background=element_blank(),axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) + labs(title="MMF Canopy precip years")





# Oak Openings
ggplot(data.graph[data.graph$State=="OH",]) + facet_grid(Canopy.Class~., scales="free") + 
	scale_x_continuous(expand=c(0,0), name="Year") +
  	scale_y_continuous(expand=c(0,0), name="BAI") +
  	# facet_wrap(~TreeID, scales="free_y", space="free") +
  	# geom_ribbon(data=gam2.weights[gam2.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3)+
  	geom_vline(data=climate.markers[climate.markers$State=="OH" & climate.markers$type=="tmean",],aes(xintercept=marker.year, color=marker), alpha=0.35)+
  	scale_color_manual(values=c("red", "blue"))+
  	xlim(1900,2015)+
  	theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), panel.background=element_blank(),axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +labs(title="Oak Openings Canopy temp years")


ggplot(data.graph[data.graph$State=="OH",]) + facet_grid(Canopy.Class~., scales="free") +
	scale_x_continuous(expand=c(0,0), name="Year") +
  	scale_y_continuous(expand=c(0,0), name="BAI") +
  	# facet_wrap(~TreeID, scales="free_y", space="free") +
  	# geom_ribbon(data=gam2.weights[gam2.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3)+
  	geom_vline(data=climate.markers[climate.markers$State=="OH" & climate.markers$type=="precip",],aes(xintercept=marker.year, color=marker), alpha=0.35)+
  	xlim(1900,2015)+
  	scale_color_manual(values=c("dodgerblue", "brown"))+
  	theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), panel.background=element_blank(),axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) + labs(title="Oak Openings Canopy precip years")

# Harvard

ggplot(data.graph[data.graph$State=="MA",]) + facet_grid(Canopy.Class~., scales="free") + 
	scale_x_continuous(expand=c(0,0), name="Year") +
  	scale_y_continuous(expand=c(0,0), name="BAI") +
  	# facet_wrap(~TreeID, scales="free_y", space="free") +
  	# geom_ribbon(data=gam2.weights[gam2.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3)+
  	geom_vline(data=climate.markers[climate.markers$State=="MA" & climate.markers$type=="tmean",],aes(xintercept=marker.year, color=marker), alpha=0.35)+
  	scale_color_manual(values=c("red", "blue"))+
  	xlim(1900,2015)+
  	theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), panel.background=element_blank(),axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) + labs(title="Harvard Canopy temp years")


ggplot(data.graph[data.graph$State=="MA",]) + facet_grid(Canopy.Class~., scales="free") +
	scale_x_continuous(expand=c(0,0), name="Year") +
  	scale_y_continuous(expand=c(0,0), name="BAI") +
  	# facet_wrap(~TreeID, scales="free_y", space="free") +
  	# geom_ribbon(data=gam2.weights[gam2.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3)+
  	geom_vline(data=climate.markers[climate.markers$State=="MA" & climate.markers$type=="precip",],aes(xintercept=marker.year, color=marker), alpha=0.35)+
  	xlim(1900,2015)+
  	scale_color_manual(values=c("dodgerblue", "brown"))+
  	theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), panel.background=element_blank(),axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) + labs(title="Harvard Canopy precip years")



# Howland
ggplot(data.graph[data.graph$State=="ME",]) + facet_grid(Canopy.Class~., scales="free") + 
	scale_x_continuous(expand=c(0,0), name="Year") +
  	scale_y_continuous(expand=c(0,0), name="BAI") +
  	# facet_wrap(~TreeID, scales="free_y", space="free") +
  	# geom_ribbon(data=gam2.weights[gam2.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3)+
  	geom_vline(data=climate.markers[climate.markers$State=="ME" & climate.markers$type=="tmean",],aes(xintercept=marker.year, color=marker), alpha=0.35)+
  	scale_color_manual(values=c("red", "blue"))+
  	xlim(1900,2015)+
  	theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), panel.background=element_blank(),axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +labs(title="Howland Canopy temp years")


ggplot(data.graph[data.graph$State=="ME",]) + facet_grid(Canopy.Class~., scales="free") +
	scale_x_continuous(expand=c(0,0), name="Year") +
  	scale_y_continuous(expand=c(0,0), name="BAI") +
  	# facet_wrap(~TreeID, scales="free_y", space="free") +
  	# geom_ribbon(data=gam2.weights[gam2.weights$data.type=="Model",], aes(x=Year, ymin=Y.rel.10.lo*100, ymax=Y.rel.10.hi*100), 	alpha=0.5) +
  	geom_line(aes(x=Year, y=BA.inc), size=2, alpha=0.5) +
	geom_ribbon(aes(x=Year, ymin=BA.inc.lwr, ymax=BA.inc.upr), alpha=0.3)+
  	geom_vline(data=climate.markers[climate.markers$State=="ME" & climate.markers$type=="precip",],aes(xintercept=marker.year, color=marker), alpha=0.35)+
  	xlim(1900,2015)+
  	scale_color_manual(values=c("dodgerblue", "brown"))+
  	theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(), panel.background=element_blank(),axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) + labs(title="HowlandCanopy precip years")
dev.off()




# Plotting the Effects
data.graph$State <- factor(data.graph$State, levels=c("MO", "IN", "OH", "MA", "ME"))

data.graph$Canopy.Class <- recode(data.graph$Canopy.Class,"'D'='Dominant';'I'='Intermediate';'S'='Suppressed'")

save(data.graph, file="processed_data/gam2_weights_graph.Rdata")
pdf("figures/Prelim_Figures/gam2_weights_No-site.pdf", width= 13, height = 8.5)
ggplot(data.graph) + facet_grid(~Canopy.Class) +
		
	geom_ribbon(aes(x=Year, ymin=weight.tmean2 - weight.tmean2.SE, ymax=weight.tmean2 + weight.tmean2.SE), alpha=0.25, fill="red") +
	geom_ribbon(aes(x=Year, ymin=weight.precip2 - weight.precip2.SE, ymax=weight.precip2 + weight.precip2.SE), alpha=0.25, fill="blue") +
	geom_ribbon(aes(x=Year, ymin=weight.dbh.recon2 - weight.dbh.recon2.SE, ymax=weight.dbh.recon2 + weight.dbh.recon2.SE), alpha=0.25, fill="green") +

	geom_line(aes(x=Year, y=weight.tmean2), size=1, color="red") +
	geom_line(aes(x=Year, y=weight.precip2), size=1, color="blue") +
	geom_line(aes(x=Year, y=weight.dbh.recon2), size=1, color="green")+
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())+
	theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
	labs(title= "Canopy model", x= expression(bold(paste("Year"))), y = expression(bold(paste("Limiting Factor Influence"))))

dev.off()	

pdf("figures/Prelim_Figures/gam2_influence_in_time_weights_temp.pdf", width= 13, height = 8.5)
ggplot(data.graph) + facet_grid(Canopy.Class~State) +
	geom_vline(data=climate.markers[climate.markers$type=="tmean",],aes(xintercept=marker.year, color=marker), alpha=0.5)+
	scale_color_manual(values=c("red", "blue"))+
	
	geom_ribbon(aes(x=Year, ymin=weight.tmean2 - weight.tmean2.SE, ymax=weight.tmean2 + weight.tmean2.SE), alpha=0.25, fill="red") +
	geom_ribbon(aes(x=Year, ymin=weight.precip2 - weight.precip2.SE, ymax=weight.precip2 + weight.precip2.SE), alpha=0.25, fill="blue") +
	geom_ribbon(aes(x=Year, ymin=weight.dbh.recon2 - weight.dbh.recon2.SE, ymax=weight.dbh.recon2 + weight.dbh.recon2.SE), alpha=0.25, fill="green") +

	geom_line(aes(x=Year, y=weight.tmean2), size=1, color="red") +
	geom_line(aes(x=Year, y=weight.precip2), size=1, color="blue") +
	geom_line(aes(x=Year, y=weight.dbh.recon2), size=1, color="green")+
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())+
	theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
	labs(title= "Canopy model", x= expression(bold(paste("Year"))), y = expression(bold(paste("Limiting Factor Influence"))))
dev.off()	

pdf("figures/Prelim_Figures/gam2_influence_in_time_weights_precip.pdf", width= 13, height = 8.5)
ggplot(data.graph) + facet_grid(Canopy.Class~State) +
	geom_vline(data=climate.markers[climate.markers$type=="precip",],aes(xintercept=marker.year, color=marker), alpha=0.5)+
	scale_color_manual(values=c("dodgerblue", "brown"))+
	
	geom_ribbon(aes(x=Year, ymin=weight.tmean2 - weight.tmean2.SE, ymax=weight.tmean2 + weight.tmean2.SE), alpha=0.25, fill="red") +
	geom_ribbon(aes(x=Year, ymin=weight.precip2 - weight.precip2.SE, ymax=weight.precip2 + weight.precip2.SE), alpha=0.25, fill="blue") +
	geom_ribbon(aes(x=Year, ymin=weight.dbh.recon2 - weight.dbh.recon2.SE, ymax=weight.dbh.recon2 + weight.dbh.recon2.SE), alpha=0.25, fill="green") +
	
	geom_line(aes(x=Year, y=weight.tmean2), size=1, color="red") +
	geom_line(aes(x=Year, y=weight.precip2), size=1, color="blue") +
	geom_line(aes(x=Year, y=weight.dbh.recon2), size=1, color="green")+
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())+
	theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
	labs(title= "Canopy model", x= expression(bold(paste("Year"))), y = expression(bold(paste("Limiting Factor Influence"))))
dev.off()	



# Checking residuals
ggplot(data=data.graph) + facet_grid(Canopy.Class~State, scales="free") +
	geom_point(aes(x=log(fit.full), y=log(BA.inc)))+
	geom_abline(intercept=0, slope=1, color="red")
	
	
	
	

gam2.data.graph <- data.graph
save(gam2.data.graph, file="processed_data/gam2_graph_data.Rdata")