library(ggplot2)
require(mgcv)
require(lsmeans)
require(car)
require(moments)

# calculating competition indices
load("processed_data/gam2_weights_processed.Rdata")
summary(gam2.weights)

load("ch2_combined_data_use.Rdata")
summary(test)

# Going to calculat the comptition index based on the maximum BA in a given plot
# making a variable for the basal area at the time of sampling

plots <- unique(test$PlotID)

for(y in unique(test$Year)){
	for(p in unique(test$PlotID)){
		BA.max <- max(test[test$PlotID==p & test$Year==y, "BA"], na.rm=T)
		for(i in unique(test[test$PlotID==p & test$Year==y,"TreeID"])){
			test[test$TreeID==i & test$Year==y, "comp.index"] <- test[test$TreeID==i & test$Year==y,"BA"] / BA.max		
		}
	}
}

summary(test)

mean(test[test$Canopy.Class=="D", "BA"]); sd(test[test$Canopy.Class=="D", "BA"])
mean(test[test$Canopy.Class=="I", "BA"]); sd(test[test$Canopy.Class=="I", "BA"])
mean(test[test$Canopy.Class=="S", "BA"]); sd(test[test$Canopy.Class=="S", "BA"])

# By Canopy Class only

othervars <- c("Year", "Canopy.Class")
comp.index.mean <- aggregate(test$comp.index, by= test[,othervars], FUN=mean, na.rm=T)
names(comp.index.mean) <- c("Year", "Canopy.Class", "Mean")


comp.index.UB <- aggregate(test$comp.index, by = test[,othervars], FUN= quantile, prob= 0.975, na.rm=T)
summary(comp.index.UB)
names(comp.index.UB) <- c("Year", "Canopy.Class", "UB")

comp.index.LB <- aggregate(test$comp.index, by = test[,othervars], FUN= quantile, prob= 0.025, na.rm=T)
names(comp.index.LB) <- c("Year", "Canopy.Class", "LB")
summary(comp.index.LB)

comp.index <- cbind(comp.index.mean, comp.index.UB$UB, comp.index.LB$LB)
summary(comp.index)
names(comp.index) <- c("Year", "Canopy.Class", "Mean", "UB", "LB")

comp.index$Canopy.Class <- factor(comp.index$Canopy.Class, levels=c("D","I", "S"))

ggplot(data=comp.index) + facet_grid(Canopy.Class~.) +
	geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=Canopy.Class), alpha= 0.45) + 
	geom_line(aes(x=Year, y = Mean, color=Canopy.Class))
	
	
# By Canopy Class and State
othervars <- c("Year", "Site","Canopy.Class")
site.ci.mean <- aggregate(test$comp.index, by= test[,othervars], FUN=mean, na.rm=T)
names(site.ci.mean) <- c("Year", "Site", "Canopy.Class", "Mean")


site.ci.UB <- aggregate(test$comp.index, by = test[,othervars], FUN= quantile, prob= 0.975, na.rm=T)
summary(site.ci.UB)
names(site.ci.UB) <- c("Year", "Site","Canopy.Class", "UB")

site.ci.LB <- aggregate(test$comp.index, by = test[,othervars], FUN= quantile, prob= 0.025, na.rm=T)
names(site.ci.LB) <- c("Year", "Site", "Canopy.Class", "LB")
summary(site.ci.LB)

comp.index.site <- cbind(site.ci.mean, site.ci.UB$UB, site.ci.LB$LB)
summary(comp.index.site)
names(comp.index.site) <- c("Year", "Site", "Canopy.Class", "Mean", "UB", "LB")	
	
comp.index.site$Canopy.Class <- factor(comp.index.site$Canopy.Class, levels= c("D", "I", "S"))	
comp.index.site$Site <- factor(comp.index.site$Site, levels=c("Missouri Ozark", "Morgan Monroe State Park", "Oak Openings Toledo", "Harvard", "Howland"))

ggplot(data=comp.index.site) + facet_grid(Canopy.Class~Site) +
	geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=Canopy.Class), alpha= 0.45) + 
	geom_line(aes(x=Year, y = Mean, color=Canopy.Class))

summary(test)


har.how <- test[test$Site %in% c("Harvard", "Howland") & test$Year==2012,]
summary(har.how)

ross.samp <- test[!test$Site %in% c("Harvard", "Howland") & test$Year==2013,]
summary(ross.samp)

comp.ind.graph <- rbind(har.how, ross.samp)
comp.ind.graph2 <- comp.ind.graph
comp.ind.graph2$Site <- as.factor("All")

comp.ind.graph3 <- rbind(comp.ind.graph, comp.ind.graph2)

comp.ind.graph3$Canopy.Class <- recode(comp.ind.graph3$Canopy.Class, "'D'='Dominant';'I'='Intermediate';'S'='Understory'")
save(comp.ind.graph3, file="processed_data/comp_index_graph.Rdata")
# Violin plots of competition indicies

comp.vio <- ggplot(data=comp.ind.graph3) +# facet_wrap(~Site)+
              	#geom_boxplot(aes(x=Site, y= comp.index, fill=NULL, color=Canopy.Class)) +
                geom_violin(aes(x=Site, y=comp.index, fill=Canopy.Class), scale="width", alpha=0.75)+
                stat_summary(aes(x=Site, y=comp.index, mapping =Canopy.Class), fun.y="quantile",fun.args=list(probs=0.5), geom="point", shape="-", size=17, position=position_dodge(width = 0.9)) +
                stat_summary(aes(x=Site, y=comp.index, mapping =Canopy.Class), fun.y="quantile",fun.args=list(probs=0.95), geom="point", shape="-", size=10, position=position_dodge(width = 0.9)) +
                stat_summary(aes(x=Site, y=comp.index, mapping =Canopy.Class), fun.y="quantile",fun.args=list(probs=0.05), geom="point", shape="-", size=10, position=position_dodge(width = 0.9)) +              
                labs(x="Canopy Class", y="Competition Index") +
                scale_fill_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
                scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
                theme(axis.line=element_line(color="black"), 
                      panel.grid.major=element_blank(), 
                      panel.grid.minor=element_blank(), 
                      panel.border=element_blank(),  
                      panel.background=element_blank(), 
                      axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
                      axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
                      strip.text=element_text(face="bold", size=rel(1.0)),
                      axis.line.x = element_line(color="black", size = 0.5),
                      axis.line.y = element_line(color="black", size = 0.5),
                      legend.position="top",
                      legend.key.size = unit(0.75, "cm"),
                      legend.text = element_text(size=rel(1.1)),
                      legend.key = element_rect(fill = "white")) + 
                guides(color=guide_legend(nrow=1)) +
                theme(axis.title.y= element_text(size=rel(1.1), face="bold")) +
                theme(axis.title.x= element_text(size=rel(1.1), face="bold"))

pdf("figures/Prelim_Figures/pub_figs/comp_index.pdf", width=13, height=8.5)
  comp.vio
dev.off()

# Histogram of tree of diameters by canopy class
test$Site <- factor(test$Site, levels=c("Missouri Ozark", "Morgan Monroe State Park", "Oak Openings Toledo", "Harvard", "Howland"))

ggplot(data=test) + facet_grid(~Site) + 
	geom_histogram(aes(x=DBH..cm., fill=Canopy.Class))

ggplot(data=test) + #facet_grid(~Site) + 
	geom_histogram(aes(x=DBH..cm., fill=Canopy.Class), binwidth=1)

ggplot(data=test) + facet_grid(Site~.) + 
	geom_density(aes(x=DBH..cm.,fill=Canopy.Class), position="stack", alpha=0.9) +
	
	labs(x="DBH (CM)", y="Density") +
		
	theme(axis.line=element_line(color="black"), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),  
		panel.background=element_blank(), 
		axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
		axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
		strip.text=element_text(face="bold", size=rel(1.0)),
		axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
        guides(color=guide_legend(nrow=1)) +
	theme(axis.title.y= element_text(size=rel(1.1), face="bold"))
	
	

#############################
# Time series of diameter reconstructions
#############################

othervars <- c("Year", "Site",  "Canopy.Class")
dbh.recon.mean <- aggregate(test$dbh.recon, by= test[,othervars], FUN=mean, na.rm=T)
names(dbh.recon.mean) <- c("Year", "Site","Canopy.Class", "Mean")


dbh.recon.UB <- aggregate(test$dbh.recon, by = test[,othervars], FUN= quantile, prob= 0.975, na.rm=T)
summary(dbh.recon.UB)
names(dbh.recon.UB) <- c("Year", "Site","Canopy.Class", "UB")

dbh.recon.LB <- aggregate(test$dbh.recon, by = test[,othervars], FUN= quantile, prob= 0.025, na.rm=T)
names(dbh.recon.LB) <- c("Year", "Site","Canopy.Class", "LB")
summary(dbh.recon.LB)

dbh.recon <- cbind(dbh.recon.mean, dbh.recon.UB$UB, dbh.recon.LB$LB)
summary(dbh.recon)
names(dbh.recon) <- c("Year", "Site","Canopy.Class", "Mean", "UB", "LB")

dbh.recon$Canopy.Class <- factor(dbh.recon$Canopy.Class, levels=c("D","I", "S"))

save(dbh.recon, file="processed_data/dbh_recon_graph.Rdata")
ggplot(data=dbh.recon[dbh.recon$Year >= 1950,]) + facet_grid(Canopy.Class~Site) +
	geom_ribbon(aes(x=Year, ymin=LB, ymax=UB, fill=Canopy.Class), alpha= 0.45) + 
	geom_line(aes(x=Year, y = Mean, color=Canopy.Class))
