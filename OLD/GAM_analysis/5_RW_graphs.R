library(dplR)
library(ggplot2)
library(car)

#--------------------------------------------------------
# Want to graph the mean rw and 95% Ci for each site
#--------------------------------------------------------
load("processed_data/gam2_density_graph_data.Rdata")

data.use <- read.csv("processed_data/AllSites_tree_plus_climate.csv", header=T)
summary(data.use)

# Subsetting Live trees
data.use <- data.use[data.use$Live.Dead=="LIVE" & !data.use$Canopy.Class=="F",]

data.use$group <- data.use$Species
data.use$group <- recode(data.use$group, "'CAOV' = 'CARYA'; 'CACO' = 'CARYA'; 
  							'CATE' = 'CARYA'; 'ACSAC' = 'ACSA'; 'BEAL' = 'BETULA'; 'BELE' = 'BETULA'; 'QUMU' = 'QUAL'")

data.use$Canopy.Class <- recode(data.use$Canopy.Class, "'C' = 'D'")
summary(data.use)

group.use <- c("ACRU", "ACSA", "BETULA", "CARYA", "FAGR", "FRAX", "PIST", "QUAL", "QURU", "QUVE", "SAAL", "TSCA", "ULRU")

data.use <- data.use[data.use$group %in% group.use,]

# 
# Aggregating by site by year to get the mean RW for each year for each canopy Class
# use teh clim.dev stuff for now from gam2.weights
cc.mean.ba.index <- data.frame(mean = aggregate(gam2.graph$Clim.Dev, by=gam2.graph[,c("Canopy.Class", "Year")], FUN=mean, na.rm=T))
names(cc.mean.ba.index)<- c("Canopy.Class", "Year", "ba.index.mean")                  
summary(cc.mean.ba.index)


# Generating the 95% CI for ba.index at each site for each year                   
# I was having trouble graphing if I generated both quantiles at the same time, so I am doing them separately and merging the upper and lower into 1 DF
cc.lwr.ba.index <- aggregate(gam2.graph$Clim.Dev, by=list(gam2.graph$Canopy.Class, gam2.graph$Year), FUN=quantile, probs=0.025, na.rm=T)
cc.upr.ba.index <- aggregate(gam2.graph$Clim.Dev, by=list(gam2.graph$Canopy.Class, gam2.graph$Year), FUN=quantile, probs=0.975, na.rm=T)                   

names(cc.lwr.ba.index) <- c("Canopy.Class", "Year", "lwr")
names(cc.upr.ba.index) <- c("Canopy.Class", "Year", "upr")

#Merging CI's
cc.ci.ba.index <- merge(cc.lwr.ba.index, cc.upr.ba.index, all.x=T, all.y=F)
summary(cc.ci.ba.index)

# Merging CI with Mean

cc.mean.ba.index <- merge(cc.mean.ba.index, cc.ci.ba.index, all.x=T, all.y=F)
summary(cc.mean.ba.index)
#sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

ba.index.graph <- ggplot(data=cc.mean.ba.index[cc.mean.ba.index$Year>=1950 & cc.mean.ba.index$Year<=2012,]) + facet_grid(Canopy.Class~., scales="free_x") +
  geom_ribbon(aes(x=Year, ymin=lwr, ymax=upr, fill=Canopy.Class), alpha=0.5) +
  geom_line(aes(x=Year, y=ba.index.mean, color=Canopy.Class), size=1) +
  geom_hline(yintercept=0,color="black", linetype="dashed", size=0.2) +
  scale_fill_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
  scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
  labs(x="Year", y="BAI-Index")+
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=rel(2)), 
        axis.text.y=element_text(angle=0, color="black", size=rel(2)), 
        strip.text=element_text(face="bold", size=rel(1.0)),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
  guides(color=guide_legend(nrow=1, title=""), fill=guide_legend(title="")) +
  theme(axis.title.y= element_text(size=rel(2.1), face="bold"))+
  theme(axis.title.x= element_text(size=rel(2.1), face="bold"))

png(filename="figures/submission1_figs/AGU_BA_index.png", width=13, height=8.5, units="in", res=600)
ba.index.graph
dev.off()

# Gettign mean BAI
load("processed_data/gam2_density_graph_data.Rdata")



# # # Subsetting Live trees
# # data.use <- data.use[data.use$Live.Dead=="LIVE" & !data.use$Canopy.Class=="F",]
# 
# data.use$group <- data.use$Species
# data.use$group <- recode(data.use$group, "'CAOV' = 'CARYA'; 'CACO' = 'CARYA'; 
#                          'CATE' = 'CARYA'; 'ACSAC' = 'ACSA'; 'BEAL' = 'BETULA'; 'BELE' = 'BETULA'; 'QUMU' = 'QUAL'")
# 
# data.use$Canopy.Class <- recode(data.use$Canopy.Class, "'C' = 'D'")
# summary(data.use)

# group.use <- c("ACRU", "ACSA", "BETULA", "CARYA", "FAGR", "FRAX", "PIST", "QUAL", "QURU", "QUVE", "SAAL", "TSCA", "ULRU")
# 
# data.use <- data.use[data.use$group %in% group.use,]

# 
# Aggregating by site by year to get the mean RW for each year for each canopy Class
# use teh clim.dev stuff for now from gam2.weights
cc.mean.bai <- data.frame(mean = aggregate(gam2.graph$BA.inc, by=gam2.graph[,c("Canopy.Class", "Year")], FUN=mean, na.rm=T))
names(cc.mean.bai)<- c("Canopy.Class", "Year", "BA.inc")                  
summary(cc.mean.bai)


# Generating the 95% CI for bai at each site for each year                   
# I was having trouble graphing if I generated both quantiles at the same time, so I am doing them separately and merging the upper and lower into 1 DF
cc.lwr.bai <- aggregate(gam2.graph$BA.inc, by=list(gam2.graph$Canopy.Class, gam2.graph$Year), FUN=quantile, probs=0.025, na.rm=T)
cc.upr.bai <- aggregate(gam2.graph$BA.inc, by=list(gam2.graph$Canopy.Class, gam2.graph$Year), FUN=quantile, probs=0.975, na.rm=T)                   

names(cc.lwr.bai) <- c("Canopy.Class", "Year", "lwr")
names(cc.upr.bai) <- c("Canopy.Class", "Year", "upr")

#Merging CI's
cc.ci.bai <- merge(cc.lwr.bai, cc.upr.bai, all.x=T, all.y=F)
summary(cc.ci.bai)

# Merging CI with Mean

cc.mean.bai <- merge(cc.mean.bai, cc.ci.bai, all.x=T, all.y=F)
summary(cc.mean.bai)
#sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

bai.graph <- ggplot(data=cc.mean.bai[cc.mean.bai$Year>=1950 & cc.mean.bai$Year<=2012,]) + facet_grid(Canopy.Class~., scales="free_x") +
    geom_ribbon(aes(x=Year, ymin=lwr, ymax=upr, fill=Canopy.Class), alpha=0.5) +
    geom_line(aes(x=Year, y=BA.inc, color=Canopy.Class), size=1)+
    geom_hline(yintercept = 0, color="black", linetype="dashed", size=0.2) +
    labs(x="Year", y=expression(bold(paste("BAI (mm"^"2",")"))))+
    scale_fill_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
    scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
    theme(axis.line=element_line(color="black"), 
          panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank(), 
          panel.border=element_blank(),  
          panel.background=element_blank(), 
          axis.text.x=element_text(angle=0, color="black", size=rel(2)), 
          axis.text.y=element_text(angle=0, color="black", size=rel(2)), 
          strip.text=element_text(face="bold", size=rel(1.0)),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          legend.position="top",
          legend.key.size = unit(0.75, "cm"),
          legend.text = element_text(size=rel(1.1)),
          legend.key = element_rect(fill = "white")) + 
    guides(color=guide_legend(nrow=1, title=""), fill=guide_legend(title="")) +
    theme(axis.title.y= element_text(size=rel(2.1), face="bold"))+
    theme(axis.title.x= element_text(size=rel(2.1), face="bold"))

png(filename="figures/submission1_figs/AGU_BAI.png", width=13, height=8.5, units="in", res=600)
bai.graph
dev.off()



#-----------------------------------------------
# Now looking at just the mean RW for the total site for each year
  
# Aggregating by site by year to get the mean RW for each year for each group by canopy class
# mean.rw <- data.frame(mean = aggregate(data.use$RW, by=list(data/use$Speciesdata.use$Site, data.use$Year), FUN=mean, na.rm=T))



mean.rw <- aggregate(data.use$RW, by=data.use[, c("Site", "group", "Canopy.Class", "Year")], FUN=mean, na.rm=T)

names(mean.rw)[names(mean.rw)=="x"]<- "rw.mean"     
summary(mean.rw)


# Generating the 95% CI for RW at each site for each year                   
# I was having trouble graphing if I generated both quantiles at the same time, so I am doing them separately and merging the upper and lower into 1 DF
lwr.rw <- aggregate(data.use$RW, by=data.use[,c("Site", "group", "Canopy.Class", "Year")], FUN=quantile, probs=0.025, na.rm=T)
upr.rw <- aggregate(data.use$RW, by=data.use[, c("Site", "group", "Canopy.Class", "Year")], FUN=quantile, probs=0.975, na.rm=T)                   

names(lwr.rw)[names(lwr.rw)=="x"]<- c("rw.lwr")      
names(upr.rw)[names(upr.rw)=="x"]<- c("rw.upr")

#Merging CI's
ci.rw <- merge(lwr.rw, upr.rw, all.x=T, all.y=F)
summary(ci.rw)

# Merging CI with Mean

mean.rw <- merge(mean.rw, ci.rw, all.x=T, all.y=F)
summary(mean.rw)

sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

#species.use <- c("TSCA", "QURU", "ACRU", "BEAL", "ACSA", "LITU", "QUAL", "CAOV", "CACO", "CATE", "JUVI", "QUVE", "PCRU", "THOC", "PIST")


mean.rw <- mean.rw[mean.rw$Site %in% sites.use,]
#mean.rw <- mean.rw[mean.rw$Species %in% species.use,]

group.colors <- read.csv("spp.Colors.csv", header=T)	
summary(group.colors)	

group.fig <- unique(data.use$group)
group.fig <- group.fig[order(group.fig)]
colors.use <- as.vector(c(paste(group.colors[group.colors$Species %in% group.fig, "color"])))
		


ggplot(data=mean.rw) + facet_grid(Canopy.Class~Site, scales="free_x") +
  geom_ribbon(aes(x=Year, ymin=rw.lwr, ymax=rw.upr, fill=group), alpha=0.5) +
  geom_line(aes(x=Year, y=rw.mean, color=group))  +
			 scale_color_manual(values=colors.use) +
			 scale_fill_manual(values=colors.use)


#-----------------------------------------------
# 
  
# Aggregating by site by year to get the mean RW for each year for each group
# mean.rw <- data.frame(mean = aggregate(data.use$RW, by=list(data/use$Speciesdata.use$Site, data.use$Year), FUN=mean, na.rm=T))



mean.rw <- aggregate(data.use$RW, by=data.use[, c("Site", "group", "Year")], FUN=mean, na.rm=T)

names(mean.rw)[names(mean.rw)=="x"]<- "rw.mean"
summary(mean.rw)


# Generating the 95% CI for RW at each site for each year                   
# I was having trouble graphing if I generated both quantiles at the same time, so I am doing them separately and merging the upper and lower into 1 DF
lwr.rw <- aggregate(data.use$RW, by=data.use[,c("Site", "group", "Year")], FUN=quantile, probs=0.025, na.rm=T)
upr.rw <- aggregate(data.use$RW, by=data.use[, c("Site", "group", "Year")], FUN=quantile, probs=0.975, na.rm=T)                   

names(lwr.rw)[names(lwr.rw)=="x"]<- c("rw.lwr")      
names(upr.rw)[names(upr.rw)=="x"]<- c("rw.upr")

#Merging CI's
ci.rw <- merge(lwr.rw, upr.rw, all.x=T, all.y=F)
summary(ci.rw)

# Merging CI with Mean

mean.rw <- merge(mean.rw, ci.rw, all.x=T, all.y=F)
summary(mean.rw)

sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

#species.use <- c("TSCA", "QURU", "ACRU", "BEAL", "ACSA", "LITU", "QUAL", "CAOV", "CACO", "CATE", "JUVI", "QUVE", "PCRU", "THOC", "PIST")


mean.rw <- mean.rw[mean.rw$Site %in% sites.use,]
#mean.rw <- mean.rw[mean.rw$Species %in% species.use,]

group.colors <- read.csv("spp.Colors.csv", header=T)	
summary(group.colors)	

group.fig <- unique(data.use$group)
group.fig <- group.fig[order(group.fig)]
colors.use <- as.vector(c(paste(group.colors[group.colors$Species %in% group.fig, "color"])))
		


ggplot(data=mean.rw) + facet_grid(group~Site, scales="free_x") +
  geom_ribbon(aes(x=Year, ymin=rw.lwr, ymax=rw.upr, fill=group), alpha=0.5) +
  geom_line(aes(x=Year, y=rw.mean, color=group))  +
			 scale_color_manual(values=colors.use) +
			 scale_fill_manual(values=colors.use)
#-----------------------------------------------
# Now looking at just the mean RW for the total site for each year
  
# Aggregating by site by year to get the mean RW for each year for each site
# mean.rw <- data.frame(mean = aggregate(data.use$RW, by=list(data/use$Speciesdata.use$Site, data.use$Year), FUN=mean, na.rm=T))



mean.rw <- aggregate(data.use$RW, by=data.use[, c("Site", "Year")], FUN=mean, na.rm=T)

names(mean.rw)[names(mean.rw)=="x"]<- "rw.mean"                  
summary(mean.rw)


# Generating the 95% CI for RW at each site for each year                   
# I was having trouble graphing if I generated both quantiles at the same time, so I am doing them separately and merging the upper and lower into 1 DF
lwr.rw <- aggregate(data.use$RW, by=data.use[,c("Site", "Year")], FUN=quantile, probs=0.025, na.rm=T)
upr.rw <- aggregate(data.use$RW, by=data.use[, c("Site", "Year")], FUN=quantile, probs=0.975, na.rm=T)                   

names(lwr.rw)[names(lwr.rw)=="x"]<- c("rw.lwr")      
names(upr.rw)[names(upr.rw)=="x"]<- c("rw.upr")

#Merging CI's
ci.rw <- merge(lwr.rw, upr.rw, all.x=T, all.y=F)
summary(ci.rw)

# Merging CI with Mean

mean.rw <- merge(mean.rw, ci.rw, all.x=T, all.y=F)
summary(mean.rw)

sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

#species.use <- c("TSCA", "QURU", "ACRU", "BEAL", "ACSA", "LITU", "QUAL", "CAOV", "CACO", "CATE", "JUVI", "QUVE", "PCRU", "THOC", "PIST")


mean.rw <- mean.rw[mean.rw$Site %in% sites.use,]
#mean.rw <- mean.rw[mean.rw$Species %in% species.use,]

group.colors <- read.csv("spp.Colors.csv", header=T)	
summary(group.colors)	

group.fig <- unique(data.use$group)
group.fig <- group.fig[order(group.fig)]
colors.use <- as.vector(c(paste(group.colors[group.colors$Species %in% group.fig, "color"])))
		


ggplot(data=mean.rw) + facet_grid(Site~., scales="free_x") +
  geom_ribbon(aes(x=Year, ymin=rw.lwr, ymax=rw.upr), alpha=0.5) +
  geom_line(aes(x=Year, y=rw.mean))  +
			 scale_color_manual(values=colors.use) +
			 scale_fill_manual(values=colors.use)