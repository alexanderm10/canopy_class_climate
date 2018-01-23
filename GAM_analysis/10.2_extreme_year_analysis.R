library(ggplot2)
require(mgcv)
require(lsmeans)
require(car)
require(moments)
require(reshape)
# Running ANOVA between Extreme years and normal years BAI for Canopy.Class
load("processed_data/gam2_weights_processed.Rdata")
summary(gam2.weights)

# Running ANOVA's to determine differences between ambient years and extreme years
# Using a lme here to take into account the differences between sites as random effeects
summary(gam2.weights)
# Truncating to 1950 becasue that's the extent of the climate analysis
gam2.weights <- gam2.weights[gam2.weights$Year >=1950 & gam2.weights$Year <=2012,]


# cr.test <- lme(log(BA.inc) ~ Precip.Mark*Canopy.Class, random=list(group=~1, Site=~1, PlotID=~1, TreeID=~1), data=gam2.weights)
# lsmeans(cr.test, pairwise~Precip.Mark*Canopy.Class, adjust="tukey")	
# cr.test2 <- lme(log(BA.inc) ~ Temp.Mark*Canopy.Class, random=list(TreeID=~1), data=gam2.weights)
# lsmeans(cr.test2, pairwise~Temp.Mark*Canopy.Class, adjust="tukey")	

# Same fixed effects structure as the gams; add an interaction between temp & precip 
# because we fit those jointly in the gam
# cr.testD <- aov(log(BA.inc) ~ Precip.Mark*Temp.Mark + Site + PlotID + TreeID + group, data=gam2.weights[gam2.weights$Canopy.Class=="D",])
# cr.testI <- aov(log(BA.inc) ~ Precip.Mark*Temp.Mark + Site + PlotID + TreeID + group, data=gam2.weights[gam2.weights$Canopy.Class=="I",])
# cr.testS <- aov(log(BA.inc) ~ Precip.Mark*Temp.Mark + Site + PlotID + TreeID + group, data=gam2.weights[gam2.weights$Canopy.Class=="S",])

# gam2.weights$BA.inc.Clim.log <- log(gam2.weights$BA.inc) - gam2.weights$fit.intercept - gam2.weights$fit.dbh.recon - gam2.weights$fit.Year
# gam2.weights$BA.inc.Clim2 <- exp(gam2.weights$BA.Inc.Clim.log)
# summary(gam2.weights)

# Going to run the anova's on the model detrended data
cr.testD <- aov(log(BA.inc.Clim) ~ Precip.Mark*Temp.Mark, data=gam2.weights[gam2.weights$Canopy.Class=="D",])
cr.testI <- aov(log(BA.inc.Clim) ~ Precip.Mark*Temp.Mark, data=gam2.weights[gam2.weights$Canopy.Class=="I",])
cr.testS <- aov(log(BA.inc.Clim) ~ Precip.Mark*Temp.Mark, data=gam2.weights[gam2.weights$Canopy.Class=="S",])

# lm.testD <- lm(log(BA.inc.Clim) ~ Precip.Mark + Temp.Mark + Site, data=gam2.weights[gam2.weights$Canopy.Class=="D",])
# lm.testD2 <- lme(log(BA.inc.Clim) ~ Precip.Mark + Temp.Mark, random=list(Site=~1), data=gam2.weights[gam2.weights$Canopy.Class=="D",])
# summary(lm.testD)
# summary(lm.testD2)
# anova(lm.testD2)


# Comparing the log-transformed
# cr.testD2 <- aov(BA.inc.Clim.log ~ Precip.Mark+Temp.Mark, data=gam2.weights[gam2.weights$Canopy.Class=="D",])
# cr.testI2 <- aov(BA.inc.Clim.log ~ Precip.Mark+Temp.Mark, data=gam2.weights[gam2.weights$Canopy.Class=="I",])
# cr.testS2 <- aov(BA.inc.Clim.log ~ Precip.Mark+Temp.Mark, data=gam2.weights[gam2.weights$Canopy.Class=="S",])
summary(cr.testD2)
# summary(cr.testI2)
# summary(cr.testS2)
# hist(resid(cr.testD2))

plot(resid(cr.testD) ~ predict(cr.testD))
plot(resid(cr.testI) ~ predict(cr.testI))
plot(resid(cr.testS) ~ predict(cr.testS))

hist(resid(cr.testD))
hist(resid(cr.testI))
hist(resid(cr.testS))


summary(cr.testD)
tukey.d <- TukeyHSD(cr.testD)
tukey.d["Precip.Mark:Temp.Mark"] # not significant, so we won't look at it
tukey.d["Precip.Mark"] # Wet grows more than ambient
tukey.d["Temp.Mark"] # cold & hot grow more than ambient??
# Saving table to publication folder
#write.table(summary(cr.testD), file="~/PhD/publications/2016/Ch2_multiple_limiting_factors/D_anova.txt")

summary(cr.testI)
tukey.i <- TukeyHSD(cr.testI)
tukey.i["Precip.Mark:Temp.Mark"] # not significant, so we won't look at it
tukey.i["Precip.Mark"]
tukey.i["Temp.Mark"]

summary(cr.testS)
tukey.s <- TukeyHSD(cr.testS)
tukey.s["Precip.Mark:Temp.Mark"] # Significant (for better or worse)
tukey.s["Precip.Mark"] # No dif
tukey.s["Temp.Mark"] # hot grows less


# lsmeans(cr.testD, pairwise~Precip.Mark, adjust="tukey")	
# lsmeans(cr.testD, pairwise~Temp.Mark, adjust="tukey")	
# lsmeans(cr.testI, pairwise~Precip.Mark, adjust="tukey")	
# lsmeans(cr.testS, pairwise~Precip.Mark, adjust="tukey")	

###########################################################
# probabity density functions for extreme years
###########################################################
load("processed_data/climate_markeryears.Rdata")
summary(gam2.weights)

# Making a combined temp/precip marker year
gam2.weights$mix.mark <- as.factor(paste(gam2.weights$Temp.Mark, gam2.weights$Precip.Mark, sep="-"))
summary(gam2.weights)

# calculating median BAI for temp and precip
median.temp.s <- data.frame(type= unique(gam2.weights$Temp.Mark))
for(i in unique(gam2.weights$Temp.Mark)){
  median.temp.s[median.temp.s$type==i,"median"] <- median(gam2.weights[gam2.weights$Temp.Mark==i & gam2.weights$Canopy.Class=="S","BA.inc.Clim"])
  # median.temp.s[median.temp.s$type==i,"median2"] <- median(gam2.weights[gam2.weights$Temp.Mark==i & gam2.weights$Canopy.Class=="S","BA.inc.Clim2"])
  # median.temp.s[median.temp.s$type==i,"median.log"] <- median(gam2.weights[gam2.weights$Temp.Mark==i & gam2.weights$Canopy.Class=="S","BA.inc.Clim.log"])
  #median.temp.s[median.temp.s$type==i,"median.rel"] <- median(gam2.weights[gam2.weights$Temp.Mark==i & gam2.weights$Canopy.Class=="S","Clim.Rel"])
}
median.temp.s$Canopy.Class <- as.factor("S")
median.temp.i <- data.frame(type= unique(gam2.weights$Temp.Mark))
for(i in unique(gam2.weights$Temp.Mark)){
  median.temp.i[median.temp.s$type==i,"median"] <- median(gam2.weights[gam2.weights$Temp.Mark==i & gam2.weights$Canopy.Class=="I","BA.inc.Clim"])
  # median.temp.i[median.temp.s$type==i,"median2"] <- median(gam2.weights[gam2.weights$Temp.Mark==i & gam2.weights$Canopy.Class=="I","BA.inc.Clim2"])
  # median.temp.i[median.temp.s$type==i,"median.log"] <- median(gam2.weights[gam2.weights$Temp.Mark==i & gam2.weights$Canopy.Class=="I","BA.inc.Clim.log"])
}
median.temp.i$Canopy.Class <- as.factor("I")

median.temp.d <- data.frame(type= unique(gam2.weights$Temp.Mark))
for(i in unique(gam2.weights$Temp.Mark)){
  median.temp.d[median.temp.s$type==i,"median"] <- median(gam2.weights[gam2.weights$Temp.Mark==i & gam2.weights$Canopy.Class=="D","BA.inc.Clim"])
  # median.temp.d[median.temp.s$type==i,"median2"] <- median(gam2.weights[gam2.weights$Temp.Mark==i & gam2.weights$Canopy.Class=="D","BA.inc.Clim2"])
  # median.temp.d[median.temp.s$type==i,"median.log"] <- median(gam2.weights[gam2.weights$Temp.Mark==i & gam2.weights$Canopy.Class=="D","BA.inc.Clim.log"])
}
median.temp.d$Canopy.Class <- as.factor("D")

median.temp <- rbind(median.temp.d, median.temp.i, median.temp.s)

#############
# Precip
median.precip.s <- data.frame(type= unique(gam2.weights$Precip.Mark))
for(i in unique(gam2.weights$Precip.Mark)){
  median.precip.s[median.precip.s$type==i,"median"] <- median(gam2.weights[gam2.weights$Precip.Mark==i & gam2.weights$Canopy.Class=="S","BA.inc.Clim"])
  # median.precip.s[median.precip.s$type==i,"median2"] <- median(gam2.weights[gam2.weights$Precip.Mark==i & gam2.weights$Canopy.Class=="S","BA.inc.Clim2"])
}
median.precip.s$Canopy.Class <- as.factor("S")
median.precip.i <- data.frame(type= unique(gam2.weights$Precip.Mark))
for(i in unique(gam2.weights$Precip.Mark)){
  median.precip.i[median.precip.s$type==i,"median"] <- median(gam2.weights[gam2.weights$Precip.Mark==i & gam2.weights$Canopy.Class=="I","BA.inc.Clim"])
  # median.precip.i[median.precip.s$type==i,"median2"] <- median(gam2.weights[gam2.weights$Precip.Mark==i & gam2.weights$Canopy.Class=="I","BA.inc.Clim2"])
}
median.precip.i$Canopy.Class <- as.factor("I")

median.precip.d <- data.frame(type= unique(gam2.weights$Precip.Mark))
for(i in unique(gam2.weights$Precip.Mark)){
  median.precip.d[median.precip.s$type==i,"median"] <- median(gam2.weights[gam2.weights$Precip.Mark==i & gam2.weights$Canopy.Class=="D","BA.inc.Clim"])
  # median.precip.d[median.precip.s$type==i,"median2"] <- median(gam2.weights[gam2.weights$Precip.Mark==i & gam2.weights$Canopy.Class=="D","BA.inc.Clim2"])
}
median.precip.d$Canopy.Class <- as.factor("D")

median.precip <- rbind(median.precip.d, median.precip.i, median.precip.s)

save(median.temp, file="processed_data/median_temp.Rdata")
save(median.precip, file="processed_data/median_precip.Rdata")

save(gam2.weights, file="processed_data/gam2_weights_graph.Rdata")
interact <- c("A-A", "hot-wet", "A-wet", "A-dry")
ggplot(gam2.weights[gam2.weights$mix.mark %in% interact,]) + facet_grid(Canopy.Class~.) +
	geom_density(aes(x=log(BA.inc.Clim),color=mix.mark, fill=mix.mark), alpha=0.1) +
  #geom_vline(data=median.temp, aes(xintercept=median, color=type)) +
  # scale_color_manual(values=c("grey50", "blue", "red")) +
	# scale_fill_manual(values=c("grey50", "blue", "red")) +
	xlim(-2,2) +
	labs(x= "log(BAI)", y="Density") +
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
	theme(axis.title.y= element_text(size=rel(1.1), face="bold"))+
	theme(axis.title.x= element_text(size=rel(1.1), face="bold"))

ggplot(gam2.weights) + facet_grid(Canopy.Class~.) +
	geom_density(aes(x=BA.inc.Clim,color=Precip.Mark, fill=Precip.Mark), alpha=0.1) +
  geom_vline(data=median.precip, aes(xintercept=median, color=type)) +
  scale_color_manual(values=c("grey50", "brown", "darkgreen")) +
	scale_fill_manual(values=c("grey50", "brown", "darkgreen")) +
	xlim(-20,20) +
	labs(x= "BAI", y="Density") +
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
	theme(axis.title.y= element_text(size=rel(1.1), face="bold"))+
	theme(axis.title.x= element_text(size=rel(1.1), face="bold"))


summary(exp(gam2.weights$BA.inc.Clim.log))

# ggplot(gam2.weights) + facet_grid(Canopy.Class~.) +
#   geom_density(aes(x=BA.inc.Clim2,color=Temp.Mark, fill=Temp.Mark), alpha=0.1) +
#   geom_vline(data=median.temp, aes(xintercept=median2, color=type)) +
#   scale_color_manual(values=c("grey50", "blue", "red")) +
#   scale_fill_manual(values=c("grey50", "blue", "red")) +
#   coord_cartesian(xlim=c(0,4)) +
#   # scale_x_continuous(limits=c(0,4)) +
#   labs(x= "BAI", y="Density") +
#   theme(axis.line=element_line(color="black"), 
#         panel.grid.major=element_blank(), 
#         panel.grid.minor=element_blank(), 
#         panel.border=element_blank(),  
#         panel.background=element_blank(), 
#         axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
#         axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
#         strip.text=element_text(face="bold", size=rel(1.0)),
#         axis.line.x = element_line(color="black", size = 0.5),
#         axis.line.y = element_line(color="black", size = 0.5),
#         legend.position="top",
#         legend.key.size = unit(0.75, "cm"),
#         legend.text = element_text(size=rel(1.1)),
#         legend.key = element_rect(fill = "white")) + 
#   guides(color=guide_legend(nrow=1)) +
#   theme(axis.title.y= element_text(size=rel(1.1), face="bold"))+
#   theme(axis.title.x= element_text(size=rel(1.1), face="bold"))
# 


# Calculating median growth for each extreme year combination
marker.median.s <- data.frame(type= unique(gam2.weights$mix.mark))

for(i in unique(gam2.weights$mix.mark)){
	marker.median.s[marker.median.s$type==i,"median"] <- median(gam2.weights[gam2.weights$mix.mark==i & gam2.weights$Canopy.Class=="S","BA.inc.Clim"])
}
marker.median.s$Canopy.Class <- as.factor("S")

marker.median.d <- data.frame(type= unique(gam2.weights$mix.mark))

for(i in unique(gam2.weights$mix.mark)){
	marker.median.d[marker.median.s$type==i,"median"] <- median(gam2.weights[gam2.weights$mix.mark==i & gam2.weights$Canopy.Class=="D","BA.inc.Clim"])
}
marker.median.d$Canopy.Class <- as.factor("D")

marker.median.i <- data.frame(type= unique(gam2.weights$mix.mark))

for(i in unique(gam2.weights$mix.mark)){
	marker.median.i[marker.median.s$type==i,"median"] <- median(gam2.weights[gam2.weights$mix.mark==i & gam2.weights$Canopy.Class=="I","BA.inc.Clim"])
}
marker.median.i$Canopy.Class <- as.factor("I")

marker.median <- rbind(marker.median.i, marker.median.d, marker.median.s)

ggplot(gam2.weights[gam2.weights$mix.mark %in% c("A-A", "hot-wet", "hot-dry", "hot-A") & gam2.weights$Site=="Harvard",]) + facet_grid(Canopy.Class~Site) +
	geom_density(aes(x=BA.inc.Clim,color=mix.mark,fill=mix.mark), alpha=0.1) +
	geom_vline(data=marker.median[marker.median$type %in% c("A-A", "hot-wet", "hot-dry", "hot-A"),], aes(xintercept=median, color=type))+
	xlim(-20,20)+ 
	labs(x= "BAI", y="Density") +
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
	theme(axis.title.y= element_text(size=rel(1.1), face="bold"))+
	theme(axis.title.x= element_text(size=rel(1.1), face="bold"))



