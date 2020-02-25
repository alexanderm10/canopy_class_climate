# Different Numbers for results section of Ch. 2 Manuscript

require(ggplot2)
require(mgcv)
require(lsmeans)
require(car)

#all input data used in GAMMS
load("ch2_combined_data_use.Rdata")
summary(test)

test$state <- recode(test$Site, c("'Missouri Ozark'='MO';'Morgan Monroe State Park'='IN';'Oak Openings Toledo'='OH';'Harvard'='MA';'Howland'='ME'"))
summary(test)


plot.data <-read.csv("raw_input_files/DOE_plus_valles.csv", header=T)
# summary(plot.data)

sites <- c("MO", "IN", "OH", "MA", "ME")
plot.data$Site..Tower. <- recode(plot.data$Site..Tower., "'Missouri Ozark'='MO';'Morgan Monroe State Park'='IN';'Oak Openings Toledo'='OH';'Harvard Forest (Tower Plot 1)'='MA';'Harvard Forest (Tower Plot 2)'='MA'; 'Howland'='ME'")
plot.data <- plot.data[plot.data$Site..Tower. %in% sites,]
plot.data$Site..Tower. <- droplevels(plot.data$Site..Tower.)

summary(plot.data)
density.data<- data.frame(Site = sites,
						  dens.mean = NA,
						  dens.sd = NA)
dim(density.data)
for(s in sites){
	density.data[density.data$Site==s, "dens.mean"] <- mean(plot.data[plot.data$Site..Tower.==s, "Density.Live..stems.ha."], na.rm=T)
	
	density.data[density.data$Site==s, "dens.sd"] <- sd(plot.data[plot.data$Site..Tower.==s, "Density.Live..stems.ha."], na.rm=T)
}
density.data




#############################################
# Site Demographic Characteristics
#############################################
#---------------------------------
# Dominant Trees

# MO
summary(unique(test[test$Canopy.Class=="D" & test$state=="MO", c("TreeID","group")]))
# IN
summary(unique(test[test$Canopy.Class=="D" & test$state=="IN", c("TreeID","group")]))
# OH
summary(unique(test[test$Canopy.Class=="D" & test$state=="OH", c("TreeID","group")]))
# MA
summary(unique(test[test$Canopy.Class=="D" & test$state=="MA", c("TreeID","group")]))
# ME
summary(unique(test[test$Canopy.Class=="D" & test$state=="ME", c("TreeID","group")]))

#---------------------------------
# Intermediate Trees
# MO
summary(unique(test[test$Canopy.Class=="I" & test$state=="MO", c("TreeID","group")]))
# IN
summary(unique(test[test$Canopy.Class=="I" & test$state=="IN", c("TreeID","group")]))
# 
summary(unique(test[test$Canopy.Class=="I" & test$state=="OH", c("TreeID","group")]))
# MA
summary(unique(test[test$Canopy.Class=="I" & test$state=="MA", c("TreeID","group")]))
# ME
summary(unique(test[test$Canopy.Class=="I" & test$state=="ME", c("TreeID","group")]))



#---------------------------------
# Suppressed Trees
# MO
summary(unique(test[test$Canopy.Class=="S" & test$state=="MO", c("TreeID","group")]))
# 
summary(unique(test[test$Canopy.Class=="S" & test$state=="IN", c("TreeID","group")]))
# OH
summary(unique(test[test$Canopy.Class=="S" & test$state=="OH", c("TreeID","group")]))
# MA
summary(unique(test[test$Canopy.Class=="S" & test$state=="MA", c("TreeID","group")]))
# ME
summary(unique(test[test$Canopy.Class=="S" & test$state=="ME", c("TreeID","group")]))


#############################################
# Site Climate Characteristics
#############################################

# Missouri 

summary(test[test$state=="MO",])


# Indiana
summary(test[test$state=="IN",])


# Ohio
summary(test[test$state=="OH",])


# Harvard
summary(test[test$state=="MA",])

# Howland
summary(test[test$state=="ME",])

#############################################
#  GAMM weights descriptions
#############################################
library(dplR)
load("processed_data/gam1_weights_processed.Rdata")
load("processed_data/gam2_weights_processed.Rdata")
load("processed_data/gam4_weights_processed.Rdata")
#load(file="processed_data/gamm_weights/gam2_weights.Rdata")
#---------------------------------------------
# Site level gam
#---------------------------------------------
summary(gam4.weights)
meow1 <- data.frame(TreeID = unique(gam4.weights$TreeID))
meow2 <- merge(meow1,gam4.weights[,c("TreeID","State")], all.x=T)
summary(meow2)


g4.mean.sens.temp <-meow2
g4.mean.sens.precip <- g4.mean.sens.temp							
g4.mean.sens.size <- g4.mean.sens.temp


for(i in unique(g4.mean.sens.temp$TreeID)){	

		g4.mean.sens.temp[g4.mean.sens.temp$TreeID==i,"mean.sens"] <- sens2(gam2.weights[gam2.weights$TreeID==i, "weight.tmean"])
		
		
	
}
summary(g4.mean.sens.temp)
g4.mean.sens.temp$type <- as.factor("temp")


for(i in unique(g4.mean.sens.precip$TreeID)){	

		g4.mean.sens.precip[g4.mean.sens.precip$TreeID==i,"mean.sens"] <- sens2(gam2.weights[gam2.weights$TreeID==i, "weight.precip"])
		
		
	
}
summary(g4.mean.sens.precip)
g4.mean.sens.precip$type <- as.factor("precip")

for(i in unique(g4.mean.sens.size$TreeID)){	

		g4.mean.sens.size[g4.mean.sens.precip$TreeID==i,"mean.sens"] <- sens2(gam2.weights[gam2.weights$TreeID==i, "weight.dbh.recon"])
		
		
	
}

summary(g4.mean.sens.size)
g4.mean.sens.size$type <-as.factor("size")

g4.mean.sens <- rbind(g4.mean.sens.temp, g4.mean.sens.precip, g4.mean.sens.size)
summary(g4.mean.sens)

#---------------------------------------------
# Species Level Gam
#---------------------------------------------

summary(gam1.weights)
meow1 <- data.frame(TreeID = unique(gam1.weights$TreeID))
meow2 <- merge(meow1,gam1.weights[,c("TreeID","group","State")], all.x=T)
summary(meow2)


g1.mean.sens.temp <-meow2
g1.mean.sens.precip <- g1.mean.sens.temp							
g1.mean.sens.size <- g1.mean.sens.temp


for(i in unique(g1.mean.sens.temp$TreeID)){	

		g1.mean.sens.temp[g1.mean.sens.temp$TreeID==i,"mean.sens"] <- sens2(gam2.weights[gam2.weights$TreeID==i, "weight.tmean"])
		
		
	
}
summary(g1.mean.sens.temp)
g1.mean.sens.temp$type <- as.factor("temp")


for(i in unique(g1.mean.sens.precip$TreeID)){	

		g1.mean.sens.precip[g1.mean.sens.precip$TreeID==i,"mean.sens"] <- sens2(gam2.weights[gam2.weights$TreeID==i, "weight.precip"])
		
		
	
}
summary(g1.mean.sens.precip)
g1.mean.sens.precip$type <- as.factor("precip")

for(i in unique(g1.mean.sens.size$TreeID)){	

		g1.mean.sens.size[g1.mean.sens.precip$TreeID==i,"mean.sens"] <- sens2(gam2.weights[gam2.weights$TreeID==i, "weight.dbh.recon"])
		
		
	
}

summary(g1.mean.sens.size)
g1.mean.sens.size$type <-as.factor("size")

g1.mean.sens <- rbind(g1.mean.sens.temp, g1.mean.sens.precip, g1.mean.sens.size)
summary(g1.mean.sens)

#---------------------------------------------
# Canopy Class Gam
#---------------------------------------------
# Creating a dataframe to look at the mean sensitivity of the weights.  To determine which canopy class shows more oscillation in teh weights term.


meow1 <- data.frame(TreeID = unique(gam2.weights$TreeID))
meow2 <- merge(meow1,gam2.weights[,c("TreeID","State", "Canopy.Class")], all.x=T)
summary(meow2)



g2.mean.sens.temp <-meow2
g2.mean.sens.precip <- g2.mean.sens.temp							
g2.mean.sens.size <- g2.mean.sens.temp

for(i in unique(g2.mean.sens.temp$TreeID)){	

		g2.mean.sens.temp[g2.mean.sens.temp$TreeID==i,"mean.sens"] <- sens2(gam2.weights[gam2.weights$TreeID==i, "weight.tmean"])
		
		
	
}
summary(g2.mean.sens.temp)
g2.mean.sens.temp$type <- as.factor("temp")


for(i in unique(g2.mean.sens.precip$TreeID)){	

		g2.mean.sens.precip[g2.mean.sens.precip$TreeID==i,"mean.sens"] <- sens2(gam2.weights[gam2.weights$TreeID==i, "weight.precip"])
		
		
	
}
summary(g2.mean.sens.precip)
g2.mean.sens.precip$type <- as.factor("precip")

for(i in unique(g2.mean.sens.size$TreeID)){	

		g2.mean.sens.size[g2.mean.sens.precip$TreeID==i,"mean.sens"] <- sens2(gam2.weights[gam2.weights$TreeID==i, "weight.dbh.recon"])
		
		
	
}

summary(g2.mean.sens.size)
g2.mean.sens.size$type <-as.factor("size")

g2.mean.sens.all <- rbind(g2.mean.sens.temp, g2.mean.sens.precip, g2.mean.sens.size)
summary(g2.mean.sens.all)
g2.mean.sens.all$time <- as.factor("all")


# Truncating to before 2000
gam2.weights.past <- gam2.weights[gam2.weights$Year < 2000,]
meow1 <- data.frame(TreeID = unique(gam2.weights.past$TreeID))
meow2 <- merge(meow1,gam2.weights.past[,c("TreeID","State", "Canopy.Class")], all.x=T)
summary(meow2)



g2.mean.sens.temp <-meow2
g2.mean.sens.precip <- g2.mean.sens.temp							
g2.mean.sens.size <- g2.mean.sens.temp

for(i in unique(g2.mean.sens.temp$TreeID)){	

		g2.mean.sens.temp[g2.mean.sens.temp$TreeID==i,"mean.sens"] <- sens2(gam2.weights.past[gam2.weights.past$TreeID==i, "weight.tmean"])
		
		
	
}
summary(g2.mean.sens.temp)
g2.mean.sens.temp$type <- as.factor("temp")


for(i in unique(g2.mean.sens.precip$TreeID)){	

		g2.mean.sens.precip[g2.mean.sens.precip$TreeID==i,"mean.sens"] <- sens2(gam2.weights.past[gam2.weights.past$TreeID==i, "weight.precip"])
		
		
	
}
summary(g2.mean.sens.precip)
g2.mean.sens.precip$type <- as.factor("precip")

for(i in unique(g2.mean.sens.size$TreeID)){	

		g2.mean.sens.size[g2.mean.sens.precip$TreeID==i,"mean.sens"] <- sens2(gam2.weights.past[gam2.weights.past$TreeID==i, "weight.dbh.recon"])
		
		
	
}

summary(g2.mean.sens.size)
g2.mean.sens.size$type <-as.factor("size")

g2.mean.sens.past <- rbind(g2.mean.sens.temp, g2.mean.sens.precip, g2.mean.sens.size)
summary(g2.mean.sens.past)
g2.mean.sens.past$time <- as.factor("past")

# Creating a dataframe to look at the mean sensitivity of the weights.  To determine which canopy class shows more oscillation in teh weights term.

# Looking at sensitivities since 2000
gam2.weights.recent <- gam2.weights[gam2.weights$Year >= 2000,]
meow1 <- data.frame(TreeID = unique(gam2.weights.recent$TreeID))
meow2 <- merge(meow1,gam2.weights.recent[,c("TreeID","State", "Canopy.Class")], all.x=T)
summary(meow2)



g2.mean.sens.temp <-meow2
g2.mean.sens.precip <- g2.mean.sens.temp							
g2.mean.sens.size <- g2.mean.sens.temp

for(i in unique(g2.mean.sens.temp$TreeID)){	

		g2.mean.sens.temp[g2.mean.sens.temp$TreeID==i,"mean.sens"] <- sens2(gam2.weights.recent[gam2.weights.recent$TreeID==i, "weight.tmean"])
		
		
	
}
summary(g2.mean.sens.temp)
g2.mean.sens.temp$type <- as.factor("temp")


for(i in unique(g2.mean.sens.precip$TreeID)){	

		g2.mean.sens.precip[g2.mean.sens.precip$TreeID==i,"mean.sens"] <- sens2(gam2.weights.recent[gam2.weights.recent$TreeID==i, "weight.precip"])
		
		
	
}
summary(g2.mean.sens.precip)
g2.mean.sens.precip$type <- as.factor("precip")

for(i in unique(g2.mean.sens.size$TreeID)){	

		g2.mean.sens.size[g2.mean.sens.precip$TreeID==i,"mean.sens"] <- sens2(gam2.weights.recent[gam2.weights.recent$TreeID==i, "weight.dbh.recon"])
		
		
	
}

summary(g2.mean.sens.size)
g2.mean.sens.size$type <-as.factor("size")

g2.mean.sens.small <- rbind(g2.mean.sens.size, g2.mean.sens.temp, g2.mean.sens.precip)
g2.mean.sens.small$time <- as.factor("mod")
summary(g2.mean.sens.small)

g2.mean.sens2 <- rbind(g2.mean.sens.past, g2.mean.sens.small, g2.mean.sens.all)
summary(g2.mean.sens2)

# JUST SOME RANDOM GRAPHS JUST TO SEE
g2.mean.sens2$State <- factor(g2.mean.sens2$State, levels =c("MO", "IN","OH","MA","ME"))

g2.mean.sens2$time <- factor(g2.mean.sens2$time, levels =c("all", "past", "mod"))


summary(g2.mean.sens2)



ggplot(data=g2.mean.sens2) + facet_grid(Canopy.Class~time) +
	geom_boxplot(aes(x=type, y = mean.sens))


ggplot(data=g2.mean.sens2[g2.mean.sens2$Canopy.Class=="D",]) + facet_grid(State*Canopy.Class~time) +
	geom_boxplot(aes(x=type, y = mean.sens))

ggplot(data=g2.mean.sens2[g2.mean.sens2$Canopy.Class=="I",]) + facet_grid(State*Canopy.Class~time) +
	geom_boxplot(aes(x=type, y = mean.sens))

ggplot(data=g2.mean.sens2[g2.mean.sens2$Canopy.Class=="S",]) + facet_grid(State*Canopy.Class~time) +
	geom_boxplot(aes(x=type, y = mean.sens))


# Running an ANOVA to determine if there are differences in teh mean sensitivity across different canopy classes

#---------------------------------------------
# Site Level Gam
#---------------------------------------------

g4.meow <- aov(mean.sens ~ State * type, data=g4.mean.sens)
summary(g4.meow)
TukeyHSD(g4.meow)

# Gettign mean and SD's for the different weights for the different canopy classes
# MO
mean(g4.mean.sens[g4.mean.sens$State=="MO" & g4.mean.sens$type=="temp","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="MO" & g4.mean.sens$type=="temp","mean.sens"], na.rm=T)

mean(g4.mean.sens[g4.mean.sens$State=="MO" & g4.mean.sens$type=="precip","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="MO" & g4.mean.sens$type=="precip","mean.sens"], na.rm=T)

mean(g4.mean.sens[g4.mean.sens$State=="MO" & g4.mean.sens$type=="size","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="MO" & g4.mean.sens$type=="size","mean.sens"], na.rm=T)

# IN
mean(g4.mean.sens[g4.mean.sens$State=="IN" & g4.mean.sens$type=="temp","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="IN" & g4.mean.sens$type=="temp","mean.sens"], na.rm=T)

mean(g4.mean.sens[g4.mean.sens$State=="IN" & g4.mean.sens$type=="precip","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="IN" & g4.mean.sens$type=="precip","mean.sens"], na.rm=T)

mean(g4.mean.sens[g4.mean.sens$State=="IN" & g4.mean.sens$type=="size","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="IN" & g4.mean.sens$type=="size","mean.sens"], na.rm=T)

# OH
mean(g4.mean.sens[g4.mean.sens$State=="OH" & g4.mean.sens$type=="temp","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="OH" & g4.mean.sens$type=="temp","mean.sens"], na.rm=T)

mean(g4.mean.sens[g4.mean.sens$State=="OH" & g4.mean.sens$type=="precip","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="OH" & g4.mean.sens$type=="precip","mean.sens"], na.rm=T)

mean(g4.mean.sens[g4.mean.sens$State=="OH" & g4.mean.sens$type=="size","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="OH" & g4.mean.sens$type=="size","mean.sens"], na.rm=T)

# MA
mean(g4.mean.sens[g4.mean.sens$State=="MA" & g4.mean.sens$type=="temp","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="MA" & g4.mean.sens$type=="temp","mean.sens"], na.rm=T)

mean(g4.mean.sens[g4.mean.sens$State=="MA" & g4.mean.sens$type=="precip","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="MA" & g4.mean.sens$type=="precip","mean.sens"], na.rm=T)

mean(g4.mean.sens[g4.mean.sens$State=="MA" & g4.mean.sens$type=="size","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="MA" & g4.mean.sens$type=="size","mean.sens"], na.rm=T)

# ME
mean(g4.mean.sens[g4.mean.sens$State=="ME" & g4.mean.sens$type=="temp","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="ME" & g4.mean.sens$type=="temp","mean.sens"], na.rm=T)

mean(g4.mean.sens[g4.mean.sens$State=="ME" & g4.mean.sens$type=="precip","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="ME" & g4.mean.sens$type=="precip","mean.sens"], na.rm=T)

mean(g4.mean.sens[g4.mean.sens$State=="ME" & g4.mean.sens$type=="size","mean.sens"], na.rm=T); sd(g4.mean.sens[g4.mean.sens$State=="ME" & g4.mean.sens$type=="size","mean.sens"], na.rm=T)




#---------------------------------------------
# Species Level Gam
---------------------------------------------

g1.meow <- aov(mean.sens ~ group * type, data=g1.mean.sens)
summary(g1.meow)
TukeyHSD(g1.meow)




#---------------------------------------------
# Canopy Class Gam
#---------------------------------------------
g2.meow <- aov(mean.sens ~ State * time * Canopy.Class * type, data=g2.mean.sens2[!g2.mean.sens2$time=="all",])
summary(g2.meow)
TukeyHSD(g2.meow)

# Gettign mean and SD's for the different weights for the different canopy classes
# Dominant
mean(g2.mean.sens[g2.mean.sens$Canopy.Class=="D" & g2.mean.sens$type=="temp","mean.sens"], na.rm=T); sd(g2.mean.sens[g2.mean.sens$Canopy.Class=="D" & g2.mean.sens$type=="temp","mean.sens"], na.rm=T)

mean(g2.mean.sens[g2.mean.sens$Canopy.Class=="D" & g2.mean.sens$type=="precip","mean.sens"], na.rm=T); sd(g2.mean.sens[g2.mean.sens$Canopy.Class=="D" & g2.mean.sens$type=="precip","mean.sens"], na.rm=T)

mean(g2.mean.sens[g2.mean.sens$Canopy.Class=="D" & g2.mean.sens$type=="size","mean.sens"], na.rm=T); sd(g2.mean.sens[g2.mean.sens$Canopy.Class=="D" & g2.mean.sens$type=="size","mean.sens"], na.rm=T)


# Intermediate
mean(g2.mean.sens[g2.mean.sens$Canopy.Class=="I" & g2.mean.sens$type=="temp","mean.sens"], na.rm=T); sd(g2.mean.sens[g2.mean.sens$Canopy.Class=="I" & g2.mean.sens$type=="temp","mean.sens"], na.rm=T)

mean(g2.mean.sens[g2.mean.sens$Canopy.Class=="I" & g2.mean.sens$type=="precip","mean.sens"], na.rm=T); sd(g2.mean.sens[g2.mean.sens$Canopy.Class=="I" & g2.mean.sens$type=="precip","mean.sens"], na.rm=T)

mean(g2.mean.sens[g2.mean.sens$Canopy.Class=="I" & g2.mean.sens$type=="size","mean.sens"], na.rm=T); sd(g2.mean.sens[g2.mean.sens$Canopy.Class=="I" & g2.mean.sens$type=="size","mean.sens"], na.rm=T)

# Suppressed
mean(g2.mean.sens[g2.mean.sens$Canopy.Class=="S" & g2.mean.sens$type=="temp","mean.sens"], na.rm=T); sd(g2.mean.sens[g2.mean.sens$Canopy.Class=="S" & g2.mean.sens$type=="temp","mean.sens"], na.rm=T)

mean(g2.mean.sens[g2.mean.sens$Canopy.Class=="S" & g2.mean.sens$type=="precip","mean.sens"], na.rm=T); sd(g2.mean.sens[g2.mean.sens$Canopy.Class=="S" & g2.mean.sens$type=="precip","mean.sens"], na.rm=T)

mean(g2.mean.sens[g2.mean.sens$Canopy.Class=="S" & g2.mean.sens$type=="size","mean.sens"], na.rm=T); sd(g2.mean.sens[g2.mean.sens$Canopy.Class=="S" & g2.mean.sens$type=="size","mean.sens"], na.rm=T)
