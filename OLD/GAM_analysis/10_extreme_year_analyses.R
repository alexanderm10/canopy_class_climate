library(ggplot2)
require(mgcv)
require(lsmeans)
require(car)
require(moments)
# Running ANOVA between Extreme years and normal years BAI for Canopy.Class
load("processed_data/gam2_weights_processed.Rdata")
summary(gam2.weights)

# Running ANOVA's to determine differences between ambient years and extreme years
# Using a lme here to take into account the differences between sites as random effeects
summary(gam2.weights)
# Truncating to 1950 becasue that's the extent of the climate analysis
gam2.weights <- gam2.weights[gam2.weights$Year >=1950,]


# Need to log transform the data to meet assumptions for the ANOVA test
head(gam2.weights)
gam2.temp.anova <- aov(log(BA.inc) ~  TreeID + Site + PlotID + group +Temp.Mark*Canopy.Class, data=gam2.weights)

# anova(gam2.lme.temp)
summary(gam2.temp.anova)
TukeyHSD(gam2.temp.anova)

# lsmeans(gam2.lme.temp, pairwise~Temp.Mark*Canopy.Class, adjust="tukey")	
hist(resid(gam2.temp.anova))
plot(resid(gam2.temp.anova)~predict(gam2.temp.anova))
skewness(log(gam2.weights$BA.inc))
kurtosis(log(gam2.weights$BA.inc))

gam2.precip.anova <- aov(log(BA.inc) ~ TreeID + Site + PlotID + group + Precip.Mark*Canopy.Class, data=gam2.weights)

anova(gam2.lme.precip)
summary(gam2.precip.anova)
TukeyHSD(gam2.precip.anova)

lsmeans(gam2.lme.precip, pairwise~Precip.Mark*Canopy.Class, adjust="tukey")	
hist(resid(gam2.lme.precip))
plot(resid(gam2.lme.precip)~predict(gam2.lme.precip))



# Recoding the ambient in temp and precip to be unique so that maybe it will plot prettier
gam2.weights$Temp.Mark <- recode(gam2.weights$Temp.Mark, "'A'='1A-Temp';'cold'='3Cool';'hot'='2Hot'")
gam2.weights$Precip.Mark <- recode(gam2.weights$Precip.Mark, "'A'='4A-Precip';'dry'='5Dry';'wet'='6Wet'")

# Reordering factors
# gam2.weights$Temp.Mark <- factor(gam2.weights$Temp.Mark, levels=c("A-Temp", "Hot", "Cool"))
# gam2.weights$Precip.Mark <- factor(gam2.weights$Precip.Mark, levels=c("A-Precip", "Dry", "Wet"))
# summary(gam2.weights)
gam2.weights$State <- factor(gam2.weights$State, levels=c("MO", "IN", "OH", "MA", "ME"))


# using boxplots to show the differences
ggplot(data=gam2.weights) + facet_grid(Canopy.Class~State) +
	geom_boxplot(aes(x=Temp.Mark, y=log(BA.inc))) +
	#geom_boxplot(aes(x=Precip.Mark, y=log(BA.inc)))
	scale_y_continuous(limits=c(-2.5,5), expand=c(0,0))

hist(log(gam2.weights$BA.inc), breaks=100)