# Conducting a series of linear correlations on the BAI-index
library(car)
library(ggplot2)
load("processed_data/site_cc_chrons.Rdata")

# data is a list of lists
# Site by canopy class
summary(sea.data.chron)

# cutting chronolgy length down to be 1950-2012


# getting site level climate data
climate.gs <- read.csv("processed_data/climate_growing_season.csv", header=T)
summary(climate.gs)

sites.use <- c("Harvard", "Howland", "Morgan Monroe State Park", "Oak Openings Toledo", "Missouri Ozark")

ch2.climate <- climate.gs[climate.gs$Site %in% sites.use,]
summary(ch2.climate)

climate.short <- ch2.climate[ch2.climate$Year >=1950 & ch2.climate$Year<=2012,]


climate.short$State <- recode(climate.short$Site, "'Howland'='ME';'Harvard'='MA';'Missouri Ozark'='MO';'Morgan Monroe State Park'='IN';
                                  'Oak Openings Toledo'='OH'")
summary(climate.short)

summary(sea.data.chron)

canopy.class <- c("dom", "int", "sup")

og.temp.cors <- data.frame(array(dim=c(length(unique(climate.short$State)), length(canopy.class))))
names(og.temp.cors) <- c("dom", "int", "sup")
row.names(og.temp.cors) <- unique(climate.short$State)

og.ppt.cors  <- data.frame(array(dim=c(length(unique(climate.short$State)), length(canopy.class))))
names(og.ppt.cors) <- c("dom", "int", "sup")
row.names(og.ppt.cors) <- unique(climate.short$State)





for(s in unique(climate.short$State)){
  for(c in unique(canopy.class)){
  og.temp.cors[s,c] <- cor(climate.short[climate.short$State==s,"tmean"], sea.data.chron[[s]][[c]][,1])
  og.ppt.cors[s,c]  <- cor(climate.short[climate.short$State==s,"precip"], sea.data.chron[[s]][[c]][,1])
    }  
}

og.ppt.corshttp://127.0.0.1:24525/graphics/plot_zoom_png?width=1733&height=985
og.temp.cors

og.ppt.stack <- stack(og.ppt.cors)
names(og.ppt.stack) <-c("cor", "canopy.class")
og.ppt.stack$State  <- as.factor(paste(row.names(og.ppt.cors)))
og.ppt.stack$type <- as.factor("precip")

og.temp.stack <- stack(og.temp.cors)
names(og.temp.stack) <-c("cor", "canopy.class")

og.temp.stack$State  <- as.factor(paste(row.names(og.temp.cors)))
og.temp.stack$type <- as.factor("temp")

og.climate.cors <- rbind(og.temp.stack, og.ppt.stack)

summary(og.climate.cors)

og.climate.cors$State <- factor(og.climate.cors$State,levels=c("ME", "MA", "OH", "IN", "MO"))

ggplot(data=og.climate.cors) + facet_grid(type~.) +
  geom_bar(aes(x=State, y=cor, fill=canopy.class), stat="identity", position="dodge", colour="black")
