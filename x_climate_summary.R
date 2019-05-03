library(car)
library(dplR)
library(ggplot2)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}


path.met <- "met_data/"

# Loading in climate data from the PRISM extracts
site.clim <- read.csv("processed_data/climate_growing_season.csv", header=T)
site.clim <- site.clim[site.clim$Year <=2014,]
region.clim <- aggregate(site.clim[,!names(site.clim) %in% c("Site.Code", "Year")], by=list(site.clim$Year), FUN=mean, na.rm=T)
names(region.clim) <- c("Year", "tmean", "precip", "vpd.min", "vpd.max")

region.clim.upper <- aggregate(site.clim[,!names(site.clim) %in% c("Site.Code", "Year")], by=list(site.clim$Year), FUN=quantile, prob=0.975, na.rm=T)
names(region.clim.upper) <- c("Year", "tmean", "precip", "vpd.min", "vpd.max")

region.clim.lower <- aggregate(site.clim[,!names(site.clim) %in% c("Site.Code", "Year")], by=list(site.clim$Year), FUN=quantile, prob=0.025, na.rm=T)
names(region.clim.lower) <- c("Year", "tmean", "precip", "vpd.min", "vpd.max")


# Calculating difference since 1950

clim.diff <- as.data.frame(array(dim=c(4,2)))           
names(clim.diff) <- c("1950", "2015")                        

row.names(clim.diff) <- c("tmean", "precip", "vpd.min", "vpd.max")

for(i in row.names(clim.diff)){
  clim.diff[row.names(clim.diff)==i,"1950"] <- region.clim[region.clim$Year==1950,i]
  clim.diff[row.names(clim.diff)==i,"2015"] <- region.clim[region.clim$Year==2015,i]
}
clim.diff
clim.diff$diff <- clim.diff[,"2015"] - clim.diff[,"1950"]

# calculating lm to get the trend in the climate data
library(nlme)
#region.clim.short <- region.clim[region.clim$Year >=1950,]

lm.tmean <- lm(tmean~Year, data=region.clim)
summary(lm.tmean)

lm.precip <- lm(precip~Year, data=region.clim)
lm.precip
summary(lm.precip)

lm.vpd.min <- lm(vpd.min~Year, data=region.clim)
summary(lm.vpd.min)


lm.vpd.max <- lm(vpd.max~Year, data=region.clim)
summary(lm.vpd.max)


clim.stack <- stack(region.clim[,!names(region.clim) %in% "Year"])
names(clim.stack) <- c("value", "variable")
clim.stack$Year <- region.clim$Year

clim.upper.stack <- stack(region.clim.upper[,!names(region.clim.upper) %in% "Year"])
names(clim.upper.stack) <- c("upper", "variable")
clim.upper.stack$Year <- region.clim$Year

clim.lower.stack <- stack(region.clim.lower[,!names(region.clim.lower) %in% "Year"])
names(clim.lower.stack) <- c("lower", "variable")
clim.lower.stack$Year <- region.clim$Year

clim.stack <- merge(clim.stack, clim.upper.stack)
clim.stack <- merge(clim.stack, clim.lower.stack)

summary(clim.stack)

ggplot(clim.stack) + facet_grid(variable~., scale="free_y") +
  geom_ribbon(aes(x=Year,ymin=lower, ymax=upper), alpha=0.5) +
  geom_line(aes(x=Year, y=value)) +
  theme_bw()


summary(clim.stack)

mean.t <- mean(clim.stack[clim.stack$variable %in% "tmean","value"])


warm.stripes <- data.frame(Year=unique(clim.stack$Year),
                           tmean = clim.stack[clim.stack$variable %in% "tmean","value"],
                           mean.t = mean.t)

head(warm.stripes)

warm.stripes$diff <- warm.stripes$tmean - mean.t
summary(warm.stripes)

warm <- ggplot(data=warm.stripes) +
  geom_bar(aes(x=Year, y=1, fill=diff, group=Year), stat="identity") +
  scale_fill_gradient2(low="dodgerblue3", mid="white", high="darkred", limits=c(-1.5,1.5), breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5)) +
  labs(x="Year", y="", fill="", title="NEUS Mean GS Temp") +
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=16, vjust= 0.5, face="bold"), 
        axis.text.y=element_blank(), 
        strip.text=element_text(face="bold", size=22),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=22),
        legend.title = element_text(size=22),
        legend.key = element_rect(fill = "white")) + 
  guides(fill = guide_colourbar(barwidth = 25, barheight = 5,title="")) +
  theme(axis.title.y= element_text(size=24, face="bold")) +
  theme(axis.title.x= element_text(size=24, face="bold")) +
  scale_x_continuous(breaks  = seq(1890,2020, by=10))


mean.p <- mean(clim.stack[clim.stack$variable %in% "precip","value"])


wet.stripes <- data.frame(Year=unique(clim.stack$Year),
                          precip = clim.stack[clim.stack$variable %in% "precip","value"],
                           mean.p = mean.p)

head(wet.stripes)

wet.stripes$diff <- wet.stripes$precip - mean.p
summary(wet.stripes)

wet <- ggplot(data=wet.stripes) +
  geom_bar(aes(x=Year, y=1, fill=diff, group=Year), stat="identity") +
  scale_fill_gradient2(low="chocolate4", mid="white", high="forestgreen") +
  labs(x="Year", y="", fill="", title="NEUS Mean GS Precip") +
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=16, vjust= 0.5, face="bold"), 
        axis.text.y=element_blank(), 
        strip.text=element_text(face="bold", size=22),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=22),
        legend.title = element_text(size=22),
        legend.key = element_rect(fill = "white")) + 
  guides(fill = guide_colourbar(barwidth = 25, barheight = 5,title="")) +
  theme(axis.title.y= element_text(size=24, face="bold")) +
  theme(axis.title.x= element_text(size=24, face="bold")) +
  scale_x_continuous(breaks  = seq(1890,2020, by=10))


library(cowplot)

png("figures/climate_deviations.png", height=15, width=30, res=300, unit="in")
  plot_grid(warm,wet,align = c("v","h"), nrow = 2)
dev.off()