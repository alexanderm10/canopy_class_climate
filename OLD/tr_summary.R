# tree ring summary information for the different sites
library(dplR)
library(car)
# load in tr tucson data

ne.tr <- read.tucson("processed_data/NE_tr_tucson.rwl")
summary(ne.tr)
ne.tr.list <- list()

for(i in unique(substr(names(ne.tr),1,2))){
  ne.tr.list[[i]] <- ne.tr[,substr(names(ne.tr),1,2)==i]
}

gb.sum <- as.data.frame(summary(ne.tr.list[[1]]))
summary(gb.sum$first)

ne.sum <- data.frame(array(dim=c(length(ne.tr.list),1)))
names(ne.sum) <- "site"
ne.sum$site <- unique(substr(names(ne.tr),1,2))


for(i in ne.sum$site) {
  temp <- as.data.frame(summary(ne.tr.list[[i]]))
  
  ne.sum[ne.sum$site==i,"first.old"] <- min(temp$first)
  ne.sum[ne.sum$site==i,"first.young"] <- max(temp$first)
  ne.sum[ne.sum$site==i,"last"] <- max(temp$last)
  ne.sum[ne.sum$site==i,"mean.first"] <- round(mean(temp$first))
  ne.sum[ne.sum$site==i,"sd.first"] <- round(sd(temp$first))
}

ne.sum


har.how.rw <- read.csv("HAR_HOW_tree_rw.csv", header=T)
summary(har.how.rw[1:2])
row.names(har.how.rw) <- har.how.rw$X
har.how.rw <- har.how.rw[,!names(har.how.rw)=="X"]
write.tucson(har.how.rw, fname="har_how_tucson.rwl",long.names = T)

har.how.tuc <- read.tucson("har_how_tucson.rwl")

har.how.sum <- data.frame(array(dim=c(2,1)))
names(har.how.sum) <- "site"

har.how.sum$site <- c("HO", "TP") 

for(i in har.how.sum$site) {
  temp <- as.data.frame(summary(har.how.tuc[,substr(names(har.how.tuc),1,2)==i]))
  
  har.how.sum[har.how.sum$site==i,"first.old"] <- min(temp$first)
  har.how.sum[har.how.sum$site==i,"first.young"] <- max(temp$first)
  har.how.sum[har.how.sum$site==i,"last"] <- max(temp$last)
  har.how.sum[har.how.sum$site==i,"mean.first"] <- round(mean(temp$first))
  har.how.sum[har.how.sum$site==i,"sd.first"] <- round(sd(temp$first))
}

har.how.sum

ne.tree.sum <- rbind(har.how.sum, ne.sum)

summary(ne.tree.sum)

ne.tree.sum$site <- as.factor(ne.tree.sum$site)


# Looking at age. Need to merge the .rwl files and look at the summary

har.how.tuc
summary(ne.tr)

ne.all.tr <- combine.rwl(har.how.tuc, ne.tr)
summary(ne.all.tr)

summary(summary(ne.all.tr))

ne.all.tr.sum <- as.data.frame(summary(ne.all.tr))

round(mean(ne.all.tr.sum$year))
round(sd(ne.all.tr.sum$year))
max(ne.all.tr.sum$year)
min(ne.all.tr.sum$year)

site.abb <- c("GB", "GE", "LF", "NR", "PS", "RH", "TP", "HO")

table1.df <- data.frame(site.code = site.abb,
                        first.range1 = NA,
                        first.range2 = NA,
                        first.mean = NA,
                        first.sd = NA,
                        age.range1 = NA,
                        age.range2 = NA,
                        age.mean = NA,
                        age.sd = NA)

for(i in table1.df$site.code) {
  table1.df[table1.df$site.code==i,"first.range1"] <- range(ne.all.tr.sum[substr(ne.all.tr.sum$series,1,2)==i,"first"], na.rm=T)[1]
  table1.df[table1.df$site.code==i,"first.range2"] <- range(ne.all.tr.sum[substr(ne.all.tr.sum$series,1,2)==i,"first"], na.rm=T)[2]
  table1.df[table1.df$site.code==i,"first.mean"] <- round(mean(ne.all.tr.sum[substr(ne.all.tr.sum$series,1,2)==i,"first"], na.rm=T))
  table1.df[table1.df$site.code==i,"first.sd"] <- round(sd(ne.all.tr.sum[substr(ne.all.tr.sum$series,1,2)==i,"first"], na.rm=T))
  
  table1.df[table1.df$site.code==i,"age.range1"] <- range(ne.all.tr.sum[substr(ne.all.tr.sum$series,1,2)==i,"year"], na.rm=T)[1]
  table1.df[table1.df$site.code==i,"age.range2"] <- range(ne.all.tr.sum[substr(ne.all.tr.sum$series,1,2)==i,"year"], na.rm=T)[2]
  table1.df[table1.df$site.code==i,"age.mean"] <- round(mean(ne.all.tr.sum[substr(ne.all.tr.sum$series,1,2)==i,"year"], na.rm=T))
  table1.df[table1.df$site.code==i,"age.sd"] <- round(sd(ne.all.tr.sum[substr(ne.all.tr.sum$series,1,2)==i,"year"], na.rm=T))
}

table1.df
write.csv(table1.df, file="table1.df.csv", row.names=F)


# Getting extent information for individual canopy layers

# Big dataframe with correct canopy class info
load("overstory_understory_combined_data_use.Rdata")
summary(test)

# gettign TreeID for each canopy layers

names.d <- unique(test$TreeID[test$Canopy.Class=="Canopy"])

names.i <- unique(test$TreeID[test$Canopy.Class=="I"])

names.u <- unique(test$TreeID[test$Canopy.Class=="U"])

######################
# Harvard tower and Howland data from dissertation
har.how.rwl <- read.tucson("har_how_tucson.rwl")

# Neil Data
tree.rw <- read.tucson("processed_data/NE_tr_tucson.rwl")
summary(tree.rw)
class(tree.rw)

tree.rw <- combine.rwl(har.how.rwl, tree.rw)

tree.d <- tree.rw[,names(tree.rw) %in% names.d]

tree.i <- tree.rw[,names(tree.rw) %in% names.i]

tree.u <- tree.rw[,names(tree.rw) %in% names.u]

# making summaries for each canopy layer into an object
tree.d.sum <- summary(tree.d)
tree.i.sum <- summary(tree.i)
tree.u.sum <- summary(tree.u)

tree.d.sum$Canopy.Class <- as.factor("Overstory")
tree.i.sum$Canopy.Class <- as.factor("Middle")
tree.u.sum$Canopy.Class <- as.factor("Understory")

summary(tree.d.sum)
summary(tree.i.sum)
summary(tree.u.sum)

dim(tree.d.sum)
dim(tree.i.sum)
dim(tree.u.sum)

# Checking to see if all sites are represented in each canpopy layer
unique(substr(tree.u.sum$series,1,2))

# merging all together to plot in ggplot
tree.rw.sum <- rbind(tree.d.sum, tree.i.sum, tree.u.sum)
dim(tree.rw.sum)

tree.rw.sum$Site <- as.factor(substr(tree.rw.sum$series,1,2))

library(ggplot2)
png(filename = "figures/pub_figs/first_year_supplement.png", height=15, width=30, res=300, unit="in")
ggplot(data=tree.rw.sum) + # facet_grid(Site~.) +
  geom_histogram(aes(x=first, color=Canopy.Class, fill=Canopy.Class), binwidth=1) +
  scale_x_continuous(limits=c(1895,2014), breaks= c(1900, 1925, 1950, 1975, 2000, 2020)) +
  scale_fill_manual(values=c("#E69F00","#009E73", "#0072B2"), guide = guide_legend(title = "")) +
  scale_color_manual(values=c("#E69F00","#009E73", "#0072B2"), guide = guide_legend(title = "")) +
  labs(x="First Year Present", y="Count") +
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=22), 
        axis.text.y=element_text(angle=0, color="black", size=22), 
        strip.text=element_text(face="bold", size=18),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=22),
        legend.key = element_rect(fill = "white")) + 
  #guides(color=guide_legend(nrow=1),)+
  theme(axis.title.x = element_text(size=28, face="bold"),
        axis.title.y= element_text(size=28, face="bold"))+
  theme(panel.spacing.x = unit(1.25,"lines"),
        panel.spacing.y = unit(1.75,"lines"))
dev.off()
