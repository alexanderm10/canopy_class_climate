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

dne.tree.sum$site <- as.factor(ne.tree.sum$site)


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
