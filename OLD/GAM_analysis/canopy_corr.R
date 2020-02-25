library(dplR)
library(ggplot2)
library(stringr)
# -------------------------------------------------------------------------------------
# Using MMF and Harvard as two examples
# -------------------------------------------------------------------------------------
load("processed_data/sites_use_climate.Rdata")
summary(climate.site)

load("processed_data/sites_rw.Rdata")
summary(sites.rw)

load("processed_data/test_tree_data_1935_2012.Rdata")
summary(test)

# Creating lists for harvard and mmf species

sites.use2 <- c("Harvard", "Morgan Monroe State Park")

summary(sites.rw[[1]])
names(sites.rw)
# canopy.data <- sites.rw[names(sites.rw) %in% sites.use2]

canopy.data <- sites.rw
summary(canopy.data)


# Gettign mmf data

short.canopy <- list()

for(s in names(canopy.data)){
	for(i in unique(test[test$Site ==s, "Canopy.Class"])){
		cols.use <- unique(test[test$Site==s & test$Canopy.Class==i,"TreeID"])
		short.canopy[[paste(s, i, sep=".")]] <- canopy.data[[s]][,paste(cols.use)]
	} 
}
summary(short.canopy)
# summary(short.canopy[["Morgan Monroe State Park.D"]])
# Making each tree an index using a 30 yr. spline
short.can.index <- list()
for(s in names(short.canopy)){
	short.can.index[[s]] <- detrend(short.canopy[[s]], method="Spline", nyrs=30)
}
summary(short.can.index)


# Making chronologies from these indicies
short.can.chr <- list()

for(s in names(short.can.index)){
	short.can.chr[[s]] <- chron(short.can.index[[s]], prewhiten=T)
}
summary(short.can.chr[[1]])



# Correlating these spp level chronologies with climate

corr.can <- list()
for(i in names(short.can.chr)){
	site <- unlist(strsplit(i, "[.]"))[1]
	corr.can[[i]] <- as.data.frame(cor(short.can.chr[[i]][, 1:2], climate.site[[site]][, c("tmean", 					"precip")], method="pearson"))
	corr.can[[i]]$chr <- row.names(corr.can[[i]])
}


corr.can[[1]]

for(i in names(corr.can)){
	corr.can[[i]][,"chr"] <- substr(corr.can[[i]][,"chr"], 4, 6)
}
corr.can[[1]]

site.df <- data.frame(Site.Can=names(corr.can))
chr <- data.frame(chr=c("std", "res"))
site.chr <- merge(site.df, chr)

canopy.df <- merge(site.chr, data.frame(Factor=c("tmean", "precip")))




for(i in names(corr.can)){
	for(j in unique(canopy.df$Factor)){
		for(k in unique(canopy.df$chr)){
						
			canopy.df[canopy.df$Site.Can==i & canopy.df$Factor==j & canopy.df$chr==k, "cor"] <- 
						
						corr.can[[i]][corr.can[[i]][,"chr"]==k, j]
		}
	}
}
summary(canopy.df)


# canopy.df$Site <- as.factor(ifelse(substr(canopy.df$Site.Can,1, 1)=="H", "HF", "MMF"))

for(i in 1:nrow(canopy.df)){
	if(substr(paste(canopy.df[i,"Site.Can"]),1,2)=="Ha") {canopy.df[i,"Site"] <- "MA"}
	if(substr(paste(canopy.df[i,"Site.Can"]),1,2)=="Ho") {canopy.df[i,"Site"] <- "ME"}
	if(substr(paste(canopy.df[i,"Site.Can"]),1,2)=="Mi") {canopy.df[i,"Site"]<- "MO"}
	if(substr(paste(canopy.df[i,"Site.Can"]),1,2)=="Mo") {canopy.df[i,"Site"] <- "IN"}
	if(substr(paste(canopy.df[i,"Site.Can"]),1,2)=="Oa") {canopy.df[i,"Site"] <- "OH"}
}
canopy.df$Site <- factor(canopy.df$Site, levels = c("MO", "IN", "OH", "MA", "ME"))
summary(canopy.df)

class(canopy.df$Site.Can)

for(i in unique(canopy.df$Site.Can)){
	canopy.df[canopy.df$Site.Can==i,"CC"] <-str_sub(canopy.df[canopy.df$Site.Can==i,"Site.Can"],-1,-1)
}
canopy.df$CC <- factor(canopy.df$CC, levels = c("D", "I", "S"))
summary(canopy.df)


# Sig value for 61 df = 0.209
canopy.df$sig <- ifelse(canopy.df$cor < -0.209 | canopy.df$cor > 0.209, "Y", "N")
canopy.df$sig <- factor(canopy.df$sig, levels = c("Y", "N"))
source("poster_theme.R")
pdf("figures/canopy_correlations.pdf", width= 13, height = 8.5)
ggplot(data = canopy.df[canopy.df$chr=="std",]) + facet_grid(Factor~Site) + 
	geom_bar(aes(x=CC, y=cor, fill=sig), stat="identity", position="dodge", colour="black") +
	geom_hline(yintercept=0.209, linetype="dashed") + 
	geom_hline(yintercept=-0.209, linetype="dashed") + 	
	geom_hline(yintercept=0, linetype="solid") +
	scale_fill_manual(values= c("green", "grey50")) + 
	poster.theme2 
	#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()
	
#####################################################################################
# Looking at violin plots for the different canopy classes and climate correlations
#####################################################################################

summary(short.can.index)


v.corr.can <- list()
for(i in names(short.can.chr)){
	site <- unlist(strsplit(i, "[.]"))[1]
	corr.can[[i]] <- as.data.frame(cor(short.can.chr[[i]][, 1:2], climate.site[[site]][, c("tmean", 					"precip")], method="pearson"))
	corr.can[[i]]$chr <- row.names(corr.can[[i]])
}
summary(corr.can[[1]])