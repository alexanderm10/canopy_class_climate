library(dplR)
library(ggplot2)
library(stringr)
library(car)
# -------------------------------------------------------------------------------------
# Using MMF and Harvard as two examples
# -------------------------------------------------------------------------------------


load("processed_data/sites_rw.Rdata")
load("processed_data/test_tree_data_1935_2012.Rdata")
load("processed_data/sites_use_climate.Rdata")
summary(climate.site[[1]])
summary(test)
# Creating lists for harvard and mmf species

sites.use2 <- c("Harvard", "Morgan Monroe State Park")

summary(sites.rw[[5]])
names(sites.rw)
# species.data <- sites.rw[names(sites.rw) %in% sites.use2]
species.data <- sites.rw
summary(species.data)

summary(species.data[[5]])
# Gettign mmf data

short.spp <- list()

for(s in names(species.data)){
	for(i in unique(test[test$Site ==s, "group"])){
		cols.use <- unique(test[test$Site==s & test$group==i,"TreeID"])
		short.spp[[paste(s, i, sep=".")]] <- as.data.frame(species.data[[s]][,paste(cols.use)])
	} 
}
summary(short.spp)
summary(short.spp[["Morgan Monroe State Park.ACSA"]])
summary(short.spp[["Oak Openings Toledo.ACSA"]])

names(short.spp[["Oak Openings Toledo.ACSA"]]) <- "OOA036"
names(short.spp[["Oak Openings Toledo.SAAL"]]) <- "OOA018"

# Making each tree an index using a 30 yr. spline
short.spp.index <- list()
for(s in names(short.spp)){
	short.spp.index[[s]] <- detrend(short.spp[[s]], method="Spline", nyrs=30)
}
summary(short.spp.index[[1]])
summary(short.spp.index)

# Making chronologies from these indicies
short.spp.chr <- list()

for(s in names(short.spp.index)){
	short.spp.chr[[s]] <- chron(short.spp.index[[s]], prewhiten=T)
}
summary(short.spp.chr[[1]])



# Correlating these spp level chronologies with climate

corr.groups <- list()
for(i in names(short.spp.chr)){
	site <- unlist(strsplit(i, "[.]"))[1]
	corr.groups[[i]] <- as.data.frame(cor(short.spp.chr[[i]][, 1:2], climate.site[[site]][, c("tmean", 					"precip")], method="pearson"))
	corr.groups[[i]]$chr <- row.names(corr.groups[[i]])
}

summary(corr.groups)
corr.groups[[]]

for(i in names(corr.groups)){
	corr.groups[[i]][,"chr"] <- substr(corr.groups[[i]][,"chr"], 4, 6)
}
corr.groups[[1]]

site.df <- data.frame(Site.Group=names(corr.groups))
chr <- data.frame(chr=c("std", "res"))
site.chr <- merge(site.df, chr)

group.df <- merge(site.chr, data.frame(Factor=c("tmean", "precip")))




for(i in names(corr.groups)){
	for(j in unique(group.df$Factor)){
		for(k in unique(group.df$chr)){
						
			group.df[group.df$Site.Group==i & group.df$Factor==j & group.df$chr==k, "cor"] <- 
						
						corr.groups[[i]][corr.groups[[i]][,"chr"]==k, j]
		}
	}
}
summary(group.df)

# Adding in Site for each entry
for(i in unique(group.df$Site.Group)){
	if(substr(paste(group.df[group.df$Site.Group==i,"Site.Group"]),1,2)=="Ha") {group.df[group.df$Site.Group==i,"Site"] <- "MA"}
	if(substr(paste(group.df[group.df$Site.Group==i,"Site.Group"]),1,2)=="Ho") {group.df[group.df$Site.Group==i,"Site"] <- "ME"}
	if(substr(paste(group.df[group.df$Site.Group==i,"Site.Group"]),1,2)=="Mi") {group.df[group.df$Site.Group==i,"Site"]<- "MO"}
	if(substr(paste(group.df[group.df$Site.Group==i,"Site.Group"]),1,2)=="Mo") {group.df[group.df$Site.Group==i,"Site"] <- "IN"}
	if(substr(paste(group.df[group.df$Site.Group==i,"Site.Group"]),1,2)=="Oa") {group.df[group.df$Site.Group==i,"Site"] <- "OH"}
}
group.df 

# Adding in Species for each entry

for(i in unique(group.df$Site.Group)){
	group.df[group.df$Site.Group==i, "Species"] <- str_sub(group.df[group.df$Site.Group==i, "Site.Group"], -4,-1)
}
group.df$Species <- as.factor(group.df$Species)

group.df$Species <- recode(group.df$Species, "'TULA' = 'BETULA'")

group.df$Site <- factor(group.df$Site, levels = c("MO", "IN", "OH", "MA", "ME"))

# Sig value for 75 df = 0.2245
group.df$sig <- ifelse(group.df$cor < -0.2245 | group.df$cor > 0.2245, "Y", "N")
group.df$sig <- factor(group.df$sig, levels = c("Y", "N"))

source("poster_theme.R")

group.df$Factor <- factor(group.df$Factor, levels=c("tmean", "precip"))

pdf("figures/prelim_figures/spp_correlations.pdf", width= 13, height = 8.5)
ggplot(data = group.df[group.df$chr=="std",]) + facet_grid(Factor~Species) + 
	geom_bar(aes(x=Site, y=cor, fill=sig), stat="identity", position="dodge", colour="black") +
	geom_hline(yintercept=0.2245, linetype="dashed") + 
	geom_hline(yintercept=-0.2245, linetype="dashed") + 	
	geom_hline(yintercept=0, linetype="solid") +
	scale_fill_manual(values= c("green", "grey50")) + 
	poster.theme2+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(2), face="bold"))
dev.off()
	

pdf("figures/spp_correlations_oak.pdf", width= 13, height = 8.5)
ggplot(data = group.df[group.df$chr=="std" & substr(group.df$Species,1,2)=="QU",]) + facet_grid(Factor ~ Site) + 
	geom_bar(aes(x=Species, y=cor, fill=sig), stat="identity", position="dodge", colour="black") +
	geom_hline(yintercept=0.2245, linetype="dashed") + 
	geom_hline(yintercept=-0.2245, linetype="dashed") + 	
	geom_hline(yintercept=0, linetype="solid") +
	scale_fill_manual(values= c("green", "grey50")) + 
	poster.theme2+
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(2), face="bold"))
dev.off()
