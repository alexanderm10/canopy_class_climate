# Reading in tree-ring data for NE sites to run diameter reconstructions for the size effect in the model
library(dplR)
library(car)
library(stringr)
# Reading in tree-level data
# just using the six sites from the NE area
# Harvard and Howland are already done from Dissertation
#
# skipping first three lines of file

gb.tree <- read.csv("Neil_data/neil_plot_data/GillBrookAllPlots.csv", skip=3, header=T)
ge.tree <- read.csv("Neil_data/neil_plot_data/GooseEggAllPlots.csv", skip=3, header=T)
pis.tree <- read.csv("Neil_data/neil_plot_data/PisgahTractAllPlots.csv", skip=3, header=T)
rh.tree <- read.csv("Neil_data/neil_plot_data/RoosterHillAllPlots.csv", skip=3, header=T)
nrp.tree <- read.csv("Neil_data/neil_plot_data/NorthRoundPondAllPlots_wplot4.csv", skip=3, header=T)
lyf.tree <- read.csv("Neil_data/neil_plot_data/LyfordAllPlots.csv", skip=3, header=T)

names(gb.tree)
names(ge.tree)
names(pis.tree)
names(rh.tree)
names(nrp.tree)
names(lyf.tree)

# Cutting down the columns that we are looking at
# need: site, tree.number, species, canopy, status, dbh, distance, azimuth
# only looking at living trees as well

gb.tree <- gb.tree[gb.tree$Status=="Li",c("Site", "Tree.Number", "Species", "Canopy", "Status", "Year.Sample", "DBH")]
ge.tree <- ge.tree[ge.tree$Status=="Li",c("Site", "Tree.Number", "Species", "Canopy", "Status", "Year.Sample", "DBH")]
pis.tree <- pis.tree[pis.tree$Status=="Li",c("Site", "Tree.Number", "Species", "Canopy", "Status", "Year.Sample", "DBH")]
rh.tree <- rh.tree[rh.tree$Status=="Li",c("Site", "Tree.Number", "Species", "Canopy", "Status", "Year.Sample", "DBH")]
nrp.tree <- nrp.tree[nrp.tree$Status=="Li",c("Site", "Tree.Number", "Species", "Canopy", "Status", "Year.Sample", "DBH")]
lyf.tree <- lyf.tree[lyf.tree$Status=="Li",c("Site", "Tree.Number", "Species", "Canopy", "Status", "Year.Sample", "DBH")]

# combining separate objects together into one tree-data object

ne.tree.data <- rbind(gb.tree, ge.tree, pis.tree, rh.tree, nrp.tree, lyf.tree)
summary(ne.tree.data)

# making a TreeID column to link rwl files with correct metadata

ne.tree.data$TreeID <- as.factor(paste0(ne.tree.data$Site, str_pad(ne.tree.data$Tree.Number,3,pad=0)))
summary(ne.tree.data)
ne.tree.data$Site.Code <- as.factor(substr(ne.tree.data$Site,1,2))

# recoding canopy status to fit in with dissertation codes
# D= dominant ; c = codominant; U = suppressed (understory); I = intermediate

ne.tree.data$Canopy.Class <- recode(ne.tree.data$Canopy, "'Dominant'='D';'dominant'='D';'codominant'='C';'Codominant'='C'; 'intermediate' ='I';'Intermediate' ='I';'suppressed'='U';'Suppressed'='U'")

# dropping the full word canopy class
ne.tree.data <- ne.tree.data[,!names(ne.tree.data)%in% "Canopy"]
summary(ne.tree.data)

ne.tree.data$Year.Sample <- as.factor(ne.tree.data$Year.Sample)

summary(ne.tree.data)
names(ne.tree.data) <- c("Plot", "Tree.Number", "Species", "Live.Dead", "Year.Sample", "DBH", "TreeID", "Site.Code", "Canopy.Class")
write.csv(ne.tree.data,"processed_data/NE_tree_metadata.csv", row.names=F)

