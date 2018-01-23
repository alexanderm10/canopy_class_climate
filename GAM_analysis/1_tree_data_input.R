library(car)
library(dplR)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}


#################################################################################################
# Loading up .csv file that has meta data and RWL files for ring widths
# Also doing some housekeeping (unit conversions, name formats) up front to make the workflow smoother
#################################################################################################

#load in core details data sheet.  Has living/dead, pith info, measurement info.
#loading the dplR to use the basal area reconstruction functions.
core.data <- read.csv("~/Dropbox/PhD/Carbon Research/Calloc_TreeRingNPP/processed_data/DOE_core_data_may2016_update.csv", na.strings=c("", "NA", "#VALUE!", "*", " "), header=T)
summary(core.data)



write.csv(core.data, file="processed_data/core_data.csv", row.names=F)

#importing the diameter files of all trees sampled: includes tree id, spp, plot assignment, and DBH 
tree.data <- read.csv("~/Dropbox/PhD/Carbon Research/Calloc_TreeRingNPP/processed_data/DOE_tree_data_may2016_update.csv", na.strings=c("", "NA", "#VALUE!", "*"), header=T)

write.csv(tree.data, file="processed_data/tree_data.csv", row.names=F)

summary(tree.data)


##################################################################
# Importing ring widths
##################################################################
# Load in Ross DOE ring widths from the east
# importing ring widths of dated samples 

tree.rw <- read.csv("~/PhD/Carbon Research/Calloc_TreeRingNPP/processed_data/DOE_AllSites_may2016_Gapfilled.csv", header=T)

write.csv(tree.rw, file="processed_data/tree_rw.csv", row.names=F)

summary(tree.rw)

##################################################################
# Getting dbh reconstruction
##################################################################

dbh.recon <- read.csv("~/PhD/Carbon Research/Calloc_TreeRingNPP/processed_data/DOE_Allsites_may2016_updateGapFilling_DBHrecon_ALL.csv", header=T, row.names=1)
write.csv(dbh.recon, file="processed_data/dbh_recon.csv", row.names=F)

summary(dbh.recon)
row.names(dbh.recon)


dbh.recon.stack <- stack(dbh.recon)
summary(dbh.recon.stack)
names(dbh.recon.stack) <- c("dbh.recon", "TreeID")
dbh.recon.stack$Year <- as.numeric(paste(row.names(dbh.recon)))
summary(tree.rw)

data.use <- merge(tree.rw, dbh.recon.stack, all.x=T, all.y=F) 
summary(data.use)

write.csv(data.use, "processed_data/tree_data_use.csv", row.names=F)

