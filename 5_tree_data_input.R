library(car)
library(dplR)
se <- function(x){
  sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}


#################################################################################################
# Loading up .csv file that has meta data and RWL files for ring widths
# Also doing some housekeeping (unit conversions, name formats) up front to make the workflow smoother
#################################################################################################

# #load in core details data sheet.  Has living/dead, pith info, measurement info.
# #loading the dplR to use the basal area reconstruction functions.
# core.data <- read.csv("~/PhD/Carbon Research/Calloc_TreeRingNPP/processed_data/DOE_core_data_may2016_update.csv", na.strings=c("", "NA", "#VALUE!", "*", " "), header=T)
# summary(core.data)
# 
# 
# 
# write.csv(core.data, file="processed_data/core_data.csv", row.names=F)

#importing the diameter files of all trees sampled: includes tree id, spp, plot assignment, and DBH 
tree.data <- read.csv("processed_data/NE_tree_metadata.csv", header=T)

write.csv(tree.data, file="processed_data/tree_data.csv", row.names=F)

summary(tree.data)


##################################################################
# Importing ring widths
##################################################################
# Load in Ross DOE ring widths from the east
# importing ring widths of dated samples 
load("processed_data/NE_tree_ringwidths.Rdata")
tree.rw <- tree.rw/10


write.csv(tree.rw, file="processed_data/tree_rw.csv", row.names=F)

summary(tree.rw)

tree.rw.stack <- stack(tree.rw)
summary(tree.rw.stack)
names(tree.rw.stack) <- c("RW", "TreeID")
tree.rw.stack$Year <- as.numeric(paste(row.names(tree.rw)))
summary(tree.rw.stack)
head(tree.rw.stack[1:5,])

##################################################################
# Getting dbh reconstruction
##################################################################

dbh.recon <- read.csv("processed_data/DBHrecon_NE.csv", header=T, row.names=1)
write.csv(dbh.recon, file="processed_data/dbh_recon.csv", row.names=F)

summary(dbh.recon)
row.names(dbh.recon)


dbh.recon.stack <- stack(dbh.recon)
summary(dbh.recon.stack)
names(dbh.recon.stack) <- c("dbh.recon", "TreeID")
dbh.recon.stack$Year <- as.numeric(paste(row.names(dbh.recon)))
summary(tree.rw.stack)
tail(tree.rw.stack)

# Combining tree data with RW and diam recon
names(tree.data)
tail(tree.data)

data.use <- merge(tree.rw.stack, tree.data ,by="TreeID",all.x=F, all.y=F)
head(data.use)
data.use <- data.use[order(data.use$Year,data.use$TreeID, decreasing=T),]


data.use2 <- merge(data.use, dbh.recon.stack, all.x=T, all.y=F) 
summary(data.use2)
tail(data.use2)

write.csv(data.use2, "processed_data/tree_data_use.csv", row.names=F)

