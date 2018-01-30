# Calculating diamteter of trees back through time.
# This allows for the BAI calculation and the size effect in the GAM models to come later.

# Loading in tree-level ring-width measurements
load("processed_data/NE_tree_ringwidths.Rdata")
summary(tree.rw[,400:450])

# Need to convert ring-width data from mm to cm

tree.rw.cm <- tree.rw/10

ring.data <- tree.rw.cm

# Loading in tree metadata

tree.data <- read.csv("processed_data/NE_tree_metadata.csv", header=T)
summary(tree.data$Site)
names(tree.data)
core.data <- read.csv("processed_data/core_metadata.csv")


# making a data frame with trees as columns and years as ros
ring.data$Year <- as.factor(row.names(ring.data))


recon.data <- ring.data[,!names(ring.data) %in% "Year"]
recon.data[(nrow(recon.data)-10):nrow(recon.data), 1:10]
# ---------------------------------------

# Ordering the data
recon.data <- recon.data[order(row.names(recon.data), decreasing=T),order(names(recon.data))]
recon.data[1:10, 1:10]
recon.data[1:10, (ncol(recon.data)-10):ncol(recon.data)]

dbh.recon <- recon.data
trees.check <- vector() # trees with negative DBH..cm.

############################################################
# Reconstructing DBH
############################################################


pb <- txtProgressBar(min=0, max=length(names(dbh.recon)), style=3)
for(j in names(dbh.recon)){
  
  setTxtProgressBar(pb, which(names(dbh.recon)==j))
  
  # Step 1: Replace filled years beyond the year in which a tree was sampled with NA (both trees.gapfilled & DBH..cm. recon); 
  # 	Gapfilled: filling the years where I changed 0 to 1e-6 back to 0
  #	DBH..cm.recon: fill year of sample with DBH..cm. when sampled
  recon.data[as.numeric(row.names(recon.data)) > tree.data[tree.data$TreeID==j, "Year.Sample"],j] <- NA
  recon.data[,j] <- ifelse(recon.data[,j]==1e-6, 0, recon.data[,j])
  
  dbh.recon[as.numeric(row.names(dbh.recon))>tree.data[tree.data$TreeID==j, "Year.Sample"],j] <- NA
  dbh.recon[as.numeric(row.names(dbh.recon))==tree.data[tree.data$TreeID==j, "Year.Sample"],j] <- tree.data[tree.data$TreeID==j, "DBH"]
  
  # Doing the DBH..cm. reconstruction	
  for(i in 2:(length(dbh.recon[,j]))){
    dbh.recon[i,j] <- ifelse(!is.na(recon.data[i-1,j]), dbh.recon[i-1,j] - recon.data[i-1,j]*2, dbh.recon[i,j]) # subtracting the previous year's growth from DBH..cm. to get that year's DBH..cm.
  }
  
  
  if(min(dbh.recon[,j], na.rm=T)<0) trees.check <- c(trees.check, j)
}
dbh.recon[1:20, 1:10]
dbh.recon[1:20, (ncol(dbh.recon)-20):ncol(dbh.recon)]
min(dbh.recon, na.rm=T)
trees.check
summary(dbh.recon[,trees.check])

# for trees with negative DBH..cm., working from the inside out
# If a tree has a negative DBH..cm., we're just going to add from the inside out (at time )
pb <- txtProgressBar(min=0, max=length(trees.check), style=3)
for(j in trees.check){
  setTxtProgressBar(pb, which(trees.check==j))
  if(min(dbh.recon[,j], na.rm=T)<0){
    dbh.recon[,j] <- recon.data[,j]
    for(i in (nrow(dbh.recon)-1):1){
      dbh.recon[i,j] <- sum(dbh.recon[i+1, j], recon.data[i,j]*2, na.rm=T)
    }
  }
}
min(dbh.recon, na.rm=T)
dbh.recon[1:10,trees.check]
summary(dbh.recon[, trees.check])
#trees.gapfilled[, trees.check]
tree.data[tree.data$TreeID %in% trees.check,]
# ---------------------------------------
write.csv(dbh.recon, "processed_data/DBHrecon_NE.csv", row.names=T)
write.csv(recon.data, "processed_data/NE_RingWidths_ALL.csv", row.names=T)

#############################################################################
# Getting the data in the correct format to be combined with the climate data.
