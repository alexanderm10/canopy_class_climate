# Reading in RW data for each tree
# Will need to aggregate from the core level to the tree level
library(stringr)

ne.tree.data <- read.csv("processed_data/NE_tree_metadata.csv", header=T)
summary(ne.tree.data$Site)

dir <- Sys.glob("Neil_data/rwl_files/*.rwl")


test <- read.tucson("Neil_data/rwl_files/GB1_ACPE.rwl")
summary(test)

core.rw <- NULL

for(i in dir){
  temp <- read.tucson(i)  
  # names(temp)[[1]] <- c(str_sub(i,-10,-5))
  
  # # having to put in a loop to fix the canada names
  # if(substr(names(temp)[[1]],1,2)== "na"){
  #   names(temp)[[1]] <- paste0("ca", names(temp)[[1]])
  # }
  
  temp$Year <- as.numeric(row.names(temp))
  
  if(is.null(core.rw)) {
    core.rw <- data.frame(temp)
  }  else { core.rw <- data.frame(merge(core.rw, temp, by= "Year", all.x=T, all.y=T))
  
  }
}

dim(core.rw)
head(core.rw[,1:10])
test <- core.rw 

# for(i in names(test)){

  names(core.rw) <- ifelse(substr(names(core.rw),1,2)=="LF",
                                paste0(substr(names(core.rw),1,3), str_pad(substr(names(core.rw),4,6),4,pad=0)),
                               paste(names(core.rw)))
# }
# paste0(ne.tree.data$Site, str_pad(ne.tree.data$Tree.Number,3,pad=0)))

summary(core.rw)

write.csv(core.rw, "processed_data/NE_ringwidths.csv")

# aggregating measurements together to get to the tree level

core.names <- data.frame(sampleID = unique(names(core.rw[,2:ncol(core.rw)])))



core.names$TreeID <- as.factor(ifelse(substr(core.names$sampleID, 1, 2)=="NR", 
                                      substr(core.names$sampleID, 1, 7), 
                                      substr(core.names$sampleID, 1, 6)))

core.names$coreID <- as.factor(ifelse(str_sub(core.names$sampleID,-1,-1)=="1", 
                                      str_sub(core.names$sampleID,-2,-2), 
                                      str_sub(core.names$sampleID,-1,-1)))

write.csv(core.names, file = "processed_data/core_metadata.csv", row.names=F)

# for(i in unique(core.names$sampleID)){
  # name.length <- nchar(i)
  # core.names[core.names$sampleID==i, "TreeID"] <- str_sub(i,-1, -name.length)
# }

# core.names$TreeID <- as.factor(core.names$TreeID)

# core.names$TreeID <- str_sub(core.names$sampleID,-1,- nchar(core.names$coreID))
# str_sub(core.names$sampleID[core.names$sampleID=="1"],-1,-1)
# summary(core.names[core.names$coreID==FALSE,])
core.names[core.names$coreID=="T",]

summary(core.names)
summary(core.names$coreID)


# taking the mean ringwidths per tree

trees <- unique(core.names$TreeID)

tree.rw <- data.frame(array(NA, dim=c(nrow(core.rw), length(trees))))
row.names(tree.rw) <- core.rw$Year # labeling the rows with the years from our rwl
names(tree.rw)<-unique(core.names$TreeID)

dim(tree.rw)

for(i in unique(trees)){
  # getting the columns we're working with
  if(substr(i, 1, 2)=="NR"){
    cols <- which(substr(names(core.rw), 1, 7)==i)
  } else {
    cols <- which(substr(names(core.rw), 1, 6)==i)
  } 
  cores <- names(core.rw)[cols] # getting the name of the cores we're working with
  
  # -----------------------
  
      # -----------------------
      # If no cores are dated, take the mean of whatever we have and call the tree undated
      if(length(cores) >1){
        tree.rw[,which(trees==i)] <- rowMeans(core.rw[,names(core.rw) %in% cores], na.rm=T) 
      } else {
        tree.rw[,which(trees==i)] <- core.rw[,names(core.rw) %in% cores]
      }
}  
dim(tree.rw)      
summary(tree.rw[,300:350])
row.names(tree.rw)

tree.rw[row.names(tree.rw) >1950,1:3]

save(tree.rw, file="processed_data/NE_tree_ringwidths.Rdata")
write.tucson(tree.rw, fname="processed_data/NE_tr_tucson.rwl", long.names = T)
