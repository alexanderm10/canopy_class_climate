# Calculating BAI for the DOE sites

data.use <- read.csv("processed_data/NESites_tree_plus_climate.csv", header=T)
summary(data.use)

# Calculating basal area increment to use as our response variable
# first lets calcualte total basal area back through time

data.use$BA <- pi*(data.use$dbh.recon/2)^2

# Now calculating basal area increment back through time
pb <- txtProgressBar(min=0, max=length(unique(data.use$TreeID)), style=3)
tind=0

data.use$BA.inc <- NA
for(t in unique(data.use$TreeID)){
  tind=tind+1
  setTxtProgressBar(pb, tind)
  # if(min(data.use[data.use$TreeID, "BA"], na.rm=T))
  dat.tree <- data.use[data.use$TreeID==t & !is.na(data.use$BA),]
  if(nrow(dat.tree)<=1) next 
  
  for(y in min(dat.tree$Year) : (max(dat.tree$Year)-1) ){
    dat.tree[dat.tree$Year==y,"BA.inc"] <- dat.tree[dat.tree$Year==y+1,"BA"] -
      dat.tree[dat.tree$Year==y,"BA"]		
  }
  
  data.use[data.use$TreeID==t & !is.na(data.use$BA),] <- dat.tree
}

summary(data.use)
write.csv(data.use, "processed_data/NESites_tree_plus_climate_and_BA.csv", row.names=F)



