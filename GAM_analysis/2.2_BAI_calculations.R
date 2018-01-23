# Calculating BAI for the DOE sites

data.use <- read.csv("processed_data/AllSites_tree_plus_climate.csv", header=T)
summary(data.use)

# Calculating basal area increment to use as our response variable
# first lets calcualte basal area

data.use$BA <- pi*(data.use$dbh.recon/2)^2

for(t in unique(data.use$TreeID)){
	# if(min(data.use[data.use$TreeID, "BA"], na.rm=T))
	if(length(data.use[data.use$TreeID==t & !is.na(data.use$BA), "Year"])<=1) next # skip things that don't have more than 1 year
	for(y in min(data.use[data.use$TreeID==t & !is.na(data.use$BA), "Year"]) : (max(data.use[data.use$TreeID==t & !is.na(data.use$BA),"Year"])-1) ){
		data.use[data.use$TreeID==t & data.use$Year==y,"BA.inc"] <- data.use[data.use$TreeID==t & data.use$Year==y+1,"BA"] -
		 					data.use[data.use$TreeID==t & data.use$Year==y,"BA"]		
	}
	
}

write.csv(data.use, "processed_data/AllSites_tree_plus_climate_and_BA.csv", row.names=F)