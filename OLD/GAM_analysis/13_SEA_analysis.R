###########################################################
# Attemping an SEA analysis
###########################################################
library(ggplot2)
require(mgcv)
require(lsmeans)
require(car)
require(moments)
require(reshape)
require(dplR)

load("processed_data/gam2_weights_processed.Rdata")
summary(gam2.weights)

summary(gam2.weights)
gam2.weights <- gam2.weights[gam2.weights$Year >=1950 & gam2.weights$Year <=2012,]
gam2.weights$Year <- as.factor(gam2.weights$Year)

sea.df <- recast(gam2.weights[,c("Year", "TreeID", "BA.inc.Clim")], Year~TreeID)
summary(sea.df)
row.names(sea.df) <- sea.df$Year
names(sea.df)
sea.df <- sea.df[,!names(sea.df)%in% "Year"]

# Chrons
mo.sea.chron <- list(dom = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="MO", "TreeID"]], prefix="MOD")),
               int = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="MO", "TreeID"]], prefix="MOI")),
               sup = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="MO", "TreeID"]], prefix="MOS")))

in.sea.chron <- list(dom = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="IN", "TreeID"]], prefix="IND")),
               int = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="IN", "TreeID"]], prefix="INI")),
               sup = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="IN", "TreeID"]], prefix="INS")))

oh.sea.chron <- list(dom = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="OH", "TreeID"]], prefix="OHD")),
               int = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="OH", "TreeID"]], prefix="OHI")),
               sup = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="OH", "TreeID"]], prefix="OHS")))

ma.sea.chron <- list(dom = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="MA", "TreeID"]], prefix="MAD")),
               int = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="MA", "TreeID"]], prefix="MAI")),
               sup = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="MA", "TreeID"]], prefix="MAS")))

me.sea.chron <- list(dom = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="ME", "TreeID"]], prefix="MED")),
               int = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="ME", "TreeID"]], prefix="MEI")),
               sup = as.data.frame(chron(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="ME", "TreeID"]], prefix="MES")))



# Upper CI
mo.sea.UB <- list(dom = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="MO", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)),
               int = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="MO", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)),
               sup = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="MO", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)))

in.sea.UB <- list(dom = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="IN", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)),
               int = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="IN", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)),
               sup = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="IN", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)))

oh.sea.UB <- list(dom = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="OH", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)),
               int = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="OH", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)),
               sup = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="OH", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)))

ma.sea.UB <- list(dom = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="MA", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)),
               int = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="MA", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)),
               sup = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="MA", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)))

me.sea.UB <- list(dom = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="ME", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)),
               int = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="ME", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)),
               sup = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="ME", "TreeID"]], 1, FUN=quantile, 0.975,na.rm=T)))

# LOWER CI
mo.sea.LB <- list(dom = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="MO", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)),
                  int = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="MO", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)),
                  sup = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="MO", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)))

in.sea.LB <- list(dom = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="IN", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)),
                  int = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="IN", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)),
                  sup = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="IN", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)))

oh.sea.LB <- list(dom = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="OH", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)),
                  int = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="OH", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)),
                  sup = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="OH", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)))

ma.sea.LB <- list(dom = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="MA", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)),
                  int = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="MA", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)),
                  sup = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="MA", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)))

me.sea.LB <- list(dom = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="ME", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)),
                  int = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="ME", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)),
                  sup = as.data.frame(apply(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="ME", "TreeID"]], 1, FUN=quantile, 0.025,na.rm=T)))


#############################################
# Individual Trees
mo.sea <- list(dom = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="MO", "TreeID"]]),
               int = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="MO", "TreeID"]]),
               sup = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="MO", "TreeID"]]))

in.sea <- list(dom = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="IN", "TreeID"]]),
               int = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="IN", "TreeID"]]),
               sup = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="IN", "TreeID"]]))

oh.sea <- list(dom = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="OH", "TreeID"]]),
               int = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="OH", "TreeID"]]),
               sup = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="OH", "TreeID"]]))

ma.sea <- list(dom = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="MA", "TreeID"]]),
               int = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="MA", "TreeID"]]),
               sup = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="MA", "TreeID"]]))

me.sea <- list(dom = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="D" & gam2.weights$State=="ME", "TreeID"]]),
               int = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="I" & gam2.weights$State=="ME", "TreeID"]]),
               sup = as.data.frame(sea.df[,names(sea.df)%in% gam2.weights[gam2.weights$Canopy.Class=="S" & gam2.weights$State=="ME", "TreeID"]]))



marker.years <- list()


hot.years <- data.frame(array(NA,dim = c(4,5)))
names(hot.years) <- unique(gam2.weights$State)

for(i in unique(gam2.weights$State)){
  hot.years[,names(hot.years)==i] <- unique(gam2.weights$Year[gam2.weights$State==i & gam2.weights$Temp.Mark=="hot"])
}
 
hot.years 

cold.years <- data.frame(array(NA,dim = c(4,5)))
names(cold.years) <- unique(gam2.weights$State)

for(i in unique(gam2.weights$State)){
  cold.years[,names(cold.years)==i] <- unique(gam2.weights$Year[gam2.weights$State==i & gam2.weights$Temp.Mark=="cold"])
}

cold.years 

wet.years <- data.frame(array(NA,dim = c(4,5)))
names(wet.years) <- unique(gam2.weights$State)

for(i in unique(gam2.weights$State)){
  years <- unique(gam2.weights$Year[gam2.weights$State==i & gam2.weights$Precip.Mark=="wet"])
  years <- years[!is.na(years)]
  wet.years[,names(wet.years)==i] <- c(years, rep(NA, nrow(wet.years)-length(years)))
  }

wet.years <-wet.years +1950


dry.years <- data.frame(array(NA,dim = c(4,5)))
names(dry.years) <- unique(gam2.weights$State)

for(i in unique(gam2.weights$State)){
  dry.years[,names(dry.years)==i] <- unique(gam2.weights$Year[gam2.weights$State==i & gam2.weights$Precip.Mark=="dry"])
}

dry.years 


marker.years <- list(hot.years, dry.years, wet.years, cold.years)
names(marker.years) <- c("hot", "dry", "wet", "cold")


sea.data <- list(mo.sea, in.sea, oh.sea, ma.sea, me.sea)
names(sea.data) <- c("MO", "IN", "OH", "MA", "ME")

# sea.output <- data.frame(array(NA, dim=c(1,11)))
# names(sea.output) <- c("lag", "se", "se.unscaled", "p", "ci.95.lower", "ci.95.upper","ci.99.lower", "ci.99.upper", "State", "Canopy.Class", "Mark")

# Indiv trees
pb <- txtProgressBar(min=0, max=length(names(sea.data)), style=3)

sea.output=NULL

for(s in names(sea.data)){
    for(c in names(sea.data[[s]])){
       for(t in names(sea.data[[s]][[c]])){  
        for(m in names(marker.years)){
          setTxtProgressBar(pb,s)
          
          marker <- as.numeric(as.vector(marker.years[[m]][,s]))
        
          df <- as.data.frame(sea.data[[s]][[c]][,t])
          row.names(df) <- c(1950:2013)
          
          yr.range <- as.numeric(row.names(df)[which(!is.na(df))]) # find the years of the core
          marker <- marker[marker>min(yr.range) & marker<max(yr.range)] # only use events that are in the range the core covers
          if(length(marker)==0) next # If the core doesn't cover any of your markers, skip this tree
          
          df <- as.data.frame(df[!is.na(df),])
         
          row.names(df) <- yr.range
          sea.temp <- sea(df, marker, lag=1, resample=10000) 
          
          sea.temp$State <- as.factor(s)
          sea.temp$Canopy.Class <- as.factor(c)
          sea.temp$Mark <- as.factor(m)
          sea.temp$TreeID <- as.factor(t)
          
          if(is.null(sea.output)){ 
            sea.output <- sea.temp
          } else{
            sea.output <- rbind(sea.output, sea.temp)
          }
      } # End marker
     } # End Tree
  } # end Canopy
} # End State
summary(sea.output)


######################################################################################################
# Chronologies
sea.data.chron <- list(mo.sea.chron, in.sea.chron, oh.sea.chron, ma.sea.chron, me.sea.chron)
names(sea.data.chron) <- c("MO", "IN", "OH", "MA", "ME")
save(sea.data.chron, file="processed_data/site_cc_chrons.Rdata")

sea.output.chron = NULL
for(s in names(sea.data.chron)){
  for(c in names(sea.data.chron[[s]])){
    #for(t in names(sea.data[[s]][[c]][,1])){  
      for(m in names(marker.years)){
        setTxtProgressBar(pb,s)
        
        marker <- as.numeric(as.vector(marker.years[[m]][,s]))
        
        df <- as.data.frame(sea.data.chron[[s]][[c]][,1])
        row.names(df) <- c(1950:2013)
        
        yr.range <- as.numeric(row.names(df)[which(!is.na(df))]) # find the years of the core
        marker <- marker[marker>min(yr.range) & marker<max(yr.range)] # only use events that are in the range the core covers
        if(length(marker)==0) next # If the core doesn't cover any of your markers, skip this tree
        
        df <- as.data.frame(df[!is.na(df),])
        
        row.names(df) <- yr.range
        sea.temp <- sea(df, marker, lag=1, resample=10000) 
        
        sea.temp$State <- as.factor(s)
        sea.temp$Canopy.Class <- as.factor(c)
        sea.temp$Mark <- as.factor(m)
       # sea.temp$TreeID <- as.factor(t)
        
        if(is.null(sea.output)){ 
          sea.output.chron <- sea.temp
        } else{
          sea.output.chron <- rbind(sea.output.chron, sea.temp)
        }
      } # End marker
    #} # End Tree
  } # end Canopy
} # End State
summary(sea.output.chron)

save(sea.output.chron, file="processed_data/sea_output_chron.Rdata")

################################################################
# UPPER CI
################################################################
sea.data.UB <- list(mo.sea.UB, in.sea.UB, oh.sea.UB, ma.sea.UB, me.sea.UB)
names(sea.data.UB) <- c("MO", "IN", "OH", "MA", "ME")


sea.output.UB = NULL

for(s in names(sea.data.UB)){
  for(c in names(sea.data.UB[[s]])){
    #for(t in names(sea.data[[s]][[c]][,1])){  
    for(m in names(marker.years)){
      setTxtProgressBar(pb,s)
      
      marker <- as.numeric(as.vector(marker.years[[m]][,s]))
      
      df <- as.data.frame(sea.data.UB[[s]][[c]][,1])
      row.names(df) <- c(1950:2013)
      
      yr.range <- as.numeric(row.names(df)[which(!is.na(df))]) # find the years of the core
      marker <- marker[marker>min(yr.range) & marker<max(yr.range)] # only use events that are in the range the core covers
      if(length(marker)==0) next # If the core doesn't cover any of your markers, skip this tree
      
      df <- as.data.frame(df[!is.na(df),])
      
      row.names(df) <- yr.range
      sea.temp <- sea(df, marker, lag=1, resample=10000) 
      
      sea.temp$State <- as.factor(s)
      sea.temp$Canopy.Class <- as.factor(c)
      sea.temp$Mark <- as.factor(m)
      # sea.temp$TreeID <- as.factor(t)
      
      if(is.null(sea.output)){ 
        sea.output.UB <- sea.temp
      } else{
        sea.output.UB <- rbind(sea.output.UB, sea.temp)
      }
    } # End marker
    #} # End Tree
  } # end Canopy
} # End State
summary(sea.output.UB)
save(sea.output.UB, file="processed_data/sea_output_UB.Rdata")
################################################################
# lowerr CI
################################################################
sea.data.LB <- list(mo.sea.LB, in.sea.LB, oh.sea.LB, ma.sea.LB, me.sea.LB)
names(sea.data.LB) <- c("MO", "IN", "OH", "MA", "ME")


sea.output.LB = NULL

for(s in names(sea.data.LB)){
  for(c in names(sea.data.LB[[s]])){
    #for(t in names(sea.data[[s]][[c]][,1])){  
    for(m in names(marker.years)){
      setTxtProgressBar(pb,s)
      
      marker <- as.numeric(as.vector(marker.years[[m]][,s]))
      
      df <- as.data.frame(sea.data.LB[[s]][[c]][,1])
      row.names(df) <- c(1950:2013)
      
      yr.range <- as.numeric(row.names(df)[which(!is.na(df))]) # find the years of the core
      marker <- marker[marker>min(yr.range) & marker<max(yr.range)] # only use events that are in the range the core covers
      if(length(marker)==0) next # If the core doesn't cover any of your markers, skip this tree
      
      df <- as.data.frame(df[!is.na(df),])
      
      row.names(df) <- yr.range
      sea.temp <- sea(df, marker, lag=1, resample=10000) 
      
      sea.temp$State <- as.factor(s)
      sea.temp$Canopy.Class <- as.factor(c)
      sea.temp$Mark <- as.factor(m)
      # sea.temp$TreeID <- as.factor(t)
      
      if(is.null(sea.output)){ 
        sea.output.LB <- sea.temp
      } else{
        sea.output.LB <- rbind(sea.output.LB, sea.temp)
      }
    } # End marker
    #} # End Tree
  } # end Canopy
} # End State
summary(sea.output.LB)
save(sea.output.LB, file="processed_data/sea_output_LB.Rdata")


######################################################################################################
# All Sites Chronologies
sea.data.chron <- list(mo.sea.chron, in.sea.chron, oh.sea.chron, ma.sea.chron, me.sea.chron)
names(sea.data.chron) <- c("MO", "IN", "OH", "MA", "ME")


sea.output.all = NULL
#for(s in names(sea.data.chron)){
  for(c in names(sea.data.chron[[s]])){
    #for(t in names(sea.data[[s]][[c]][,1])){  
    for(m in names(marker.years)){
      setTxtProgressBar(pb,s)
      
      marker <- as.numeric(as.vector(marker.years[[m]][,s]))
      
      df <- as.data.frame(sea.data.chron[[s]][[c]][,1])
      row.names(df) <- c(1950:2013)
      
      yr.range <- as.numeric(row.names(df)[which(!is.na(df))]) # find the years of the core
      marker <- marker[marker>min(yr.range) & marker<max(yr.range)] # only use events that are in the range the core covers
      if(length(marker)==0) next # If the core doesn't cover any of your markers, skip this tree
      
      df <- as.data.frame(df[!is.na(df),])
      
      row.names(df) <- yr.range
      sea.temp <- sea(df, marker, lag=1, resample=10000) 
      
      sea.temp$State <- as.factor("All")
      sea.temp$Canopy.Class <- as.factor(c)
      sea.temp$Mark <- as.factor(m)
      # sea.temp$TreeID <- as.factor(t)
      
      if(is.null(sea.output)){ 
        sea.output.all <- sea.temp
      } else{
        sea.output.all <- rbind(sea.output.all, sea.temp)
      }
    } # End marker
    #} # End Tree
  } # end Canopy
#} # End State
summary(sea.output.all)

save(sea.output.chron, file="processed_data/sea_output_chron.Rdata")
# Graphing of SEA output
# loading in datasets

load("processed_data/sea_output_LB.Rdata")
load("processed_data/sea_output_UB.Rdata")
load("processed_data/sea_output_chron.Rdata")



sea.output.chron$type <- as.factor("chron")
sea.output.UB$type <- as.factor("UB")
sea.output.LB$type <- as.factor("LB")

sea.output.chron$lag <- as.factor(sea.output.chron$lag)
sea.output.chron$sig <- as.factor(ifelse(sea.output.chron$p < 0.05, "Y", "N"))

sea.output.all$lag <- as.factor(sea.output.all$lag)
sea.output.all$sig <- as.factor(ifelse(sea.output.all$p < 0.05, "Y", "N"))
sea.output.all$type <- as.factor("chron")

sea.output.UB$lag <- as.factor(sea.output.UB$lag)
sea.output.UB$sig <- as.factor(ifelse(sea.output.UB$p < 0.05, "Y", "N"))

sea.output.LB$lag <- as.factor(sea.output.LB$lag)
sea.output.LB$sig <- as.factor(ifelse(sea.output.LB$p < 0.05, "Y", "N"))

sea.output.ci <- rbind(sea.output.UB, sea.output.LB)
summary(sea.output.ci)

summary(sea.output.UB)
summary(sea.output.LB)

sea.output.combo <- rbind(sea.output.chron, sea.output.UB, sea.output.LB, sea.output.all)




ggplot(data=sea.output.chron) + facet_grid(Mark~State) +
  geom_point(aes(x=lag, y=se, color=sig, shape=Canopy.Class), size=5)+
  geom_errorbar(aes(x=lag, ymin=sea.output.LB[,"se"], ymax=sea.output.UB[,"se"]))+
  geom_hline(yintercept=0, linetype="dashed")+
  scale_color_manual(values=c("grey", "red"))+
  theme_bw()


sea.output.combo$Canopy.Class <-recode(sea.output.combo$Canopy.Class, "'dom'='Dominant';'int'='Intermediate';'sup'='Understory'")
sea.output.combo$Mark <- recode(sea.output.combo$Mark, "'hot'='Hot';'cold'='Cool';'wet'='Wet';'dry'='Dry'")
sea.output.combo$Mark <- factor(sea.output.combo$Mark, levels=c("Hot", "Cool", "Dry", "Wet"))
save(sea.output.combo,file="processed_data/sea_output_combo.Rdata")

pdf("figures/Prelim_Figures/pub_figs/sea_mean.pdf", width=13, height=8.5)
ggplot(data=sea.output.combo[sea.output.combo$type=="chron",]) + facet_grid(Mark~State) +
  geom_line(aes(x=lag, y=se, color=Canopy.Class, group=Canopy.Class),size=0.5)+
  geom_point(aes(x=lag, y=se, color=Canopy.Class,position="identity"), size=4)+
  
  scale_color_manual(values=c("#0072B2", "#009E73", "#E69F00"))+
  geom_hline(yintercept=0, linetype="dashed")+
  #scale_color_manual(values=c("red", "burlywood3", "darkgreen", "blue")) +
  labs(x="Lag", y="Scaled BA-Inex")+
  theme(axis.line=element_line(color="black"), 
        panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(), 
        panel.border=element_blank(),  
        panel.background=element_blank(), 
        axis.text.x=element_text(angle=0, color="black", size=rel(1)), 
        axis.text.y=element_text(angle=0, color="black", size=rel(1)), 
        strip.text=element_text(face="bold", size=rel(1.0)),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="top",
        legend.key.size = unit(0.75, "cm"),
        legend.text = element_text(size=rel(1.1)),
        legend.key = element_rect(fill = "white")) + 
  guides(color=guide_legend(nrow=1)) +
  theme(axis.title.y= element_text(size=rel(1.1), face="bold")) +
  theme(axis.title.x= element_text(size=rel(1.1), face="bold"))
  
dev.off()


# SEA example
library(graphics)
library(utils)
data(cana157)
event.years <- c(1631, 1742, 1845)
cana157.sea <- sea(cana157, event.years)
foo <- cana157.sea$se.unscaled
names(foo) <- cana157.sea$lag
barplot(foo, col = ifelse(cana157.sea$p < 0.05, "grey30", "grey75"), 
        ylab = "RWI", xlab = "Superposed Epoch")

