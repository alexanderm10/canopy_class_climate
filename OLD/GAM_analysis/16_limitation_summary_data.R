library(dplR)
# pulling canopy class limitation numbers for the results.

load("processed_data/gam2_weights_graph.R")
summary(data.graph)
gam2.limits <- data.graph

# Use weight.tmean2; weight.precip2; weight.dbh.recon2


# summary stats on the limitation timeseries
#Tmean

rwl.stats(as.data.frame(gam2.limits[gam2.limits$Canopy.Class=="Dominant", "weight.tmean2"]))
rwl.stats(as.data.frame(gam2.limits[gam2.limits$Canopy.Class=="Intermediate", "weight.tmean2"]))
rwl.stats(as.data.frame(gam2.limits[gam2.limits$Canopy.Class=="Suppressed", "weight.tmean2"]))

# Precip
rwl.stats(as.data.frame(gam2.limits[gam2.limits$Canopy.Class=="Dominant", "weight.precip2"]))
rwl.stats(as.data.frame(gam2.limits[gam2.limits$Canopy.Class=="Intermediate", "weight.precip2"]))
rwl.stats(as.data.frame(gam2.limits[gam2.limits$Canopy.Class=="Suppressed", "weight.precip2"]))


# Size
rwl.stats(as.data.frame(gam2.limits[gam2.limits$Canopy.Class=="Dominant", "weight.dbh.recon2"]))
rwl.stats(as.data.frame(gam2.limits[gam2.limits$Canopy.Class=="Intermediate", "weight.dbh.recon2"]))
rwl.stats(as.data.frame(gam2.limits[gam2.limits$Canopy.Class=="Suppressed", "weight.dbh.recon2"]))
