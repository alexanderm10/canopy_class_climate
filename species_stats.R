load("processed_data/noaa_meta.Rdata")

summary(noaa.meta)

sites.neil <- read.csv("input_data/neil_ross_combo_update.csv", header=T)
summary(sites.neil)

fundiv.sites <- read.csv("input_data/FunDiv_european_sites.csv", header=T)

load("itrdb_europe.Rdata")
europe.itrdb <- itrdb.out
summary(europe.itrdb)

# US ITRDB
length(unique(noaa.meta$species.code))

# Europe ITRDB
length(unique(europe.itrdb$species.code))

# getting unique US Species

us.itrdb.spp <- noaa.meta$species.code
combo.spp <- sites.neil$species

us.spp <- c(paste(us.itrdb.spp), paste(combo.spp))

unique.spp <- as.data.frame(as.factor(unique(us.spp)))
names(unique.spp) <- "Ross.postdoc.list"

write.csv(unique.spp, file="processed_data/US_spp.csv", row.names=F)
