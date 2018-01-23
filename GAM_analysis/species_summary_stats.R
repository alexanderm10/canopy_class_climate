# Getting some species stats for the grant
# Run fancy mapping script first

# Bounding box used for maps
lat.min <- 25.7617
lat.max <- 46.8640
lon.min <- -96.4003
lon.max <- -67.9980

# Loading in ITRDB
summary(dat.map)

# getting only the species that are within the bounding box
load("processed_data/noaa_meta.Rdata")
east.itrdb <- noaa.meta
summary(east.itrdb)

unique(east.itrdb$species.code)
length(unique(east.itrdb$species.code))
nrow(east.itrdb)

east.expand <- read.csv("input_data/neil_ross_combo_update.csv", header=T)
summary(east.expand)
unique(east.expand$species)
length(unique(east.itrdb$species.code))
nrow(east.itrdb)

# Christy's species
cr.tree.data <- read.csv("input_data/christy_TreeData.csv")

summary(cr.tree.data)
unique(cr.tree.data$Spp)
length(unique(cr.tree.data$Spp))

# Ross Species
mra.tree.data <- read.csv("input_data/AllSites_tree_plus_climate_and_BA.csv")
summary(mra.tree.data)

mra.tree.data <- mra.tree.data[!mra.tree.data$Site %in% c("Niwot", "Valles Caldera Upper", "Valles Caldera Lower"),]

unique(mra.tree.data$Species)
length(unique(mra.tree.data$Species))


# combine species
mra.spp <- data.frame(species = mra.tree.data$Species,
                      ID = as.factor("mra"))
cr.spp <- data.frame(species = cr.tree.data$Spp,
                     ID = as.factor("cr"))
neil.spp <- data.frame(species = east.expand$Spp,
                       ID = as.factor("neil"))


expanded.spp.combo <- rbind(mra.spp, cr.spp, neil.spp)
unique(expanded.spp.combo$species)
length(unique(expanded.spp.combo$species))

length(unique(expanded.spp.combo$species[!expanded.spp.combo$species %in% east.itrdb$Spp]))

# we are adding at least 40 new species not included in teh ITRDB