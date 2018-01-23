library(dplR)
library(stringr)
dat.out <- "raw_input_files/ITRDB_data"

# load in metadata file

meta.data <- read.csv("processed_data/ne_chron_metdata.csv", header=T)
summary(meta.data$species)

meta.data$species <- substr(meta.data$species,1,4)

# Get list of QUAL chronologies
sites.QUAL <- meta.data[meta.data$species=="QUAL","ITRDB.id"]
chrons <- dir(dat.out, "a.crn") # getting all ARSTAN chronologies
chrons <- chrons[which(toupper(substr(chrons, 1, 5)) %in% toupper(sites.QUAL))] # Getting only our QUAL chrons
chrons


# bringing in all chronologies to make a matrix that we can use in the SVD
#setting input directory where the chronologies are located

dir <- Sys.glob("raw_input_files/chrons/ITRDB_data/")
dir.in <-  dir(dir, "a.crn")

test <- read.crn("raw_input_files/chrons/ITRDB_data/cana033a.crn")
summary(test)

# Need to load in all of the chronology files and organize into a single matrix
itrdb.chrons <- NULL

for(i in paste0(dir, dir.in)){
  temp <- read.crn(i, header=T)  
  names(temp)[[1]] <- c(str_sub(i,-10,-6))
  
  # having to put in a loop to fix the canada names
  if(substr(names(temp)[[1]],1,2)== "na"){
    names(temp)[[1]] <- paste0("ca", names(temp)[[1]])
  }
  
  temp$Year <- as.numeric(row.names(temp))
  
  if(is.null(itrdb.chrons)) {
    itrdb.chrons <- data.frame(temp[,c(1,3)])
  }  else { itrdb.chrons <- data.frame(merge(itrdb.chrons, temp[,!names(temp) %in% "samp.depth"], by= "Year", all.x=T, all.y=T))
  
  }
}

summary(itrdb.chrons)

row.names(itrdb.chrons) <- itrdb.chrons$Year



# trimming down the matrix to have a common overlapping period
# Choosing a period of 1900-1970 for now
itrdb.chrons2 <- itrdb.chrons[itrdb.chrons$Year <=1970 & itrdb.chrons$Year >= 1900,]

itrdb.chrons2 <- itrdb.chrons2[,!names(itrdb.chrons2) %in% "Year"]  

head(itrdb.chrons2)
tail(itrdb.chrons2)

# removing chronologies that do not fit within the window we are looking for

itrdb.chrons3 <- itrdb.chrons2[,colSums(is.na(itrdb.chrons2))==0]
dim(itrdb.chrons2)
dim(itrdb.chrons3)


save(itrdb.chrons3, file="processed_data/itrdb_chron_matrix.Rdata")
