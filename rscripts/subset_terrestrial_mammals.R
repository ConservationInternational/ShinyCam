
####
# subset_terrestrial_mammals.R
# This shrinks the huge terrestrial mammals shapefile dataset and takes only the species that are in our data
# As it is terrestrial mammals, this was done only to TEAM dataset
# From 600 Mb to 100 Mb
# Please, refer to this to website http://www.iucnredlist.org/technical-documents/spatial-data and search for 
# Terrestrial Mammals that is under the main dataset Mammals. We only worked with terrestrial mammals in danger, 
# but the same procedure can be extended to other datasets.


library(dplyr)
library(rgeos)
library(rgdal)
library(tidyr)
# Set the path and workspace to to main ShinyCam directory (i.e. the one that has README.md file, ShinyApps directory,etc)
shinycam_path <- "/Users/efegraus/Documents/GitHub/ShinyCam"
setwd(shinycam_path)
shapefile_path <- paste(shinycam_path,"/ShinyApps/LeafletApp/data/Shapefiles/TERRESTRIAL_MAMMALS/",sep="")
terrestrial_mammals <- readOGR(shapefile_path, "TERRESTRIAL_MAMMALS", verbose = T) %>%
  spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
# read the TEAM data
data_path <- "ShinyApps/LeafletApp/data/"
Primary <- read.csv(paste(data_path,"marin_rate_of_detection_120secs.csv",sep=""), header = TRUE)
Primary <- unite(Primary, "newBinomial", Genus, Species, sep =" ",remove = FALSE)

indices<-c()
for(i in 1:nrow(terrestrial_mammals)){
  if (length(grep( as.character(terrestrial_mammals$binomial)[i], as.factor(Primary$newBinomial), ignore.case=TRUE , value=TRUE)) > 0)
      indices <- c(indices,i)
}
new_terrestrial_mammals <- terrestrial_mammals[indices,]
###EHF - MAKE IT WRITE TOE THE PROCESSED DATA OR SHAPEILFE DIR
writeOGR(layer_options = "RESIZE=YES" , obj=new_terrestrial_mammals, dsn="shrinked_data", layer="new_terrestrial_mammals", driver="ESRI Shapefile" )
