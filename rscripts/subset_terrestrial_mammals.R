
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

shapefile_path <- "shapefiles"
terrestrial_mammals <- readOGR(shapefile_path, "TERRESTRIAL_MAMMALS", verbose = T) %>%
  spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
# read the TEAM data
Primary <- read.csv("team_rate_of_detection.csv", header = TRUE)
Primary <- unite(Primary, "newBinomial", Genus, Species, sep =" ",remove = FALSE)

indices<-c()
for(i in 1:nrow(terrestrial_mammals)){
  if (length(grep( as.character(terrestrial_mammals$binomial)[i], as.factor(Primary$newBinomial), ignore.case=TRUE , value=TRUE)) > 0)
      indices <- c(indices,i)
}
new_terrestrial_mammals <- terrestrial_mammals[indices,]

writeOGR(layer_options = "RESIZE=YES" , obj=new_terrestrial_mammals, dsn="shrinked_data", layer="new_terrestrial_mammals", driver="ESRI Shapefile" )
