#####################
# species_occurence.R
# Aggregate occurences across time for each camera/location
# Takes output count csv from animal_count.R

library(tidyverse)
library(lubridate)
library(dplyr)
library(plyr)

data <- read_csv("~/Documents/Projects/ShinyCam/ShinyApps/LeafletApp/data/raw_dataprep/final_count_120secs.csv") 

#get unique camera ID's and lat longs
data_camera <- select(data, Deployment.Location.ID, Latitude.Resolution, Longitude.Resolution)
unq_id <- data_camera[!duplicated(data_camera$Deployment.Location.ID),] #choose only the first recorded lat long for a camera 
                                                                        #(can't use unique() because of the floating point)

data %>%
  select(Project.ID, Genus.Species, Deployment.Location.ID, total) %>%
  group_by(Project.ID, Genus.Species, Deployment.Location.ID) %>%
  dplyr::summarise(event_total = n(), individual_total = sum(total), max_sighted = max(total)) %>%
  inner_join(., unq_id, by = "Deployment.Location.ID") %>%
  unique() -> data_new

# genus_species <- cbind(data_new, as.data.frame(str_split_fixed(data_new$Genus.Species, " ", 2)))
# colnames(genus_species) <- c("Genus", "Species")
# data_names <- cbind(data_new, as.tibble(genus_species))

output_path <-"ShinyApps/LeafletApp/data/raw_dataprep/"
write.csv(data_new, paste(output_path, "marin_species_occurence.csv"))


