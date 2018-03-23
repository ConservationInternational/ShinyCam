#####################
# species_occurence.R
# Aggregate occurences across time for each camera/location
# The count is a crude number that doesn't account for the number of individuals in a picture

library(tidyverse)
library(lubridate)
library(dplyr)
library(plyr)

data <- read_csv("~/Documents/Projects/ShinyCam/ShinyApps/LeafletApp/data/raw_dataprep/final_count_120secs.csv") 
data %>%
  select(Project.ID, Genus.Species, Deployment.Location.ID, total) %>%
  group_by(Project.ID, Genus.Species, Deployment.Location.ID) %>%
  summarise(event_total = n(), individual_total = sum(total), max_sighted = max(total)) %>%
  inner_join(., select(data, Deployment.Location.ID, Latitude.Resolution, Longitude.Resolution), by = "Deployment.Location.ID") %>%
  unique() -> data_new

# genus_species <- cbind(data_new, as.data.frame(str_split_fixed(data_new$Genus.Species, " ", 2)))
# colnames(genus_species) <- c("Genus", "Species")
# data_names <- cbind(data_new, as.tibble(genus_species))

output_path <-"ShinyApps/LeafletApp/data/raw_dataprep"
write.csv(data_new, paste(output_path, "marin_species_occurence.csv"))
