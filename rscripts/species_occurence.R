#####################
# species_occurence.R
# Aggregate occurences across time for each camera/location
# The count is a crude number that doesn't account for the number of individuals in a picture

library(tidyverse)
library(lubridate)
library(dplyr)
library(plyr)

read_csv("~/Documents/Projects/ShinyCam/ShinyApps/LeafletApp/data/raw_dataprep/marin_data.csv") %>%
  select(Project.ID,Camera.Deployment.Begin.Date,Camera.Deployment.End.Date,
         Latitude.Resolution, Longitude.Resolution,Photo.Type,Image.ID,Genus.Species,
         Count,Deployment.Location.ID,Event,Date_Time.Captured) %>%
  filter(Photo.Type == "Animal") -> data_marin

data_marin %>%
  select(Project.ID, Latitude.Resolution, Longitude.Resolution, Genus.Species, Deployment.Location.ID) %>%
  add_count(Project.ID, Latitude.Resolution, Longitude.Resolution, Genus.Species, Deployment.Location.ID) %>%
  unique() -> data_new

output_path <-"~/Documents/Projects/ShinyCam/ShinyApps/LeafletApp/data/"
write.csv(data_new, paste(output_path, "marin_species_occurence.csv"))
