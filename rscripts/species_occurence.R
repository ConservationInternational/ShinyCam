#####################
# species_occurence.R
# Aggregate occurences across time for each camera/location
# Takes output count csv from animal_count.R
# Clear the environment and load libraries
rm(list = ls())
library(tidyverse)
library(lubridate)
library(dplyr)
library(plyr)
# Set the path and workspace to to main ShinyCam directory (i.e. the one that has README.md file, ShinyApps directory,etc)
shinycam_path <- "/Users/efegraus/Documents/GitHub/ShinyCam/"
setwd(shinycam_path)
#Set project name
prj_name<- "marin" # No spaces in names

# load the final_count 120sec data file
species_occ_filename <- list.files(path="ShinyApps/LeafletApp/data/processed/", pattern = paste("final_count_120secs_",prj_name,".csv",sep=""))
data <- as.data.frame(fread(paste("ShinyApps/LeafletApp/data/processed/",species_occ_filename,sep="")))

#data <- read_csv("ShinyApps/LeafletApp/data/processed/final_count_120secs_marin_smpt.csv") 

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

output_path <-"ShinyApps/LeafletApp/data/processed/"
write.csv(data_new, paste(output_path, "species_occurence.csv",sep=""))


