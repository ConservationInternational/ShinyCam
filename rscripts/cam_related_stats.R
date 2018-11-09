####
# cam_related_stats.R
# EHF: We need to review all of these and have cleaner operational statistics.
#
# This file includes the following summmary stats:
# All stats are by project ID and camera ID (deployment location ID)
# 01_count_images: Count of images
# 02_count_blanks: Count of blanks per camera trap
# 03_count_unknowns: Count of unknown images
# 04_count_uncatalogued: Count of uncatalogued images
# 05_count_wildlife: Count of wildlife images (see excluded species list)
# 06_count_human_related: Count of human-related images (uses excluded species list)
# 07_avg_photos_per_deployment: Average # of images per camera deployment
# test for temporal_fix branch
###############################
rm(list = ls())
library(dplyr)
library(data.table)
# Set the path and workspace  to to main ShinyCam directory (i.e. the one that has README.md file, ShinyApps directory,etc)
shinycam_path <- "/Users/efegraus/Documents/GitHub/ShinyCam/"
prj_name<- "Papandayan" # No spaces in names
setwd(shinycam_path)
source("rscripts/RShiny_functions.R")
# Set the file name and path to your raw data files. This should be a file name that is in the correct format. See README process for explanation
#Load Data
#df_name <- "YOUR FILENAME HERE"
#Exmample
df_name <- "Papandayan_joined_data.csv" # 
# Set the path to your local camera trap data file in the raw_dataprep ditectory
# ct_data <- read.csv(paste("YOUR LOCALPATH TO THE REAW DATA FILE",df_name,sep=""))
# Example
ct_data <-fread(paste("ShinyApps/LeafletApp/data/raw_dataprep/",df_name, sep=""))
###############################
# No user edits from here down.
old <- Sys.time()
#seting the output path
output_path <-paste(shinycam_path,"ShinyApps/LeafletApp/data/processed/",sep="")
###############################
# 01: Count of images [Number of images per camera in each location]
dm_01_count_images<- ct_data %>% group_by(Project.ID,Camera.ID,Deployment.Location.ID) %>% summarize(count_images = n())

write.csv(dm_01_count_images, file=paste(output_path,"dm_01_count_images.csv",sep=""), row.names = FALSE)

# 02: Count of blanks per camera trap
dm_02_count_blanks<- ct_data %>% group_by(Project.ID,Camera.ID,Deployment.Location.ID) %>% summarize(count_blanks = sum(Photo.Type == "Blank"))

write.csv(dm_02_count_blanks, file=paste(output_path,"dm_02_count_blanks.csv",sep=""), row.names = FALSE)

# 03: Count of unknown images
dm_03_count_unknowns <- ct_data %>% group_by(Project.ID,Camera.ID,Deployment.Location.ID) %>% summarize(count_unknowns = sum(Photo.Type == "Unknown"))

write.csv(dm_03_count_unknowns, file=paste(output_path,"dm_03_count_unknowns.csv",sep=""), row.names = FALSE)

# 04: Count of uncatalogued images
dm_04_count_uncatalogued  <- ct_data %>% group_by(Project.ID,Camera.ID,Deployment.Location.ID) %>% summarize(count_uncatalogued = sum(Photo.Type == ""))

write.csv(dm_04_count_uncatalogued, file=paste(output_path,"dm_04_count_uncatalogued.csv",sep=""), row.names = FALSE)

# 05: Count of animal images (not humans) # EHF: THis needs to be reevaluated and clearly communicated what is being filtered here.
# List Genus.Species to ignore:
# Homo sapiens%
# Canis familiaris%
# Felis silvestris
# Bos taurus
# Equus caballus-ferus

pattern <- c('Homo sapiens|Canis familiaris|Felis silvestris|Bos taurus|Equus caballus-ferus')
ind <-  which(ct_data$Genus.Species %like% pattern)
species <- unique(ct_data$Genus.Species[ind])

# excluded species list (for reference)
# "Bos taurus"                        "Homo sapiens-tourist"              "Canis familiaris"
# "Canis familiaris-leash"            "Homo sapiens-bicyclist"            "Homo sapiens-resident"
# "Homo sapiens-official-vehicle"     "Equus caballus-ferus"              "Homo sapiens-non-official-vehicle"
# "Felis silvestris"                  "Homo sapiens"
dm_05_count_wildlife <- ct_data %>% group_by(Project.ID,Camera.ID,Deployment.Location.ID) %>% 
  summarize(count_wildlife = sum(Photo.Type == "Animal" & !(Genus.Species %in% species)))

write.csv(dm_05_count_wildlife,file=paste(output_path,"dm_05_count_wildlife.csv",sep=""), row.names = FALSE)

# 06: Count of human-related images

dm_06_count_human_related <- group_by(ct_data, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(count_human_related = n_distinct(Image.ID[Photo.Type == "Animal"
  & (Genus.Species %in% species)]))

write.csv(dm_06_count_human_related, file=paste(output_path,"dm_06_count_human_related.csv",sep=""), row.names = FALSE)

# 07: Average # of photos per camera deployment

dm_07_avg_photos_per_deployment <- group_by(ct_data, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(num_deployments = n_distinct(Deployment.ID),
  count_photos = n_distinct(Image.ID)) %>%
  mutate(avg_photos_per_deployment = round(count_photos / num_deployments, 2))

write.csv(dm_07_avg_photos_per_deployment, file=paste(output_path,"dm_07_avg_photos_per_deployment.csv",sep=""), row.names = FALSE)

