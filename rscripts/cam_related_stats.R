####
# cam_related_stats.R
# This file includes the following summmary stats:
# All stats are by project ID and camera ID (deployment location ID)
# 01_count_images: Count of images
# 02_count_blanks: Count of blanks per camera trap
# 03_count_unknowns: Count of unknown images
# 04_count_uncatalogued: Count of uncatalogued images
# 05_count_wildlife: Count of wildlife images (see excluded species list)
# 06_count_human_related: Count of human-related images (uses excluded species list)
# 07_avg_photos_per_deployment: Average # of images per camera deployment

rm(list = ls())

library(dplyr)
library(data.table)

setwd("/Users/saksman/ShinyCam/rscripts/")

dm <-  fread("../data/original/marin_data.csv", header = TRUE, sep = ',',
  data.table = FALSE)

# 01: Count of images [Number of images per camera in each location]

dm_01_count_images <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(count_images = n_distinct(Image.ID))

write.csv(dm_01_count_images, "../data/processed/dm_01_count_images.csv", row.names = FALSE)

# 02: Count of blanks per camera trap

dm_02_count_blanks <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(count_blanks = n_distinct(Image.ID[Photo.Type == "Blank"]))

write.csv(dm_02_count_blanks, "../data/processed/dm_02_count_blanks.csv", row.names = FALSE)

# 03: Count of unknown images

dm_03_count_unknowns <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(count_unknowns = n_distinct(Image.ID[Photo.Type == "Unknown"]))

write.csv(dm_03_count_unknowns, "../data/processed/dm_03_count_unknowns.csv", row.names = FALSE)

# 04: Count of uncatalogued images

dm_04_count_uncatalogued <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(count_uncatalogued = n_distinct(Image.ID[Photo.Type == ""]))

write.csv(dm_04_count_uncatalogued, "../data/processed/dm_04_count_uncatalogued.csv", row.names = FALSE)

# 05: Count of animal images (not humans)
# List Genus.Species to ignore:
# Homo sapiens%
# Canis familiaris%
# Felis silvestris
# Bos taurus
# Equus caballus-ferus

pattern <- c('Homo sapiens|Canis familiaris|Felis silvestris|Bos taurus|Equus caballus-ferus')
ind <-  which(dm$Genus.Species %like% pattern)
species <- unique(dm$Genus.Species[ind])

# excluded species list (for reference)
# "Bos taurus"                        "Homo sapiens-tourist"              "Canis familiaris"
# "Canis familiaris-leash"            "Homo sapiens-bicyclist"            "Homo sapiens-resident"
# "Homo sapiens-official-vehicle"     "Equus caballus-ferus"              "Homo sapiens-non-official-vehicle"
# "Felis silvestris"                  "Homo sapiens"

dm_05_count_wildlife <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(count_wildlife = n_distinct(Image.ID[Photo.Type == "Animal"
  & !(Genus.Species %in% species)]))

write.csv(dm_05_count_wildlife, "../data/processed/dm_05_count_wildlife.csv", row.names = FALSE)

# 06: Count of human-related images

dm_06_count_human_related <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(count_human_related = n_distinct(Image.ID[Photo.Type == "Animal"
  & (Genus.Species %in% species)]))

write.csv(dm_06_count_human_related, "../data/processed/dm_06_count_human_related.csv", row.names = FALSE)

# 07: Average # of photos per camera deployment

dm_07_avg_photos_per_deployment <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(num_deployments = n_distinct(Deployment.ID),
  count_photos = n_distinct(Image.ID)) %>%
  mutate(avg_photos_per_deployment = round(count_photos / num_deployments, 2))

write.csv(dm_07_avg_photos_per_deployment, "../data/processed/dm_07_avg_photos_per_deployment.csv", row.names = FALSE)

