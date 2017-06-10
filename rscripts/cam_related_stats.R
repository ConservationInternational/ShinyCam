####
# cam_related_stats.R
# This file includes the following summmary stats:
# All stats are by project ID and camera ID (deployment location ID)
# 01: Count of images
# 02: Count of blanks per camera trap
# 03: Count of unknown images
# 04: Count of uncatalogued images
# 05: Count of wildlife images (see excluded species list)
# 06: Count of human-related images (uses excluded species list)
# 07: Average # of images per camera

rm(list = ls())

library(dplyr)
library(data.table)

setwd("/Users/saksman/ShinyCam/rscripts/")

dm <-  fread("../data/original/marin_data.csv", header = TRUE, sep = ',',
  data.table = FALSE)

# 01: Count of images [Number of images per camera in each location]

dm_01 <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(count_images = n_distinct(Image.ID))

write.csv(dm_01, "../data/processed/dm_01.csv", row.names = FALSE)

# 02: Count of blanks per camera trap

dm_02 <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(count_blanks = n_distinct(Image.ID[Photo.Type == "Blank"]))

write.csv(dm_02, "../data/processed/dm_02.csv", row.names = FALSE)

# 03: Count of unknown images

dm_03 <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(count_unknowns = n_distinct(Image.ID[Photo.Type == "Unknown"]))

write.csv(dm_03, "../data/processed/dm_03.csv", row.names = FALSE)

# 04: Count of uncatalogued images

dm_04 <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(count_uncatalogued = n_distinct(Image.ID[Photo.Type == ""]))

write.csv(dm_04, "../data/processed/dm_04.csv", row.names = FALSE)

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

dm_05 <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(count_wildlife = n_distinct(Image.ID[Photo.Type == "Animal"
  & !(Genus.Species %in% species)]))

write.csv(dm_05, "../data/processed/dm_05.csv", row.names = FALSE)

# 06: Count of human-related images

dm_06 <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(count_wildlife = n_distinct(Image.ID[Photo.Type == "Animal"
  & (Genus.Species %in% species)]))

write.csv(dm_06, "../data/processed/dm_06.csv", row.names = FALSE)

# 07: Average # of photos per camera (deployment ID)

dm_07 <- group_by(dm, Project.ID, Camera.ID, Deployment.Location.ID) %>%
  summarize(num_deployments = n_distinct(Deployment.ID),
  count_photos = n_distinct(Image.ID)) %>%
  mutate(photos_per_deployment = round(count_photos / num_deployments, 2))

write.csv(dm_07, "../data/processed/dm_07.csv", row.names = FALSE)

