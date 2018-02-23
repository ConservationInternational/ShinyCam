#######
# Eric Fegraus - 12/17/2017
# This script will merge Wildlife Insights formatted data into one dataframe and write out csv's
# General Process: 1. Merge data, 2. animal_count.R, cam_trap_nights.R, final_data.R
rm(list = ls())
# Load libraries
library(dplyr)
library(lubridate)
library(plyr)

#Set working dir
shinycam_path <- "/Users/efegraus/work/DataKind/ShinyCam_new/ShinyCam"
setwd(shinycam_path)

############################ 
# Make this generic so loads all WI formatted data in a top level directory and creates one big dataframe.
# Change the paths and datasets names to make this work. Data MUST be in Wildlife Insights downloadformat.
# Provide a template
#######
# Marin Data Load
# Cheda Jewel
cj_cameras <- read.csv("data/20-Cheda-and-Jewel-GGNRA/ChedaJewel-Cheda-and-Jewel-GGNRA-Cameras.csv")
cj_deployments <- read.csv("data/20-Cheda-and-Jewel-GGNRA/ChedaJewel-Cheda-and-Jewel-GGNRA-Deployments.csv")
cj_images <- read.csv("data/20-Cheda-and-Jewel-GGNRA/ChedaJewel-Cheda-and-Jewel-GGNRA-Images.csv")
#SP
sp_cameras <- read.csv("data/17-Samuel-P-Taylor-State-Park/Samuel-P-Taylor-Samuel-P-Taylor-State-Park-Cameras.csv")
sp_deployments <- read.csv("data/17-Samuel-P-Taylor-State-Park/Samuel-P-Taylor-Samuel-P-Taylor-State-Park-Deployments.csv")
sp_images <- read.csv("data/17-Samuel-P-Taylor-State-Park/Samuel-P-Taylor-Samuel-P-Taylor-State-Park-Images.csv")
#WD
wd_cameras <- read.csv("data/24-Marin-Municipal-Water-District/MMWD-Marin-Municipal-Water-District-Cameras.csv")
wd_deployments <- read.csv("data/24-Marin-Municipal-Water-District/MMWD-Marin-Municipal-Water-District-Deployments.csv")
wd_images <- read.csv("data/24-Marin-Municipal-Water-District/MMWD-Marin-Municipal-Water-District-Images.csv")
#GG
gg_cameras <- read.csv("data/25-Gary-Giacomini-MCP/GaryGiacomini-Gary-Giacomini-MCP-Cameras.csv")
gg_deployments <- read.csv("data/25-Gary-Giacomini-MCP/GaryGiacomini-Gary-Giacomini-MCP-Deployments.csv")
gg_images <- read.csv("data/25-Gary-Giacomini-MCP/GaryGiacomini-Gary-Giacomini-MCP-Images.csv")

#WH
wh_cameras <- read.csv("data/26-Marin-County-Parks-Cascade-Canyon-and-White-Hill/MCP-South-Marin-County-Parks-Cascade-Canyon-and-White-Hill-Cameras.csv")
wh_deployments <- read.csv("data/26-Marin-County-Parks-Cascade-Canyon-and-White-Hill/MCP-South-Marin-County-Parks-Cascade-Canyon-and-White-Hill-Deployments.csv")
wh_images <-      read.csv("data/26-Marin-County-Parks-Cascade-Canyon-and-White-Hill/MCP-South-Marin-County-Parks-Cascade-Canyon-and-White-Hill-Images.csv")

# South
s_cameras <- read.csv("data/27-Marin-County-Parks-French-Ranch-and-Roy-s-Redwoods/MCP-North-Marin-County-Parks-French-Ranch-and-Roy-s-Redwoods-Cameras.csv")
s_deployments <- read.csv("data/27-Marin-County-Parks-French-Ranch-and-Roy-s-Redwoods/MCP-North-Marin-County-Parks-French-Ranch-and-Roy-s-Redwoods-Deployments.csv")
s_images <-      read.csv("data/27-Marin-County-Parks-French-Ranch-and-Roy-s-Redwoods/MCP-North-Marin-County-Parks-French-Ranch-and-Roy-s-Redwoods-Images.csv")
#######
#Bind the datasetst together
cameras <-rbind(cj_cameras, sp_cameras,wd_cameras,gg_cameras,wh_cameras,s_cameras)
deployments <-rbind(cj_deployments, sp_deployments,wd_deployments,gg_deployments,wh_deployments,s_deployments)
images <-rbind(cj_images, sp_images,wd_images,gg_images,wh_images,s_images)

# Join them together
cam_deploy <-inner_join(deployments,cameras,by="Camera.ID")
marin_data <- inner_join(images,cam_deploy,by="Deployment.ID")

# Write out the fully combined dataset.
write.csv(marin_data,file="data/marin_data.csv")
# Remove all images that we know don't have an animal
marin_data_animals <- marin_data[which(marin_data$Photo.Type == "Animal"),]# | marin_data$Photo.Type == "Unknown" 
##  | marin_data$Photo.Type == "Unidentifiable"),]
# Write out the full dataset with only animal images/records
write.csv(marin_data_animals,file="data/marin_data_animals.csv")
############################ 
