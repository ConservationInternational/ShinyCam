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
shinycam_path <- "/Users/efegraus/work/DataKind/MarinDownload/july2018/"
setwd(shinycam_path)

############################ 
# Make this generic so loads all WI formatted data in a top level directory and creates one big dataframe.
# Change the paths and datasets names to make this work. Data MUST be in Wildlife Insights downloadformat.
# Provide a template
#######
# Load each Project Individually
# Cheda Jewel
cj_cameras <- read.csv("20-Cheda-and-Jewel-GGNRA/ChedaJewel-Cheda-and-Jewel-GGNRA-Cameras.csv")
cj_deployments <- read.csv("20-Cheda-and-Jewel-GGNRA/ChedaJewel-Cheda-and-Jewel-GGNRA-Deployments.csv")
cj_images <- read.csv("20-Cheda-and-Jewel-GGNRA/ChedaJewel-Cheda-and-Jewel-GGNRA-Images.csv")
#SP
sp_cameras <- read.csv("17-Samuel-P-Taylor-State-Park/Samuel-P-Taylor-Samuel-P-Taylor-State-Park-Cameras.csv")
sp_deployments <- read.csv("17-Samuel-P-Taylor-State-Park/Samuel-P-Taylor-Samuel-P-Taylor-State-Park-Deployments.csv")
sp_images <- read.csv("17-Samuel-P-Taylor-State-Park/Samuel-P-Taylor-Samuel-P-Taylor-State-Park-Images.csv")
#WD
wd_cameras <- read.csv("24-Marin-Municipal-Water-District/MMWD-Marin-Municipal-Water-District-Cameras.csv")
wd_deployments <- read.csv("24-Marin-Municipal-Water-District/MMWD-Marin-Municipal-Water-District-Deployments.csv")
wd_images <- read.csv("24-Marin-Municipal-Water-District/MMWD-Marin-Municipal-Water-District-Images.csv")
#GG
gg_cameras <- read.csv("25-Gary-Giacomini-MCP/GaryGiacomini-Gary-Giacomini-MCP-Cameras.csv")
gg_deployments <- read.csv("25-Gary-Giacomini-MCP/GaryGiacomini-Gary-Giacomini-MCP-Deployments.csv")
gg_images <- read.csv("25-Gary-Giacomini-MCP/GaryGiacomini-Gary-Giacomini-MCP-Images.csv")

#WH
wh_cameras <- read.csv("26-Marin-County-Parks-Cascade-Canyon-and-White-Hill/MCP-South-Marin-County-Parks-Cascade-Canyon-and-White-Hill-Cameras.csv")
wh_deployments <- read.csv("26-Marin-County-Parks-Cascade-Canyon-and-White-Hill/MCP-South-Marin-County-Parks-Cascade-Canyon-and-White-Hill-Deployments.csv")
wh_images <-      read.csv("26-Marin-County-Parks-Cascade-Canyon-and-White-Hill/MCP-South-Marin-County-Parks-Cascade-Canyon-and-White-Hill-Images.csv")

# South
s_cameras <- read.csv("27-Marin-County-Parks-French-Ranch-and-Roy-s-Redwoods/MCP-North-Marin-County-Parks-French-Ranch-and-Roy-s-Redwoods-Cameras.csv")
s_deployments <- read.csv("27-Marin-County-Parks-French-Ranch-and-Roy-s-Redwoods/MCP-North-Marin-County-Parks-French-Ranch-and-Roy-s-Redwoods-Deployments.csv")
s_images <-      read.csv("27-Marin-County-Parks-French-Ranch-and-Roy-s-Redwoods/MCP-North-Marin-County-Parks-French-Ranch-and-Roy-s-Redwoods-Images.csv")

# Beach
b_cameras <- read.csv("28-South-MWPIP-Beach/Beach-South-MWPIP-Beach-Cameras.csv")
b_deployments <- read.csv("28-South-MWPIP-Beach/Beach-South-MWPIP-Beach-Deployments.csv")
b_images <-      read.csv("28-South-MWPIP-Beach/Beach-South-MWPIP-Beach-Images.csv")

# East
e_cameras <- read.csv("29-South-MWPIP-East/East-South-MWPIP-East-Cameras.csv")
e_deployments <- read.csv("29-South-MWPIP-East/East-South-MWPIP-East-Deployments.csv")
# Pop-off notes columns
e_deployments2 <- subset(e_deployments,select=-c(Notes,Notes1))
e_images <-      read.csv("29-South-MWPIP-East/East-South-MWPIP-East-Images.csv")

# Tam
t_cameras <- read.csv("30-South-MWPIP-Tam/Tam-South-MWPIP-Tam-Cameras.csv")
t_deployments <- read.csv("30-South-MWPIP-Tam/Tam-South-MWPIP-Tam-Deployments.csv")
# Pop-off notes columns
t_deployments2 <- subset(t_deployments,select=-c(Notes,Notes1))
t_images <-      read.csv("30-South-MWPIP-Tam/Tam-South-MWPIP-Tam-Images.csv")

# wood
w_cameras <- read.csv("31-South-MWPIP-Wood/Wood-South-MWPIP-Wood-Cameras.csv")
w_deployments <- read.csv("31-South-MWPIP-Wood/Wood-South-MWPIP-Wood-Deployments.csv")
w_images <-      read.csv("31-South-MWPIP-Wood/Wood-South-MWPIP-Wood-Images.csv")

#######
#Bind the datasetst together
cameras <-rbind(cj_cameras, sp_cameras,wd_cameras,gg_cameras,wh_cameras,s_cameras,b_cameras,e_cameras,t_cameras,w_cameras)
deployments <-rbind(cj_deployments, sp_deployments,wd_deployments,gg_deployments,wh_deployments,s_deployments,b_deployments,e_deployments2,t_deployments2,w_deployments)
images <-rbind(cj_images, sp_images,wd_images,gg_images,wh_images,s_images,b_images,e_images,t_images,w_images)

# Join them together
cam_deploy <-inner_join(deployments,cameras,by="Camera.ID")
marin_data <- inner_join(images,cam_deploy,by="Deployment.ID")

# Write out the fully combined dataset.
write.csv(marin_data,file="marin_data.csv")
# Remove all images that we know don't have an animal
marin_data_animals <- marin_data[which(marin_data$Photo.Type == "Animal"),]# | marin_data$Photo.Type == "Unknown" 
##  | marin_data$Photo.Type == "Unidentifiable"),]
# Write out the full dataset with only animal images/records
write.csv(marin_data_animals,file="marin_data_animals.csv")
############################ 
