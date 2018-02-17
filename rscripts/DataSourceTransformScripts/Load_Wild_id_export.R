#######
# Eric Fegraus - 12/17/2017
# This script will get data exported from Wild.ID ready for analysis. Wild.ID provides several output formats.
# This format will focus initially on the standardized .csv output (from within Wild.ID navigation menu select:
# "Export->Export Data and Images->CSV Format -> Without Images) but also work with the Wildlife Insights export
# format. 

rm(list = ls())
# Load libraries
library(dplyr)
library(lubridate)
library(plyr)
library(data.table)
library(measurements)
#Set working dir
setwd("/Users/efegraus/work/DataKind/Indonesia/RawData/csv_format/")
############################ 
# This file will work with exports from Wild.ID, specifically the .csv format and the Wildlife Insights format. 
# Load CSV exported file types here
# Be sure to setwd() above to the directory at the highest level and that there are no other miscellaneous csv files. 
ct_data <- ldply( .data = list.files(pattern="*.csv",recursive = TRUE),
                  .fun = read.csv,
                  header = TRUE
)
ct_data <- subset(ct_data,!is.na(Photo.Type)) # just in case empty rows come in from your datafiles
#####
# Load the Wildlife Insights exported file types here. You only need to use one of these depending on what you exported from Wild.ID
# Code to be complete here
#
#
#
#
#

############################
# Custom data clean up code here
# Example cleaning up some date problems
ct_data$Photo.Date <- gsub("2006","2016",ct_data$Photo.Date)
ct_data$Camera.Start.Date <- gsub("2006","2016",ct_data$Camera.Start.Date)

# Spatial Data Fix
ct_spatial <- read.csv("../raw_original_2016/joined_data.csv")
spatial_data <- distinct(ct_spatial,Depolyment.Location.ID,Longitude.Resolution,Latitude.Resolution)
full_ct_locations <- distinct(ct_data,Camera.Trap.Name)
all_cts <- left_join(full_ct_locations,spatial_data,by=c("Camera.Trap.Name" = "Depolyment.Location.ID"))
write.csv(all_cts,"all_camera_trap_locations.csv")
new_spatial <- read.csv("Modified_spatial_from_anton.csv")
new_spatial$Selatan_S <-gsub("07 ","7 ",new_spatial$Selatan_S)
new_spatial$Selatan_S <-gsub("7 ","-7 ",new_spatial$Selatan_S)
new_spatial$Selatan_S <-gsub("'"," ",new_spatial$Selatan_S)
new_spatial$Selatan_S <-gsub("\"","",new_spatial$Selatan_S)
new_spatial$Timur._E <-gsub("'"," ",new_spatial$Timur._E)
new_spatial$Timur._E <-gsub("\"","",new_spatial$Timur._E)

new_lat <- conv_unit(new_spatial$Selatan_S,"deg_min_sec","dec_deg")
new_long <- conv_unit(new_spatial$Timur._E,"deg_min_sec","dec_deg")
new_spatial$new_lat <- new_lat
new_spatial$new_long <- new_long

all_cts_final <- left_join(all_cts,new_spatial,by=c("Camera.Trap.Name"= "new_name"))
all_cts_final$Longitude.Resolution[38:90]<- all_cts_final$new_long[38:90]
all_cts_final$Latitude.Resolution[38:90]<- all_cts_final$new_lat[38:90]
final_spatial <- select(all_cts_final,Camera.Trap.Name,Latitude.Resolution,Longitude.Resolution)
full_data <- left_join(ct_data,final_spatial,by=c("Camera.Trap.Name"= "Camera.Trap.Name"))
full_data <- select(full_data,-Latitude,-Longitude,-Longitude.Resolution.x,-Latitude.Resolution.x,-X)
#end custom code

############################
# Here you should have one dataframe that has all the data needed. We need to make sure the column names are named
# appropriately for ShinyCam. 
setnames(full_data,"Project.Name","Project.ID")
full_data$Deployment.ID <- paste(full_data$Camera.Start.Date,full_data$Camera.Trap.Name,sep="-")
setnames(full_data,"Raw.Name","Image.ID")
full_data$Location <- "TBC"
setnames(full_data,"Person.Identifying.the.Photo","Photo.Type.Identified.by")
full_data$Genus.Species <- paste(full_data$Genus,full_data$Species,sep=" ")
full_data$IUCN.Identification.Numbers <- "TBC"
full_data$Age <- "TBC"
full_data$Sex <- "TBC"
full_data$Individual.ID <- "TBC"
full_data$Animal.recognizable <- "TBC"
full_data$Date_Time.Captured <- paste(full_data$Photo.Date,full_data$Photo.time)
setnames(full_data,"Sampling.Event","Event")
setnames(full_data,"Number.of.Animals","Count")
#setnames(full_data,"Photo.Notes","individual.Animal.notes")
setnames(full_data,"Camera.Trap.Name","Deployment.Location.ID")
setnames(full_data,"Longitude.Resolution.y","Longitude.Resolution")
setnames(full_data,"Latitude.Resolution.y","Latitude.Resolution")
setnames(full_data,"Camera.Start.Date","Camera.Deployment.Begin.Date")
setnames(full_data,"Camera.End.Date","Camera.Deployment.End.Date")
full_data$Bait.Type <- "None"
full_data$Bail.Description <- NA
full_data$Feature.Type <- "TBC"
full_data$Feature.Type.methodolgy <- "TBC"
setnames(full_data,"Camera.Serial.Number","Camera.ID")
full_data$Quiet.Period.Setting <- "TBC"
full_data$Restriction.on.access <- "TBC"
full_data$Camera.Failure.Details <- "TBC"
full_data$Project.ID.y <- "TBC"
setnames(full_data,"Camera.Manufacturer","Make")
setnames(full_data,"Camera.Model","Model")
full_data$Year.Purchased <- "9999"

joined_data <- full_data
############################
# Write out the fully combined dataset.
# Create a path for output
my_path <- "/Users/efegraus/work/DataKind/ShinyCam_new/ShinyCam/ShinyApps/LeafletApp/data/raw_dataprep/"
write.csv(joined_data,file=paste(my_path,"indonesia_joined_data.csv", sep=""))



