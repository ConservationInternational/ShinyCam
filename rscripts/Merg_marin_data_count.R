rm(list = ls())
# Load libraries
library(dplyr)
library(lubridate)
library(plyr)

#Set working dir
setwd("/Users/efegraus/work/DataKind/ShinyCam")
############################ 

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

#Bind the datasetst together
cameras <-rbind(cj_cameras, sp_cameras,wd_cameras,gg_cameras,wh_cameras)
deployments <-rbind(cj_deployments, sp_deployments,wd_deployments,gg_deployments,wh_deployments)
images <-rbind(cj_images, sp_images,wd_images,gg_images,wh_images)

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
###############################

marin_data_animals <-read.csv("data/marin_data_animals.csv")
# Shrink data frame
marin_data_new <- select(marin_data_animals,Project.ID,Camera.Deployment.Begin.Date,Camera.Deployment.End.Date,Latitude.Resolution,Longitude.Resolution,Photo.Type,Image.ID,Genus.Species,Count,Deployment.Location.ID,Event,Date_Time.Captured)
# Subset by project if needed
sub_data <-c("ChedaJewel") # Set variable to label future dataframes and .csv's
marin_data_subset <- filter(marin_data_new,Project.ID == sub_data)
################################
# CALCULATE FINAL COUNTS PER MONTH PER YEAR PER PROJECT PER DEPLOYMENT
################################
# Creat the groups
marin_data_order <-f.order.data(marin_data_subset)
marin_data_order$Date_Time.Captured <-ymd_hms(marin_data_order$Date_Time.Captured)
marin_events <- f.separate.events(marin_data_order,.02)
# Deal with records that have more than 15 or images per group.
summary(marin_events$Date_Time.Captured)
summary(marin_events$grp)
problems <-as.data.frame(table(marin_events$grp))

# Dates with more than 15 images per group
actual_problems <- problems[which(problems$Freq >9),]
actual_problems$Check <- NA
for (i in 1:length(actual_problems$Freq)) {
  check_data <- marin_events[which(marin_events$grp== actual_problems$Var1[i]),]
   if (length(unique(check_data$Deployment.Location.ID)) > 1) {
     print(paste(actual_problems$Var1[i],"More than one Dep LocationID"))
    }
   if (length(unique(check_data$Genus.Species)) > 1) {
     print(paste(actual_problems$Var1[i],"More than one Genus Species"))
     # Build a list of problematic grps to remove based on duplicate species
     actual_problems$Check[i] <- 1
     #remove_recs[i]<-temp
   }
  if (length(unique(check_data$Event)) > 1) {
    print(paste(actual_problems$Var1[i],"More than one Event"))
  }
}

# Drop image.id column
marin_events_no_image <- select(marin_events,-Image.ID)
# Remove Dups (Image ID oftern makes unique but removed above)
marin_events_distinct <- distinct(marin_events_no_image)
# Unique groups
unique_grps <- unique(marin_events_distinct$grp)
# output dataframe with same str as input
output_df <- marin_events_distinct[0,]
# This loop may take awhile...6 hours or so for some Marin projects.
for (i in 1:length(unique(unique_grps))) {
  temp <- marin_events_distinct[which(marin_events_distinct$grp == unique_grps[i]),]
  # Case 1: Only 1 record
  if (nrow(temp) == 1) {
    output_df <- rbind(output_df,temp)
    # More than 1 record but with same species and count
  } else if (length(unique(paste(temp$Genus.Species,temp$Count))) == 1) {
    output_df <- rbind(output_df,temp[1,]) # just get the first one as they are the same
    # More than 1 record but with same species and different count
  } else if (length(unique(paste(temp$Genus.Species,temp$Count))) > 1) {
    # Could have more than one species
    num_recs_want <- length(unique(paste(temp$Genus.Species,temp$Count)))
    temp2 <- distinct(temp,Genus.Species,Count,.keep_all = TRUE)
    if (nrow(temp2) != num_recs_want) {
      print(paste("ERROR"))
      break
    }
    output_df <-rbind(output_df,temp2)
  }
  
}
# Write out output_df if needed
#write.csv(output_df,file="output_df")


# Add in year_month
output_df$Year <-year(output_df$Date_Time.Captured)
output_df$Month <- month(output_df$Date_Time.Captured)
# Calculate  summary information
final_count <- ddply(output_df,.(Project.ID,Latitude.Resolution,Longitude.Resolution,
                          Event,Deployment.Location.ID,Month,Year,Genus.Species),summarize,total=sum(Count))
out_file_name <- paste("data/final_count_",sub_data,".csv",sep="")
write.csv(final_count,file=out_file_name)


