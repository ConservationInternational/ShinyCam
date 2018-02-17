###############################
# animal_count.R
# Calculate the animal counts per project per deployment per month.
# It is suggested to run this for the entire dataset.
# EHF ADD MORE DESCRIPTION
# Output:
# 1. File containing the groupings
# 2. File containing the final counts
###############################
# Clear the environment and load libraries
rm(list = ls())
library(lubridate)
library(dplyr)
library(plyr)

# Set the path and workspace to to main ShinyCam directory (i.e. the one that has README.md file, ShinyApps directory,etc)
shinycam_path <- "/Users/efegraus/work/DataKind/ShinyCam_new/ShinyCam"
prj_name<- "KPHK_Guntur_Papandayan" # No spaces in names
setwd(shinycam_path)
source("rscripts/RShiny_functions.R")
old <- Sys.time()
###############################
#Load Data
df_name <- "indonesia_joined_data.csv" # This should be a file name that is in the correct format. See README process for explanation
ct_data <-read.csv(paste("ShinyApps/LeafletApp/data/raw_dataprep/",df_name, sep=""))
# Remove all images that we know don't have an animal
data_animals <- ct_data[which(ct_data$Photo.Type == "Animal"),]# | marin_data$Photo.Type == "Unknown" 

# Shrink data frame
data_new <- select(data_animals,Project.ID,Camera.Deployment.Begin.Date,Camera.Deployment.End.Date,Latitude.Resolution,Longitude.Resolution,Photo.Type,Image.ID,Genus.Species,Count,Deployment.Location.ID,Event,Date_Time.Captured)
# Create the groups 
data_order <-f.order.data(data_new)
data_order$Date_Time.Captured <-ymd_hms(data_order$Date_Time.Captured)
event_runs<- c(120,3600,86400) # 3 million record dataset run times are: 120 ~4hrs, 3600 (30mins) ~47 mins, 1 day 86400 ~42 mins
for (m in 1:length(event_runs)) {
  events <- f.separate.events(data_order,event_runs[m]) 
  # EHF: Think next 4 lines below can be deleted. Test with Marin
  # Drop image.id column
  # events_no_image <- select(events,-Image.ID)
  # Remove Dups (Image ID oftern makes unique but removed above)
  # events_distinct <- distinct(marin_events_no_image)
  # Unique groups
  events_distinct <- events
  unique_grps <- unique(events_distinct$grp)
  # unique_grps <- unique(marin_events_distinct$grp)
  
  # output dataframe with same str as input
  output_df <- events_distinct[0,]
  # This loop may take awhile...6 hours or so for some Marin projects.
  for (i in 1:length(unique(unique_grps))) {
    temp <- events_distinct[which(events_distinct$grp == unique_grps[i]),]
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
  # Tag the datframe with the interval
  output_df$event_interval <- event_runs[m]
  output_df$prj_name <- prj_name
  new <- Sys.time() - old 
  print(paste("Time event group",event_runs[m],"secs took",new,"to run and is complete"))
  # Print out the events file and then the final_counts
  output_path <-"ShinyApps/LeafletApp/data/raw_dataprep/"
  write.csv(output_df,file=paste(output_path,"data_event_",event_runs[m],"secs_",prj_name,".csv", sep=""))
  #Final Count
  output_df$Year <-year(output_df$Date_Time.Captured)
  output_df$Month <- month(output_df$Date_Time.Captured)
  # Calculate  summary information
  final_count <- ddply(output_df,.(prj_name,Project.ID,Latitude.Resolution,Longitude.Resolution,
                                   Event,Deployment.Location.ID,Month,Year,Genus.Species),summarize,total=sum(Count))
  write.csv(final_count,file=paste(output_path,"final_count_",event_runs[m],"secs_",prj_name,".csv", sep=""))
  
}  
