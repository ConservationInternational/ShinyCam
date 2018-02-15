####
# cam_trap_nights.R
# Calculate the number of camera trap nights per camera per deployment
# Needs more description

rm(list = ls())
# Load libraries
library(dplyr)
library(lubridate)
library(plyr)
library(zoo)

# Set the path and workspace to to main ShinyCam directory (i.e. the one that has README.md file, ShinyApps directory,etc)
shinycam_path <- "/Users/efegraus/work/DataKind/ShinyCam_new/ShinyCam"
prj_name<- "KPHK_Guntur_Papandayan" # No spaces in names
setwd(shinycam_path)
source("rscripts/RShiny_functions.R")
###############################
#Load Data
df_name <- "indonesia_joined_data.csv" # This should be a file name that is in the correct format. See README process for explanation
ct_data <-read.csv(paste("ShinyApps/LeafletApp/data/raw_dataprep/",df_name, sep=""))
ct_data_new <- select(ct_data,Project.ID,Deployment.Location.ID,Camera.Deployment.Begin.Date,Camera.Deployment.End.Date,Date_Time.Captured)
old <- Sys.time() 

# Ensure date columns are correct
ct_data_new$Date_Time.Captured <- as.POSIXct(strptime(ct_data_new$Date_Time.Captured,"%Y-%m-%d %H:%M:%S"))
no_dates <- ct_data_new[which(is.na(ct_data_new$Date_Time.Captured)==TRUE),]
# if no_dates has any records this means you need to do some data cleaning.
# Pop-off these records as we can't use them
ct_data_new <- filter(ct_data_new,!is.na(Date_Time.Captured))

# Creat date data types
ct_data_new$Camera.Deployment.Begin.Date <- as.POSIXct(ct_data_new$Camera.Deployment.Begin.Date)
ct_data_new$Camera.Deployment.End.Date <- as.POSIXct(ct_data_new$Camera.Deployment.End.Date)

# Get the unique list of deploymnets
unique_deployments <- unique(ct_data_new$Deployment.Location.ID)
# Setup the output dataframe for trap nights/days
output_df <- ct_data_new[NA,]
output_df <- select(output_df,Project.ID,Deployment.Location.ID,Camera.Deployment.Begin.Date,Camera.Deployment.End.Date)
output_df <- output_df[(1),]
output_df$Year <-NA
output_df$Month <-NA
output_df$TrapDays <- NA
output_df$TotalCheck <- NA
output_df$LastDay <- as.POSIXct(NA)
prob_deployments <-NA
# Begin the loops. Go by deployment location ID first
for (i in 1:length(unique_deployments)) {
  temp <- filter(ct_data_new,Deployment.Location.ID == unique_deployments[i])
  # Get all the begin Dates for each deployment
  unique_begin <- unique(temp$Camera.Deployment.Begin.Date)
  # After getting all data for a deployment locations go through each deployment at that location
  for (j in 1:length(unique_begin)) {
    # Check for date problems
    temp2 <- filter(temp,Camera.Deployment.Begin.Date == unique_begin[j])
    # get the start month
    start_eom <- eom(unique_begin[j])
    # Get the date of the last image
    max_date <- as.POSIXct(range(temp2$Date_Time.Captured))
    #end_date <- as.POSIXct(max_date)
    tz(max_date) <- "America/Los_Angeles"
    # end_eom has both the last day of the first month and the last day of the last month 
    # from the images
    end_eom<- eom(max_date)
    
    # PUT THE ENTIRE THING WITHIN an IF
    
    # Need to identify the image dates (max and min) that don't fit within the deployment begin and End
    # dates
    interval1 <- interval(unique_begin[j],end_eom[2]+86399) # add 23:59:59 time to the last day
    if ((max_date[1] %within% interval1) & (max_date[2] %within% interval1)) {
      print(paste("ok",unique_deployments[i],unique_begin[j],max_date[1],i,j,sep=","))
    
    
    # Begin to build the dataframe
    #output_temp <- output_temp[(1),]
    month_diff_start <- round((as.yearmon(strptime(max_date[2], format = "%Y-%m-%d"))-
                     as.yearmon(strptime(start_eom, format = "%Y-%m-%d")))*12)
    # Case 1: Start date and last image occur in the same month
    #if (start_eom == end_eom[2]) { # change to use month_diff is month_diff =0 do difftime
     # break
      #dtime<- as.numeric(difftime(start_eom,unique_begin[j],units = c("days")))
      #output_df$Deployment.Location.ID <- unique_deployments[i]
      #output_df$trap_days <-round(dtime)
    #} else if (month_dff >=1) {
      # cycle through each month and count the trap days in each month
      output_temp <- output_df[NA,]
      total_months <- month_diff_start +1
      output_temp <- output_temp[(1:total_months),]
      for (k in 1:total_months) { 
        
          # Complete the dataframe
          output_temp$Project.ID[k] <- unique(temp2$Project.ID)
          output_temp$Deployment.Location.ID[k] <- unique(temp2$Deployment.Location.ID)
          output_temp$Camera.Deployment.Begin.Date[k] <- unique(temp2$Camera.Deployment.Begin.Date)
          output_temp$Camera.Deployment.End.Date[k] <- unique(temp2$Camera.Deployment.End.Date)
          output_temp$Year[k] <- year(start_eom %m+% months(k) %m-% months(1))
          output_temp$Month[k] <- month(start_eom %m+% months(k) %m-% months(1))
          if (k == 1 & k != total_months) { # If there is more than one month get the difference between the end of the month and the begin date.  
            output_temp$TrapDays[k] <- as.numeric(difftime(start_eom,unique_begin[j],units = c("days"))+1) # Get the days in the first month: need to add 1 to include first day
          } else if (k>=2 & k < total_months) { # if there is more than 1 month start AND not the last month, get the number of days in that month
            output_temp$TrapDays[k] <- day(start_eom %m+% months(k) %m-% months(1)) # Get the total # of days in this month. All days in month included
          } else if (k >1 & k == total_months) { # if there is more than one month and k is now the last month
            output_temp$TrapDays[k] <- day(max_date[2]) # Get the day of the month and assign it to trap days that month
          } else if ( k==1 & k==total_months) { # Get the number of days if the camera only ran in one month (i.e., started and stopped in same month)
            output_temp$TrapDays[k] <- as.numeric(difftime(max_date[2],unique_begin[j],units = c("days"))+1) 
          } else { 
            print(paste("ERROR2222"))
            break
          }
          
          # Get the true difference in days between deployment start and the last image
          # to use as a check later on;
          output_temp$TotalCheck[k] <- as.numeric(difftime(max_date[2],unique_begin[j],units = c("days")))
          output_temp$LastDay[k] <- as.POSIXct(max_date[2])
          
      }
      output_df <-rbind(output_df,output_temp)
          
    } else {
      print(paste("ERROR4",unique_deployments[i],interval1,max_date[1],max_date[2],i,j,sep=","))
        prob_deployments <- rbind(paste(unique_deployments[i],unique_begin[j],max_date[1],max_date[2],sep="|"),prob_deployments)
      break
    }  
    
  } 
}  
new <- Sys.time() - old 
# Print out the events file and then the final_counts
output_path <-"ShinyApps/LeafletApp/data/raw_dataprep/"
write.csv(output_df,file=paste(output_path,"trap_days_",prj_name,".csv", sep=""))
if (length(prob_deployments) > 1) {
  print("There are some problems with the deployments that snuck past the filters check out the file PRJNAME_problem_deployments.csv")
  prob_file_name <- paste("data/raw_dataprep/",prj_name,"_problem_deployments.csv",sep="")
  write.csv(prob_deployments,prob_file_name)
}

######################




