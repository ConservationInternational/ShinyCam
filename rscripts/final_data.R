###############################
# Rates_calc.R
# This will calculate Rates of dection for input into ShinyCam
# Utilizes information from both animal_count.R and cam_trap_nights.R
###############################
rm(list = ls())
# Load libraries
library(dplyr)
library(lubridate)
library(plyr)
library(tidyr)
# Set the path and workspace to to main ShinyCam directory (i.e. the one that has README.md file, ShinyApps directory,etc)
shinycam_path <- "/Users/efegraus/Documents/GitHub/ShinyCam"
prj_name<- "Papandayan" # No spaces in names and needs to be same as used in other files2419

###############################
# Load the final_count data files
output_path <-"ShinyApps/LeafletApp/data/processed/"
setwd(paste(shinycam_path,"/",output_path,sep=""))
event_runs<- c(120,3600,86400) 
trap_name <- paste("trap_days_",prj_name,".csv",sep="")
# NEED TO LOOP THROUGH EACH OF THE FINAL COUNT DATASETS

for (i in 1:length(event_runs)) {
  final_count_name <- paste("final_count_",event_runs[i],"secs_",prj_name,".csv",sep="")
  
  # ct_data <- ldply( .data = list.files(pattern=pat,recursive = TRUE),
  #                   .fun = read.csv,
  #                   header = TRUE
  # )
  # 
  # ct_data <- ldply( .data = list.files(pattern="^data_event_86400secs_KPHK",recursive = TRUE),
  #                   .fun = read.csv,
  #                   header = TRUE
  # )
  
  
  # Select the dataset for final rates of detection calculation
  final_count <- read.csv(final_count_name)
  
  # Remove the empty species - why?
  #final_count2 <- filter(final_count,Genus.Species != "")
  final_count2 <- arrange(final_count,Project.ID,Deployment.Location.ID,Year,Month,Genus.Species)
  ## Write out if you want to check
  #write.csv(final_count3,file="data/final_count3.csv")
  
  final_trap <- read.csv(trap_name)
  final_trap2 <- arrange(final_trap,Project.ID,Deployment.Location.ID,Camera.Deployment.Begin.Date,Year,Month)
  ## Write out the sorted trap days if needed
  #write.csv(final_trap2,file="data/final_trap2.csv") # Before the summarize to get total days per month where needed
  
  ## Calculate the Total Trap days by month
  final_trap3 <- ddply(final_trap2,.(Project.ID,Deployment.Location.ID,Year,Month),summarize,TotalTrapDays=sum(TrapDays))
  ## Get the problem trap months/years and send to lizzy for further review
  #write.csv(final_trap3,file="data/final_trap3.csv")
  problem_traps <- final_trap3[which(final_trap3$TotalTrapDays >=33),]
  final_trap4 <- filter(final_trap3, TotalTrapDays < 33)
  ## Write them out the final trap days + the problem traps
  #write.csv(final_trap4,file="data/final_trap4.csv")
  ## Write out problem deployments
  #write.csv(problem_traps,"data/problem_deployments.csv")
  
  # Combine the datasets
  combined <- left_join(final_count2,final_trap4, by = c("Project.ID" = "Project.ID", "Deployment.Location.ID" = "Deployment.Location.ID","Year"="Year","Month"="Month"))
  # Remove all records with total animals = NA --> why is this happening? and total trap days = NA --> from image dates not fallin in the appropriate being and end deployment dates
  filter_combined <- na.omit(combined)
  final_combined <- arrange(filter_combined,Project.ID,Deployment.Location.ID,Year,Month)
  ###
  #final_combined <- select(final_combined,-year_month)
  # Calculate Rates of detection
  final_combined$Rate.Of.Detection <- final_combined$total/(final_combined$TotalTrapDays/100)
  # Export the final file if wanted
  #write.csv(final_combined,file="data/combined.csv")
  
  # Create the final Rate of Detection Data File
  final_rates_files <- final_combined
  final_rates_files$Sampling.Type <- "Monthly"
  # Change month to Sampling Period
  colnames(final_rates_files)[colnames(final_rates_files)=="Month"] <- "Sampling.Period"
  
  #final_rates_files2 <- final_rates_files[,c(13,2,6,3,4,14,7,8,9,12)]
  # Change rate to Data Source
  #colnames(final_rates_files2)[colnames(final_rates_files2)=="Rate"] <- "Data.Source"
  # Split Genus Species
  rates_of_detection <- separate(final_rates_files,Genus.Species,into=c("Genus","Species"),sep="\\s", fill="right",extra="merge")
  rates_of_detection$Rate.Of.Detection <- round(rates_of_detection$Rate.Of.Detection,digits=2)
  
  write.csv(rates_of_detection,file=paste(prj_name,"_rate_of_detection_",event_runs[i],".csv",sep=""))
}
##
# Load them all together
#mmwd_rates <- read.csv("data/MMWD_rate_of_detection_MARIN.csv")
#gg_rates <- read.csv("data/GG_rate_of_detection_MARIN.csv")
#cj_rates <- read.csv("data/ChedaJewel_rate_of_detection_MARIN.csv")
#total_rates <- rbind(gg_rates,mmwd_rates,cj_rates)
# Write out final dataset
#write.csv(total_rates,file="data/rate_of_detection_MARIN_total.csv")

##########
#####
#which(final_trap2$Deployment.Location.ID == "")
##not_in_final_trap2<- anti_join(final_count2,final_trap2, by = c("Project.ID","Deployment.Location.ID","Year","Month"))
#final_trap2[which(final_trap2$Deployment.Location.ID == "HHH71"),]
#final_count3[which(final_count3$Deployment.Location.ID == "HHH71"),]
##not_in_final_count2<- anti_join(final_trap2,final_count2, by = c("Project.ID","Deployment.Location.ID","Year","Month"))
##combined <- anti_join(final_count2,final_trap2, by = c("Project.ID","Deployment.Location.ID","Year","Month"))
#final_trap4$Month<-as.factor(final_trap4$Month)
#final_trap4$Year<-as.factor(final_trap4$Year)
#final_count2$Month <- as.factor(final_count2$Month)
#final_count2$Year <- as.factor(final_count2$Year)
#final_count_unique <- unique(paste(final_count$Project.ID,final_count$Deployment.Location.ID,final_count$Year,final_count$Month)) # 2633 deployments
#trap_unique <- unique(paste(final_trap$Project.ID,final_trap$Deployment.Location.ID,final_trap$Year,final_trap$Month))
#count_unique<- unique(paste(final_count2$Project.ID,final_count2$Deployment.Location.ID,final_count2$Year,final_count2$Month)) 

