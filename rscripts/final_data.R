rm(list = ls())
# Load libraries
library(dplyr)
library(lubridate)
library(plyr)
library(tidyr)
setwd("/Users/efegraus/work/DataKind/ShinyCam")
# Select the dataset for final rates of detection calculation
sub_data <-c("ChedaJewel") # Set variable to label future dataframes and .csv's
# Need to rename MMWD datassets and GG datasets for below
final_count <- read.csv(paste("data/final_count_",sub_data,".csv", sep=""))

# Remove the empty species
final_count2 <- filter(final_count,Genus.Species != "")
final_count3 <- arrange(final_count2,Project.ID,Deployment.Location.ID,Year,Month,Genus.Species)
## Write out if you want to check
#write.csv(final_count3,file="data/final_count3.csv")

final_trap <- read.csv(paste("data/",sub_data,"_trap_days.csv",sep=""))
final_trap2 <- arrange(final_trap,Project.ID,Deployment.Location.ID,Camera.Deployment.Begin.Date,Year,Month)
## Write out the sorted trap days if needed
#write.csv(final_trap2,file="data/final_trap2.csv") # Before the summarize to get total days per month where needed

## Calculate the Total Trap days
final_trap3 <- ddply(final_trap2,.(Project.ID,Deployment.Location.ID,Year,Month),summarize,TotalTrapDays=sum(TrapDays))
## Get the problem trap months/years and send to lizzy for further review
#write.csv(final_trap3,file="data/final_trap3.csv")
problem_traps <- final_trap3[which(final_trap3$TotalTrapDays >33),]
final_trap4 <- filter(final_trap3, TotalTrapDays < 33)
## Write them out the final trap days + the problem traps
#write.csv(final_trap4,file="data/final_trap4.csv")
## Write out problem deployments
#write.csv(problem_traps,"data/problem_deployments.csv")

# Combine the datasets
combined <- left_join(final_count2,final_trap4, by = c("Project.ID" = "Project.ID", "Deployment.Location.ID" = "Deployment.Location.ID","Year"="Year","Month"="Month"))
final_combined <- arrange(combined,Project.ID,Deployment.Location.ID,Year,Month)
###
# Shorten the dataset by problematic dates
final_combined$year_month <- paste(final_combined$Year,final_combined$Month,sep="-")
final_combined <- filter(final_combined,year_month != "2014-1")
final_combined <- filter(final_combined,year_month != "2014-2")
final_combined <- filter(final_combined,year_month != "2014-9")
final_combined <- filter(final_combined,year_month != "2016-3")
final_combined <- filter(final_combined,year_month != "2016-4")
final_combined <- filter(final_combined,year_month != "2016-5")
final_combined <- filter(final_combined,year_month != "2016-6")
unique_dates <- sort(unique(paste(final_combined$year_month)))
final_combined <- select(final_combined,-year_month)
# Calculate Rates of detection
final_combined$Rate.Of.Detection <- final_combined$total/(final_combined$TotalTrapDays/100)
# Export the final file if wanted
#write.csv(final_combined,file="data/combined.csv")

# Create the final Rate of Detection Data File
final_rates_files <- final_combined
final_rates_files$Rate <- "Marin" 
final_rates_files$Sampling.Type <- "Monthly"

final_rates_files2 <- final_rates_files[,c(13,2,6,3,4,14,7,8,9,12)]
# Change month to Sampling Period
colnames(final_rates_files2)[colnames(final_rates_files2)=="Month"] <- "Sampling.Period"
# Change rate to Data Source
colnames(final_rates_files2)[colnames(final_rates_files2)=="Rate"] <- "Data.Source"

# Split Genus Species
Marin_rates_of_detection <- separate(final_rates_files2,Genus.Species,into=c("Genus","Species"),sep=" ")
#

write.csv(Marin_rates_of_detection,file=paste("data/",sub_data,"_rate_of_detection_MARIN.csv",sep=""))

##
# Load them all together
mmwd_rates <- read.csv("data/MMWD_rate_of_detection_MARIN.csv")
gg_rates <- read.csv("data/GG_rate_of_detection_MARIN.csv")
cj_rates <- read.csv("data/ChedaJewel_rate_of_detection_MARIN.csv")
total_rates <- rbind(gg_rates,mmwd_rates,cj_rates)
# Write out final dataset
write.csv(total_rates,file="data/rate_of_detection_MARIN_total.csv")

##########
#####
#which(final_trap2$Deployment.Location.ID == "")
##not_in_final_trap2<- anti_join(final_count2,final_trap2, by = c("Project.ID","Deployment.Location.ID","Year","Month"))
#final_trap2[which(final_trap2$Deployment.Location.ID == "HHH71"),]
#final_count3[which(final_count3$Deployment.Location.ID == "HHH71"),]
##not_in_final_count2<- anti_join(final_trap2,final_count2, by = c("Project.ID","Deployment.Location.ID","Year","Month"))
##combined <- anti_join(final_count2,final_trap2, by = c("Project.ID","Deployment.Location.ID","Year","Month"))


