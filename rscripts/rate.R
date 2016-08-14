## Eva Tang
## SFdatakind Data Dive Aug 2016
## This script is intent to calculate the number of tray days

library(dplyr)
library(zoo)
library(lubridate)

## Load team data
team <- read.csv("C://Users//tangkk//Documents//Big Data//datakind//data dive 201608//Terrestrial_Vertebrate_Cols_Edited.csv", stringsAsFactors=FALSE)

head(team)

# Data cleasning
print(paste("Original data size -> ", dim(team)[1], sep=""))

#team <- team[team$Photo.Type != 'Sampling Dates', ]
#print(paste("1. Remove Photo.Type == Sampling Dates -> ", dim(team)[1], sep=""))
#team <- team[team$Count > 0, ]
#print(paste("2. Remove Count == 0-> ", dim(team)[1], sep=""))
#team <- team[!is.na(team$Species), ]
#print(paste("3. Remove Species is NA ", dim(team)[1], sep=""))

print(paste("Number of distinct Deployment.Location.ID -> ", length(unique(team$Deployment.Location.ID)), sep=""))
print(paste("Total genus -> ", length(unique(team$Genus)), sep=""))
print(paste("Total species -> ", length(unique(team$Species)), sep=""))

## Number of sites per project
group_by(team, Project.ID) %>% summarise(location_cnt=length(unique(Deployment.Location.ID)))

## Extract camera deployment information
deployment <- unique(team[, c('Project.ID', 'Sampling.Period', 'Deployment.Location.ID', 'Camera.Deployment.Begin.Date', 'Camera.Deployment.End.Date', 'Camera.Serial.Number')])
head(deployment)

## calculate trap days 
deployment$trap_days <- abs(as.numeric(difftime(deployment$Camera.Deployment.End.Date, deployment$Camera.Deployment.Begin.Date, units = c("days"))))
deployment$trap_days <- floor(deployment$trap_days)

## trap days per deployment location and sampling period for TEAM
result <- group_by(deployment, Project.ID, Deployment.Location.ID, Sampling.Period) %>% summarise(trap_days=sum(trap_days), begin=min(Camera.Deployment.Begin.Date), end=max(Camera.Deployment.End.Date), num_deploy=length(unique(Camera.Deployment.Begin.Date))) %>% select(Deployment.Location.ID, Sampling.Period, trap_days)
print(result)

write.csv(result, "C://Users//tangkk//Documents//Big Data//datakind//data dive 201608//team_trap_days.csv", row.names=FALSE)




## For Marin the trap day aggregation will be at YYYY and MM level not Sampling Period

marin <- read.csv("C://Users//tangkk//Documents//Big Data//datakind//data dive 201608//Marin_Merged.csv", stringsAsFactors=FALSE)

## Extract camera deployment information
deployment <- unique(marin[, c('Project.ID', 'Deployment.ID', 'Camera.Deployment.Begin.Date', 'Camera.Deployment.End.Date', 'Camera.ID')])
head(deployment)

deployment$trap_days <- abs(as.numeric(difftime(deployment$Camera.Deployment.End.Date, deployment$Camera.Deployment.Begin.Date, units = c("days")))+1)
deployment$trap_days <- floor(deployment$trap_days)

## return the End of Month given a date
eom <- function(date) {
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2 
  year <- date.lt$year
  year <- year + as.integer(mon==13) # if month was December add a year
  mon[mon==13] <- 1
  iso = ISOdate(1900+year, mon, 1, hour=0)
  result = as.POSIXct(iso) - 86400 # subtract one day
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}

deployment$Camera.Deployment.Begin.Date <- as.POSIXct(deployment$Camera.Deployment.Begin.Date)
deployment$Camera.Deployment.End.Date <- as.POSIXct(deployment$Camera.Deployment.End.Date)

deployment$span_month <- 0
deployment$start_month <- 0
deployment$end_month <- 0


## Calculate month span for each camera
for (i in 1:dim(deployment)[1]) {
  if (as.Date(deployment[i, 'Camera.Deployment.Begin.Date']) < as.Date(deployment[i, 'Camera.Deployment.End.Date'])) {
    deployment[i, 'start_month'] <- as.numeric(format(deployment[i, 'Camera.Deployment.Begin.Date'],'%m'))
    deployment[i, 'end_month'] <- as.numeric(format(deployment[i, 'Camera.Deployment.End.Date'],'%m'))
  } else {
    deployment[i, 'span_month'] <- -1
  }
}

deployment$span_month <- ifelse(deployment$end_month < deployment$start_month,  deployment$end_month + 12 - deployment$start_month, deployment$end_month - deployment$start_month) + 1
table(deployment$span_month, useNA="ifany")

## Expand deployment from camera duration across month to one row per month

result <- data.frame(Project.ID=character(), Deployment.ID=character(), Camera.Deployment.Begin.Date=character(), Camera.Deployment.End.Date=character(), 
                     begin=character(), end=character(), trap_days=numeric(), stringsAsFactors=FALSE)

for (i in 1:dim(deployment)[1]) {
  for (j in 1:deployment[i, 'span_month']) {
    if (j == 1) {
      result[nrow(result)+1, ] <- c(deployment[i, 'Project.ID'], deployment[i, 'Deployment.ID'], as.character(deployment[i, 'Camera.Deployment.Begin.Date']), as.character(deployment[i, 'Camera.Deployment.End.Date']), 
                                   as.character(deployment[i, 'Camera.Deployment.Begin.Date']), as.character(eom(deployment[i, 'Camera.Deployment.Begin.Date'])),  
                                   as.numeric(difftime(eom(deployment[i, 'Camera.Deployment.Begin.Date']), as.Date(deployment[i, 'Camera.Deployment.Begin.Date']), units="days"))+1)
      prev <- eom(deployment[i, 'Camera.Deployment.Begin.Date'])
      
## Not needed since Marin camera start and end date don't have HH:MM:SS info
#      begin_hh <- ifelse(is.na(as.numeric(substring(deployment[i, 'Camera.Deployment.Begin.Date'], 12,13))), 0, as.numeric(substring(deployment[i, 'Camera.Deployment.Begin.Date'], 12,13)))
#      end_hh <- ifelse(is.na(as.numeric(substring(deployment[i, 'Camera.Deployment.End.Date'], 12,13))), 0, as.numeric(substring(deployment[i, 'Camera.Deployment.End.Date'], 12,13)))
#      begin_mm <- ifelse(is.na(as.numeric(substring(deployment[i, 'Camera.Deployment.Begin.Date'], 15,16))), 0, as.numeric(substring(deployment[i, 'Camera.Deployment.Begin.Date'], 15,16)))
#      end_mm <- ifelse(is.na(as.numeric(substring(deployment[i, 'Camera.Deployment.End.Date'], 15,16))), 0, as.numeric(substring(deployment[i, 'Camera.Deployment.End.Date'], 15,16)))
#      result[nrow(result), 'trap_days'] <- ifelse(end_hh > begin_hh, as.numeric(result[nrow(result), 'trap_days'])+1, ifelse(end_hh == begin_hh & end_mm > begin_mm, as.numeric(result[nrow(result), 'trap_days'])+1, as.numeric(result[nrow(result), 'trap_days'])))
      #      print(result)
    } else if (j == deployment[i, 'span_month']) {
      result[nrow(result)+1, ] <- c(deployment[i, 'Project.ID'], deployment[i, 'Deployment.ID'], as.character(deployment[i, 'Camera.Deployment.Begin.Date']), as.character(deployment[i, 'Camera.Deployment.End.Date']), 
                                   as.character(prev + days(1)), as.character(deployment[i, 'Camera.Deployment.End.Date']), 
                                   as.numeric(difftime(deployment[i, 'Camera.Deployment.End.Date'], prev + days(1), units="days"))+1)
    }
    else {
      result[nrow(result)+1, ] <- c(deployment[i, 'Project.ID'], deployment[i, 'Deployment.ID'], as.character(deployment[i, 'Camera.Deployment.Begin.Date']), as.character(deployment[i, 'Camera.Deployment.End.Date']), 
                                   as.character(prev + days(1)), as.character(eom(prev + days(1))), 
                                   as.numeric(difftime(eom(prev + days(1)), prev + days(1), units="days"))+1)
      prev <- as.Date(result[nrow(result), 'end'])
    }
  }
}
result$year <- as.numeric(substring(result$begin, 1, 4))
result$month <- as.numeric(substring(result$begin, 6, 7))
result$trap_days <- floor(as.numeric(result$trap_days))

print(head(deployment))
print(head(result))

## Vallidate trap days per camera is the same as trap day per year/month
sum(deployment$trap_days)
sum(result$trap_days)

## There are 13 records with different trap days
r <- group_by(result, Deployment.ID, Camera.Deployment.Begin.Date) %>% summarise(days=sum(trap_days))
deployment$Camera.Deployment.Begin.Date <- as.character(deployment$Camera.Deployment.Begin.Date)
c <- full_join(deployment, r, by=c('Deployment.ID', 'Camera.Deployment.Begin.Date'))
c[c$trap_days != c$days,]

## all of them involve a Feb 29 date, the expand calcuation is correct
deployment[deployment$Deployment.ID=='XX64_20160222_20160328',]
result[result$Deployment.ID=='XX64_20160222_20160328', ]