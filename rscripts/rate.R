## Eva Tang

library(dplyr)
library(zoo)
library(lubridate)


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

group_by(team, Project.ID) %>% summarise(location_cnt=length(unique(Deployment.Location.ID)))

deployment <- unique(team[, c('Project.ID', 'Sampling.Period', 'Deployment.Location.ID', 'Camera.Deployment.Begin.Date', 'Camera.Deployment.End.Date', 'Camera.Serial.Number')])
head(deployment)

deployment$trap_days <- as.numeric(difftime(deployment$Camera.Deployment.End.Date, deployment$Camera.Deployment.Begin.Date, units = c("days")))
deployment$trap_days <- floor(deployment$trap_days)

result <- group_by(deployment, Project.ID, Deployment.Location.ID, Sampling.Period) %>% summarise(trap_days=sum(trap_days), begin=min(Camera.Deployment.Begin.Date), end=max(Camera.Deployment.End.Date), num_deploy=length(unique(Camera.Deployment.Begin.Date))) %>% select(Deployment.Location.ID, Sampling.Period, trap_days)
print(result)

write.csv(result, "C://Users//tangkk//Documents//Big Data//datakind//data dive 201608//team_trap_days.csv", row.names=FALSE)

deployment$Camera.Deployment.Begin.Date <- as.POSIXct(deployment$Camera.Deployment.Begin.Date)
deployment$Camera.Deployment.End.Date <- as.POSIXct(deployment$Camera.Deployment.End.Date)

deployment$span_month <- 0
deployment$start_month <- 0
deployment$end_month <- 0

for (i in 1:dim(deployment)[1]) {
  if (as.Date(deployment[i, 'Camera.Deployment.Begin.Date']) < as.Date(deployment[i, 'Camera.Deployment.End.Date'])) {
#    deployment[i, 'span_month'] <- length(seq(from=as.Date(deployment[i, 'Camera.Deployment.Begin.Date']), to=as.Date(deployment[i, 'Camera.Deployment.End.Date']), by='month'))
    deployment[i, 'start_month'] <- as.numeric(format(deployment[i, 'Camera.Deployment.Begin.Date'],'%m'))
    deployment[i, 'end_month'] <- as.numeric(format(deployment[i, 'Camera.Deployment.End.Date'],'%m'))
#    print(paste(i, " - ", deployment[i, 'span_month'], sep=""))
  } else {
    deployment[i, 'span_month'] <- -1
  }
}

deployment$span_month <- ifelse(deployment$end_month < deployment$start_month,  deployment$start_month + 12 - deployment$end_month, deployment$end_month - deployment$start_month) + 1

result <- data.frame(Deployment.Location.ID=character(), Camera.Deployment.Begin.Date=character(), Camera.Deployment.End.Date=character(), 
                     begin=character(), end=character(), trap_days=numeric(), stringsAsFactors=FALSE)

#for (i in 1:dim(deployment)[1]) {
for (i in 1:2) {
  for (j in 1:deployment[i, 'span_month']) {
    if (j == 1) {
      result[nrow(result)+1, ] <- c(deployment[i, 'Deployment.Location.ID'], as.character(deployment[i, 'Camera.Deployment.Begin.Date']), as.character(deployment[i, 'Camera.Deployment.End.Date']), 
                                   as.character(deployment[i, 'Camera.Deployment.Begin.Date']), as.character(eom(deployment[i, 'Camera.Deployment.Begin.Date'])),  
                                   as.numeric(difftime(eom(deployment[i, 'Camera.Deployment.Begin.Date']), as.Date(deployment[i, 'Camera.Deployment.Begin.Date']), units="days")))
      prev <- eom(deployment[i, 'Camera.Deployment.Begin.Date'])
#      print(prev)
      
    } else if (j == deployment[i, 'span_month']) {
      result[nrow(result)+1, ] <- c(deployment[i, 'Deployment.Location.ID'], as.character(deployment[i, 'Camera.Deployment.Begin.Date']), as.character(deployment[i, 'Camera.Deployment.End.Date']), 
                                   as.character(prev + days(1)), as.character(deployment[i, 'Camera.Deployment.End.Date']), 
                                   as.numeric(difftime(eom(deployment[i, 'Camera.Deployment.Begin.Date']), as.Date(deployment[i, 'Camera.Deployment.Begin.Date']), units="days")))
    }
    else {
      result[nrow(result)+1, ] <- c(deployment[i, 'Deployment.Location.ID'], as.character(deployment[i, 'Camera.Deployment.Begin.Date']), as.character(deployment[i, 'Camera.Deployment.End.Date']), 
                                   as.character(prev + days(1)), as.character(eom(prev + days(1))), 
                                   as.numeric(difftime(eom(deployment[i, 'Camera.Deployment.Begin.Date']), as.Date(deployment[i, 'Camera.Deployment.Begin.Date']), units="days")))
      
    }
  }
}
print(head(deployment))
print(head(result))

# r <- sapply(result$begin, function(x) ifelse(nchar(x) > 10, strptime(x, "%Y-%m-%d %H:%M:%OS"), strptime(x, "%Y-%m-%d")))

# r <- apply(result$begin, 2, function(x) ifelse(nchar(result$begin) > 10, strptime(result$begin, "%Y-%m-%d %H:%M:%OS"), strptime(result$begin, "%Y-%m-%d"))

# result$begin <- as.POSIXct(strftime(result$begin, format=" %h:%m:%s"))
# result$end <- as.POSIXct(result$end)

# deployment$trap_days <- difftime(deployment$Camera.Deployment.End.Date, deployment$Camera.Deployment.Begin.Date, units='days')
# result$trap_days <- as.numeric(difftime(result$end, result$begin, units='days'))

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

# x <- seq(as.POSIXct("2001-01-10"),as.POSIXct("2005-12-10"),by="months")
# data.frame(before=x,after=eom(x))

# head(deployment)

# deployment[4142:4145,]

# deployment$trap_month <- NULL


# number of camera per deployment location
# group_by(deployment, Deployment.Location.ID) %>% summarise(cnt=length(Deployment.Location.ID)) %>% arrange(-cnt)

# deployment[deployment$Deployment.Location.ID == 'CT-VB-1-1', ]