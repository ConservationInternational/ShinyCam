# KEY FUNCTIONS BELOW

#Order the data by Sampling unit name and photo raw name. This will order images chronologically
# Custumizing the order for Marin data
f.order.data<-function(data){
  indx<-order(data$Project.ID,data$Deployment.Location.ID,data$Event,data$Date_Time.Captured)
  data<-data[indx,]
  data
}


#Separate independent photographic events for a species in a given camera trap and date. thresh gives the threshold for considering events separate
#thresh is in minutes
f.separate<-function(data,thresh){
  
  #diff(data$td.photo)
  l<-length(data)
  interval<-diff(data)#abs(c(data[2:l],NA)-data)
  interval<-interval/60 #convert to minutes
  interval<-as.numeric(interval)
  ev <- 1; # assign ev variable
  res <- numeric() # Set blank vector
  cond <- interval>thresh #determine which interval values are greater than the threshold
  
  for(i in 1:(l-1)){
    if(!cond[i]) { # assign uniqe event determine by threshold
      ev<-ev       # If time diff is less than thres keep the same event
    } else {
      ev<-ev+1     # If time difference is greater than thresh assign new event
    }  
    res<-c(res,ev)
  }
  c(1,res)
}



#function to separate independent events, extract from the list and paste together with the data set.
#This function removes records that are NOT images.. e.g. Sampling Date records
f.separate.events<-function(data,thresh){
  
  #e.data<-by(data$td.photo,data$Sampling.Unit.Name,f.separate,thresh)
  indx<-which(is.na(data$Date_Time.Captured))
  if(length(indx)>0) {
    data<-data[-indx,]
  }
  e.data<-f.separate(data$Date_Time.Captured,thresh)
  #e.data<-data.frame(grp=unlist(e.data))
  data.frame(data,grp=paste(data$Event,".",data$Deployment.Location.ID,".",e.data,sep=""))
}


eom <- function(date) {
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2 
  year <- date.lt$year
  year <- year + as.integer(mon==13) # if month was December add a year
  mon[mon==13] <- 1
  iso = ISOdate(1900+year, mon, 1,hour=0,tz=attr(date,"tz"))
  result = as.POSIXct(iso) - 86400 # subtract one day
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}
