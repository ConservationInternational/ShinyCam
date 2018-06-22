f.order.data<-function(data){
  indx<-order(data$Project.ID,data$Genus.Species,data$Deployment.Location.ID,data$date.time.capture.format, decreasing = FALSE)
  data<-data[indx,]
  data
}