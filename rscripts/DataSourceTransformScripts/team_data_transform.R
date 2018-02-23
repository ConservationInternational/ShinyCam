# Eric Fegraus January 2018
# Get team dataset to be the same as the Wildlife Insights template that used in ShinyCam.
# This is for a custom, advanced download of camera trap data from the TEAM database.
library(dplyr)
shinycam_path <- "/Users/efegraus/work/DataKind/ShinyCam_new/ShinyCam"
setwd(shinycam_path)
team <- load("ct_data_adv2017-07-18")
team_names <- colnames(cam_trap_data_adv)

marin.data.complete=fread("/Users/efegraus/work/DataKind/Leaflet App 062117_PreProcessing_Final 2/PreProcessing/data/marin_data.csv",header=TRUE, sep = ',')
marin_names <- colnames(marin.data.complete)

# Create the dataframe that will be replaced
team_new  <- cam_trap_data_adv
# Fix names in TEAM
setnames(team_new,"ID","V1")
setnames(team_new,"Site.Name","Project.ID")
team_new$Deployment.ID <- paste(cam_trap_data_adv$Sampling.Period,cam_trap_data_adv$Sampling.Unit.Name,sep="-")
setnames(team_new,"Raw.Name","Image.ID")
team_new$Location <- "TBC"
team_new$Photo.Type.Identified.by <- "TBC"
team_new$Genus.Species <- paste(cam_trap_data_adv$Genus,cam_trap_data_adv$Species,sep=" ")
team_new$IUCN.Identification.Numbers <- "TBC"
setnames(team_new, "Photo.Taken.Time","Date_Time.Captured")
team_new$Age <- "TBC"
team_new$Sex <- "TBC"
team_new$Individual.ID <- "TBC"
team_new$Animal.recognizable <- "TBC"
setnames(team_new,"Number.of.Animals","Count")
setnames(team_new,"Photo.Notes","individual.Animal.notes")
team_new$individual.Animal.notes <- paste(team_new$individual.Animal.Notes,team_new$Camera.Notes)
team_new$Project.ID.X <- "TBC"
setnames(team_new,"Event.ID","Event")
team_new$Array.Name <- "TBC"
team_new$Deployment.Location.ID <- cam_trap_data_adv$Sampling.Unit.Name
setnames(team_new,"Longitude","Longitude.Resolution")
setnames(team_new,"Latitude","Latitude.Resolution")
setnames(team_new,"Camera.Start.Date.and.Time","Camera.Deployment.Begin.Date")
setnames(team_new,"Camera.End.Date.and.Time","Camera.Deployment.End.Date")
team_new$Bait.Type <- "None"
team_new$Bail.Description <- NA
team_new$Feature.Type <- "TBC"
team_new$Feature.Type.methodolgy <- "TBC"
setnames(team_new,"Camera.Serial.Number","Camera.ID")
team_new$Quiet.Period.Setting <- "TBC"
team_new$Restriction.on.access <- "TBC"
team_new$Camera.Failure.Details <- "TBC"
team_new$Project.ID.y <- "TBC"
setnames(team_new,"Camera.Make","Make")
setnames(team_new,"Camera.Model","Model")
team_new$Serial.Number <- team_new$Camera.ID # same as Camera Id
team_new$Year.Purchased <- "9999"

# Drop all the columns that don't need to be there.
team_final <- select(team_new,-Flash,-Triplet.Number,-Class,-Family,-Order,-Metadata.Id,-Metadata.exif_date_time,-Metadata.exif_date_time_original,-Metadata.exif_date_time_digitized,-Metadata.exif_metering_mode,-Metadata.exif_flash_name,-Metadata.jpeg_comment_dat,-Metadata.jpeg_comment_bat,-Metadata.jpeg_comment_ill,-Metadata.jpeg_comment_lbl,-Metadata.jpeg_comment_nam,-Metadata.jpeg_comment_ver,-Exposure.Time,-Sequence.Info,-Moon.Phase,-Temperature,-Memory.Card.Serial.Number,-Sampling.Unit.Name,-Genus,-Species,-Camera.Notes,-Sampling.Period)
sort(colnames(team_final))
sort(colnames(marin.data.complete))
save(team_final,file="team_final_for_shinycam")

