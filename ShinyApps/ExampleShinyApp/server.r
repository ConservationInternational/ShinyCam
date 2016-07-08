########################################################
# Goal:
# This file defines the backend of our Shiny app.
# In our case, given a camera serial number chosen by the
# user (in ui.r), we create and style a plot.
########################################################

# install.packages('shiny')
# install.packages('data.table')
# install.packages('devtools')
# devtools::install_github("hadley/ggplot2")

library(shiny)
library(data.table)
library(ggplot2)

##########################################################################
# Data prep:
# Read the data, and munge. We reshape the data to get each row like:
# (serial #, species, number of this species seen)
# So, rows are unique combinations of (serial #, species).
#
# We create a second table:
# (serial #, camera make, camera model)
# This acts as a key to get the camera's data, given a serial number.
#
# Data issues:
# Camera make/model are likely entered by hand, and there are some 
# inconsistencies in the way people present this data (all caps, 
# extra information, etc.).
# Also, the same animal will appear multiple times in a single 'burst'
# of photos. At present, we count this animal multiple times. I'm 
# not sure yet how to handle this issue.
##########################################################################

data.dt <- fread(input = 'zcat < data/processed/Terrestrial_Vertebrate.csv.gz')
setnames(data.dt, names(data.dt), make.names(names(data.dt)))

# Get rid of incomplete data. (Is this the right choice here?)
data.dt <- data.dt[Camera.Make != '' & Species != '']

# A key to go from camera serial number to make and model:
# We spot fix inconsistencies in camera make/model names. 
# This isn't really quite the right thing to do - it won't generalize.
make_and_model <- unique(data.dt[, .(Camera.Serial.Number, Camera.Make, Camera.Model)])
make_and_model[, Camera.Make  := gsub("'", '', Camera.Make)]
make_and_model[, Camera.Model := gsub("'", '', Camera.Model)]
make_and_model[, Camera.Model := gsub("RAPIDFIRE", "RapidFire", Camera.Model)]
make_and_model[, Camera.Model := gsub("RM45 RapidFire", "RapidFire", Camera.Model)]

# Convert 'Number.of.Animals' from string to numeric and sum.
# This glosses over the fact that the same animal will be seen multiple times in one 
# 'burst' of the camera!
data.dt[, Number.of.Animals := as.numeric(data.dt$Number.of.Animals)]
data.dt <- data.dt[, sum(Number.of.Animals), by = .(Species, Camera.Serial.Number)]
setnames(data.dt, "V1", "Animals.Seen")

###############################################################
# Define the backend
# We make a single bar chart. 
# To do so, we select the subset of data corresponding 
# to the camera chosen by the user.Then we plot with ggplot2.
###############################################################

shinyServer(function(input, output) {
  
  # Get the subset of the data corresponding to the camera serial number selected in the app.
  # Then we reorder the table so the bars in the bar chart are ordered longest to shortest.
  this_camera <- reactive({
                      subset <- data.dt[Camera.Serial.Number == input$camera, ]
                      subset[, Species := factor(Species, levels = subset[order(Animals.Seen), Species])]
  })
  
  # Get the camera's make and model:
  camera_make_and_model <- reactive({make_and_model[Camera.Serial.Number == input$camera, ]})
  
  # Plot:
  output$speciesPlot <- renderPlot(
                            ggplot(this_camera(), aes(x = Species, y = Animals.Seen)) + 
                              geom_bar(stat  = 'identity', # Just show the count that we've already computed.
                                       fill  = '#0F99D4',  # CI blue
                                       color = '#0A668C'   # CI blue, a little darker
                              ) + 
                              labs(x        = 'Species', 
                                   y        = "Number of Animals Seen", 
                                   title    = paste("Species Seen by Camera", input$camera), 
                                   caption  = "Data from Conservation International, July 2016",
                                   subtitle = paste("Camera is a", unique(camera_make_and_model()$Camera.Make), unique(camera_make_and_model()$Camera.Model))
                              ) + 
                              coord_flip() + # show labels on y-axis, count on x-axis
                              theme_bw()     # white background
                        )
  
  
})