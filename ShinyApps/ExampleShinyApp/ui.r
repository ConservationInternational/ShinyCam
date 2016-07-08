########################################################
# Goal:
# This file defines the user interface of our Shiny app.
# In our case, we just create a drop down menu for the 
# user to select the camera serial number, and then 
# we display the plot (which is designed in server.r).
########################################################

# install.packages('shiny')
# install.packages('data.table')

library(shiny)
library(data.table)


########################################################
# Data prep:
# Read the data, and clean it up a little bit.
# We just need a list of all unique camera serial numbers.
# I subset to get camera serial numbers corresponding to
# cameras that took more than 5 photos.
########################################################

# Read data and clean the column names:
data.dt <- fread(input = 'zcat < data/processed/Terrestrial_Vertebrate.csv.gz')
setnames(data.dt, names(data.dt), make.names(names(data.dt)))

# Count how many photos each camera took:
data.dt <- data.dt[, .N, by = .(Species, Camera.Make, Camera.Serial.Number)]

# Get rid of incomplete data. (Is this the right choice here?)
data.dt <- data.dt[Camera.Make != '' & Species != '']

# Get a list of unique serial numbers for cameras that took > 5 photos:
cameras <- unique(data.dt[N > 5, Camera.Serial.Number])

########################################################
# Define the UI.
# We just design the layout: a column containing a
# dropdown menu and a plot.
########################################################

shinyUI(
  fluidPage(    
    title = "What do our cameras see?",
    
    verticalLayout(
      selectInput("camera", "Camera", choices=cameras, selected="RM11AA08000474"),
      hr(),
      plotOutput("speciesPlot"),
      hr()
    )
  )
  
)

