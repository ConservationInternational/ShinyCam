library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
source("scripts/kernel_density_estimate.R")
source("scripts/extra_plot.R")

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]


## HACK (Michael): Temporary placement of the function.
createTimeStamp = function(samplingPeriod){
    timeString = paste(samplingPeriod, "01", sep = "-")
    as.Date(timeString, "%Y-%m-%d")
}

#Global
GRADIENT_SCALE <- 2
lat_long_data <- read.csv("data/rate_of_detection.csv")
#mapping_dataset()

# Read species information
species.table <- read.csv("data/taxonomy_scientific_name_20160813.csv")
red.list.table <- read.csv("data/taxonomy_red_list_status_20160813.csv")
red.list.table <- subset(red.list.table, id %in% c(3,4,8,9,5))

shinyServer(function(input, output, session) {

  # Read in input data based on project
  dataset_input <- reactive({
    if (input$dataset=="TEST!") {
      read.csv("./data/rate_of_detection.csv") %>%
          ## HACK (Michael): To temporary clean the data
          subset(., Rate.Of.Detection >= 0 & Rate.Of.Detection < Inf) %>%
          mutate(., timeStamp = createTimeStamp(samplingPeriod = Sampling.Period))
    }
  })

  # Select Region
  output$site_checkbox <- renderUI({
    labels <- as.character(unique(dataset_input()$Project.ID))
    selectInput("site_selection", "Select Sites/Subregions", choices = labels, selected = labels,
                multiple=TRUE)
  })

  # Select site
  site_selection <- reactive({
    subset(dataset_input(), as.character(Project.ID) %in% input$site_selection)
  })


  ######  ADD SLIDER

  filterDataset <- reactive ({
       data_by_time <- filter(dataset_input, Sampling.Type == input$select_time)

       #### TEMP

       data_by_time <- mutate(data_by_time, timestamp = NA)

       data_by_time$timestamp <- createTimeStamp(data_by_time$Year, data_by_time$Sampling.Period)

       return(data_by_time$timestamp)
  })



  observe({

       updateSliderInput(session, inputId="time_slider", step = NULL)


  })



  ######

  # Create reactive data.frame containing only species present in selected sites
  # in selected project area
  present.species <- reactive({
    species <- unique(site_selection()[c("Genus", "Species")])
    present.species <- species.table[species.table$genus %in% species$Genus &
                                       species.table$species %in% species$Species,]
    present.species
  })
  # Create reactive vector containing the genus and species (concatenated) that
  # are present in the project area
  present.species.names <- reactive({
    species <- unique(site_selection()[c("Genus", "Species")])
    paste(species$Genus, species$Species)
  })

  # Render guild selector
  output$guild.control <- renderUI({
    guild.list <- sort(unique(as.character(present.species()$guild)))
    checkboxGroupInput("guild", "Select Guilds", choices=guild.list,
                       selected=NULL)
  })

  # Render RED selector
  output$red.control <- renderUI({
    red.list <- sort(unique(as.character(red.list.table$description[red.list.table$id %in% present.species()$red_list_status_id])))
    checkboxGroupInput("red", "Select Red List Categories", choices=red.list,
                       selected=NULL)
  })

  # Render species selection
  output$species.list <- renderUI({
    selectInput("species", "Select Species (Multiple Possible)",
                choices=sort(present.species.names()), selected=NULL, multiple=TRUE)
  })


  ## Interactive Map ###########################################

  # Get unique pairs of lat long values for plotting cam locations
  locs <- select(lat_long_data, Latitude, Longitude)
  cam_lat_longs <- unique(locs)

  # Create the map
  output$map <- renderLeaflet({
    dat <- get_KDE_polygons(site_selection())
    leaflet(cam_lat_longs) %>%
      addTiles(
        urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}"
      ) %>%
      addCircleMarkers(~Longitude, ~Latitude, weight=2, radius=2, color="black", fillOpacity=1, layerId=NULL) %>%
      addPolygons(data=dat$poly, color = brewer.pal(dat$nlev, "Greens")[dat$levs], stroke=FALSE)

  })

  # TODO use filtered data as input
  #subsettedData <- select(TEAM_data,
  #  'Project'=Project.ID, 'Deployment Location ID'=Deployment.Location.ID,
  #  'Latitude'=Latitude.Resolution, 'Longitude'=Longitude.Resolution,
  #  'Sampling Type'=Sampling.Type, 'Sampling Period'=Sampling.Period, 'Year'=Year,
  #  'Genus'=Genus, 'Species'=Species, 'Rate Of Detection'=Rate.Of.Detection
  #)

  # TODO change file name based on filters
  output$downloadData <- downloadHandler(
    filename = function() { paste('data', '.csv', sep='') },
    content = function(file) {
      write.csv(subsettedData, file)
    }
  )

  output$table <- DT::renderDataTable({
    df <- subsettedData %>%
      mutate(Go = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })

  # Generate controls

  # Read in input data based on project
  #dataset_input <- reactive({
  #  if (input$dataset=="TEST!") {
  #    read.csv("./data/rate_of_detection.csv")
  #  }
  #})

  # Add



  #

  # Update species selection based on RED and guild
  observe({
     #Modify selection based on nulls
    if (is.null(input$red) & is.null(input$guild)) {
      selected.names <- NULL
    } else if (is.null(input$red)) {
      trows <- as.character(present.species()$guild) %in% input$guild
      selected.species <- present.species()[trows,]
      selected.names <- paste(selected.species$genus, selected.species$species)
    } else if (is.null(input$guild)) {
      trows <- as.character(present.species()$red_list_status_id %in%
                              red.list.table$id[red.list.table$description %in% input$red])
      selected.species <- present.species()[trows,]
      selected.names <- paste(selected.species$genus, selected.species$species)
    } else {
      guilds <- (as.character(present.species()$guild) %in% input$guild)

      reds <- as.character(present.species()$red_list_status_id) %in%
                        red.list.table$id[red.list.table$description %in% input$red]
      if (is.null(guilds) & is.null(reds)) {
        selected.names <- NULL
      } else if (is.null(guilds)) {
        selected.species <- present.species()[reds,]
        selected.names <- paste(selected.species$genus, selected.species$species)
      } else if (is.null(reds)) {
        selected.species <- present.species()[guilds,]
        selected.names <- paste(selected.species$genus, selected.species$species)
      } else {
        selected.species <- present.species()[guilds & reds,]
        selected.names <- paste(selected.species$genus, selected.species$species)
      }

    }

    updateSelectInput(session, "species", "Select Species (Multiple Possible)",
                      choices=present.species.names(), selected=selected.names)

  })

  ### 2 reactive dataframes:
  ### Both have a group column (as factor)
  # 1 - time subset
  # 2 - full data

  # Subset dataframe for plotting (no time subset)
  # Subset by project, site, frequency, and selected species
  plotting_dataset <- reactive({
    subset(site_selection(), )
  })

  # Subset dataframe for mapping (time subset)
  # Subset by project, site, frequency, selected species, current time
  mapping_dataset <- reactive ({
    subset(plotting_dataset(), )
  })

  # Subset dataframe for plotting based on map click
  camera_dataset <- reactive ({
    subset(site_selection(), )
  })


    ## Additional plots
    ## NOTE (Michael): This plot is only meaningful when the number of groupings
    ##                 .are small
    output$camera_ts_benchmark = renderPlot({
        plotCameraBenchmark(full_data = plotting_dataset(),
                            camera_data = camera_dataset(),
                            time = "timeStamp",
                            group = "Genus",
                            rate = "Rate.Of.Detection",
                            facet = FALSE)
        })

    output$total_ts = renderPlot({
        plotTotalTs(full_data = plotting_dataset(),
                    time = "timeStamp",
                    rate = "Rate.Of.Detection",
                    aggFUN = mean)
    })

    ## NOTE (Michael): This plot is not displayed correctly due to the Inf
    ##                 values in the data.
    output$top_five_plot = renderPlot({
        groupTopFive(plotting_dataset(),
                     group = "Genus",
                     rate = "Rate.Of.Detection")
    })


})
