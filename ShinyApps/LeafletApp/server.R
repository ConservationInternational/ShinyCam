detach("package:dplyr", unload=TRUE) # This is an unfortunate hack necessitated 
# by multiple packages with a "select" function
# Better solutions very much welcomed
library(raster)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(gstat)
library(sp)
library(data.table)
library(KernSmooth)
library(viridis)
library(rgdal)
library(rgeos)
source("scripts/kernel_density_estimate.R")
source("scripts/extra_plot.R")

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
#set.seed(100)
#zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
#zipdata <- zipdata[order(zipdata$centile),]

# Set parameters
raster_col <- "Reds" # IDW raster pallette 

# Read species information
OVERLAY_OPACITY <- 0.5
species.table <- read.csv("data/taxonomy_scientific_name_20160813_marin.csv")

red.list.table <- read.csv("data/taxonomy_red_list_status_20160813.csv")
red.list.table <- subset(red.list.table, id %in% c(3,4,8,9,5))             ##   Id numbers help filter rows with conservation statuses we care about
                                                                           ##   when evaluating redlist categories.

# Read shapefiles for park boundaries

shapefile_path <- "data/Shapefiles"

MCPparks <- readOGR(shapefile_path, "MCPparks", verbose = FALSE) %>%
     spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

GGNRA_incChedaJewel <- readOGR(shapefile_path, "GGNRA_incChedaJewel", verbose = FALSE) %>%
     spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

MMWD <- readOGR(shapefile_path, "MMWD", verbose = FALSE) %>%
     spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

SamuelPTaylor <- readOGR(shapefile_path, "SamuelPTaylor", verbose = FALSE) %>%
     spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))


shinyServer(function(input, output, session) {
  # Set up values for delayed map display
  values <- reactiveValues(starting = TRUE,
                           clickedMarker=list(id=NULL))
  # the same for species alert tab
  values.2 <- reactiveValues(starting = TRUE,
                           clickedMarker=list(id=NULL))
  session$onFlushed(function() {
    values$starting <- FALSE
  })
  
  # Function to read in input data based on project (TEAM or Marin)
  dataset_input <- reactive({
    if (input$dataset=="TEAM") {
      indat <- as.data.frame(fread("./data/team_rate_of_detection.csv"))   
    } else if (input$dataset == "MWPIP") {
      indat <- as.data.frame(fread("./data/rate_of_detection_MARIN_total.csv"))
    }
    names(indat) <- make.names(names(indat))                               ##   Make column names syntactically-valid (no spaces)
    names(indat)[names(indat) == "Longitude.Resolution"] <- "Longitude"
    names(indat)[names(indat) == "Latitude.Resolution"] <- "Latitude"
    indat <- subset(indat, Rate.Of.Detection >= 0 & Rate.Of.Detection < Inf)
    

    createTimeStamp <- function(samplingPeriod, year) {
      timeString = paste(year, samplingPeriod, "01", sep = "-")
      as.Date(timeString, "%Y-%m-%d")
    }

    indat$timestamp <- createTimeStamp(indat$Sampling.Period, indat$Year)

    indat
  })

  # Render Site Checkbox to select region
  output$site_checkbox <- renderUI({
    labels <- as.character(unique(dataset_input()$Project.ID))
    selectInput("site_selection", "Select Sites/Subregions", choices = labels, selected = labels[[1]])
  })

  # Reactive function to select site
  site_selection <- reactive({
    subset(dataset_input(), as.character(Project.ID) %in% input$site_selection)
  })

  # Create reactive data.frame containing only species present in selected sites
  # in selected project area
  present.species <- reactive({
    species <- unique(site_selection()[c("Genus", "Species")])
    present.species <- species.table[species.table$genus %in% species$Genus &
                                       species.table$species %in% species$Species,]
    # Switch out "" with "Unknown"
    present.species$guild <- as.factor(ifelse(as.character(present.species$guild) == "", "Unknown", as.character(present.species$guild)))

    present.species
  })
  # Create reactive vector containing the genus and species (concatenated) that
  # are present in the selected sites in the project area
  present.species.names <- reactive({
    species <- unique(site_selection()[c("Genus", "Species")])
    as.character(paste(species$Genus, species$Species))
  })

  # Render frequency selector
  output$frequency.control <- renderUI({
    frequencies <- unique(as.character(dataset_input()$Sampling.Type))
    selectInput("select_time", label = "Sampling Frequency",
                choices=frequencies, selected=1)
  })

  # Render time selector
  output$time.control <- renderUI({
    tmin <- min(dataset_input()$timestamp, na.rm=TRUE)
    tmax <- max(dataset_input()$timestamp, na.rm=TRUE)
    selectInput("time_slider", label = "Select Time", choices = as.character(unique(site_selection()$timestamp)))
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
                choices=sort(as.character(present.species.names())), selected=NULL, multiple=TRUE)
  })
  
  ## Interactive Map for Rate of Detection ######################################
  
  # Create the map
  output$map <- renderLeaflet({
    #Make idw (Inverse Distance Weighted interpolation) raster
    if (!values$starting) {
      if (nrow(mapping_dataset())>1) {
        # Ideally, "zero" counts would be included in the raster interpolation. They
        # are not currently. The broken code below should get close to implementing 
        # this with the caveat that it assumes that valid counts were available at all
        # sites for all times.
        #loc <- select(subset(site_selection(), as.character(Project.ID) %in% input$site_selection), 
        #              Latitude, Longitude)
        #in.dat.raw <- mapping_dataset()[c("Latitude", "Longitude", "Deployment.Location.ID", "Rate.Of.Detection")]
        #in.dat.agg <- aggregate(in.dat.raw, by=list(in.dat.raw$Deployment.Location.ID), FUN=mean)
        #in.dat <- left_join(loc, in.dat.agg, by=c("Latitude", "Longitude"))
        #in.dat[is.na(in.dat$Rate.Of.Detection),] <- 0
        
        # Calculate mean rate of detection values for each unique location
        in.dat.raw <- mapping_dataset()[c("Latitude", "Longitude", "Deployment.Location.ID", "Rate.Of.Detection")]
        in.dat.tab <- in.dat <- aggregate(select(in.dat.raw, -Deployment.Location.ID), 
                                          by=list(Deployment.Location.ID = in.dat.raw$Deployment.Location.ID), 
                                          FUN=mean)
        
        # Create the grid upon which the rate of detection values will be interpolated.
        ncells <- 50 # Set the number of cells in x and y direction
        pad.pct <- 0.1 # Set the xy padding around input points as percentage of range
        coordinates(in.dat) <- ~ Longitude + Latitude
        x.range <- c(min(in.dat$Longitude), max(in.dat$Longitude))
        y.range <- c(min(in.dat$Latitude), max(in.dat$Latitude))
        x.diff <- x.range[2]-x.range[1]
        y.diff <- y.range[2]-y.range[1]
        grd <- expand.grid(x = seq(from = x.range[1]-pad.pct*x.diff, to = x.range[2]+pad.pct*x.diff, by = (x.diff)/ncells),
                           y = seq(from = y.range[1]-pad.pct*y.diff, to = y.range[2]+pad.pct*y.diff, by = (y.diff)/ncells))
        coordinates(grd) <- ~x + y
        gridded(grd) <- TRUE
        
        # Compute IDW (starts as a matrix)
        tidw <- idw(Rate.Of.Detection ~ 1, locations=in.dat, newdata=grd, nmax=10, idp=4)
        dw.output = as.data.frame(tidw)
        names(dw.output)[1:3] <- c("long", "lat", "var1.pred")
        
        # Uncomment to turn on transparency for lowest quintile
        #dw.output[which(dw.output$var1.pred <= unname(quantile(dw.output$var1.pred, OVERLAY_OPACITY))), "var1.pred"] <- NA # Make parts of the raster opaque
        
        coordinates(dw.output) <- ~long+lat # Make the matrix into spatialPointsDataFrame
        proj4string(dw.output) <- CRS("+init=epsg:4326")
        gridded(dw.output) <- TRUE
        idw.raster <- raster(dw.output, layer=1, values=TRUE) # Make spatialPointsDataFrame into raster
      } else {
        idw.raster <- NULL
        # this selects the data for the whole site - we do this to support the dynamic view based on protected species/site selected
        in.dat <- site_selection()[c("Latitude", "Longitude", "Deployment.Location.ID", "Rate.Of.Detection")]
        
      }
      
      #dat <- get_KDE_polygons(site_selection())
      # Currently, the color scale for the raster in the map is set dynamically, meaning
      # that the rasters can't really be compared when the species selection changes.
      # It would be good to add a legend or a fixed color scale.
      unique_sites <-  unique(select(site_selection(), Deployment.Location.ID, Latitude, Longitude)) 
      if(nrow(in.dat)<1){ # this is never displayed, it only helps to avoid error at the very beginning (the rows of in.dat are 0 which doesn't help the view) 
        tmap <- leaflet(unique_sites) %>%
          addTiles(
            urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}",
            attribution = "Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC"
          )  %>% setView(-122.6,37.9,zoom=10)  
        
        #addProviderTiles("Thunderforest.Outdoors") # Like this one but let's integrate above.
      } else {
        # Allows dynamic view based on protected species/site selected
        # it takes the mean position of the selected species - if none was selected, considers all the species
        tmap <- leaflet(unique_sites) %>%
          addTiles(
            urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}",
            attribution = "Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC"
          )  %>% setView(mean(in.dat$Longitude),mean(in.dat$Latitude),zoom=10) 
      }
      
      if (nrow(site_selection())>0) {
        tmap <- tmap %>%
          addCircleMarkers(~Longitude, ~Latitude, layerId=NULL, weight=2, radius=2, color="black", fillOpacity=1)
      }
      
      # Park Boundary Checkbox
      if (input$boundary_checkbox == TRUE) {
        tmap <- tmap %>% 
          addPolygons(data = MCPparks, weight = 2, fill=FALSE) %>%
          addPolygons(data = GGNRA_incChedaJewel, weight = 2, fill=FALSE) %>%
          addPolygons(data = MMWD, weight = 2, fill=FALSE) %>%
          addPolygons(data = SamuelPTaylor, weight = 2, fill=FALSE)
      }
      
      
      
      if (!is.null(idw.raster)) {
        
        pal <- colorNumeric(
          palette = raster_col,
          domain = values(idw.raster)
        )
        
        tmap %>%
          addCircleMarkers(~Longitude, ~Latitude, weight=2, data= in.dat.tab, radius=~sqrt(Rate.Of.Detection), color="red", 
                           fillOpacity=1, layerId=in.dat.tab$Deployment.Location.ID, 
                           popup = ~paste("Deployment ID:", in.dat.tab$Deployment.Location.ID, "<br>Mean Rate of Detection:",in.dat.tab$Rate.Of.Detection)) %>%
          addRasterImage(idw.raster, opacity=0.4, colors=pal) %>%
          # Using leaflet built in legend function, which provides limited 
          # customizability. Consider updating to make custom legend in sidebar.
          addLegend(position = "bottomleft", pal = pal, 
                    values = values(idw.raster),
                    title = "Species density<br>(Interpolated)") # Units?
      } else {
        tmap
      }
    } else {
      NULL
    }
  })

  # Generate controls
  
  # Update species selection based on RED and guild
  # TODO: The if/else logic here could be cleaned up
  observe({
    #Modify selection based on nulls
    if (is.null(input$red) & is.null(input$guild)) {
      selected.names <- NULL
    } else if (is.null(input$red)) {
      trows <- as.character(present.species()$guild) %in% input$guild
      selected.species <- present.species()[trows,]
      selected.names <- paste(selected.species$genus, selected.species$species)
    } else if (is.null(input$guild)) {
      trows <- as.character(present.species()$red_list_status_id) %in%
        red.list.table$id[red.list.table$description %in% input$red]
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
    
    # Update species selection menu       
    selected.names <- sort(as.character(selected.names))
    
    updateSelectInput(session, "species", "Select Species (Multiple Possible)",
                      choices=sort(as.character(present.species.names())), selected=selected.names)
    
  })
  
  # Subset dataframe for plotting (no time subset)
  # Subset by project, site, frequency, and selected species
  plotting_dataset <- reactive({
    if (!is.null(input$species)) {
      subset(site_selection(), (Sampling.Type==input$select_time) & (paste(Genus, Species) %in% input$species))
    } else {
      data.frame()
    }
  })
  
  # Subset dataframe for mapping (time subset)
  # Subset by project, site, frequency, selected species, current time
  mapping_dataset <- reactive ({
    if (!is.null(input$species) & (!is.null(input$time_slider))) {
      subset(plotting_dataset(), timestamp==input$time_slider)
    } else {
      #subset(plotting_dataset(), rep(FALSE, times=nrow(plotting_dataset())))
      data.frame()
    }
  })
  
  # Subset dataframe for plotting based on map click 
  # This is subsetted on camera and speecies. Possibly species could be removed from filter.
  camera_dataset <- reactive ({
    subset(site_selection(), Deployment.Location.ID==values$clickedMarker$id & (paste(Genus, Species) %in% input$species))
  })
  
  # observe the marker click info and change the value depending on click location 
  observeEvent(input$map_marker_click,{
    values$clickedMarker <- input$map_marker_click
  }
  )
  observeEvent(input$map_click,{
    values$clickedMarker$id <- NULL
  })
  observeEvent(input$species, {
    values$clickedMarker$id <- NULL
  })
  
  
  # ############# Species Alert Tab ##############################
  # Copy/pasted the previous functions and added ".2" to them to avoid repetition of previous input/output
  
  # Function to read in input data based on project (TEAM or Marin)
  dataset_input.2 <- reactive({
    if (input$dataset.2=="TEAM") {
      indat <- as.data.frame(fread("./data/team_rate_of_detection.csv"))   
    } else if (input$dataset.2 == "MWPIP") {
      indat <- as.data.frame(fread("./data/rate_of_detection_MARIN_total.csv"))
    }
    names(indat) <- make.names(names(indat))                               ##   Make column names syntactically-valid (no spaces)
    names(indat)[names(indat) == "Longitude.Resolution"] <- "Longitude"
    names(indat)[names(indat) == "Latitude.Resolution"] <- "Latitude"
    indat <- subset(indat, Rate.Of.Detection >= 0 & Rate.Of.Detection < Inf)
    
    createTimeStamp.2 <- function(samplingPeriod, year) {
      timeString = paste(year, samplingPeriod, "01", sep = "-")
      as.Date(timeString, "%Y-%m-%d")
    }
    
    indat$timestamp <- createTimeStamp.2(indat$Sampling.Period, indat$Year)
    
    indat
  })
  
  # Render Site Checkbox to select region
  output$site_checkbox.2 <- renderUI({
    labels <- as.character(unique(dataset_input.2()$Project.ID))
    selectInput("site_selection.2", "Select Sites/Subregions", choices = labels, selected = labels[[1]])
  })
  
  # Reactive function to select site
  site_selection.2 <- reactive({
    subset(dataset_input.2(), as.character(Project.ID) %in% input$site_selection.2)
  })
  
  # Create reactive data.frame containing only species present in selected sites
  # in selected project area
  present.species.2 <- reactive({
    species <- unique(site_selection.2()[c("Genus", "Species")])
    present.species <- species.table[species.table$genus %in% species$Genus &
                                       species.table$species %in% species$Species,]
    # Switch out "" with "Unknown"
    present.species$guild <- as.factor(ifelse(as.character(present.species$guild) == "", "Unknown", as.character(present.species$guild)))
    
    present.species
  })
  # Create reactive vector containing the genus and species (concatenated) that
  # are present in the selected sites in the project area
  present.species.names.2 <- reactive({
    species <- unique(site_selection.2()[c("Genus", "Species")])
    as.character(paste(species$Genus, species$Species))
  })
  
  # Render frequency selector
  output$frequency.control.2 <- renderUI({
    frequencies <- unique(as.character(dataset_input.2()$Sampling.Type))
    selectInput("select_time.2", label = "Sampling Frequency",
                choices=frequencies, selected=1)
  })
  
  # Render time selector
  output$time.control.2 <- renderUI({
    tmin <- min(dataset_input.2()$timestamp, na.rm=TRUE)
    tmax <- max(dataset_input.2()$timestamp, na.rm=TRUE)
    selectInput("time_slider.2", label = "Select Time", choices = as.character(unique(site_selection.2()$timestamp)))
  })
  
  # Render guild selector
  output$guild.control.2 <- renderUI({
    guild.list <- sort(unique(as.character(present.species.2()$guild)))
    checkboxGroupInput("guild.2", "Select Guilds", choices=guild.list,
                       selected=NULL)
  })
  
  # Render RED selector
  output$red.control.2 <- renderUI({
    red.list <- sort(unique(as.character(red.list.table$description[red.list.table$id %in% present.species()$red_list_status_id])))
    checkboxGroupInput("red.2", "Select Red List Categories", choices=red.list,
                       selected=NULL)
  })
  
  # Render species selection
  output$species.list.2 <- renderUI({
    selectInput("species.2", "Select Species (Multiple Possible)",
                choices=sort(as.character(present.species.names.2())), selected=NULL, multiple=TRUE)
  })
  

  ## Interactive Map for Species Alert Tab ####

  # Create the map
  output$map.2 <- renderLeaflet({
    #Make idw (Inverse Distance Weighted interpolation) raster
    if (!values.2$starting) {
    if (nrow(mapping_dataset.2())>1) {
      # Ideally, "zero" counts would be included in the raster interpolation. They
      # are not currently. The broken code below should get close to implementing 
      # this with the caveat that it assumes that valid counts were available at all
      # sites for all times.
      #loc <- select(subset(site_selection(), as.character(Project.ID) %in% input$site_selection), 
      #              Latitude, Longitude)
      #in.dat.raw <- mapping_dataset()[c("Latitude", "Longitude", "Deployment.Location.ID", "Rate.Of.Detection")]
      #in.dat.agg <- aggregate(in.dat.raw, by=list(in.dat.raw$Deployment.Location.ID), FUN=mean)
      #in.dat <- left_join(loc, in.dat.agg, by=c("Latitude", "Longitude"))
      #in.dat[is.na(in.dat$Rate.Of.Detection),] <- 0
      
      # Calculate mean rate of detection values for each unique location
      in.dat.raw <- mapping_dataset.2()[c("Latitude", "Longitude", "Deployment.Location.ID", "Rate.Of.Detection")]
      in.dat.tab <- in.dat <- aggregate(select(in.dat.raw, -Deployment.Location.ID), 
                                        by=list(Deployment.Location.ID = in.dat.raw$Deployment.Location.ID), 
                                        FUN=mean)
      
      # Create the grid upon which the rate of detection values will be interpolated.
      ncells <- 50 # Set the number of cells in x and y direction
      pad.pct <- 0.1 # Set the xy padding around input points as percentage of range
      coordinates(in.dat) <- ~ Longitude + Latitude
      x.range <- c(min(in.dat$Longitude), max(in.dat$Longitude))
      y.range <- c(min(in.dat$Latitude), max(in.dat$Latitude))
      x.diff <- x.range[2]-x.range[1]
      y.diff <- y.range[2]-y.range[1]
      grd <- expand.grid(x = seq(from = x.range[1]-pad.pct*x.diff, to = x.range[2]+pad.pct*x.diff, by = (x.diff)/ncells),
                         y = seq(from = y.range[1]-pad.pct*y.diff, to = y.range[2]+pad.pct*y.diff, by = (y.diff)/ncells))
      coordinates(grd) <- ~x + y
      gridded(grd) <- TRUE
      
      # Compute IDW (starts as a matrix)
      tidw <- idw(Rate.Of.Detection ~ 1, locations=in.dat, newdata=grd, nmax=10, idp=4)
      dw.output = as.data.frame(tidw)
      names(dw.output)[1:3] <- c("long", "lat", "var1.pred")
      
      # Uncomment to turn on transparency for lowest quintile
      #dw.output[which(dw.output$var1.pred <= unname(quantile(dw.output$var1.pred, OVERLAY_OPACITY))), "var1.pred"] <- NA # Make parts of the raster opaque
      
      coordinates(dw.output) <- ~long+lat # Make the matrix into spatialPointsDataFrame
      proj4string(dw.output) <- CRS("+init=epsg:4326")
      gridded(dw.output) <- TRUE
      idw.raster <- raster(dw.output, layer=1, values=TRUE) # Make spatialPointsDataFrame into raster
    } else {
      idw.raster <- NULL
      # this selects the data for the whole site - we do this to support the dynamic view based on protected species/site selected
      in.dat <- site_selection.2()[c("Latitude", "Longitude", "Deployment.Location.ID", "Rate.Of.Detection")]

    }

    #dat <- get_KDE_polygons(site_selection())
    # Currently, the color scale for the raster in the map is set dynamically, meaning
    # that the rasters can't really be compared when the species selection changes.
    # It would be good to add a legend or a fixed color scale.
    unique_sites <-  unique(select(site_selection.2(), Deployment.Location.ID, Latitude, Longitude)) 
    if(nrow(in.dat)<1){ # this is never displayed, it only helps to avoid error at the very beginning (the rows of in.dat are 0 which doesn't help the view) 
      tmap <- leaflet(unique_sites) %>%
         addTiles(
           urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}",
           attribution = "Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC"
         )  %>% setView(-122.6,37.9,zoom=10)  
           
           #addProviderTiles("Thunderforest.Outdoors") # Like this one but let's integrate above.
    } else {
      # Allows dynamic view based on protected species/site selected
      # it takes the mean position of the selected species - if none was selected, considers all the species
      tmap <- leaflet(unique_sites) %>%
        addTiles(
          urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}",
          attribution = "Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC"
        )  %>% setView(mean(in.dat$Longitude),mean(in.dat$Latitude),zoom=10) 
    }
         
    if (nrow(site_selection.2())>0) {
      tmap <- tmap %>%
        addCircleMarkers(~Longitude, ~Latitude, layerId=NULL, weight=2, radius=2, color="black", fillOpacity=1)
    }
    
    # Park Boundary Checkbox
    if (input$boundary_checkbox.2 == TRUE) {
         tmap <- tmap %>% 
              addPolygons(data = MCPparks, weight = 2, fill=FALSE) %>%
              addPolygons(data = GGNRA_incChedaJewel, weight = 2, fill=FALSE) %>%
              addPolygons(data = MMWD, weight = 2, fill=FALSE) %>%
              addPolygons(data = SamuelPTaylor, weight = 2, fill=FALSE)
    }
    
    
    
    if (!is.null(idw.raster)) {
      
      pal <- colorNumeric(
        palette = raster_col,
        domain = values(idw.raster)
      )
      
      tmap %>%
        addCircleMarkers(~Longitude, ~Latitude, weight=2, data= in.dat.tab, radius=~sqrt(Rate.Of.Detection), color="red", 
                         fillOpacity=1, layerId=in.dat.tab$Deployment.Location.ID, 
                         popup = ~paste("Deployment ID:", in.dat.tab$Deployment.Location.ID, "<br>Mean Rate of Detection:",in.dat.tab$Rate.Of.Detection)) %>%
        addRasterImage(idw.raster, opacity=0.4, colors=pal) %>%
        # Using leaflet built in legend function, which provides limited 
        # customizability. Consider updating to make custom legend in sidebar.
        addLegend(position = "bottomleft", pal = pal, 
                  values = values(idw.raster),
                  title = "Species density<br>(Interpolated)") # Units?
    } else {
      tmap
    }
    } else {
      NULL
    }
  })
  
  # Update species selection based on RED and guild
  # TODO: The if/else logic here could be cleaned up
  observe({
    #Modify selection based on nulls
    if (is.null(input$red.2) & is.null(input$guild.2)) {
      selected.names <- NULL
    } else if (is.null(input$red.2)) {
      trows <- as.character(present.species()$guild) %in% input$guild.2
      selected.species <- present.species.2()[trows,]
      selected.names <- paste(selected.species$genus, selected.species$species)
    } else if (is.null(input$guild.2)) {
      trows <- as.character(present.specie.2()$red_list_status_id) %in%
        red.list.table$id[red.list.table$description %in% input$red.2]
      selected.species <- present.species.2()[trows,]
      selected.names <- paste(selected.species$genus, selected.species$species)
    } else {
      guilds <- (as.character(present.species.2()$guild) %in% input$guild.2)
      
      reds <- as.character(present.species()$red_list_status_id) %in%
        red.list.table$id[red.list.table$description %in% input$red.2]
      if (is.null(guilds) & is.null(reds)) {
        selected.names <- NULL
      } else if (is.null(guilds)) {
        selected.species <- present.species.2()[reds,]
        selected.names <- paste(selected.species$genus, selected.species$species)
      } else if (is.null(reds)) {
        selected.species <- present.species.2()[guilds,]
        selected.names <- paste(selected.species$genus, selected.species$species)
      } else {
        selected.species <- present.species()[guilds & reds,]
        selected.names <- paste(selected.species$genus, selected.species$species)
      }
      
    }
    
    # Update species selection menu       
    selected.names <- sort(as.character(selected.names))
    
    updateSelectInput(session, "species.2", "Select Species (Multiple Possible)",
                      choices=sort(as.character(present.species.names.2())), 
                      selected=selected.names)
    
  })
  
  # Subset dataframe for plotting (no time subset)
  # Subset by project, site, frequency, and selected species
  plotting_dataset.2 <- reactive({
    if (!is.null(input$species.2)) {
      subset(site_selection.2(), (Sampling.Type==input$select_time.2) & (paste(Genus, Species) %in% input$species.2))
    } else {
      data.frame()
    }
  })
  
  # Subset dataframe for mapping (time subset)
  # Subset by project, site, frequency, selected species, current time
  mapping_dataset.2 <- reactive ({
    if (!is.null(input$species.2) & (!is.null(input$time_slider.2))) {
      subset(plotting_dataset.2(), timestamp==input$time_slider.2)
    } else {
      #subset(plotting_dataset(), rep(FALSE, times=nrow(plotting_dataset())))
      data.frame()
    }
  })
  
  # Subset dataframe for plotting based on map click 
  # This is subsetted on camera and speecies. Possibly species could be removed from filter.
  camera_dataset <- reactive ({
    subset(site_selection.2(), Deployment.Location.ID==values.2$clickedMarker$id & (paste(Genus, Species) %in% input$species.2))
  })
  
  # observe the marker click info and change the value depending on click location 
  observeEvent(input$map_marker_click.2,{
    values.2$clickedMarker <- input$map_marker_click.2
  }
  )
  observeEvent(input$map_click.2,{
    values.2$clickedMarker$id <- NULL
  })
  observeEvent(input$species.2, {
    print("Hello!!!")
    values.2$clickedMarker$id <- NULL
  })
  

})
