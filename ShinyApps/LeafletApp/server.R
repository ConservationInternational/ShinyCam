library(dplyr)
detach("package:dplyr", unload=TRUE) # This is an unfortunate hack necessitated 
# by multiple packages with a "select" function
# Better solutions very much welcomed
library(raster)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(gstat)
library(sp)
library(data.table)
library(KernSmooth)
library(viridis)
library(rgdal)
library(rgeos)
library(tidyr)
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
species.table <- read.csv("data/taxonomy_scientific_name_20160813.csv")

red.list.table <- read.csv("data/taxonomy_red_list_status_20160813.csv")
red.list.table <- subset(red.list.table, id %in% c(3,4,8,9,5))             ##   Id numbers help filter rows with conservation statuses we care about
##   when evaluating redlist categories.

# Read Camera Stats
dm_01_count_images <- read.csv('data/processed/dm_01_count_images.csv', stringsAsFactors = FALSE) 
dm_02_count_blanks <- read.csv('data/processed/dm_02_count_blanks.csv', stringsAsFactors = FALSE)
dm_03_count_unknowns <- read.csv('data/processed/dm_03_count_unknowns.csv', stringsAsFactors = FALSE)
dm_04_count_uncatalogued <- read.csv('data/processed/dm_04_count_uncatalogued.csv', stringsAsFactors = FALSE)
dm_05_count_wildlife <- read.csv('data/processed/dm_05_count_wildlife.csv', stringsAsFactors = FALSE)
dm_06_count_human_related <- read.csv('data/processed/dm_06_count_human_related.csv', stringsAsFactors = FALSE)
dm_07_avg_photos_per_deployment <- read.csv('data/processed/dm_07_avg_photos_per_deployment.csv', stringsAsFactors = FALSE)

# Read shapefiles for park boundaries
## Need to modify 
#shapefile_path <- "data/Shapefiles"
# Read shapefiles for park boundaries

shapefile_path <- "data/Shapefiles/Marin/"

#GunturPapandayan
#GP <- readOGR(shapefile_path, "KPHK GUNTUR-PAPANDAYAN", verbose = FALSE) %>%
#   spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
MCPparks <- readOGR(shapefile_path, "MCPparks", verbose = FALSE) %>%
  spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
# 
GGNRA_incChedaJewel <- readOGR(shapefile_path, "GGNRA_incChedaJewel", verbose = FALSE) %>%
  spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
# 
MMWD <- readOGR(shapefile_path, "MMWD", verbose = FALSE) %>%
  spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
# 
SamuelPTaylor <- readOGR(shapefile_path, "SamuelPTaylor", verbose = FALSE) %>%
  spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

## Functions for data cleanup on import ##
rename_cols <- function(data){  ##   Make column names syntactically-valid (no spaces)
  names(data) <- make.names(names(data))                               
  names(data)[names(data) == "Longitude.Resolution"] <- "Longitude"
  names(data)[names(data) == "Latitude.Resolution"] <- "Latitude"
  return(data)
}

createTimeStamp <- function(samplingPeriod, year) {
  timeString = paste(year, samplingPeriod, "01", sep = "-")
  as.Date(timeString, "%Y-%m-%d")
}



shinyServer(function(input, output, session) {
  # Set up values for delayed map display
  values <- reactiveValues(starting = TRUE,
                           clickedMarker=list(id=NULL))
  # the same for species alert tab -- not sure if it is necessary
  values.2 <- reactiveValues(starting = TRUE,
                             clickedMarker=list(id=NULL))
  session$onFlushed(function() {
    values$starting <- FALSE
    values.2$starting <- FALSE
  })


  # Function to read in input data based on project (TEAM or Marin)
  dataset_input <- reactive({
    #  if (input$dataset=="TEAM") {
    #  indat <- as.data.frame(fread("./data/team_rate_of_detection.csv"))   
    # } else if (input$dataset == "MWPIP") {
    indat <- rename_cols(as.data.frame(fread("./data/marin_rate_of_detection_120secs.csv")))
    #}
    # Why subset
    # indat <- subset(indat, Rate.Of.Detection >= 0 & Rate.Of.Detection < Inf)
    
    indat$timestamp <- createTimeStamp(indat$Sampling.Period, indat$Year)
    
    indat
  })
  
  # Render Site Checkbox to select region
  output$site_checkbox <- renderUI({
    labels <- as.character(unique(dataset_input()$Project.ID))
    
    ##    Add "All Sites" option to labels vector if dataset is "MWPIP"
    if(input$dataset == "MWPIP"){
      labels <- c(labels, "All Sites")
    }
    
    selectInput("site_selection", "Select Sites/Subregions", choices = labels, selected = labels[[1]])
  })
  
  ## Check if "All Sites" is the selected site
  check_allsites <- reactive({
    input$site_selection == "All Sites" %>%
      return()
  })
  
  
  # Reactive function to select site
  site_selection <- reactive({
    print(check_allsites())
    
    
    if(req(input$site_selection) == "All Sites"){
      dataset_input()
    } else{
      
      subset(dataset_input(), as.character(Project.ID) %in% input$site_selection)
    }
    
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
    selectInput("time_slider", label = "Select Time", choices = sort(as.character(unique(site_selection()$timestamp))))
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
  
  
  ## Leaflet Detection Rate Map ###########################################
  
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
      
      # Park Boundary Checkbox - Showing Shapefile names needs to be dynamic
      if (input$boundary_checkbox == TRUE) {
        tmap <- tmap %>% 
          # UPDATE HERE for SPATIAL
          #addPolygons(data = GP, weight = 2, fill=FALSE)# %>%
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
                    title = "Rates of Detection<br>(Interpolated)") # Units?
      } else {
        tmap
      }
    } else {
      NULL
    }
  })
  
  # TODO use filtered data as input
  #subsettedData <- select(TEAM_data,
  #  'Project'=Project.ID, 'Deployment Location ID'=Deployment.Location.ID,
  #  'Latitude'=Latitude.Resolution, 'Longitude'=Longitude.Resolution,
  #  'Sampling Type'=Sampling.Type, 'Sampling Period'=Sampling.Period, 'Year'=Year,
  #  'Genus'=Genus, 'Species'=Species, 'Rate Of Detection'=Rate.Of.Detection
  #)
  
  
  # Data-export functionality for "Download Data" button
  # TODO Update downloads...
  #data_event <- as.data.frame(fread("./data/_trap_days.csv"))
  
  output$downloadData1 <- downloadHandler(
    filename = function() { paste('data', '.csv', sep='') },
    content = function(file) {
      write.csv(data_event, file)
    }
  )
  # Code to download a file
  output$downloadData2 <- downloadHandler(
    filename <- function() {
      paste("combined", ".csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("data/combined.csv", file)
    },
    contentType = "application/zip"
  )
  
  #  Render JavaScript table widget for "Data Explorer" tab
  output$table <- DT::renderDataTable({
    #df <- subsettedData %>%
    # TODO: Confirm that mapping_dataset is the correct df to display
    df <- mapping_dataset() %>%
      mutate(Go = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
  
  ## Render JavaScript table widget for "Camera Stats" tab
  
  camera_stat <- reactive({
    
    ## A resuable function for all the aggregations that involve counting images
    ## Note the use of both Standard and Nonstandard Evalution in dplyr functions
    
    aggregations <- function(df, metric) {
      
      sum_param <- paste0("sum(", metric,")") 
      
      if (input$selectAgg == 2) {
        
        df %>%
          group_by(Project.ID) %>%
          summarise_(sum_param) %>%
          return()
        
      } else if (input$selectAgg == 3) {
        
        df %>%
          group_by(Camera.ID) %>%
          summarise_(sum_param) %>%
          return()
      } else {
        
        return(df)
      }
      
    }
    
    ## Control flow for aggregations for every camera-related stat
    
    if(input$selectStat == 1){
      
      aggregations(dm_01_count_images, "count_images") 
      
    } else if (input$selectStat == 2 ){
      
      aggregations(dm_02_count_blanks, "count_blanks")
      
    } else if (input$selectStat == 3){
      
      aggregations(dm_03_count_unknowns, "count_unknowns")
      
    } else if (input$selectStat == 4){
      
      aggregations(dm_04_count_uncatalogued, "count_uncatalogued")
      
    } else if (input$selectStat == 5) {
      
      aggregations(dm_05_count_wildlife, "count_wildlife")
      
    } else if (input$selectStat == 6) {
      
      aggregations(dm_06_count_human_related, "count_human_related")
      
    } else {
      
      camera_df <- dm_07_avg_photos_per_deployment
      
      if (input$selectAgg == 1) {
        
        return(camera_df)    
        
      } else if (input$selectAgg == 2) {
        
        camera_df %>%
          group_by(Project.ID) %>%
          summarize(num_deployments = sum(num_deployments), count_photos = sum(count_photos)) %>%
          mutate(avg_photos_per_deployment = round(count_photos / num_deployments, 2)) %>%
          return()
        
      } else if (input$selectAgg == 3) {
        
        camera_df %>%
          group_by(Camera.ID) %>%
          summarize(num_deployments = sum(num_deployments), count_photos = sum(count_photos)) %>%
          mutate(avg_photos_per_deployment = round(count_photos / num_deployments, 2)) %>%
          return()
        
      }
    }
    
    
  })
  
  
  # camera_calc <- reactive({
  #      
  #      camera_stat() %>% group_by(Project.ID)
  #           summarize(sum(count_images))
  # })
  # 
  output$camtable <- DT::renderDataTable({
    
    df <- camera_stat()
    
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
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
  
  
  ## Additional plots
  
  ## NOTE (Michael): This plot is only meaningful when the number of
  ##                 groupings are small.
  
  output$camera_ts_benchmark = renderPlot({
    if (!is.null(values$clickedMarker$id) & !is.null(input$species)) {
      plotCameraBenchmark(full_data = plotting_dataset(),
                          camera_data = camera_dataset(),
                          time = "timestamp",
                          group = "Genus",
                          rate = "Rate.Of.Detection",
                          facet = FALSE)
    } else {NULL}
  })
  
  output$camera_ts_benchmark_facet = renderPlot({
    if (!is.null(values$clickedMarker$id) & !is.null(input$species)) {
      plotCameraBenchmark(full_data = plotting_dataset(),
                          camera_data = camera_dataset(),
                          time = "timestamp",
                          group = "Genus",
                          rate = "Rate.Of.Detection",
                          facet = TRUE)
    } else {NULL}
  })
  
  output$total_ts = renderPlot({
    if (!is.null(values$clickedMarker$id) & !is.null(input$species)) {
      plotTotalTs(full_data = plotting_dataset(),
                  time = "timestamp",
                  rate = "Rate.Of.Detection",
                  aggFUN = mean)
    } else {NULL}
  })
  
  ## NOTE (Michael): This plot is not displayed correctly due to the Inf
  ##                 values in the data.
  output$top_five_plot = renderPlot({
    if (!is.null(input$species)) {
      groupTopFive(plotting_dataset(),
                   group = "Genus",
                   rate = "Rate.Of.Detection")
    } else {NULL}
  })
  
  output$health_ts = renderPlot({
    if (!is.null(input$species)) {
      health_timeseries(data = plotting_dataset(),
                        group = "Genus",
                        rate = "Rate.Of.Detection",
                        year = "Year")
    } else {NULL}
  })
  
  # # ############# Species Alert Tab ##############################
  # # Copy/pasted the previous functions and added ".2" to them to avoid repetition of previous input/output
  # #T his should be same as inital load..no need to reload
  # # Function to read in input data based on project (TEAM or Marin)
  # dataset_input.2 <- reactive({
  #   #if (input$dataset.2=="TEAM") {
  #   # indat <- as.data.frame(fread("./data/team_rate_of_detection.csv"))   
  #   #} else if (input$dataset.2 == "MWPIP") {
  #   indat <- rename_cols(as.data.frame(fread("./data/marin_rate_of_detection_120secs.csv"))) ## This should be same as inital load..no need to reload
  #   #}
  #   indat <- subset(indat, Rate.Of.Detection >= 0 & Rate.Of.Detection < Inf)
  #   indat$timestamp <- createTimeStamp(indat$Sampling.Period, indat$Year)
  #   
  #   indat
  # })
  # 
  # # Render Site Checkbox to select region
  # output$site_checkbox.2 <- renderUI({
  #   labels <- as.character(unique(dataset_input.2()$Project.ID))
  #   selectInput("site_selection.2", "Select Sites/Subregions", choices = labels, selected = labels[[1]])
  # })
  # 
  # # Reactive function to select site
  # site_selection.2 <- reactive({
  #   subset(dataset_input.2(), as.character(Project.ID) %in% input$site_selection.2)
  # })
  # 
  # # Create reactive data.frame containing only species present in selected sites
  # # in selected project area
  # present.species.2 <- reactive({
  #   species <- unique(site_selection.2()[c("Genus", "Species")])
  #   present.species <- species.table[species.table$genus %in% species$Genus &
  #                                      species.table$species %in% species$Species,]
  #   # Switch out "" with "Unknown"
  #   present.species$guild <- as.factor(ifelse(as.character(present.species$guild) == "", "Unknown", as.character(present.species$guild)))
  #   
  #   present.species
  # })
  # # Create reactive vector containing the genus and species (concatenated) that
  # # are present in the selected sites in the project area
  # present.species.names.2 <- reactive({
  #   species <- unique(site_selection.2()[c("Genus", "Species")])
  #   as.character(paste(species$Genus, species$Species))
  # })
  # 
  # # Render frequency selector
  # output$frequency.control.2 <- renderUI({
  #   frequencies <- unique(as.character(dataset_input.2()$Sampling.Type))
  #   selectInput("select_time.2", label = "Sampling Frequency",
  #               choices=frequencies, selected=1)
  # })
  # 
  # # Render time selector
  # output$time.control.2 <- renderUI({
  #   tmin <- min(dataset_input.2()$timestamp, na.rm=TRUE)
  #   tmax <- max(dataset_input.2()$timestamp, na.rm=TRUE)
  #   selectInput("time_slider.2", label = "Select Time", choices = as.character(unique(site_selection.2()$timestamp)))
  # })
  # 
  # # Render guild selector
  # output$guild.control.2 <- renderUI({
  #   guild.list <- sort(unique(as.character(present.species.2()$guild)))
  #   checkboxGroupInput("guild.2", "Select Guilds", choices=guild.list,
  #                      selected=NULL)
  # })
  # 
  # # Render RED selector
  # output$red.control.2 <- renderUI({
  #   red.list <- sort(unique(as.character(red.list.table$description[red.list.table$id %in% present.species()$red_list_status_id])))
  #   checkboxGroupInput("red.2", "Select Red List Categories", choices=red.list,
  #                      selected=NULL)
  # })
  # 
  # # Render species selection
  # output$species.list.2 <- renderUI({
  #   selectInput("species.2", "Select Species (Multiple Possible)",
  #               choices=sort(as.character(present.species.names.2())), selected=NULL, multiple=TRUE)
  # })
  # 
  # #Choose the data event subsetting. All data is now preprocessed and matches these subsets
  # output$subsettingradio <- renderUI({
  #   radioButtons("radiosubsetting", label = HTML("<b>Time Duration Subsetting</b>"),
  #                choices = list(" 2 Minutes" = 1, "30 Minutes" = 2,"1 Day" = 3), selected = 1)
  # })
  # 
  # 
  # ## Interactive Map for Species Alert Tab ####
  # 
  # # Create the map
  # output$map.2 <- renderLeaflet({
  #   # Prepare data
  #   if (!values.2$starting) {
  #     if (nrow(mapping_dataset.2())>0) {
  #       
  #       # subset and unite Genus and Species to get Binomial (which is more useful for specie identification)
  #       in.dat.raw <- mapping_dataset.2()[c("Latitude", "Longitude", "Deployment.Location.ID", "Genus", "Species")] %>%
  #         unite("Binomial", Genus, Species, sep =" ",remove = TRUE)
  #       # now get number of observation of each specie in a location ID
  #       in.dat <- in.dat.agg <- in.dat.raw %>% 
  #         group_by(Latitude, Longitude, Deployment.Location.ID, Binomial) %>% mutate(Binomial.count = n())
  #       
  #       alert_vector <- c()
  #       # print(unique(terrestrial_mammals$binomial))
  #       if (nrow(present.species.2()) >= 1){  # if there is data in the current selection
  #         for (i in 1:nrow(in.dat)){
  #           # convert each coordinate in in.dat to spacial point
  #           sp <- SpatialPoints(matrix(c(in.dat$Longitude[i],in.dat$Latitude[i]), ncol=2), proj4string = CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
  #           # subset for the species for row i
  #           index = sum(terrestrial_mammals$binomial == in.dat$Binomial[i])
  #           if(index > 0){
  #             print(paste("Found specie", in.dat$Binomial[i]))
  #           }
  #           spgeom <- terrestrial_mammals[terrestrial_mammals$binomial == in.dat$Binomial[i],]
  #           # union of shapefiles that belong to that specie
  #           # try to convert
  #           alert_vector <- c(alert_vector, isPointInBoundaries(sp, spgeom, in.dat$Binomial[i]))
  #         }
  #         in.dat$alert <- alert_vector
  #       } 
  #     } else {
  #       # this selects the data for the whole site - we do this to support the dynamic view based on protected species/site selected
  #       in.dat <- site_selection.2()[c("Latitude", "Longitude", "Deployment.Location.ID", "Genus", "Species")] %>%
  #         unite("Binomial", Genus, Species, sep =" ",remove = TRUE)
  #       in.dat$alert <- c()
  #     }
  #     
  #     if (nrow(mapping_dataset.2()) >= 1) {
  #       # adding different colors to circles (points representing cameras) in map
  #       unique_sites <-  unique(select(site_selection.2(), Deployment.Location.ID, Latitude, Longitude)) 
  #       unique_sites <- left_join(unique_sites, in.dat, by=c("Latitude", "Longitude"))
  #       
  #       # aggregate data in unique_sites to have in one column all the names of the out of boundaries species
  #       # also if the site has at least one FALSE, which means, one specie out of boundaries, it will be marked as FALSE and colored red
  #       # remove all missing data before manipulating
  #       unique_sites <- unique_sites[complete.cases(unique_sites), ]
  #       # initialize new dataframe
  #       dt <- unique_sites %>% 
  #         group_by(Deployment.Location.ID.x, Latitude, Longitude) %>%
  #         summarize(count=n())
  #       n <- nrow(dt)
  #       df <- data.frame(Deployment.Location=character(n),
  #                        Latitude=double(n), 
  #                        Longitude=double(n), 
  #                        Species=character(n),
  #                        CountSpecies=integer(n),
  #                        InsideBoundaries=character(n),
  #                        stringsAsFactors=FALSE) 
  #       
  #       df$Deployment.Location <-dt$Deployment.Location.ID.x
  #       df$Latitude = dt$Latitude
  #       df$Longitude = dt$Longitude
  #       df$CountSpecies = dt$count
  #       df$InsideBoundaries <- "TRUE"  # start with all TRUE (all inside)
  #       # first set the not founds
  #       for(i in 1:nrow(unique_sites)){
  #         if(unique_sites$alert[i] == "NOT FOUND"){
  #           row_index <- which(df$Deployment.Location == unique_sites$Deployment.Location.ID.x[i])
  #           df[row_index, "InsideBoundaries"] <- "NOT FOUND"
  #         }
  #       }
  #       # now overwrite anything if there is a FALSE found
  #       for(i in 1:nrow(unique_sites)){
  #         if(unique_sites$alert[i] == "FALSE"){
  #           row_index <- which(df$Deployment.Location == unique_sites$Deployment.Location[i])
  #           df[row_index, "InsideBoundaries"] <- "FALSE"
  #           df[row_index, "Species"] <- paste(df[row_index, "Species"], unique_sites$binomial[i])  # ALEX: NEEDS IMPROVEMENT - WHAT IF THERE ARE TOO MANY REPETEAD SPECIES
  #         }
  #       }
  #       unique_sites <- df
  #     }
  #     else{
  #       unique_sites <- c()
  #     }
  #     
  #     if(nrow(in.dat)<1){ # this is never displayed, it only helps to avoid error at the very beginning (the rows of in.dat are 0 which doesn't help the view) 
  #       tmap <- leaflet(unique_sites) %>%
  #         addTiles(
  #           urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}",
  #           attribution = "Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC"
  #         )  %>% setView(-122.6,37.9,zoom=10)  
  #       
  #       #addProviderTiles("Thunderforest.Outdoors") # Like this one but let's integrate above.
  #     } else {
  #       # Allows dynamic view based on protected species/site selected
  #       # it takes the mean position of the selected species - if none was selected, considers all the species
  #       tmap <- leaflet(unique_sites) %>%
  #         addTiles(
  #           urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}",
  #           attribution = "Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC"
  #         )  %>% setView(mean(in.dat$Longitude),mean(in.dat$Latitude),zoom=10) 
  #     }
  #     
  #     # palette for the three cases: true (inside boundaries), false (outside boudaries), or not found.
  #     pal <- colorFactor(c("black", "red", "blue"), 
  #                        levels = c("TRUE", "FALSE", "NOT FOUND"))
  #     
  #     if (nrow(mapping_dataset.2())>0) {
  #       # print(head(unique_sites,30))
  #       tmap <- tmap %>%
  #         addCircleMarkers(~Longitude, ~Latitude, layerId=NULL, weight=2, radius=4,
  #                          color=~pal(InsideBoundaries), fillOpacity=1, 
  #                          popup = ~paste("Deployment ID:", Deployment.Location, 
  #                                         "<br>Species out of boundaries:", Species)) %>%  # alert
  #         addLegend(position = "topleft", title = "Legend",
  #                   labels=c("Inside boundaries","Outside boundaries", "Not found in shapefiles"), colors=c("black", "red", "blue"))
  #     }
  #     
  #     # Park Boundary Checkbox === looks like it doesn't work
  #     if (input$boundary_checkbox.2 == TRUE) {
  #       tmap <- tmap %>% 
  #         addPolygons(data = MCPparks, weight = 2, fill=FALSE) %>%
  #         addPolygons(data = GGNRA_incChedaJewel, weight = 2, fill=FALSE) %>%
  #         addPolygons(data = MMWD, weight = 2, fill=FALSE) %>%
  #         addPolygons(data = SamuelPTaylor, weight = 2, fill=FALSE)
  #     }
  #     # show boundaries for selected species (and if you click on them, it will tell you which specie)
  #     if (length(input$species.2)>0) {
  #       selected_polygons = terrestrial_mammals[terrestrial_mammals$binomial %in% input$species.2,]
  #       pal2 <- colorFactor(heat.colors(8), selected_polygons$binomial)
  #       tmap <- tmap %>%
  #         addPolygons(data = selected_polygons, weight = 5, fill=FALSE,
  #                     color=~pal2(binomial),
  #                     popup = ~paste("Specie:", binomial))
  #     }
  #   }
  #   tmap
  #   
  # })
  # 
  # # Update species selection based on RED and guild
  # # TODO: The if/else logic here could be cleaned up
  # observe({
  #   #Modify selection based on nulls
  #   if (is.null(input$red.2) & is.null(input$guild.2)) {
  #     selected.names <- NULL
  #   } else if (is.null(input$red.2)) {
  #     trows <- as.character(present.species()$guild) %in% input$guild.2
  #     selected.species <- present.species.2()[trows,]
  #     selected.names <- paste(selected.species$genus, selected.species$species)
  #   } else if (is.null(input$guild.2)) {
  #     trows <- as.character(present.specie.2()$red_list_status_id) %in%
  #       red.list.table$id[red.list.table$description %in% input$red.2]
  #     selected.species <- present.species.2()[trows,]
  #     selected.names <- paste(selected.species$genus, selected.species$species)
  #   } else {
  #     guilds <- (as.character(present.species.2()$guild) %in% input$guild.2)
  #     
  #     reds <- as.character(present.species()$red_list_status_id) %in%
  #       red.list.table$id[red.list.table$description %in% input$red.2]
  #     if (is.null(guilds) & is.null(reds)) {
  #       selected.names <- NULL
  #     } else if (is.null(guilds)) {
  #       selected.species <- present.species.2()[reds,]
  #       selected.names <- paste(selected.species$genus, selected.species$species)
  #     } else if (is.null(reds)) {
  #       selected.species <- present.species.2()[guilds,]
  #       selected.names <- paste(selected.species$genus, selected.species$species)
  #     } else {
  #       selected.species <- present.species()[guilds & reds,]
  #       selected.names <- paste(selected.species$genus, selected.species$species)
  #     }
  #     
  #   }
  #   
  #   # Update species selection menu       
  #   selected.names <- sort(as.character(selected.names))
  #   
  #   updateSelectInput(session, "species.2", "Select Species (Multiple Possible)",
  #                     choices=sort(as.character(present.species.names.2())), 
  #                     selected=selected.names)
  #   
  # })
  # 
  # # Subset dataframe for plotting (no time subset)
  # # Subset by project, site, frequency, and selected species
  # plotting_dataset.2 <- reactive({
  #   if (!is.null(input$species.2)) {
  #     subset(site_selection.2(), (Sampling.Type==input$select_time.2) & (paste(Genus, Species) %in% input$species.2))
  #   } else {
  #     data.frame()
  #   }
  # })
  # 
  # # Subset dataframe for mapping (time subset)
  # # Subset by project, site, frequency, selected species, current time
  # mapping_dataset.2 <- reactive ({
  #   if (!is.null(input$species.2) & (!is.null(input$time_slider.2))) {
  #     subset(plotting_dataset.2(), timestamp==input$time_slider.2)
  #   } else {
  #     #subset(plotting_dataset(), rep(FALSE, times=nrow(plotting_dataset())))
  #     data.frame()
  #   }
  # })
  # 
  # # Subset dataframe for plotting based on map click 
  # # This is subsetted on camera and speecies. Possibly species could be removed from filter.
  # camera_dataset.2 <- reactive ({
  #   subset(site_selection.2(), Deployment.Location.ID==values.2$clickedMarker$id & (paste(Genus, Species) %in% input$species.2))
  # })
  # 
  # # observe the marker click info and change the value depending on click location 
  # observeEvent(input$map_marker_click.2,{
  #   values.2$clickedMarker <- input$map_marker_click.2
  # }
  # )
  # observeEvent(input$map_click.2,{
  #   values.2$clickedMarker$id <- NULL
  # })
  # observeEvent(input$species.2, {
  #   values.2$clickedMarker$id <- NULL
  # })
  # 


###########################################
############ Species Spotter Tab ##########
###########################################
# Read Data into reactive for flexiblity in using other datasets
dataset_input_occ <- reactive({
  occ <- rename_cols(as.data.frame(fread("./data/marin_species_occurence.csv")))
  occ
})

# Render Site Checkbox to select region
output$site_checkbox_occ <- renderUI({
  labels <- as.character(unique(dataset_input_occ()$Project.ID))
  labels <- c(labels, "All Sites")
  selectInput("site_selection_occ", "Select Sites/Subregions", choices = labels, selected = labels[[1]])
})

## Check if "All Sites" is the selected site
check_allsites_occ <- reactive({
  input$site_selection_occ == "All Sites" %>%
    return()
})


# Reactive function to select site
site_selection_occ <- reactive({
  print(check_allsites_occ())


  if(req(input$site_selection_occ) == "All Sites"){
    dataset_input_occ()
  } else{

    subset(dataset_input_occ(), as.character(Project.ID) %in% input$site_selection_occ)
  }

})

# Create reactive data.frame containing only species present in selected sites
# in selected project area
present.species_occ <- reactive({
  species <- unique(site_selection_occ()[c("Genus", "Species")])
  present.species_occ <- species.table[species.table$genus %in% species$Genus &
                                     species.table$species %in% species$Species,]
  # Switch out "" with "Unknown"
  present.species_occ$guild <- as.factor(ifelse(as.character(present.species_occ$guild) == "", "Unknown", 
                                                as.character(present.species_occ$guild)))
  
  present.species_occ
})

# Render guild selector
output$guild.control_occ <- renderUI({
  guild.list <- sort(unique(as.character(present.species_occ()$guild)))
  checkboxGroupInput("guild_occ", "Select Guilds", choices=guild.list,
                     selected=NULL)
})


# Render species selection
output$species.list_occ <- renderUI({
  selectInput("species_occ", "Select Species (Multiple Possible)",
              choices=sort(as.character(site_selection_occ()$Genus.Species)), selected=NULL, multiple=TRUE)
})


## Interactive Map for Species Spotter Tab ####

#Create the map
output$map_occ <- renderLeaflet({
  # Prepare data
  if (!values.2$starting) {
    if (nrow(species_dataset_occ())>0) {
      species_sites <-  unique(select(site_selection_occ(), Deployment.Location.ID, Latitude, Longitude))
      species_sites$present <- ifelse(species_sites$Deployment.Location.ID %in% 
                                        species_dataset_occ()$Deployment.Location.ID, "Y", "N")
      #species_sites <- left_join(species_sites, select(species_dataset_occ, Deployment.Location.ID, n), by = Deployment.Location.ID)
      pal <- colorFactor(c("black", "red"), domain = c("N", "Y"))
      tmap_occ <- leaflet(species_sites) %>%
        addTiles(
          urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}",
          attribution = "Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC"
        )  %>% 
        setView(mean(species_dataset_occ()$Longitude),mean(species_dataset_occ()$Latitude),zoom=10) %>%
        addCircleMarkers(~Longitude, ~Latitude, layerId=NULL, weight=2, radius=4, fillOpacity=1, color = ~pal(present)
                         #popup = ~paste("Deployment ID:", Deployment.Location,
                         #"<br>Species out of boundaries:", Species)
                         )
    } else{
      tmap_occ <- leaflet() %>%
        addTiles(
          urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}",
          attribution = "Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, 
                        NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC"
        )  %>% setView(-122.6,37.9,zoom=10) 
    }
  }
  tmap_occ
})

# Update species selection based on RED and guild
# TODO: The if/else logic here could be cleaned up
observe({
  #Modify selection based on nulls
  if (is.null(input$guild_occ)) {
    selected.names_occ <- NULL
  } else {
    trows <- as.character(present.species_occ()$guild) %in% input$guild_occ
    selected.names_occ <- present.species_occ()[trows,]
    selected.names_occ <- paste(selected.names_occ$genus, selected.names_occ$species)
  } 
  # Update species selection menu       
  selected.names_occ <- sort(as.character(selected.names_occ))
  
  updateSelectInput(session, "species_occ", "Select Species (Remove all but one)",
                    choices=sort(as.character(site_selection_occ()$Genus.Species)), 
                    selected=selected.names_occ)
  
})

# Subset dataframe for plotting (no time subset)
# Subset by project, site, selected species
species_dataset_occ <- reactive({
  if (!is.null(input$species_occ)) {
    subset(site_selection_occ(), (paste(Genus, Species) %in% input$species_occ))
  } else {
    data.frame()
  }
})

# Subset dataframe for plotting based on map click
# This is subsetted on camera and speecies. Possibly species could be removed from filter.
camera_dataset_occ <- reactive ({
  subset(site_selection_occ(), Deployment.Location.ID==values.2$clickedMarker$id & (paste(Genus, Species) %in% input$species_occ))
})

# observe the marker click info and change the value depending on click location
observeEvent(input$map_marker_click_occ,{
  values.2$clickedMarker <- input$map_marker_click_occ
}
)
observeEvent(input$map_click_occ,{
  values.2$clickedMarker$id <- NULL
})
observeEvent(input$species_occ, {
  values.2$clickedMarker$id <- NULL
})

})

