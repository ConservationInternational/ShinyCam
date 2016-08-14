library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(rCharts)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

TEAM_data <- read.csv("data/TEAM_data.csv")

shinyServer(function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$baseMap <- renderMap({
    baseMap <- Leaflet$new() 
    baseMap$setView(c(0.6,-76) ,4) 
    baseMap$tileLayer(provider="Esri.WorldImagery")
    baseMap
  })
  
  #output$map <- renderLeaflet({
  #  leaflet() %>%
      #addTiles(
      #  urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #  attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      #) %>%
  #    addTiles(
  #        urlTemplate = "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
  #        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
  #    ) %>%
   #   setView(lng = -93.85, lat = 37.45, zoom = 4) 
  #})
  
  # Create heatmap overlay
  output$heatMap <- renderUI({
    #Create JSON like data from the lat long and intensity data
    
    j <- paste0("[",TEAM_data[,"Latitude.Resolution"], ",", TEAM_data[,"Longitude.Resolution"], ",", TEAM_data[,"Rate.Of.Detection"]/2, "]", collapse=",")
    j <- paste0("[",j,"]")
    j
    tags$body(tags$script(HTML(sprintf("
                                       var addressPoints = %s
                                       var heat = L.heatLayer(addressPoints).addTo(map)"
                                       , j
    ))))
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    hist(zipsInBounds()$centile,
      breaks = centileBreaks,
      main = "SuperZIP score (visible zips)",
      xlab = "Percentile",
      xlim = range(allzips$centile),
      col = '#00DD00',
      border = 'white')
  })

  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size

    if (colorBy == "superzip") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
      pal <- colorFactor("Spectral", colorData)
    } else {
      colorData <- zipdata[[colorBy]]
      pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)
    }

    if (sizeBy == "superzip") {
      # Radius is treated specially in the "superzip" case.
      radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    } else {
      radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    }

    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
        stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
        layerId="colorLegend")
  })

  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- allzips[allzips$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("Score:", as.integer(selectedZip$centile)),
      tags$strong(HTML(sprintf("%s, %s %s",
        selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      ))), tags$br(),
      sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
      sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
      sprintf("Adult population: %s", selectedZip$adultpop)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  
  # transform on sampling type
  keepCols <- c(
    'Project.ID', 'Deployment.Location.ID',
    'Latitude.Resolution', 'Longitude.Resolution',
    'Sampling.Type', 'Sampling.Period', 'Year',
    'Genus', 'Species', 'Rate.Of.Detection'
  )
  
  # TODO use filtered data
  subsettedData <- TEAM_data[keepCols]
  colnames(subsettedData) <- c(
    'Project', 'Deployment Location ID',
    'Latitude', 'Longitude',
    'Sampling Type', 'Sampling Period', 'Year',
    'Genus', 'Species', 'Rate Of Detection'
  )

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
})
