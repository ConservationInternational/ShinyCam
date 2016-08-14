library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(rCharts)
source("scripts/kernel_density_estimate.R")

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

#TEAM_data <- read.csv("data/TEAM_data.csv")
#TEAM_data$Longitude.Resolution <- TEAM_data$Longitude.Resolution + sample(-3:3)
#TEAM_data$Latitude.Resolution <- TEAM_data$Latitude.Resolution + sample(-3:3)

#Global
GRADIENT_SCALE <- 2
dat <- read.csv("data/rate_of_detection.csv")

shinyServer(function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  locs <- select(dat, Latitude, Longitude)
  cam_lat_longs <- unique(locs)
  
  output$map <- renderLeaflet({
  leaflet(cam_lat_longs) %>% 
    addTiles(
      urlTemplate = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>% 
    addCircleMarkers(~Longitude, ~Latitude, weight=2, radius=2, color="black", fillOpacity=1) %>% 
    addPolygons(data=dat_pgons$poly, color = brewer.pal(dat_pgons$nlev, "Greens")[dat_pgons$levs], stroke=FALSE)
  
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
#   observe({
#     colorBy <- input$color
#     sizeBy <- input$size
# 
#     if (colorBy == "superzip") {
#       # Color and palette are treated specially in the "superzip" case, because
#       # the values are categorical instead of continuous.
#       colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
#       pal <- colorFactor("Spectral", colorData)
#     } else {
#       colorData <- zipdata[[colorBy]]
#       pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)
#     }
# 
#     if (sizeBy == "superzip") {
#       # Radius is treated specially in the "superzip" case.
#       radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
#     } else {
#       radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
#     }
# 
#     leafletProxy("map", data = zipdata) %>%
#       clearShapes() %>%
#       addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
#         stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
#       addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
#         layerId="colorLegend")
#   })

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
  
  # TODO use filtered data as input
  subsettedData <- select(TEAM_data,
    'Project'=Project.ID, 'Deployment Location ID'=Deployment.Location.ID,
    'Latitude'=Latitude.Resolution, 'Longitude'=Longitude.Resolution,
    'Sampling Type'=Sampling.Type, 'Sampling Period'=Sampling.Period, 'Year'=Year,
    'Genus'=Genus, 'Species'=Species, 'Rate Of Detection'=Rate.Of.Detection
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
