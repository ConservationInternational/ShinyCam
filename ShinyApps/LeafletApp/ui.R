library(raster)
library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop",
  "Data source" = "TEAM"
)

samplingFrequency <- c(
    "Annual" = "annual",
    "Seasonal" = "season",
    "Monthly" = "monthly"
    )

shinyUI(navbarPage("Rates of detection", id="nav",

##   Tab for Interactive Map
  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      #chartOutput("baseMap", "leaflet"),
      leafletOutput("map", width="100%", height="100%"),
      #tags$style('.leaflet {height: 100%; width: 100%;}'),
      #tags$head(tags$script(src="http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js")),
      #uiOutput('heatMap'),
      
      # Portion of side panel menu always present.
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 60, left = "auto", right = 20, bottom = 10,
        width = 330, height = "auto", style = "overflow-y:scroll",

        h2("Rates of detection"),

      	selectInput("dataset", "Camera Trap Project", c("TEAM", "MWPIP")),
        uiOutput("site_checkbox"),
      	#####     SLIDER

      #   # TODO: Make this reactive based on frequencies present in input data
      # 	selectInput("select_time", label = "Sampling Frequency",
      # 	            choices = list("Annual" = 'annual', "Seasonal" = 'seasonal', "Monthly" = 'monthly'),
      # 	            selected = 1),
      #
      #   # TODO: Make this into a reactive UI based on min() and max() of actual data for sites.
      # 	sliderInput("time_slider", label = "Select Time", min = 0,
      # 	            max = 100, value = 50, timeFormat = "%Y-%m-%d"),
        #radioButtons("humans", "Show Humans?", c("Humans", "No Humans"),
        #            selected="No Humans"),
      
      
        checkboxInput("boundary_checkbox", label = "Display Park Boundaries", value = FALSE),
      
        uiOutput("guild.control"),
        uiOutput("red.control"),
        uiOutput("species.list"),
        uiOutput("frequency.control"),
        uiOutput("time.control"),
        hr(),
        #uiOutput("time.selection")#,
        # selectInput("color", "Color", vars),
        # selectInput("size", "Size", vars, selected = "adultpop"),
        # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
        #                  # Only prompt for threshold when coloring or sizing by superzip
        #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        # ),
        # selectInput(inputId = "samplingFrequency",
        #             label = "Sampling Frequency",
        #             choices = samplingFrequency),
        # checkboxInput(inputId = "show_human",
        #               label = "Show Human Activities?"),
        # plotOutput("histCentile", height = 200),
                                        # plotOutput("scatterCollegeIncome", height = 250)
      
        # Portion of side panel menu that appears at bottom after species have been selected.
        conditionalPanel(
          condition = 'input.species != null',
          #plotOutput("histCentile", height = 200),
          #plotOutput("scatterCollegeIncome", height = 250),
          h3("Site-Specific Plots"),
          h4("Time Series of Site-Wide Rate of Detection"),
          plotOutput("total_ts", height = 200),
          h4("Top 5 Genera by Rate of Detection"),
          plotOutput("top_five_plot", height = 200),

          #h4("Overall Trends"),
          #plotOutput("health_ts", height = 200),
        #hr(),
        h3("Camera Specific Plots"),
        h4("(Click a red point on the map to enable)"),
        plotOutput("camera_ts_benchmark", height = 200),
        plotOutput("camera_ts_benchmark_facet", height = 200)
      )
        
      ),

      # Left# Portion of side panel always present.
      tags$div(id="cite",
        'Data compiled for ', vars['Data source']
      )
    )
  ),

##   Tab for Data Explorer
  tabPanel("Data explorer",
    fluidRow(
      column(1,
        downloadButton('downloadData', 'Download')
      )
    ),
    hr(),
    DT::dataTableOutput("table")
  ),

  

##   Tab for Operational Statistics
##   NOTE: ADD OPERATIONAL STATS FEATURES TO THE UI HERE
     tabPanel("Operational stats"),


##   Tab for Administrative Statistics
##   NOTE: ADD ADMIN STATS FEATURES TO THE UI HERE
     tabPanel("Administrative stats"),

conditionalPanel("false", icon("crosshair"))
))
