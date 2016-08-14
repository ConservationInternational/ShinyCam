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

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 60, left = "auto", right = 20, bottom = 10,
        width = 330, height = "auto", style = "overflow-y:scroll",

        h2("Rates of detection"),
      	selectInput("dataset", "Camera Trap Project", c("TEAM")),
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
        uiOutput("guild.control"),
        uiOutput("red.control"),
        uiOutput("species.list"),
        uiOutput("frequency.control"),
        uiOutput("time.control")
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
      ),

      tags$div(id="cite",
        'Data compiled for ', vars['Data source']
      )
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(1,
        downloadButton('downloadData', 'Download')
      )
    ),
    hr(),
    DT::dataTableOutput("table")
  ),

  conditionalPanel("false", icon("crosshair"))
))
