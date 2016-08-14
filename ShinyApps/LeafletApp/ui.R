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
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Rates of detection"),
        selectInput("dataset", "Camera Trap Project", c("TEST!")),
        uiOutput("site_checkbox"),
        radioButtons("humans", "Show Humans?", c("Humans", "No Humans"),
                    selected="No Humans"),
        uiOutput("guild.control"),
        uiOutput("red.control"),
        uiOutput("species.list"),
        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "adultpop"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                         # Only prompt for threshold when coloring or sizing by superzip
                         numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        ),
        selectInput(inputId = "samplingFrequency",
                    label = "Sampling Frequency",
                    choices = samplingFrequency),
        checkboxInput(inputId = "show_human",
                      label = "Show Human Activities?"),
        plotOutput("ts_plot", height = 200),
        plotOutput("agg_ts_plot", height = 200),
        plotOutput("decompose_ts_plot", height = 200)
      ),

      tags$div(id="cite",
        'Data compiled for ', vars['Data source']
      )
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
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
