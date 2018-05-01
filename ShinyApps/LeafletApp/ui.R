# ShinyCam ui.R
#

library(raster)
library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Data source" = "ShinyCam"
)

samplingFrequency <- c("Annual" = "annual",
                       "Seasonal" = "season",
                       "Monthly" = "monthly")

shinyUI(navbarPage("ShinyCam",id = "nav",
    
    ##   Tab for Interactive Map
    tabPanel("Rates of Detection",
      #the sidebar includes all the subsetting rules
      sidebarPanel(
        selectInput("dataset", "Camera Trap Project", c("MWPIP")),
        ## THIS NEEDS TO BE CUSTOMIZED
        uiOutput('subsettingradio'),
        uiOutput("frequency.control"),
        uiOutput("site_checkbox"),
        uiOutput("guild.control"),
        uiOutput("red.control"),
        uiOutput("species.list"),
        width = 3),
      #the mainpanel is the detection rate map
      mainPanel(
        tabsetPanel(##   Tab for Interactive Map
          tabPanel("Map",
            div(class = "inner",
      #This style is intended to control the leaflet app dimensions
            tags$style(type = "text/css", "
              .inner {
              float: left;
              position: fixed;
              margin-left:-10px;
              width: 500px;
              height:650px;
              margin-top:10px;
              }"),
            tags$head(
              # Include our custom CSS
              includeCSS("styles.css"),
              includeScript("gomap.js")
            ),
            leafletOutput("map", width = "120%", height = "110%"),
            # Portion of side panel menu always present.
            # Shiny versions prior to 0.11 should use class="modal" instead.
            absolutePanel(
              id = "controls",
              class = "panel panel-default",
              fixed = TRUE,
              draggable = FALSE,
              top = 140,
              left = "auto",
              right = 100,
              bottom = 10,
              width = 300,
              height = 160,
              checkboxInput("boundary_checkbox", label = "Display Park Boundaries", value = FALSE),
              uiOutput("time.control")
            ),
            # Left# Portion of side panel always present.
              tags$div(id = "cite",
                     'Powered by ShinyCam' # , vars['Data source']
              )
            )
          ),
      #this subtab displays the data event table and allows it to be downloaded
      tabPanel("Data Event Table",
        fluidRow(
          column(1, br(),
            downloadButton('downloadData1', 'Download')
          )
        ),
        br(),
        fluidRow(HTML("The data event table shows all photograph events
            (after subsetting by the time interval) for each species.")),
        hr(),
        DT::dataTableOutput('dataeventtable')
      ),
      
      tabPanel("Detection Rate Table",
        fluidRow(column(1, br(),
          downloadButton('downloadData2', 'Download')
        )),
        br(),
        fluidRow(HTML("The detection rate table shows the number of data events per 100 trap nights for each species and for all camera sites within the selected Site/Subregion."
          )
        ),
        hr(),
        DT::dataTableOutput('datacapturerates')
      ),
      
      #this tab shows the detection rates plus all the sites without detections. NOt a useful visualization but is needed for mapping
      tabPanel("Detection Rate Table with Zeros",
        fluidRow(column(1, br(),
          downloadButton('downloadData3', 'Download')
        )),
        br(),
        fluidRow(
          HTML("The detection rate table includes the detection rates from the previous table and adds the sites that were deployed but did not detect the selected species at the camera site."
          )
        ),
        hr(),
        DT::dataTableOutput('datacaptureratesZeros')
        )
      )
    )
  ), #End main tab

# 
# #fluidRow(
# #div(class="outer",
# 
# #tags$head(
# # Include our custom CSS
# #        includeCSS("styles.css"),
# #        includeScript("gomap.js")
# #     ),
# #chartOutput("baseMap", "leaflet"),
# #      leafletOutput("map", width="100%", height="100%"),
# #tags$style('.leaflet {height: 100%; width: 100%;}'),
# #tags$head(tags$script(src="http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js")),
# #uiOutput('heatMap'),
# 
# # Portion of side panel menu always present.
# # Shiny versions prior to 0.11 should use class="modal" instead.
# #      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
# #        draggable = FALSE, top = 60, left = "auto", right = 20, bottom = 10,
# #        width = 330, height = "auto", style = "overflow-y:scroll",
# 
# #        h2("Rates of detection"),
# #
# #       	#selectInput("dataset", "Camera Trap Project", c("TEAM", "MWPIP")), #### EHF - ONE PROJECT AT A TIME
# #         selectInput("dataset", "Camera Trap Project", c("MWPIP")),
# #
# #         uiOutput("site_checkbox"),
# #
# #       	#####     SLIDER
# #
# #       #   # TODO: Make this reactive based on frequencies present in input data
# #       # 	selectInput("select_time", label = "Sampling Frequency",
# #       # 	            choices = list("Annual" = 'annual', "Seasonal" = 'seasonal', "Monthly" = 'monthly'),
# #       # 	            selected = 1),
# #       #
# #       #   # TODO: Make this into a reactive UI based on min() and max() of actual data for sites.
# #       # 	sliderInput("time_slider", label = "Select Time", min = 0,
# #       # 	            max = 100, value = 50, timeFormat = "%Y-%m-%d"),
# #         #radioButtons("humans", "Show Humans?", c("Humans", "No Humans"),
# #         #            selected="No Humans"),
# #
# #
# #         checkboxInput("boundary_checkbox", label = "Display Park Boundaries", value = FALSE),
# #
# #         uiOutput("guild.control"),
# #         uiOutput("red.control"),
# #         uiOutput("species.list"),
# #         uiOutput("frequency.control"),
# #         uiOutput("time.control"),
# #       width=3),
# #
# #       mainPanel(
# #         tabsetPanel(##   Tab for Interactive Map
# #           tabPanel("Map",
# #                    div(class="inner",
# #
# #                        #This style is intended to control the leaflet app dimensions
# #                        tags$style(type = "text/css", "
# #                                   .inner {
# #                                   float: left;
# #                                   position: fixed;
# #                                   margin-left:-150px;
# #                                   width: 400px;
# #                                   height: 50px;
# #                                   }"),
# #
# #                        tags$head(
# #                          # Include our custom CSS
# #                          includeCSS("styles.css"),
# #                          includeScript("gomap.js")
# #                        ),
# #
# #                        leafletOutput("map", width="120%", height="110%"),
# #
# #                        # Portion of side panel menu always present.
# #                        # Shiny versions prior to 0.11 should use class="modal" instead.
# #                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
# #                                      draggable = FALSE, top = 140, left = "auto", right = 100, bottom = 10,
# #                                      width = 300, height = 160,
# #                                      checkboxInput("boundary_checkbox", label = "Display Park Boundaries", value = FALSE),
# #                                      uiOutput("time.control")
# #         #uiOutput("time.selection")#,
# #
# #         # selectInput(inputId = "samplingFrequency",
# #         #             label = "Sampling Frequency",
# #         #             choices = samplingFrequency),
# #         # checkboxInput(inputId = "show_human",
# #         #               label = "Show Human Activities?"),
# #
# #
# #         # Portion of side panel menu that appears at bottom after species have been selected.
# #        # conditionalPanel(
# #         #  condition = 'input.species != null',
# #
# #          # h3("Site-Specific Plots"),
# #           #h4("Time Series of Site-Wide Rate of Detection"),
# #           #plotOutput("total_ts", height = 200),
# #           #h4("Top 5 Genera by Rate of Detection"),
# #           #plotOutput("top_five_plot", height = 200),
# #
# #           #h4("Overall Trends"),
# #           #plotOutput("health_ts", height = 200),
# #         #hr(),
# #         #h3("Camera Specific Plots"),
# #         #h4("(Click a red point on the map to enable)"),
# #         #plotOutput("camera_ts_benchmark", height = 200),
# #         #plotOutput("camera_ts_benchmark_facet", height = 200)
# #       #)
# #
# #       ),
# #
# #       # Left# Portion of side panel always present.
# #       tags$div(id="cite",
# #         'Data compiled for ', vars['Data source']
# #       )
# #     )
# #   )
# #   ),
#
# ##   Tab for Data Explorer
# #  tabPanel("Data explorer",
# #    fluidRow(
# #      column(1,
# #        downloadButton('downloadData', 'Download')
# #      )
# #    ),
# #    hr(),
# #    DT::dataTableOutput("table")
# #  ),


#############################
#### Tab for Camera Statistics Statistics
tabPanel("Camera stats",
         fluidRow(
           ##   Dropdown widgets
           column(
             3,
             selectInput(
               "selectStat",
               label = h4("Select Statistic"),
               choices = list(
                 "Count of images" = 1,
                 "Count of blank images" = 2,
                 "Count of unknown images" = 3,
                 "Count of uncatalogued images" = 4,
                 "Count of wildlife images" = 5,
                 "Count of human-related images" = 6,
                 "Average photos per deployment" = 7
               ),
               selected = 1
             ),
             
             selectInput(
               "selectAgg",
               label = h4("Select Aggregation Field"),
               choices = list(
                 "Project ID & Camera ID" = 1,
                 "Project ID" = 2,
                 "Camera ID" = 3
               ),
               selected = 1
             )
           ),
           ##   Data Table
           column(9,
                  DT::dataTableOutput("camtable"))
         )),

#############################
##    Tab for Species Alert
##    NOTE: This code is based on Interactive Map Tab
tabPanel(
  "Species alert",
  div(
    class = "outer",
    tags$head(# Include our custom CSS
      includeCSS("styles.css"),
      includeScript("gomap.js")),
    leafletOutput("map.2", width = "100%", height =
                    "100%"),
    # Portion of side panel menu always present.
    # Shiny versions prior to 0.11 should use class="modal" instead.
    absolutePanel(
      id = "controls.2",
      class = "panel panel-default",
      fixed = TRUE,
      draggable = FALSE,
      top = 60,
      left = "auto",
      right = 20,
      bottom = 10,
      width = 330,
      height = "auto",
      style = "overflow-y:scroll",

      h2("Species alert"),

      selectInput("dataset.2", "Camera Trap Project", c("TEAM", "MWPIP")),
      uiOutput("site_checkbox.2"),

      checkboxInput("boundary_checkbox.2", label = "Display Park Boundaries", value = FALSE),

      uiOutput("guild.control.2"),
      uiOutput("red.control.2"),
      uiOutput("species.list.2"),
      uiOutput("frequency.control.2"),
      uiOutput("time.control.2"),
      hr()
    ),

    # Left# Portion of side panel always present.
    tags$div(id = "cite2",
             #'Data compiled for ', vars['Data source']
             'Powered by ShinyCam')
  )
),
conditionalPanel("false", icon("crosshair")),

#############################
##    Tab for Species Spotter
##    NOTE: This code is based on Interactive Map Tab
tabPanel(
  "Species Spotter",
  sidebarPanel(
    h2("Species Spotter"),
    uiOutput("site_checkbox_occ"),
    checkboxInput("boundary_checkbox_occ", label = "Display Park Boundaries", value = FALSE),
    uiOutput("guild.control_occ"),
    uiOutput("species.list_occ"),
    width = 3,
    textOutput("timetext")),
  #the mainpanel is the detection rate map
  mainPanel(
    tabsetPanel(##   Tab for Interactive Map
      tabPanel("Map", div(class = "inner",
              #This style is intended to control the leaflet app dimensions
              tags$style(type = "text/css", "
                              .inner {
                              float: left;
                              position: fixed;
                              margin-left:-10px;
                              width: 500px;
                              height:650px;
                              margin-top:10px;
                              }"),
            tags$head(
              # Include our custom CSS
              includeCSS("styles.css"),
              includeScript("gomap.js")),

            leafletOutput("map_occ", width = "120%", height = "110%"),
            # Left# Portion of side panel always present.
            tags$div(id = "cite",'Powered by ShinyCam' # , vars['Data source']
            )
                    )

      ),
      #this subtab displays the data event table and allows it to be downloaded
      tabPanel("Species Spotter Data",
               DT::dataTableOutput('speciestable')
               ),
      tabPanel("Selected Camera Data",
               DT::dataTableOutput('camera_time_table')
      )
              )
            )
),
###########################
##    Tab for Temp Activity 
tabPanel("Temporal Activity",
         #the sidebar includes all the subsetting rules
         sidebarLayout(sidebarPanel(uiOutput("site_checkbox_temp"),
                                    uiOutput("guild.control_temp"),
                                    uiOutput("red.control_temp"),
                                    uiOutput("ta.species1"),
                                    uiOutput("ta.species2"),
                                    uiOutput("temporal_activity_time"),
                                    width=3),
                       
                       mainPanel(tabsetPanel(tabPanel("Species Kernel Density Plot(s)",
                                                      HTML("Select two species to fit a circular kernel density and 
                                                           evaluate temporal actvity via the methods of Ridout and 
                                                           Linkie (2009). If a species does not have a minimum of 
                                                           five data events, there will be no kernel analysis; 
                                                           there will be a rose plot. The temporal activity analysis 
                                                           uses the data event table. One can subset these data by 
                                                           sampling month or year. The default is to us all data 
                                                           from all months and years. In the next sub-tab, one can 
                                                           plot both species together; the title will display three 
                                                           estimates of overlap."),
                                                      br(),
                                                      fluidRow(column(1,plotOutput("tempact1", height = 400, width=800)))),
                                             
                                             tabPanel("Species Kernel Overlap Plot",
                                                      fluidRow(column(1,plotOutput("tempact2", height = 400, width=800)))),
                                             
                                             tabPanel("Rose Plots",
                                                      fluidRow(column(1,plotOutput("tempact3", height = 400, width=800))))),
                                 
                                 conditionalPanel("false", icon("crosshair"))	
                       )))

))# Close shiny