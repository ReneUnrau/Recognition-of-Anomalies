# ui (from tutorial)

library(shiny)
library(leaflet)
library(shinythemes)

shinyUI(fluidPage( 

  #theme = "bootstrap.css",
  includeCSS("www/styles.css"),
  
  headerPanel("Recognition of Anomalies"),

  sidebarLayout(
    sidebarPanel(
      
      # Data chooser
      h4("Data Selection"),
      #icon("calendar"),
      
      # select dates
      dateRangeInput("dates", label = "Date range", start = as.Date("2015-01-02"), weekstart = 1,),
      
      
      # select boundingbox
      checkboxInput("checkbox_bb", label = "Consider current map as boundingbox", value = TRUE),
      
      
      # select limit
      sliderInput("limit_slider", label = "Select the number of tracks", min = 1, max = 100, value = 2),
      
      
      # start the search for tracks
      actionButton("search_btn", label = "Search", icon = icon("search")),
      br(), br(),
      
      # select a specific track
      uiOutput("tracks"),
      
      # select traffic signals
      checkboxInput("checkbox_signals", label = "Show traffic signals", value = FALSE),
      
      # Recognition of Anomalies - Analysis
      br(),
      h4("Anomaly Analysis"),
      
      selectInput("analysis_method", label="Choose method for analysis",
                  choices = list("Outliers", "Speed differences", "Compare neighbors", "Unexpected stops", 
                                 "Unexpected car turns"),
                  selected = "Outliers"),
      conditionalPanel(
        condition = "input.analysis_method == 'Speed differences'",
        selectInput("difference_selector", "Choose a threshold difference",list(1,2,3,4,5,6,7,8,9))
      ),
      
      # Select an attribute which serves as input for the analysis
      conditionalPanel(
        condition = "input.analysis_method == 'Outliers' || input.analysis_method == 'Compare neighbors'",
        selectInput("attribute_selector", label="Choose an attribute to display",
                    choices = list("CO2", "Speed", "RPM", "GPS Speed", "GPS Accuracy",
                                   "GPS HDOP", "GPS PDOP",  "GPS VDOP", 
                                   "MAF", "Calculated MAF", "Engine Load", "Throttle Position",
                                   "Intake Pressure", "Intake Temperature"
                    ),
                    selected = "CO2")
      ),
      
      # start the search for tracks
      actionButton("anomalies_btn", label = "Show Anomalies", icon = icon("bullseye")),
      
      # Text field for analysis message
      br(),br(),
      htmlOutput("analysis_message")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                   leafletMap(
                     "map", "100%", 500,
                     # By default OpenStreetMap tiles are used; we want nothing in this case
                     #initialTileLayer = NULL,
                     #initialTileLayerAttribution = NULL,
                     options=list(
                       center = c(51.96, 7.62),
                       zoom = 10,
                       maxBounds = list(list(17, -180), list(59, 180))
                     )
                   ),
                   icon = icon("globe"),
                   splitLayout(
                    cellWidths = c("auto", "auto", "auto"),
                    style = "margin-top: 10px; padding-bottom: 10px",
                    actionButton("showStart_btn", label = "Show start", icon = icon("step-backward")),
                    actionButton("centerTrack_btn", label = "Center track", icon = icon("compress")),
                    actionButton("showFinish_btn", label = "Show finish", icon = icon("step-forward"))
                   ),
                   flowLayout(
                    style = "border: 1px solid silver; padding-left: 15px;",
                    verticalLayout(
                      h4("Legend"),
                      splitLayout(
                        cellWidths = 200,
                        p(
                      p(span("Circle ", style = "color:blue"), ": Track Measurement"),
                      p(span("Marker ", style = "color:blue"), ": Anomaly")),
                      p(
                      p(span("Circle ", style = "color:#FF0040"), ": Traffic Light"),
                      p(span("Circle ", style = "color:#00FF91"), ": Start/Finish"))
                      )
                    )
                   )
                 ),
        tabPanel("Plot", h3(textOutput("caption")),  
                 selectInput("graphType", label="Choose a graph type to plot",
                             choices = list("Boxplot", "Bar", "Line", "Scatter"),
                             selected = "Boxplot"),
                 plotOutput("plot"), icon = icon("bar-chart-o") ),
        tabPanel("Table", dataTableOutput(outputId="table"), icon = icon("table")),
        tabPanel("Log", verbatimTextOutput("log"), icon = icon("code")),
        tabPanel("Info", imageOutput("logo", height = 100), htmlOutput("info_text") , icon = icon("info"))
      )
    )
  )
))