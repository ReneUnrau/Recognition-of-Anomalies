# ui (from tutorial)

library(shiny)
library(leaflet)

shinyUI(fluidPage(
  titlePanel("Recognition of Anomalies"),

  sidebarLayout(
    sidebarPanel(
      
      # Data chooser
      h4("Data Selection"),
      
      
      # select dates
      dateRangeInput("dates", label = "Date range", start = as.Date("2015-01-02")),
      
      
      # select boundingbox
      checkboxInput("checkbox_bb", label = "Consider current map as boundingbox", value = TRUE),
      
      
      # select limit
      sliderInput("limit_slider", label = "Select the number of tracks", min = 1, max = 100, value = 2),
      
      
      # start the search for tracks
      actionButton("search_btn", label = "Search", icon = icon("search")),
      
      # select a specific track
      uiOutput("tracks"),
      
      # select traffic signals
      checkboxInput("checkbox_signals", label = "Show traffic signals in the district of Muenster", value = FALSE),
      
      # Recognition of Anomalies - Analysis
      br(),
      h4("Anomaly Analysis"),
      
      # Select an attribute which serves as input for the analysis
      selectInput("attribute_selector", label="Choose an attribute to display",
                  choices = list("CO2", "Speed", "RPM", "GPS Speed", "GPS Accuracy",
                                 "GPS HDOP", "GPS PDOP",  "GPS VDOP", 
                                 "MAF", "Calculated MAF", "Engine Load", "Throttle Position",
                                 "Intake Pressure", "Intake Temperature"
                                 ),
                  selected = "Percent White"),
      
      selectInput("analysis_method", label="Choose method for analysis",
                  choices = list("Outliers", "Compare neighbors", "Unexpected stops", "Car turns"),
                  selected = "Outliers"),
      
      # start the search for tracks
      actionButton("anomalies_btn", label = "Show Anomalies", icon = icon("dot-circle-o")),
      
      # Text field for analysis message
      br(),
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
                   br(),
                   h4("Legend"),
                   p(span("Point ", style = "color:blue"), ": Track Measurement"),
                   p(span("Marker ", style = "color:blue"), ": Anomaly"),
                   p(span("Point ", style = "color:red"), ": Traffic Light")
                 ),
        tabPanel("Plot", h3(textOutput("caption")),  
                 selectInput("graphType", label="Choose a graph type to plot",
                             choices = list("Boxplot", "Bar", "Line", "Scatter"),
                             selected = "Boxplot"),
                 plotOutput("plot"), icon = icon("bar-chart-o") ),
        tabPanel("Table", tableOutput("table"), icon = icon("table")),
        tabPanel("Log", verbatimTextOutput("log"), icon = icon("code")),
        tabPanel("Info", textOutput("info_text"), icon = icon("info"))
      )
    )
  )
))