# ui (from tutorial)

library(shiny)
library(leaflet)

shinyUI(fluidPage(
  titlePanel("Filter options"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Data chooser
      h4("Data Selection"),
      
      
      # select dates
      dateRangeInput("dates", label = "Date range", start = as.Date("2015-01-02")),
      
      
      # select boundingbox
      checkboxInput("checkbox", label = "Consider current map as boundingbox", value = TRUE),
      
      
      # select limit
      sliderInput("limit_slider", label = "Select the number of tracks", min = 1, max = 100, value = 2),
      
      
      # start the search for tracks
      actionButton("search_btn", label = "Search"),
      
      # select a specific track
      uiOutput("tracks"),
      fluidRow(column(10, verbatimTextOutput("selectedTracksList"))),
      
      # Recognition of Anomalies - Analysis
      br(),
      h4("Anomaly Analysis"),
      
      # Select an attribute which serves as input for the analysis
      selectInput("attribute_selector", label="Choose an attribute to display",
                  choices = list("Co2", "Calculated.MAF", "Engine.Load", "GPS.Accuracy",
                                 "GPS.HDOP", "GPS.PDOP", "GPS.Speed", "GPS.VDOP", 
                                 "Intake.Pressure", "Intake. Temperature",
                                 "MAF", "Rpm", "Speed", "Throttle.Position"),
                  selected = "Percent White"),
      
      selectInput("analysis_method", label="Choose method for analysis",
                  choices = list("Outliers", "Compare neighbors"),
                  selected = "Outliers"),
      
      # start the search for tracks
      actionButton("anomalies_btn", label = "Show Anomalies")
      
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
                   br(),
                   h4("Legend"),
                   p(span("Blue dot ", style = "color:blue"), ": Track Measurement"),
                   p(span("Red dot ", style = "color:red"), ": Outlier")
                 ),
        tabPanel("Plot", plotOutput("plot")), 
        tabPanel("Table", tableOutput("table")),
        tabPanel("Log", verbatimTextOutput("log"))
      )
    )
  )
))