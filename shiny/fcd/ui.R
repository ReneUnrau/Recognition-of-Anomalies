# ui (from tutorial)

library(shiny)
library(leaflet)

shinyUI(fluidPage(
  titlePanel("Filter options"),
  
  sidebarLayout(
    sidebarPanel(
      
      # select dates
      dateRangeInput("dates", label = "Date range", start = as.Date("2015-01-02")),
      fluidRow(column(12, verbatimTextOutput("selectedDates"))),
      
      # select boundingbox
      checkboxInput("checkbox", label = "Consider current map as boundingbox", value = TRUE),
      fluidRow(column(12, verbatimTextOutput("boundingBoxText"))),
      
      # select limit
      sliderInput("limit_slider", label = "Select the number of tracks", min = 1, max = 100, value = 2),
      fluidRow(column(4, verbatimTextOutput("limit"))),
      
      # start the search for tracks
      actionButton("search_btn", label = "Search"),
      
      # select a specific track
      uiOutput("tracks"),
      fluidRow(column(10, verbatimTextOutput("selectedTracksList"))),
      
      # start the search for tracks
      actionButton("anomalies_btn", label = "Show Anomalies")
      
    ),
    
    mainPanel(
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
     
    )
  )
))