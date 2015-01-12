# ui (from tutorial)

library(shiny)
library(leaflet)

currentTracksList = list("track1", "track2", "track3") # needs to be created from filter

shinyUI(fluidPage(
  titlePanel("Filter options"),
  
  sidebarLayout(
    sidebarPanel(
      
      # select dates
      dateRangeInput("dates", label = "Date range"),
      fluidRow(column(10, verbatimTextOutput("selectedDates"))),
      
      # start the search for tracks
      actionButton("search", label = "Search"),
      
      # select tracks from search
      selectInput("tracksList", 
                  label = "Choose a Track to display",
                  choices = currentTracksList,
                  selected = currentTracksList[1]),
      fluidRow(column(10, verbatimTextOutput("selectedTracksList"))),
      htmlOutput("boundingBoxText")
    ),
    
    mainPanel(
      leafletMap(
        "map", "100%", 500,
        # By default OpenStreetMap tiles are used; we want nothing in this case
        #initialTileLayer = NULL,
        #initialTileLayerAttribution = NULL,
        options=list(
          center = c(40, -98.85),
          zoom = 4,
          maxBounds = list(list(17, -180), list(59, 180))
        )
      )  
    )
  )
))