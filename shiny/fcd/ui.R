# ui (from tutorial)

library(shiny)

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
      fluidRow(column(10, verbatimTextOutput("selectedTracksList")))
    ),
    
    mainPanel()
  )
))