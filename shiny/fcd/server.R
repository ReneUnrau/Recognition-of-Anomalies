# server.R

shinyServer(function(input, output, session) {
  
  # create leaflet map
  map <- createLeafletMap(session, "map")
  
  
  # output selected dates:
  output$selectedDates <- renderText({
    paste("First date: ", input$dates[1], "\n Second date: ", input$dates[2])
  })
  
  # output the selected track:
  output$selectedTracksList <- renderText({ 
    paste("Your selection: ", input$tracksList)
  })
  
  # output the boundingbox:
  output$boundingBoxText <- renderText({ 
    NE_LAT <- paste("NE_LAT", input$map_bounds[1])
    NE_LON <- paste("NE_LON", input$map_bounds[2])
    SW_LAT <- paste("SW_LAT", input$map_bounds[3])
    SW_LON <- paste("SW_LON", input$map_bounds[4])
    
    HTML(paste(NE_LAT, NE_LON, SW_LAT, SW_LON, sep = '<br/>'))
    
  })

})