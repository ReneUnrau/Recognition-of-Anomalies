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

})