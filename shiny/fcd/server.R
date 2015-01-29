# server.R

source ("import.R")

# Functions
displayNeighborAnomalies = function (track, attr, map) {
  # TO DO ->add function to renes drawer function, add checkbox for visualization of anomalies
  data <- switch(attr, 
                 "Co2" = track@data$CO2,
                 "Calculated.MAF" = track@data$Calculated.MAF,
                 "Engine.Load" = track@data$Engine.Load,
                 "GPS.Accuracy" = track@data$GPS.Accuracy,
                 "GPS.HDOP" = track@data$GPS.HDOP,
                 "GPS.PDOP" = track@data$GPS.PDOP,
                 "GPS.Speed" = track@data$GPS.Speed,
                 "GPS.VDOP" = track@data$GPS.VDOP,
                 "Intake.Pressure" = track@data$Intake.Pressure,
                 "Intake.Temperature" = track@data$Intake.Temperature,
                 "MAF" = track@data$MAF,
                 "Rpm" = track@data$Rpm,
                 "Speed" = track@data$Speed,
                 "Throttle.Position" = track@data$Throttle.Position)
  
  # first draft: get biggest difference
  biggestDiff = 0
  index = 0
  for(i in 1:(length(data)-1)){
    diff <- abs(data[i] - data[i+1])
    if (diff > biggestDiff)
      biggestDiff = diff
      index = i
  }
  
  map$clearMarkers()
  tr_coordinates = track@sp@coords
  latitude <- as.numeric((tr_coordinates[index,2]))
  longitude <- as.numeric((tr_coordinates[index,1]))
  map$addMarker(latitude, longitude, toString(index))
}


findOutliers = function (track, attr, map) {
  # TO DO ->add function to renes drawer function, add checkbox for visualization of anomalies
  data <- switch(attr, 
                 "Co2" = track@data$Co2,
                 "Calculated.MAF" = track@data$Calculated.MAF,
                 "Engine.Load" = track@data$Engine.Load,
                 "GPS.Accuracy" = track@data$GPS.Accuracy,
                 "GPS.HDOP" = track@data$GPS.HDOP,
                 "GPS.PDOP" = track@data$GPS.PDOP,
                 "GPS.Speed" = track@data$GPS.Speed,
                 "GPS.VDOP" = track@data$GPS.VDOP,
                 "Intake.Pressure" = track@data$Intake.Pressure,
                 "Intake.Temperature" = track@data$Intake.Temperature,
                 "MAF" = track@data$MAF,
                 "Rpm" = track@data$Rpm,
                 "Speed" = track@data$Speed,
                 "Throttle.Position" = track@data$Throttle.Position)
  
  ex_low = c()
  ex_high = c()
  
  # Calculate lower and higher border of whiskers
  lower_border <- quantile(data, probs=0.25) - (1.5*IQR(data)) #Lower border for extremes
  upper_border <- quantile(data, probs=0.75) + (1.5*IQR(data)) #Upper border for extremes
  
  # Get values of outliers
  lows <- data[data<lower_border] #High extremes
  highs <- data[data>upper_border] #Low extremes
  
  # Get indices corresponding to those outliers
  indices_low <- which(data<lower_border)
  indices_high <- which(data>upper_border)
  
  # Merge indices to single array
  indices <- c(indices_low,indices_high)
  indices
  print("Number of outliers:")
  print(length(indices))
  
  # show track on map
  tr_coordinates = track@sp@coords
  map$clearShapes()
  for(i in 1:nrow(tr_coordinates)){
    latitude <- as.numeric((tr_coordinates[i,2]))
    longitude <- as.numeric((tr_coordinates[i,1]))
    
    map$addCircle(latitude, longitude, 5, toString(i))
  }  
  
  # Draw corresponding measurements as marker on Map
  map$clearMarkers()
  coordinates = track@sp@coords
  for(i in indices){
    latitude <- as.numeric((coordinates[i,2]))
    longitude <- as.numeric((coordinates[i,1]))
    
    map$addMarker(latitude, longitude, toString(i))
  }
}

changeTrackSelection = function(){
  # get length of current tracksCollection
  len = length(trCol@tracksCollection)
  numbers = c(1:len)
  tracknames = paste("track", numbers)
  # assign tracknames to tracksList
  selectInput("tracksList", 
              label = "Choose a Track to display",
              choices = tracknames,
              selected = tracknames[1])
}

create_bbox_from_input = function(ll, ur){
  return(matrix(c(ll, ur),ncol=2,dimnames=list(c("x","y"),c("min","max"))))
}

create_timeinterval_from_input = function(start, end){
  start = as.POSIXct(start)
  end = as.POSIXct(end)
  return(c(start,end))
}

# get the tracknumber from a trackname (i.e. "track 1" -> 1)
get_trackNumber = function(trackname){
  trackname = toupper(trackname)
  return(as.numeric(gsub("TRACK", "", trackname)))
}

# select a track from TracksCollection
get_track = function(tracksCollection, tracknumber){
  return(tracksCollection[tracknumber][1])
}

# create default trackcollection
stableURL = "https://envirocar.org/api/stable"
# create default bbox for Muenster
ll = c(7.6,51.95) # lower left : 51.928179, 7.573629
ur = c(7.65,52) # upper right: 51.985515, 7.674909
default_bbox = matrix(c(ll, ur),ncol=2,dimnames=list(c("x","y"),c("min","max")))
trCol = importEnviroCar(serverUrl = stableURL, bbox = default_bbox, limit = 1)

currentTrack <- 0

getPopUpContent <- function(index){
  content <- as.character(tagList(
    tags$h4("Trackelement: ", index),
    sprintf("CO2: %s", currentTrack@data$CO2[index]), tags$br(),
    sprintf("Calculated MAF: %s", currentTrack@data$Calculated.MAF[index]), tags$br(),
    sprintf("Engine Load: %s", currentTrack@data$Engine.Load[index]), tags$br(),
    sprintf("GPS Accuracy: %s", currentTrack@data$GPS.Accuracy[index]), tags$br(),
    sprintf("GPS HDOP: %s", currentTrack@data$GPS.HDOP[index]), tags$br(),
    sprintf("GPS PDOP: %s", currentTrack@data$GPS.PDOP[index]), tags$br(),
    sprintf("GPS Speed: %s", currentTrack@data$GPS.Speed[index]), tags$br(),
    sprintf("GPS VDOP: %s", currentTrack@data$GPS.VDOP[index]), tags$br(),
    sprintf("Intake Pressure: %s", currentTrack@data$Intake.Pressure[index]), tags$br(),
    sprintf("Intake Temperature: %s", currentTrack@data$Intake.Temperature[index]), tags$br(),
    sprintf("MAF: %s", currentTrack@data$MAF[index]), tags$br(),
    sprintf("Rpm: %s", currentTrack@data$Rpm[index]), tags$br(),
    sprintf("Speed: %s", currentTrack@data$Speed[index]), tags$br(),
    sprintf("Throttle Position: %s", currentTrack@data$Throttle.Position[index]), tags$br()
  ))
  content
}

# get initial track
currentTrack = get_track(trCol, 1)

shinyServer(function(input, output, session) {
  
  # create leaflet map
  map = createLeafletMap(session, "map")
  
    
  # change TracksCollection if search button is used
  observe({
    if (input$search_btn == 0) 
      return()
    isolate({
      time = create_timeinterval_from_input(input$dates[1], input$dates[2])
      print(paste("time -> ", time))
      limit = input$limit_slider
      print(paste("limit -> ", limit))
      # just get the bbox if the checkbox is clicked!
      if(input$checkbox[1]){
        bbox = create_bbox_from_input(c(input$map_bounds[4],input$map_bounds[3]), c(input$map_bounds[2],input$map_bounds[1]))
        print(paste("bbox -> ", bbox))
        trCol <<- importEnviroCar(serverUrl = stableURL, bbox = bbox, timeInterval = time, limit = limit)
      }
      # else do not consider the bbox
      else {
        trCol <<- importEnviroCar(serverUrl = stableURL, timeInterval = time, limit = limit)
      }
      print("Tracks loaded!")
      # make choosable tracks refering to trackscollection
      output$tracks = renderUI({
        changeTrackSelection()
      })
    })
  })
  
  # change currentTrack if another track is chosen
  observe({
    if(is.null(input$tracksList))
      return()
    isolate({
      print("observe currentTrack")
      # get number from track selection
      num = get_trackNumber(input$tracksList)
      currentTrack <<- get_track(trCol, num)
      
      # show track on map
      coordinates = currentTrack@sp@coords
      map$clearShapes()
      for(i in 1:nrow(coordinates)){
        latitude <- as.numeric((coordinates[i,2]))
        longitude <- as.numeric((coordinates[i,1]))
        
        map$addCircle(latitude, longitude, 5, toString(i))
      }
    })
  })
  
  # show Anomalies on map
  observe({
    if (input$anomalies_btn == 0) 
      return()
    
    isolate({
      chosenMethod <- input$analysis_method
      print(chosenMethod)
      if(chosenMethod == "Outliers"){
        findOutliers(currentTrack, input$anomalies_btn, map)
        
      } else if (chosenMethod == "Compare neighbors"){        
        displayNeighborAnomalies(currentTrack, input$anomalies_btn, map)
      }
    })
  })
  
  # output the selected track name:
  output$selectedTracksList = renderText({
    paste("Selected Track: ", "\n", input$tracksList, "\n First speed: ", "\n", currentTrack@connections$speed[1], "km/h")
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    # show selected track data
  })
  
  markerClickObs <- observe({
    map$clearPopups()
    event <- input$map_marker_click
    
    if (is.null(event)) {
      return ()
    }
    index <- as.integer(event$id)
    content <- getPopUpContent(index)
    map$showPopup(event$lat, event$lng, content)
  })
    
  shapeClickObs <- observe({
    map$clearPopups()
    event <- input$map_shape_click
    
    if (is.null(event)) {
      return ()
    }    
    index <- as.integer(event$id)
    content <- getPopUpContent(index)
    map$showPopup(event$lat, event$lng, content)
  })
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$plot expressions
  formulaText <- reactive({
    paste("time ~", input$attribute_selector)
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against time
  output$plot <- renderPlot({
    boxplot(as.formula(formulaText()), 
            data = #currentTrack@data@[input$attribute_selector]
              ,
            outline = highs)
  })
  
  summaryText <- reactive({
    summary(currentTrack)
  })
  
  # just the summary of the current track that is displayed
  output$log <- renderPrint({
    summaryText()
  })
})