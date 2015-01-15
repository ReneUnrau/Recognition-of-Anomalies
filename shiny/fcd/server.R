# server.R

source ("import.R")

# Functions
findOutliers = function (track, map) {
  # TO DO -> make function generice, add function to renes drawer function, add checkbox for visualization of anomalies
  data = track@data$GPS.Accuracy
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
  
  # Draw corresponding points on Map
  coordinates = track@sp@coords
  for(i in indices){
    latitude <- as.numeric((coordinates[i,2]))
    longitude <- as.numeric((coordinates[i,1]))
  
  # REMOVE LOOP AFTER PRESENTATION!!  
  for (i in 1:10){
    map$addCircle(latitude, longitude, 5, options = list(color = '#ff0000'))
    }
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

# get initial track
currentTrack = get_track(trCol, 1)

shinyServer(function(input, output, session) {
  
  # create leaflet map
  map = createLeafletMap(session, "map")
  
  # output selected dates:
  output$selectedDates = renderText({
    paste("Dates: \n First date: ", input$dates[1], "\n Second date: ", input$dates[2])
  })
  
  # output the boundingbox:
  output$boundingBoxText = renderText({
    if(input$checkbox[1]){
      TEXT = "Boundingbox:"
      NE_LAT = paste("NE_LAT", input$map_bounds[1])
      NE_LON = paste("NE_LON", input$map_bounds[2])
      SW_LAT = paste("SW_LAT", input$map_bounds[3])
      SW_LON = paste("SW_LON", input$map_bounds[4])
      paste(TEXT, "\n", NE_LAT, "\n", NE_LON, "\n", SW_LAT, "\n", SW_LON, "\n")
    } else {
      paste("BBox not selected!", "\n")
    }
  })
  
  # output the selected limit
  output$limit = renderPrint({ input$limit_slider })
  
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
        
        map$addCircle(latitude, longitude, 5)
      }
    })
  })
  
  # show Anomalies on map
  observe({
    if (input$anomalies_btn == 0) 
      return()
    isolate({
      findOutliers(currentTrack, map)
    })
  })
  
  # output the selected track name:
  output$selectedTracksList = renderText({
    paste("Selected Track: ", "\n", input$tracksList, "\n First speed: ", "\n", currentTrack@connections$speed[1], "km/h")
  })
  
  


})