# draw whole track with blue circles - indices used as shape-id
drawTrack = function(track, map){
  coordinates = track@sp@coords
  map$clearShapes()
  for(i in 1:nrow(coordinates)){
    latitude <- as.numeric((coordinates[i,2]))
    longitude <- as.numeric((coordinates[i,1]))
    
    map$addCircle(latitude, longitude, 5, toString(i))
  }  
}

#draw single measurements (anomalies) with markers - indices used as marker-id
drawMarkers = function(indices, track, map){
  map$clearMarkers()
  coordinates = track@sp@coords
  for(i in indices){
    latitude <- as.numeric((coordinates[i,2]))
    longitude <- as.numeric((coordinates[i,1]))
    map$addMarker(latitude, longitude, toString(i))
  }
}

getPopUpContent <- function(index, track){
  # traffic signals have an ID >= 10000
  if(index >= 10000){
    content <- as.character(tagList(
      tags$h4("Traffic Light: ", index)
    ))
  # it must be a track ID otherwise
  } else {
    content <- as.character(tagList(
      tags$h4("Trackelement: ", index),
      sprintf("CO2: %s", track@data$CO2[index]), tags$br(),
      sprintf("Speed: %s", track@data$Speed[index]), tags$br(),
      sprintf("RPM: %s", track@data$Rpm[index]), tags$br(),
      sprintf("GPS Speed: %s", track@data$GPS.Speed[index]), tags$br(),
      sprintf("GPS Accuracy: %s", track@data$GPS.Accuracy[index]), tags$br(),
      sprintf("GPS Bearing: %s", track@data$GPS.Bearing[index]), tags$br(),
      sprintf("GPS HDOP: %s", track@data$GPS.HDOP[index]), tags$br(),
      sprintf("GPS PDOP: %s", track@data$GPS.PDOP[index]), tags$br(),
      sprintf("GPS VDOP: %s", track@data$GPS.VDOP[index]), tags$br(),
      sprintf("MAF: %s", track@data$MAF[index]), tags$br(),
      sprintf("Calculated MAF: %s", track@data$Calculated.MAF[index]), tags$br(),
      sprintf("Engine Load: %s", track@data$Engine.Load[index]), tags$br(),
      sprintf("Throttle Position: %s", track@data$Throttle.Position[index]), tags$br(),
      sprintf("Intake Pressure: %s", track@data$Intake.Pressure[index]), tags$br(),
      sprintf("Intake Temperature: %s", track@data$Intake.Temperature[index]), tags$br()
    ))
  }
  content
}

# zoom to starting point of track
zoomToStart = function(track, map){
  
  map$clearMarkers()
  coordinates = track@sp@coords
  lat <- as.numeric((coordinates[1,2]))
  lng <- as.numeric((coordinates[1,1]))
  map$setView(lat, lng, 16, forceReset = FALSE)
  map$addCircleMarker(lat, lng, 10, toString(1), list(color='#00FF91'))
  
}

# zoom to finish point of track
zoomToFinish = function(track, map){
  
  map$clearMarkers()
  coordinates = track@sp@coords
  lastMeasurement <- nrow(coordinates)
  lat <- as.numeric((coordinates[lastMeasurement,2]))
  lng <- as.numeric((coordinates[lastMeasurement,1]))
  map$setView(lat, lng, 16, forceReset = FALSE)
  map$addCircleMarker(lat, lng, 10, toString(lastMeasurement), list(color='#00FF91'))
  
}

# center the track
centerTrack = function(track, map){
  
  map$clearMarkers()
  coordinates = track@sp@coords
  lastMeasurement <- nrow(coordinates)
  # lower left coordinate
  lat_ll <- as.numeric((coordinates[1,2]))
  lng_ll <- as.numeric((coordinates[1,1]))
  # upper right: 51.985515, 7.674909
  lat_ur <- as.numeric((coordinates[lastMeasurement,2]))
  lng_ur <- as.numeric((coordinates[lastMeasurement,1]))
  map$fitBounds(lat_ll, lng_ll, lat_ur, lng_ur)
  
}
