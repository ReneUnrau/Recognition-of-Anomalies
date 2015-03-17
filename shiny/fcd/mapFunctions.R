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
      paste("CO2: ", track@data$CO2[index], "g/s"), tags$br(),
      paste("Speed: ", track@data$Speed[index], "km/h"), tags$br(),
      paste("RPM: ", track@data$Rpm[index], "u/min"), tags$br(),
      paste("GPS Speed: ", track@data$GPS.Speed[index], "km/h"), tags$br(),
      paste("GPS Accuracy: ", track@data$GPS.Accuracy[index], "m"), tags$br(),
      paste("GPS Bearing: ", track@data$GPS.Bearing[index], "°"), tags$br(),
      paste("GPS HDOP: ", track@data$GPS.HDOP[index]), tags$br(),
      paste("GPS PDOP: ", track@data$GPS.PDOP[index]), tags$br(),
      paste("GPS VDOP: ", track@data$GPS.VDOP[index]), tags$br(),
      paste("MAF: ", track@data$MAF[index], "l/s"), tags$br(),
      paste("Calculated MAF: ", track@data$Calculated.MAF[index], "g/s"), tags$br(),
      paste("Engine Load: ", track@data$Engine.Load[index], "%"), tags$br(),
      paste("Throttle Position: ", track@data$Throttle.Position[index], "V"), tags$br(),
      paste("Intake Pressure: ", track@data$Intake.Pressure[index], "kPa"), tags$br(),
      paste("Intake Temperature: ", track@data$Intake.Temperature[index], "C"), tags$br()
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
