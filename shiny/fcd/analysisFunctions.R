# load sp for traffic signals anomaly
require (sp)

displayNeighborAnomalies = function (track, attr, map) {
  # TO DO ->add function to renes drawer function, add checkbox for visualization of anomalies
  data <- switch(attr, 
                 "CO 2" = track@data$CO2,
                 "Calculated MAF" = track@data$Calculated.MAF,
                 "Engine Load" = track@data$Engine.Load,
                 "GPS Accuracy" = track@data$GPS.Accuracy,
                 "GPS HDOP" = track@data$GPS.HDOP,
                 "GPS PDOP" = track@data$GPS.PDOP,
                 "GPS Speed" = track@data$GPS.Speed,
                 "GPS VDOP" = track@data$GPS.VDOP,
                 "Intake Pressure" = track@data$Intake.Pressure,
                 "Intake Temperature" = track@data$Intake.Temperature,
                 "MAF" = track@data$MAF,
                 "RPM" = track@data$Rpm,
                 "Speed" = track@data$Speed,
                 "Throttle Position" = track@data$Throttle.Position)
  
  # first draft: get biggest difference
  biggestDiff = 0
  index = 0
  for(i in 1:(length(data)-1)){
    diff <- abs(data[i] - data[i+1])
    if (diff > biggestDiff)
      biggestDiff = diff
    index = i
  }
  
  drawMarkers(index, track, map)
}


findOutliers = function (track, attr, map) {
  # TO DO ->add function to renes drawer function, add checkbox for visualization of anomalies
  data <- switch(attr, 
                 "CO2" = track@data$Co2,
                 "Calculated MAF" = track@data$Calculated.MAF,
                 "Engine Load" = track@data$Engine.Load,
                 "GPS Accuracy" = track@data$GPS.Accuracy,
                 "GPS HDOP" = track@data$GPS.HDOP,
                 "GPS PDOP" = track@data$GPS.PDOP,
                 "GPS Speed" = track@data$GPS.Speed,
                 "GPS VDOP" = track@data$GPS.VDOP,
                 "Intake Pressure" = track@data$Intake.Pressure,
                 "Intake Temperature" = track@data$Intake.Temperature,
                 "MAF" = track@data$MAF,
                 "RPM" = track@data$Rpm,
                 "Speed" = track@data$Speed,
                 "Throttle Position" = track@data$Throttle.Position)
  
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
  print("Number of outliers:")
  print(length(indices))
  
  # Draw corresponding measurements as marker on Map
  drawMarkers(indices, track, map)
}


#load the traffic signals of the district of Muenster
traffic_signals = read.csv("traffic_signals.csv", sep = ";")
coordinates(traffic_signals) = ~x+y

# function to check whether there are unexpected stops that cannot be related to fraffic signals (>50m away)
findTrafficSignalAnomalies = function (track, map) {
  # save indices of anomalies
  indices = c()
  # check if the speed is equal to 0 at any point (ignoring NA values and first / last point in a track)
  for(i in 2:length(track)-1)
    if(track@data$Speed[i] == 0 && !is.na(track@data$Speed[i])){
      # check the distance to the next traffic signal
      distToTrafficSignals = spDistsN1(pts = traffic_signals@coords, pt = track@sp@coords[i,], longlat = T)
      # get the min distance
      minDist = min(distToTrafficSignals)
      # check if that distance is > 50m (0.05km)
      if(minDist > 0.05){
        # add index to anomalies
        indices = c(indices, i)
      }
    }
  drawMarkers(indices, track, map)
}

# function to check whether there are unexpected car turns (i.e. no normal left/right turns)
# author: Daniel Sawatzky
findTurnAnomalies = function (track, map) {
  
  # get bearing of car
  bearing <- track@data$GPS.Bearing
  
  # save indices of anomalies
  indices = c()
  
  # check if the bearing difference is greater than 90 deg (ignoring NA values and first / last point in a track)
  for(i in 2:length(track)-1)
    bearing_diff = bearing[i] - bearing[i-1]
    if(bearing_diff > 90 && !is.na(bearing[i])){
      # add index to anomalies
      indices = c(indices, i)
    }
  drawMarkers(indices, track, map)
  
}
