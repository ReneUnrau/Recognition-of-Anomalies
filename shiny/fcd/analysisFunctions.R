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
  
  drawMarkers(index, track, map)
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
  print("Number of outliers:")
  print(length(indices))
  # Draw corresponding measurements as marker on Map
  drawMarkers(indices, track, map)
}