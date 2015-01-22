## Work with car data

# test to call function from another file
source("import.R")
require(rgdal)
require(RCurl)
require(RJSONIO)
require(stringr)
require(maptools)
require(spacetime)
require(trajectories)
require(sp)

stableURL = "https://envirocar.org/api/stable"
trackID = "545aa4f4e4b0b53890a2e62c"

# get single track
queryTracksCollection = importEnviroCar(stableURL, trackID)

## get the measurements
measurements = queryTracksCollection@tracksCollection$Tracks1@tracks$Track1@data
head(measurements)

## get the points
queryPoints = queryTracksCollection@tracksCollection$Tracks1@tracks$Track1@sp

## plot the points of the track
plot(queryPoints@coords, col="green")

## weekday filter (just some testing)
d = as.Date("2014-11-13")

# function to get the weekday from a date
printWeekday = function(date){
  weekdayNumber = as.POSIXlt(date)$wday # get the number of the weekday
  weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  weekdays[weekdayNumber]
}

print(printWeekday(d))

# testing bbox
ll = c(7.6,51.95) # lower left : 51.928179, 7.573629
ur = c(7.65,52) # upper right: 51.985515, 7.674909
boundingbox = matrix(c(ll, ur),ncol=2,dimnames=list(c("x","y"),c("min","max")))
boundingbox

# disable timeouts
setInternet2(use=NA)
setInternet2(use=FALSE)
setInternet2(use=NA)

#queryBoundingBox = importEnviroCar(serverUrl=stableURL, bbox=boundingbox) # loads all tracks from boundingbox

# get trackIDs from bbox
trIds = getTrackIDs(serverUrl=stableURL, bbox=boundingbox)
trIds

# just get the first 5 IDs
tr5 = trIds[1:5]
tr5

trCol5 = importEnviroCar(serverUrl = stableURL, trackIDs = tr5) # track 1 and 2 seem to be identical
plot(trCol5@tracksCollection$Tracks1@tracks$Track1@sp@coords, type="l", col="red")
lines(trCol5@tracksCollection$Tracks3@tracks$Track1@sp@coords, type="l", col="blue")
lines(trCol5@tracksCollection$Tracks4@tracks$Track1@sp@coords, type="l", col="green")
lines(trCol5@tracksCollection$Tracks5@tracks$Track1@sp@coords, type="l", col="yellow")

# get number of tracks in trackscollection
length(trCol5@tracksCollection)


trIdsTestLimit = getTrackIDs(serverUrl=stableURL, bbox=boundingbox, limit=5, verbose=T)
trIdsTestLimit

# test timeinterval
start = as.POSIXct(x = "2015-01-01")
end = as.POSIXct(x = "2015-01-12")
time_interval = c(start, end)
trIdsTestTimeLimit = getTrackIDs(serverUrl=stableURL, timeInterval = time_interval, limit=5, verbose=T)
trIdsTestTimeLimit

# get the tracknumber from a trackname (i.e. "track 1" -> 1)
get_trackNumber = function(trackname){
  trackname = toupper(trackname)
  return(as.numeric(gsub("TRACK", "", trackname)))
}

# test function
tr_num = get_trackNumber("Track2")

# select a track from TracksCollection
get_track = function(tracksCollection, tracknumber){
  return(tracksCollection[tracknumber][1])
}

# test if tracks work
tr1 = get_track(trCol5, 1)
tr2 = get_track(trCol5, 2)
plot(tr1@sp@coords, type="l", col="red")
lines(tr2@sp@coords, type="l", col="blue")

tr1@connections$speed[1]
tr2@connections$speed[1]

# Functions
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
    
    map$addCircle(latitude, longitude, 5)
  }  
  
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
findOutliers(tr1)
