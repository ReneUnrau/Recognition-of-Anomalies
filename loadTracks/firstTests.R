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
