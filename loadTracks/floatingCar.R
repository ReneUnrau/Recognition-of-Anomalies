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

# testing bbox (DOES NOT WORK YET!)
ll = c(7.57,51.92) # lower left : 51.928179, 7.573629
ur = c(7.68,52) # upper right: 51.985515, 7.674909
exampleBB = matrix(c(ll,ur), ncol=2)
colnames(exampleBB) = c("min", "max")
exampleBB
class(exampleBB)
exampleBB = bbox(exampleBB)
# boundingbox = matrix(c(7.61,51.96,7.62,51.97),ncol=2,dimnames=list(c("x","y"),c("min","max")))
boundingbox = matrix(c(ll, ur),ncol=2,dimnames=list(c("x","y"),c("min","max")))
boundingbox

exampleBB = data.frame(ll, ur) # an example bounding box 
exampleBB[2,1]

queryBoundingBox = importEnviroCar(serverUrl=stableURL, bbox=boundingbox)

trIds = getTrackIDs(serverUrl=stableURL, bbox=boundingbox)
