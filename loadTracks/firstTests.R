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

tracksURL = "https://envirocar.org/api/stable"
trackID = "545aa4f4e4b0b53890a2e62c"

queryTracksCollection = importEnviroCar(tracksURL, trackID)


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
