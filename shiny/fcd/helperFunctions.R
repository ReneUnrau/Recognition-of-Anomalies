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