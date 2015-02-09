#import outsourced and external files
source ("import.R")
source ("analysisFunctions.R")
source ("mapFunctions.R")
source ("helperFunctions.R")

#Functions
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
  
  # change TracksCollection if search button is used
  observe({
    if (input$search_btn == 0) 
      return()
    isolate({
      time = create_timeinterval_from_input(input$dates[1], input$dates[2])
      limit = input$limit_slider
      # just get the bbox if the checkbox is clicked!
      if(input$checkbox[1]){
        bbox = create_bbox_from_input(c(input$map_bounds[4],input$map_bounds[3]), c(input$map_bounds[2],input$map_bounds[1]))
        trCol <<- importEnviroCar(serverUrl = stableURL, bbox = bbox, timeInterval = time, limit = limit)
      }
      # else do not consider the bbox
      else {
        trCol <<- importEnviroCar(serverUrl = stableURL, timeInterval = time, limit = limit)
      }
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
      # get number from track selection and make object global
      num = get_trackNumber(input$tracksList)
      currenttrack <- get_track(trCol, num)
      # show track on map
      drawTrack(currenttrack, map)
    })
  })
  
  # show Anomalies on map
  observe({
    if (input$anomalies_btn == 0) 
      return()
    
    isolate({
      chosenMethod <- input$analysis_method
      # get number from track selection
      num = get_trackNumber(input$tracksList)
      #make track object global for popupcontent access
      currenttrack <- get_track(trCol, num)
      if(chosenMethod == "Outliers"){
        findOutliers(currenttrack, input$anomalies_btn, map)
        
      } else if (chosenMethod == "Compare neighbors"){        
        displayNeighborAnomalies(currenttrack, input$anomalies_btn, map)
      }
    })
  })
  
  # handle click event for marker - id is used to access content
  markerClickObs <- observe({
    map$clearPopups()
    event <- input$map_marker_click
    
    if (is.null(event)) {
      return ()
    }
    index <- as.integer(event$id)
    content <- getPopUpContent(index, currentTrack)
    map$showPopup(event$lat, event$lng, content)
  })
  
  # handle click event for shape - id is used to access content
  shapeClickObs <- observe({
    map$clearPopups()
    event <- input$map_shape_click
    
    if (is.null(event)) {
      return ()
    }    
    index <- as.integer(event$id)
    content <- getPopUpContent(index, currentTrack)
    map$showPopup(event$lat, event$lng, content)
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    # show selected track data
  })
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$plot expressions
  formulaText <- reactive({
    paste("time ~", input$attribute_selector)
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against time
  output$plot <- renderPlot({
    boxplot(as.formula(formulaText()), 
            data = #currentTrack@data@[input$attribute_selector]
              ,
            outline = highs)
  })
  
  summaryText <- reactive({
    summary(currentTrack)
  })
  
  # just the summary of the current track that is displayed
  output$log <- renderPrint({
    summaryText()
  })
})