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
  tracknames = paste("Track", numbers)
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
  
  # initially change track selection (default track is added)
  output$tracks = renderUI({
    changeTrackSelection()
  })
  
  # change TracksCollection if search button is used
  observe({
    if (input$search_btn == 0) 
      return()
    isolate({
      time = create_timeinterval_from_input(input$dates[1], input$dates[2])
      limit = input$limit_slider
      # just get the bbox if the checkbox is clicked!
      if(input$checkbox_bb[1]){
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
      currentTrack <<- get_track(trCol, num)
      # show track on map
      drawTrack(currentTrack, map)
    })
  })
  
  # show the traffic signals of the district of Muenster if the checkbox is clicked
  observe({
    if(input$checkbox_signals[1]){
      # load the traffic signals as red dots
      coordinates = traffic_signals@coords
      for(i in 1:nrow(coordinates)){
        latitude <- as.numeric((coordinates[i,2]))
        longitude <- as.numeric((coordinates[i,1]))
        map$addCircle(latitude, longitude, 5, paste("traffic_signal",i), list(color='#FF0040'))
      }
    } else {
      # delete all circles but add the track again
      drawTrack(currentTrack, map)
    }
  })
  
  # show Anomalies on map and display plot
  observe({
    if (input$anomalies_btn == 0) 
      return()
    
    isolate({
      chosenMethod <- input$analysis_method
      if(chosenMethod == "Outliers"){
        findOutliers(currentTrack, input$attribute_selector, map)
        output$plot <- renderPlot({
          boxplot(currentTrack@data$Speed, main="Boxplot representing selected attribute for chosen track", 
                xlab=input$attribute_selector, ylab="ylab description")
        })
        output$analysis_message <- renderText({
          paste(span("Anomalies found: ", style = "color:red"))
        })
        
      } else if (chosenMethod == "Compare neighbors"){        
        neighborBoxplot <- displayNeighborAnomalies(currentTrack, input$attribute_selector, map)
        output$plot <- renderPlot({
          neighborBoxplot
        })
        output$analysis_message <- renderText({
          paste(span("Anomalies found: ", style = "color:red"))
        })
      } else if (chosenMethod == "Unexpected stops"){        
        findTrafficSignalAnomalies(currentTrack, map)
      } else if (chosenMethod == "Unexpected car turns"){        
        findTurnAnomalies(currentTrack, map)
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
    # data needs to be in a different format: xtable
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
  
  summaryText <- reactive({
    summary(currentTrack)
  })
  
  # just the summary of the current track that is displayed
  output$log <- renderPrint({
    summaryText()
  })

  # information with: a tutorial, list of enviroCar related terms and definitions, used libraries, collaborators
  output$info_text <- renderText({
    paste(h3("How-To and FAQ"),
          p(span("Tutorial", style = "font-weight:bold"), br(), "Coming soon..."),
          p(span("FAQ ", style = "font-weight:bold"),br(), "Coming soon..."),
          br(),
          h3("Definitions"),
          p(span("MAF", style = "font-weight:bold"), "= Mass Air Flow: the mass flowrate of air entering a fuel-injected internal combustion engine.",
            br(), "..."),
          br(),
          h3("Libraries"),
          p("Used libraries: https://github.com/jcheng5/leaflet-shiny"),
          br(),
          h3("Collaborators"),
          p("This application was developed by Tobias Brüggentisch, Daniel Sawatzky, Lars Syfuß and René Unrau."))
  })

  
})