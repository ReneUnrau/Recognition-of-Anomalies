#import outsourced and external files
source ("import.R")
source ("analysisFunctions.R")
source ("mapFunctions.R")
source ("helperFunctions.R")

library(ggplot2)

#Functions
changeTrackSelection = function(){
  # get length of current tracksCollection
  len = length(trCol@tracksCollection)
  numbers = c(1:len)
  tracknames = paste("Track", numbers)
  # assign tracknames to tracksList
  selectInput("tracksList", 
              label = "Choose a track to display",
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
  
  # change TracksCollection if search button is used. Progressbar indicates the status of loading tracks
  observe({
    if (input$search_btn == 0) 
      return()
    withProgress(message = 'Loading...', detail = "Track 1", value = 0, {
      
      isolate({
        time = create_timeinterval_from_input(input$dates[1], input$dates[2])
        limit = input$limit_slider
        inc = 1/limit

        for (i in 1:limit) {
          incProgress(inc, detail = paste("Track", i))
          
          # just get the bbox if the checkbox is clicked!
          if(input$checkbox_bb[1]){
            bbox = create_bbox_from_input(c(input$map_bounds[4],input$map_bounds[3]), c(input$map_bounds[2],input$map_bounds[1]))
            trCol <<- importEnviroCar(serverUrl = stableURL, bbox = bbox, timeInterval = time, limit = limit)
          }
          # else do not consider the bbox
          else {
            trCol <<- importEnviroCar(serverUrl = stableURL, timeInterval = time, limit = limit)
          }
          
        }
        # make choosable tracks refering to trackscollection
        output$tracks = renderUI({
          changeTrackSelection()
        })
        
        setProgress(1)
        
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
      withProgress(message = 'Displaying traffic signals...', value = 0, {
        # load the traffic signals as red dots
        coordinates = traffic_signals@coords
        for(i in 1:nrow(coordinates)){
          latitude <- as.numeric((coordinates[i,2]))
          longitude <- as.numeric((coordinates[i,1]))
          # start assigning IDs with 10000
          map$addCircle(latitude, longitude, 5, toString(10000+i), list(color='#FF0040'))
          incProgress(0.05)
        }
        setProgress(1)
      })
    } else {
      # delete all circles but add the track again
      drawTrack(currentTrack, map)
    }
  })
  
  # show Anomalies on map and display plot
  observe({
    if (input$anomalies_btn == 0) 
      return()
    withProgress(message = 'Finding anomalies...', value = 0, {
      isolate({
        chosenMethod <- input$analysis_method
        if(chosenMethod == "Outliers"){
          result <- findOutliers(currentTrack, input$attribute_selector, map)
          output$plot <- renderPlot({
            boxplot(result, main="Boxplot representing selected attribute for chosen track", 
                    xlab=input$attribute_selector, ylab="ylab description")
          })
          
          } else if (chosenMethod == "Compare neighbors"){        
            differences <- displayNeighborAnomalies(currentTrack, input$attribute_selector, map)
            output$plot <- renderPlot({
              boxplot(differences, main="Selected attribute differences between neighbors for chosen track", 
                      xlab=input$attribute_selector, ylab="ylab description")
            })
            result <- ""
            
          } else if (chosenMethod == "Unexpected stops"){        
            result <-  findTrafficSignalAnomalies(currentTrack, map)
          } else if (chosenMethod == "Unexpected car turns"){        
            result <-  findTurnAnomalies(currentTrack, map)
          } else if (chosenMethod == "Speed differences"){
            result <-  findSpeedAnomalies(currentTrack, map, input$difference_selector)
          }
          
          incProgress(0.5)
          
          output$analysis_message <- renderText({
            if (length(result) == 0 ){
              paste(span("No anomalies found!", style = "color:red"))
            } else {
              paste(span("Anomalies found: ", style = "color:red"), length(result))
            }
          
          })
          
          setProgress(1)
        })
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
  output$table <- renderDataTable({
    # show selected track data
    data <- mpg
    
    data
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
          p(span("FAQ ", style = "font-weight:bold"), br(), "Where can I get this project? You can find the source code on", icon("github"), "GitHub: ", a(href="https://github.com/ReneU/Recognition-of-Anomalies", "https://github.com/ReneU/Recognition-of-Anomalies") ),
          br(),
          h3("Definitions"),
          p(span("CO2", style = "font-weight:bold"), "= Carbon dioxide: The emission of CO2 is measured in kg/h",
            br(), 
            span("MAF", style = "font-weight:bold"), "= Mass Air Flow: the mass flowrate of air entering a fuel-injected internal combustion engine. Unit: l/s. The calculated MAF is measured in grams per second. Range from 0 .. 655.35 g/s",
            br(),
            span("RPM", style = "font-weight:bold"), "= Revolutions per minute: measure of the frequency of rotation, specifically the number of rotations around a fixed axis in one minute. Range from 0 to 16383.75 rpm. Unit: u/min",
            br(),
            span("GPS HDOP", style = "font-weight:bold"), "= Horizontal Dilution of Precision: Measure of accuracy in 2-D position, for example Latitude and Longitude. Range from 1 (ideal) to > 20 (poor)", 
            br(),
            span("GPS VDOP", style = "font-weight:bold"), "= Vertical Dilution of Precision: Measure of accuracy in 1-D position (height). Range from 1 (ideal) to > 20 (poor)", 
            br(),
            span("GPS PDOP", style = "font-weighft:bold"), "= Position Dilution of Precision: Measure of accuracy in 3-D position, also called spherical DOP. Range from 1 (ideal) to > 20 (poor)", 
            br(), "..."
          ),
          br(),
          h3("Libraries"),
          p("Leaflet bindings for Shiny: ", a(href="https://github.com/jcheng5/leaflet-shiny", "https://github.com/jcheng5/leaflet-shiny"), br(), "Package sp"),
          br(),
          h3("Developers"),
          p("This application was developed by Tobias Brüggentisch, Daniel Sawatzky, Lars Syfuß and René Unrau."))
  })
  
  # Center map at first measurement
  observe({
    if (input$showStart_btn == 0) 
      return()
    isolate({
      zoomToStart(currentTrack, map)
    })
  })
  
  # Center map at last measurement
  observe({
    if (input$showFinish_btn == 0) 
      return()
    isolate({
      zoomToFinish(currentTrack, map)
    })
  })
  
  # Center track
  observe({
    if (input$centerTrack_btn == 0) 
      return()
    isolate({
      centerTrack(currentTrack, map)
    })
  })
  
  # image2 sends pre-rendered images
  output$logo <- renderImage({
      return(list(
        src = "www/enviroCarLogo.png",
        contentType = "image/png",
        alt = "Logo",
        height = 100
      ))  
    
  }, deleteFile = FALSE)
})