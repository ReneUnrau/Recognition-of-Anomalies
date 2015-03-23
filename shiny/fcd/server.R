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
          outliers <- findOutliers(currentTrack, input$attribute_selector, map)
          output$plot <- renderPlot({
            boxplot(outliers["data"], main="Boxplot representing selected attribute for chosen track", 
                    xlab=input$attribute_selector, ylab="Unit")
          })
          #result <- indices
          result <- outliers[["indices"]]
          
          } else if (chosenMethod == "Compare neighbors"){        
            analysisResult <- displayNeighborAnomalies(currentTrack, input$attribute_selector, map)
            output$plot <- renderPlot({
              boxplot(analysisResult["differences"], main="Selected attribute differences between neighbors for chosen track", 
                      xlab=input$attribute_selector, ylab="Unit")
            })
            result <- analysisResult[["indices"]]
            
          } else if (chosenMethod == "Unexpected stops"){        
            result <-  findTrafficSignalAnomalies(currentTrack, map)
          } else if (chosenMethod == "Unexpected turns"){        
            result <-  findTurnAnomalies(currentTrack, map, input$bearing_slider)
          } else if (chosenMethod == "Speed differences"){
            result <-  findSpeedAnomalies(currentTrack, map, input$difference_selector)
          }
          
          # increment the progress to 50 percent
          incProgress(0.5)
          
          output$analysis_message <- renderText({
            if (length(result) == 0){
              paste(span("No anomalies found!", style = "color:red"))
            } else {
              paste(span("Anomalies found: ", style = "color:red"), length(result))
            }
          
          })
          # finished progress
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
    #data <- mpg
    #data
  })
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$plot expressions
  formulaText <- reactive({
    paste("Plot for attribute: ", input$attribute_selector)
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
          p(span("Tutorial", style = "font-weight:bold"), br(), "On the left-hand side you can find a sidebar. Under 'Data Selection' you can define a time range. Furthermore, you can select the number of tracks to load (many tracks will decrease the performance!). Click on the 'Search'-Button to load the tracks. Then, you can select which track to display on the map.", br(), "Now, you can start to find anomalies. Choose one of the following analysis methods: "),
          tags$ul(tags$li(span("Outliers: ", style = "font-style:italic"), "This method compares the values of a specific parameter for a given track. Values that are unusually high or low compared to the other ones are identified and considered as outliers."), tags$li(span("Speed Differences: ", style = "font-style:italic"), "This method compares the different speed attributes of a track and highlights the corresponding points on the map, if the difference is greater than a threshold value."), tags$li(span("Compare Neighbours: ", style = "font-style:italic"), "This method looks at the differences between the values of two neighbor measurements (regarding the track, forward facing) for the chosen attribute."),  tags$li(span("Unexpected Stops: ", style = "font-style:italic"),"This method identifies all measurements where the speed is equal to 0 and a traffic signal is 50 or more meters away. Check 'Show traffic signals' to display them as a red dot."), tags$li(span("Unexpected Turns: ", style = "font-style:italic"),"This method is similar to the unexpected stop analysis and finds car turns. The difference angle between two measurements can be set with the slider. By default the method finds the position where the car turned more than 90\u00B0.")),
          p(span("A plot view is available under the tab next to 'Map'. We currently provide boxplots only for the 'Outliers'- and 'Compare Neighbours'-Analysis.", br(), "The Log shows more information about the track data.")),
          p(span("FAQ ", style = "font-weight:bold"), br(), "Where can I get this project? You can find the source code on", icon("github"), "GitHub: ", a(href="https://github.com/ReneU/Recognition-of-Anomalies", "https://github.com/ReneU/Recognition-of-Anomalies") ),
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
            span("GPS PDOP", style = "font-weight:bold"), "= Position Dilution of Precision: Measure of accuracy in 3-D position, also called spherical DOP. Range from 1 (ideal) to > 20 (poor)"
          ),
          h3("Libraries"),
          tags$ul(tags$li("Leaflet bindings for Shiny: ", a(href="https://github.com/jcheng5/leaflet-shiny", "https://github.com/jcheng5/leaflet-shiny")), tags$li("R package sp"), tags$li("enviroCar API: ", a(href="https://www.envirocar.org", "https://www.envirocar.org"), br(), imageOutput("logo", height = 100))),
          h3("Developers"),
          p("This application was developed by Tobias Brüggentisch, Daniel Sawatzky, Lars Syfuß and René Unrau.", br(),br(), "Institute for Geoinformatics (ifgi)", br(), "University of Münster", br(), "\u00A9 2015"))
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
  
  
  observe({
    if (input$reset_btn == 0) 
      return()
    map$clearMarkers()
    map$clearPopups()
    output$analysis_message <- renderText({
     
        paste(span(""))
      
    })
    
  })
  
})