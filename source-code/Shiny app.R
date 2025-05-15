options(shiny.version = "1.8.0")
library(shiny)
library(maps)
library(ggmap)
library(sf)
library(geosphere)
library(tidyverse)
library(plotly)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Tracking Data Test"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Example Visualization", tabName = "examples", icon = icon("globe")),
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Data Visualization", tabName = "visualization", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # tab 1 for Example Visualization
      tabItem(tabName = "examples",
              fluidRow(
                box(title = "KML Visualization Settings", width = 8, status = "info", solidHeader = TRUE,
                    selectInput("example", "Choose an Example:",
                                choices = c("Example 1", "Example 2", "Example 3", "Example as.kml run"),
                                selected = "Example 1"),
                    selectInput("example_simulation_icons", "Simulation Icons?:", choices = c("TRUE", "FALSE"), selected = "TRUE"),
                    selectInput("example_error_circle", "Show Error Circle?", choices = c("TRUE", "FALSE"), selected = "TRUE"),
                    numericInput("example_duration", "Duration (seconds):", value = 60),
                    numericInput("example_sequencetime", "Sequence Time (s):", value = 5),
                    selectInput("example_pov_cam", "Use POV Camera?", choices = c("TRUE", "FALSE"), selected = "FALSE"),
                    selectInput("example_follow_cam", "Follow Camera?", choices = c("TRUE", "FALSE"), selected = "TRUE"),
                    numericInput("example_num_simulations", "Number of Simulations:", value = 10),
                    selectInput("example_animal_icon", "Animal Icons?", choices = c("TRUE", "FALSE"), selected = "TRUE"),
                    selectInput("SimulationColor", "Simulation Color:", choices = c("A30000ff", "Red", "Yellow", "Green"), selected = "Blue"),
                    selectInput("PredictColor", "Prediction Color:", choices = c("A30000ff", "Red", "Yellow", "Green"), selected = "Blue")
                ),
                box(title = "Example Plot", width = 8, plotlyOutput("examplePlot"))
              )
      ),
      
      # tab 2 Data Upload
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Upload Inputs", width = 6, status = "primary", solidHeader = TRUE,
                    actionButton("submit", "Submit"),
                    fileInput("file", "Upload CSV File"),
                    fileInput("kmlfile", "Choose a KML file"),
                    selectInput("plotType", "Do you have a CTMM Model?:",
                                choices = c("Yes", "No"),
                                selected = "No"),
                    conditionalPanel(
                      condition = "input.plotType == 'Yes'",
                      fileInput("Model_Object", "Input a CTMM Model")
                    )
                ),
                box(title = "Simulation Settings", width = 6, status = "info", solidHeader = TRUE,
                    numericInput("Duration", "Duration (seconds):", value = 60),
                    numericInput("sequencetime", "Sequence Time (s):", value = 5),
                    numericInput("SimulationNumber", "Number of Simulations:", value = 5),
                    selectInput("SimulationColor_upload", "Simulation Color:", choices = c("A30000ff", "Red", "Yellow", "Green"), selected = "Blue"),
                    selectInput("PredictColor", "Prediction Color:", choices = c("A30000ff", "Red", "Yellow", "Green"), selected = "Blue"),
                    selectInput("AnimalIcon", "Animal Icons?:", choices = c("TRUE", "FALSE"), selected = "TRUE"),
                    selectInput("SimulationIcon", "Simulation Icons?:", choices = c("TRUE", "FALSE"), selected = "TRUE"),
                    selectInput("ErrorCircle", "Show Error Circle?", choices = c("TRUE", "FALSE"), selected = "TRUE"),
                    selectInput("POVCam", "Use POV Camera?", choices = c("TRUE", "FALSE"), selected = "FALSE"),
                    selectInput("FollowCam", "Follow Camera?", choices = c("TRUE", "FALSE"), selected = "TRUE")
                )
                
              )
      ),
      
      # tab 3 Data Visualization
      tabItem(tabName = "visualization",
              fluidRow(
                box(title = "Data-based Visualization", width = 12, plotlyOutput("uploadPlot"))
              )
      )
    )
  )
)


server <- function(input, output) {
  library(ctmm)
  library(geosphere)
  library(tidyverse)
  library(sf)
  
  # Load pre-existing data
  load("Cillia21.RData")
  Example1_data <- read_sf("test.kml")
  Example2_data <- read_sf("output.kml")
  Example3_data <- read_sf("output - Copy.kml")
  
  # User-uploaded data
  Data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE)
  })
  
  telemetryData <- eventReactive(input$submit, {
    req(Data())
    as.telemetry(Data())
  })
  
  Model <- reactive({
    req(input$Model_Object)
    env <- new.env()
    load(input$Model_Object$datapath, envir = env)
    obj_names <- ls(env)
    validate(need(length(obj_names) == 1, "Uploaded RData file must contain exactly one object."))
    get(obj_names[1], envir = env)
  })
  
  # --- Example Plot Output ---
  output$examplePlot <- renderPlotly({
    if (input$example == "Example as.kml run") {
      as.kml(CTMM = list(FIT),
             all_tour = FALSE,
             simulation_icons = TRUE,
             error_circle = TRUE,
             duration = 60, 
             animals = list(Cillia), 
             sequencetime = 5,
             pov_cam = FALSE,
             follow_cam = TRUE,
             num_simulations = 10)
      data <- read_sf("test.kml")
    } else {
      data <- switch(input$example,
                     "Example 1" = Example1_data,
                     "Example 2" = Example2_data,
                     "Example 3" = Example3_data)
    }
    
    req(data)
    line_data <- data %>% filter(st_geometry_type(geometry) == "LINESTRING")
    point_data <- data %>% filter(st_geometry_type(geometry) == "POINT")
    
    p <- ggplot() +
      geom_sf(data = line_data, color = input$SimulationColor, size = 1) +
      geom_sf(data = point_data, color = "red", size = 3) +
      theme_minimal() +
      labs(title = "Example Animal Path", x = "Longitude", y = "Latitude")
    
    ggplotly(p)
  })
  
  # --- Uploaded Data Plot Output ---
  output$uploadPlot <- renderPlotly({
    req(telemetryData())
    df <- telemetryData()
    
    FIT <- if (input$plotType == "No") {
      GUESS <- ctmm.guess(df, interactive = FALSE)
      ctmm.fit(df, GUESS, trace = TRUE)
    } else {
      Model()
    }
    
    library(ctmm)
    library(geosphere)
    library(tidyverse)
    library(sf)
    
    as.kml <- function(animals = list(),
                       CTMM = list(), 
                       all_tour = FALSE,
                       duration = 180,
                       num_simulations = 10,
                       animal_icon = TRUE,
                       error_circle = FALSE,
                       simulation_icons= FALSE,
                       pov_cam = TRUE,
                       manual_cam = FALSE,
                       follow_cam = FALSE,
                       color_sim = "A3ff0000",
                       iconsize = 1.5, 
                       markerimage = "https://pixelartmaker-data-78746291193.nyc3.digitaloceanspaces.com/image/fbc13eca20f56cd.png", 
                       color_icon = "A30000ff",
                       color_pred = "ffffffff",
                       sequencetime = 3,
                       circlepoints = 15,
                       confidence = 2.45,
                       path_altitude = 0,
                       cam_altitude = 300,
                       coords=  list(longitude = "longitude", 
                                     latitude = "latitude", 
                                     timestamp = "timestamp")) {
      
      if (is.null(CTMM)) {
        print("Please provide model('s)")
      }
      
      animal_data = animals
      number_of_animals <- length(animal_data)
      
      create_path_kml <- function(coords,altitude, name, color) {
        path_coords <- paste(coords[, 1], coords[, 2], altitude, sep = ",", collapse = " ")
        sprintf('
  <Placemark>
    <name>%s</name>
    <Style>
      <LineStyle>
        <color>%s</color>
        <width>2</width>
      </LineStyle>
    </Style>
    <LineString>
      <gx:altitudeMode>relativeToSeaFloor</gx:altitudeMode>
      <coordinates>
        %s
      </coordinates>
    </LineString>
  </Placemark>\n', name, color, path_coords) #user designated
      }
      
      create_icon_kml <- function(lon,lat, name, color, markerimage, iconsize, iconorder=1) {
        coord_str <- paste(lon[1], lat[1], sep = ",") 
        
        sprintf('
  <Placemark id = "%s">
    <name>%s</name>
    <Style>
      <IconStyle>
        <color>%s</color>
        <scale>%.1f</scale>
        <zIndex>%f</zIndex>
        <Icon>
          <href>%s</href>
        </Icon>
      </IconStyle>
    </Style>
    <Point>
      <coordinates>%s,0</coordinates>
    </Point>
  </Placemark>', 
                name, 
                name,# Placemark name
                color,
                iconsize,
                iconorder,# Icon scale
                markerimage,     # URL for the icon image
                coord_str     # Coordinates
        )
      }
      
      generate_kml_for_animals <- function(animal_data) {
        kml_content <- ''
        all_predictions <- list()
        
        results <- data.frame(time = numeric(), i = numeric(), stringsAsFactors = FALSE)
        all_animals <- data.frame(time = numeric(), i = numeric(), stringsAsFactors = FALSE)
        all_circle <- data.frame(time = numeric(), i = numeric(), stringsAsFactors = FALSE)
        animal_names <- unlist(animals)
        for (i in seq_along(animal_data)) {
          
          # Variables made for each animal each loop.
          
          animal <- animal_data[[i]]
          CTMM_Model <- CTMM[[i]]
          lon <- animal[[coords$longitude]]
          lat <- animal[[coords$latitude]]
          if (!is.null(CTMM)) {
            
            
            SEQ <- seq(from = animal$t[1], to = animal$t[nrow(animal)], by = sequencetime %#% 'min')
            
            simulations <- list()
            for (sim in 1:num_simulations) {
              simulations[[sim]] <- simulate(data = animal, CTMM_Model, t = SEQ, complete = TRUE)
            }
            
            PRED <- predict(CTMM_Model, data = animal, t = SEQ, complete = TRUE)
            
            # Calculate the duration of each animation in seconds
            
            rowcount <- nrow(PRED)
            
            calculated_duration <- duration /rowcount
            
            
            processing <- data.frame(
              t = PRED$t,
              longitude = PRED$longitude,
              latitude = PRED$latitude,
              COV_x_x = PRED$COV.x.x,
              COV_y_y = PRED$COV.y.y
            )
            # project function taken from ctmm
            
            project <- function(x, from = DATUM, to = DATUM) 
            {
              if (to == from) {
                return(x)
              }
              x <- data.frame(x)
              from <- sf::st_crs(from)
              to <- sf::st_crs(to)
              x <- sf::st_as_sf(x, coords = 1:2, crs = from)
              x <- sf::st_transform(x, crs = to)
              x <- sf::st_coordinates(x)
              colnames(x) <- c("x", "y")
              return(x)
            }
            
            # this function is what makes the circle coordinates, confidence and circlepoints can be changed by user.
            
            generate_circle_coord <- function(longitude, latitude, radius, num_points = circlepoints, confidencevalue = confidence, crs_proj = NULL) {
              crs_proj <- crs_proj %||% "+proj=utm +zone=33 +datum=WGS84"
              
              center_coords <- project(data.frame(x = longitude, y = latitude), from = 4326, to = crs_proj)
              
              angles <- seq(0, 2 * pi, length.out = num_points)
              circle_cartesian <- data.frame(
                x = center_coords[1] + (radius * cos(angles))* confidencevalue,
                y = center_coords[2] + (radius * sin(angles))* confidencevalue
              )
              
              circle_longlat <- project(circle_cartesian, from = crs_proj, to = 4326)
              
              as.data.frame(circle_longlat) %>%
                setNames(c("circle_longitude", "circle_latitude"))
            }
            
            # add raduis into each column and then use the circle coord function to 
            # generate the long lat for each circle point        
            processing <- processing %>%
              rowwise() %>%
              mutate(
                radius = sqrt((COV_x_x + COV_y_y) / 2),
                circle_coords = list(generate_circle_coord(longitude, latitude, radius))
              ) %>%
              ungroup()
            
            initial_coords <- paste(
              apply(processing$circle_coords[[1]], 1, function(row) paste(row, collapse = ",")),
              collapse = " "
            )  
            
            
            predict_path <- cbind(PRED$longitude, PRED$latitude)
            
            all_predictions[[i]] <- PRED           #stores the predictions to be used in the All Tour section
            
            # creates any number of simulations the user wants, with color being set by the user.
            
            for (sim in 1:num_simulations) {
              lon <- simulations[[sim]]$longitude
              lat <- simulations[[sim]]$latitude
              sim_coords <- cbind(simulations[[sim]]$longitude, simulations[[sim]]$latitude)
              kml_content <- paste0(kml_content, create_path_kml(sim_coords, path_altitude, name = paste("Animal", i, "Simulation", sim), color_sim))
              kml_content <- paste0(kml_content, create_icon_kml(lon,lat, name = paste("Simulation", sim, i), color_sim, markerimage,iconsize))
            }
            animalname <- animal_names[i] 
            
            icon <- as.character(markerimage)
            # creates the main predicted path
            kml_content <- paste0(kml_content, create_path_kml(predict_path, path_altitude, name = paste("Animal", i, "Predict Path"), color_pred))
            
            kml_content <-paste0(kml_content, create_icon_kml(PRED$longitude,PRED$latitude, name = paste("Animal", i), color_icon, icon,iconsize,iconorder=2))
            
            # initial position for each animal marker
            
            kml_content <- paste0(kml_content, sprintf( '
  <Placemark id="moving_circle_%s">
   <name> Moving Animal Circle %s </name>
    <Polygon>
    <Style>
      <PolyStyle>
        <color>A30000ff</color> 
      </PolyStyle>
    </Style>
      <outerBoundaryIs>
        <LinearRing>
          <coordinates>%s</coordinates>
        </LinearRing>
      </outerBoundaryIs>
    </Polygon>
  </Placemark>
  ', i, i, initial_coords))
            
            #####
            # tour content for each animal (animation portion) # 
            
            
            kml_content <- paste0(kml_content, sprintf('
  <gx:Tour>
    <name>Animal %s Animated Path</name>
    <gx:Playlist>
  ', animalname))
            
            num_timesteps <- nrow(PRED)
            
            all_change_content <- vector("list", num_timesteps)
            all_camera_content <- vector ("list", num_timesteps)
            if (simulation_icons == TRUE) { 
              num_timesteps <- nrow(simulations[[1]])
              
              
              
              for (b in 1:num_timesteps) {
                
                current_row <- data.frame(time = simulations[[1]]$t[b], i = i, stringsAsFactors = FALSE)
                
                for (sim in 1:length(simulations)) {
                  current_sim <- simulations[[sim]]  
                  
                  # Get the longitude and latitude for the current timestep
                  lon <- current_sim$longitude[b]
                  lat <- current_sim$latitude[b]
                  
                  # Create a new column for the current simulation
                  current_row[[paste0("Simulation_", sim)]] <- paste(lon,",",lat)
                }
                results <- rbind(results, current_row)
                
                for (sim in 1:length(simulations)) {
                  current_sim <- simulations[[sim]]  
                  
                  lon <- current_sim$longitude[b]
                  lat <- current_sim$latitude[b]
                  
                  change_section <- sprintf('
      <Placemark targetId="Simulation %d %d">
        <Point>
          <coordinates>%f,%f,0</coordinates>
        </Point>
      </Placemark>',
                                            sim, i, lon, lat)
                  
                  # Add to the existing content in this row
                  all_change_content[[b]] <- paste0(all_change_content[[b]], change_section)
                }
              }
            }
            if (animal_icon == TRUE) { 
              num_timesteps <- nrow(PRED)
              #animal_icon_ids[[i]] <- paste0(sprintf('"Animal %d"', animalname))
              
              for (b in 1:num_timesteps) {
                current_row <- data.frame(
                  time = PRED$t[b],  # Access time column in PRED
                  i = i, 
                  stringsAsFactors = FALSE
                )
                
                lon <- PRED$longitude[b]
                lat <- PRED$latitude[b]
                
                # Add the animal coordinates column
                current_row[[paste0("Animal")]] <- paste(lon, ",", lat)
                
                all_animals <- rbind(all_animals, current_row)
                change_section <- sprintf('
      <Placemark targetId="Animal %d">
        <Point>
          <coordinates>%f,%f,0</coordinates>
        </Point>
      </Placemark>',
                                          i, lon, lat)
                
                all_change_content[[b]] <- paste0(all_change_content[[b]], change_section)
              }
            }
            if (error_circle == TRUE) { 
              num_timesteps <- nrow(processing)
              #circle_ids[[i]] <- paste0(sprintf('"moving_circle_%d"', animalname))
              
              for (b in 1:num_timesteps) {
                coords_str <- paste(
                  apply(processing$circle_coords[[b]], 1, function(row) paste(row, collapse = ",")), 
                  collapse = " "
                )     
                current_row <- data.frame(
                  time = PRED$t[b],  # Access time column in PRED
                  i = i, 
                  stringsAsFactors = FALSE
                )
                
                
                
                current_row[[paste0("Circle")]] <- paste(coords_str)
                
                all_circle <- rbind(all_circle, current_row)
                
                change_section <- sprintf('
            <Placemark targetId="moving_circle_%s">
              <Polygon>
                <outerBoundaryIs>
                  <LinearRing>
                    <coordinates>%s</coordinates>
                  </LinearRing>
                </outerBoundaryIs>
              </Polygon>
            </Placemark>',
                                          i, coords_str)
                
                all_change_content[[b]] <- paste0(all_change_content[[b]], change_section)
              }
            }
            if (pov_cam == TRUE){ 
              num_timesteps <- nrow(processing)
              
              for (b in 1:num_timesteps) {
                lon1 <- PRED$longitude[b]
                lat1 <- PRED$latitude[b]
                lon2 <- PRED$longitude[b+1]
                lat2 <- PRED$latitude[b+1]
                
                current_row <- data.frame(
                  time = PRED$t[b],  # Access time column in PRED
                  i = i, 
                  stringsAsFactors = FALSE
                )
                
                
                
                
                azimuth <- bearing(c(lon1, lat1), c(lon2, lat2))
                
                range <- distHaversine(c(lon1, lat1), c(lon2, lat2))
                
                change_section <- sprintf('
              
        <gx:duration>%f</gx:duration>
        <gx:flyToMode>smooth</gx:flyToMode>
        <LookAt>
          <longitude>%f</longitude>
          <latitude>%f</latitude>
          <altitude>10</altitude>
          <heading>%f</heading>
          <tilt>90</tilt>
          <range>%f</range>
          <gx:altitudeMode>relativeToGround</gx:altitudeMode>
        </LookAt>
      ',
                                          calculated_duration, lon2, lat2, azimuth,range)
                
                all_camera_content[[b]] <- paste0(all_camera_content[[b]], change_section)
              }
            }
            if (follow_cam == TRUE){          num_timesteps <- nrow(processing)
            
            for (b in 1:num_timesteps) {
              lon <- PRED$longitude[b]
              lat <- PRED$latitude[b]
              
              current_row <- data.frame(
                time = PRED$t[b],  # Access time column in PRED
                i = i, 
                stringsAsFactors = FALSE
              )
              
              change_section <- sprintf('
              
        <gx:duration>%f</gx:duration>
        <gx:flyToMode>smooth</gx:flyToMode>
        <LookAt>
          <longitude>%f</longitude>
          <latitude>%f</latitude>
          <altitude>%f</altitude>
          <tilt>0</tilt>
          <gx:altitudeMode>relativeToGround</gx:altitudeMode>
        </LookAt>
      ',
                                        calculated_duration, lon, lat, cam_altitude)
              
              all_camera_content[[b]] <- paste0(all_camera_content[[b]], change_section)
              
            }
            
            }
            if (manual_cam == TRUE){ 
              num_timesteps <- nrow(processing)
              
              for (b in 1:num_timesteps) {
                lon1 <- PRED$longitude[b]
                lat1 <- PRED$latitude[b]
                lon2 <- PRED$longitude[b+1]
                lat2 <- PRED$latitude[b+1]
                
                current_row <- data.frame(
                  time = PRED$t[b],  # Access time column in PRED
                  i = i, 
                  stringsAsFactors = FALSE
                )
                
                
                
                
                azimuth <- bearing(c(lon1, lat1), c(lon2, lat2))
                
                range <- distHaversine(c(lon1, lat1), c(lon2, lat2))
                
                change_section <- sprintf('
              
        <gx:duration>%f</gx:duration>
      ',
                                          calculated_duration)
                
                all_camera_content[[b]] <- paste0(all_camera_content[[b]], change_section)
              }
            }
            
            for (b in 1:nrow(predict_path)) {
              
              
              kml_content <- paste0(kml_content, sprintf('
      <gx:AnimatedUpdate>
        <gx:duration>%f</gx:duration>
        <Update>
          <Change>
              %s
          </Change>
        </Update>
      </gx:AnimatedUpdate>
  
      <gx:FlyTo>
        %s
      </gx:FlyTo>
      ', calculated_duration, all_change_content[[b]], all_camera_content[[b]]))
              
              # monitor message for each animated update
              message(sprintf("Animating position %d for Animal %d", b, i))
            }
            
            # close the tour section after all animals have had their tour generated
            
            kml_content <- paste0(kml_content, '
    </gx:Playlist>
  </gx:Tour>
  ')
            
            
            
            
          }
          
        } 
        
        if (all_tour) {
          
          combined_predictions <- cbind(all_animals, all_circle, results)
          #####
          
          long_lat_df <- combined_predictions %>%
            select(matches("Animal|Simulation_")) %>%  # Select only columns with coordinates
            pivot_longer(everything(), names_to = "Type", values_to = "Coordinates") %>%
            separate(Coordinates, into = c("Longitude", "Latitude"), sep = ",", convert = TRUE)
          # Calculate the bounding box
          min_lon<- min(long_lat_df$Longitude)
          max_lon<- max(long_lat_df$Longitude)
          min_lat<- min(long_lat_df$Latitude)
          max_lat<- max(long_lat_df$Latitude)
          
          
          mid_lat <- (min_lat+ max_lat) / 2
          
          mid_lon <- (min_lon + max_lon) / 2
          
          top_left <- c(min_lon, max_lat)
          
          bottom_right <- c(max_lon, min_lat)
          
          max_distance <- distVincentyEllipsoid(top_left, bottom_right)
          
          altitude <- max_distance * 2
          
          #####
          processing <- data.frame(
            t = combined_predictions$t,
            longitude = combined_predictions$longitude,
            latitude = combined_predictions$latitude,
            COV_x_x = combined_predictions$COV.x.x,
            COV_y_y = combined_predictions$COV.y.y,
            animalid = combined_predictions$animalid
          )
          
          
          # The tour animation for each animal
          
          
          kml_content <- paste0(kml_content, sprintf('
  <gx:Tour>
    <name>Animal %s Animated Path</name>
    <gx:Playlist>
  ', animalname))
          num_timesteps <- nrow(results)
          num_column <- ncol(results) - 2
          all_change_content <- vector("list", num_timesteps)
          if (simulation_icons == TRUE) { 
            
            for (b in 1:num_timesteps) {
              
              
              # Add simulation icons content for this timestep
              for (sim in 1:num_column) {
                column_name <- paste0("Simulation_", sim)
                i <- results$i
                current_sim <- results[[column_name]]  # Access current simulation
                icon_coords <- current_sim[b]
                
                
                change_section <- sprintf('
      <Placemark targetId="Simulation %d %d">
        <Point>
          <coordinates>%s,0</coordinates>
        </Point>
      </Placemark>',
                                          sim, i[b], icon_coords)
                # Add to the existing content in this row
                all_change_content[[b]] <- paste0(all_change_content[[b]], change_section)
              }
            }
          }
          
          if (animal_icon == TRUE) {
            
            
            
            for (t in 1:num_timesteps) {
              
              
              # Add animal icons content for this timestep
              i <- all_animals$i[t]
              animal_coord <- all_animals$Animal[t]
              
              change_section <- sprintf('
      <Placemark targetId="Animal %d">
        <Point>
          <coordinates>%s,0</coordinates>
        </Point>
      </Placemark>',
                                        i, animal_coord)
              
              # Add to the existing content in this row
              all_change_content[[t]] <- paste0(all_change_content[[t]], change_section)
              
            }
          }
          if (error_circle == TRUE) { 
            num_timesteps <- nrow(all_animals)
            
            for (t in 1:num_timesteps) {
              i <- all_circle$i[t]
              coords_str <- all_circle$Circle[t] 
              change_section <- sprintf('
            <Placemark targetId="moving_circle_%s">
              <Polygon>
                <outerBoundaryIs>
                  <LinearRing>
                    <coordinates>%s</coordinates>
                  </LinearRing>
                </outerBoundaryIs>
              </Polygon>
            </Placemark>',
                                        i, coords_str)
              
              # Add to the existing content in this row
              all_change_content[[t]] <- paste0(all_change_content[[t]], change_section)
            }
          }
          
          for (i in 1:nrow(combpath)) {
            
            
            kml_content <- paste0(kml_content, sprintf('
      <gx:AnimatedUpdate>
        <gx:duration>%f</gx:duration>
        <Update>
          <Change>
              %s
          </Change>
        </Update>
      </gx:AnimatedUpdate>
  
      <gx:FlyTo>
        <gx:duration>%f</gx:duration>
        <gx:flyToMode>smooth</gx:flyToMode>
        <LookAt>
          <longitude>%f</longitude>
          <latitude>%f</latitude>
          <altitude>300</altitude>
          <heading>0</heading>
          <tilt>0</tilt>
          <range>1000</range>
          <gx:altitudeMode>relativeToGround</gx:altitudeMode>
        </LookAt>
      </gx:FlyTo>
      ', calculated_duration, all_change_content[[i]], calculated_duration, processing$longitude[i], processing$latitude[i]))
            
            # monitor message for each animated update
            message(sprintf("Animating position %d for Moving Circle Polygon %s", i, animalname))
          }
          kml_content <- paste0(kml_content, '
    </gx:Playlist>
  </gx:Tour>
  ')
          
          
          
          
        }
        
        
        return(kml_content)
      }
      
      if (length(animals) > 0) {
        kml_content <- generate_kml_for_animals(animals)
      } else {
        kml_content <- generate_kml_for_animals(list(data))
      }
      
      kml_header <- '<?xml version="1.0" encoding="UTF-8"?>
  <kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2">
  <Document>
  <name>Animal Simulation Paths</name>'
      
      kml_footer <- '</Document>
  </kml>'
      
      kml_output <- paste0(kml_header, kml_content, kml_footer)
      writeLines(kml_output, "test.kml")
      
    }
    
    as.kml(
      CTMM = list(FIT),
      all_tour = FALSE,
      error_circle = input$ErrorCircle == "TRUE",
      color_sim = input$SimulationColor_upload,
      color_pred = input$PredictColor,
      duration = input$Duration,
      sequencetime = input$sequencetime,
      animals = list(df),
      animal_icon = input$AnimalIcon == "TRUE",
      simulation_icons = input$SimulationIcon == "TRUE",
      pov_cam = input$POVCam == "TRUE",
      follow_cam = input$FollowCam == "TRUE",
      num_simulations = input$SimulationNumber
    )
    
    kml_data <- read_sf("test.kml")
    line_data <- kml_data %>% filter(st_geometry_type(geometry) == "LINESTRING")
    point_data <- kml_data %>% filter(st_geometry_type(geometry) == "POINT")
    
    p <- ggplot() +
      geom_sf(data = line_data, color = input$SimulationColor_upload, size = 1) +
      geom_sf(data = point_data, color = "red", size = 3) +
      theme_minimal() +
      labs(title = "Uploaded Data Path", x = "Longitude", y = "Latitude")
    
    ggplotly(p)
  })
}
shinyApp(ui, server)
