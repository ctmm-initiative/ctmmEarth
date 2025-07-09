library(ctmm)
library(geosphere)
library(tidyverse)
library(sf)
as.kml <- function(DATA,
                   CTMM = list(), 
                   all_tour = FALSE,
                   duration = 180,
                   num_simulations = 10,
                   animal_icon = TRUE,
                   error_circle = FALSE,
                   error_ellipses = FALSE,
                   simulation_icons= FALSE,
                   camera_mode = "follow",
                   all_camera_mode ="central",
                   color_sim = "red",
                   iconsize = 1.5, 
                   icon_image = "https://pixelartmaker-data-78746291193.nyc3.digitaloceanspaces.com/image/fbc13eca20f56cd.png", 
                   color_icon = "blue",
                   color_pred = "green",
                   sequencetime = 3,
                   opacity = 65,
                   circlepoints = 15,
                   confidence = 2.45,
                   path_altitude = 0,
                   cam_altitude = 300,
                   heading = 0,
                   tilt = 0,
                   range =0,
                   filename = "Output.kml",
                   coords=  list(longitude = "longitude", 
                                 latitude = "latitude", 
                                 timestamp = "timestamp")) {
  
  if (is.null(CTMM)) {
    print("Please provide model('s)")
  }
  # To fit the models to the telemetry list and convert individual to a list to allow for proper use in as.kml
  if (class(DATA) == "list") {
    if (is.list(CTMM) && all(sapply(CTMM, inherits, "ctmm"))) {
      ids_from_fit <- sapply(CTMM, function(model) model@info$identity)
      
      DATA <- DATA[ids_from_fit]
    } 
    } else if (class(DATA) == "telemetry") {
         DATA = list(DATA)
  }
  animal_data = DATA
  number_of_animals <- length(animal_data)
  
  # Converts a color name and opacity percentage to KML color format
  get_kml_color <- function(color_name, opacity_percent) {
    rgb_vals <- col2rgb(color_name)
    red <- rgb_vals[1]
    green <- rgb_vals[2]
    blue <- rgb_vals[3]
    
    red_hex <- sprintf("%02X", red)
    green_hex <- sprintf("%02X", green)
    blue_hex <- sprintf("%02X", blue)
    
    alpha <- round((opacity_percent / 100) * 255)
    alpha_hex <- sprintf("%02X", alpha)
    
    paste0(alpha_hex, blue_hex, green_hex, red_hex)
  }
  
  create_path_kml <- function(coords, altitude, name, color_name, opacity_percent) {
    path_coords <- paste(coords[, 1], coords[, 2], altitude, sep = ",", collapse = " ")
    kml_color <- get_kml_color(color_name, opacity_percent)
    
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
  </Placemark>\n', 
            name, kml_color, path_coords
    )
  }
  
  
  create_icon_kml <- function(lon, lat, name, color_name, opacity_percent,
                              icon_image, iconsize, iconorder = 1) {
    coord_str <- paste(lon[1], lat[1], sep = ",")
    kml_color <- get_kml_color(color_name, opacity_percent)
    
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
            name, name,
            kml_color,
            iconsize,
            iconorder,
            icon_image,
            coord_str)
  }
  
  
  generate_kml_for_animals <- function(animal_data) {
    kml_content <- ''
    all_predictions <- list()
    
    results <- data.frame(time = numeric(), i = numeric(), stringsAsFactors = FALSE)
    all_animals <- data.frame(time = numeric(), i = numeric(), stringsAsFactors = FALSE)
    all_circle <- data.frame(time = numeric(), i = numeric(), stringsAsFactors = FALSE)
    all_ellipsoid <- data.frame(time = numeric(), i = numeric(), stringsAsFactors = FALSE)
    
    animal_names <- unlist(DATA)
    for (i in seq_along(animal_data)) {
      # this is here due to me making the data not a list anymore, but that breaks the code
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
          COV_y_y = PRED$COV.y.y,
          COV_x_y = PRED$COV.x.y
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
        
        generate_ellipse_coord <- function(longitude, latitude, sigma, level = 0.95, crs_proj = NULL) {
          crs_proj <- crs_proj %||% "+proj=utm +zone=33 +datum=WGS84"

          center_coords <- project(data.frame(x = longitude, y = latitude), from = 4326, to = crs_proj)

          mu <- as.numeric(center_coords[1, ])
          sigma <- sigma
          ellipse_proj <- ellipsograph(mu, sigma, level = 0.95, PLOT = FALSE)

          ellipse_longlat <- project(ellipse_proj, from = crs_proj, to = 4326)
          as.data.frame(ellipse_longlat) %>%
            setNames(c("ellipse_longitude", "ellipse_latitude"))
        }
        
        
        
        # add raduis into each column and then use the circle coord function to 
        # generate the long lat for each circle point   
        if(error_circle == TRUE){
        processing <- processing %>%
          rowwise() %>%
          mutate(
            radius = sqrt((COV_x_x + COV_y_y) / 2),
            circle_coords = list(generate_circle_coord(longitude, latitude, radius))
          ) %>%
          ungroup()
        }
        #########################################  ERROR ellipsoid   FROM CTMM? #####################
        clamp <- function(x, lower, upper) {
          pmax(lower, pmin(x, upper))
        }
        
        ellipsograph <- function(mu, sigma, level=0.95, fg="black", bg=NA, PLOT=FALSE,num_points = circlepoints, ...) {
          Eigen <- eigen(sigma)
          std <- Eigen$values
          std[1] <- clamp(std[1], 0, Inf)
          std[2] <- clamp(std[2], 0, std[1])
          std <- sqrt(std)
          vec <- Eigen$vectors
          
          alpha <- 1 - level
          z <- sqrt(-2*log(alpha))  # confidence scale
          
          num <- circlepoints
          theta <- 2*pi*(0:num)/(num+1)
          Sin <- sin(theta)
          Cos <- cos(theta)
          
          x <- mu[1] + z*(Cos*std[1]*vec[1,1] + Sin*std[2]*vec[1,2])
          y <- mu[2] + z*(Cos*std[1]*vec[2,1] + Sin*std[2]*vec[2,2])
          
          if (PLOT) {
            plot(x, y, type = "l", asp = 1, col = fg, ...)
            points(mu[1], mu[2], pch = 19, col = "red") # Mark center
          } else {
            return(cbind(x = x, y = y))
          }
        }
        #########################################
        
        if (error_ellipses == TRUE) {
          processing <- processing %>%
            rowwise() %>%
            mutate(
              sigma = list(matrix(c(COV_x_x, COV_x_y, COV_x_y, COV_y_y), nrow = 2, byrow = TRUE)),
              ellipses_coords = list(generate_ellipse_coord(
                longitude, latitude,
                sigma = sigma,
                level = confidence,
              ))
            ) %>%
            ungroup()
          
        }
        predict_path <- cbind(PRED$longitude, PRED$latitude)
        
        initial_coords <- predict_path[[1]]
        
        
        all_predictions[[i]] <- PRED           #stores the predictions to be used in the All Tour section
        
        # creates any number of simulations the user wants, with color being set by the user.
        for (sim in 1:num_simulations) {
          lon <- simulations[[sim]]$longitude
          lat <- simulations[[sim]]$latitude
          sim_coords <- cbind(simulations[[sim]]$longitude, simulations[[sim]]$latitude)
          kml_content <- paste0(kml_content, create_path_kml(sim_coords, path_altitude, name = paste("Animal", i, "Simulation", sim), color_sim, opacity))
          kml_content <- paste0(kml_content, create_icon_kml(lon,lat, name = paste("Simulation", sim, i), color_sim, opacity,icon_image,iconsize))
        }
        animalname <- animal_names[i] 
        
        icon <- as.character(icon_image)
        # creates the main predicted path
        kml_content <- paste0(kml_content, create_path_kml(predict_path, path_altitude, name = paste("Animal", i, "Predict Path"), color_pred,opacity))
        
        kml_content <-paste0(kml_content, create_icon_kml(PRED$longitude,PRED$latitude, name = paste("Animal", i), color_icon,opacity, icon,iconsize,iconorder=2))
        
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
 kml_content <- paste0(kml_content, sprintf( '
  <Placemark id="moving_ellipsoid_%s">
   <name> Moving Animal Ellipsoid %s </name>
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
      # Pulls the timestamps and formats them
        PRED$timestamps_24hr_week <- format(as.POSIXct(PRED$timestamp, tz = "UTC"),
                                            format = "%Y-%m-%dT%H:%M:%SZ")
        
        
        
        if (simulation_icons == TRUE) { 
          num_timesteps <- nrow(simulations[[1]])
          
          for (b in 1:num_timesteps) {
            timestamp <- PRED$timestamps_24hr_week[b]
            current_row <- data.frame(time = simulations[[1]]$t[b], i = i, stringsAsFactors = FALSE)
            
            for (sim in 1:length(simulations)) {
              current_sim <- simulations[[sim]]  
              lon <- current_sim$longitude[b]
              lat <- current_sim$latitude[b]
              current_row[[paste0("Simulation_", sim)]] <- paste(lon,",",lat)
            }
            results <- rbind(results, current_row)
            
            for (sim in 1:length(simulations)) {
              current_sim <- simulations[[sim]]  
              lon <- current_sim$longitude[b]
              lat <- current_sim$latitude[b]
              
              change_section <- sprintf('
      <Placemark targetId="Simulation %d %d">
        <TimeStamp><when>%s</when></TimeStamp>
        <Point>
          <coordinates>%f,%f,0</coordinates>
        </Point>
      </Placemark>',
                                        sim, i, timestamp, lon, lat)
              
              all_change_content[[b]] <- paste0(all_change_content[[b]], change_section)
            }
          }
        }
        
        if (animal_icon == TRUE) { 
          num_timesteps <- nrow(PRED)
          
          for (b in 1:num_timesteps) {
            timestamp <- PRED$timestamps_24hr_week[b]
            current_row <- data.frame(time = PRED$t[b], i = i, stringsAsFactors = FALSE)
            lon <- PRED$longitude[b]
            lat <- PRED$latitude[b]
            current_row[["Animal"]] <- paste(lon, ",", lat)
            all_animals <- rbind(all_animals, current_row)
            
            change_section <- sprintf('
      <Placemark targetId="Animal %d">
        <TimeStamp><when>%s</when></TimeStamp>
        <Point>
          <coordinates>%f,%f,0</coordinates>
        </Point>
      </Placemark>',
                                      i, timestamp, lon, lat)
            
            all_change_content[[b]] <- paste0(all_change_content[[b]], change_section)
          }
        }
        
        if (error_circle == TRUE) { 
          num_timesteps <- nrow(processing)
          
          for (b in 1:num_timesteps) {
            timestamp <- PRED$timestamps_24hr_week[b]
            coords_str <- paste(
              apply(processing$circle_coords[[b]], 1, function(row) paste(row, collapse = ",")), 
              collapse = " "
            )     
            current_row <- data.frame(time = PRED$t[b], i = i, stringsAsFactors = FALSE)
            current_row[["Circle"]] <- paste(coords_str)
            all_circle <- rbind(all_circle, current_row)
            
            change_section <- sprintf('
      <Placemark targetId="moving_circle_%s">
        <TimeStamp><when>%s</when></TimeStamp>
        <Polygon>
          <outerBoundaryIs>
            <LinearRing>
              <coordinates>%s</coordinates>
            </LinearRing>
          </outerBoundaryIs>
        </Polygon>
      </Placemark>',
                                      i, timestamp, coords_str)
            
            all_change_content[[b]] <- paste0(all_change_content[[b]], change_section)
          }
        }
        
        if (error_ellipses == TRUE) { 
          num_timesteps <- nrow(processing)
          
          for (b in 1:num_timesteps) {
            timestamp <- PRED$timestamps_24hr_week[b]
            coords_str <- paste(
              apply(processing$ellipses_coords[[b]], 1, function(row) paste(row, collapse = ",")), 
              collapse = " "
            )     
            current_row <- data.frame(time = PRED$t[b], i = i, stringsAsFactors = FALSE)
            current_row[["Ellipsoid"]] <- paste(coords_str)
            all_ellipsoid <- rbind(all_ellipsoid, current_row)
            
            change_section <- sprintf('
      <Placemark targetId="moving_ellipsoid_%s">
        <TimeStamp><when>%s</when></TimeStamp>
        <Polygon>
          <outerBoundaryIs>
            <LinearRing>
              <coordinates>%s</coordinates>
            </LinearRing>
          </outerBoundaryIs>
        </Polygon>
      </Placemark>',
                                      i, timestamp, coords_str)
            
            all_change_content[[b]] <- paste0(all_change_content[[b]], change_section)
          }
        }
        
        if (camera_mode == "pov"){ 
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
        <gx:TimeStamp>
        <when>%s</when>
        </gx:TimeStamp>
        </LookAt>
      ',
                                      calculated_duration, lon2, lat2, azimuth,range,timestamp)
            
            all_camera_content[[b]] <- paste0(all_camera_content[[b]], change_section)
          }
        }
        if (camera_mode == "follow"){          num_timesteps <- nrow(processing)
        
        for (b in 1:num_timesteps) {
          lon <- PRED$longitude[b]
          lat <- PRED$latitude[b]
          timestamp <- PRED$timestamps_24hr_week[b]
          
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
          <heading>%f</heading>          
          <tilt>%f</tilt>                
          <range>%f</range>
          <gx:altitudeMode>relativeToGround</gx:altitudeMode>
        <gx:TimeStamp>
        <when>%s</when>
        </gx:TimeStamp>
        </LookAt>

      ',
                                    calculated_duration, lon, lat, cam_altitude, heading, tilt, range, timestamp)
          
          all_camera_content[[b]] <- paste0(all_camera_content[[b]], change_section)
          timeupdate_section <- sprintf('
        <gx:AnimatedUpdate>
          <gx:duration>0.0</gx:duration>
          <Update>
            <targetHref/>
            <Change>
              <Document targetId="doc">
                <TimeStamp>
                  <when>%s</when>
                </TimeStamp>
              </Document>
            </Change>
          </Update>
        </gx:AnimatedUpdate>', timestamp)
          
          all_camera_content[[b]] <- paste0(all_camera_content[[b]], timeupdate_section)
          
        }
        
        }
        if (camera_mode == "test"){          num_timesteps <- nrow(processing)
        
        for (b in 1:num_timesteps) {
          lon <- PRED$longitude[b]
          lat <- PRED$latitude[b]
          timestamp <- PRED$timestamps_24hr_week[b]
          
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
          <altitude>%f</altitude>       <!-- somewhat above ground, but not too high -->
          <heading>90</heading>          <!-- face east; adjust for desired direction -->
          <tilt>125</tilt>                <!-- steep angle, closer to ground level view -->
          <range>500</range> 
          <gx:altitudeMode>relativeToGround</gx:altitudeMode>
        <gx:TimeStamp>
        <when>%s</when>
        </gx:TimeStamp>
        </LookAt>

      ',
                                    calculated_duration, lon, lat, cam_altitude,timestamp)
          
          all_camera_content[[b]] <- paste0(all_camera_content[[b]], change_section)
          timeupdate_section <- sprintf('
        <gx:AnimatedUpdate>
          <gx:duration>0.0</gx:duration>
          <Update>
            <targetHref/>
            <Change>
              <Document targetId="doc">
                <TimeStamp>
                  <when>%s</when>
                </TimeStamp>
              </Document>
            </Change>
          </Update>
        </gx:AnimatedUpdate>', timestamp)
          
          all_camera_content[[b]] <- paste0(all_camera_content[[b]], timeupdate_section)
          
        }}
        if (camera_mode == "manual"){ 
          num_timesteps <- nrow(processing)
          timestamp <- PRED$timestamps_24hr_week[b]
          
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
        <gx:TimeStamp>
        <when>%s</when>
        </gx:TimeStamp>
      ',
                                      calculated_duration,timestamp)
            
            all_camera_content[[b]] <- paste0(all_camera_content[[b]], change_section)
            timeupdate_section <- sprintf('
        <gx:AnimatedUpdate>
          <gx:duration>0.0</gx:duration>
          <Update>
            <targetHref/>
            <Change>
              <Document targetId="doc">
                <TimeStamp>
                  <when>%s</when>
                </TimeStamp>
              </Document>
            </Change>
          </Update>
        </gx:AnimatedUpdate>', timestamp)
            
            all_camera_content[[b]] <- paste0(all_camera_content[[b]], timeupdate_section)
          }
        }
        # monitor message for each animated update
        message(sprintf("Writing kml fil Animal %d", i))    
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
      
      rowcount <- nrow(combined_predictions)
      
      calculated_duration <- duration /rowcount
      
      # The tour animation for each animal
      
      
      kml_content <- paste0(kml_content, sprintf('
  <gx:Tour>
    <name>Animal %s Animated Path</name>
    <gx:Playlist>
  ', animalname))
      num_timesteps <- nrow(results)
      num_column <- ncol(results) - 2
      all_change_content <- vector("list", num_timesteps)
      all_camera_content <- vector ("list", num_timesteps)
      
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
        
        
        
        for (b in 1:num_timesteps) {
          
          
          # Add animal icons content for this timestep
          i <- all_animals$i[b]
          animal_coord <- all_animals$Animal[b]
          
          change_section <- sprintf('
      <Placemark targetId="Animal %d">
        <Point>
          <coordinates>%s,0</coordinates>
        </Point>
      </Placemark>',
                                    i, animal_coord)
          
          # Add to the existing content in this row
          all_change_content[[b]] <- paste0(all_change_content[[b]], change_section)
          
        }
      }
      if (error_circle == TRUE) { 
        num_timesteps <- nrow(all_animals)
        
        for (b in 1:num_timesteps) {
          i <- all_circle$i[b]
          coords_str <- all_circle$Circle[b] 
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
          all_change_content[[b]] <- paste0(all_change_content[[b]], change_section)
        }
      }
      if (central_cam == TRUE){         
        num_timesteps <- nrow(all_animals)

        
      for (b in 1:num_timesteps) {
        
        animal_coord <- all_animals$Animal[b]
        
        current_row <- data.frame(
          time = all_animals$t[b],  # Access time column in all_animals
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
                                  calculated_duration, mid_lon, mid_lat, altitude)
        
        all_camera_content[[b]] <- paste0(all_camera_content[[b]], change_section)
        
      }
      
      }
      if (manual_cam == TRUE){ 
        num_timesteps <- nrow(all_animals)
        
        for (b in 1:num_timesteps) {

          
          change_section <- sprintf('
              
        <gx:duration>%f</gx:duration>
      ',
                                    calculated_duration)
          
          all_camera_content[[b]] <- paste0(all_camera_content[[b]], change_section)
        }
      }

      for (b in 1:nrow(combined_predictions)) {
        
        
        # monitor message for each animated update
        message(sprintf("Animating combined animal tour"))
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

      }
      kml_content <- paste0(kml_content, '
    </gx:Playlist>
  </gx:Tour>
  ')
      
      
      
      
    }
    
    
    return(kml_content)
  }
  
  if (length(DATA) > 0) {
    kml_content <- generate_kml_for_animals(DATA)
  } else {
    kml_content <- generate_kml_for_animals(list(data))
  }
  
  kml_header <- '<?xml version="1.0" encoding="UTF-8"?>
  <kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2">
  <Document id="doc">
  <name>Animal Simulation Paths</name>'
  
  kml_footer <- '</Document>
  </kml>'
  
  kml_output <- paste0(kml_header, kml_content, kml_footer)
  writeLines(kml_output, filename)
}

 data("buffalo")
 # Mvubu <- buffalo$Mvubu
  Cillia <- buffalo$Cilla
 # Pepper <- buffalo$Pepper
 # Mvubu <- Mvubu[180:200,]
  Cillia <- Cillia[180:200,]
 # Pepper <- Pepper[180:200,]
  #GUESS <- ctmm.guess(Cillia, interactive = FALSE)
  #FIT <- ctmm.fit(Cillia, GUESS, trace = TRUE)
 # GUESS2 <- ctmm.guess(Mvubu, interactive = FALSE)
 # FIT2 <- ctmm.fit(Mvubu, GUESS2, trace = TRUE)
as.kml(Cillia,
       CTMM = list(FIT),
       all_tour = FALSE,
       simulation_icons = TRUE,
       error_circle = TRUE,
       error_ellipses = TRUE,
       central_cam = FALSE,
       camera_mode = "follow",
       color_sim = "red",
       color_pred = "white",
       sequencetime = 3,
       duration = 60,
       circlepoints = 25,
       cam_altitude = 50,
       num_simulations = 10)
