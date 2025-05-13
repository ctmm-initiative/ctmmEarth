# ctmmEarth Description

as.kml will generate a kml file from a tracking dataset and a movement model fitted to that dataset’s animals. This kml file can then be used within Google Earth Pro to record a tour, outputting an animation that will follow the animal along its path. Uncertainty estimations can be made in the form of simulations and error circles.

## Usage
as.kml(animals = list(Cillia), 
       CTMM = list (Cillia_Model)
       tour = ( duration = 60,
       camera_mode = Follow_Above)
       kml_simulation = (TRUE, 
       simulation_icons = TRUE,
       num_simulations = 10))
## Arguments
animals = Telemetry objects for each animal.

CTMM = Movement model fitted to each animal, listed in the same order as Animals provided.

all_tour = Optional argument to generate an overhead tour with all animals moving

duration = How long the tour lasts

num_simulations = How many simulated paths are generated

animal_icon = Whether an animal icon is created or not

error_circle = Whether an error circle is created or not

simulation_icons = Optional argument to generate icons on the simulated paths.

pov_cam = Camera mode that will attempt to simulate the point of view of an animal.

manual_cam = Camera mode where the user will position a still view for the animation to be played from.

follow_cam = Camera mode where an overhead view will be generated following an animals path

color_sim = Color for the simulation generated

iconsize = How large icons generated are if they are generated

icon_image = Link to the icon image desired. By default, will be googles red icon.

color_icon = Color for the icon generate

color_pred = color for the predicted path

sequencetime = Sequence time used for the simulations

circlepoints = How many points are generated in the error circle

confidence = 2d confidence value given to the error circle

path_altitude = Altitude of the generated paths

cam_altitude = Altitude of the camera

Coords = Optional argument to specify if column names deviate from the format longitude, latitude, timestamp.

## Details

A movement model must be provided that fits each animal in the dataset and is ordered appropriately (CTMM = list(x,y,z) and animals = list(x,y,z)). After the kml file is generated, color, icon size, and certain placemarks' visibility can be changed within Google Earth Pro.

## Value

as.kml outputs a kml file containing the animation to the users working directory

as.kml will return a message after each animal animation is written into the kml.

