#########################################
# Les données spatiales avec R          #
#########################################

# Les fichiers de forme ----
library(sf)
pathshp <- system.file("shape/nc.shp", package = "sf")
# Qu'en est-il ?
dir("C:/Users/sbouayad/AppData/Local/R/win-library/4.2/sf/shape")

#lecture
map <- st_read(pathshp, quiet = TRUE)
class(map)

head(map)

plot(map[1]) # plot first attribute

# Les coordonnées ----

st_crs("EPSG:4326")$Name
st_crs("EPSG:4326")$proj4string
st_crs("EPSG:4326")$epsg

#Functions sf::st_crs() allow us to get the CRS of spatial data.
library(sf)
pathshp <- system.file("shape/nc.shp", package = "sf")
map <- st_read(pathshp, quiet = TRUE)

# Get CRS
# st_crs(map)
# Transform CRS
map2 <- st_transform(map, crs = "EPSG:4326")
# Get CRS
# st_crs(map2)

# les anciens packages ----
# Before the sf package was developed, the sp package was used to represent and 
# work with vector spatial data. sp as well as the rgdal (Bivand et al., 2023), 
# rgeos (Bivand and Rundel, 2022) and maptools (Bivand and Lewin-Koh, 2022) 
# packages are no longer maintained and will retire. Using old packages, 
# the rgdal::readOGR() function can be used to read a file. 
# Data can be accessed with sp_object@data, and the sp::spplot() 
# function can be used to plot sp spatial objects.

library(sf)
library(sp)
library(rgdal)
pathshp <- system.file("shape/nc.shp", package = "sf")
sp_object <- rgdal::readOGR(pathshp, verbose = FALSE)
class(sp_object)

# The st_as_sf() function of sf can be used to transform a sp object to a 
# sf object (st_as_sf(sp_object)). Also, a sf object can be transformed 
# to a sp object with as(sf_object, “Spatial”).

# Le package sf ----
# The sf package (Pebesma, 2022a) can be used to represent and work with spatial 
# vector data including points, polygons, and lines, and their associated 
# information. The sf package uses sf objects that are extensions 
# of data frames containing a collection of simple features or spatial objects 
# with possibly associated data.
# We can read a sf object with the st_read() function of sf. For example, here we read the nc shapefile of sf which contains the counties of North Carolina, USA, as well as their name, 
# number of births, and number of sudden infant deaths in 1974 and 1979.

library(sf)
pathshp <- system.file("shape/nc.shp", package = "sf")
nc <- st_read(pathshp, quiet = TRUE)
class(nc)

# - sf (simple feature): each row of the data.frame is a single simple feature 
# consisting of attributes and geometry.
# - sfc (simple feature geometry list-column): the geometry column of the data.frame 
# is a list-column of class sfc with the geometry of each simple feature.
# - sfg (simple feature geometry): each of the rows of the sfc list-column corresponds 
# to the simple feature geometry (sfg) of a single simple feature

print(nc)

plot(nc)

# on peut manipuler les objets
nc[1, ] # first row
nc[nc$NAME == "Ashe", ] # row with NAME "Ashe"
nc[1, "NWBIR74"] # first row, column with name NWBIR74
nc[1, "NWBIR74", drop = TRUE] # drop geometry

# Geometries printed in abbreviated form
st_geometry(nc)
# View complete geometry by selecting one
st_geometry(nc)[[1]]

# Les principales fonctions de sf ----

# st_read() reads a sf object,
# st_write() writes a sf object,
# st_crs() gets or sets a new coordinate reference system (CRS),
# st_transform() transforms data to a new CRS,
# st_intersection() intersects sf objects,
# st_union() combines several sf objects into one,
# st_simplify() simplifies a sf object,
# st_coordinates() retrieves coordinates of a sf object,
# st_as_sf() converts a foreign object to a sf object.

library(sf)
library(ggplot2)
map <- read_sf(system.file("shape/nc.shp", package = "sf"))
head(map)

# Delete polygon
map <- map[-which(map$FIPS %in% c("37125", "37051")), ]
ggplot(map) + geom_sf(aes(fill = SID79))

# Combine geometries
ggplot(st_union(map, by_feature = FALSE) %>% st_sf()) + geom_sf()

# Simplify
ggplot(st_simplify(map, dTolerance = 10000)) + geom_sf()

# Transformer des données ponctuelles en objet sf

library(sf)
library(mapview)

d <- data.frame(
  place = c("London", "Paris", "Madrid", "Rome"),
  long = c(-0.118092, 2.349014, -3.703339, 12.496366),
  lat = c(51.509865, 48.864716, 40.416729, 41.902782),
  value = c(200, 300, 400, 500))
class(d)


dsf <- st_as_sf(d, coords = c("long", "lat"))
st_crs(dsf) <- 4326
class(dsf)

# sf object with points.
mapview(dsf)