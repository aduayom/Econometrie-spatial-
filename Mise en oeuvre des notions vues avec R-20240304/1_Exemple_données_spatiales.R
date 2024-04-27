##############################################
# Les différents types de données spatiales  #
##############################################
# Données surfaciques ----
#Example of areal data. Number of sudden infant deaths in counties of North Carolina, USA, in 1974.
library(sf)
library(mapview)
d <- st_read(system.file("shape/nc.shp", package = "sf"),
             quiet = TRUE)
mapview(d, zcol = "SID74")

# Example of areal data. Household income in $1000 USD in neighborhoods in Columbus, Ohio, in 1980.
library(spData)
library(ggplot2)
d <- st_read(system.file("shapes/columbus.shp",
                         package = "spData"), quiet = TRUE)
ggplot(d) + geom_sf(aes(fill = INC))

#Example of areal data. Household income in $1000 USD in neighborhoods in Columbus, Ohio, in 1980.
library(terra)
d <- rast(system.file("ex/elev.tif", package = "terra"))
plot(d)

# Données géostatistiques ----
#Example of geostatistical data. Topsoil lead concentrations at locations sampled in a flood plain of the river Meuse, The Netherlands.

library(sp)
library(sf)
library(mapview)

data(meuse)
meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
mapview(meuse, zcol = "lead", map.types = "CartoDB.Voyager")


# Example of geostatistical data. Price per square meter of a set of apartments in Athens, Greece, in 2017.
library(spData)
mapview(properties, zcol = "prpsqm")


#Example of geostatistical data. Malaria prevalence at specific locations in Zimbabwe.
library(malariaAtlas)
d <- getPR(country = "Zimbabwe", species = "BOTH")
ggplot2::autoplot(d)

# Données ponctuelles ----
# Examples of point patterns. Top: Locations of fires in Castilla-La Mancha, Spain, between 1998 and 2007. Bottom: Locations and types of cells in a tissue.
library(spatstat)
plot(clmfires, use.marks = FALSE, pch = ".")

library(spatstat)
plot(hamster)
install.packages("sparr")

#Example of point pattern. Locations of cases and controls of primary biliary cirrhosis in north-eastern England between 1987 and 1994.
library(sparr)
data(pbc)
plot(unmark(pbc[which(pbc$marks == "case"), ]), main = "cases")
axis(1)
axis(2)
title(xlab = "Easting", ylab = "Northing")
plot(unmark(pbc[which(pbc$marks == "control"), ]),
     pch = 3, main = "controls")
axis(1)
axis(2)
title(xlab = "Easting", ylab = "Northing")