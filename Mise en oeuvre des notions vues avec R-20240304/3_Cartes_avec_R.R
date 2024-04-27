#################################
# Faire des cartes avec R       #
#################################

# Les données ----

# Cartes statiques et interactives
# - ggplot2 (Wickham et al., 2022a), 
# - leaflet (Cheng et al., 2022a), 
# - mapview (Appelhans et al., 2022), 
# - tmap (Tennekes, 2022). 

#sudden infant deaths in the counties of North Carolina, USA, 
#in 1974 and 1979 which are in the sf package (Pebesma, 2022a).

library(sf)
nameshp <- system.file("shape/nc.shp", package = "sf")
d <- st_read(nameshp, quiet = TRUE)
d$vble <- d$SID74
d$vble2 <- d$SID79

# Avec ggplot2

library(ggplot2)
library(viridis)
ggplot(d) + geom_sf(aes(fill = vble)) +
  scale_fill_viridis() + theme_bw()

#sauvegarder les données
png("plot.png")
ggplot(d) + geom_sf(aes(fill = vble)) +
  scale_fill_viridis() + theme_bw()
dev.off()

#carte interactive
library(plotly)
g <- ggplot(d) + geom_sf(aes(fill = vble))
ggplotly(g)

# Avec leaflet ----
st_crs(d)$epsg
d <- st_transform(d, 4326)
library(leaflet)
pal <- colorNumeric(palette = "YlOrRd", domain = d$vble)
l <- leaflet(d) %>% addTiles() %>%
  addPolygons(color = "white", fillColor = ~ pal(vble),
              fillOpacity = 0.8) %>%
  addLegend(pal = pal, values = ~vble, opacity = 0.8)
l

l %>% addMiniMap()

# Saves map.html
library(htmlwidgets)
saveWidget(widget = l, file = "map.html")

# Takes a screenshot of the map.html created
# and saves it as map.png
library(webshot)
# webshot::install_phantomjs()
webshot(url = "map.html", file = "map.png")

# getwd() et setwd() pour voir où sont les fichiers
# On aurait pu mettre le chemin du fichier directement dans saveWidget() 
# file = “directory/map.html”, 
# url = “directory/map.html”. 

# Avec mapview ----

library(mapview)
mapview(d, zcol = "vble")
library(RColorBrewer)
pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))
mapview(d, zcol = "vble", map.types = "CartoDB.DarkMatter",
        col.regions = pal, layer.name = "SDI")
map1 <- mapview(d, zcol = "vble")
leaflet::addMiniMap(map1@map)

# Avec tmap ----
library(tmap)
tmap_mode("plot")
tm_shape(d) + tm_polygons("vble")

# Données ponctuelles ----
#les packages précédents permettent de représenter des données ponctuelles

library(maps)
d <- world.cities
# Select South Africa
d <- d[which(d$country.etc == "South Africa"), ]
# Transform data to sf object
d <- st_as_sf(d, coords = c("long", "lat"))
# Assign CRS
st_crs(d) <- 4326
$vble <- d$pop
d$size <- sqrt(d$vble)/100

# Map of point data created with ggplot2.
ggplot(d) + geom_sf(aes(col = vble, size = size)) +
  scale_color_viridis()

# Map of point data created with leaflet.
pal <- colorNumeric(palette = "viridis", domain = d$vble)
leaflet(d) %>% addTiles() %>%
  addCircles(lng = st_coordinates(d)[, 1],
             lat = st_coordinates(d)[, 2],
             radius = ~sqrt(vble)*10,
             color = ~pal(vble), popup = ~name) %>%
  addLegend(pal = pal, values = ~vble, position = "bottomright")

# Map of point data created with mapview.
d$size <- sqrt(d$vble)
mapview(d, zcol = "vble", cex = "size")

# Map of point data created with tmap.
tmap_mode("view")
tm_shape(d) + tm_dots("vble", scale = sqrt(d$vble)/500,
                      palette = "viridis")