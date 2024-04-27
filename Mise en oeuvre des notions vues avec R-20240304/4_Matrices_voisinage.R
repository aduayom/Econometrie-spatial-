###############################################
# Données surfaciques : matrices de voisinage #
###############################################

# Les données ----
library(spData)
library(sf)
library(spdep)
library(ggplot2)
map <- st_read(system.file("shapes/columbus.shp",
                           package = "spData"), quiet = TRUE)

# La contiguité ----
library(spdep)
nb <- spdep::poly2nb(map, queen = TRUE)
head(nb)

plot(st_geometry(map), border = "lightgray")
plot.nb(nb, st_geometry(map), add = TRUE)

#visualisation des voisins d'un polygone donnée
id <- 20 # area id
map$neighbors <- "other"
map$neighbors[id] <- "area"
map$neighbors[nb[[id]]] <- "neighbors"
ggplot(map) + geom_sf(aes(fill = neighbors)) + theme_bw() +
  scale_fill_manual(values = c("gray30", "gray", "white"))

# Les k plus proches voisins ----
# Les 3 plus proches voisins
coo <- st_centroid(map)
nb <- knn2nb(knearneigh(coo, k = 3)) # k number nearest neighbors
plot(st_geometry(map), border = "lightgray")
plot.nb(nb, st_geometry(map), add = TRUE)

# Voisinage fondée sur la distance ----
# Neighbors based on distance
nb <- dnearneigh(x = st_centroid(map), d1 = 0, d2 = 0.4)
plot(st_geometry(map), border = "lightgray")
plot.nb(nb, st_geometry(map), add = TRUE)

#Pour que chacun ait au moins un voisin
coo <- st_centroid(map)
# k is the number nearest neighbors
nb1 <- knn2nb(knearneigh(coo, k = 1))
dist1 <- nbdists(nb1, coo)
summary(unlist(dist1))

# Les matrices de pondération spatiale ----
# Spatial weights matrix based on a binary neighbor list
# - nb list with neighbors,
# - style indicates the coding scheme chosen. 
#   For example, style =B is the basic binary coding, 
#   and W is row standardized (1/number of neighbors),
# - zero.policy is used to take into account regions with 0 neighbors. 

nb <- poly2nb(map, queen = TRUE)
nbw <- spdep::nb2listw(nb, style = "W")
nbw$weights[1:3]

m1 <- listw2mat(nbw)
lattice::levelplot(t(m1),
                   scales = list(y = list(at = c(10, 20, 30, 40),
                                          labels = c(10, 20, 30, 40))))

#Spatial weights matrix based on inverse distance values

coo <- st_centroid(map)
nb <- poly2nb(map, queen = TRUE)
dists <- nbdists(nb, coo)
ids <- lapply(dists, function(x){1/x})

nbw <- nb2listw(nb, glist = ids, style = "B")
nbw$weights[1:3]

m2 <- listw2mat(nbw)
lattice::levelplot(t(m2),
                   scales = list(y = list(at = c(10, 20, 30, 40),
                                          labels = c(10, 20, 30, 40))))