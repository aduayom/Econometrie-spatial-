#############################
# Autocorrélation Spatiale  #
#############################

# Test de Moran ----
library(spData)
library(sf)
library(mapview)
map <- st_read(system.file("shapes/boston_tracts.shp",
                           package = "spData"), quiet = TRUE)
map$vble <- map$MEDV
mapview(map, zcol = "vble")

# Neighbors
library(spdep)
nb <- poly2nb(map, queen = TRUE) # queen shares point or border
nbw <- nb2listw(nb, style = "W")

# Global Moran's I
gmoran <- moran.test(map$vble, nbw,
                     alternative = "greater")
gmoran

gmoran[["estimate"]][["Moran I statistic"]] # Moran's I
gmoran[["statistic"]] # z-score
gmoran[["p.value"]] # p-value



#Monte-Carlo simulation of Moran I
gmoranMC <- moran.mc(map$vble, nbw, nsim = 999)
gmoranMC

hist(gmoranMC$res)
abline(v = gmoranMC$statistic, col = "red")

# Diagramme de Moran 
moran.plot(map$vble, nbw)

# I de Moran Locaux : LISA
# - Ii: Local Moran's I statistic for each area,
# - E.Ii: Expectation Local Moran's I statistic,
# - Var.Ii: Variance Local Moran's I statistic,
# - Z.Ii: z-score,
# - Pr(z > E(Ii)), Pr(z < E(Ii)) or Pr(z != E(Ii)): p-value for an alternative hypothesis greater, less or two.sided, respectively.


lmoran <- localmoran(map$vble, nbw, alternative = "greater")
head(lmoran)

# Représentations graphiques
library(tmap)
tmap_mode("plot")

map$lmI <-   lmoran[, "Ii"] #   local Moran's I
map$lmZ <-   lmoran[, "Z.Ii"]   # z-scores
# p-values   corresponding to   alternative greater
map$lmp <-   lmoran[, "Pr(z > E(Ii))"]

p1 <- tm_shape(map) +
  tm_polygons(col = "vble", title = "vble", style = "quantile") +
  tm_layout(legend.outside = TRUE)

p2 <- tm_shape(map) +
  tm_polygons(col = "lmI", title = "Local Moran's I",
              style = "quantile") +
  tm_layout(legend.outside = TRUE)

p3 <- tm_shape(map) +
  tm_polygons(col = "lmZ", title = "Z-score",
              breaks = c(-Inf, 1.65, Inf)) +
  tm_layout(legend.outside = TRUE)

p4 <- tm_shape(map) +
  tm_polygons(col = "lmp", title = "p-value",
              breaks = c(-Inf, 0.05, Inf)) +
  tm_layout(legend.outside = TRUE)

tmap_arrange(p1, p2, p3, p4)

# Valeurs significatives
tm_shape(map) + tm_polygons(col = "lmZ",
                            title = "Local Moran's I", style = "fixed",
                            breaks = c(-Inf, -1.96, 1.96, Inf),
                            labels = c("Negative SAC", "No SAC", "Positive SAC"),
                            palette = c("blue", "white", "red")) +
  tm_layout(legend.outside = TRUE)

# Clusters sur le diagramme de Moran ----

# Local Moran's I allows us to identify clusters of the following types:
# 
# High-High: areas of high values with neighbors of high values,
# High-Low: areas of high values with neighbors of low values,
# Low-High: areas of low values with neighbors of high values,
# Low-Low: areas of low values with neighbors of low values.

lmoran <- localmoran(map$vble, nbw, alternative = "two.sided")
head(lmoran)

map$lmp <- lmoran[, 5] # p-values are in column 5

mp <- moran.plot(as.vector(scale(map$vble)), nbw)

head(mp)

map$quadrant <- NA
# high-high
map[(mp$x >= 0 & mp$wx >= 0) & (map$lmp   <= 0.05), "quadrant"]<- 1
# low-low
map[(mp$x <= 0 & mp$wx <= 0) & (map$lmp   <= 0.05), "quadrant"]<- 2
# high-low
map[(mp$x >= 0 & mp$wx <= 0) & (map$lmp   <= 0.05), "quadrant"]<- 3
# low-high
map[(mp$x <= 0 & mp$wx >= 0) & (map$lmp   <= 0.05), "quadrant"]<- 4
# non-significant
map[(map$lmp > 0.05), "quadrant"] <- 5

# Hot Spots et Cold spots ----
tm_shape(map) + tm_fill(col = "quadrant", title = "",
                        breaks = c(1, 2, 3, 4, 5, 6),
                        palette = c("red", "blue", "lightpink", "skyblue2", "white"),
                        labels = c("High-High", "Low-Low", "High-Low",
                                   "Low-High", "Non-significant")) +
  tm_legend(text.size = 1) + tm_borders(alpha = 0.5) +
  tm_layout(frame = FALSE, title = "Clusters") +
  tm_layout(legend.outside = TRUE)