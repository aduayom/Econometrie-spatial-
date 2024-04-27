## ----setup,message=F,warning=F----------------
knitr::opts_chunk$set(echo = TRUE)
library(sf)
library(spdep)
library(scales)
library(tmap)
library(ggplot2)
library(gridExtra)
library(knitr)
library(rgdal)
opts_knit$set(eval.after = 'fig.cap')


## ---------------------------------------------
setwd("F:/Salima/Formation 2021-2022/CIRAD/Séquences/Séquence3_avecR/2_Application2")

## ----Données,message=F,warning=F--------------
# Importation des données avec sp et avec sf
medhouse<-readOGR("medianhousing","medianhousing")
medianhousing<-st_read("medianhousing","medianhousing")


## ----Voisins Rook, fig.show="hold", out.width="50%", fig.cap=cap----
#Coordonnées
coords <- st_coordinates(st_centroid(st_geometry(medianhousing)))
# Liste des voisins pour chaque secteur de recensement avec frontière commune 
voisin_rook = poly2nb(medianhousing, queen = FALSE)
voisin_queen=poly2nb(medianhousing)
# Nombre de voisins par secteur de recensement avec frontière commune 
medianhousing$nb_voisin = card(voisin_rook)
# Représentations graphiques  avec tmap
tm_shape(medianhousing) +
  tm_polygons('nb_voisin', style = 'cat', palette = "-RdBu", title = "Nombre de voisins") 
ggplot(medianhousing, aes(x = nb_voisin, fill = as.factor(nb_voisin))) +
  geom_bar() +
  scale_fill_brewer("Nombre de voisins", palette = "RdBu", direction = -1)
cap = "<center>*Figure 1 : Répartition des secteurs de recensement du comté de Middlesex par nombre de voisins (critère rook)*</center>"


## ----Contiguité, fig.show="hold", out.width="50%", fig.cap=cap----
# Représentations graphe de contiguité
medianhousing_sp<-as(medianhousing,Class="Spatial")
plot(medianhousing_sp, border="lightgray")
plot(voisin_queen, coordinates(medianhousing_sp),add=TRUE,col="red")
plot(voisin_rook, coordinates(medianhousing_sp),add=TRUE,col="blue")
cap = "<center>*Figure 1b : Graphe de contiguïté Queen (rouge) et Rook (Bleu)*</center>"


## ----distance,,message=F,warning=F------------
#Plus proches voisins
coords<-coordinates(medianhousing_sp)
k1neigh <- knearneigh(coords, k = 1, longlat = TRUE) # 1-nearest neighbor
k2neigh <- knearneigh(coords, k = 2, longlat = TRUE) # 2-nearest neighbor
#Distance inverse
dist.mat <- as.matrix(dist(coords, method = "euclidean"))
dist.mat.inv <- 1 / dist.mat # 1 / d_{ij}
diag(dist.mat.inv) <- 0 # 0 in the diagonal
dist.mat.inv[1:5, 1:5]
dist.mat.inve <- mat2listw(dist.mat.inv, style = "W", row.names = medianhousing_sp$Name)
summary(dist.mat.inve)


## ----spplot,,message=F,warning=F--------------
spplot(medianhousing_sp, "Median_val", main = "Revenu Médian", 
       col = "transparent")
#Contrôle des couleurs
library(RColorBrewer)
my.palette <- brewer.pal(n = 7, name = "OrRd")
spplot(medianhousing_sp, "Median_val", col.regions = my.palette, cuts = 6, col = "transparent")
#Contrôle des catégories
library(classInt)
breaks.qt <- classIntervals(medianhousing_sp$Median_val, n = 6, style = "quantile", intervalClosure = "right")


## ----I de Moran, fig.align="center", fig.cap=cap----
# Test de Moran : des variantes
moran.test(medianhousing$Median_val,listw = nb2listw(voisin_rook, style = "W"))
moran.test(medianhousing$Median_val,listw = nb2listw(voisin_rook, style = "W"),randomisation=FALSE)
bperm=moran.mc(medianhousing$Median_val,listw = nb2listw(voisin_rook, style = "W"),nsim=999)
mean(bperm$res[1:999])
var(bperm$res[1:999])
summary(bperm$res[1:999])
hist(bperm$res,freq=TRUE,breaks=20,xlabs="I de Moran simulés")

# Diagramme de Moran 
moran = moran.plot(medianhousing$Median_val, listw = nb2listw(voisin_rook, style = "W"), xlab = "Revenu médian", ylab = "Décalage spatial du revenu médian")

cap = "*Figure 2 : Diagramme de Moran (pour le revenu médian)*"


## ----I de Moran local, fig.show="hold", out.width="80%"----
# Calcul des I de Moran locaux 
moran_lcl = localmoran(medianhousing$Median_val,listw = nb2listw(voisin_rook, style = "W"))
medianhousing = cbind(medianhousing,moran_lcl)

# Représentation graphique des I de Moran Locaux 
tm_shape(medianhousing) + 
  tm_polygons(col = "Ii", style = "quantile", midpoint = NA, palette = "RdBu", title = "I de Moran locaux")+
  tm_layout(main.title = "Figure 3 : I de Moran locaux des revenus médians du comté de Middlesex", main.title.position = "center", main.title.size = 0.8)

# # I de Moran locaux significatifs
# medianhousing$pval = ifelse(medianhousing$Pr.z...0. <= 0.0001, "<= 0.0001",
#                             ifelse(medianhousing$Pr.z...0. <= 0.001, "<= 0.001",
#                                    ifelse(medianhousing$Pr.z...0. <= 0.01, "<= 0.01",
#                                           ifelse(medianhousing$Pr.z...0. <= 0.05, "<= 0.05","Non significatif"))))
# 
# # Représentation graphique des I de Moran locaux significatifs 
# tm_shape(medianhousing) + 
#   tm_polygons(col = "pval", style = "quantile", title = "Seuils de significativité", midpoint = NA, palette = "-Blues")+
#   tm_layout(main.title = "Figure 4 : Carte de significativité des LISA \n(en fonction des seuils de significativité)", main.title.position = "center", main.title.size = 0.8)
# 
# LISA cluster map 
# Revenu médian centré
medianhousing$Median_val_centered = medianhousing$Median_val - mean(medianhousing$Median_val)
# I de Moran centré 
medianhousing$Ii_centered = medianhousing$Ii - mean(medianhousing$Ii)
# Seuil de significativité 
signif = 0.1
# Construction des quadrants : low-low, low-high, high-low, high-high
medianhousing$quadrant[medianhousing$Median_val_centered >0 & medianhousing$Ii_centered>0] = 4
medianhousing$quadrant[medianhousing$Median_val_centered <0 & medianhousing$Ii_centered<0] = 1
medianhousing$quadrant[medianhousing$Median_val_centered <0 & medianhousing$Ii_centered>0] = 2
medianhousing$quadrant[medianhousing$Median_val_centered >0 & medianhousing$Ii_centered<0] = 3
medianhousing$quadrant[medianhousing$Pr.z...0. > signif] = 0

# Représentation graphique des quadrants de Moran 
tm_shape(medianhousing) +
  tm_polygons("quadrant", palette = c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red"), style = "cat", title = "Quadrants", labels = c("Non significatif","Low-low","Low-high","High-low","High-high")) +
  tm_layout(main.title = "Figure 5 : Carte de significativité des LISA \n(en fonction des quadrants du diagramme de Moran)", main.title.position = "center", main.title.size = 0.8)


## ---- fig.align="center", fig.show="hold", out.width="80%"----
# Création des centroïdes
coord = st_coordinates(st_centroid(medianhousing$geometry, of_largest_polygon=TRUE))

# Liste des voisins à  une distance maximum de 12597.9 mètres du centroïde pour chaque secteur de recensement 
voisin_dist = dnearneigh(coord,0,12597.9)

# Nombre de voisins C  une distance maximum de 12597.9 mètres du centraC/de par secteur de recensement  
medianhousing$nb_voisin_dist = card(voisin_dist)

# Représentation graphique 
tm_shape(medianhousing) +
  tm_polygons('nb_voisin_dist', style = 'quantile', palette = "Blues", title = "Nombre de voisins") +
  tm_layout(main.title = "Figure 6 : Nombres de voisin à  une distance maximum \nde 12597.9 mètres du centroC/de", main.title.position = "center", main.title.size = 0.8)

# Calcul de la statistique Getis-Ord Gi
medianhousing$gstat  = localG(medianhousing$Median_val, nb2listw(voisin_dist, style = 'B'))

# Classification du z-score 
medianhousing$zscore = ifelse(medianhousing$gstat < -2.58, "a", 
                              ifelse(medianhousing$gstat <= -1.96, "b",
                                     ifelse(medianhousing$gstat <= -1.65, "c",
                                            ifelse(medianhousing$gstat <= 1.65, "d",
                                                   ifelse(medianhousing$gstat <= 1.96, "e",
                                                          ifelse(medianhousing$gstat <= 2.58, "f","g"))))))

# Représentation graphique de la répartition du z-score de la statistique 
tm_shape(medianhousing) + 
  tm_polygons("zscore", palette = "-RdYlBu", style = "cat", midpoint = NA, labels = c("< -2.58","-2.58 to -1.96","-1.96 to -1.65","-1.65 to 1.65", "1.65 to 1.96","1.96 to 2.58","> 2.58"), title = "Z-score") +
  tm_layout(main.title = "Figure 7 : Z-score de la statistique Getis-Ord des revenus médians du comté de Middlesex", main.title.position = "center", main.title.size = 0.8)


