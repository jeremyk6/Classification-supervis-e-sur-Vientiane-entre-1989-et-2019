# Classification d'images issues de satellites Landsat 5 et 8
# Authors: Jérémy Kalsron
#          Mathilde Argoud
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(tidyverse)  # pour read_csv et ggplot2
library(raster)     # pour lire les rasters et faire les compositions
library(rasterVis)  # pour visualiser les rasters
library(rgdal)      # pour lire les pois
library(rpart)      # pour la classification CART
library(caret)      # pour la matrice de confusion

writePng = function(plot, filename) {
  png(filename)
  print(plot)
  dev.off()
}

# 1989
r1989 = raster("img/img_1989.tif", band = 3)
g1989 = raster("img/img_1989.tif", band = 2)
b1989 = raster("img/img_1989.tif", band = 1)
ir1989 = raster("img/img_1989.tif", band = 4)
rgb1989 = stack(r1989, g1989, b1989)
irc1989 = stack(ir1989, r1989, g1989)
ndvi_1989 = (ir1989 - r1989) / (ir1989 + r1989)
# plotRGB(rgb1989, axes = FALSE, stretch = "lin", main = "Vraie couleur")
# plotRGB(irc1989, axes = FALSE, stretch = "lin", main = "Infrarouge fausse couleur")

# 1999
r1999 = raster("img/img_1999.tif", band = 3)
g1999 = raster("img/img_1999.tif", band = 2)
b1999 = raster("img/img_1999.tif", band = 1)
ir1999 = raster("img/img_1999.tif", band = 4)
rgb1999 = stack(r1999, g1999, b1999)
irc1999 = stack(ir1999, r1999, g1999)
ndvi_1999 = (ir1999 - r1999) / (ir1999 + r1999)
# plotRGB(rgb1999, axes = FALSE, stretch = "lin", main = "Vraie couleur")
# plotRGB(irc1999, axes = FALSE, stretch = "lin", main = "Infrarouge fausse couleur")

# 2009
r2009 = raster("img/img_2009.tif", band = 3)
g2009 = raster("img/img_2009.tif", band = 2)
b2009 = raster("img/img_2009.tif", band = 1)
ir2009 = raster("img/img_2009.tif", band = 4)
rgb2009 = stack(r2009, g2009, b2009)
irc2009 = stack(ir2009, r2009, g2009)
ndvi_2009 = (ir2009 - r2009) / (ir2009 + r2009)
# plotRGB(rgb2009, axes = FALSE, stretch = "lin", main = "Vraie couleur")
# plotRGB(irc2009, axes = FALSE, stretch = "lin", main = "Infrarouge fausse couleur")

# 2019
r2019 = raster("img/img_2019.tif", band = 3)
g2019 = raster("img/img_2019.tif", band = 2)
b2019 = raster("img/img_2019.tif", band = 1)
ir2019 = raster("img/img_2019.tif", band = 4)
rgb2019 = stack(r2019, g2019, b2019)
irc2019 = stack(ir2019, r2019, g2019)
ndvi_2019 = (ir2019 - r2019) / (ir2019 + r2019)
# plotRGB(rgb2019, axes = FALSE, stretch = "lin", main = "Vraie couleur")
# plotRGB(irc2019, axes = FALSE, stretch = "lin", main = "Infrarouge fausse couleur")

# Lecture du fichier de correspondance id/classe
classes = read_csv("classes.csv")

# Lecture du fichier de validation pour les matrices de confusion
val = readOGR("pois/validation.gpkg")

#
# Classification pour l'image 1989
#

# Lecture du fichier de pois
pois_1989 = readOGR("pois/pois_1989.gpkg")

# Échantillonage des valeurs du raster aux pois
samples = raster::extract(irc1989, pois_1989, df = TRUE)
samples = samples[, -1] # Suppression du champ ID

# Jointure des échantillons avec la classe correspondante
samples_df = data.frame(classvalue = pois_1989$sclass, samples)

# Création du modèle de classification par arbre de décision (algorithme CART)
cart = rpart(as.factor(classvalue)~., data=samples_df, method = 'class', minsplit = 5)
# Affichage de l'arbre
plot(cart, uniform=TRUE, main="Arbre de classification")
text(cart, cex = 0.8)

# Classification selon le modèle établi
pr1989 = predict(irc1989, cart, type='class')

# Filtre majoritaire pour lisser le résultat
pr1989 = focal(pr1989, matrix(1, 3, 3), fun = modal)

# Affichage de l'image classifiée
pr1989 = ratify(pr1989)
levels(pr1989) = data.frame("ID" = classes$sID, "legend" = classes$Nom_sID)
plot = levelplot(pr1989, maxpixels = 1e6, col.regions = classes$Couleur_sID, scales=list(draw=FALSE), main="Vientiane en 1989")
writePng(plot, "out/map1989.png")

# Écriture de l'image classifiée
writeRaster(pr1989, "out/classification_1989.tif", overwrite=TRUE)

# Matrice de confusion

# Échantillonage des valeurs de la classification aux pois de validation
val1989 = raster::extract(pr1989, val, df = TRUE)
val1989 = val1989[, -1] # Suppression du champ ID
val1989 = data.frame(classification = val$sID_1989, validation = val1989)

fclass = factor(val1989$classification, levels = classes$sID, labels = classes$Nom_sID)
fval = factor(val1989$validation, levels = classes$sID, labels = classes$Nom_sID)

c1989 = confusionMatrix(fclass, fval)
write.csv(c1989$table, "out/confusion_1989.csv")

# Surface par classe

surf1989 = data.frame(freq(pr1989)) %>% inner_join(classes, by = c("value" = "sID")) %>% dplyr::select(Nom_sID, nb = count, color = Couleur_sID)
surf1989$surface = (surf1989$nb * xres(pr1989) * xres(pr1989))/1000000
ggplot(surf1989, aes(x=factor(Nom_sID, level=Nom_sID), y=surface, fill=color)) + 
  geom_bar(position="dodge", stat="identity", fill=surf1989$color) +
  labs(title = "Surface par classe en 1989", x= "", y = "Surface (km²)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("out/plot1989.png")

#
# Classification pour l'image 1999
#

# Lecture du fichier de pois
pois_1999 = readOGR("pois/pois_1999.gpkg")

# Échantillonage des valeurs du raster aux pois
samples = raster::extract(irc1999, pois_1999, df = TRUE)
samples = samples[, -1] # Suppression du champ ID

# Jointure des échantillons avec la classe correspondante
samples_df = data.frame(classvalue = pois_1999$sclass, samples)

# Création du modèle de classification par arbre de décision (algorithme CART)
cart = rpart(as.factor(classvalue)~., data=samples_df, method = 'class', minsplit = 5)
# Affichage de l'arbre
plot(cart, uniform=TRUE, main="Arbre de classification")
text(cart, cex = 0.8)

# Classification selon le modèle établi
pr1999 = predict(irc1999, cart, type='class')

# Filtre majoritaire pour lisser le résultat
pr1999 = focal(pr1999, matrix(1, 3, 3), fun = modal)

# Affichage de l'image classifiée
pr1999 = ratify(pr1999)
levels(pr1999) = data.frame("ID" = classes$sID, "legend" = classes$Nom_sID)
plot = levelplot(pr1999, maxpixels = 1e6, col.regions = classes$Couleur_sID, scales=list(draw=FALSE), main="Vientiane en 1999")
writePng(plot, "out/map1999.png")

# Écriture de l'image classifiée
writeRaster(pr1999, "out/classification_1999.tif", overwrite=TRUE)

# Matrice de confusion

# Échantillonage des valeurs de la classification aux pois de validation
val1999 = raster::extract(pr1999, val, df = TRUE)
val1999 = val1999[, -1] # Suppression du champ ID
val1999 = data.frame(classification = val$sID_1999, validation = val1999)

fclass = factor(val1999$classification, levels = classes$sID, labels = classes$Nom_sID)
fval = factor(val1999$validation, levels = classes$sID, labels = classes$Nom_sID)

c1999 = confusionMatrix(fclass, fval)
write.csv(c1999$table, "out/confusion_1999.csv")

# Surface par classe

surf1999 = data.frame(freq(pr1999)) %>% inner_join(classes, by = c("value" = "sID")) %>% dplyr::select(Nom_sID, nb = count, color = Couleur_sID)
surf1999$surface = (surf1999$nb * xres(pr1999) * xres(pr1999))/1000000
ggplot(surf1999, aes(x=factor(Nom_sID, level=Nom_sID), y=surface, fill=color)) + 
  geom_bar(position="dodge", stat="identity", fill=surf1999$color) +
  labs(title = "Surface par classe en 1999", x= "", y = "Surface (km²)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("out/plot1999.png")

#
# Classification pour l'image 2009
#

# Lecture du fichier de pois
pois_2009 = readOGR("pois/pois_2009.gpkg")

# Échantillonage des valeurs du raster aux pois
samples = raster::extract(irc2009, pois_2009, df = TRUE)
samples = samples[, -1] # Suppression du champ ID

# Jointure des échantillons avec la classe correspondante
samples_df = data.frame(classvalue = pois_2009$sclass, samples)

# Création du modèle de classification par arbre de décision (algorithme CART)
cart = rpart(as.factor(classvalue)~., data=samples_df, method = 'class', minsplit = 5)
# Affichage de l'arbre
plot(cart, uniform=TRUE, main="Arbre de classification")
text(cart, cex = 0.8)

# Classification selon le modèle établi
pr2009 = predict(irc2009, cart, type='class')

# Filtre majoritaire pour lisser le résultat
pr2009 = focal(pr2009, matrix(1, 3, 3), fun = modal)

# Affichage de l'image classifiée
pr2009 = ratify(pr2009)
levels(pr2009) = data.frame("ID" = classes$sID, "legend" = classes$Nom_sID)
plot = levelplot(pr2009, maxpixels = 1e6, col.regions = classes$Couleur_sID, scales=list(draw=FALSE), main="Vientiane en 2009")
writePng(plot, "out/map2009.png")

# Écriture de l'image classifiée
writeRaster(pr2009, "out/classification_2009.tif", overwrite=TRUE)

# Matrice de confusion

# Échantillonage des valeurs de la classification aux pois de validation
val2009 = raster::extract(pr2009, val, df = TRUE)
val2009 = val2009[, -1] # Suppression du champ ID
val2009 = data.frame(classification = val$sID_2009, validation = val2009)

fclass = factor(val2009$classification, levels = classes$sID, labels = classes$Nom_sID)
fval = factor(val2009$validation, levels = classes$sID, labels = classes$Nom_sID)

c2009 = confusionMatrix(fclass, fval)
write.csv(c2009$table, "out/confusion_2009.csv")

# Surface par classe

surf2009 = data.frame(freq(pr2009)) %>% inner_join(classes, by = c("value" = "sID")) %>% dplyr::select(Nom_sID, nb = count, color = Couleur_sID)
surf2009$surface = (surf2009$nb * xres(pr2009) * xres(pr2009))/1000000
ggplot(surf2009, aes(x=factor(Nom_sID, level=Nom_sID), y=surface, fill=color)) + 
  geom_bar(position="dodge", stat="identity", fill=surf2009$color) +
  labs(title = "Surface par classe en 2009", x= "", y = "Surface (km²)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("out/plot2009.png")

#
# Classification pour l'image 2019
#

# Lecture du fichier de pois
pois_2019 = readOGR("pois/pois_2019.gpkg")

# Échantillonage des valeurs du raster aux pois
samples = raster::extract(irc2019, pois_2019, df = TRUE)
samples = samples[, -1] # Suppression du champ ID

# Jointure des echantillons avec la classe correspondante
samples_df = data.frame(classvalue = pois_2019$sclass, samples)

# Création du modèle de classification par arbre de décision (algorithme CART)
cart = rpart(as.factor(classvalue)~., data=samples_df, method = 'class', minsplit = 5)
# Affichage de l'arbre
plot(cart, uniform=TRUE, main="Arbre de classification")
text(cart, cex = 0.8)

# Classification selon le modèle établi
pr2019 = predict(irc2019, cart, type='class')

# Filtre majoritaire pour lisser le résultat
pr2019 = focal(pr2019, matrix(1, 3, 3), fun = modal)

# Affichage de l'image classifiée
pr2019 = ratify(pr2019)
levels(pr2019) = data.frame("ID" = classes$sID, "legend" = classes$Nom_sID)
plot = levelplot(pr2019, maxpixels = 1e6, col.regions = classes$Couleur_sID, scales=list(draw=FALSE), main="Vientiane en 2019")
writePng(plot, "out/map2019.png")

# Écriture de l'image classifiée
writeRaster(pr2019, "out/classification_2019.tif", overwrite=TRUE)

# Matrice de confusion

# Échantillonage des valeurs de la classification aux pois de validation
val2019 = raster::extract(pr2019, val, df = TRUE)
val2019 = val2019[, -1] # Suppression du champ ID
val2019 = data.frame(classification = val$sID_2019, validation = val2019)

fclass = factor(val2019$classification, levels = classes$sID, labels = classes$Nom_sID)
fval = factor(val2019$validation, levels = classes$sID, labels = classes$Nom_sID)

c2019 = confusionMatrix(fclass, fval)
write.csv(c2019$table, "out/confusion_2019.csv")

# Surface par classe

surf2019 = data.frame(freq(pr2019)) %>% inner_join(classes, by = c("value" = "sID")) %>% dplyr::select(Nom_sID, nb = count, color = Couleur_sID)
surf2019$surface = (surf2019$nb * xres(pr2019) * xres(pr2019))/1000000
ggplot(surf2019, aes(x=factor(Nom_sID, level=Nom_sID), y=surface, fill=color)) + 
  geom_bar(position="dodge", stat="identity", fill=surf2019$color) +
  labs(title = "Surface par classe en 2019", x= "", y = "Surface (km²)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("out/plot2019.png")

# Évolution des surfaces par classe sur 30 ans

surf =  (dplyr::select(surf1989, Nom_sID, nbPixels = nb, surface) %>% mutate(annee = "1989")) %>%
  rbind((dplyr::select(surf1999, Nom_sID, nbPixels = nb, surface) %>% mutate(annee = "1999"))) %>% 
  rbind((dplyr::select(surf2009, Nom_sID, nbPixels = nb, surface) %>% mutate(annee = "2009"))) %>%
  rbind((dplyr::select(surf2019, Nom_sID, nbPixels = nb, surface) %>% mutate(annee = "2019")))

ggplot(surf, aes(x=Nom_sID, y=surface, fill=annee)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Évolution des surfaces par classe et par année", x= "", y = "Surface (km²)", fill = "Année") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("out/plot_evolution.png")

# Taux d'évolution des surfaces par classe

tx_evo = data.frame()
l_annee = unique(surf$annee)
for(i in 2:length(l_annee)-1) { # Pour chaque année
  for(classe in classes$Nom_sID) { # Pour chaque classe
    s = NA
    a1 = (surf %>% filter(Nom_sID == classe, annee == l_annee[i]) %>% dplyr::select(surface))$surface
    a2 = (surf %>% filter(Nom_sID == classe, annee == l_annee[i+1]) %>% dplyr::select(surface))$surface
    if(!is_empty(a1) && !is_empty(a2)) { # On vérifie qu'aucune des deux années n'est vide
      s = ((a2-a1)/a1)*100 # On calcule le taux d'évolution pour l'intervalle d'année
    }
    tx_evo = rbind(tx_evo, data.frame(classe = classe, annees = paste(l_annee[i], l_annee[i+1], sep="-"), taux = s))
  }
}
write.csv(tx_evo, "out/taux_evolution.csv")

# Comparaison surfaces végétalisées NDVI et classifiée

surfVeg = rbind(
  data.frame(freq(ndvi_1989 > 0.3)) %>% filter(value == 1) %>% mutate(annee = "1989", surfaceNDVI = (count*xres(pr1989)*xres(pr1989))/1000000) %>% dplyr::select(annee, nbPixelsNDVI = count, surfaceNDVI),
  data.frame(freq(ndvi_1999 > 0.3)) %>% filter(value == 1) %>% mutate(annee = "1999", surfaceNDVI = (count*xres(pr1999)*xres(pr1999))/1000000) %>% dplyr::select(annee, nbPixelsNDVI = count, surfaceNDVI),
  data.frame(freq(ndvi_2009 > 0.3)) %>% filter(value == 1) %>% mutate(annee = "2009", surfaceNDVI = (count*xres(pr2009)*xres(pr2009))/1000000) %>% dplyr::select(annee, nbPixelsNDVI = count, surfaceNDVI),
  data.frame(freq(ndvi_2019 > 0.4)) %>% filter(value == 1) %>% mutate(annee = "2019", surfaceNDVI = (count*xres(pr2019)*xres(pr2019))/1000000) %>% dplyr::select(annee, nbPixelsNDVI = count, surfaceNDVI)
) %>% inner_join(
  surf %>% filter(Nom_sID %in% c("Forêt", "Culture en activité")) %>% group_by(annee) %>% summarise(nbPixelsClass = sum(nbPixels), surfaceClass = sum(surface)),
)
write.csv(surfVeg, "out/surf_veg.csv")

