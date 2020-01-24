Ce programme réalise une classification supervisée de quatres images multispectrales sur Vientiane entre 1989 et 2019 issues des satellites Landsat 5 et Landsat 8 à l'aide de l'algorithme CART. Les données d'images utilisées sont fournies dans le dossier img. L'exécution complète du script enregistre dans le dossier out les quatres images classifiées en TIF, ainsi qu'une carte de chaque image au format png. Des graphiques d'évolution des surfaces sont également exportées au format PNG, ainsi que les matrice de confusion.

# Entrées

* Images Landsat : fournies dans le dossier img sous le nom img_XXXX.tif.
* Fichier de classes : utilisé pour définir les classes, leurs identifiants et couleurs. Fourni sous le nom classes.csv. 
* Points de contrôles de classification : fournis dans le dossier pois sous le nom pois_XXXX.gpkg.
* Points de validation : fournis dans dossier pois sous le nom validation.gpkg.

# Sorties (dossier out)

* Images classifiées : classification_XXXX.tif
* Cartes des classification : mapXXXX.png
* Matrice de confusion : confusion_XXXX.csv
* Graphique de surface par classe : plotXXXX.png
* Évolution des surfaces par classe sur 30 ans : plot_evolution.png
* Taux d'évolution des surfaces par classe : taux_evolution.csv
* Comparaison des surfaces végétalisées détectées par NDVI et par classification : surf_veg.csv
