# Consignes
- FuncWD
- Solea.csv

Donner aux participants les documents suivants :
- Données: [01_Original_data](Dossier complet)
- Modèle avec toutes les données. [FR_Student_AllDataModel](Dossier complet, script Student_AllDataModel.R inclus)
- Modèle sur les données positives + présence-absence + Delta. [FR_Student_PresAbs_Positive](Dossier complet, script Student_PresAbs_Positive.R inclus)
- Prédictions spatialisées. [FR_Student_HSI](Dossier complet, script Student_HSI.R inclus)

Formateur
- Toute la formation. [FR_Teacher_AllDataModel_PresAbs_Positive_HSI](Dossier complet)
- Script R - modèle toutes données. [FR_Teacher_AllDataModel](Teacher_AllDataModel.R)
- Script R - modèle Delta + Carte. [FR_Teacher_PresAbs_Positive_HSI](Teacher_PresAbs_Positive_HSI.R)

# Améliorations
## Stats
Faire un rappel préalable sur:
- ce qu'est un modèle linéaire
- ce qu'est l'ajustement d'un paramètre
- les hypothèses de construction d'un lm
- ce qu'est une distribution
- ce que sont les résidus

## R
- [ ] Passer du package {raster} à {terra}: C'est le petit frère, {raster} est obsolète. 
    + Les fonctions ont quasiment toutes les mêmes noms
- Sauver les objets spatiaux en RData pour éviter les names réduits à cause de ESRI shp
- Supprimer `gsub` de la partie Student
- Homogénéiser les noms de variables R en concordance avec le nom des jeux de données. Ou écrire directement dans le script le nom correct
- Référence figure interactions FR
-[x] AllDataModel: Uncomment (eval=TRUE) 'Analyse output of model chosen' (Student version)
- AllDataModel: GetData -> Coller les 'm' des classes de bathy (comments)
- PresAbs_positive: dataset.posit => dataset
-[x] PresAbs: "Other interesting graph" not in Student
- HSI: Data preparation à mettre après lecture Raster pour l'explication

## SIG
- Théorie des objets SIG
- Récupération données IGN
- Préparation directe des données pour la modélisation
- Lecture d'un fichier de MARS3D en netcdf + script de lecture du nouveau format
- Modifier titre du Rnw du cours de SIG pour faire des sections Rstudio
