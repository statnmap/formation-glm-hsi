# formation-glm

The goal of formation-glm is to ...

Utiliser "KnitOrPurl.R" pour préparer les supports selon vos envies.

```
# CASES ------------------------------------------------------------------------
# Create a HTML only with Alldata and another for Delta model
ClassicWay <- eval.AllDataExploration <- eval.AllDataModel <-
  eval.PresAbsModel <- eval.PositiveModel <- eval.HSI <- FALSE

# -- Choose your way --
# === Normal way - 2 days :
ClassicWay <- TRUE
# -- DAY 1 : set to TRUE --
eval.AllDataExploration <- eval.AllDataModel <- TRUE
# -- DAY 2 : set to TRUE --
eval.PresAbsModel <- eval.PositiveModel <- FALSE
eval.HSI <- FALSE

# === Rapid way - 1 day :
ClassicWay <- FALSE
eval.PositiveModel <- FALSE
eval.HSI <- FALSE
# -- Morning + Begin Afternoon : set to TRUE --
eval.AllDataExploration <- eval.AllDataModel <- FALSE
# -- Afternoon : set to TRUE --
eval.PresAbsModel <- TRUE
```
