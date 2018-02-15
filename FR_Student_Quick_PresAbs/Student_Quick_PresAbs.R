# Tutorial : SpatialGLM
# Maintainer : S. Rochette, StatnMap, sebastienrochettefr@gmail.com
# Citation : Rochette, S., Rivot, E., Morin, J., Mackinson, S., Riou, P., Le Pape, O. (2010). Effect of nursery habitat degradation on flatfish population renewal. Application to Solea solea in the Eastern Channel (Western Europe). Journal of sea Research, 64.
# Version : 2017
# Encoding UTF-8

  # Generated with R and knitr : R version - Students


# Clean memory
# rm(list = ls())

# Load libraries ---------------------------------------------------------------
library(sp)
library(rgdal)
library(raster)
library(maps)
library(mapdata)
library(dplyr)
library(here)

# Define working directories ---------------------------------------------------
WD <- here()
# Folder of original files
origWD <- here("01_Original_data")
# Folder for outputs
saveWD <- here("02_Outputs")
# Folder where to save outputs from R
figWD <- here("03_Figures")
# Folder where complementary functions are stored
funcWD <- here("04_Functions")

# Other libraries ----
library(lattice) #to improve graphics
library(MASS)
library(ggplot2)
# devtools::install_github('bhaskarvk/widgetframe')
library(leaflet)
library(ggmap)

# Download other personnal graphic functions
source(paste0(funcWD, "/function.Gamma.Hist.R"))
source(paste0(funcWD, "/function.chisq.gof.R"), encoding = "Latin1")


# Presence-Absence Model =======================================================
# Additionnal libraries ----
library(pROC)

# Get data =====================================================================
# Load Coastlines shapefile
Coasts_Vilaine_wgs84 <- readOGR(...) 

# Load the data shapefile
Stations_covariates_wgs84 <- readOGR(...)
names(Stations_covariates_wgs84)[9:16] <- 
  c("SeaLim", "Coast", "Zone_bio", "Sedim", "id", "Bathy", "Pente", "TPI")

# Transform to non-spatial dataset
dataset <- as.tbl(Stations_covariates_wgs84@data)

# Clean dataset names
names(dataset) <- gsub("_pt", "", names(dataset))

# Add a new covariate: bathymetry as class factor covariate using cut
# c("1depth < 5m", "2depth 5-10m", "3depth 10-20 m", "4depth 20-50 m")
dataset$Bathy_c <- cut(
  -dataset$Bathy, 
  breaks = c(min(-dataset$Bathy), 5, 10, 20, 50),
  labels = c("1depth < 5m", "2depth 5-10m", "3depth 10-20 m", "4depth 20-50 m"),
  include.lowest = TRUE
)
# Add a new column for Presence-absence
dataset$Presence <- ...

# Explore Presence/Absence data ================================================
# _Balance of data against covariates ----
table(dataset$Presence)
table(...)

# _Covariates potential effect ----
# Histograms (ggplot2)
g <- ggplot(dataset) +
  geom_histogram(aes(Presence))
plot(g)

# Mean by class of factor
barplot(tapply(dataset$Presence, dataset$Year, mean), 
        xaxt = "n", space = 0)
axis(1, at = 1:length(levels(dataset$Year)) - 0.5,
     labels = levels(dataset$Year), cex.axis = 0.6)

# _Interactions between covariates ----
interaction.plot(...)

# Fitting a Bernoulli model on presence/absence data ===========================
# _Fit ----
Dens01.glm <- glm(Presence ~ ..., family = binomial, data = dataset)

# Change the link function ?
Dens02.glm <- glm(Presence ~ ...,
                  family = binomial(link = "cloglog"), data = dataset)

model <- ...

# _Outputs ----
# Anova
summary(model)
anova(model, test = "Chisq")

# % explained deviance
pc_dev_expl <- anova(model)[-1, 2] * 100 / anova(model)[1, 4]
# % explained per factor
print(cbind(rownames(anova(model))[-1], round(pc_dev_expl , digit = 1)))
# % explained per df
print(cbind(rownames(anova(model))[-1], 
            round(pc_dev_expl / anova(model)[-1, 1], digit = 1)))

# Residuals
par(mfrow = c(2, 2))
plot(model, which = c(1, 2, 3, 4))

# _Area under the curve ----
calc.roc <- roc(dataset$Presence, fitted(model))
plot(calc.roc, auc.polygon = TRUE)
auc(calc.roc)

# _Validation of the model ----
calib.dataset <- ...
valid.dataset <- ...
Dens01.valid.glm <- glm()
valid.fit <- ...

# Cross-validation quality indice

