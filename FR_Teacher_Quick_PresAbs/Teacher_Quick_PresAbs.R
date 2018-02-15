# Tutorial : SpatialGLM
# Maintainer : S. Rochette, StatnMap, sebastienrochettefr@gmail.com
# Citation : Rochette, S., Rivot, E., Morin, J., Mackinson, S., Riou, P., Le Pape, O. (2010). Effect of nursery habitat degradation on flatfish population renewal. Application to Solea solea in the Eastern Channel (Western Europe). Journal of sea Research, 64.
# Version : 2017
# Encoding UTF-8

  # Generated with R and knitr: R version - Teacher


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
# WD <- "/mnt/Data/Formation_TD-GLM_Cours_Agro/TD_Vilaine_SIG-et-GLM_2017_01_3days"
WD <- here()
# Folder of original files
origWD <- here("01_Original_data")
# Folder for outputs
saveWD <- here("02_Outputs")
# Folder where to save outputs from R
figWD <- here("03_Figures")
# Folder where complementary functions are stored
funcWD <- here("04_Functions")
# Copy maintained complementary functions from local directory
# if (file.exists("~/Rshiny/Map_creation_git/Rsources/Map.output.fun.R")) {
#   tmp <- file.copy("~/Rshiny/Map_creation_git/Rsources/Map.output.fun.R",
#                    paste0(funcWD, "/Map.output.fun.R"), overwrite = TRUE)
# }
# Manual operations
Manuel <- FALSE

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
Coasts_Vilaine_wgs84 <- readOGR(dsn = saveWD, layer = "Dept_Contour_wgs84") 

# Load the data shapefile
# dataset <- readOGR(dsn = saveWD, layer = "Stations_covariates_wgs84")
Stations_covariates_wgs84 <- readr::read_rds(
  path = paste0(saveWD, "/Stations_covariates_wgs84.rds"))

# Transform to non-spatial dataset
dataset.tmp <- as.tbl(Stations_covariates_wgs84@data)
dataset.tmp
dim(dataset.tmp)
summary(dataset.tmp)

# Clean dataset
names(dataset.tmp) <- gsub("_pt", "", names(dataset.tmp))

# Add a new covariate: bathymetry as class factor covariate
# c("1depth < 5 m", "2depth 5 - 10 m", "3depth 10 - 20 m", "4depth 20 - 50 m")
dataset.orig <- dataset.tmp %>%
  mutate(Bathy_c = 
           cut(-Bathy, breaks = c(min(-Bathy), 5, 10, 20, max(-Bathy, 50)),
               labels = c("1depth < 5m", "2depth 5-10m",
                          "3depth 10-20m", "4depth 20-50m"),
               include.lowest = TRUE)) %>%
  # dplyr::select(-matches("coords"), -optional, -id) %>%
  dplyr::select(lat:Density, Sedim, Bathy, Bathy_c) %>%
  mutate(Year = as.factor(as.character(Year)),
         Presence = 1*(Density > 0))

dataset <- dataset.orig
rm(dataset.tmp)

# Explore Presence/Absence data ================================================
# _Balance of data against covariates ----
table(dataset$Presence)
table(dataset$Presence, dataset$Bathy_c)
table(dataset$Presence, dataset$Sedim)
table(dataset$Presence, dataset$Year)

# _Covariates potential effect ----
# Histograms (ggplot2)
g <- ggplot(dataset) +
  geom_histogram(aes(Presence),
                 bins = 30, fill = "cyan", col = "grey20")
plot(g)

g + facet_wrap(~Bathy_c)
g + facet_wrap(~Sedim)
g_hist <- g + facet_grid(Sedim ~ Bathy_c)

ggplot(dataset) +
  geom_histogram(aes(log(Presence + 1)),
                 bins = 30, fill = "orange", col = "grey40") +
  facet_grid(Bathy_c ~ Sedim)

# Mean by class of factor
ggplot(dataset, aes(Year, Presence)) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "grey70")

ggplot(dataset, aes(Bathy_c, Presence)) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "grey70") +
  facet_wrap(~Sedim)

# _Interactions between covariates ----
par(mfrow = c(2, 2))
interaction.plot(dataset$Bathy_c, dataset$Year, dataset$Presence)
interaction.plot(dataset$Year, dataset$Sedim, dataset$Presence)
interaction.plot(dataset$Sedim, dataset$Bathy_c, dataset$Presence)

# Fitting a Bernoulli model on presence/absence data ===========================
# _Fit ----
null.m <- glm(Presence ~ 1, family = binomial, data = dataset)
Dens01.glm <- stepAIC(null.m, Presence ~ (Bathy + Bathy_c + Sedim + Year) ^ 2,
                      data = dataset, direction = "both")

Dens01.glm <- glm(Presence ~ Bathy + Sedim, family = binomial, data = dataset)

# Change the link function ?
# Compare different options ?
Dens02.glm <- glm(Presence ~ Bathy + Sedim,
                  family = binomial(link = "cloglog"), data = dataset)
Dens03.glm <- glm(Presence ~ Bathy + Sedim, 
                  family = binomial(link = "probit"), data = dataset)

AIC(Dens01.glm, Dens02.glm, Dens03.glm)

model <- Dens01.glm

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

# _Others interesting graphs ----
dataset.pred <- dataset %>% 
  mutate(PredictPA = c(model$fitted),
         ResidPA = c(resid(model, type = "response")))

# Fitted vs Observed
ggplot(dataset.pred) +
  geom_histogram(aes(PredictPA, fill = factor(Presence))) +
  facet_wrap(~ Presence) +
  guides(fill = FALSE)

gpred <- ggplot(dataset.pred) +
  geom_violin(aes(factor(Presence), PredictPA),
              draw_quantiles = 0.5)
gpred

# Repartition of residuals against covariates
ggplot(dataset.pred) +
  geom_histogram(aes(ResidPA)) +
  facet_wrap(~ Bathy_c)

# Same with violin boxplots
ggplot(dataset.pred) + 
  geom_violin(aes(Bathy_c, PredictPA),
              draw_quantiles = 0.5)

ggplot(dataset.pred) + 
  geom_violin(aes(Bathy_c, ResidPA))

# _Area under the curve ----
calc.roc <- roc(dataset$Presence, fitted(model))
plot(calc.roc, auc.polygon = TRUE)
auc(calc.roc)

# _Validation of the model on Years 2000-2003 ----
calib.dataset <- dataset[-which(
  dataset$Year %in% c("2000", "2001", "2002", "2003")), ]
valid.dataset <- dataset[which(
  dataset$Year %in% c("2000", "2001", "2002", "2003")), ]
Dens01.valid.glm <- glm(Presence ~ Bathy + Sedim,
                        family = binomial, data = calib.dataset)
valid.fit <- predict(Dens01.valid.glm,
                     newdata = valid.dataset, type = "response")

# Area under the curve on validation
valid.roc <- roc(valid.dataset$Presence, valid.fit)
plot(valid.roc, auc.polygon = TRUE)
auc(valid.roc)

