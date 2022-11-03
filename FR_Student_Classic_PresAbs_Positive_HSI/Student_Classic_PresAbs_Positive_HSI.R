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










# Positive Model ===============================================================
# Load the data shapefile
Stations_covariates_wgs84 <- readOGR(...)
names(Stations_covariates_wgs84)[9:16] <- 
  c("SeaLim", "Coast", "Zone_bio", "Sedim", "id", "Bathy", "Pente", "TPI")



# Transform to non-spatial dataset
dataset.orig <- as.tbl(Stations_covariates_wgs84@data)

# Clean dataset names
names(dataset.orig) <- gsub("_pt", "", names(dataset.orig))

# Add a new covariate: bathymetry as class factor covariate using cut
# c("1depth < 5m", "2depth 5-10m", "3depth 10-20 m", "4depth 20-50 m")
dataset.orig$Bathy_c <- cut(
  -dataset.orig$Bathy, 
  breaks = c(min(-dataset.orig$Bathy), 5, 10, 20, 50),
  labels = c("1depth < 5m", "2depth 5-10m", "3depth 10-20 m", "4depth 20-50 m"),
  include.lowest = TRUE
)
# Extract dataset with only positive values
dataset <- dataset.orig...



# Explore data -----------------------------------------------------------------
# _Balance of sampling plan ----
levels(dataset$Bathy_c)

table(dataset$Year, dataset$Sedim)

ftable(table(dataset$Year, dataset$Bathy_c, dataset$Sedim))




# _Covariates potential effect ----
# Plot the data by class of factors
coplot(dataset$Density ~ dataset$Bathy | dataset$Sedim)
pairs(Density ~ Bathy * Sedim, data = dataset)

# Histograms (ggplot2)
g <- ggplot(dataset) +
  geom_histogram(aes(Density))
plot(g)

g + facet_wrap(~Bathy_c)

# Boxplots
boxplot(dataset$Density)	
boxplot(dataset$Density ~ dataset$Bathy_c)
boxplot(dataset$Density ~ dataset$Bathy_c*dataset$Sedim,
        border = rep(1 + 1:length(unique(dataset$Bathy_c)),
                     length(unique(dataset$Sedim))))


# _Distribution type of the data ----
# Tests of normality
par(mfrow = c(1,2))
qqnorm(dataset$Density) ; qqline(dataset$Density, col = "red")
qqnorm(log(dataset$Density + 1)) ; qqline(log(dataset$Density + 1), col = "red")

# Log-Normal ?
par(mfrow = c(2, 2))
tmp <- tapply(log(dataset.posit$Density), dataset.posit$Bathy_c, 
              function(x) {
                qqnorm(x)
                qqline(x, col = "red")
              })

# Gamma ?
par(mfcol = c(4, 4))
tapply(dataset.posit$Density, list(dataset.posit$Bathy_c, dataset.posit$Sedim), 
       function(x) {
         if (length(x) <= 1) {plot(1, 1)
           } else {
             Gamma.Hist(x)
           }
       })

# Relation  mean (mu) / variance (V) 
# (Gauss: V=cste; Poisson: V=mu; Gamma and lognormal: V=mu^2)
# To be assessed by combination of significant factors
dataset.MuVar <- filter(dataset, Density > 0) %>%
  group_by(Sedim, Bathy_c, Year) %>%
  summarize(mean = mean(Density), variance = var(Density))

ggplot(dataset.MuVar) +
  geom_point(aes(x = mean, y = variance)) +
  stat_function(fun = function(x) x, aes(col = "y=x")) + 
  stat_function(fun = function(x) 100*x, aes(col = "y=100*x")) + 
  stat_function(fun = function(x) x^2, aes(col = "y=x^2"))











# Fitting GLM on positive data =================================================
# Try a Gamma distribution
Densposit1.glm <- glm(..., family = Gamma, data = dataset)

par(mfrow = c(2, 2))
plot(Densposit1.glm, which = c(1, 2, 3, 4))

# Poisson (or quasi poisson)
Densposit2.glm <- glm(round(Density) ~ 1, family = poisson, data = dataset)

par(mfrow = c(2, 2))
plot(Densposit2.glm, which = c(1, 2, 3, 4))

# Gaussian on log-transformed values
Densposit3.glm <- glm(log(Density) ~ ..., data = dataset)

# Analyse of model goodness of fit and residuals
par(mfcol = c(2, 2))
plot(Densposit3.glm, which = c(1, 2, 3, 4))

# Compare AIC
AIC(Densposit1.glm, Densposit2.glm, Densposit3.glm)

# _Additionnal analysis of residuals of the chosen model ----
model <- ...
# Standardised Pearson residuals vs fitted
# => Test for the Homogeneity of the variance
plot(fitted(model), rstandard(model, type = "pearson"),
     main = "Res. Pearson STD vs Pred.")

# Residuals of Standardised Deviance vs fitted
# => Test for Normality and for influence of observations
qqnorm(rstandard(model, type = "deviance"), ylim = c(-3, 3),
       main = "Normal Q-Q plot sur Res. deviance std.")
qqline(rstandard(model, type = "deviance"), col = "red")

# Cook's distance
# => Influence of each observation on parameters estimation
barplot(cooks.distance(model))

# Data vs Fitted
plot(log(dataset$Density), fitted(model))
abline(b = 1, a = 0, col = "red")








# _Validation of the model on Years 2000-2003 ----
calib.dataset <- dataset[-which(
  dataset$Year %in% c("2000", "2001", "2002", "2003")), ]
valid.dataset <- dataset[which(
  dataset$Year %in% c("2000", "2001", "2002", "2003")), ]
Dens.valid.glm <- glm(log(Density) ~ poly(Bathy, 3) + Sedim,
             family = gaussian, data = calib.dataset)
valid.fit <- predict(Dens.valid.glm,
                     newdata = valid.dataset, type = "response")

# Expected mean of the density (if glm() on LOG-TRANSFORMED DATA has been used)
# The so-called Laurent correction
var.logdens.pos <- var(residuals(Dens.valid.glm))
valid.fit.corrected <- exp(valid.fit + 0.5 * var.logdens.pos)

# figure
qplot(valid.dataset$Density, valid.fit.corrected) +
  geom_abline(slope = 1, intercept = 0, col = "red")

# Pearson coefficient
cor(valid.dataset$Density, valid.fit, method = "pearson")
# If log-transformed data
cor(valid.dataset$Density, valid.fit.corrected, method = "pearson")

# MSE
MSE <- mean((valid.dataset$Density - valid.fit.corrected)^2)
RMSE <- sqrt(MSE) 



# Presence-Absence Model =======================================================
# Additionnal libraries ----
library(pROC)






# Get data ====
# Load Coastlines shapefile
Coasts_Vilaine_wgs84 <- readOGR(...) 

# Load the data shapefile (if you know how to play with spatial data)
Stations_covariates_wgs84 <- readOGR(...)
names(Stations_covariates_wgs84)[9:16] <- 
  c("SeaLim", "Coast", "Zone_bio", "Sedim", "id", "Bathy", "Pente", "TPI")

# Otherwise use this
# Stations_covariates_wgs84 <- readr::read_rds(
  # path = paste0(saveWD, "/Stations_covariates_wgs84.rds"))


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


## # _Outputs ----
## # Anova
## summary(model)
## anova(model, test = "Chisq")
## 
## # % explained deviance
## pc_dev_expl <- anova(model)[-1, 2] * 100 / anova(model)[1, 4]
## # % explained per factor
## print(cbind(rownames(anova(model))[-1], round(pc_dev_expl , digit = 1)))
## # % explained per df
## print(cbind(rownames(anova(model))[-1],
##             round(pc_dev_expl / anova(model)[-1, 1], digit = 1)))
## 
## # Residuals
## par(mfrow = c(2, 2))
## plot(model, which = c(1, 2, 3, 4))
## 

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












# _Validation of the model ----
calib.dataset <- ...
valid.dataset <- ...
Dens01.valid.glm <- glm()
valid.fit <- ...

# Cross-validation quality indice









# Coupling the two sub-models ==================================================
# Sub-models selected ----
dataset.orig <- ...
dataset.posit <- ...

# Save dataset
readr::write_rds(dataset.orig, path = paste0(saveWD, "/dataset.orig.rds"))

# Run models
Dens01.glm <- glm(Presence ~ ...,
                  family = ..., data = dataset.orig)
Densposit.glm <- glm(Density ~ ...,
                     family = ..., data = dataset.posit)





# Predict and compare to observations ----
# Comment 1
# Formula for the expected mean of a Bernouilli-glm model
# E(X) = E(X | Ip=1)*p(Ip=1) + E(X | Ip=0)*p(Ip=0) = E(X | Ip=1)*p(Ip=1)
# The variance V(X) is not easy to obtain

# Comment 2
# Fitted values have not the same dimension for the 2 models
# The model on positive values is done on a subset of the data only
# ==> We must use the predict.glm() function to build predictions

# Expected mean of the presence probability
presence.pred <- predict.glm(Dens01.glm, newdata = dataset.orig,
                             type = "response", se = FALSE)

# The following script is valuable for a Gaussian GLM() on log-transformed value
# Expected mean of the log-density from the glm model
E.logdens.pos <- predict.glm(Densposit.glm, newdata = dataset.orig,
                             type = "response", se = FALSE)

# Expected mean of the density (if lm() on log-transformed data has ben used)
# The so-called Laurent correction
var.logdens.pos <- var(residuals(Densposit.glm))
E.dens.pos <- exp(E.logdens.pos + 0.5 * var.logdens.pos)

# Delta model : Expected mean of conditional model
E.dens <- presence.pred * E.dens.pos









# Predictions for new places with known covariates -----------------------------
# Dowload the file with factors for predictions
predictions <- readr::read_delim(paste0(origWD, "/predictions.csv"),
                                 delim = ";")
predictions

# Expected mean of the presence probability
presence.pred <- predict.glm(Dens01.glm, newdata = predictions,
                             type = "response", se = FALSE)

# Expected mean of the log-density from the glm model
E.logdens.pos <- predict.glm(Densposit.glm, newdata = predictions,
                             type = "response", se = FALSE)

# Expected mean of the density (if lm() on log-transformed data has ben used)
# The so-called Laurent correction
var.logdens.pos <- var(residuals(Densposit.glm))
E.dens.pos <- exp(E.logdens.pos + 0.5 * var.logdens.pos)

# Delta model : Expected mean of conditional model
E.dens <- presence.pred * E.dens.pos



# Species distribution model ===================================================
# Data preparation
dataset.orig <- readr::read_rds(paste0(saveWD, "/dataset.orig.rds")) %>%
  mutate(Sediment_class = as.factor(as.numeric(Sedim)),
         Bathy_class = as.factor(as.numeric(Bathy_c)))
dataset.posit <- filter(dataset.orig, Presence == 1)

# Read multi-layer raster
RastForPred.orig <- stack(paste0(saveWD, "/RastForPred_L93.grd"))
# Modify raster layers for predictions
# Bathy_c = cut(-Bathy, breaks = c(min(-Bathy), 5, 10, 20, max(-Bathy, 50)),
#                labels = c("1depth < 5m", "2depth 5-10m",
#                           "3depth 10-20m", "4depth 20-50m"),
#                include.lowest = TRUE)
RastForPred_L93 <- stack(raster(RastForPred.orig, "bathy_L93"), 
                     cut(-1 * raster(RastForPred.orig, "bathy_L93"),
                         breaks = c(min(-dataset.orig$Bathy), 5, 10, 20,
                                    max(-dataset.orig$Bathy, 50)),
               include.lowest = TRUE),
                     raster(RastForPred.orig, "Sediment_area_L93"))
names(RastForPred_L93) <- c("Bathy", "Bathy_class", "Sediment_class")
# Remove Rocks from Sediment_class raster
values(RastForPred_L93)[which(values(RastForPred_L93)[,3] == 4),3] <- NA
# Unproject Raster into wgs84
RastForPred <- projectRaster(
  RastForPred_L93, 
  crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
  method = "ngb")

# Run models
Dens01.glm <- glm(Presence ~ ...,
                  family = ..., data = dataset.orig)
Densposit.glm <- glm(...,
                     family = ..., data = dataset.posit)



# Predict a young sole density for each cell of the grid ----
# Function to predict with raster objects
predmod <- function(object, newdata) {
  res <-
  predict.glm(object, newdata, se.fit = FALSE, type = "response")
  res
}

pred.pres <- predict(object = RastForPred, model = Dens01.glm,
                     fun = predmod, index = 1)
pred.logdens.pos <- predict(object = RastForPred, model = Densposit.glm,
                            fun = predmod, index = 1)

# Expected mean of the density (if lm() on log-transformed data has ben used)
# The so-called Laurent correction
var.logdens.pos <- var(residuals(Densposit.glm))
pred.dens.pos <- exp(pred.logdens.pos + 0.5 * var.logdens.pos)

# Combine the 2 sub-models
pred.dens <- pred.pres * pred.dens.pos

