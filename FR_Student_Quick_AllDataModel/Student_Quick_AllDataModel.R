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


# REMEMBER ====-----------------------------------------------------------------
# Define what you are looking for, which question you try to answer!

# EXPLORATION OF THE DATA
# - Explore the balance of the sampling plan among the environmental covariates
# - Explore the values of the observations against environmental covariates to detect potential correlations
# - Explore interactions between covariates effects on observations
# - Explore the possible distribution forms (gaussian, log-normal, ...) of observations on combination of covariates.

# ==== WARNING ====
# Scripts below are just giving some examples, but are not playing "best" models ! 
# Try other solutions, build your own ! 

# Get data --------------------------------------------------------------------
# Load Coastlines shapefile
Coasts_Vilaine_wgs84 <- readOGR(...) 

# Load the data shapefile
# Stations_covariates_wgs84 <- readOGR(...)
# names(Stations_covariates_wgs84)[9:16] <- 
#   c("SeaLim", "Coast", "Zone_bio", "Sedim", "id", "Bathy", "Pente", "TPI")
Stations_covariates_wgs84 <- readr::read_rds(
  path = paste0(saveWD, "/Stations_covariates_wgs84.rds"))

# Plot data
plot(Stations_covariates_wgs84)
plot(Coasts_Vilaine_wgs84, usePolypath = F, lwd = 2, col = "grey", add = TRUE)
box()
# Transform to non-spatial dataset
dataset <- as.tbl(Stations_covariates_wgs84@data)

# Clean dataset names
names(dataset) <- gsub("_pt", "", names(dataset))

# Add a new covariate: bathymetry as class factor covariate using cut
# c("1depth < 5m", "2depth 5-10m", "3depth 10-20 m", "4depth 20-50 m")
dataset$Bathy_c <- cut(-dataset$Bathy, ..., include.lowest = TRUE)

# Extract dataset with only positive values
dataset <- dataset %>%
  dplyr::filter(Density > 0)
# Explore data -----------------------------------------------------------------
# _Balance of sampling plan ----
levels(dataset$Bathy_c)

table(dataset$Year, dataset$Sedim)

ftable(table(dataset$Year, dataset$Bathy_c, dataset$Sedim))

# _Covariates potential effect ----
# Simple plot of the data
par(mfrow = c(1, 2))
plot(dataset$Density)
plot(log(dataset$Density + 1))

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
        border = rep(1 + 1:length(unique(dataset$Bathy_c)), length(unique(dataset$Sedim))))

# _Distribution type of the data ----
# Tests of normality
par(mfrow = c(1,2))
qqnorm(dataset$Density) ; qqline(dataset$Density, col = "red")
qqnorm(log(dataset$Density + 1)) ; qqline(log(dataset$Density + 1), col = "red")

ks.test(rnorm(1000, 0, 1), pnorm)
ks.test(dataset$Density, pnorm)
ks.test(log(dataset$Density + 1), pnorm)

chisq.gof(rnorm(1000, 0, 1), distribution = "normal")
chisq.gof(log(dataset$Density + 1), distribution = "normal")
# Comments : these tests are to be used with caution (need lot of data)

# Gamma ?
Gamma.Hist(dataset$Density + 1)

shape <- mean(dataset$Density) * mean(dataset$Density) / var(dataset$Density)
chisq.gof(dataset$Density, distribution = "gamma", shape = shape)

# _Distribution with covariates ----
# qqplot
par(mfrow = c(2, length(levels(as.factor(dataset$Bathy_c)))))
tapply(dataset$Density, dataset$Bathy_c,
       function(x) {qqnorm(x); qqline(x, col = "red")})

# Statistical tests
tapply(dataset$Density, dataset$Bathy_c,
       function(x) {ks.test(x, y = pnorm)})
tapply(dataset$Density, dataset$Bathy_c,
       function(x) {chisq.gof(x, distribution = "normal")})

# Histograms with Gamma distribution
par(mfcol = c(1, length(levels(as.factor(dataset$Bathy_c)))))
tapply(dataset$Density + 1, dataset$Bathy_c, Gamma.Hist)

# Relation  mean (mu) / variance (V)
# (Gauss: V=cste; Poisson: V=mu; Gamma and lognormal: V=mu^2)
# To be assessed by combination of factors
dataset.pos <- dataset[dataset$Density > 0,]
mean <- tapply(dataset.pos$Density,
               interaction(dataset.pos$Sedim, 
                           dataset.pos$Bathy_c,
                           dataset.pos$Year),
               mean)

variance <- tapply(dataset.pos$Density,
                   interaction(dataset.pos$Sedim, 
                               dataset.pos$Bathy_c,
                               dataset.pos$Year),
                   var)

x11()
plot(mean[order(mean)], variance[order(mean)])
points(mean[order(mean)], mean[order(mean)],
       col = "green", lwd = 2, type = "l")
points(mean[order(mean)], 100*mean[order(mean)],
       col = "red", lwd = 3, type = "l")
points(mean[order(mean)], mean[order(mean)]^2,
       col = "blue", lwd = 3, type = "l")

# _Interactions between covariates ----
# Density
interaction.plot(x.factor = dataset$Bathy_c, trace.factor = dataset$Year,
                 response = dataset$Density)

# log density
interaction.plot(x.factor = dataset$Bathy_c,  trace.factor = dataset$Year,
                 response = log(dataset$Density + 1))


# REMEMBER ====-----------------------------------------------------------------
# Define what you are looking for, which question you try to answer!

# MODEL TESTING
# Test different form of models regarding combination of covariates and distribution form of the residuals;
# Compare models using statistical tools (AIC, anova, …), cross-validation tools but also your knowledge of the dataset and the answers you are looking for;
# Analyse the residuals. Analyse their distribution and if your hypotheses are verified.

# REMEMBER
# Only when hypotheses on distribution law are verified, physical covariates and interactions selected may start to be interpreted...

# ==== WARNING ====
# Scripts below are just giving some examples, but are not playing best solutions !
# Try other solutions, build your own ! 

# Try simple Linear models ----
Dens1.lm <- lm(Density ~ Bathy + Sedim, data = dataset)
summary.glm <- summary(Dens1.lm)
summary.glm

# Diagnostic of the residuals of Dens1.lm
par(mfcol = c(2,2))
plot(Dens1.lm, which = 1:3)
hist(resid(Dens1.lm), main = "Hist. of residuals")

# Analysis of variance
aov1 <- anova(Dens1.lm, test = "Chisq") 
aov1

AIC(Dens1.lm)
# Try Generalized Linear Models ----
# Family = Poisson ? / Overdispersed poisson ?
# Poisson
Dens2.glm <- glm(round(Density) ~ ...,
                 family = poisson, data = dataset)

# Poisson x Overdispersion
Dens2.glm <- glm(round(Density) ~ ...,
                 family = quasipoisson, data = dataset)

# Family = Gamma ?
Dens3.glm <- glm(Density ~ ...,
                 family = Gamma, data = dataset)

# Using step AIC
null.m <- glm((Density) ~ 1, family = Gamma, data = dataset)
Dens3.glm <- stepAIC(null.m, 
                     (Density) ~ (Bathy_c + Sedim + Year)^2,
                     data = dataset, direction = "both")

# Gaussian on Log-transformed data ?
null.m <- glm(log(Density) ~ 1, 
              family = gaussian, data = dataset)
Dens4.glm <- stepAIC(...)

# WARNING - AIC values for gaussian models on log-transformed data

# The AIC criteria uses the Deviance. 
# A gaussian deviance with log-transformed values (what is done in AIC(model))
# IS NOT equivalent to computing a LogNormal deviance with non-transformed values
# A correction must be applied to get the TRUE AIC value

AIC <- AIC(Dens4.glm) + 2 * sum(log(dataset$Density))
AIC

# Choosen model ?
model <- ...
## # Analyse ouputs of the chosen model ----
## # Parameters estimates
## summary(model)
## 
## # Analysis of variance
## anova(model, test = "Chisq")
## 
## # % explained deviance
## pc_dev_expl <- anova(model)[-1,2]*100 / anova(model)[1,4]
## # % explained per factor
## print(cbind(rownames(anova(model))[-1], round(pc_dev_expl, digit = 1)))
## # % explained per df
## print(cbind(rownames(anova(model))[-1],
##             round(pc_dev_expl / anova(model)[-1, 1], digit = 1)))
## 
## # Model goodness of fit and residuals
## par(mfcol = c(2, 2))
## plot(model, which = 1:4)
## 
## # std Pearson residuals vs pred
## par(mfrow = c(2, 3))
## plot(dataset$Density + ClassicWay*1, rstandard(model, type = "pearson"),
##      main = "Res. Pearson STD vs Obs.")
## plot(fitted(model), rstandard(model, type = "pearson"),
##      main = "Res. Pearson STD vs Pred.")
## 
## # std Deviance residuals	
## qqnorm(rstandard(model, type = "deviance"),
##        ylim = c(-3, 3),
##        main = "Normal Q-Q plot sur Res. deviance std.")
## qqline(rstandard(model, type = "deviance"), col = "red")
## 
## # Cook's distance
## plot(cooks.distance(model), type = "h", main = "Cook's distance")
## 
## # Predictions vs data
## pred <- model$fitted
## # obs <- dataset$Density+1
## obs <- log(dataset$Density + ClassicWay*1)
## plot(obs, pred, xlim = c(0, 10), ylim = c(0, 10), main = "Pred vs Obs")
## abline(b = 1, a = 0, col = "red")
## 
## # Homogeneity of the variance of residuals per class of bathymetry
## dataset$Resid <- model$resid
## boxplot(dataset$Resid ~ dataset$Bathy_c, main = "Resid vs Bathy_c")
## 
# Predictions ----
# Dowload the file with factors for predictions
predictions <- readr::read_delim(paste0(origWD, "/predictions.csv"),
                                 delim = ";")
predictions

# Compute predictions (in the scale of the response)
pred.Density <- predict.glm(model, predictions, type = "r", se = T)

# Plot
plot(pred.Density$fit[1:4],
     col = "red", xaxt = "n", xlab = "Bathy_c",
     main = "Pred. for year 1985", 
     ylim = c(min(pred.Density$fit), max(pred.Density$fit)))
points(pred.Density$fit[5:8], col = "forestgreen", xaxt = "n")
points(pred.Density$fit[9:12], col = "blue", xaxt = "n")
axis(1, at = 1:4, labels = levels(dataset$Bathy_c))
legend("topright", fill = c("red", "forestgreen", "blue"), 
       legend = c("mud", "sand", "coarse"))

