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
Coasts_Vilaine_wgs84 <- readOGR(dsn = saveWD, layer = "Dept_Contour_wgs84") 

# Load the data shapefile
# dataset <- readOGR(dsn = saveWD, layer = "Stations_covariates_wgs84")
Stations_covariates_wgs84 <- readr::read_rds(
  path = paste0(saveWD, "/Stations_covariates_wgs84.rds"))

# Plot data
plot(Stations_covariates_wgs84)
plot(Coasts_Vilaine_wgs84, usePolypath = F, lwd = 2, col = "grey", add = TRUE)
box()
sp.limits <- extend(extent(Stations_covariates_wgs84), c(0.1, 0.05))
map.limits <- extend(extent(Stations_covariates_wgs84), c(0.2, 0.1))
map <- get_map(bbox(map.limits), source = "google",
               filename = paste0(saveWD, "/get_map"))

g <- ggmap(map, extent = "normal", maprange = FALSE) +
  geom_point(data = Stations_covariates_wgs84@data, aes(lon, lat),
             col = "red", alpha = 0.1) +
  coord_map(xlim = c(xmin(sp.limits),
                     xmax(sp.limits)),
            ylim = c(ymin(sp.limits),
                     ymax(sp.limits))) +
  theme_minimal()
print(g)

## # Leaflet map
## m <- leaflet(data = Stations_covariates_wgs84) %>%
##   addTiles(
##     urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
##   addMarkers(clusterOptions = markerClusterOptions()) %>%
##   addCircleMarkers(radius = 5, fillOpacity = 0.5, stroke = FALSE)
## m  # Print the map
## 
# Transform to non-spatial dataset
dataset.tmp <- as.tbl(Stations_covariates_wgs84@data)
dataset.tmp
dim(dataset.tmp)
summary(dataset.tmp)
names(dataset.tmp)

# Clean dataset
names(dataset.tmp) <- gsub("_pt", "", names(dataset.tmp))

# Add a new covariate: bathymetry as class factor covariate
# c("1depth < 5 m", "2depth 5 - 10 m", "3depth 10 - 20 m", "4depth 20 - 50 m")
dataset <- dataset.tmp %>%
  mutate(Bathy_c = 
           cut(-Bathy, breaks = c(min(-Bathy), 5, 10, 20, max(-Bathy, 50)),
               labels = c("1depth < 5m", "2depth 5-10m",
                          "3depth 10-20m", "4depth 20-50m"),
               include.lowest = TRUE)) %>%
  # dplyr::select(-matches("coords"), -optional, -id) %>%
  dplyr::select(lat:Density, Sedim, Bathy, Bathy_c) %>%
  mutate(Year = as.factor(as.character(Year)))

rm(dataset.tmp)

# Extract dataset with only positive values
dataset <- Stations_covariates_wgs84 %>%
  dplyr::filter(Density > 0)


# Explore data -----------------------------------------------------------------
# _Balance of sampling plan ----
levels(dataset$Year)
levels(dataset$Bathy_c)
levels(dataset$Sedim)

dataset %>% 
  count(Year, Sedim) %>% 
  tidyr::spread(Sedim, n)

table(dataset$Year, dataset$Sedim)
table(dataset$Year, dataset$Bathy_c)
table(dataset$Bathy_c, dataset$Sedim)

ftable(table(dataset$Year, dataset$Bathy_c, dataset$Sedim))

# Comments FACTORIAL Plan
# - Absence of some years
# - Few data in high bathymetry
# - More data on "mud" than on "sand"
# - bathy / sediment relation : mud with low bathy // sand with low bathy
# - High bathy * Sand = no data

# _Covariates potential effect ----
# Simple plot of the data
par(mfrow = c(1, 2))
plot(dataset$Density)
plot(log(dataset$Density + 1))

# Plot the data by class of factors
ggplot() +
  geom_point(data = dataset, aes(Bathy, Density)) +
  facet_wrap(~Sedim)

pairs(Density ~ Bathy * Sedim, data = dataset)
# Comment: This graph is not well adpated to qualitative covariates 
# but it can be very useful for quantitative covariates
GGally::ggpairs(dataset, columns = 4:7)

# Histograms (ggplot2)
g <- ggplot(dataset) +
  geom_histogram(aes(Density),
                 bins = 30, fill = "cyan", col = "grey20")
plot(g)

g + facet_wrap(~Bathy_c)
g + facet_wrap(~Sedim)
g + facet_grid(Bathy_c ~ Sedim)

ggplot(dataset) +
  geom_histogram(aes(log(Density + 1)),
                 bins = 30, fill = "orange", col = "grey40") +
  facet_grid(Bathy_c ~ Sedim)

# Boxplots (ggplot2)
ggplot(dataset) +
  geom_boxplot(aes(x = "", y = Density))

ggplot(dataset) +
  geom_boxplot(aes(x = Bathy_c, y = Density, fill = Bathy_c))
ggplot(dataset) +
  geom_boxplot(aes(x = Sedim, y = Density, fill = Sedim))

ggplot(dataset) +
  geom_boxplot(aes(x = Bathy_c, y = log(Density + 1), fill = Sedim))
ggplot(dataset) +
  geom_boxplot(aes(x = Year, y = log(Density + 1)), fill = "cyan")

# Distribution among factors
glog <- ggplot(dataset) +
  stat_bin(aes(log(Density + 1), fill = Bathy_c),
           bins = 20, alpha = 0.3, position = "identity",
           geom = "area", col = "grey30") +
  facet_wrap(~Sedim)
glog

# Examples of distributions
ggplot() +
  stat_function(aes(x = -5:50, col = "normal"), fun = dnorm, n = 201,  
                args = list(mean = 20, sd = 5), lwd = 1.1) +
  stat_function(aes(x = -1:70, col = "log-normal1"), fun = dlnorm, n = 201,  
                args = list(meanlog = log(5), sdlog = 1)) +
  stat_function(aes(x = -1:80, col = "log-normal2"), fun = dlnorm, n = 201,  
                args = list(meanlog = log(20), sdlog = 1)) +
  stat_function(aes(x = -1:10, col = "gamma1"), fun = dgamma, n = 201,  
                args = list(shape = 5/3, scale = 3)) +
  stat_function(aes(x = -1:50, col = "gamma2"), fun = dgamma, n = 201,  
                args = list(shape = 20/3, scale = 3)) +
  theme(legend.position = "bottom")
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

# Examples of two factors distributions
pop <- data.frame(size = rnorm(5000, 10, 1),
                  type = "males", example = "ex1") %>%
  bind_rows(data.frame(size = rnorm(3000, 12, 3),
                       type = "females", example = "ex1")) %>%
  bind_rows(data.frame(size = rnorm(5000, 10, 1),
                       type = "males", example = "ex2")) %>%
  bind_rows(data.frame(size = rnorm(3000, 16, 3),
                       type = "females", example = "ex2"))

ggplot() +
  stat_bin(data = pop, aes(size, fill = type), 
           geom = "area", alpha = 0.25, col = "grey45",
           position = "identity") + 
  stat_bin(data = pop, aes(size, fill = "all"), 
           geom = "area", alpha = 0.25, col = "grey15", weight = 1.5) +
  facet_wrap(~example)

# _Distribution with covariates ----
# qqplot
par(mfrow = c(2, length(levels(as.factor(dataset$Bathy_c)))))
tapply(dataset$Density, dataset$Bathy_c,
       function(x) {qqnorm(x); qqline(x, col = "red")})
tapply(log(dataset$Density + 1), dataset$Bathy_c, 
       function(x) {qqnorm(x); qqline(x, col = "red")})

# Statistical tests
tapply(log(dataset$Density + 1), dataset$Bathy_c,
       function(x) {ks.test(x, y = pnorm)})
tapply(log(dataset$Density + 1), dataset$Bathy_c,
       function(x) {chisq.gof(x, distribution = "normal")})

# Histograms with Gamma distribution
par(mfcol = c(1, length(levels(as.factor(dataset$Bathy_c)))))
tapply(dataset$Density + 1, dataset$Bathy_c, Gamma.Hist)

# Relation  mean (mu) / variance (V) (Gauss: V=cste; Poisson: V=mu; Gamma and lognormal: V=mu^2)
# To be assessed by combination of significant factors
dataset.MuVar <- filter(dataset, Density > 0) %>%
  group_by(Sedim, Bathy_c, Year) %>%
  summarize(mean = mean(Density), variance = var(Density))

ggplot(dataset.MuVar) +
  geom_point(aes(x = mean, y = variance)) +
  stat_function(fun = function(x) x, aes(col = "y=x")) + 
  stat_function(fun = function(x) 100*x, aes(col = "y=100*x")) + 
  stat_function(fun = function(x) x^2, aes(col = "y=x^2"))

pop <- data.frame(treatment = sample(c(1, 2), 200, replace = TRUE),
                  temp = round(runif(200, 10, 12))) %>% as.tbl %>%
  mutate(y_noInt = treatment +
           temp - 0.2 * temp * (temp >= 12) +
           rnorm(200, 0, 1),
         y_Int = treatment + 
           ifelse(treatment == 1, 
                  temp - 0.2 * temp * (temp >= 12),
                  temp + 0.1 * temp * (temp >= 12)) +
           rnorm(200, 0, 1)
  ) %>%
  tidyr::gather(Interaction, size, -treatment, -temp) %>%
  mutate(treatment = factor(treatment),
         temp = factor(temp))

ggplot(pop) + 
  geom_boxplot(aes(x = temp, y = size,
                   fill = treatment),
               position = "identity", alpha = 0.5) +
  facet_wrap(~Interaction) +
  theme(legend.position = "bottom")

# _Interactions between covariates ----
# Density
par(mfrow = c(2, 1))
interaction.plot(x.factor = dataset$Bathy_c, trace.factor = dataset$Year,
                 response = dataset$Density)
interaction.plot(x.factor = dataset$Sedim, trace.factor = dataset$Year,
                 response = dataset$Density)

# log density
par(mfrow = c(2, 2))
interaction.plot(x.factor = dataset$Bathy_c,  trace.factor = dataset$Year,
                 response = log(dataset$Density + 1))
interaction.plot(x.factor = dataset$Sedim, trace.factor = dataset$Year,
                 response = log(dataset$Density + 1),  col = rainbow(15))
interaction.plot(x.factor = dataset$Sedim, trace.factor = dataset$Bathy_c,
                 response = log(dataset$Density + 1) )


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

# Example of homogeneous residuals
n <- 1000
epsilon <- rnorm(n, 0, 5)
x <- runif(n, 0, 10)
y <- 2*x + 5 + epsilon

par(mfrow = c(1,3))
plot(x, y, pch = 20)
abline(5, 2, col = "red", lwd = 2)
hist(epsilon, breaks = 20, col = "grey")
qqnorm(epsilon); qqline(epsilon, col = "red")

# Diagnostic of the residuals of Dens1.lm
par(mfcol = c(2,2))
plot(Dens1.lm, which = 1:3)
hist(resid(Dens1.lm), main = "Hist. of residuals")

# Analysis of variance
aov1 <- anova(Dens1.lm, test = "Chisq") 
aov1

AIC(Dens1.lm)
# Example of over-parametrisation
n <- 10
x <- runif(n, 0, 100)
data <- data.frame(x = x, y = 2*x + 3 + 50*sin(x))

lm1 <- lm(y ~ x, data = data)
lm2 <- lm(y ~ poly(x, 3), data = data)
lm3 <- lm(y ~ poly(x, 5), data = data)

aovEx <- anova(lm1, lm2, lm3, test = "Chisq")
aovExAIC <- cbind(model = paste0("lm", 1:3), aovEx, "||" , AIC(lm1, lm2, lm3))

ggplot(data, aes(x, y)) + 
  geom_point(cex = 3) + 
  stat_smooth(method = "lm", formula = y ~ x, 
              aes(col = "lm1; y~x"), se = FALSE) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3),
              aes(col = "lm2; y~poly(x,3)"), se = FALSE) + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 5),
              aes(col = "lm3; y~poly(x,5)"), se = FALSE) +
  theme(legend.position = "bottom")

# Try Generalized Linear Models ----
# Family = Poisson ? / Overdispersed poisson ?
# Poisson
Dens2.glm <- glm(round(Density) ~ Bathy_c + Sedim + Year,
                 family = poisson, data = dataset)

par(mfrow = c(2,2))
plot(Dens2.glm, which = c(1, 2, 3, 4))

AIC(Dens2.glm)

# Poisson x Overdispersion
Dens2.glm <- glm(round(Density) ~ Bathy_c + Sedim + Year,
                 family = quasipoisson, data = dataset)

par(mfrow = c(2,2))
plot(Dens2.glm, which = c(1, 2, 3, 4))

# Family = Gamma ?
Dens3.glm <- glm(Density ~ Bathy_c + Sedim + Year,
  family = Gamma, data = dataset)

par(mfrow = c(2,2))
plot(Dens3.glm, which = c(1, 2, 3, 4))

# standardized Pearson residuals vs predictions
plot(fitted(Dens3.glm), rstandard(Dens3.glm, type = "pearson"), 
     main = "Res. Pearson STD vs Pred.")

# Using step AIC
null.m <- glm(Density ~ 1, family = Gamma, data = dataset)
Dens3.glm <- stepAIC(null.m, 
                     Density ~ (Bathy_c + Sedim + Year)^2,
                     data = dataset, direction = "both")

# Gaussian on Log-transformed data ?
null.m <- glm(log(Density) ~ 1, family = gaussian, data = dataset)
Dens4.glm <- stepAIC(null.m, 
                     log(Density) ~ (Bathy_c + Sedim + Year) ^ 2, 
                     data = dataset, direction = "both")

# Dens4.glm <- glm(log(Density) ~ Bathy_c + Sedim, family = gaussian, data = dataset)

summary(Dens4.glm)

par(mfrow = c(2,2))
plot(Dens4.glm, which = c(1, 2, 3, 4))

# WARNING - AIC values for gaussian models on log-transformed data

# The AIC criteria uses the Deviance. 
# A gaussian deviance with log-transformed values (what is done in AIC(model))
# IS NOT equivalent to computing a LogNormal deviance with non-transformed values
# A correction must be applied to get the TRUE AIC value

AIC <- AIC(Dens4.glm) + 2 * sum(log(dataset$Density))
AIC

# Chosen model ?
if (ClassicWay) {
model <- glm(log(Density+1) ~ Bathy_c + Sedim, family = gaussian, data = dataset)
} else {
model <- glm(log(Density) ~ Bathy_c + Sedim, family = gaussian, data = dataset)  
}

# Analyse ouputs of the chosen model ----
# Parameters estimates
summary(model)

# Analysis of variance
anova(model, test = "Chisq") 

# % explained deviance
pc_dev_expl <- anova(model)[-1,2]*100 / anova(model)[1,4]
# % explained per factor
print(cbind(rownames(anova(model))[-1], round(pc_dev_expl, digit = 1)))
# % explained per df
print(cbind(rownames(anova(model))[-1],
            round(pc_dev_expl / anova(model)[-1, 1], digit = 1)))

# Model goodness of fit and residuals
par(mfcol = c(2, 2))
plot(model, which = 1:4)

# std Pearson residuals vs pred
par(mfrow = c(2, 3))
plot(dataset$Density + ClassicWay*1, rstandard(model, type = "pearson"),
     main = "Res. Pearson STD vs Obs.")
plot(fitted(model), rstandard(model, type = "pearson"), 
     main = "Res. Pearson STD vs Pred.")

# std Deviance residuals	
qqnorm(rstandard(model, type = "deviance"),
       ylim = c(-3, 3),
       main = "Normal Q-Q plot sur Res. deviance std.")
qqline(rstandard(model, type = "deviance"), col = "red")

# Cook's distance
plot(cooks.distance(model), type = "h", main = "Cook's distance")

# Predictions vs data
pred <- model$fitted
# obs <- dataset$Density+1
obs <- log(dataset$Density + ClassicWay*1)
plot(obs, pred, xlim = c(0, 10), ylim = c(0, 10), main = "Pred vs Obs")
abline(b = 1, a = 0, col = "red")

# Homogeneity of the variance of residuals per class of bathymetry
dataset$Resid <- model$resid
boxplot(dataset$Resid ~ dataset$Bathy_c, main = "Resid vs Bathy_c")

# Predictions ----
# Dowload the file with factors for predictions
predictions <- readr::read_delim(paste0(origWD, "/predictions.csv"),
                                 delim = ";")

# Compute predictions (in the scale of the response)
pred.Density <- cbind(predictions, predict.glm(model, predictions, type = "r", se = T)) %>%
  group_by(Sedim, Bathy_c) %>%
  summarise(mean.fit = mean(fit),
            mean.se = mean(se.fit)) %>%
  ungroup()

# library(emojifont)
ggplot(pred.Density, aes(x = Bathy_c, y = mean.fit, colour = Sedim)) +
  geom_point() +
  #geom_text(family = 'EmojiOne', size = 8, label = emoji('fish'),
  #          show.legend = FALSE) + 
  geom_errorbar(aes(ymin = mean.fit - mean.se, ymax = mean.fit + mean.se), 
                width = 0.2) +
  theme(legend.position = "bottom")

