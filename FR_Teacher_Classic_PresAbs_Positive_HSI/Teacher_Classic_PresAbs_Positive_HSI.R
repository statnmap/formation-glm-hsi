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










# Positive Model ===============================================================
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
  mutate(Year = as.factor(as.character(Year))) 

# Extract dataset with only positive values
dataset <- dataset.orig %>%
  filter(Density > 0)

dim(dataset)
rm(dataset.tmp)



# Explore data =================================================================
# _Balance of sampling plan ----
levels(dataset$Year)
levels(dataset$Bathy_c)
levels(dataset$Sedim)

table(dataset$Year, dataset$Sedim)
table(dataset$Year, dataset$Bathy_c)
table(dataset$Bathy_c, dataset$Sedim)

ftable(table(dataset$Year, dataset$Bathy_c, dataset$Sedim))



# _Covariates potential effect ----
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
  geom_histogram(aes(log(Density)),
                 bins = 30, fill = "orange", col = "grey40") +
  facet_grid(Bathy_c ~ Sedim)

# Boxplots (ggplot2)
ggplot(dataset) +
  geom_boxplot(aes(x = "", y = log(Density)))

ggplot(dataset) +
  geom_boxplot(aes(x = Sedim, y = log(Density), fill = Sedim))
ggplot(dataset) +
  geom_boxplot(aes(x = Bathy_c, y = log(Density), fill = Bathy_c)) +
  facet_wrap(~Sedim)

ggplot(dataset) +
  geom_boxplot(aes(x = Year, y = log(Density)), fill = "cyan")

# Distribution among factors
glog <- ggplot(dataset) +
  geom_boxplot(aes(x = Bathy_c, y = log(Density), fill = Bathy_c)) +
  facet_wrap(~Sedim) +
  theme(legend.position = "bottom")
glog



# _Distribution type of the data ----
# Tests of normality
par(mfrow = c(1,2))
qqnorm(dataset$Density) ; qqline(dataset$Density, col = "red")
qqnorm(log(dataset$Density + 1)) ; qqline(log(dataset$Density + 1), col = "red")

# Log-Normal ?
par(mfrow = c(2, 2))
tmp <- tapply(log(dataset$Density), dataset$Bathy_c, 
              function(x) {
                qqnorm(x)
                qqline(x, col = "red")
              })

# Gamma ?
x11()
par(mfcol = c(4, 4))
tapply(dataset$Density, list(dataset$Bathy_c, dataset$Sedim), 
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

# _Interactions between covariates ----
# log density
par(mfrow = c(2, 2))
interaction.plot(x.factor = dataset$Bathy_c,  trace.factor = dataset$Year,
                 response = log(dataset$Density))
interaction.plot(x.factor = dataset$Year,  trace.factor = dataset$Bathy,
                 response = log(dataset$Density),  col = rainbow(15))
interaction.plot(x.factor = dataset$Sedim, trace.factor = dataset$Year,
                 response = log(dataset$Density),  col = rainbow(15))
interaction.plot(x.factor = dataset$Sedim, trace.factor = dataset$Bathy_c,
                 response = log(dataset$Density) )







# Fitting GLM on positive data =================================================
# Try a Gamma distribution
null.m <- glm(Density ~ 1, family = Gamma, data = dataset)
Densposit1.glm <- stepAIC(null.m, Density ~ (Bathy_c + Sedim + Year) ^ 2,
                          data = dataset, direction = "both")

Densposit1.glm <- glm(Density ~ Bathy_c + Year, family = Gamma, data = dataset)

par(mfrow = c(2, 2))
plot(Densposit1.glm, which = c(1, 2, 3, 4))

# Poisson (or quasi poisson)
null.m <- glm(round(Density) ~ 1, family = poisson, data = dataset)
Densposit2.glm <- stepAIC(null.m, Density ~ (Bathy_c + Sedim + Year) ^ 2,
                           data = dataset, direction = "both")

par(mfrow = c(2, 2))
plot(Densposit2.glm, which = c(1, 2, 3, 4))

# Gaussian on log-transformed values
null.m <- glm(log(Density) ~ 1, data = dataset)
Densposit3.glm <- stepAIC(null.m, log(Density) ~ (Bathy + Sedim + Year) ^ 2,
                          data = dataset, direction = "both")

Densposit3.glm <- glm(log(Density) ~ poly(Bathy, 3) + Sedim,
                      family = gaussian, data = dataset)

# Analyse of model goodness of fit and residuals
par(mfcol = c(2, 2))
plot(Densposit3.glm, which = c(1, 2, 3, 4))

# Compare AIC
AIC(Densposit1.glm)
AIC(Densposit2.glm) # Be careful with data transformation !
AIC(Densposit3.glm) + 2 * sum(log(dataset$Density))


# _Additionnal analysis of residuals of the chosen model ----
model <- glm(log(Density) ~ poly(Bathy, 3) + Sedim,
             family = gaussian, data = dataset)
# Standardised Pearson residuals vs fitted
# => Test for the Homogeneity of the variance
g1 <- ggplot(data.frame(fitted = fitted(model),
                        residuals = rstandard(model, type = "pearson")),
             aes(fitted, residuals)) +
  geom_point() +
  geom_smooth()

# Residuals of Standardised Deviance vs fitted
# => Test for Normality and for influence of observations
g2 <- ggplot(data.frame(residuals = rstandard(model, type = "pearson"))) +
  geom_qq(aes(sample = residuals)) +
  geom_abline(slope = 1, intercept = 0, col = "red")

# Cook's distance
# => Influence of each observation on parameters estimation
g3 <- ggplot(data.frame(x = 1:nrow(dataset), cook = cooks.distance(model)), aes(x, cook)) +
  geom_bar(stat = "identity")

# Data vs Fitted
g4 <- ggplot(data.frame(data = log(dataset$Density), fitted = fitted(model)),
             aes(data, fitted)) +
  geom_point() +
  geom_smooth() +
  geom_abline(slope = 1, intercept = 0, col = "red")

gridExtra::grid.arrange(g1, g2, g3, g4, ncol = 2)









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


# Example for Delta model ----
a <- 4
m <- matrix(
  sample(x = c(0, 0, 0, 1, 2, 5, 10), size = a*a, replace = TRUE),
  ncol = a)
m.gg <- tbl_df(as.data.frame(m)) %>%
      mutate(y = rev(seq(1, a, length = a))) %>%
      tidyr::gather(x, value, -y) %>%
      mutate(x = rep(seq(1, a, length = a), each = a))

g <- ggplot(m.gg, aes(x, y)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), size = 8) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_void() +
  theme(legend.position = "none", 
        panel.border = element_rect(fill = "transparent")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))




print(g)


# Coupling the two sub-models ==================================================
# Sub-models selected ----
dataset.orig <- mutate(dataset.orig, Presence = 1*(Density > 0))
dataset.posit <- filter(dataset.orig, Presence == 1)

# Save dataset
readr::write_rds(dataset.orig, path = paste0(saveWD, "/dataset.orig.rds"))

# Run models
Dens01.glm <- glm(Presence ~ Bathy_c + Sedim,
                  family = binomial, data = dataset.orig)
Densposit.glm <- glm(log(Density) ~ poly(Bathy, 3),
                     family = gaussian, data = dataset.posit)






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

# Plot observations vs predictions
dataset.orig <- mutate(dataset.orig, DeltaPred = E.dens)

ggplot(dataset.orig, aes(Density, DeltaPred)) +  
  geom_point() +
  geom_jitter(width = 0, height = 15) +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  theme_classic()

ggplot(dataset.orig, aes(Density, DeltaPred)) +  
  geom_point() +
  stat_bin2d(aes(fill = ..count.., alpha = ..count..), bins = 300) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 20)) +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha_continuous(range = c(0.1, 0.8)) +
  guides(fill = FALSE, alpha = FALSE) +
  theme_classic()

# Warning : these are delta data, moreover with positive lognormal values
# Confidence intervals cannot be calculated with classical formula, they are not symetric
# Estimation errors is here a complex problem


gdataset <- filter(dataset.orig, Density < 30, Year %in% c(1990:2004))
g1 <- ggplot(gdataset) +
  geom_histogram(aes(Density), bins = 30) +
  coord_cartesian(xlim = c(-10, 30)) +
  ylab(NULL)

g2 <- ggplot(gdataset) +
  stat_summary(aes(x = "mean_sd", y = Density), fun.data = "mean_sdl") +
  coord_flip(ylim = c(-10, 30)) +
  xlab(NULL)
    
g3 <- ggplot(gdataset) +
  stat_summary(aes(x = "med_quant", y = Density), fun.y = median,
               fun.ymin = function(x) quantile(x, 0.1),
               fun.ymax = function(x) quantile(x, 0.9),
               geom = "crossbar") +
  coord_flip(ylim = c(-10, 30)) +
  xlab(NULL)

g <- cowplot::plot_grid(g1, g2, g3, labels = c("(a)", "(b)", "(c)"),
                        ncol = 1, rel_heights = c(3, 1, 1), align = "v")
g
rm(g);dev.off()




# Predictions for new places with known covariates -----------------------------
# Dowload the file with factors for predictions
predictions <- readr::read_delim(paste0(origWD, "/predictions.csv"),
                                 delim = ";")

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

# Compute predictions (in the scale of the response)
pred.Density <- mutate(predictions,
                       PA = presence.pred,
                       pos = E.logdens.pos,
                       exp.pos = E.dens.pos,
                       fit = E.dens) %>%
  group_by(Sedim, Bathy_c) %>%
  summarise(mean.fit = mean(fit))

ggplot(pred.Density, aes(Bathy_c, mean.fit, colour = Sedim)) +
  geom_point(cex = 2) +
  theme(legend.position = "bottom")


# Example of fit out of covariates bounds ----
ggplot(dataset.posit, 
       aes(x = -Bathy, y = log(Density))) +
  geom_point(cex = 0.8, col = "grey") +
  geom_boxplot(aes(group = cut_width(-Bathy, 2)),
               position = position_identity()) +
  # stat_summary(aes(x = round(-Bathy))) +
  geom_smooth(method = "glm", formula = y ~ x, 
              aes(colour = "y~x"), se = FALSE, fullrange = TRUE) +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2), 
              aes(colour = "y~poly(x,2)"), se = FALSE, fullrange = TRUE) +
  geom_smooth(method = "glm", formula = y ~ poly(x, 3), 
              aes(colour = "y~poly(x,3)"), se = FALSE, fullrange = TRUE) +
  xlim(0, 28) +
  theme_classic()



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
Dens01.glm <- glm(Presence ~ Bathy_class + Sediment_class,
                  family = binomial, data = dataset.orig)
Densposit.glm <- glm(log(Density) ~ poly(Bathy, 3),
                     family = gaussian, data = dataset.posit)


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


# Coastlines
Coasts_Vilaine_wgs84 <- readOGR(dsn = saveWD, layer = "Dept_Contour_wgs84",
                                verbose = FALSE) %>%
  broom::tidy()

# Combine all predictions
r.Predict <- stack(pred.pres, pred.logdens.pos, pred.dens.pos, pred.dens)
names(r.Predict) <- c("1.presence", "2a.log.pos", "2b.pos", "3.delta.pred")

plot(r.Predict)

# Only predictions
brk <- c(0,
         quantile(dataset.orig$Density, c(0.1, 0.5, 0.75, 0.95)),
         maxValue(pred.dens))

gplot(pred.dens) +
  geom_tile(aes(x, y, fill = value)) +
  geom_polygon(data = Coasts_Vilaine_wgs84, aes(long, lat, group = group),
               fill = "grey", col = "black") +
  coord_quickmap(xlim = c(xmin(r.Predict), xmax(r.Predict)),
                  ylim = c(ymin(r.Predict), ymax(r.Predict))) +
  scale_fill_gradientn(na.value = NA, colours = rev(heat.colors(5)), 
                       values = scales::rescale(brk), guide = "legend")


