################################ L-W modeling  ##########################################

#To determine whether L-W relationships differ significantly by site or not

#################################################################################################

######## Set-up ########

remove(list = ls())

#Set working directory
setwd("C:/Users/rclar/Dropbox/Pinsky_Lab/PIRE_Proj/REU_Summer_2019/Adriana_Project/Sphyraena_obtusata/")
getwd()

#load libraries
library(tidyverse)
library(ggpubr)
library(rsq)

#read in data
Obtusata <- read.csv("AC_Sphyraena_obtusata_Dat_for_R.csv")
colnames(Obtusata) <- c("Specimen_ID","Species","Location","Site","Date","Total_Length",
                        "Standard_Length","Weight","Gonad_Weight")
Obtusata <- subset(Obtusata, !is.na(Gonad_Weight)) #remove individuals with no gonad info
Obtusata$SL_cube <- (Obtusata$Standard_Length)^3
Obtusata$log_W <- log(Obtusata$Weight)
Obtusata$log_SL <- log(Obtusata$Standard_Length)

################################################################################################################################################

######## Visualize relationship with scatterplots ########

notranform_length_weightscatterplot <- ggplot(data = Obtusata, aes(y = Weight, x = Standard_Length, color = Location)) + 
  geom_point() + stat_function(fun = pwr)
  geom_smooth(method = "nls", formula = y ~ a*x^b, method.args = list(start = list(a=0.00288, b=3.304)), se = FALSE)
logtransform_length_weightscatterplot <- ggplot(data = Obtusata, aes(y = log_W, x = log_SL, color = Location)) + 
  geom_point() + geom_smooth()

pwr <- function(x) {
  0.00288*(x^3.304)
}

######## Checking correlation between SL & W ########

#if have high correlation, then SL would likely explain a lot of variation in W
#correlation of W to SL total
cor(Obtusata$Weight, Obtusata$Standard_Length) #0.947 --> makes sense as we know strong relationship

#########################################################################################

######## Building models ########
#using log-transformed values because know exponential relationship (W ~ L^3)

######## log_W ~ log_SL model ########

#building GLM for length
#W as a function of SL
W_length_model <- glm(log_W ~ log_SL, family = gaussian, data = Obtusata)
W_length_sum <- summary(W_length_model)
plot(W_length_model) #provides plots to check fit
W_length_AIC <- AIC(W_length_model) #-123.655
W_length_rsq <- rsq(W_length_model) #0.941

#pull slope and intercept
W_length_coef <- as.data.frame(W_length_sum$coefficients)
W_length_coef <- as.data.frame(t(W_length_coef$Estimate))
colnames(W_length_coef) <- c("intercept", "slope")

#write power function for transformation from log scale
W_length_coef$T_intercept <- exp(W_length_coef$intercept)
pwr_length <- function(x) {
  W_length_coef$T_intercept*(x^W_length_coef$slope)
}

#plot model
W_length_model_plot <- ggplot(data = Obtusata, aes(x = log_SL, y = log_W, color = Location)) + 
  geom_point() + geom_abline(slope = W_length_coef$slope, intercept = W_length_coef$intercept)
W_length_model_plot
noT_W_length_model_plot <- ggplot(data = Obtusata, aes(y = Weight, x = Standard_Length, color = Location)) + 
  geom_point() + stat_function(fun = pwr_length, color = "black")
noT_W_length_model_plot

######## log_W ~ Location model ########
#not sure about this section

#building GLM for location
#W as a function of location
W_location_model <- glm(log_W ~ Location, family = gaussian, data = Obtusata)
W_location_sum <- summary(W_location_model)
plot(W_location_model) #provides plots to check fit
W_location_AIC <- AIC(W_location_model) #229.280
W_location_rsq <- rsq(W_location_model) #0.221

#pull slope and intercept
W_location_coef <- as.data.frame(t(coef(W_location_model)))
W_location_coef$Kalibo_intercept <- W_location_coef$LocationKalibo + W_location_coef$`(Intercept)`

#write power function for transformation from log scale
W_location_coef$T_intercept <- exp(W_location_coef$`(Intercept)`)
W_location_coef$T_Kalibo_intercept <- exp(W_location_coef$Kalibo_intercept)
pwr_location_D <- function(x) {
  W_location_coef$T_intercept*(x)
}
pwr_location_K <- function(x) {
  W_location_coef$T_Kalibo_intercept*(x)
}

#plot model
W_location_model_plot <- ggplot(data = Obtusata, aes(x = log_SL, y = log_W, color = Location)) + 
  geom_point() + 
  geom_hline(yintercept = W_location_coef$Kalibo_intercept, color = "blue") + 
  geom_hline(yintercept = W_location_coef$`(Intercept)`, color = "red")
W_location_model_plot
noT_W_location_model_plot <- ggplot(data = Obtusata, aes(y = Weight, x = Standard_Length, color = Location)) + 
  geom_point() + stat_function(fun = pwr_location_K, color = "blue") + 
  stat_function(fun = pwr_location_D, color = "red")
noT_W_location_model_plot

######## log_W ~ log_SL + Location model ########

#building GLM for length & location
#W as a function of SL and location
W_length_location_model <- glm(log_W ~ log_SL + Location, family = gaussian, data = Obtusata)
W_length_location_sum <- summary(W_length_location_model)
plot(W_length_location_model) #provides plots to check fit
W_length_location_AIC <- AIC(W_length_location_model) #-316.373
W_length_location_rsq <- rsq(W_length_location_model) #0.986

#pull slope and intercept
W_length_location_coef <- as.data.frame(t(coef(W_length_location_model)))
W_length_location_coef$Kalibo_intercept <- W_length_location_coef$LocationKalibo + W_length_location_coef$`(Intercept)`

#write power functions for transformation from log scale
W_length_location_coef$T_intercept <- exp(W_length_location_coef$`(Intercept)`)
W_length_location_coef$T_Kalibo_intercept <- exp(W_length_location_coef$Kalibo_intercept)
pwr_length_location_D <- function(x) {
  W_length_location_coef$T_intercept*(x^W_length_location_coef$log_SL)
}
pwr_length_location_K <- function(x) {
  W_length_location_coef$T_Kalibo_intercept*(x^W_length_location_coef$log_SL)
}

#plot model
W_length_location_model_plot <- ggplot(data = Obtusata, aes(x = log_SL, y = log_W, color = Location)) + 
  geom_point() + 
  geom_abline(slope = W_length_location_coef$log_SL, intercept = W_length_location_coef$Kalibo_intercept, color = "blue") + 
  geom_abline(slope = W_length_location_coef$log_SL, intercept = W_length_location_coef$`(Intercept)`, color = "red")
W_length_location_model_plot
noT_W_length_location_model_plot <- ggplot(data = Obtusata, aes(y = Weight, x = Standard_Length, color = Location)) + 
  geom_point() + stat_function(fun = pwr_length_location_K, color = "blue") + 
  stat_function(fun = pwr_length_location_D, color = "red")
noT_W_length_location_model_plot

######## log_W ~ log_SL*Location model ########

#building multiple linear regression model for length & weight w/interaction term
#does relationship between W & SL vary by location (instead of additive W & location)
W_interaction_model <- glm(log_W ~ log_SL*Location, family = gaussian, data = Obtusata)
W_interaction_sum <- summary(W_interaction_model)
plot(W_interaction_model) #provides plots to check fit
W_interaction_AIC <- AIC(W_interaction_model) #-314.397
W_interaction_rsq <- rsq(W_interaction_model) #0.986

#pull slope and intercept
W_interaction_coef <- as.data.frame(t(coef(W_interaction_model)))
W_interaction_coef$Kalibo_intercept <- W_interaction_coef$LocationKalibo + W_interaction_coef$`(Intercept)`
W_interaction_coef$Kalibo_slope <- W_interaction_coef$log_SL + W_interaction_coef$`log_SL:LocationKalibo`

#write power functions for transformation from log scale
W_interaction_coef$T_intercept <- exp(W_interaction_coef$`(Intercept)`)
W_interaction_coef$T_Kalibo_intercept <- exp(W_interaction_coef$Kalibo_intercept)
pwr_interaction_D <- function(x) {
  W_interaction_coef$T_intercept*(x^W_interaction_coef$log_SL)
}
pwr_interaction_K <- function(x) {
  W_interaction_coef$T_Kalibo_intercept*(x^W_interaction_coef$Kalibo_slope)
}

#plot model
W_interaction_model_plot <- ggplot(data = Obtusata, aes(x = log_SL, y = log_W, color = Location)) + 
  geom_point() + 
  geom_abline(slope = W_interaction_coef$log_SL, intercept = W_interaction_coef$Kalibo_intercept, color = "blue") + 
  geom_abline(slope = W_interaction_coef$log_SL, intercept = W_interaction_coef$`(Intercept)`, color = "red")
W_interaction_model_plot
noT_W_interaction_model_plot <- ggplot(data = Obtusata, aes(y = Weight, x = Standard_Length, color = Location)) + 
  geom_point() + stat_function(fun = pwr_interaction_K, color = "blue") + 
  stat_function(fun = pwr_interaction_D, color = "red")
noT_W_interaction_model_plot
