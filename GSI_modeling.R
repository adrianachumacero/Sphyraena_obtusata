################################ GSI modeling  ##########################################

#To determine whether relationship between GSI & length differs significantly by site or not

#################################################################################################

######## Set-up ########

remove(list = ls())

#Set working directory
setwd("C:/Users/rclar/Dropbox/Pinsky_Lab/PIRE_Proj/REU_Summer_2019/Adriana_Project/Sphyraena_obtusata/")
getwd()

#load libraries
library(tidyverse)
library(ggpubr)
library(lme4)

#read in data
Obtusata <- read.csv("AC_Sphyraena_obtusata_Dat_for_R.csv")
colnames(Obtusata) <- c("Specimen_ID","Species","Location","Site","Date","Total_Length",
                        "Standard_Length","Weight","Gonad_Weight")
Obtusata <- subset(Obtusata, !is.na(Gonad_Weight)) #remove individuals with no gonad info
Obtusata$GSI <- (Obtusata$Gonad_Weight/Obtusata$Weight)*100 #add GSI parameter
Obtusata$SL_cube <- (Obtusata$Standard_Length)^3

################################################################################################################################################

######## Visualize relationship with scatterplots ########
GSI_lengthscatterplot <- ggplot(data = Obtusata, aes(y = GSI, x = Standard_Length, color = Location)) + 
  geom_point() + geom_smooth(method = "lm")
GSI_lengthscatterplot #geom_smooth used loess method (locally weighted least squares regression)

######## Visualize distributions with boxplots & density plots ########

#subsetting data to each pop to compare within pops
Obtusata_Dumaguete <- subset(Obtusata, Location == "Dumaguete")
Obtusata_Kalibo <- subset(Obtusata, Location == "Kalibo")

#boxplots for GSI
boxplot(Obtusata_Dumaguete$GSI, main = "GSI Dumaguete")
boxplot(Obtusata_Kalibo$GSI, main = "GSI Kalibo")

#boxplots for standard length
boxplot(Obtusata_Dumaguete$Standard_Length, main = "SL Dumaguete")
boxplot(Obtusata_Kalibo$Standard_Length, main = "SL Kalibo")

#boxplots for GSI & SL combined
boxplot(Obtusata$GSI, main = "GSI") #lots of outliers so seems like def shouldn't be treated as one unit
boxplot(Obtusata$Standard_Length, main = "SL")

#density plots for GSI
ggdensity(Obtusata_Dumaguete$GSI, main = "GSI Dumaguete")
ggdensity(Obtusata_Kalibo$GSI, main = "GSI Kalibo")

#density plots for SL
ggdensity(Obtusata_Dumaguete$Standard_Length, main = "SL Dumaguete")
ggdensity(Obtusata_Kalibo$Standard_Length, main = "SL Kalibo")

#density plots for GSI & SL combined
ggdensity(Obtusata$GSI, main = "GSI")
ggdensity(Obtusata$Standard_Length, main = "SL")

######## Checking correlation between GSI & SL ########

#if have high correlation, then SL would likely explain a lot of variation in GSI
#correlation of GSI to SL total
cor(Obtusata$GSI, Obtusata$Standard_Length) #0.43 --> not super high but also not 0

#correlation of GSI to SL Dumaguete
cor(Obtusata_Dumaguete$GSI, Obtusata_Dumaguete$Standard_Length) #0.46 #weak but not none

#correlation of GSI to SL Kalibo
cor(Obtusata_Kalibo$GSI, Obtusata_Kalibo$Standard_Length) #0.54 #weak but not none

########################################################################################

######## Building models ########

######## GSI ~ SL model ########

#building GLM for length
#GSI as a function of SL
GSI_length_model <- glm(GSI ~ Standard_Length, family = gaussian, data = Obtusata)
GSI_length_sum <- summary(GSI_length_model)
plot(GSI_length_model) #provides plots to check fit
GSI_length_AIC <- AIC(GSI_length_model) #376.981

#pull slope and intercept
GSI_length_coef <- as.data.frame(GSI_length_sum$coefficients)
GSI_length_coef <- as.data.frame(t(GSI_length_coef$Estimate))
colnames(GSI_length_coef) <- c("intercept", "slope")

#plot model
GSI_length_model_plot <- ggplot(data = Obtusata, aes(x = Standard_Length, y = GSI, color = Location)) + 
  geom_point() + geom_abline(slope = GSI_length_coef$slope, intercept = GSI_length_coef$intercept)
GSI_length_model_plot

######## GSI ~ Location model ########

#building GLM for location
#GSI as a function of location
#location is random effect variable bc generalizing to other groups (other sampling sites)
#treat as random even though only two levels?
GSI_location_model <- glm(GSI ~ Location, family = gaussian, data = Obtusata)
GSI_location_sum <- summary(GSI_location_model)
plot(GSI_location_model) #provides plots to check fit
GSI_location_AIC <- AIC(GSI_location_model) #327.173

#pull intercepts
GSI_location_coef <- as.data.frame(t(coef(GSI_location_model)))
GSI_location_coef$Kalibo_intercept <- GSI_location_coef$LocationKalibo + GSI_location_coef$`(Intercept)`

#plot model
GSI_location_model_plot <- ggplot(data = Obtusata, aes(x = Standard_Length, y = GSI, color = Location)) + 
  geom_point() + geom_hline(yintercept = GSI_location_coef$Kalibo_intercept, color = "blue") + 
  geom_hline(yintercept = GSI_location_coef$`(Intercept)`, color = "red")
GSI_location_model_plot

######## GSI ~ SL + Location model ########

#building GLM for length & location
#GSI as a function of SL and location
#GSI_length_location_model <- glmer(GSI ~ Standard_Length + (1|Location), family = gaussian, data = Obtusata)
GSI_length_location_model <- glm(GSI ~ Standard_Length + Location, family = gaussian, data = Obtusata)
GSI_length_location_sum <- summary(GSI_length_location_model)
plot(GSI_length_location_model) #provides plots to check fit
GSI_length_location_AIC <- AIC(GSI_length_location_model) #309.519

#pull slope and intercept
GSI_length_location_coef <-as.data.frame(t(coef(GSI_length_location_model))) #gives coefficients for modeling
GSI_length_location_coef$Kalibo_intercept <- GSI_length_location_coef$LocationKalibo + GSI_length_location_coef$`(Intercept)`

#plot model
GSI_length_location_model_plot <- ggplot(data = Obtusata, aes(x = Standard_Length, y = GSI, color = Location)) + 
  geom_point() + 
  geom_abline(slope = GSI_length_location_coef$Standard_Length, intercept = GSI_length_location_coef$Kalibo_intercept, color = "blue") + 
  geom_abline(slope = GSI_length_location_coef$Standard_Length, intercept = GSI_length_location_coef$`(Intercept)`, color = "red")
GSI_length_location_model_plot

######## GSI ~ SL*Location model ########

#building multiple linear regression model for length & location w/interaction term
#does relationship between GSI & SL vary by location (instead of additive SL & location)
#GSI_interaction_model <- glmer(GSI ~ (Standard_Length | Location), family = gaussian, data = Obtusata)
GSI_interaction_model <- glm(GSI ~ Standard_Length*Location, family = gaussian, data = Obtusata)
GSI_interaction_sum <- summary(GSI_interaction_model)
plot(GSI_interaction_model) #provides plots to check fit
GSI_interaction_AIC <- AIC(GSI_interaction_model) #288.738

#pull slope and intercept
GSI_interaction_coef <- as.data.frame(t(coef(GSI_interaction_model)))
GSI_interaction_coef$Kalibo_intercept <- GSI_interaction_coef$LocationKalibo + GSI_interaction_coef$`(Intercept)`
GSI_interaction_coef$Kalibo_slope <- GSI_interaction_coef$Standard_Length + GSI_interaction_coef$`Standard_Length:LocationKalibo`

#plot model
GSI_interaction_plot <- ggplot(data = Obtusata, aes(x = Standard_Length, y = GSI, color = Location)) + 
  geom_point() + 
  geom_abline(slope = GSI_interaction_coef$Kalibo_slope, intercept = GSI_interaction_coef$Kalibo_intercept, color = "blue") + 
  geom_abline(slope = GSI_interaction_coef$Standard_Length, intercept = GSI_interaction_coef$`(Intercept)`, color = "red")
GSI_interaction_plot