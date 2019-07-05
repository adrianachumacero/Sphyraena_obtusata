############################################################## L-W & GSI Example ##################################################################

#L-W pulled from fishR vignette by Dr. Derek Ogle (2013)
#GSI data pulled from study by Ghanbahadur & Ghanbahadur and published in Trends in Fisheries Research (2012)

################################################################################################################################################

######## Set-up ########

remove(list = ls())

#Set working directory
setwd("C:/Users/rclar/Dropbox/Pinsky_Lab/PIRE_Proj/REU_Summer_2019/Adriana_Project/")
getwd()

#load libraries
library("FSAdata")
library("tidyverse")

#read in data
data(RuffeSLRH92)
str(RuffeSLRH92)

################################################################################################################################################

######## Calculate length-weight relationship ########

#clean dataset to remove individuals with NAs for weight and length
ruffe2 <- subset(RuffeSLRH92, !is.na(weight) & !is.na(length))

#log-transform length and weight
ruffe2$logL <- log(ruffe2$length) #add column to dataframe that has log-transformed length
ruffe2$logW <- log(ruffe2$weight)

#run linear model with log-transformed weight and length
lm_lLlW <- lm(logW~logL, data = ruffe2)
lm_lLlW
summary(lm_lLlW) #show intercept, error & r2

#quick plot of model
log_LW_plot <- ggplot(data = ruffe2, aes(x = logL, y = logW)) + geom_point() + geom_smooth(method = 'lm')
LW_plot <- ggplot(data = ruffe2, aes(x = length, y = weight)) + geom_point()
log_LW_plot
LW_plot

###############################################################################################################################################

######## Calculate GSI ########

#create dataframe
fish_ID <- c("Cc_001", "Cc_002", "Cc_003", "Cc_004", "Cc_005", "Cc_006", "Cc_007", "Cc_008", "Cc_009", "Cc_010", "Cc_011", "Cc_012")
fish_body_weight <- c(1000, 1775, 1500, 1000, 800, 668, 776, 825.25, 699.25, 690.20, 680, 670)
fish_gonad_weight <- c(130, 260.96, 200, 110, 40, 50, 60, 100, 80, 70, 65, 60)
Ccarpio_df <- data.frame(fish_ID, fish_body_weight, fish_gonad_weight)

#calculate GSI
Ccarpio_df$GSI <- (Ccarpio_df$fish_gonad_weight / Ccarpio_df$fish_body_weight) * 100
