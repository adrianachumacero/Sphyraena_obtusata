############################################################## L-W & GSI Example ##################################################################

#L-W pulled from fishR vignette by Dr. Derek Ogle (2013)
#GSI data pulled from study by Ghanbahadur & Ghanbahadur and published in Trends in Fisheries Research (2012)

################################################################################################################################################

######## Set-up ########

remove(list = ls())

#Set working directory
setwd("C:/Users/rclar/Dropbox/Pinsky_Lab/PIRE_Proj/REU_Summer_2019/Adriana_Project/Sphyraena_obtusata/")
getwd()

#load libraries
library(FSAdata)
library(tidyverse)
library(ggpubr)
library(Rmisc)

#read in data
data(RuffeSLRH92)
str(RuffeSLRH92)

################################################################################################################################################

######## Check for normality w/raw data########

#clean dataset to remove individuals with NAs for weight and length
ruffe2 <- subset(RuffeSLRH92, !is.na(weight) & !is.na(length))

#density plot to see if distribution of length is normal
length_density <- ggdensity(ruffe2$length,
                            main = "Density plot of length",
                            xlab = "Fish length")
length_density #want to see if there is a bell curve or not (here, it is roughly bell-shaped)

#Q-Q plot (quantile-quantile) to see if distribution of length is normal
length_qq <- ggqqplot(ruffe2$length)
length_qq #want to see data points falling on 45-degree line or within CI (shaded region) here, tailing off at the end, suggesting curve is skewed to left -- more samples than would expect with short lengths and fewer samples than would expect with long lengths

#Shapiro-Wilk's test for normality for length
length_shapiro <- shapiro.test(ruffe2$length) #visualization (density & Q-Q plots) is useful but statistical test most powerful
length_shapiro #here, p-value tells whether distribution is significantly different from normal or not. If p > 0.05, then distribution is different than normal (here, it is)

######## Check for normality w/log-transformed data########

#since raw data is not normally distributed, log-transform dataa & check for normality
ruffe2$logL <- log(ruffe2$length) #add column to dataframe that has log-transformed length

#density plot to see if distribution of log-transformed length is normal
log_length_density <- ggdensity(ruffe2$logL,
                            main = "Density plot of log-transformed length",
                            xlab = "Log-transformed length")
log_length_density #now curve heavily skewed to right

#Q-Q plot (quantile-quantile) to see if distribution of log-transformed length is normal
log_length_qq <- ggqqplot(ruffe2$logL)
log_length_qq #now even farther off 45-degree line

#Shapiro-Wilk's test for normality for log-transformed length
log_length_shapiro <- shapiro.test(ruffe2$logL) 
log_length_shapiro #unsurprisingly (given results of visualization), log-transformed data even less of a normal distribution than before

#BUT since using very large dataset (n > 30), can assume Central Limit Theorem holds and proceed as if normally distributed
#Always important to check, however bc if raw data was very skewed may run into issues later down the line (here, raw distribution looks ~bell-shaped)

################################################################################################################################################

######## Calculate 95% CI for mean length ########

#calculate length statistics
length_mean <- mean(ruffe2$length) #calculate mean
length_SD <- sd(ruffe2$length) #calculate standard deviation
length_n <- 736 #set sample size

#calculate standard error for length
length_SE <- qnorm(0.975)*(length_SD/sqrt(length_n)) #calculate standard error by hand for 95% CI

#get bounds for 95% CI
lower_bound_length_CI <- length_mean - length_SE
upper_bound_length_CI <- length_mean + length_SE

length_95_CI <- CI(ruffe2$length, ci = 0.95) #another way to get confidence interval using Rmisc package

################################################################################################################################################

######## Calculate length-weight relationship ########

#log-transform weight
ruffe2$logW <- log(ruffe2$weight) #add column to dataframe that has log-transformed weight

#run linear model with log-transformed weight and length
lm_lLlW <- lm(logW~logL, data = ruffe2)
lm_lLlW
summary(lm_lLlW) #show intercept, error & r2

#quick plot of model
log_LW_plot <- ggplot(data = ruffe2, aes(x = logL, y = logW)) + geom_point() + geom_smooth(method = 'lm')
LW_plot <- ggplot(data = ruffe2, aes(x = length, y = weight)) + geom_point() + geom_smooth(method = 'auto') + annotate(geom = "text", x = 50, y = 75, label = "power curve")
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

##############################################################################################################################################

######## anova example ########

lm_1 <- lm(logW ~ logL, data = ruffe2[1:368,])
lm_2 <- lm(logL ~ logW, data = ruffe2[369:736,])
anova(lm_1, lm_2)