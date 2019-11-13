######## Set-up ########

remove(list = ls())

#Set working directory
setwd("/Users/admir/Downloads/S.obtusata_Project/Sphyraena_obtusata")
Packages <- c("tidyverse","ggpubr","Rmisc")
invisible(suppressPackageStartupMessages(lapply(Packages,library,character.only = TRUE)))

#read in data
Obtusata <- read.csv("AC_Sphyraena_obtusata_Dat_for_R.csv")
colnames(Obtusata) <- c("Specimen_ID","Species","Location","Site","Date","Total_Length",
                        "Standard_Length","Weight","Gonad_Weight")

################################################################################################################################################

######## Check for normality w/raw data########

###################DUMAGUETE ONLY######################

###########LENGTH##########
Obtusata_DGT <- subset(Obtusata, Location == "Dumaguete") #subsetting data to only include fish from Dumaguete

length_density_DGT <- ggdensity(Obtusata_DGT$Total_Length, main = "Density plot of length for Dumaguete", xlab = "Fish length")
length_density_DGT

length_qq_DGT <- ggqqplot(Obtusata_DGT$Total_Length)
length_qq_DGT

#Shapiro-Wilk's statistical test for normality 
length_shapiro_DGT <- shapiro.test(Obtusata_DGT$Total_Length) #visualization (density & Q-Q plots) is useful but statistical test most powerful
length_shapiro_DGT

##LOG TRANSFORM##
Obtusata_DGT$logL <- log(Obtusata_DGT$Total_Length)

log_length_density_DGT <- ggdensity(Obtusata_DGT$logL, main = "Density plot of log-transformed length for DGT", xlab = "Log-transformed length")
log_length_density_DGT

#Q-Q plot (quantile-quantile) to see if distribution of log-transformed length is normal
log_length_qq_DGT <- ggqqplot(Obtusata_DGT$logL)
log_length_qq_DGT

#Shapiro-Wilk's statistical test for normality for log-transformed length
log_length_shapiro_DGT <- shapiro.test(Obtusata_DGT$logL) 
log_length_shapiro_DGT

##########WEIGHT##########
weight_density_DGT <- ggdensity(Obtusata_DGT$Weight, main = "Density plot of weight for Dumaguete", xlab = "Fish weight")
weight_density_DGT

weight_qq_DGT <- ggqqplot(Obtusata_DGT$Weight)
weight_qq_DGT

#Shapiro-Wilk's statistical test for normality 
weight_shapiro_DGT <- shapiro.test(Obtusata_DGT$Weight) 
weight_shapiro_DGT

##LOG TRANSFORM##
Obtusata_DGT$logW <- log(Obtusata_DGT$Weight)

log_weight_density_DGT <- ggdensity(Obtusata_DGT$logW, main = "Density plot of log-transformed weight for DGT", xlab = "Log-transformed weight")
log_weight_density_DGT

#Q-Q plot (quantile-quantile) to see if distribution of log-transformed length is normal
log_weight_qq_DGT <- ggqqplot(Obtusata_DGT$logL)
log_weight_qq_DGT

#Shapiro-Wilk's statistical test for normality for log-transformed weight
log_weight_shapiro_DGT <- shapiro.test(Obtusata_DGT$logW) 
log_weight_shapiro_DGT

##############GSI###########
Obtusata_DGT_GSI <- subset(Obtusata_DGT, !is.na(Gonad_Weight)) #get rid of missing gonad weights
Obtusata_DGT_GSI$GSI <- (Obtusata_DGT_GSI$Gonad_Weight/Obtusata_DGT_GSI$Weight) * 100 #calculate GSI

GSI_density_DGT <- ggdensity(Obtusata_DGT_GSI$GSI, main = "Density plot of GSI for Dumaguete", xlab = "Fish GSI")
GSI_density_DGT

GSI_qq_DGT <- ggqqplot(Obtusata_DGT_GSI$GSI)
GSI_qq_DGT

#Shapiro-Wilk's statistical test for normality 
GSI_shapiro_DGT <- shapiro.test(Obtusata_DGT_GSI$GSI) 
GSI_shapiro_DGT

#LOG TRANSFORM#
Obtusata_DGT_GSI$logGSI <- log(Obtusata_DGT_GSI$GSI)

log_GSI_density_DGT <- ggdensity(Obtusata_DGT_GSI$logGSI, main = "Density plot of log-tranformed GSI for DGT", xlab = "Log-transformed GSI")
log_GSI_density_DGT

log_GSI_qq_DGT <- ggqqplot(Obtusata_DGT_GSI$logGSI)
log_GSI_qq_DGT

#Shapiro-Wilk's statistical test for normality for log-transformed GSI
log_GSI_shapiro_DGT <- shapiro.test(Obtusata_DGT_GSI$logGSI) 
log_GSI_shapiro_DGT


####################KALIBO ONLY#######################
Obtusata_KAL <- subset(Obtusata, Location == "Kalibo") #subsetting data to only include fish from Kalibo

length_density_KAL <- ggdensity(Obtusata_KAL$Total_Length, main = "Density plot of length for Kalibo", xlab = "Fish length")
length_density_KAL

length_qq_KAL <- ggqqplot(Obtusata_KAL$Total_Length)
length_qq_KAL

#Shapiro-Wilk's statistical test for normality 
length_shapiro_KAL <- shapiro.test(Obtusata_KAL$Total_Length) 
length_shapiro_KAL

##LOG TRANSFORM##
Obtusata_KAL$logL <- log(Obtusata_KAL$Total_Length)

log_length_density_KAL <- ggdensity(Obtusata_KAL$logL, main = "Density plot of log-transformed length for KAL", xlab = "Log-transformed length")
log_length_density_KAL

#Q-Q plot (quantile-quantile) to see if distribution of log-transformed length is normal
log_length_qq_KAL <- ggqqplot(Obtusata_KAL$logL)
log_length_qq_KAL

#Shapiro-Wilk's statistical test for normality for log-transformed length
log_length_shapiro_KAL <- shapiro.test(Obtusata_KAL$logL) 
log_length_shapiro_KAL

##########WEIGHT##########
weight_density_KAL <- ggdensity(Obtusata_KAL$Weight, main = "Density plot of weight for Kalibo", xlab = "Fish weight")
weight_density_KAL

weight_qq_KAL <- ggqqplot(Obtusata_KAL$Weight)
weight_qq_KAL

#Shapiro-Wilk's statistical test for normality 
weight_shapiro_KAL <- shapiro.test(Obtusata_KAL$Weight) 
weight_shapiro_KAL

##LOG TRANSFORM##
Obtusata_KAL$logW <- log(Obtusata_KAL$Weight)

log_weight_density_KAL <- ggdensity(Obtusata_KAL$logW, main = "Density plot of log-transformed weight for KAL", xlab = "Log-transformed weight")
log_weight_density_KAL

#Q-Q plot (quantile-quantile) to see if distribution of log-transformed length is normal
log_weight_qq_KAL <- ggqqplot(Obtusata_KAL$logL)
log_weight_qq_KAL

#Shapiro-Wilk's statistical test for normality for log-transformed weight
log_weight_shapiro_KAL <- shapiro.test(Obtusata_KAL$logW) 
log_weight_shapiro_KAL

##############GSI###########
Obtusata_KAL_GSI <- subset(Obtusata_KAL, !is.na(Gonad_Weight)) #get rid of missing gonad weights
Obtusata_KAL_GSI$GSI <- (Obtusata_KAL_GSI$Gonad_Weight/Obtusata_KAL_GSI$Weight) * 100 #calculate GSI

GSI_density_KAL <- ggdensity(Obtusata_KAL_GSI$GSI, main = "Density plot of GSI for Kalibo", xlab = "Fish GSI")
GSI_density_KAL

GSI_qq_KAL <- ggqqplot(Obtusata_KAL_GSI$GSI)
GSI_qq_KAL

#Shapiro-Wilk's statistical test for normality 
GSI_shapiro_KAL <- shapiro.test(Obtusata_KAL_GSI$GSI) 
GSI_shapiro_KAL

#LOG TRANSFORM#
Obtusata_KAL_GSI$logGSI <- log(Obtusata_KAL_GSI$GSI)

log_GSI_density_KAL <- ggdensity(Obtusata_KAL_GSI$logGSI, main = "Density plot of log-tranformed GSI for KAL", xlab = "Log-transformed GSI")
log_GSI_density_KAL

log_GSI_qq_KAL <- ggqqplot(Obtusata_KAL_GSI$logGSI)
log_GSI_qq_KAL

#Shapiro-Wilk's statistical test for normality for log-transformed GSI
log_GSI_shapiro_KAL <- shapiro.test(Obtusata_KAL_GSI$logGSI) 
log_GSI_shapiro_KAL



#############################FULL FULL FULL FULL FULL DATASET##############################
#clean dataset to remove individuals with NAs for weight and length
Obtusata <- subset(Obtusata, !is.na(Weight) & !is.na(Total_Length))

#density plot to see if distribution of length is normal
length_density <- ggdensity(Obtusata$Total_Length,
                            main = "Density plot of length",
                            xlab = "Fish length")
length_density #want to see if there is a bell curve or not (here, it is roughly bell-shaped)

#Q-Q plot (quantile-quantile) to see if distribution of length is normal
length_qq <- ggqqplot(Obtusata$Total_Length)
length_qq #want to see data points falling on 45-degree line or within CI (shaded region) here, tailing off at the end, suggesting curve is skewed to left -- more samples than would expect with short lengths and fewer samples than would expect with long lengths

#Shapiro-Wilk's test for normality for length
length_shapiro <- shapiro.test(Obtusata$Total_Length) #visualization (density & Q-Q plots) is useful but statistical test most powerful
length_shapiro #here, p-value tells whether distribution is significantly different from normal or not. If p > 0.05, then distribution is different than normal (here, it is)

######## Check for normality w/log-transformed data########

#since raw data is not normally distributed, log-transform dataa & check for normality
Obtusata$logL <- log(Obtusata$Total_Length) #add column to dataframe that has log-transformed length

#density plot to see if distribution of log-transformed length is normal
log_length_density <- ggdensity(Obtusata$logL,
                                main = "Density plot of log-transformed length",
                                xlab = "Log-transformed length")
log_length_density #now curve heavily skewed to right

#Q-Q plot (quantile-quantile) to see if distribution of log-transformed length is normal
log_length_qq <- ggqqplot(Obtusata$logL)
log_length_qq #now even farther off 45-degree line

#Shapiro-Wilk's test for normality for log-transformed length
log_length_shapiro <- shapiro.test(Obtusata$logL) 
log_length_shapiro #unsurprisingly (given results of visualization), log-transformed data even less of a normal distribution than before

#BUT since using very large dataset (n > 30), can assume Central Limit Theorem holds and proceed as if normally distributed
#Always important to check, however bc if raw data was very skewed may run into issues later down the line (here, raw distribution looks ~bell-shaped)

################################################################################################################################################

######## Calculate 95% CI for mean length ########

#calculate length statistics
length_mean <- mean(Obtusata$Total_Length) #calculate mean
length_SD <- sd(Obtusata$Total_Length) #calculate standard deviation
length_n <- 148 #set sample size

#calculate standard error for length
length_SE <- qnorm(0.975)*(length_SD/sqrt(length_n)) #calculate standard error by hand for 95% CI

#get bounds for 95% CI
lower_bound_length_CI <- length_mean - length_SE
upper_bound_length_CI <- length_mean + length_SE

length_95_CI <- CI(Obtusata$Total_Length, ci = 0.95) #another way to get confidence interval using Rmisc package

################################################################################################################################################

######## Calculate length-weight relationship ########

#log-transform weight
Obtusata$logW <- log(Obtusata$Weight) #add column to dataframe that has log-transformed weight

#run linear model with log-transformed weight and length
lm_lLlW <- lm(logW~logL, data = Obtusata)
lm_lLlW
summary(lm_lLlW) #show intercept, error & r2

#quick plot of model
log_LW_plot <- ggplot(data = Obtusata, aes(x = logL, y = logW)) + geom_point() + geom_smooth(method = 'lm')
LW_plot <- ggplot(data = Obtusata, aes(x = Total_Length, y = Weight)) + geom_point() + geom_smooth(method = 'auto') + annotate(geom = "text", x = 50, y = 75, label = "power curve")
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