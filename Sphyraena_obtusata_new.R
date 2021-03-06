######Setup#####
setwd("/Users/admir/Downloads/S.obtusata_Project/Sphyraena_obtusata")
Packages <- c("tidyverse")
invisible(suppressPackageStartupMessages(lapply(Packages,library,character.only = TRUE)))

######Load files needed#####
Obtusata <- read.csv("AC_Sphyraena_obtusata_Dat_for_R.csv")
colnames(Obtusata) <- c("Specimen_ID","Species","Location","Site","Date","Total_Length",
                          "Standard_Length","Weight","Gonad_Weight")

####################Dumaguete####################
#####Calculating LWR#####

#subset data to include only Dumaguete population
Obtusata_DGT<- subset(Obtusata, Location == "Dumaguete")

#remove NAs from weight and total length
Obtusata_DGT_LWR <- subset(Obtusata_DGT, !is.na(Weight) & !is.na(Total_Length))

#log-transform L and W
Obtusata_DGT_LWR$logL <- log(Obtusata_DGT_LWR$Total_Length)
Obtusata_DGT_LWR$logW <- log(Obtusata_DGT_LWR$Weight)

#create linear model with log-transformed W and L
full_lm_lLlW <- lm(logW~logL, data = Obtusata_DGT_LWR)
full_lm_lLlW
summary(full_lm_lLlW)

#plot model
Obtusata_DGT_logLW_plot <- ggplot(data = Obtusata_DGT_LWR, aes(x = logL, y = logW)) +
  geom_point() + geom_smooth(method = "lm") + ggtitle(label = "logLWR of S.obtusata from Dumaguete")
Obtusata_DGT_LW_plot <- ggplot(data = Obtusata_DGT_LWR, aes(x = Total_Length, y = Weight)) +
  geom_point() + geom_smooth(method = "nls", formula = y ~ a*x^b, start = list(a = 0.0035597, b = 3.059256), data = Obtusata_DGT_LWR, se = F) +
  ggtitle(label = "LWR of S.obtusata from Dumaguete")

Obtusata_DGT_logLW_plot
Obtusata_DGT_LW_plot

#####Calculating GSI#####

#remove NAs from gonad weight
Obtusata_DGT_GSI <- subset(Obtusata_DGT, !is.na(Gonad_Weight))

#raw GSI and TL
Obtusata_DGT_GSI$GSI <- (Obtusata_DGT_GSI$Gonad_Weight/Obtusata_DGT_GSI$Weight) * 100
ggplot(data = Obtusata_DGT_GSI, aes(x = Total_Length, y = GSI)) + 
  geom_point() + ggtitle(label = "Relationship between GSI and total length", subtitle = "From Dumaguete")

mean(Obtusata_DGT_GSI[["GSI"]])

#####Calculating Fulton's Condition Factor#####
Obtusata_DGT$K <- 100 * (Obtusata_DGT$Weight/(Obtusata_DGT$Total_Length)^3)

####################Kalibo####################
#####Calculating LWR#####

#subset data to include only Kalibo population
Obtusata_KAL <- subset(Obtusata, Location == "Kalibo")

#remove NAs from weight and total length
Obtusata_KAL_LWR <- subset(Obtusata_KAL, !is.na(Weight) & !is.na(Total_Length))

#log-transform L and W
Obtusata_KAL_LWR$logL <- log(Obtusata_KAL_LWR$Total_Length)
Obtusata_KAL_LWR$logW <- log(Obtusata_KAL_LWR$Weight)

#create linear model with log-transformed W and L
full_lm_lLlW <- lm(logW~logL, data = Obtusata_KAL_LWR)
full_lm_lLlW
summary(full_lm_lLlW)

#plot model
Obtusata_KAL_logLW_plot <- ggplot(data = Obtusata_KAL_LWR, aes(x = logL, y = logW)) +
  geom_point() + geom_smooth(method = "lm") + ggtitle(label = "logLWR of S.obtusata from Kalibo")
Obtusata_KAL_LW_plot <- ggplot(data = Obtusata_KAL_LWR, aes(x = Total_Length, y = Weight)) +
  geom_point() + geom_smooth(method = "nls", formula = y ~ a*x^b, start = list(a = 0.004027, b = 3.106475), data = Obtusata_KAL_LWR, se = F) +
  ggtitle(label = "LWR of S.obtusata from Kalibo")

Obtusata_KAL_logLW_plot
Obtusata_KAL_LW_plot

#####Calculating GSI#####

#remove NAs from gonad weight
Obtusata_KAL_GSI <- subset(Obtusata_KAL, !is.na(Gonad_Weight))

#raw GSI and TL
Obtusata_KAL_GSI$GSI <- (Obtusata_KAL_GSI$Gonad_Weight/Obtusata_KAL_GSI$Weight) * 100
ggplot(data = Obtusata_KAL_GSI, aes(x = Total_Length, y = GSI)) + 
  geom_point() + ggtitle(label = "Relationship between GSI and total length", subtitle = "From Kalibo")

mean(Obtusata_KAL_GSI[["GSI"]])

#####Calculating Fulton's Condition Factor#####
Obtusata_KAL$K <- 100 * (Obtusata_KAL$Weight/(Obtusata_KAL$Total_Length)^3)

####################Both####################
#####Calculating LWR#####

#remove NAs from weight and total length
Obtusata_full_LWR <- subset(Obtusata, !is.na(Weight) & !is.na(Total_Length))

#log-transform L and W
Obtusata_full_LWR$logL <- log(Obtusata_full_LWR$Total_Length)
Obtusata_full_LWR$logW <- log(Obtusata_full_LWR$Weight)

#create linear model with log-transformed W and L
full_lm_lLlW <- lm(logW~logL, data = Obtusata_full_LWR)
full_lm_lLlW
summary(full_lm_lLlW)

#plot model
Obtusata_full_logLW_plot <- ggplot(data = Obtusata_full_LWR, aes(x = logL, y = logW, colour = Location)) +
  geom_point() + geom_smooth(method = "lm") + ggtitle(label = "logLWR of S.obtusata from 2 populations")
Obtusata_full_LW_plot <- ggplot(data = Obtusata_full_LWR, aes(x = Total_Length, y = Weight, colour = Location)) +
  geom_point() + geom_smooth(method = "nls", formula = y ~ a*x^b, start = list(a = -6.16923, b = 3.24846), data = Obtusata_full_LWR, se = F) +
  ggtitle(label = "LWR of S.obtusata from 2 populations")

Obtusata_full_logLW_plot
Obtusata_full_LW_plot

#####Calculating GSI#####

#remove NAs from gonad weight
Obtusata_full_GSI <- subset(Obtusata, !is.na(Gonad_Weight))

#raw GSI and TL
Obtusata_full_GSI$GSI <- (Obtusata_full_GSI$Gonad_Weight/Obtusata_full_GSI$Weight) * 100
ggplot(data = Obtusata_full_GSI, aes(x = Total_Length, y = GSI, colour = Location)) + 
  geom_point() + ggtitle(label = "Relationship between GSI and total length", subtitle = "From 2 populations")

mean(Obtusata_full_GSI[["GSI"]])

####################Sub-setting data to include only lengths seen in Dumaguete AND Kalibo####################
####Sub-setting for total length between 21cm and 30cm#####

Obtusata_sub <- subset(Obtusata, Total_Length > 21 & Total_Length < 30)

#####Running the rest of my analysis on subset dataframe#####

#log transform L and W
Obtusata_sub$logL <- log(Obtusata_sub$Total_Length)
Obtusata_sub$logW <- log(Obtusata_sub$Weight)

#create linear model with log-transformed W and L
full_lm_lLlW <- lm(logW~logL, data = Obtusata_sub)
full_lm_lLlW
summary(full_lm_lLlW)

#plot model
Obtusata_sub_logLW_plot <- ggplot(data = Obtusata_sub, aes(x = logL, y = logW, colour = Location)) +
  geom_point() + geom_smooth(method = "lm") + ggtitle(label = "logLWR of S.obtusata from 2 populations")
Obtusata_sub_LW_plot <- ggplot(data = Obtusata_sub, aes(x = Total_Length, y = Weight, colour = Location)) +
  geom_point() + geom_smooth(method = "nls", formula = y ~ a*x^b, start = list(a = 0.0035597, b = 3.059256), data = Obtusata_sub, se = F) +
  ggtitle(label = "LWR of S.obtusata from 2 populations")

Obtusata_sub_logLW_plot
Obtusata_sub_LW_plot

#####Calculating GSI#####

#remove NAs from gonad weight
Obtusata_sub_GSI <- subset(Obtusata_sub, !is.na(Gonad_Weight))

#raw GSI and TL
Obtusata_sub_GSI$GSI <- (Obtusata_sub_GSI$Gonad_Weight/Obtusata_sub_GSI$Weight) * 100
ggplot(data = Obtusata_sub_GSI, aes(x = Total_Length, y = GSI, colour = Location)) + 
  geom_point() + ggtitle(label = "Relationship between GSI and total length", subtitle = "From 2 populations")

#####BOXPLOTS#####

boxplot(GSI~Location, data=Obtusata_sub_GSI, main="GSI by location", 
        xlab="Location", ylab="GSI", col=(c("red","blue")))
boxplot(Total_Length~Location, data=Obtusata_sub, main="Total length by location", 
        xlab="Location", ylab="Total Length", col=(c("red","blue")))
boxplot(Weight~Location, data=Obtusata_sub, main="Weight by location", 
        xlab="Location", ylab="Weight", col=(c("red","blue")))

#####MANN - WHITNEY U TEST#####

wilcox.test(Obtusata_sub_GSI$GSI~Obtusata_sub_GSI$Location)
wilcox.test(Obtusata_sub$Total_Length~Obtusata_sub$Location)
wilcox.test(Obtusata_sub$Weight~Obtusata_sub$Location)