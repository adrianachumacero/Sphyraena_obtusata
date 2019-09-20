######Setup#####
setwd("/Users/admir/Downloads/S.obtusata_Project")
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
