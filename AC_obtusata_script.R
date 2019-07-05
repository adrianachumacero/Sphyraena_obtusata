######Setup#####
setwd("/Users/admir/Downloads/S.obtusata_Project")
library("tidyverse")

######Load files needed#####
Obtusata_dat <- read.csv("AC_Sphyraena_obtusata_Dat_for_R.csv")
colnames(Obtusata_dat) <- c("Specimen_ID","Species","Location","Site","Date","Total_Length",
                            "Standard_Length","Weight","Gonad_Weight")

######Calculating LWR#####

#remove NAs from weight and total length
Obtusata_sub <- subset(Obtusata_dat, !is.na(Weight) & !is.na(Total_Length))

#log-transform L and W
Obtusata_sub$logL <- log(Obtusata_sub$Total_Length)
Obtusata_sub$logW <- log(Obtusata_sub$Weight)

#create linear model with log-transformed W and L
lm_lLlW <- lm(logW~logL, data = Obtusata_sub)
lm_lLlW
summary(Obtusata_lm_lLlW)

#plot model
Obtusata_logLW_plot <- ggplot(data = Obtusata_sub, aes(x = logL, y = logW)) +
  geom_point() + geom_smooth(method = "lm")
Obtusata_LW_plot <- ggplot(data = Obtusata_sub, aes(x = Total_Length, y = Weight)) +
  geom_point()
Obtusata_logLW_plot
Obtusata_LW_plot

#####Calculating GSI#####

#remove NAs from gonad weight
Obtusata_GSI_sub <- subset(Obtusata_dat, !is.na(Gonad_Weight))

#GSI
Obtusata_GSI_sub$GSI <- (Obtusata_GSI_sub$Gonad_Weight/Obtusata_GSI_sub$Weight) * 100
