######Setup#####
setwd("/Users/admir/Downloads/S.obtusata_Project")
Packages <- c("tidyverse")
invisible(suppressPackageStartupMessages(lapply(Packages,library,character.only = TRUE)))

######Load files needed#####
Obtusata_1 <- read.csv("AC_Sphyraena_obtusata_Dat_for_R.csv")
Obtusata_2 <- read.csv("Sphyraena_obtusata_KAL.csv")
colnames(Obtusata_1) <- c("Specimen_ID","Species","Location","Site","Date","Total_Length",
                            "Standard_Length","Weight","Gonad_Weight")
colnames(Obtusata_2) <- c("Specimen_ID","Species","Location","Site","Date","Total_Length",
                          "Standard_Length","Weight","Gonad_Weight")

#####Join Dumaguete and Kalibo dataframes#####

#using merge
Obtusata_full <- merge(Obtusata_1, Obtusata_2, by = c("Specimen_ID", "Species", "Location", "Site", "Date",
                                                      "Total_Length", "Standard_Length", "Weight", "Gonad_Weight"), all = TRUE)

######Calculating LWR#####

#remove NAs from weight and total length
Obtusata_full_LWR <- subset(Obtusata_full, !is.na(Weight) & !is.na(Total_Length))

#log-transform L and W
Obtusata_full_LWR$logL <- log(Obtusata_full_LWR$Total_Length)
Obtusata_full_LWR$logW <- log(Obtusata_full_LWR$Weight)

#create linear model with log-transformed W and L
full_lm_lLlW <- lm(logW~logL, data = Obtusata_full_LWR)
full_lm_lLlW
summary(full_lm_lLlW)

#plot model
Obtusata_full_logLW_plot <- ggplot(data = Obtusata_full_LWR, aes(x = logL, y = logW, colour = Location)) +
  geom_point() + geom_smooth(method = "lm")
Obtusata_full_LW_plot <- ggplot(data = Obtusata_full_LWR, aes(x = Total_Length, y = Weight, colour = Location)) +
  geom_point()
Obtusata_full_logLW_plot
Obtusata_full_LW_plot

#####Calculating GSI#####

#remove NAs from gonad weight
Obtusata_full_GSI <- subset(Obtusata_full, !is.na(Gonad_Weight))

#GSI
Obtusata_full_GSI$GSI <- (Obtusata_full_GSI$Gonad_Weight/Obtusata_full_GSI$Weight) * 100
ggplot(data = Obtusata_full_GSI, aes(x = Standard_Length, y = GSI, colour = Location)) + 
  geom_point()

mean(Obtusata_full_GSI[["GSI"]])

Obtusata_full_GSI$logGSI <- log(Obtusata_full_GSI$GSI)
ggplot(data = Obtusata_full_GSI, aes(x = Standard_Length, y = logGSI, colour = Location)) +
  geom_point()