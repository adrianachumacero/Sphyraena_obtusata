######Setup#####
setwd("/Users/admir/Downloads/S.obtusata_Project")
Packages <- c("tidyverse")
invisible(suppressPackageStartupMessages(lapply(Packages,library,character.only = TRUE)))

######Load files needed#####
Obtusata_KAL <- read.csv("Sphyraena_obtusata_KAL.csv")
colnames(Obtusata_KAL) <- c("Specimen_ID","Species","Location","Site","Date","Total_Length",
                            "Standard_Length","Weight","Gonad_Weight")

######Calculating LWR#####

#remove NAs from weight and total length
Obtusata_KAL_LWR <- subset(Obtusata_KAL, !is.na(Weight) & !is.na(Total_Length))

#log-transform L and W
Obtusata_KAL_LWR$logL <- log(Obtusata_KAL$Total_Length)
Obtusata_KAL_LWR$logW <- log(Obtusata_KAL$Weight)

#create linear model with log-transformed W and L
KAL_lm_lLlW <- lm(logW~logL, data = Obtusata_KAL_LWR)
KAL_lm_lLlW
summary(KAL_lm_lLlW)

#plot model
Obtusata_KAL_logLW_plot <- ggplot(data = Obtusata_KAL_LWR, aes(x = logL, y = logW)) +
  geom_point() + geom_smooth(method = "lm")
Obtusata_KAL_LW_plot <- ggplot(data = Obtusata_KAL_LWR, aes(x = Total_Length, y = Weight)) +
  geom_point()
Obtusata_KAL_logLW_plot
Obtusata_KAL_LW_plot

#####Calculating GSI#####

#remove NAs from gonad weight
Obtusata_KAL_GSI <- subset(Obtusata_KAL, !is.na(Gonad_Weight))

#GSI
Obtusata_KAL_GSI$GSI <- (Obtusata_KAL_GSI$Gonad_Weight/(Obtusata_KAL_GSI$Weight - Obtusata_KAL_GSI$Gonad_Weight)) * 100
ggplot(data = Obtusata_KAL_GSI, aes(x = Standard_Length, y = GSI)) + 
  geom_point()

Obtusata_KAL_GSI$GSI_2 <- (Obtusata_KAL_GSI$Gonad_Weight/Obtusata_KAL_GSI$Weight) * 100
ggplot(data = Obtusata_KAL_GSI, aes(x = Standard_Length, y = GSI_2)) + 
  geom_point()

mean(Obtusata_KAL_GSI[["GSI"]])
mean(Obtusata_KAL_GSI[["GSI_2"]])

Obtusata_KAL_GSI$logGSI <- log(Obtusata_KAL_GSI$GSI)
ggplot(data = Obtusata_KAL_GSI, aes(x = Standard_Length, y = logGSI)) +
  geom_point()

Obtusata_KAL_GSI$logGSI_2 <- log(Obtusata_KAL_GSI$GSI_2)
ggplot(data = Obtusata_KAL_GSI, aes(x = Standard_Length, y = logGSI_2)) +
  geom_point()

#####Compare relationship between logGSI and total length/weight#####

#logGSI and TL
ggplot(data = Obtusata_KAL_GSI, aes(x = Total_Length, y = logGSI)) +
  geom_point()

ggplot(data = Obtusata_KAL_GSI, aes(x = Total_Length, y = logGSI_2)) +
  geom_point()

#logGSI and Weight
ggplot(data = Obtusata_KAL_GSI, aes(x = Weight, y = logGSI)) +
  geom_point()

ggplot(data = Obtusata_KAL_GSI, aes(x = Weight, y = logGSI_2)) +
  geom_point()

#####GLM comparison#####

#checking distribution of GSI data
KAL_GSI_dist <- hist(Obtusata_KAL_GSI$GSI)
KAL_GSI_dist <- hist(Obtusata_KAL_GSI$GSI_2)

#log transform TL, SL, and Weight
Obtusata_KAL_GSI$logTL <- log(Obtusata_KAL_GSI$Total_Length)
Obtusata_KAL_GSI$logSL <- log(Obtusata_KAL_GSI$Standard_Length)
Obtusata_KAL_GSI$logWeight <- log(Obtusata_KAL_GSI$Weight)

#making GLMs
GSI_KAL_SL_glm <- glm(Obtusata_KAL_GSI$GSI~Obtusata_KAL_GSI$Standard_Length, family = Gamma)
GSI_KAL_TL_glm <- glm(Obtusata_KAL_GSI$GSI~Obtusata_KAL_GSI$Total_Length, family = Gamma)
GSI_KAL_Weight_glm <- glm(Obtusata_KAL_GSI$GSI~Obtusata_KAL_GSI$Weight, family = Gamma)

GSI2_KAL_SL_glm <- glm(Obtusata_KAL_GSI$GSI_2~Obtusata_KAL_GSI$Standard_Length, family = Gamma)
GSI2_KAL_TL_glm <- glm(Obtusata_KAL_GSI$GSI_2~Obtusata_KAL_GSI$Total_Length, family = Gamma)
GSI2_KAL_Weight_glm <- glm(Obtusata_KAL_GSI$GSI_2~Obtusata_KAL_GSI$Weight, family = Gamma)

#check significance of each model
AIC(GSI_KAL_SL_glm)
AIC(GSI_KAL_TL_glm) #most significant model
AIC(GSI_KAL_Weight_glm)

AIC(GSI2_KAL_SL_glm)
AIC(GSI2_KAL_TL_glm) #still most significant model
AIC(GSI2_KAL_Weight_glm)