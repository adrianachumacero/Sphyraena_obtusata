######Setup#####
setwd("/Users/admir/Downloads/S.obtusata_Project")
Packages <- c("tidyverse","xlsx")
invisible(suppressPackageStartupMessages(lapply(Packages,library,character.only = TRUE)))


######Load files needed#####
Obtusata_dat <- read.csv("AC_Sphyraena_obtusata_Dat_for_R.csv")
colnames(Obtusata_dat) <- c("Specimen_ID","Species","Location","Site","Date","Total_Length",
                            "Standard_Length","Weight","Gonad_Weight")

######Calculating LWR#####

#remove NAs from weight and total length
Obtusata_LWR <- subset(Obtusata_dat, !is.na(Weight) & !is.na(Total_Length))

#log-transform L and W
Obtusata_LWR$logL <- log(Obtusata_LWR$Total_Length)
Obtusata_LWR$logW <- log(Obtusata_LWR$Weight)

#create linear model with log-transformed W and L
lm_lLlW <- lm(logW~logL, data = Obtusata_LWR)
lm_lLlW
summary(lm_lLlW)

#plot model
Obtusata_logLW_plot <- ggplot(data = Obtusata_LWR, aes(x = logL, y = logW)) +
  geom_point() + geom_smooth(method = "lm")
Obtusata_LW_plot <- ggplot(data = Obtusata_LWR, aes(x = Total_Length, y = Weight)) +
  geom_point()
Obtusata_logLW_plot
Obtusata_LW_plot

#another plot but with axes names
coef(lm_lLlW)
exp(-5.630074)

plot(Obtusata_LWR$logW~Obtusata_LWR$logL, pch=19, col="black", xlab="logLength (cm)", ylab= "logWeight (g)")
plot(Obtusata_LWR$Weight~Obtusata_LWR$Total_Length, pch=19, col="black", xlab="Length (cm)", ylab= "Weight (g)")

#checking distribution of LWR data
Length_dist <- hist(Obtusata_LWR$Total_Length)
Weight_dist <- hist(Obtusata_LWR$Weight)

#####Calculating GSI#####

#remove NAs from gonad weight
Obtusata_GSI <- subset(Obtusata_dat, !is.na(Gonad_Weight))

#GSI
Obtusata_GSI$GSI <- (Obtusata_GSI$Gonad_Weight/(Obtusata_GSI$Weight - Obtusata_GSI$Gonad_Weight)) * 100
ggplot(data = Obtusata_GSI, aes(x = Standard_Length, y = GSI)) + 
  geom_point()

Obtusata_GSI$GSI_2 <- (Obtusata_GSI$Gonad_Weight/Obtusata_GSI$Weight) * 100
ggplot(data = Obtusata_GSI, aes(x = Standard_Length, y = GSI_2)) +
  geom_point()

mean(Obtusata_GSI[["GSI"]])
mean(Obtusata_GSI[["GSI_2"]])

Obtusata_GSI$logGSI <- log(Obtusata_GSI$GSI)
ggplot(data = Obtusata_GSI, aes(x = Standard_Length, y = logGSI)) +
  geom_point()

Obtusata_GSI$logGSI_2 <- log(Obtusata_GSI$GSI_2)
ggplot(data = Obtusata_GSI, aes(x = Standard_Length, y = logGSI_2)) +
  geom_point()

#####Compare relationship between logGSI and total length/weight#####

#logGSI and TL
ggplot(data = Obtusata_GSI, aes(x = Total_Length, y = logGSI)) +
  geom_point()

ggplot(data = Obtusata_GSI, aes(x = Total_Length, y = logGSI_2)) +
  geom_point()

#logGSI and Weight
ggplot(data = Obtusata_GSI, aes(x = Weight, y = logGSI)) +
  geom_point()

ggplot(data = Obtusata_GSI, aes(x = Weight, y = logGSI_2)) +
  geom_point()

#####GLM comparison#####

#checking distribution of GSI data
GSI_dist <- hist(Obtusata_GSI$GSI)
GSI_dist <- hist(Obtusata_GSI$GSI_2)

#log transform TL, SL, and Weight
Obtusata_GSI$logTL <- log(Obtusata_GSI$Total_Length)
Obtusata_GSI$logSL <- log(Obtusata_GSI$Standard_Length)
Obtusata_GSI$logWeight <- log(Obtusata_GSI$Weight)


#making GLMs
GSI_SL_glm <- glm(Obtusata_GSI$GSI~Obtusata_GSI$Standard_Length, family = Gamma(link = "log"))
GSI_TL_glm <- glm(Obtusata_GSI$GSI~Obtusata_GSI$Total_Length, family = Gamma(link = "log"))
GSI_Weight_glm <- glm(Obtusata_GSI$GSI~Obtusata_GSI$Weight, family = Gamma(link = "log"))

GSI2_SL_glm <- glm(Obtusata_GSI$GSI_2~Obtusata_GSI$Standard_Length, family = Gamma(link = "log"))
GSI2_TL_glm <- glm(Obtusata_GSI$GSI_2~Obtusata_GSI$Total_Length, family = Gamma(link = "log"))
GSI2_Weight_glm <- glm(Obtusata_GSI$GSI_2~Obtusata_GSI$Weight, family = Gamma(link = "log"))


#check significance of each model
AIC(GSI_SL_glm)
AIC(GSI_TL_glm) #most significant model
AIC(GSI_Weight_glm)

AIC(GSI2_SL_glm)
AIC(GSI2_TL_glm) #still most significant model
AIC(GSI2_Weight_glm)

#####Exporting to Excel#####
write.xlsx(Obtusata_GSI, file = "Sphyraena_obtusata.xlsx", sheetName = "Data", col.names = T, row.names = T, 
           append = F)
