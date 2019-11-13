######Setup#####
setwd("/Users/admir/Downloads/S.obtusata_Project/Sphyraena_obtusata/")
Packages <- c("tidyverse")
invisible(suppressPackageStartupMessages(lapply(Packages,library,character.only = TRUE)))

######Load files needed#####
Obtusata <- read.csv("AC_Sphyraena_obtusata_Dat_for_R.csv")

colnames(Obtusata) <- c("Specimen_ID","Species","Location","Site","Date","Total_Length",
                            "Standard_Length","Weight","Gonad_Weight")

######Calculating LWR#####

#remove NAs from weight and total length
Obtusata_LWR <- subset(Obtusata, !is.na(Weight) & !is.na(Total_Length))

#log-transform L and W
Obtusata_LWR$logL <- log(Obtusata_LWR$Total_Length)
Obtusata_LWR$logW <- log(Obtusata_LWR$Weight)

#create linear model with log-transformed W and L
full_lm_lLlW <- lm(logW~logL, data = Obtusata_LWR)
full_lm_lLlW
summary(full_lm_lLlW)

#plot model
Obtusata_logLW_plot <- ggplot(data = Obtusata_LWR, aes(x = logL, y = logW, colour = Location)) +
  geom_point() + geom_smooth(method = "lm") + ggtitle(label = "logLWR of S.obtusata from 2 populations")
Obtusata_LW_plot <- ggplot(data = Obtusata_LWR, aes(x = Total_Length, y = Weight, colour = Location)) +
  geom_point() + geom_smooth(method = "nls", formula = y ~ a*x^b, start = list(a = -6.16923, b = 3.24846), data = Obtusata_LWR, se = F) +
  ggtitle(label = "LWR of S.obtusata from 2 populations")

Obtusata_logLW_plot
Obtusata_LW_plot

#####Calculating GSI#####

#remove NAs from gonad weight
Obtusata_GSI <- subset(Obtusata, !is.na(Gonad_Weight))

#raw GSI and TL
Obtusata_GSI$GSI <- (Obtusata_GSI$Gonad_Weight/Obtusata_GSI$Weight) * 100
ggplot(data = Obtusata_GSI, aes(x = Total_Length, y = GSI, colour = Location)) + 
  geom_point() + ggtitle(label = "Relationship between GSI and total length", subtitle = "From 2 populations")

mean(Obtusata_GSI[["GSI"]])

Obtusata_GSI$logGSI <- log(Obtusata_GSI$GSI)
##ggplot(data = Obtusata_full_GSI, aes(x = Standard_Length, y = logGSI, colour = Location)) + geom_point()

#####GLMs#####

GSI_TL_glm <- glm(Obtusata_GSI$GSI~Obtusata_GSI$Total_Length, family = "Gamma")
GSI_Weight_glm <- glm(Obtusata_GSI$GSI~Obtusata_GSI$Weight, family = "Gamma")

AIC(GSI_TL_glm)
AIC(GSI_Weight_glm)

#####Boxplot#####

boxplot(GSI~Location, data=Obtusata_GSI, main="GSI by location", 
        xlab="Location", ylab="GSI", col=(c("red","blue")))
boxplot(Total_Length~Location, data=Obtusata_GSI, main="Total length by location", 
        xlab="Location", ylab="Total Length", col=(c("red","blue")))
boxplot(Weight~Location, data=Obtusata_GSI, main="Weight by location", 
        xlab="Location", ylab="Weight", col=(c("red","blue")))

#####Normality test#####

#NORMALITY (need packages nortest, car, and HMisc)
library(car)
library(nortest)
ad.test(Obtusata_full_GSI$Weight)
ad.test(data1$log)
hist(Obtusata_full_GSI$Weight, xlab=" ", main="Histogram of Weight Full data ", ylab= " ")
hist(Obtusata_full_GSI$logGSI, xlab=" ", main="Histogram of logGSI Full data ", ylab= " ")


fit <- lm(Obtusata_full_GSI$Total_Length~Obtusata_full_GSI$Location, data=Obtusata_full_GSI)

fit <- lm(Obtusata_full_GSI$logGSI~Obtusata_full_GSI$Location, data=Obtusata_full_GSI)


qqPlot(fit, main="QQ Plot of TL")
library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals of TL")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
spreadLevelPlot(fit)
# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Data not normally distributed so used a Mann - Whitney U test (non-parametric t- test equivalent)
wilcox.test(Obtusata_full_GSI$GSI~Obtusata_full_GSI$Location)
wilcox.test(Obtusata_full_GSI$Total_Length~Obtusata_full_GSI$Location)
wilcox.test(Obtusata_full_GSI$Weight~Obtusata_full_GSI$Location)

#####L-W relationship#####
#regression model codes
#Attach/plot:

x<-c(Obtusata_dat$Total_Length)
y<-c(Obtusata_dat$Weight)
plot(log(y)~log(x))
#linear model of log-transformed data
mod1<-lm(log(y)~log(x))
coef(mod1)
exp(-5.638077)
#run power model using coefficients
plot(y~x, pch=19, col="black", xlab="Length(cm)", ylab= "Weight(cm)", main = "LWR Dumaguete")
mod1<- nls(y ~ a * x^b, start=list(a=0.003559707, b =3.059256), data=Obtusata_dat, trace=TRUE)
summary(mod1)
AIC(mod1)
x<-seq(14,40)
#plot predicted line
mod2 <-(predict(mod1, list(x=x)))
lines(mod2~x, lwd=3, col="blue")

text(20, 150, expression(W == 0.0035597 * L^3.059256))

x<-c(Obtusata_KAL$Total_Length)
y<-c(Obtusata_KAL$Weight)
plot(log(y)~log(x))
#linear model of log-transformed data
mod1<-lm(log(y)~log(x))
coef(mod1)
exp(-5.5163)
#run power model using coefficients
plot(y~x, pch=19, col="black", xlab="Length (cm)", ylab= "Weight(g)", ylim=c(0, 200), xlim=c(10, 40), main = "LWR Kalibo")
mod1<- nls(y ~ a * x^b, start=list(a=0.004027, b =3.106475), data=Obtusata_KAL, trace=TRUE)
summary(mod1)
AIC(mod1)
x<-seq(14,40)
#plot predicted line
mod2 <-(predict(mod1, list(x=x)))
lines(mod2~x, lwd=3, col="blue")

text(20, 150, expression(W == 0.004027 * L^3.106475))

     