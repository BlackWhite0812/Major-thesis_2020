#########################################################################
# Correlation Analysis: Linear Mixed-effect model and stepwise feature selection to find the 
  # relationship between the LULC area change and social-economic factors
# Yuting Zou
# March 2020
#########################################################################


########################## Part1: Set the script environment and workspace ##############################
#Install and Load the necessary libraries 
library(nlme)
library(MASS)
library(GGally)
library(Hmisc)
library(forecast)
# Set workspace
setwd('D:/college/Thesis/R code/Data/CorrelationAnalysis')

################ Part2: Load data from file and divide it into training and test dataset ################
# Load data from data file and set x,y variables
LMMData <- na.omit(read.csv('LMM.csv', header = T))

############################## Part3: Check the colinearity of the data #############################
# pairs(y7~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11,data = LMMData, main='Scatterplot matrix for impervious surface area 
#       and all variables',col=c('red','blue'),pch=c(1,4))
# ggcorr(LMMData[4:14],
#        label = TRUE,
#        label_alpha = TRUE,
#        name = 'correlation')
#################### Part4: Explore the relationship between explanatory variable ####################
############### and predictor (area of agriculture, wetland and impervious surface)###################
########################################Agriculture land (y1)
lmeAG <- lme(y1~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, random= ~ 1|city, data=LMMData)
summary(lmeAG)
# Check the autocorrelation for variables
xreg.area <- as.matrix(LMMData[,4:14])
Arima.area <- auto.arima(LMMData$y1,xreg=xreg.area,allowmean=FALSE,, allowdrift=TRUE)
acf(residuals(lmeAG))
acf(residuals(lmeAG),type = 'partial')
lmeAG2 <- lme(y1 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, random= ~ 1|city, correlation = corARMA(form = ~ year | city, q=0, p=1),
            data=LMMData, method = 'ML')
resultAG = stepAIC(lmeAG2,direction = 'both')
summary(resultAG)
########################################Wetland (y5)
lmeWE <- lme(y5~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, random= ~ 1|city, data=LMMData)
summary(lmeWE)
# Check the autocorrelation for variables
xreg.area <- as.matrix(LMMData[,4:14])
Arima.area <- auto.arima(LMMData$y5,xreg=xreg.area,allowmean=FALSE,, allowdrift=TRUE)
acf(residuals(lmeWE))
acf(residuals(lmeWE),type = 'partial')
lmeWE2 <- lme(y5 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, random= ~ 1|city,
              data=LMMData, method = 'ML')
resultWE = stepAIC(lmeWE2,direction = 'both',trace = TRUE)
summary(resultWE)
########################################Impervious surface (y7)
lmeIP <- lme(y7~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, random= ~ 1|city, data=LMMData, method = 'ML')
summary(lmeIP)
xreg.area <- as.matrix(LMMData[,4:14])
Arima.IParea <- auto.arima(LMMData$y7,xreg=xreg.area,allowmean=FALSE,, allowdrift=TRUE)
acf(residuals(lmeIP))
acf(residuals(lmeIP),type = 'partial')
lmeIP2 <- lme(y7~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, random= ~ 1|city, correlation = corARMA(form = ~ year|city, q=0, p=1),
            data=LMMData, method = 'ML')
resultIP = stepAIC(lmeIP2,direction = 'both')
summary(resultIP)
########################################Residential area (y9)
lmeRe <- lme(y9~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, random= ~ 1|city, data=LMMData, method = 'ML')
summary(lmeRe)
xreg.area <- as.matrix(LMMData[,4:14])
Arima.REarea <- auto.arima(LMMData$y9,xreg=xreg.area,allowmean=FALSE,, allowdrift=TRUE)
Arima.REarea
acf(residuals(lmeRe))
acf(residuals(lmeRe),type = 'partial')
ctr <- lmeControl(msVerbose = TRUE)
lmeRE2 <- lme(y9~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, random= ~ 1|city, correlation = corARMA(form = ~ year|city, q=0, p=1),
            data=LMMData, method = 'ML')
resultRE = stepAIC(lmeRE2,direction = 'both')
summary(resultRE)
########################################Industrial area (y10)
lmeIN <- lme(y10~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, random= ~ 1|city, data=LMMData, method = 'ML')
summary(lmeIN)
xreg.area <- as.matrix(LMMData[,4:14])
Arima.INarea <- auto.arima(LMMData$y10,xreg=xreg.area,allowmean=FALSE,, allowdrift=TRUE)
Arima.INarea
acf(residuals(lmeIN))
acf(residuals(lmeIN),type = 'partial')
lmeIN2 <- lme(y10~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, random= ~ 1|city, correlation = corARMA(form = ~ year|city, q=0, p=1),
              data=LMMData, method = 'ML')
resultIN = stepAIC(lmeIN2,direction = 'both')
summary(resultIN)
########################################Others (y11)
lmeOT <- lme(y11~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, random= ~ 1|city, data=LMMData, method = 'ML')
summary(lmeOT)
xreg.area <- as.matrix(LMMData[,4:13])
Arima.OTarea <- auto.arima(LMMData$y11,xreg=xreg.area,allowmean=FALSE,, allowdrift=TRUE)
Arima.OTarea
acf(residuals(lmeOT))
acf(residuals(lmeOT),type = 'partial')
lmeOT2 <- lme(y11~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, random= ~ 1|city,correlation = corARMA(form = ~ year|city, q=0, p=10),
              data=LMMData, method = 'ML')
resultOT = stepAIC(lmeOT2,direction = 'both')
summary(resultOT)
