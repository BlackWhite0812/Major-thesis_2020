#########################################################################
# Interrupted Time-series Analysis on Impervious land in bridge-connected cities
# Yuting Zou
# March 2020
#########################################################################

########################## Part1: Set the script environment and workspace ##############################
# Load library what is needed later
install.packages("car")
library(car)
library(nlme)

#Set the workspace for the data file
setwd('D:/college/Thesis/R code/data/InterruptedTime-seriesAnalysis/ITSA_LULC')

#################################### Part2: Load data from the file #####################################
# Load data from data file
dataJNHZ <- read.csv('IPJNHZV3.csv',header = T)
dataJXHZ <- read.csv('IPJXHZV3.csv',header = T)
dataNBHZ <- read.csv('IPNBHZV3.csv',header = T)

############################################# Part3: Initial Plot #######################################
# # Plot the time series for the Impervious Surface area in Jiaxing
# plot(dataJNHZ$time[1:18],dataJNHZ$IP[1:18],
#      ylab = "Impervious surface for Jiaxing and Ningbo",
#      ylim = c(0,2800),
#      xlab = 'Year',
#      type = 'l',
#      col = 'blue',
#      xaxt = 'n',
#      main = 'Area change of Impervious Surface in Jiaxing & Ningbo and Hangzhou 
#      from 2000-2017')
# 
# # Add in control group Impervious surface area in Hangzhou
# points(dataJNHZ$time[19:36],dataJNHZ$IP[19:36],
#        type = 'l',
#        col = 'red')
# 
# # Add in legend
# legend("bottomright",legend = c('Jiaxing & Ningbo','Hangzhou'),
#        col = c('blue', 'red'), pch = 20)
# 
# # Add x-axies year labels
# axis(1, at = 1:18, labels = dataJNHZ$year[1:18])
# 
# # Add individual point for the figure
# points(dataJNHZ$time[1:18],dataJNHZ$IP[1:18],
#        col = 'blue',
#        pch = 20)
# 
# points(dataJNHZ$time[19:36], dataJNHZ$IP[19:36],
#        col = 'red',
#        pch = 20)
# 
# # Lable the time when the bridge is open to traffic
# abline(v = 9.5,lty = 2)
# 
# # Add in legend
# legend("bottomright",legend = c('Jiaxing & Ningbo','Hangzhou'),
#        col = c('blue', 'red'), pch = 20)

#################################### Part 4: Preliminary Analysis ######################################
#Jiaxing & Ningbo
# Standard OLS regression model
model_ols <- lm(IP ~ time + NBJX + NBJXtime + level + trend + NBJXlevel + NBJXtrend, data = dataJNHZ)
summary(model_ols)
confint(model_ols)
# ACF plots
par(mfrow = c(1,2),ask = F)
acf(residuals(model_ols))
acf(residuals(model_ols),type = 'partial')

#Jiaxing
# Standard OLS regression model
model_ols <- lm(IP ~ time + Jiaxing + Jiaxingtime + level + trend + Jiaxinglevel + Jiaxingtrend, data = dataJXHZ)
summary(model_ols)
confint(model_ols)
# ACF plots (p=0,q=0)
par(mfrow = c(1,2),ask = F)
acf(residuals(model_ols))
acf(residuals(model_ols),type = 'partial')

#Ningbo
# Standard OLS regression model
model_ols <- lm(IP ~ time + Ningbo + Ningbotime + level + trend + Ningbolevel + Ningbotrend, data = dataNBHZ)
summary(model_ols)
confint(model_ols)
# ACF plots
par(mfrow = c(1,2),ask = F)
acf(residuals(model_ols))
acf(residuals(model_ols),type = 'partial')
############################ Part 5: Fit the GLS regression model ######################################
#Jiaxing & Ningbo
model_q1p1 = gls(IP ~ time + NBJX + NBJXtime + level + trend + NBJXlevel + NBJXtrend,  
                 data = dataJNHZ, correlation=corARMA(q=1,p=1,form=~time|NBJX), method = 'ML')
summary(model_q1p1)
confint(model_q1p1)

# Jiaxing
model_p8q1 = gls(IP ~ time + Jiaxing + Jiaxingtime + level + trend + Jiaxinglevel + Jiaxingtrend, 
                 data = dataJXHZ, correlation=corARMA(p=8,q=1, form=~time|Jiaxing), method = 'ML')
summary(model_p8q1)
confint(model_p8q1)

#Ningbo
model_p6q1 = gls(IP ~ time + Ningbo + Ningbotime + level + trend + Ningbolevel + Ningbotrend, 
                 data = dataNBHZ, correlation=corARMA(p=6,q=1,form=~time|Ningbo),method = 'ML')
summary(model_p6q1)
confint(model_p6q1)
####################################### Part 6: Diagnostic tests #########################################
# # Likelihood-ratio tests to check whether the parameters of the AR process for the errors are necessary and sufficient
model_p10q1 <- update(model_q1p1,correlation=corARMA(q=1,p=10,form=~time|NBJX))
anova(model_q1p1,model_p10q1)
AIC(model_q1p1)
AIC(model_p10q1)
####################################### Part 7: Plot results ############################################
# First plot the raw data points for the Ningbo
par(mfrow=c(1,1))
plot(dataJNHZ$time[1:18],dataJNHZ$IP[1:18],
     ylim=c(100,3000),
     ylab="Impervious surface",
     xlab="Year",
     pch=20,
     col="white",
     xaxt="n",
     main = "ITSA for impervious change in Jiaxing and Ningbo city 
     (with control group of Hangzhou)")

# Add x-axis year labels
axis(1, at=1:18, labels=dataJNHZ$year[1:18])
# Label the policy change
abline(v=9.5,lty=2)

# # Add in the points for the control and intervention groups
# points(dataJNHZ$time[19:36],dataJNHZ$IP[19:36],
#        col="pink",
#        pch=20)
# points(dataJXHZ$time[1:18],dataJXHZ$IP[1:18],
#        col="navy",
#        pch=20)
# points(dataNBHZ$time[1:18],dataNBHZ$IP[1:18],
#        col="orange",
#        pch=20)

# Plot the first line segment for the intervention groups
lines(dataJNHZ$time[1:9], fitted(model_p10q1)[1:9], col="blue",lwd=2)
lines(dataJXHZ$time[1:9], fitted(model_p8q1)[1:9], col="navy",lwd=2)
lines(dataNBHZ$time[1:9], fitted(model_p6q1)[1:9], col="orange",lwd=2)

# Add the second line segment for the intervention groups
lines(dataJNHZ$time[10:18], fitted(model_p10q1)[10:18], col="blue",lwd=2)
lines(dataJXHZ$time[10:18], fitted(model_p8q1)[10:18], col="navy",lwd=2)
lines(dataNBHZ$time[10:18], fitted(model_p6q1)[10:18], col="orange",lwd=2)

# Add the counterfactual for the intervention groups
segments(10, model_p10q1$coef[1] + model_p10q1$coef[2]*10 + model_p10q1$coef[3]+model_p10q1$coef[4]*10 + 
           model_p10q1$coef[5] + model_p10q1$coef[6],
         18, model_p10q1$coef[1] + model_p10q1$coef[2]*18 + model_p10q1$coef[3]+model_p10q1$coef[4]*18 + 
           model_p10q1$coef[5] + model_p10q1$coef[6]*9,
         lty=2,col='blue',lwd=2)
segments(10, model_p8q1$coef[1] + model_p8q1$coef[2]*10 + model_p8q1$coef[3]+model_p8q1$coef[4]*10 + 
           model_p8q1$coef[5] + model_p8q1$coef[6],
         18, model_p8q1$coef[1] + model_p8q1$coef[2]*18 + model_p8q1$coef[3]+model_p8q1$coef[4]*18 + 
           model_p8q1$coef[5] + model_p8q1$coef[6]*9,
         lty=2,col='navy',lwd=2)
segments(10, model_p6q1$coef[1] + model_p6q1$coef[2]*10 + model_p6q1$coef[3]+model_p6q1$coef[4]*10 + 
           model_p6q1$coef[5] + model_p6q1$coef[6],
         18, model_p6q1$coef[1] + model_p6q1$coef[2]*18 + model_p6q1$coef[3]+model_p6q1$coef[4]*18 + 
           model_p6q1$coef[5] + model_p6q1$coef[6]*9,
         lty=2,col='orange',lwd=2)

# Plot the first line segment for the control group
lines(dataJNHZ$time[19:27], fitted(model_p10q1)[19:27], col="red",lwd=2)

# Add the second line segment for the control
lines(dataJNHZ$time[28:36], fitted(model_p10q1)[28:36], col="red",lwd=2)

# Add the counterfactual for the control group
segments(1, model_p10q1$coef[1]+model_p10q1$coef[2],
         18,model_p10q1$coef[1]+model_p10q1$coef[2]*18,
         lty=2,col='red',lwd=2)

# Add in a legend
legend("topleft", legend=c("Jiaxing & Ningbo","Jiaxing","Ningbo","Hangzhou"), col=c("blue","navy","orange","red"),lwd=2)
