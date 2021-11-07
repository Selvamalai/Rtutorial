## Set the file directions

setwd("C:/Users/Selvamalai/Desktop/UEA Materials/Econometrics/Lab/Lab 11 a Nov 7th-Panel")

# Installing and loading required Packages

install.packages("tidyverse")
library(tidyverse)
install.packages("gplots")
library(gplots)
install.packages("tseries")
library(tseries)
install.packages("plm")
library(plm)
install.packages("car")
library(car)
install.packages("lmtest")
install.packages("zoo")
library(zoo)
library(lmtest)
library(plm)

#Data Import and Tidying
library(readxl)

dataPanel101 <- read_excel("Panel_Excercise_Web_Data.xlsx")

dataPanel101 <- plm.data(dataPanel101, index=c("country","year"))
dataPanel101 <- pdata.frame(dataPanel101, index=c("country","year"))


# view tabular data
dataPanel101

# Exploratory Data Analysis

coplot(y ~ year|country, type="b", data=dataPanel101)

library(car)   ## to get the scatterplot

scatterplot(y~year|country, data=dataPanel101)
library(ggplot2)
library(plm)

require(carData)
require(car)
require(plm)
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)

??plotmeans  # when I could not able to find plotmeans
library(gplots)
plotmeans(y ~ country, data = dataPanel101)

#Heterogeneity across years
plotmeans(y ~ year, data = dataPanel101)


### Panel Data Modeling

## Basic OLS model

#The basic OLS regression model does not consider heterogeneity across countries or across years

ols <-lm(y ~ x1, data = dataPanel101)
summary(ols)
library(stargazer)
stargazer(ols, type="text")

# FItted values

yhat <- ols$fitted

ggplot(data=dataPanel101, aes(x=x1,y=y))+geom_point() 

??geom_smooth

# I was unable incorporate geom_smooth without any error


## Fixed Effects Model

# Country-Specific Fixed Effects using Dummy Variables (LSDV Model)

fixed.dum <-lm(y ~ x1 + factor(country) - 1, data = dataPanel101)
summary(fixed.dum)
stargazer(fixed.dum, type = "text")


#Fit
yhat <- fixed.dum$fitted
scatterplot(yhat ~ dataPanel101$x1 | dataPanel101$country,  xlab ="x1", ylab ="yhat", boxplots = FALSE,smooth = FALSE)
abline(lm(dataPanel101$y~dataPanel101$x1),lwd=3, col="red")

# OLS vs LSDV
#Each component of the factor variable (country) is absorbing the effects particular to each country. Predictor x1 was not significant in the OLS model, once controlling for differences across countries, x1 became significant in the OLS_DUM (i.e. LSDV model)

# Country-Specific Fixed Effects using the plm package

fixed <- plm(y ~ x1, data=dataPanel101, model="within")
summary(fixed)
stargazer(fixed, type = "text")

# Display the fixed effects (constants for each country)

fixef(fixed)

##The coeff of x1 indicates how much Y changes overtime, on average per country, when X increases by one unit.

# Fixed Effects vs OLS
# (Testing for fixed effects, null: OLS better than fixed)
pFtest(fixed, ols)


##If the p-value is < 0.05 then the fixed effects model is a better choice

####4.3 Random Effects Model


random <- plm(y ~ x1, data=dataPanel101, model="random")
summary(random)
stargazer(random,type = "text")


# Fixed vs Random
# Decide which model is better using a Hausman test where the null hypothesis is that the preferred model is random effects vs. the alternative the fixed effects (see Green, 2008, chapter 9). It basically tests whether the unique errors are correlated with the regressors, the null hypothesis is they are not. If the p-value is significant (for example <0.05) then use fixed effects, if not use random effects.

phtest(fixed, random)

#=> We should use the random effects model



#######Regression Diagnostics



## Time-fixed effects testing
fixed.time <- plm(y ~ x1 + factor(year), data=dataPanel101, model="within")
summary(fixed.time)
stargazer(fixed.time, type = "text")

# Testing time-fixed effects. The null is that no time-fixed effects are needed

pFtest(fixed.time, fixed)
plmtest(fixed, c("time"), type=("bp"))

#=If the p value < 0.05 then use time-fixed effects. In this example, no need to use time-fixed effects.


## Serial correlation testing


pbgtest(fixed)

#Because p-value > 0.05, we conclude that there is NO serial correlation


###Unit roots/stationarity testing

#The Dickey-Fuller test to check for stochastic trends.

#H0) The null hypothesis is that the series has a unit root (i.e. non-stationary)
#If unit root is present you can take the first difference of the variable.

library(tseries) ### test use adf test

adf.test(dataPanel101$y, k=2)

#Because p-value < 0.05, we conclude that the series does NOT have unit root. In other words, the series is stationary


# Heteroskedasticity testing
#H0) The null hypothesis for the Breusch-Pagan test is homoskedasticity

bptest(y ~ x1 + factor(country), data = dataPanel101, studentize=F)

#Because p-value < 0.05, we detect hetersokedasticity

# => If hetersokedasticity is detected we need to use a robust covariance matrix (Sandwich estimator) to account for it

# Controlling for heteroskedasticity: Random effects

#The -vcovHC- function estimates three heteroskedasticity-consistent covariance estimators:

# "white1" - for general heteroskedasticity but no serial correlation. Recommended for random effects.
# "white2" - is "white1" restricted to a common variance within groups. Recommended for random effects.
# arellano" - both heteroskedasticity and serial correlation. Recommended for fixed effects.

# Original coefficients

coeftest(random) 
# Heteroskedasticity consistent coefficients
coeftest(random, vcovHC) 


## Controlling for heteroskedasticity: Fixed effects

# Original coefficients

coeftest(fixed)
# Heteroskedasticity consistent coefficients (Arellano)
coeftest(fixed, vcovHC(fixed, method = "arellano"))


