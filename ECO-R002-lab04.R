setwd("C:/Users/Selvamalai/Desktop/UEA Materials/Econometrics/Lab")


## Load packages

install.packages("data.table")
library(data.table)

install.packages("ggplot2")
library(ggplot2)

install.packages("stargazer")

library(stargazer)

## Load data

load("affairs.RData")
dt.affairs<-data.table(data)
rm(data)


## Summary statistics
stargazer(dt.affairs,type = "text")


## Two-sided hypothesis test

dt.affairs[,religious:=relig>3]

# Run t.test

dt.affairs[,.N,by=religious]
dt.affairs[,t.test(naffairs~religious)]


# One-sided test

dt.affairs[,t.test(affair~religious,alternative=c("greater"))]

dt.affairs[,t.test(naffairs~religious,alternative=c("greater"))]

## Multiple Regression Model

# Loading a data

dt.mktg<-data.table(read.csv("DirectMarketing.csv"))
dt.mktg<-setnames(dt.mktg,tolower(names(dt.mktg)))

# Get to know the data

nrow(dt.mktg)
colnames(dt.mktg)
head(dt.mktg)

summary(dt.mktg)
stargazer(dt.mktg,type = "text")


# Explore the data graphically

qplot(data = dt.mktg
      ,x=age
      ,geom="bar")+theme_bw()+xlim("Young","Middle","Old")

qplot(data = dt.mktg
      ,x=gender
      ,geom="bar")+theme_bw()


qplot(data = dt.mktg
      ,x=ownhome
      ,geom="bar")+theme_bw()

qplot(data = dt.mktg
      ,x=married
      ,geom="bar")+theme_bw()

qplot(data = dt.mktg
      ,x=factor(children)
      ,geom="bar")+theme_bw()

qplot(data = dt.mktg
      ,x=history
      ,geom="bar")+theme_bw()

qplot(data = dt.mktg
      ,x=factor(catalogs)
      ,geom="bar")+theme_bw()

qplot(data = dt.mktg
      ,x=salary
      ,geom="bar")+theme_bw()

qplot(data = dt.mktg
      ,x=salary
      ,geom="histogram")+theme_bw()

qplot(data = dt.mktg
      ,x=salary
      ,geom="density")+theme_bw()

qplot( data = dt.mktg
       , x = amountspent
       , geom = "histogram") + theme_bw()

qplot( data = dt.mktg
       , x = amountspent
       , geom = "density") + theme_bw()

qplot( data = dt.mktg
       , x = factor(age)
       ,y=amountspent
       , geom = "boxplot") + theme_bw()+xlim("Young","Middle","Old")


qplot( data = dt.mktg
       , x = factor(ownhome)
       ,y=amountspent
       , geom = "boxplot") + theme_bw()

qplot( data = dt.mktg
       , x = factor(married)
       ,y=amountspent
       , geom = "boxplot") + theme_bw()

qplot( data = dt.mktg
       , x = factor(location)
       ,y=amountspent
       , geom = "boxplot") + theme_bw()

qplot( data = dt.mktg
       , x = factor(children)
       ,y=amountspent
       , geom = "boxplot") + theme_bw()

qplot( data = dt.mktg
       , x = factor(catalogs)
       ,y=amountspent
       , geom = "boxplot") + theme_bw()

## Simple Regression- Interpretation

lm1 <- lm(amountspent ~ salary, data = dt.mktg)
stargazer(lm1, type = "text")

lm1 <- lm(amountspent ~ location, data = dt.mktg)
stargazer(lm1, type = "text")

dt.mktg[location=="Close", mean(amountspent)]

dt.mktg[location=="Far", mean(amountspent)]

lm3 <- lm(amountspent ~ history, data = dt.mktg)
stargazer(lm3,type = "text")
dt.mktg[history=="High", mean(amountspent)]

dt.mktg[history=="Low", mean(amountspent)]

dt.mktg[history=="Medium", mean(amountspent)]

## Multiple Regression Model

lm.spend1 <- lm( amountspent ~ gender + location + salary + children + catalogs
                 , data = dt.mktg)

stargazer(lm.spend1 , type = "text")

lm.spend2 <- lm(amountspent ~ ., data = dt.mktg)

stargazer(lm.spend1, lm.spend2 , type = "text")

# Predict amount spent by new customer

new.client <- data.table( gender = "Male"
                          , location = "Close"
                          , salary = 53700
                          , children = 1
                          , catalogs = 12)

new.client
my.pred <- predict(lm.spend1, newdata = new.client)
my.pred

my.pred <- predict(lm.spend1, newdata = new.client, interval="prediction", level = .95)
my.pred
