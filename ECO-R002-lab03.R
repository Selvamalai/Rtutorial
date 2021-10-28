library(data.table)
library(ggplot2)
install.packages("Hmisc")
library(Hmisc)

## Loading data
getwd()
setwd("C:/Users/Selvamalai/Desktop/UEA Materials/Econometrics/Lab")
list.files()

sales<-read.csv("sales-data.csv")

dt.sales<-data.table(sales)

## Exploring the data

ncol(dt.sales)
colnames(dt.sales)
head(dt.sales)
stargazer(dt.sales,type="text")
summary(dt.sales)


## Graphical explanation 

qplot(data = dt.sales
      ,x=advertising
      ,y=sales
      ,geom="point")+theme_bw()

dt.sales[,cor(sales,advertising)]
dt.sales[,rcorr(sales,advertising)]


## Simple Linear Regression Model

lm.sales<-lm(sales~advertising, data = dt.sales)
summary(lm.sales)

stargazer(lm.sales, type="text")


coeffs=coefficients(lm.sales)
coeffs

## Plot

qplot(data = dt.sales
      ,x=advertising
      ,y=sales
      ,geom = c("point","smooth"),method=lm)+theme_bw()+labs(x="advertising", y="sales dollars")

## Predicted values

advertising=100
sales=coeffs[1]+coeffs[2]*advertising
sales


my.budget=data.table(advertising=100)
predict(lm.sales,my.budget)
predict(lm.sales,my.budget, interval = "predict")
