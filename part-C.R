getwd()
rm()
setwd("C:/Users/Selvamalai/Desktop/UEA Materials/Econometrics/Lab/Exercise2")
load("dt_wages.RData")
require(data.table)

dt.wages<-data.table(data)

head(dt.wages)

## generate confidencet interval
dt.wages[,list(avg_wage=mean(wage),as_wage=sd(wage))]


## hypothesis testing

dt.wages[female=1,t.test(wage,mu=5)]

dt.wages[,t.test(wage-female)]