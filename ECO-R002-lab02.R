install.packages("ggplot2")
library(ggplot2)
install.packages("stargazer")
library(stargazer)
load("ceosal2.RData")

##Data Analysis##

dt.ceo.salaries<-data.table(data)
library(data.table)
dt.ceo.salaries<-data.table(data)
rm(data)

##Descriptive Statistics##

# How many CEOS are in the sample?

nrow(dt.ceo.salaries)
dt.ceo.salaries[,sum(grad)]

nrow(dt.ceo.salaries[grad==1,1])

## What is the percentage of CEOs with graduate degrees?

dt.ceo.salaries[,sum(grad)]/nrow(dt.ceo.salaries)

dt.ceo.salaries[,mean(salary)]  ## mean salary

mean(dt.ceo.salaries[,salary]) ## Alternative way

dt.ceo.salaries[grad==0,mean(salary)]  ## Mean salary for those without a graduate degree

dt.ceo.salaries[,list(n_ceo=.N),by=college]

##t-test

t.test(dt.ceo.salaries[,salary],mu=800) #salary mean is statistically different from 800?

t.test(dt.ceo.salaries[,salary]~dt.ceo.salaries[,grad])

 ## Tables

dt.ceo.salaries[,list(mean_salary=mean(salary),
                      sd_salary=sd(salary)
                      ,min_salary=min(salary)
                      ,max_salary=median(salary))]
## Summary statistics

dt.ceo.salaries[,list(mean_salary=mean(salary)
                      ,sd_salary=sd(salary)
                      ,min_salary=min(salary)
                      ,max_salary=max(salary)),by=list(grad,college)]
stargazer(dt.ceo.salaries,type = "text")


stargazer(dt.ceo.salaries[grad==1,list(age,salary)],type="text")


##Quick Plots##

#Histogram#

qplot(data=dt.ceo.salaries
      ,x=salary
      ,geom = "histogram")


#Age

qplot(data=dt.ceo.salaries
      ,x=age
      ,geom = "histogram")

##Scatterplot

qplot(data=dt.ceo.salaries
      ,x=sales
      ,y=profits
      ,geom = "point")

## Barplot

qplot(data=dt.ceo.salaries
      ,x=factor(grad)
      ,geom="bar")


## Line

qplot(data=dt.ceo.salaries
      ,x=sales
      ,y=profits
      ,geom="line")

##Facet Wrap

library(ggplot2)
qplot(data = dt.ceo.salaries
            ,x=salary
            ,geom = "histogram")+facet_wrap(~grad)
library(ggplot2)


#Customizing Plots


qplot(data=dt.ceo.salaries
      ,x=salary
      ,geom = "histogram"
      ,fill=factor(grad,levels = c(0,1),labels = c("Yes","No")))+
  theme_bw()+
  ylim(0,50)+
  xlim(0,4000)+
  labs(title = "MY PLOT",x="CEO Salary", y="Numberof CEOs",fill="Grad.Degree")
