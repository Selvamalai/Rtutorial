setwd("C:/Users/Selvamalai/Desktop/UEA Materials/Econometrics/Lab/Homework Week 2-Lab10")
require(data.table)
require(ggplot2)
require(stargazer)
load("dt_wages.RData")
head(dt.wages)
summary(dt.wages)
plot1 <- ggplot( data = dt.wages, aes(x = wage))
plot1 + geom_density() + facet_wrap( ~ south ) + theme_bw()


dt.bef.aftt<-data.table(dt.wages)
head(dt.bef.aftt)
## Graphical representation
qplot( data = dt.wages, x = south, y = wage
       , fill = factor(south)
       , geom = "boxplot") + theme_bw() + xlab("moment") + ylab("Wages")

## to find the difference in mean wage

dt.bef.aftt<-data.table(dt.wages) # 
dt.bef.aftt<-dt.bef.aftt[,list(mean_wage=mean(wage,na.rm=TRUE)),by=list(south)]# to find the mean wages for each group
dt.bef.aftt

# testing mean difference using clean data
dt.bef.aftt.clean<-dt.wages[!is.na(wage),]


t.test(dt.bef.aftt.clean[south==0,wage],dt.bef.aftt.clean[south==1,wage])

# Mean difference

6.176991-5.386898

### part b of the question 2

## Effect on wage when race and gender are controlled

dt.bef.aftt.clean<-dt.wages[!is.na(wage),]

lm1 <- lm(wage ~ south, data = dt.bef.aftt.clean)
stargazer(lm1,type = "text")

lm2 <- lm(wage ~ south+nonwhite+female, data = dt.bef.aftt.clean)

stargazer(lm2,type = "text")


## c. part of question 2

lm3<-lm(wage ~ south+nonwhite+ female+ south*nonwhite+ south*female,data = dt.bef.aftt.clean)
stargazer(lm3, type="text")

lm4<-lm(wage ~ nonwhite,data = dt.bef.aftt.clean)
stargazer(lm4, type="text")


## part d of question 2 
## Iv regression

install.packages("AER")
library(AER)
ivreg1<-ivreg(wage~south+female|female+nonwhite, data=dt.bef.aftt.clean)
stargazer(ivreg1,type = "text")


# adding a control variable experience
lm5<-lm(wage~south+female+nonwhite+south*nonwhite+south*female+south*exper, data=dt.bef.aftt.clean)
stargazer(lm5,type = "text")

 ## Covariance & summary statistics

dt.1<-dt.bef.aftt.clean[south==0,wage]
dt.2<-dt.bef.aftt.clean[south==1,wage]

summary(dt.1)
summary(dt.2)


dt.south.0<-dt.bef.aftt.clean[south==0]
dt.south.1<-dt.bef.aftt.clean[south==1]

summary(dt.south.0)
summary(dt.south.1)


#equal variance
l
var.lm<-lm(dt.wages$wage~dt.wages$south)
stargazer(var.lm, type = "text")
anova(var.lm)

##two-sample t-test
t.test(dt.bef.aftt.clean$wage~dt.bef.aftt.clean$south,var.equal=FALSE,paired=FALSE)
