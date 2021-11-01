## Load package datatable

install.packages("data.table")
require(data.table)
install.packages("sandwich")
install.packages("lmtest")
install.packages("zoo")
require(sandwich)
require(lmtest)
require(zoo)
require(stargazer)

install.packages(data.table)
require(data.table)

## Violation of Simple Linear Regression Assumption2

set.seed(1984)
x1<-rnorm(n=1000, mean=0,sd=3) # create independent variable 1
x2<-rnorm(n=1000, mean=0,sd=4) # create independent variable 2
e<-rnorm(n=1000, mean=0, sd=2) # create error

y<-2+3*x1+4*x2+e  # create y cordinate to population model

## Create a dataframe named "dt.population"

dt.population<-data.table(y,x1,x2)   # create table
dt.population<-dt.population[order(y)]
dt.population


## help(sample) #when in doubt use help

r.sample.rows<-sample(1:nrow(dt.population),size = 100)
r.sample.rows # shows the vector of 100 randomly selected row numbers

r.sample<-dt.population[r.sample.rows,]
head(r.sample)

## Run regression on a random sample:

summary(lm(y~x1+x2,data=r.sample))

## Extract a non-random sample from the population:

nr.sample<-dt.population[1:100,]
head(nr.sample)


# Regression on a non-random sample:

summary(lm(y~x1+x2,data = nr.sample))

### Violation of Simple Linear Regression Assumption 3

set.seed(1984)
x1<-rnorm(n=1000,mean = 0, sd=2) # create indep. variable 1
x2<-rep(3,times=1000)
e<-rnorm(n=1000,mean=0, sd=1)  # create error
y<-2+3*x1+4*x2+e

# run regression including constant

summary(lm(y~x1+x2))


# Very high colinearity: x1 and X2 are very highly correlated

set.seed(1984)
x1<-rnorm(n=1000, mean=0, sd=2)  # create indep. variable 1
x2<-0.4*x1+rnorm(n=1000, mean=0, sd=0.01)
e<-rnorm(n=1000, mean=0, sd=1)
y<-2+3*x1+4*x2+e

# muliticolinearity affexts standard errors and thus the significance of results

cor.test(x2,x1)
summary(lm(y~x1+x2))


### Violation of Simple Linear Regression Assumption 4

set.seed(1984)
x1<-rnorm(n=1000, mean=0, sd=3)
x2<-rnorm(n=1000, mean=x1, sd=5)
plot(x1,x2)

cor.test(x=x1, y=x2)

e<-rnorm(n=1000, mean=0,sd=1)
y<-2+3*x1+4*x2+e
out.y.full<-lm(y~x1+x2)
out.y.x1.om<-lm(y~x1)
stargazer(out.y.full, out.y.x1.om, type = "text")


set.seed(1984)
x1<-rnorm(n=1000, mean=0, sd=3)
x2<-rnorm(n=1000, mean =-x1, sd=5 )
plot(x1,x2)


cor.test(x=x1,y=x2)
cor.test(x1,x2)


e<-rnorm(n=1000, mean = 0,sd=1)
y<-2+3*x1+4*x2+e
out.y.full<-lm(y~x1+x2)
out.y.x1.om<-lm(y~x1)
stargazer(out.y.full,out.y.x1.om,type = "text")


# The bias will be

out.y.full<-lm(y~x1+x2)
coeffs.full<-coefficients(out.y.full)
b2_hat<-coeffs.full[3]
b1_hat<-coeffs.full[2]

out.part.x2<-lm(x2~x1)
coeffs.part<-coefficients(out.part.x2)
delta<-coeffs.part[2]
bias<-delta*b2_hat
bias

b1_tilda=b1_hat+bias
b1_tilda


### Violation of simple linear regression Assumption 5

# SLR5: homoskedasticity

set.seed(1984)
x1<-rnorm(n=1000, mean=0,sd=3)
x2<-rnorm(n=1000, mean=0, sd=4)
e<-rnorm(n=1000, mean=0,sd=1)
s<-exp(0.4*x1)

yhat<-2+3*x1+4*x2+s*e  # model with heteroskedasticity
plot(x1, yhat)   # you cannot detect hetroskedasticity just by looking at the plot
 summary(out.lm.hat<-lm(yhat~x1+x2))
 
plot(y=residuals(out.lm.hat),x=x1) # residuals plot with heroskedasticity

# Create the same model without heteroskedasticity

ynorm<-2+3*x1+4*x2+e # model with no heteroskedasticity
summary(out.lm<-lm(ynorm~x1+x2))

plot(y=residuals(out.lm),x=x1)  # residuals plot without heteroskedasticity

# How can we know whether or not we have heteroskedasticity in our data?

help("bptest")

bptest(ynorm~x1+x2) # high p-value, can't reject null of constant variance

bptest(yhat~x1+x2) # low p-value, reject null of constant variance

## Variances of the regression coefficients

vcov(out.lm)
diag(vcov(out.lm))

sqrt(diag(vcov(out.lm))) #standard errors without adjustment

sqrt(diag(vcovHC(out.lm)))  # standard errors with adjustment

# With heteroskedasticity

vcovHC(out.lm.hat)
diag(vcovHC(out.lm.hat))

sqrt(diag(vcov(out.lm.hat)))  # standard errors without adjustment
sqrt(diag(vcovHC(out.lm.hat))) # standard errors with adjustment

coeftest(out.lm.hat)  # before correction
coeftest(out.lm.hat,vcov. = vcovHC(out.lm.hat))#after correction

# Extras

dt.population[1,]
dt.population[1:5]
c<-c(1,7,8)
dt.population[c,]
head(dt.population[,1])
head(dt.population[,1:2])
