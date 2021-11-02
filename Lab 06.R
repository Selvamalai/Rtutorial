
### Question 2

getwd()
require(data.table)
load("ceosal2.RData")
dthouse<-data.table(data)
require(stargazer)
list.files()
stargazer(dthouse,type = "text")
colnames(dthouse)
housep.lm<-lm(dthouse$salary~dthouse$sales+dthouse$mktval)
stargazer(housep.lm, type = "text")
loghousep.lm<-lm(dthouse$lsalary~dthouse$lsales+dthouse$lmktval)

stargazer(loghousep.lm, type="text")
coefficients(loghousep.lm)

## adding another variable profits
loghousep2.lm<-lm(dthouse$lsalary~dthouse$lsales+dthouse$lmktval+dthouse$profits)
stargazer(loghousep2.lm,type = "text")

# adding ceoten and comten to a multiple regression model


loghousep3.lm<-lm(dthouse$lsalary~dthouse$lsales+dthouse$lmktval+dthouse$profits+dthouse$ceoten+dthouse$comten)
stargazer(loghousep3.lm,type = "text")

#prediction of salary
coeffici=coefficients(loghousep3.lm)
lsalary<-coeffici[1]+ coeffici[2]*dthouse$lsales+coeffici[3]*dthouse$lmktval+coeffici[4]*dthouse$profits+coeffici[5]*dthouse$ceoten
+coeffici[6]*dthouse$comten
lsalary ## logarithm of salary

salary<-exp(lsalary) ## to convert into salary
salary
asalary<-dthouse$salary   #Actual salary
psalary<-salary  # predicted salary
resid<-asalary-psalary
head(resid)


###Question 1


getwd()
require(data.table)
load("hprice1.RData")
dtpri<-data.table(data)
require(stargazer)
list.files()
pri.lm<-lm(dtpri$price~dtpri$sqrft+dtpri$bdrms)
stargazer(pri.lm,type = "text")

Prediction
coeffpri<-coefficients(pri.lm)
price<-coeffpri[1]+coeffpri[2]*dtpri$sqrft+coeffpri[3]*dtpri$bdrms # prediction for whole sample

price1<-coeffpri[1]+coeffpri[2]*2438+coeffpri[3]*4  # predicted house price when sqrft=2438 and bedrms=4

price1

## addition of another variable colonial

pri1.lm<-lm(dtpri$price~dtpri$sqrft+dtpri$bdrms+dtpri$colonial)
stargazer(pri1.lm,type="text")
