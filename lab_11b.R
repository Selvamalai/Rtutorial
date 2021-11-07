### Data and packages

getwd()
setwd("C:/Users/Selvamalai/Desktop/UEA Materials/Econometrics/Lab/Lab 11 b Novth DiD")
 install.packages("data.table")

require(data.table)

 install.packages("ggplot2") 
install.packages("stargazer")
require(ggplot2)
require(stargazer)

load("fastfood3.RData")
load("fastfood4.RData")
load("fastfood.RData")


### Analysis and Results

## Explore the data

head(dt.fastfood)


## Plots

# Change in wages
library(ggplot2)


.rs.restartR()

plot1<- ggplot(data=dt.fastfood,aes(x=wage))
plot1+geom_density()+facet_wrap(~state+time)+theme_bw()

qplot(data=dt.fastfood, x=factor(time),y=emptot
      , fill=factor(state)
      , geom = "boxplot")+theme_bw()+xlab("time")+ylab("FTE Employment")
  
## Means of key variables

dt.bf.aft<-data.table(dt.fastfood)  ## create a new table called dt.bf.aft



dt.bf.aft<-dt.bf.aft[,list(mean_emptot=mean(dt.bf.aft$emptot , na.rm=TRUE)
  ,mean_wage=mean(dt.bf.aft$wage, na.rm=TRUE)
  ,mean_pmeal=mean(dt.bf.aft$pmeal,na.rm=TRUE)
  ,mean_hrsopen=mean(dt.bf.aft$hrsopen, na.rm=TRUE)
  ),by=list(state,time)]   ## Specify the list of grouping variables 

dt.bf.aft

## 
dt.bf.aft.clean<-dt.fastfood[!is.na(wage)]
dt.bf.aft.clean <- dt.bf.aft.clean[!is.na(pmeal),]
dt.bf.aft.clean <- dt.bf.aft.clean[!is.na(emptot),]
dt.bf.aft.clean <- dt.bf.aft.clean[!is.na(hrsopen),]
dt.bf.aft.clean <- dt.bf.aft.clean[!is.na(emptot),]


dt.bf.aft.clean <- data.table(dt.fastfood.clean)
dt.bf.aft.clean <- dt.bf.aft.clean[, list(
  mean_emptot = mean(emptot , na.rm=TRUE)
  , mean_wage = mean(wage , na.rm=TRUE)
  , mean_pmeal = mean(pmeal , na.rm=TRUE)
  , mean_hrsopen = mean(hrsopen , na.rm=TRUE)
), by=list(state, time)]
dt.bf.aft.clean



## Difference in FTE employment between NJ and PA at T0
t.test( dt.fastfood.clean[state==0 & time==0, emptot]
        , dt.fastfood.clean[state==1 & time==0, emptot])


## Difference in FTE employment between NJ and PA at T1

t.test( dt.fastfood.clean[state==0 & time==1, emptot]
        , dt.fastfood.clean[state==1 & time==1, emptot])


### Difference in Difference

#Using all data
(21.02743-20.44557) - (21.16558-23.33117) # 2.74745

#Using the clean clean data (balanced sub sample)

(20.71293-20.51397) - (21.50000-23.62687) # 2.32583

## Regression

# Effect on employment

lm1 <- lm( emptot ~ time + state + time*state, data = dt.fastfood.clean)




t.test(dt.fastfood.clean[stage==0&time==0,emptot]
       ,dt.fastfood.clean[state==1&time==0,emptot])
stargazer(lm1, type = "text")


coeffs <- coefficients(lm1)
coeffs


## Are the coefficients statistically significant?
dt.bf.aft.clean


#(??1) = emptot10 - emptot00 =

21.50000 - 23.62687


#(??2) = emptot01 - emptot00 =
21.50000 - 23.62687

  
#(??3) = (emptot11 - emptot10)-(emptot01 - emptot00) =


(20.71293 - 20.51397) - (21.50000 - 23.62687)

## Add controls for chain and ownership
lm <- lm( emptot ~ time + state + time*state + factor(chain) + co_owned
          , data = dt.fastfood.clean)

stargazer(lm, type = "text")


### Changes in meal prices

qplot( data = dt.fastfood, x = factor(time), y = pmeal
       , fill = factor(state)
       , geom = "boxplot") + theme_bw() + xlab("time") + ylab("Meal Price")


## Effect on meal prices
lm <- lm( pmeal ~ time + state + time*state
          , data = dt.fastfood.clean)

stargazer(lm, type = "text")


# Change in number of hours of operation:

qplot( data = dt.fastfood, x = factor(time), y = hrsopen
       , fill = factor(state)
       , geom = "boxplot") + theme_bw() + xlab("time") + ylab("Hours of Operation")

## Effect on hours open

lm <- lm( hrsopen ~ time + state + time*state
          , data = dt.fastfood.clean)
stargazer(lm, type = "text")

## Effect on the fraction of full-time employees

lm <- lm( fracft ~ time + state + time*state, data = dt.fastfood.clean)
stargazer(lm, type = "text")

## Alternative Specifications

summary(dt.fastfood.clean$gap)


lm <- lm( emptot ~ gap * time , data = dt.fastfood.clean)
stargazer(lm, type = "text")



### Extras: Data preparation

## Create variables for analysis

dt.fastfood.orig <- data.table(dt.fastfood.orig)


dt.fastfood.orig <- dt.fastfood.orig[,
                                     ":=" (
                                       emptot = emppt * 0.5 + empft + nmgrs
                                       ,emptot2 = emppt2 * 0.5 + empft2 + nmgrs2
                                       ,demp = (emppt2 * 0.5 + empft2 + nmgrs2) - (emppt * 0.5 + empft + nmgrs)
                                       ,gap = ifelse( state == 0, 0
                                                      , ifelse( wage_st >= 5.05
                                                                , 0
                                                                , (5.05-wage_st)/wage_st))
                                       ,nj = state
                                       ,bk = as.double(chain == 1)
                                       ,kfc = as.double(chain == 2)
                                       ,roys = as.double(chain == 3)
                                       ,wendys = as.double(chain == 4)
                                       ,pmeal = psoda + pfry + pentree
                                       ,pmeal2 = psoda2 + pfry2 + pentree2
                                       ,dpmeal = (psoda2 + pfry2 + pentree2) - (psoda + pfry + pentree)
                                       ,closed = as.double(status2 == 3)
                                       ,fracft = empft / (emppt * 0.5 + empft + nmgrs)
                                       ,fracft2 = ifelse( emppt2 * 0.5 + empft2 + nmgrs2 > 0
                                                          , empft2 / (emppt2 * 0.5 + empft2 + nmgrs2)
                                                          , NA)
                                       ,atmin = as.double(wage_st == 4.25)
                                       ,newmin = as.double(wage_st == 5.05)
                                       ,icode = ifelse( state == 0, "PA STORE",
                                                        ifelse( is.na(wage_st)
                                                                , "NJ Store Bad Wage"
                                                                , ifelse(wage_st == 4.25
                                                                         , "NJ Store Low-Wage"
                                                                         , ifelse(wage_st >= 5.0
                                                                                  , "NJ Store High-Wage"
                                                                                  , "NJ Store Med-Wage")))))]



dt.fastfood.orig <- dt.fastfood.orig[ order(nj) ]
dt.fastfood.orig <- dt.fastfood.orig[!status2 %in% c(0,2,4,5)]
head(dt.fastfood.orig)


## Create panel
dt.fastfood.panel <- rbind(
  dt.fastfood.orig[,list( emptot
                          , gap
                          , demp
                          , state
                          , chain
                          , co_owned
                          , atmin
                          , meals
                          , wage = wage_st
                          , hrsopen
                          , pmeal
                          , fracft
                          , time = 0
                          , id = 1:nrow(dt.fastfood.orig))]
  ,dt.fastfood.orig[,list( emptot = emptot2
                           , gap
                           , demp
                           , state
                           , chain
                           , co_owned
                           , atmin
                           , meals = meals2
                           , wage = wage_st2
                           , hrsopen = hrsopen2
                           , pmeal = pmeal2
                           , fracft = fracft2
                           , time = 1
                           , id = 1:nrow(dt.fastfood.orig))])

head(dt.fastfood.panel)

