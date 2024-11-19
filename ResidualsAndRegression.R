#install.packages("ggplot2")
#install.packages("olsrr")
#install.packages("PerformanceAnalytics")
install.packages("dplyr")

library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
library(dplyr)

ghg <- read.csv("/cloud/project/activity06/Deemer_GHG_Data.csv")
ghg$log.ch4 <- log(ghg$ch4+1) ##changing the ch4 value into a logarithmic scale. 

ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)

unique(ghg$Region)
ghg$BorealV <- ifelse(ghg$Region == "Boreal", 1, 0)
ghg$TropicalV <- ifelse(ghg$Region == "Tropical", 1, 0)
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)
ghg$HydroV <- ifelse(ghg$hydropower == "yes", 1, 0)
mod.full <- lm(log.ch4 ~ airTemp + 
                 log.age + mean.depth + 
                 log.DIP + 
                 log.precip + BorealV, data=ghg)

summary(mod.full)
res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

##qqplot
qqnorm(res.full, pch=19, col="grey50")
##qqplot shows that the residuals are normal. 
shapiro.test(res.full)
#calculates test stat, W, where the null is that the data is normally distributed. So high P value indiciates that we accept the null. 
#only want to use this test for smaller datasets. 

plot(fit.full, res.full, pch=19, col="grey50")
abline(h=0)

reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

chart.Correlation(reg.data, histogram = TRUE, pch=19)


# run stepwise
full.step <- ols_step_forward_aic(mod.full)
# view table
full.step 

plot(full.step )

predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="prediction")

predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="confidence")

## this graph (stepwise) shows how much each variable has influence. Air temp has the most, while depth has the least. 

#homework problem 1 
#start with making the transformation of CO_2
ghg$COTransform <- (1/(ghg$co2 + 1000))
ghg$t.age <- (1/(ghg$age+1000))
ghg$log.DIP <- log(ghg$DIP+1)
ghg$t.precip <- (1/(ghg$precipitation))
ghg$log.chloroA <- log(ghg$chlorophyll.a)
plot(ghg$log.chloroA)

mod.CO2Full <- lm(COTransform ~ BorealV +
                    age + 
                    log.DIP + 
                    mean.depth
                    + 
                 log.precip,data=ghg)


summary(mod.CO2Full)

#all are showing to be significant 

#next step: check assumptions 
#normality 
res.fullCO <- rstandard(mod.CO2Full)
fit.fullCO <- fitted.values(mod.CO2Full)
qqnorm(res.fullCO, pch=19, col="grey50")
qqline(res.fullCO)
#good enough for this test 

##residuals test 2-4
plot(fit.fullCO,res.fullCO, pch=19, col="grey50")
abline(h=0)

##thoughts -> very randomly distributed! 
reg.dataCO <- data.frame(ghg$log.DIP,
                       ghg$age,ghg$mean.depth,
                       ghg$BorealV,
                       ghg$log.precip)
chart.Correlation(reg.dataCO, histogram=TRUE, pch=19)
#good!

# run stepwise
full.stepCO <- ols_step_forward_aic(mod.CO2Full)
# view table
full.stepCO
plot(full.stepCO)


