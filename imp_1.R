##  IMPORT WHOLE DATASET OF cuddalore

library(tseries)
library(forecast)
library(fpp2)

library(readxl)
Cuddalore <- read_excel("F:/EXCEL/Cuddalore, Tamil Nadu .xlsx", 
                        sheet = "Cuddalore, Tamil Nadu ")
View(Cuddalore)

## RETRIVING ALL ATTRIBUTES OF THE DATASET

all <- Cuddalore[,5:10]
View(all)

plot(all)  ## plot all attributes

library(corrplot) ## loading library corrplot

cor(all)  ## find correlation coefficient of all attributes

cr <- cor(all)

corrplot(cr)  ## no strong correlation between temperature and all other attributes

##IMPORT DATA OF THE MONTH JANUARY
library(readxl)
jan <- read_excel("F:/EXCEL/Cuddalore, Tamil Nadu .xlsx", 
                  sheet = "jan")
View(jan)

## RETRIVING ALL ATTRIBUTES OF JANUARY

temp1 <-jan[,5:10]
View(temp1)

cor(temp1)  ## find correlation coefficient of all attributes

cr1 <- cor(temp1)

corrplot(cr1)  ##  strong correlation between temperature and wind speed in the month of january

#VAR model
library(urca)
library(mFilter)
library(vars)
library(tidyverse)

temp_jan <- ts(jan$temp , frequency = 24)
wind_jan <- ts(jan$`wind speed`, frequency = 24)

autoplot(cbind(temp_jan , wind_jan))

mod_jan <- cbind(temp_jan , wind_jan)
view(mod_jan)

colnames(mod_jan) <- cbind("temp" , "wind")
head(mod_jan)

mod_jan_70 <- ts(mod_jan[1:3643 , 1:2])  ## for 70% testing data
view(mod_jan_70)

lagselect <- VARselect(mod_jan_70 , lag.max = 70, type = "const")
lagselect$selection

mod_jan70_var <- VAR(mod_jan_70 , p = 50 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_jan70_var)

accuracy(mod_jan70_var$varresult[[1]])  ## checking accuracy of VAR

mod_jan_80 <- ts(mod_jan[1:4163 , 1:2])  ## for 80% testing data
view(mod_jan_80)

lagselect <- VARselect(mod_jan_80 , lag.max = 70, type = "const")
lagselect$selection

mod_jan80_var <- VAR(mod_jan_70 , p = 50 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_jan80_var)

accuracy(mod_jan80_var$varresult[[1]])  ## checking accuracy of VAR

mod_jan_90 <- ts(mod_jan[1:4684 , 1:2])  ## for 90% testing data
view(mod_jan_90)

lagselect <- VARselect(mod_jan_90 , lag.max = 70, type = "const")
lagselect$selection

mod_jan90_var <- VAR(mod_jan_90 , p = 51 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_jan90_var)

accuracy(mod_jan90_var$varresult[[1]])  ## checking accuracy of VAR

#neural network MODEL

library(MASS)
library(neuralnet)

set.seed(123)
apply(mod_jan , 2, range)
maxvalue <- apply(mod_jan , 2, max)
minvalue <- apply(mod_jan , 2, min)

data_frame <- as.data.frame(scale(mod_jan, center = minvalue, scale = maxvalue - minvalue))

ind <- sample(1:nrow(data_frame) , 3643)  ## 70% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

allvars <- colnames(data_frame)
predictionvars <- allvars[!allvars%in%"temp"]
predictionvars<- paste(predictionvars,collapse = "+")
form = as.formula(paste("temp~",predictionvars,collapse = "+"))

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

library(Metrics) #accuracy checking

rmse(predictions , actualvalues)  ## 70% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 70% testing data

ind <- sample(1:nrow(data_frame) ,4163)  ## 80% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 80% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 80% testing data

ind <- sample(1:nrow(data_frame) ,4684)  ## 90% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 90% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 90% training data

##IMPORT DATA OF THE MONTH FEBRUARY

library(readxl)
feb <- read_excel("F:/EXCEL/Cuddalore, Tamil Nadu .xlsx", 
                  sheet = "feb")
View(feb)

## RETRIVING ALL ATTRIBUTES OF FEBRUARY

temp_feb <- feb[,5:10]
View(temp_feb)

cor(temp_feb)  ## find correlation coefficient of all attributes

cr2 <- cor(temp_feb)

corrplot(cr2)  ## strong correlation between temperature and wind in the month of february

temp_feb <- ts(feb$temp , frequency = 24)
wind_feb <- ts(feb$`wind speed`, frequency = 24)

autoplot(cbind(temp_feb , wind_feb))

mod_feb <- cbind(temp_feb , wind_feb)
view(mod_feb)

colnames(mod_feb) <- cbind("temp" , "wind")
head(mod_feb)

mod_feb_70 <- ts(mod_feb[1:3327 , ])  ## for 70% testing data
view(mod_feb_70)

lagselect <- VARselect(mod_feb_70 , lag.max = 70, type = "const")
lagselect$selection

mod_feb70_var <- VAR(mod_feb_70 , p = 57 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_feb70_var)

accuracy(mod_feb70_var$varresult[[1]])  ## checking accuracy of VAR

mod_feb_80 <- ts(mod_feb[1:3802 , ])  ## for 80% testing data
view(mod_feb_80)

lagselect <- VARselect(mod_feb_80 , lag.max = 70, type = "const")
lagselect$selection

mod_feb80_var <- VAR(mod_feb_70 , p = 57 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_feb80_var)

accuracy(mod_feb80_var$varresult[[1]])  ## checking accuracy of VAR

mod_feb_90 <- ts(mod_feb[1:4278 , ])  ## for 90% testing data
view(mod_feb_90)

lagselect <- VARselect(mod_feb_90 , lag.max = 70, type = "const")
lagselect$selection

mod_feb90_var <- VAR(mod_feb_90 , p = 57 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_feb90_var)

accuracy(mod_feb90_var$varresult[[1]])  ## checking accuracy of VAR

#neural network MODEL

set.seed(123)
apply(mod_feb , 2, range)
maxvalue <- apply(mod_feb , 2, max)
minvalue <- apply(mod_feb , 2, min)

data_frame <- as.data.frame(scale(mod_feb, center = minvalue, scale = maxvalue - minvalue))

ind <- sample(1:nrow(data_frame) , 3327)  ## 70% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

allvars <- colnames(data_frame)
predictionvars <- allvars[!allvars%in%"temp"]
predictionvars<- paste(predictionvars,collapse = "+")
form = as.formula(paste("temp~",predictionvars,collapse = "+"))

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2] )
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

library(Metrics) #accuracy checking

rmse(predictions , actualvalues)  ## 70% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 70% testing data

ind <- sample(1:nrow(data_frame) , 3802)  ## 80% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 80% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 80% testing data

ind <- sample(1:nrow(data_frame) ,4278)  ## 90% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 90% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 90% training data

##IMPORT DATA OF THE MONTH MARCH

library(readxl)
mar <- read_excel("F:/EXCEL/Cuddalore, Tamil Nadu .xlsx", 
                  sheet = "mar")
View(mar)

## RETRIVING ALL ATTRIBUTES OF MARCH

temp_mar <- mar[ , 5:10]
View(temp_mar)

cor(temp_mar)  ## find correlation coefficient of all attributes

cr3 <- cor(temp_mar)

corrplot(cr3)  ##  strong correlation between temperature and wind in the month of march

temp_apr <- ts(apr$temp , frequency = 24)
wind_apr <- ts(apr$`wind speed`, frequency = 24)

autoplot(cbind(temp_apr , wind_apr))

mod_apr <- cbind(temp_apr , wind_apr)
view(mod_apr)

colnames(mod_apr) <- cbind("temp" , "wind")
head(mod_apr)

mod_apr_70 <- ts(mod_apr[1:3529 , ])  ## for 70% testing data
view(mod_apr_70)

lagselect <- VARselect(mod_apr_70 , lag.max = 70, type = "const")
lagselect$selection

mod_apr70_var <- VAR(mod_apr_70 , p = 51 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_apr70_var)

accuracy(mod_apr70_var$varresult[[1]])  ## checking accuracy of VAR

mod_mar_80 <- ts(mod_mar[1:4167 , ])  ## for 80% testing data
view(mod_mar_80)

lagselect <- VARselect(mod_mar_80 , lag.max = 70, type = "const")
lagselect$selection

mod_mar80_var <- VAR(mod_mar_70 , p = 50 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_mar80_var)

accuracy(mod_mar80_var$varresult[[1]])  ## checking accuracy of VAR

mod_mar_90 <- ts(mod_mar[1:4688 , 1:2])  ## for 90% testing data
view(mod_mar_90)

lagselect <- VARselect(mod_mar_90 , lag.max = 70, type = "const")
lagselect$selection

mod_mar90_var <- VAR(mod_mar_90 , p = 50 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_mar90_var)

accuracy(mod_mar90_var$varresult[[1]])  ## checking accuracy of VAR

#neural network MODEL

set.seed(123)
apply(mod_mar , 2, range)
maxvalue <- apply(mod_mar , 2, max)
minvalue <- apply(mod_mar , 2, min)

data_frame <- as.data.frame(scale(mod_mar, center = minvalue, scale = maxvalue - minvalue))

ind <- sample(1:nrow(data_frame) , 3646)  ## 70% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

allvars <- colnames(data_frame)
predictionvars <- allvars[!allvars%in%"temp"]
predictionvars<- paste(predictionvars,collapse = "+")
form = as.formula(paste("temp~",predictionvars,collapse = "+"))

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 70% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 70% testing data

ind <- sample(1:nrow(data_frame) ,4167)  ## 80% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 80% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 80% testing data

ind <- sample(1:nrow(data_frame) ,4688)  ## 90% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 90% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 90% training data

##IMPORT DATA OF THE MONTH APRIL

library(readxl)
apr <- read_excel("F:/EXCEL/Cuddalore, Tamil Nadu .xlsx", 
                  sheet = "apr")
View(apr)

## RETRIVING ALL ATTRIBUTES OF APRIL

temp_apr <- apr[ , 5: 10]
View(temp_apr)

cor(temp_apr)  ## find correlation coefficient of all attributes

cr4 <- cor(temp_apr)

corrplot(cr4)  ## strong correlation between temperature and wind speed in the month of april

temp_apr <- ts(apr$temp , frequency = 24)
wind_apr <- ts(apr$`wind speed`, frequency = 24)

autoplot(cbind(temp_apr , wind_apr))

mod_apr <- cbind(temp_apr , wind_apr)
view(mod_apr)

colnames(mod_apr) <- cbind("temp" , "wind")
head(mod_apr)

mod_apr_70 <- ts(mod_apr[1:3529 , ])  ## for 70% testing data
view(mod_apr_70)

lagselect <- VARselect(mod_apr_70 , lag.max = 70, type = "const")
lagselect$selection

mod_apr70_var <- VAR(mod_apr_70 , p = 51 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_apr70_var)

accuracy(mod_apr70_var$varresult[[1]])  ## checking accuracy of VAR

mod_apr_80 <- ts(mod_apr[1:4031 , ])  ## for 80% testing data
view(mod_apr_80)

lagselect <- VARselect(mod_apr_80 , lag.max = 70, type = "const")
lagselect$selection

mod_apr80_var <- VAR(mod_apr_70 , p = 51 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_apr80_var)

accuracy(mod_apr80_var$varresult[[1]])  ## checking accuracy of VAR

mod_apr_90 <- ts(mod_apr[1:4537 , 1:2])  ## for 90% testing data
view(mod_apr_90)

lagselect <- VARselect(mod_apr_90 , lag.max = 70, type = "const")
lagselect$selection

mod_apr90_var <- VAR(mod_apr_90 , p = 50 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_apr90_var)

accuracy(mod_apr90_var$varresult[[1]])  ## checking accuracy of VAR

#neural network MODEL

set.seed(123)
apply(mod_apr , 2, range)
maxvalue <- apply(mod_apr , 2, max)
minvalue <- apply(mod_apr , 2, min)

data_frame <- as.data.frame(scale(mod_apr, center = minvalue, scale = maxvalue - minvalue))

ind <- sample(1:nrow(data_frame) , 3529)  ## 70% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

allvars <- colnames(data_frame)
predictionvars <- allvars[!allvars%in%"temp"]
predictionvars<- paste(predictionvars,collapse = "+")
form = as.formula(paste("temp~",predictionvars,collapse = "+"))

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 70% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 70% testing data

ind <- sample(1:nrow(data_frame) ,4031)  ## 80% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 80% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 80% testing data

ind <- sample(1:nrow(data_frame) ,4537)  ## 90% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 90% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 90% training data

##IMPORT DATA OF THE MONTH MAY

library(readxl)
may <- read_excel("F:/EXCEL/Cuddalore, Tamil Nadu .xlsx", 
                  sheet = "may")
View(may)

## RETRIVING ALL ATTRIBUTES OF MAY

temp_may <- may[, 5:10]
View(temp_may)

cor(temp_may)  ## find correlation coefficient of all attributes

cr5 <- cor(temp_may)

corrplot(cr5)  ## no strong correlation between temperature and all other attributes in the month of may

TEMP_MAY <- ts(may$temp, frequency = 24) 
View(TEMP_MAY)

plot.ts(TEMP_MAY)

adf.test(TEMP_MAY)   ## check stationarity

may_70 <-temp_may[1:3646 , 1]  ## 70% data extracting for training
View(may_70)

## forecast for 70% training data

## seasonal naive model 

fit_may70_naive <- snaive(may_70)

print(summary(fit_may70_naive))

checkresiduals(fit_may70_naive)

forecast_may70_naive = forecast(fit_may70_naive)  ##forecast model 

forecast_may70_naive

plot(forecast_may70_naive)

may_70 <- ts(temp_may[1:3646 , 1])
View(may_70)

## exponential smoothing model

fit_may70_exp <- ets(may_70)

print(summary(fit_may70_exp))

checkresiduals(fit_may70_exp)

forecast_may70_exp = forecast(fit_may70_exp) ## forecast model

forecast_may70_exp

plot(forecast_may70_exp)

## ARIMA model

fit_may70_arima <- auto.arima(may_70)

print(summary(fit_may70_arima))

checkresiduals(fit_may70_arima)

forecast_may70_arima = forecast(fit_may70_arima) ## forecast model

forecast_may70_arima

plot(forecast_may70_arima)

may_80 <- ts(temp_may[1:4167, 1])   ## 80% data extracting for training

View(may_80)

## forecast for 80% training data

## seasonal naive model 

fit_may80_naive <- snaive(may_80)

print(summary(fit_may80_naive))

checkresiduals(fit_may80_naive)

forecast_may80_naive = forecast(fit_may80_naive)  ##forecast model 

forecast_may80_naive

plot(forecast_may80_naive)

## exponential smoothing model

fit_may80_exp <- ets(may_80)

print(summary(fit_may80_exp))

checkresiduals(fit_may80_exp)

forecast_may80_exp = forecast(fit_may80_exp) ## forecast model

forecast_may80_exp

plot(forecast_may80_exp)

## ARIMA model

fit_may80_arima <- auto.arima(may_80)

print(summary(fit_may80_arima))

checkresiduals(fit_may80_arima)

forecast_may80_arima = forecast(fit_may80_arima) ## forecast model

forecast_may80_arima

plot(forecast_may80_arima)

may_90 <- ts(temp_may[1:4688 ,1])    ## 90% data extracting for training
View(may_90)

## forecast for 90% training data

## seasonal naive model 

fit_may90_naive <- snaive(may_90)

print(summary(fit_may90_naive))

checkresiduals(fit_may90_naive)

forecast_may90_naive = forecast(fit_may90_naive)  ##forecast model 

forecast_may90_naive

plot(forecast_may90_naive)

## exponential smoothing model

fit_may90_exp <- ets(may_90)

print(summary(fit_may90_exp))

checkresiduals(fit_may90_exp)

forecast_may90_exp = forecast(fit_may90_exp) ## forecast model

forecast_may90_exp

plot(forecast_may90_exp)

## ARIMA model

fit_may90_arima <- auto.arima(may_90)

print(summary(fit_may90_arima))

checkresiduals(fit_may90_arima)

forecast_may90_arima = forecast(fit_may90_arima) ## forecast model

forecast_may90_arima

plot(forecast_may90_arima)

##IMPORT DATA OF THE MONTH JUNE

library(readxl)
jun <- read_excel("F:/EXCEL/Cuddalore, Tamil Nadu .xlsx", 
                  sheet = "june")
View(jun)

## RETRIVING ALL ATTRIBUTES OF JUNE

temp_jun <- jun[,5:10]
View(temp_jun)

cor(temp_jun)  ## find correlation coefficient of all attributes

cr6 <- cor(temp_jun)

corrplot(cr6)  ## no strong correlation between temperature and all other attributes in the month of june

TEMP_JUN <- ts(jun$temp, frequency = 24) 
View(TEMP_JUN)

plot.ts(TEMP_JUN)
adf.test(TEMP_JUN)   ## check stationarity

jun_70 <- ts(temp_jun[1:3529 , 1])    ## 70% data extracting for training
View(jun_70) 

## forecast for 70% training data

## seasonal naive model 

fit_jun70_naive <- snaive(jun_70)

print(summary(fit_jun70_naive))

checkresiduals(fit_jun70_naive)

forecast_jun70_naive = forecast(fit_jun70_naive)  ##forecast model 

forecast_jun70_naive

plot(forecast_jun70_naive)

## exponential smoothing model

fit_jun70_exp <- ets(jun_70)

print(summary(fit_jun70_exp))

checkresiduals(fit_jun70_exp)

forecast_jun70_exp = forecast(fit_jun70_exp) ## forecast model

forecast_jun70_exp

plot(forecast_jun70_exp)

## ARIMA model

fit_jun70_arima <- auto.arima(jun_70)

print(summary(fit_jun70_arima))

checkresiduals(fit_jun70_arima)

forecast_jun70_arima = forecast(fit_jun70_arima) ## forecast model

forecast_jun70_arima

plot(forecast_jun70_arima)

jun_80 <- ts(temp_jun[1:4033 , 1])    ## 80% data extracting for training
View(jun_80) 

## forecast for 80% training data

## seasonal naive model 

fit_jun80_naive <- snaive(jun_80)

print(summary(fit_jun80_naive))

checkresiduals(fit_jun80_naive)

forecast_jun80_naive = forecast(fit_jun80_naive)  ##forecast model 

forecast_jun80_naive

plot(forecast_jun80_naive)

## exponential smoothing model

fit_jun80_exp <- ets(jun_80)

print(summary(fit_jun80_exp))

checkresiduals(fit_jun80_exp)

forecast_jun80_exp = forecast(fit_jun80_exp) ## forecast model

forecast_jun80_exp

plot(forecast_jun80_exp)

## ARIMA model

fit_jun80_arima <- auto.arima(jun_80)

print(summary(fit_jun80_arima))

checkresiduals(fit_jun80_arima)

forecast_jun80_arima = forecast(fit_jun80_arima) ## forecast model

forecast_jun80_arima

plot(forecast_jun80_arima)

jun_90 <- ts(temp_jun[1:4537 , 1])    ## 90% data extracting for training
View(jun_90) 

## forecast for 90% training data

## seasonal naive model 

fit_jun90_naive <- snaive(jun_90)

print(summary(fit_jun90_naive))

checkresiduals(fit_jun90_naive)

forecast_jun90_naive = forecast(fit_jun90_naive)  ##forecast model 

forecast_jun90_naive

plot(forecast_jun90_naive)

## exponential smoothing model

fit_jun90_exp <- ets(jun_90)

print(summary(fit_jun90_exp))

checkresiduals(fit_jun90_exp)

forecast_jun90_exp = forecast(fit_jun90_exp) ## forecast model

forecast_jun90_exp

plot(forecast_jun90_exp)

## ARIMA model

fit_jun90_arima <- auto.arima(jun_90)

print(summary(fit_jun90_arima))

checkresiduals(fit_jun90_arima)

forecast_jun90_arima = forecast(fit_jun90_arima) ## forecast model

forecast_jun90_arima

plot(forecast_jun90_arima)

##IMPORT DATA OF THE MONTH JULY

library(readxl)
jul <- read_excel("F:/EXCEL/Cuddalore, Tamil Nadu .xlsx", 
                  sheet = "july")
View(jul)

## RETRIVING ALL ATTRIBUTES OF JULY

temp_jul <- jul[ , 5:10]
View(temp_jul)

cor(temp_jul)  ## find correlation coefficient of all attributes

cr7 <- cor(temp_jul)

corrplot(cr7)  ## no strong correlation between temperature and all other attributes in the month of july

TEMP_JUL <- ts(jul$temp, frequency = 24) 
View(TEMP_JUL)

plot.ts(TEMP_JUL)

adf.test(TEMP_JUL)   ## check stationarity

jul_70 <- ts(temp_jul[1:3646 , 1] ) ## 70% data extracting for training
View(jul_70)

## forecast for 70% training data

## seasonal naive model 

fit_jul70_naive <- snaive(jul_70)

print(summary(fit_jul70_naive))

checkresiduals(fit_jul70_naive)

forecast_jul70_naive = forecast(fit_jul70_naive)  ##forecast model 

forecast_jul70_naive

plot(forecast_jul70_naive)

## exponential smoothing model

fit_jul70_exp <- ets(jul_70)

print(summary(fit_jul70_exp))

checkresiduals(fit_jul70_exp)

forecast_jul70_exp = forecast(fit_jul70_exp) ## forecast model

forecast_jul70_exp

plot(forecast_jul70_exp)

## ARIMA model

fit_jul70_arima <- auto.arima(jul_70)

print(summary(fit_jul70_arima))

checkresiduals(fit_jul70_arima)

forecast_jul70_arima = forecast(fit_jul70_arima) ## forecast model

forecast_jul70_arima

plot(forecast_jul70_arima)

jul_80 <- ts(temp_jul[1:4167 , 1])  ## 80% data extracting for training
View(jul_80)

## forecast for 80% training data

## seasonal naive model 

fit_jul80_naive <- snaive(jul_80)

print(summary(fit_jul80_naive))

checkresiduals(fit_jul80_naive)

forecast_jul80_naive = forecast(fit_jul80_naive)  ##forecast model 

forecast_jul80_naive

plot(forecast_jul80_naive)

## exponential smoothing model

fit_jul80_exp <- ets(jul_80)

print(summary(fit_jul80_exp))

checkresiduals(fit_jul80_exp)

forecast_jul80_exp = forecast(fit_jul80_exp) ## forecast model

forecast_jul80_exp

plot(forecast_jul80_exp)

## ARIMA model

fit_jul80_arima <- auto.arima(jul_80)

print(summary(fit_jul80_arima))

checkresiduals(fit_jul80_arima)

forecast_jul80_arima = forecast(fit_jul80_arima) ## forecast model

forecast_jul80_arima

plot(forecast_jul80_arima)

jul_90 <- ts(temp_jul[1:4688 ,1])  ## 90% data extracting for training
View(jul_90)

## forecast for 90% training data

## seasonal naive model 

fit_jul90_naive <- snaive(jul_90)

print(summary(fit_jul90_naive))

checkresiduals(fit_jul90_naive)

forecast_jul90_naive = forecast(fit_jul90_naive)  ##forecast model 

forecast_jul90_naive

plot(forecast_jul90_naive)

## exponential smoothing model

fit_jul90_exp <- ets(jul_90)

print(summary(fit_jul90_exp))

checkresiduals(fit_jul90_exp)

forecast_jul90_exp = forecast(fit_jul90_exp) ## forecast model

forecast_jul90_exp

plot(forecast_jul90_exp)

## ARIMA model

fit_jul90_arima <- auto.arima(jul_90)

print(summary(fit_jul90_arima))

checkresiduals(fit_jul90_arima)

forecast_jul90_arima = forecast(fit_jul90_arima) ## forecast model

forecast_jul90_arima

plot(forecast_jul90_arima)

##IMPORT DATA OF THE MONTH AUGUST

library(readxl)
aug <- read_excel("F:/EXCEL/Cuddalore, Tamil Nadu .xlsx", 
                  sheet = "aug")
View(aug)

## RETRIVING ALL ATTRIBUTES OF August

temp_aug <- aug[,5:10]
View(temp_aug)

cor(temp_aug)  ## find correlation coefficient of all attributes

cr8 <- cor(temp_aug)

corrplot(cr8)  ##  no strong correlation between temperature and other attributes in the month of August

TEMP_aug <- ts(aug$temp, frequency = 24) 
View(TEMP_aug)

plot.ts(TEMP_aug)
adf.test(TEMP_aug)   ## check stationarity

aug_70 <- ts(temp_aug[1:3646 , 1] ) ## 70% data extracting for training
View(aug_70)

## forecast for 70% training data

## seasonal naive model 

fit_aug70_naive <- snaive(aug_70)

print(summary(fit_aug70_naive))

checkresiduals(fit_aug70_naive)

forecast_aug70_naive = forecast(fit_aug70_naive)  ##forecast model 

forecast_aug70_naive

plot(forecast_aug70_naive)

## exponential smoothing model

fit_aug70_exp <- ets(aug_70)

print(summary(fit_aug70_exp))

checkresiduals(fit_aug70_exp)

forecast_aug70_exp = forecast(fit_aug70_exp) ## forecast model

forecast_aug70_exp

plot(forecast_aug70_exp)

## ARIMA model

fit_aug70_arima <- auto.arima(aug_70)

print(summary(fit_aug70_arima))

checkresiduals(fit_aug70_arima)

forecast_aug70_arima = forecast(fit_aug70_arima) ## forecast model

forecast_aug70_arima

plot(forecast_aug70_arima)

aug_80 <- ts(temp_aug[1:4167 , 1])  ## 80% data extracting for training
View(aug_80)

## forecast for 80% training data

## seasonal naive model 

fit_aug80_naive <- snaive(aug_80)

print(summary(fit_aug80_naive))

checkresiduals(fit_aug80_naive)

forecast_aug80_naive = forecast(fit_aug80_naive)  ##forecast model 

forecast_aug80_naive

plot(forecast_aug80_naive)

## exponential smoothing model

fit_aug80_exp <- ets(aug_80)

print(summary(fit_aug80_exp))

checkresiduals(fit_aug80_exp)

forecast_aug80_exp = forecast(fit_aug80_exp) ## forecast model

forecast_aug80_exp

plot(forecast_aug80_exp)

## ARIMA model

fit_aug80_arima <- auto.arima(aug_80)

print(summary(fit_aug80_arima))

checkresiduals(fit_aug80_arima)

forecast_aug80_arima = forecast(fit_aug80_arima) ## forecast model

forecast_aug80_arima

plot(forecast_aug80_arima)

aug_90 <- ts(temp_aug[1:4688 ,1])  ## 90% data extracting for training
View(aug_90)

## forecast for 90% training data

## seasonal naive model 

fit_aug90_naive <- snaive(aug_90)

print(summary(fit_aug90_naive))

checkresiduals(fit_aug90_naive)

forecast_aug90_naive = forecast(fit_aug90_naive)  ##forecast model 

forecast_aug90_naive

plot(forecast_aug90_naive)

## exponential smoothing model

fit_aug90_exp <- ets(aug_90)

print(summary(fit_aug90_exp))

checkresiduals(fit_aug90_exp)

forecast_aug90_exp = forecast(fit_aug90_exp) ## forecast model

forecast_aug90_exp

plot(forecast_aug90_exp)

## ARIMA model

fit_aug90_arima <- auto.arima(aug_90)

print(summary(fit_aug90_arima))

checkresiduals(fit_aug90_arima)

forecast_aug90_arima = forecast(fit_aug90_arima) ## forecast model

forecast_aug90_arima

plot(forecast_aug90_arima)

##IMPORT DATA OF THE MONTH September

library(readxl)
sep <- read_excel("F:/EXCEL/Cuddalore, Tamil Nadu .xlsx", 
                  sheet = "sep")
View(sep)

## RETRIVING ALL ATTRIBUTES OF September

temp_sep <- sep[,5:10]
View(temp_sep)

cor(temp_sep)  ## find correlation coefficient of all attributes

cr9 <- cor(temp_sep)

corrplot(cr9)  ##  no strong correlation between temperature and other attributes in the month of August

TEMP_sep <- ts(sep$temp, frequency = 24) 
View(TEMP_sep)

plot.ts(TEMP_sep)
adf.test(TEMP_sep)   ## check stationarity

sep_70 <- ts(temp_sep[1:3528 , 1] ) ## 70% data extracting for training
View(sep_70)

## forecast for 70% training data

## seasonal naive model 

fit_sep70_naive <- snaive(sep_70)

print(summary(fit_sep70_naive))

checkresiduals(fit_sep70_naive)

forecast_sep70_naive = forecast(fit_sep70_naive)  ##forecast model 

forecast_sep70_naive

plot(forecast_sep70_naive)

## exponential smoothing model

fit_sep70_exp <- ets(sep_70)

print(summary(fit_sep70_exp))

checkresiduals(fit_sep70_exp)

forecast_sep70_exp = forecast(fit_sep70_exp) ## forecast model

forecast_sep70_exp

plot(forecast_sep70_exp)

## ARIMA model

fit_sep70_arima <- auto.arima(sep_70)

print(summary(fit_sep70_arima))

checkresiduals(fit_sep70_arima)

forecast_sep70_arima = forecast(fit_sep70_arima) ## forecast model

forecast_sep70_arima

plot(forecast_sep70_arima)

sep_80 <- ts(temp_sep[1:4032 , 1])  ## 80% data extracting for training
View(sep_80)

## forecast for 80% training data

## seasonal naive model 

fit_sep80_naive <- snaive(sep_80)

print(summary(fit_sep80_naive))

checkresiduals(fit_sep80_naive)

forecast_sep80_naive = forecast(fit_sep80_naive)  ##forecast model 

forecast_sep80_naive

plot(forecast_sep80_naive)

## exponential smoothing model

fit_sep80_exp <- ets(sep_80)

print(summary(fit_sep80_exp))

checkresiduals(fit_sep80_exp)

forecast_sep80_exp = forecast(fit_sep80_exp) ## forecast model

forecast_sep80_exp

plot(forecast_sep80_exp)

## ARIMA model

fit_sep80_arima <- auto.arima(sep_80)

print(summary(fit_sep80_arima))

checkresiduals(fit_sep80_arima)

forecast_sep80_arima = forecast(fit_sep80_arima) ## forecast model

forecast_sep80_arima

plot(forecast_sep80_arima)

sep_90 <- ts(temp_sep[1:4536 ,1])  ## 90% data extracting for training
View(sep_90)

## forecast for 90% training data

## seasonal naive model 

fit_sep90_naive <- snaive(sep_90)

print(summary(fit_sep90_naive))

checkresiduals(fit_sep90_naive)

forecast_sep90_naive = forecast(fit_sep90_naive)  ##forecast model 

forecast_sep90_naive

plot(forecast_sep90_naive)

## exponential smoothing model

fit_sep90_exp <- ets(sep_90)

print(summary(fit_sep90_exp))

checkresiduals(fit_sep90_exp)

forecast_sep90_exp = forecast(fit_sep90_exp) ## forecast model

forecast_sep90_exp

plot(forecast_sep90_exp)

## ARIMA model

fit_sep90_arima <- auto.arima(sep_90)

print(summary(fit_sep90_arima))

checkresiduals(fit_sep90_arima)

forecast_sep90_arima = forecast(fit_sep90_arima) ## forecast model

forecast_sep90_arima

plot(forecast_sep90_arima)

##IMPORT DATA OF THE MONTH october

library(readxl)
oct <- read_excel("F:/EXCEL/Cuddalore, Tamil Nadu .xlsx", 
                  sheet = "oct")
View(oct)

## RETRIVING ALL ATTRIBUTES OF OCTOBER

temp_oct <- oct[,5:10]
View(temp_oct)

cor(temp_oct)  ## find correlation coefficient of all attributes

cr10 <- cor(temp_oct)

corrplot(cr10)  ##  no strong correlation between temperature and other attributes in the month of August

TEMP_oct <- ts(oct$temp, frequency = 24) 
View(TEMP_oct)

plot.ts(TEMP_oct)
adf.test(TEMP_oct)   ## check stationarity

oct_70 <- ts(temp_oct[1:3646 , 1] ) ## 70% data extracting for training
View(oct_70)

## forecast for 70% training data

## seasonal naive model 

fit_oct70_naive <- snaive(oct_70)

print(summary(fit_oct70_naive))

checkresiduals(fit_oct70_naive)

forecast_oct70_naive = forecast(fit_oct70_naive)  ##forecast model 

forecast_oct70_naive

plot(forecast_oct70_naive)

## exponential smoothing model

fit_oct70_exp <- ets(oct_70)

print(summary(fit_oct70_exp))

checkresiduals(fit_oct70_exp)

forecast_oct70_exp = forecast(fit_oct70_exp) ## forecast model

forecast_oct70_exp

plot(forecast_oct70_exp)

## ARIMA model

fit_oct70_arima <- auto.arima(oct_70)

print(summary(fit_oct70_arima))

checkresiduals(fit_oct70_arima)

forecast_oct70_arima = forecast(fit_oct70_arima) ## forecast model

forecast_oct70_arima

plot(forecast_oct70_arima)

oct_80 <- ts(temp_oct[1:4166 , 1])  ## 80% data extracting for training
View(oct_80)

## forecast for 80% training data

## seasonal naive model 

fit_oct80_naive <- snaive(oct_80)

print(summary(fit_oct80_naive))

checkresiduals(fit_oct80_naive)

forecast_oct80_naive = forecast(fit_oct80_naive)  ##forecast model 

forecast_oct80_naive

plot(forecast_oct80_naive)

## exponential smoothing model

fit_oct80_exp <- ets(oct_80)

print(summary(fit_oct80_exp))

checkresiduals(fit_oct80_exp)

forecast_oct80_exp = forecast(fit_oct80_exp) ## forecast model

forecast_oct80_exp

plot(forecast_oct80_exp)

## ARIMA model

fit_oct80_arima <- auto.arima(oct_80)

print(summary(fit_oct80_arima))

checkresiduals(fit_oct80_arima)

forecast_oct80_arima = forecast(fit_oct80_arima) ## forecast model

forecast_oct80_arima

plot(forecast_oct80_arima)

oct_90 <- ts(temp_oct[1:4687 ,1])  ## 90% data extracting for training
View(oct_90)

## forecast for 90% training data

## seasonal naive model 

fit_oct90_naive <- snaive(oct_90)

print(summary(fit_oct90_naive))

checkresiduals(fit_oct90_naive)

forecast_oct90_naive = forecast(fit_oct90_naive)  ##forecast model 

forecast_oct90_naive

plot(forecast_oct90_naive)

## exponential smoothing model

fit_oct90_exp <- ets(oct_90)

print(summary(fit_oct90_exp))

checkresiduals(fit_oct90_exp)

forecast_oct90_exp = forecast(fit_oct90_exp) ## forecast model

forecast_oct90_exp

plot(forecast_oct90_exp)

## ARIMA model

fit_oct90_arima <- auto.arima(oct_90)

print(summary(fit_oct90_arima))

checkresiduals(fit_oct90_arima)

forecast_oct90_arima = forecast(fit_oct90_arima) ## forecast model

forecast_oct90_arima

plot(forecast_oct90_arima)

##IMPORT DATA OF THE MONTH NOVEMBER

library(readxl)
nov <- read_excel("F:/EXCEL/Cuddalore, Tamil Nadu .xlsx", 
                  sheet = "nov")
View(nov)

## RETRIVING ALL ATTRIBUTES OF November

temp_nov <- nov[,5:10]
View(temp_nov)

cor(temp_nov)  ## find correlation coefficient of all attributes

cr11 <- cor(temp_nov)

corrplot(cr11)  ##  strong correlation between temperature and wind speed in the month of november
#VAR model

TEMP_NOV <- ts(nov$temp, frequency = 24)
WIND_NOV <- ts(nov$`wind speed` , frequency = 24)

autoplot(cbind(TEMP_NOV,WIND_NOV))

mod_nov <- cbind(TEMP_NOV,WIND_NOV)
view(mod_nov)

colnames(mod_nov) <- cbind("temp" , "wind")
head(mod_nov)

mod_nov_70 <- ts(mod_nov[1:3528 , 1:2])  ## for 70%% training data
view(mod_nov_70)

lagselect <- VARselect(mod_nov_70 , lag.max = 70, type = "const")
lagselect$selection

mod_nov70_var <- VAR(mod_nov_70 , p = 52 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_nov70_var)

accuracy(mod_nov70_var$varresult[[1]])  ## checking accuracy of VAR

mod_nov_80 <- ts(mod_nov[1:4032 , 1:2])  ## for 80% training data
view(mod_nov_80)

lagselect <- VARselect(mod_nov_80 , lag.max = 70, type = "const")
lagselect$selection

mod_nov80_var <- VAR(mod_nov_80 , p = 52 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_nov80_var)

accuracy(mod_nov80_var$varresult[[1]])  ## checking accuracy of VAR

mod_nov_90 <- ts(mod_nov[1:4536 , 1:2])  ## for 90% training data
view(mod_nov_90)

lagselect <- VARselect(mod_nov_90 , lag.max = 70, type = "const")
lagselect$selection

mod_nov90_var <- VAR(mod_nov_90 , p = 52 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_nov90_var)

accuracy(mod_nov90_var$varresult[[1]])  ## checking accuracy of VAR

#neural network MODEL

set.seed(123)
apply(mod_nov , 2, range)
maxvalue <- apply(mod_nov , 2, max)
minvalue <- apply(mod_nov , 2, min)

data_frame <- as.data.frame(scale(mod_nov, center = minvalue, scale = maxvalue - minvalue))

ind <- sample(1:nrow(data_frame) , 3528)  ## 70% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

allvars <- colnames(data_frame)
predictionvars <- allvars[!allvars%in%"temp"]
predictionvars<- paste(predictionvars,collapse = "+")
form = as.formula(paste("temp~",predictionvars,collapse = "+"))

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 70% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 70% testing data

ind <- sample(1:nrow(data_frame) ,4032 )  ## 80% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 80% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 80% testing data

ind <- sample(1:nrow(data_frame) ,4536)  ## 90% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 90% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 90% training data

# for month of DECEMBER

library(readxl)
dec <- read_excel("F:/EXCEL/Cuddalore, Tamil Nadu .xlsx", 
                  sheet = "dec")
View(dec)

## RETRIVING ALL ATTRIBUTES OF december

temp_dec <- dec[,5:10]
View(temp_dec)

cor(temp_dec)  ## find correlation coefficient of all attributes

cr12 <- cor(temp_dec)

corrplot(cr12)  ##  strong correlation between temperature and wind speed in the month of December

#VAR model

TEMP_DEC  <- ts(dec$temp, frequency = 24)
WIND_DEC  <- ts(dec$`wind speed`, frequency = 24)

autoplot(cbind(TEMP_DEC,WIND_DEC))

mod_dec <- cbind(TEMP_DEC,WIND_DEC)
view(mod_dec)

colnames(mod_dec) <- cbind("temp" , "wind")
head(mod_dec)

mod_dec_70 <- ts(mod_dec[1:3646 , 1:2])  ## for 70%% training data
view(mod_dec_70)

lagselect <- VARselect(mod_dec_70 , lag.max = 70, type = "const")
lagselect$selection

mod_dec70_var <- VAR(mod_dec_70 , p = 59 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_dec70_var)

accuracy(mod_dec70_var$varresult[[1]])  ## checking accuracy of VAR

mod_dec_80 <- ts(mod_dec[1:4166 , 1:2])  ## for 80% training data
view(mod_dec_80)

lagselect <- VARselect(mod_dec_80 , lag.max = 100, type = "const")
lagselect$selection

mod_dec80_var <- VAR(mod_dec_80 , p = 98 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_dec80_var)

accuracy(mod_dec80_var$varresult[[1]])  ## checking accuracy of VAR

mod_dec_90 <- ts(mod_dec[1:4687 , 1:2])  ## for 90% training data
view(mod_dec_90)

lagselect <- VARselect(mod_dec_90 , lag.max = 100, type = "const")
lagselect$selection

mod_dec90_var <- VAR(mod_dec_90 , p = 98 ,type = "const" , season = NULL , exogen = NULL)
summary(mod_dec90_var)

accuracy(mod_dec90_var$varresult[[1]])  ## checking accuracy of VAR

#neural network MODEL

set.seed(123)
apply(mod_dec , 2, range)
maxvalue <- apply(mod_dec , 2, max)
minvalue <- apply(mod_dec , 2, min)

data_frame <- as.data.frame(scale(mod_dec, center = minvalue, scale = maxvalue - minvalue))

ind <- sample(1:nrow(data_frame) , 3646)  ## 70% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

allvars <- colnames(data_frame)
predictionvars <- allvars[!allvars%in%"temp"]
predictionvars<- paste(predictionvars,collapse = "+")
form = as.formula(paste("temp~",predictionvars,collapse = "+"))

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 70% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 70% testing data

ind <- sample(1:nrow(data_frame) ,4167)  ## 80% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 80% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 80% testing data

ind <- sample(1:nrow(data_frame) ,4688)  ## 90% testing data
trainDF <- data_frame[ind,]
testDF <- data_frame[-ind,]

neuralmodel <- neuralnet(formula = form ,trainDF,  hidden = c(2,1), linear.output = T)

plot(neuralmodel)

predictions <- compute(neuralmodel , testDF[,1:2])
str(predictions)

predictions <- predictions$net.result * (max(testDF$temp) - min(testDF$temp)) + min(testDF$temp)
actualvalues <- (testDF$temp) * (max(testDF$temp) -min(testDF$temp)) + min(testDF$temp)

rmse(predictions , actualvalues)  ## 90% testing data
mape(predictions, actualvalues)

plot(testDF$temp, predictions, main = "actual vs predictions " , xlab = "actual" , ylab = "predicted")  ## for 90% training data
