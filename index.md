## WELCOME TO INAYAH CHAUDHARY

**Student & Innovator:**
The following showcases projects, ideas, and achievements 

### Project: Stock Market Forecasting 

Using R and data extracted from Yahoo Finance, I used ARIMA modelling to forecast stock market prices using the SPY (S&P 500 ETF Trust) market. I used SPY an ETF as the stock ticker symbol and a data range of 5 years. I created a trajectory and was able to establish whether the trend indicates if its a good time to buy, hold, or sell. 

```markdown

##########################
#####Log residuals stock price forecasting method#####
##########################

##install packages
install.packages("timeSeries")
install.packages("quantmod")
install.packages("tseries")
install.packages("xts")
install.packages("forecast")


##load data + load libraries
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)


##pull data from Yahoo Finance 
getSymbols('SPY', from='2017-06-18', to='2022-06-18', verbose=TRUE)

##class is xts/zoo
class(`SPY`)

##find days close price for each trading day
SPY_Close_Prices = SPY[,4]

##plot data 
par(mfrow=c(1,1))
plot(SPY_Close_Prices)

##class is xts/zoo
class(SPY_Close_Prices)

##plot the data and get initial auto arima pdq values 
par(mfrow=c(1,2))
Acf(SPY_Close_Prices, main='ACF for Differenced Series')
Pacf(SPY_Close_Prices, main='PACF for Differenced Series')
auto.arima(SPY_Close_Prices, seasonal=FALSE) ##pdq = (5,1,2)
##from plots of ACF and PACF we get 1, 1, 1 for pdq


##log residuals to remove non-stationary properties
##to make data more stable compute the log returns for the stock
logs= diff(log(SPY_Close_Prices), lag=1)
logs=logs[!is.na(logs)]

##plot log returns for more accurate forecasting (eliminates non-stationary areas)
par(mfrow=c(1,1))
plot(logs, type='l', main='Log Returns Plot')

##ADF (augmented Dickey-Fuller test) test for p-value
print(adf.test(logs)) ##p-value < 0.01

auto.arima(logs, seasonal=FALSE) ##AIC/BIC = -7435/-7420
str(logs) ##xts object

##split dataset into two (80/20 training and testing)
sample_size = floor(0.80 * nrow(logs))
set.seed(108) ##to allow for reproducability 
train_indices <- sample(seq_len(nrow(logs)), size= sample_size)

train <- logs[train_indices, ]
test <- logs[-train_indices, ]

par(mfrow=c(1,2))
Acf(train, main='ACF for Differenced Series')
Pacf(train, main='PACF for Differenced Series')
auto.arima(train, seasonal=FALSE) ##pdq = 2, 0, 0 
##AIC/BIC = -5997/-5982 ##from plots of ACF and PACF pdq = 7, 0, 19

##look at residuals for the autoarima and custom arima models based on pdq values
fit1 <- auto.arima(train, seasonal=FALSE) # autoarima (2, 0, 0)
tsdisplay(residuals(fit1), lag.max=40, main='(2, 0, 0) Model Residuals')

fit2 <- arima(train, order=c(7, 0, 19)) # customarima (1, 0, 1)
tsdisplay(residuals(fit2), lag.max=40, main='(7, 0, 19) Model Residuals')

##original data without logs of returns
fit3 = auto.arima(SPY_Close_Prices, seasonal=FALSE)
tsdisplay(residuals(fit3), lag.max=40, main='Original, Non-Log returns Model Residuals')

##custom arima fit from fit2 applied to original dataset
fit4 = arima(SPY_Close_Prices, order=c(6,0,19)) ##change others to (6, 0, 19)
  
tsdisplay(residuals(fit4), lag.max=40, main='(6,0,19) Model Residuals on Original Dataset')

##plots of all arima models
par(mfrow=c(2,2))
##auto arima 
period <- 100
fcast1 <- forecast(fit1, h=period)
plot(fcast1)
##custom arima 
fcast2 <- forecast(fit2, h=period)
plot(fcast2)
##original non-log returns data
fcast3 <- forecast(fit3, h=period)
plot(fcast3)
##custom arima applied to original non-log returns dataset
fcast4 <- forecast(fit4, h=period)
plot(fcast4)

##closer look at auto arima on original dataset
par(mfrow=c(1,2))
plot(fcast3)
plot(fcast4)

##look at accuracy
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)

##look at actual forecasted values on original dataset(fcast3)
#1 auto arima 
fcast3
#2 custom arima 
fcast4

##Conclusions
#We have a high accuracy prediction with a 0% increase on the auto arima and 
# approximately 20% decrease in the custom arima. Overall there is a strong 
#projection of a 5%-10% loss over the next 100 days in the SPY ETF and overall 
# stock market. Due to the ongoing war and poor economic state globally, these 
#predictions are not exact prices, instead these conclusions should be used in 
#a directional manner.

###################################

```

### Contact & Information
i5chaudh@uwaterloo.ca
