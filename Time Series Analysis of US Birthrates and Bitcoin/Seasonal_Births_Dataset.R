library(TSA)
library(tidyverse)
library(dplyr)
library(tseries)
library(lubridate)
library(forecast)
library(rugarch)
library(MTS)

#import births dataset
births <- read.csv("C:/Users/sarah/OneDrive/Desktop/Grad School/MA-641 (Time Series)/Group Project/BirthMonthsData.csv/BirthMonthsData.csv", header=TRUE)

#look at data
head(births,10)
str(births)

#check for any missing values
na_count <- sapply(births, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
print(na_count)

#Split data to only look at USA
usa <- births[births$Country.or.Area=="United States of America",c("Year","Month","Number_of_Births")]

#Remove "Total" rows from data since it was summing up the total births per year and including it in the time series as if it was another month
usa_births <- usa[-(which(usa$Month %in% "Total")),]
head(usa_births,20)

#histogram
hist(usa_births$Number_of_Births, main = "Histogram of USA births", xlab = "Number of Births")

#Convert to time series
usa_births_ts <-ts(usa_births$Number_of_Births, frequency = 12, start = c(1969))

#Basic Stats
summary(usa_births_ts)
sd(usa_births_ts)

#Time Series Plot of the data
plot(usa_births_ts, type='l', ylab="Number of Births", main="Time Series - Births per Year: USA")

#check for seasonality
decomposed_data <- decompose(usa_births_ts)
plot(decomposed_data)

#Check for stationarity with time series plot with labels, and ACF and PACF plots
#win.graph(width=20, height=10,pointsize=8) #- this is for R studio
plot(usa_births_ts, type='l', ylab="Number of Births", main="Time Series - Births per Year: USA")
points(y=usa_births_ts, x=time(usa_births_ts), pch=as.vector(season(usa_births_ts)))

#ACF, PACF, EACF Check function
corr_plots <- function(data, max_lag, title){
  acf.func <- acf(data, lag.max = max_lag, plot = FALSE)
  plot(acf.func, main = paste(title, "~ ACF"))
  
  pacf.func <- pacf(data, lag.max = max_lag, plot = FALSE)
  plot(pacf.func, main = paste(title, "~ PACF"))
  
  eacf(data)
}

#ACF, PACF, and EACF plots for USA births dataset
corr_plots(usa_births_ts, 72, "USA Births Time Series")

#Note: Null Hypothesis of Augmented Dickey-Fuller test is that the ts is non-stationary
adf.test(usa_births_ts)

#plots show non-stationarity and adf test confirms.
#This means the data will need to be made stationary

#use ndiff and nsdiffs to determine number of seasonal and non-seasonal differences needed
ndiffs(usa_births_ts,alpha=0.05)
nsdiffs(usa_births_ts,alpha = 0.05)

#Take first seasonal difference of data and check for stationarity
seasonal_diff <- diff(usa_births_ts,12)
plot(seasonal_diff, type='l', ylab="Number of Births", main="Time Series - Seasonal Difference")

adf.test(seasonal_diff)
corr_plots(seasonal_diff, 300, "Seasonal Difference")

#Augmented Dickey-Fuller Test indicates that taking seasonal difference once
#makes the time series stationary

#Sugested models: SARIMA(2,1,3)12, SARIMA(3,1,3)12, SARIMA(4,1,4)12, SARIMA(5,1,5)12

#check for seasonality of diff data
nsdiffs(seasonal_diff,alpha = 0.05)
ndiffs(seasonal_diff, alpha = 0.05)

#Model comparison and testing

sarima_model_test <- function(model_name, ts_data, p, d, q, P, D, Q, s){
  model_name = Arima(ts_data, order = c(p,d,q), seasonal = list(order=c(P,D,Q), period=s), method = "ML")
  return(model_name)
}

#Run different models for parameter estimation
m1.births <- sarima_model_test(m1.births, seasonal_diff, 0, 1, 0, 2, 0, 3, 12)
m2.births <- sarima_model_test(m2.births, seasonal_diff, 0, 1, 0, 3, 0, 3, 12)
m3.births <- sarima_model_test(m3.births, seasonal_diff, 0, 1, 0, 4, 0, 4, 12)
m4.births <- sarima_model_test(m4.births, seasonal_diff, 0, 1, 3, 4, 0, 4, 12)
m5.births <- sarima_model_test(m5.births, seasonal_diff, 3, 0, 0, 2, 1, 3, 12)
m6.births <- sarima_model_test(m6.births, seasonal_diff, 3, 0, 0, 3, 1, 3, 12)
m7.births <- sarima_model_test(m7.births, seasonal_diff, 0, 1, 0, 5, 1, 5, 12)
m8.births <- sarima_model_test(m8.births, seasonal_diff, 0, 0, 3, 5, 1, 5, 12)
m9.births <- sarima_model_test(m9.births, seasonal_diff, 0, 0, 5, 5, 1, 5, 12)

#Compare models
m1.births
m2.births
m3.births
m4.births
m5.births
m6.births
m7.births
m8.births
m9.births

#Best model so far is model 4 ARIMA(0,1,3)xSARIMA(4,0,4)[12] with AIC = 11592.63
#residual check of best model
checkresiduals(m4.births)
tsdiag(m4.births, gof = 30, omit.initial = FALSE)
#Looks like a better model can be found

#FOR LOOP MODEL SEARCH CAN BE IMPROVED
#Loop to find best model within identified range

best.aic = Inf  # Set to Inf for a more general initial value
p.best = 0
q.best = 0
P.best = 0
Q.best = 0
best.model = NULL  # Initialize a variable to store the best model

for (p in 0:5) {
  for (q in 0:5) {
    for (P in 0:5) {
      for (Q in 0:5) {
        skip_to_next <- FALSE
        tryCatch({
          model <- sarima_model_test(m1.births, seasonal_diff, p, 0, q, P, 1, Q, 12)
          if(model$aic<best.aic){
            best.aic = model$aic
            p.best = p
            q.best = q
            P.best = P
            Q.best = Q
            best.model = model  # Save the current best model
          }
        }, error = function(e){
          skip_to_next <<- TRUE
        })
        if(skip_to_next) {next}
      }
    }
  }
}

cat("Best model identified from loop is ARIMA(", p.best, ",", 0, ",", q.best, ")xSARIMA(", P.best, ",", 1, ",", Q.best, ")[12] with AIC =", best.aic, "\n")


#Best model for 1 non-seasonal difference identified from loop is 
#ARIMA(4,1,5)xSARIMA(4,0,4)[12] with AIC = 11275.89

#Best model for 1 non-seasonal difference identified from loop is 
#ARIMA(3,0,5)xSARIMA(5,1,5)[12] with AIC = 11150.65

#Best model found from "grid search" of models
best.model <- sarima_model_test(best.model, seasonal_diff, 3, 0, 5, 5, 1, 5, 12)
best.model$aic

tsdiag(best.model, gof = 30, omit.initial = FALSE)
#GARCH model attempt
#log difference transform
log_usa <- diff(log(usa_births_ts),12)

#square and absolute values of log difference transform
square_log <- log_usa^2
abs_log <- abs(log_usa)

#Time series plots of GARCH values
plot(square_log, type='l', ylab="Number of Births", main="Time Series - Squared GARCH Values")
plot(abs_log, type='l', ylab="Number of Births", main="Time Series - Absolute GARCH Values")

#Squared value GARCH

#ACF, PACF, EACF of square log
corr_plots(square_log, 72, "Squared Log: GARCH")

#ACF, PACF, EACF of absolute log
corr_plots(square_log, 72, "Absolute Value Log: GARCH")

#GARCH model function
sgarch_model_test <- function(model_name, fit_name, ts_data, P, Q, p, q, sample_num){
  model_name <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(P,Q)), mean.model = list(armaOrder=c(p,q)))
  fit_name <- ugarchfit(data = ts_data, spec = model_name, out.sample = sample_num)
}

#GARCH models
sqgarch1 <- sgarch_model_test(sqgarch1, fit_sqgarch1, square_log, 1, 4, 0, 0, 10)
sqgarch2 <- sgarch_model_test(sqgarch2, fit_sqgarch2, square_log, 2, 4, 0, 0, 10)
sqgarch3 <- sgarch_model_test(sqgarch3, fit_sqgarch3, square_log, 3, 3, 0, 0, 10)
absgarch1 <- sgarch_model_test(absgarch1, fit_absgarch1, abs_log, 1, 1, 0, 0, 10)

#GARCH Model Comparison
sqgarch1
sqgarch2
sqgarch3
absgarch1

#Residual Analysis 

win.graph(width=20, height=10,pointsize=8)
plot(rstandard(best.model),ylab='Standardized Residuals', type='o',
     main=expression(Residuals~~from~~the~~ARIMA(list(3,0,5))%*%(list(5,1,5))[12]~~Model))
abline(h=0)

acf(rstandard(best.model), 72)
hist(rstandard(best.model),xlab='Standardized Residuals',
     main=expression(Residuals~~from~~the~~ARIMA(list(3,0,5))%*%(list(5,1,5))[12]~~Model))

win.graph(width=8, height=8,pointsize=8)
qqnorm(window(rstandard(best.model),start=c(1970,2)),main=expression(Normal~~Q-Q~~Plot))
title(main=expression(Residuals~~from~~the~~ARIMA(list(3,0,5))%*%(list(5,1,5))[12]~~Model),
      line=3)
qqline(window(rstandard(best.model),start=c(1970,2)))

#Null Hypothesis for Shapiro-Wilk is data is normal
shapiro.test(rstandard(best.model))

#Note: Null Hypothesis for Ljung-Box test is autocorrelation for chosen lags are all 0
Box.test(rstandard(best.model))
tsdiag(best.model, gof = 30, omit.initial = FALSE)

#Forecasting
#Original usa births data fitted with best model parameters 
final.model <- Arima(usa_births_ts, order = c(3,0,5), seasonal = list(order=c(5,1,5), period=12))

#Forescast

final.model%>%
  forecast(h=12*5) %>%
  autoplot()
