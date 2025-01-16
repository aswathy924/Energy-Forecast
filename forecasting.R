#clear all variables 
rm(list = ls(all = TRUE))

library(forecast)
library(fpp2)

#Reading the data and adjusting it to the requirements
setwd("C:/users/visma/Downloads/")
data <- read.csv("Electric_Production.csv", header = TRUE)
data$DATE <- as.Date(data$DATE, format = "%d-%m-%Y")
Y <- ts(data$electricityproduction,start=c(1985,1),frequency = 12)
#Plotting the initial data
plot(Y,main="Electricity Production vs Time", xlab = "Time", ylab = "Electricity Production")

#Checking the Seasonality and Trend
decomposed <- stl(Y, s.window = "periodic")
seasonal_component <- decomposed$time.series[, "seasonal"]
trend_component <- decomposed$time.series[, "trend"]
plot(seasonal_component,main="Seasonal Plot")
plot(trend_component,main="Trend Plot")
#Comparing magnitudes
seasonal_magnitude <- max(seasonal_component) - min(seasonal_component)
trend_magnitude <- max(trend_component) - min(trend_component)
#Assessing prominence
if (seasonal_magnitude > trend_magnitude) {
  cat("\nThe given data has a strong seasonality\n\n")
} else {
  cat("\nThe given data has a strong trend\n\n")
}

###################################################################
#After analyzing, we concluded that the given data has strong trend
#Thus we go for :
#1.Time Series Decomposition model(STL)
#2.Exponential Smoothing model(ETS)
#3.ARIMA model
###################################################################

#Splitting data set into train data and test data for forecasting purpose
train_percent <- 0.8
train_size <- floor(length(Y) * train_percent)
train_data <- window(Y, end = c(2010, 6))  
test_data <- window(Y, start = c(2010, 7))

#1.Time Series Decomposition
decomposed<-stl(train_data, s.window="periodic")
seasonal_component <- decomposed$time.series[, "seasonal"]
trend_component <- decomposed$time.series[, "trend"]
additive_component<-trend_component + seasonal_component
summary(additive_component)
additive_forecast <- forecast(additive_component, h = 24)
plot(additive_forecast,main="Time Series Decomposition")

#2.Exponential Smoothing
ets_model <- ets(train_data)
summary(ets_model)
ets_forecast <- forecast(ets_model, h = 24)
plot(ets_forecast,main="Exponential Smoothing")

#3.ARIMA 
arima_model <- auto.arima(train_data)
summary(arima_model)
arima_forecast <- forecast(arima_model, h = 24)
plot(arima_forecast,main="ARIMA")

#Finding the best model by checking the errors in the forecast data
methods <- c("STL", "ETS", "ARIMA")
results <- data.frame(Method = character(0), RMSE = numeric(0), MAE = numeric(0))
#Displaying all the errors of the train data and test data for each models
for (method in methods) {
  # Estimate the model
  if (method == "ARIMA") {
    model <- auto.arima(train_data)
  } else if (method == "STL") {
    model <- additive_component  
  } else if (method == "ETS") {
    model <- ets(train_data)
  }
  forecast_values <- forecast(model, h = 24)
  accuracy_metrics <- accuracy(forecast_values, test_data)
  print(method)
  print(accuracy_metrics)
  rmse <-accuracy_metrics[2,"RMSE"]
  mae <- accuracy_metrics[2,"MAE"]
  
  results <- rbind(results, data.frame(Method = method, RMSE = rmse, MAE = mae))
}
#Displaying the final table with which the comparison is done
cat("\n")
print(results)

#Finding the best model which has minimum error
best_method_rmse <- results[which.min(results$RMSE), ]
best_method_mae <- results[which.min(results$MAE), ]
cat("\nBest Model (based on RMSE):")
print(best_method_rmse)
cat("\nBest Model (based on MAE):")
print(best_method_mae)


#######################################################
#Since ARIMA has the lowest errors 
#The best fitted model using Goodness of fit is ARIMA
#For more accuracy ,checking the residuals of the model 
#######################################################


final_model <- auto.arima(Y)
checkresiduals(final_model)

#Since the residuals show some abnormalities 
#Doing log transformation to the data 
transformed_Y <- log(Y)
final_model<-auto.arima(transformed_Y)
checkresiduals(final_model)


####################################################################
#After transformation,
#the autocorrelation at all lags lie between the confidence interval
#the residuals are almost normally distributed 
#Hence we can say that the ARIMA model with transformed data
#is the best fitted model 
#####################################################################


cat("\nAfter all the analysis,the best fitted model for this Electricity Production dataset is the ARIMA model\n")
cat("\nTherefore predicting the values of the production for the next 2 years")
summary(final_model)
final_forecast <- forecast(final_model, h = 24)
plot(final_forecast,main="Predicted electricity production for the next 2 years")