### installing packages and library
install.packages('forecast')
library('forecast')
library('tseries')
library('ggplot2')

### Plotting the gas time series data
gas.ts1 = ts(gas, start=c(1956,1), end=c(1995,8), frequency=12)
View(gas.ts1)
plot(gas.ts1)

### Decomposition of the data
gas.decom = stl(gas.ts1, s.window = "periodic")
gas.decom
plot(gas.decom)

### Checking stationarity
adf.test(gas.ts1, alternative = "stationary")

### ACF and PACF plot
acf(gas.ts1, lag.max = 30)
pacf(gas.ts1, lag.max = 20)


### Removing seasonal component
deseasonal_gas.ts1 = seasadj(gas.decom)
plot(deseasonal_gas.ts1)

### Differencing time series data
gas.ts1_df = diff(deseasonal_gas.ts1, differences = 2)
plot(gas.ts1_df)
adf.test(gas.ts1_df, alternative = "stationary")

### ACF and PACF plot on difference data
acf(gas.ts1_df)
acf(gas.ts1_df, lag.max = 25, main = 'ACF for difference data')
pacf(gas.ts1_df, lag.max = 25, main = 'PACF for difference data')

### Splitting the time series data into training and test data sets
gas.train = window(deseasonal_gas.ts1, start = c(1970,1), end = c(1993,12))
gas.test = window(deseasonal_gas.ts1, start = c(1994,1))
gasARIMA = arima(gas.train, order = c(12,2,5))
gasARIMA
tsdisplay(residuals(gasARIMA), lag.max = 20, main = 'Residual Model')

### Fitting with auto arima
gas.autoarima = auto.arima(gas.train, seasonal = FALSE)
gas.autoarima
gas.autoarima1 = auto.arima(gas.train, stationary = TRUE)
gas.autoarima1
tsdisplay(residuals(gas.autoarima), lag.max = 25, main = 'Auto arima residual model')

### Ljung box test
# H0 = Residuals are independent
# Ha = Residuals are not independent

Box.test(gasARIMA)
Box.test(gas.autoarima)

### Forecasting with ARIMA model
fcast = forecast(gasARIMA, h = 12)
plot(fcast)

fcast1 = forecast(gas.autoarima, h = 12)
plot(fcast1)

fcast2 = forecast(gas.autoarima1, h = 12)
plot(fcast2)

fit1 = auto.arima(gas.ts1, seasonal = FALSE)
fcast3 = forecast(fit1, h = 12)
plot(fcast3)

### Accuracy of the forecast
accu1 = forecast(gasARIMA)
accuracy(accu1, gas.test)
accu2 = forecast(gas.autoarima)
accuracy(accu2, gas.test)
accu3 = forecast(gas.autoarima1)
accuracy(accu3, gas.test)
