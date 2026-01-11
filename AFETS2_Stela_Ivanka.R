rm(list=ls())

setwd("C:\\Users\\HP\\Documents\\AFETS\\AFETS_2\\work")
install.packages("tseries")
install.packages("forecast")
install.packages("ggplot2")
library(tseries) 
library(forecast) 
library(ggplot2)

data_url <- "https://github.com/febse/data/raw/refs/heads/main/ts/SOFIX-Stocks.xlsx"

install.packages("httr")
install.packages("readxl")
library(readxl)
library(httr)

#loading the data
data_url <- "https://github.com/febse/data/raw/refs/heads/main/ts/SOFIX-Stocks.xlsx"
GET(data_url, write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)


#cleaning + differencing
y_clean <- na.omit(y[y > 0]) 
log_returns <- diff(log(y_clean))

log_y <- log(y)
log_returns <- diff(log_y)

plot.ts(log_returns, 
        main="Logarithmic Returns", 
        ylab="Returns", 
        col="black")
abline(h = 0, col = "red", lty = 2)

adf_final <- adf.test(log_returns)
print(adf_final)

#diagrams on the columns
y <- na.omit(df[[4]]) 
y_ts <- ts(y)
plot.ts(y_ts, main="Price Series", ylab="Level", col="blue")

#ACF + PACF
acf(y_ts, main="ACF for Price Level")
pacf(y_ts, main="PACF for Price Level")

# ARIMA
ARMA_110 <- Arima(y_ts, order = c(1, 1, 0))
ARMA_011 <- Arima(y_ts, order = c(0, 1, 1))
ARMA_111 <- Arima(y_ts, order = c(1, 1, 1))

print(ARMA_110)
print(ARMA_011)
print(ARMA_111)

#Heteroscedasticity - yes
res <- residuals(ARMA_011)
plot(res^2, type="h", main="Squared Residuals (Test for Heteroscedasticity)", 
     ylab="Squared Errors", col="red")

#dividing the data into est set and test set
y_est <- na.omit(df[[3]]) 
y_test <- na.omit(df[[20]]) 
y_est_ts <- ts(y_est)
y_test_ts <- ts(y_test)

#estimation of the arima models
est_110 <- Arima(y_est_ts, order = c(1, 1, 0))
est_011 <- Arima(y_est_ts, order = c(0, 1, 1))
est_111 <- Arima(y_est_ts, order = c(1, 1, 1))

#AIC + BIC
comparison <- data.frame(
  Model = c("ARMA(1,1,0)", "ARMA(0,1,1)", "ARMA(1,1,1)"),
  AIC = c(est_110$aic, est_011$aic, est_111$aic),
  BIC = c(est_110$bic, est_011$bic, est_111$bic)
)
print(comparison)

#Best Candidate Model
best_model_name <- comparison$Model[which.min(comparison$AIC)]
cat("\nBest Candidate Model (Lowest AIC):", best_model_name, "\n")

# ACF of residuals BCM
best_residuals <- residuals(est_011)
Acf(best_residuals, main="ACF of Residuals (Model Adequacy Check)")

#Ljung-Box Test
Box.test(best_residuals, lag = 10, type = "Ljung-Box", fitdf = 2)

s <- spec.pgram(log_returns, spans = c(3,3), plot = FALSE)

log_returns_clean <- na.omit(log_returns)

# Plotting the Magnitude Specter
s <- spec.pgram(log_returns_clean, spans = c(3,3), plot = TRUE, 
                main = "Magnitude Specter (Periodogram)", 
                xlab = "Frequency (cycles per day)", 
                ylab = "Magnitude/Power")

#Fit a regression model with seasonal patterns
fourier_terms <- fourier(ts(y_est, frequency = 5), K = 2)
seasonal_reg <- lm(y_est ~ fourier_terms)
summary(seasonal_reg)

# BCM forecast vs comparison model
fc_best <- forecast(est_011, h = length(y_test_ts))
fc_other <- forecast(est_110, h = length(y_test_ts))

print(fc_best)
print(fc_other)

# actual values Vs forecast
plot(y_test_ts, main="Actual vs Forecasted Prices", 
     ylab="Price", col="black", lwd=2)
lines(fc_best$mean, col="blue", lwd=2)
lines(fc_other$mean, col="red", lwd=2)

