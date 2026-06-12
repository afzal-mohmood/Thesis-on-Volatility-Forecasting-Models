cat('\f')
rm(list = ls())
getwd()

#install.packages("readxl")
library(readxl)
library(dplyr)
library(e1071)   # Alternative package for moments (optional)

stock = read_excel("stock.xlsx")
crypto = read_excel("crypto.xlsx")


# Convert Date column to Date format
stock$Date <- as.Date(stock$Date, format = "%Y/%m/%d")
crypto$Date <- as.Date(crypto$Date, format = "%Y/%m/%d")


str(stock)
str(crypto)

# Compute log returns for all series
log_returns <- list(
  SPX = diff(log(stock$SPX)),
  IXIC = diff(log(stock$IXIC)),
  DJI = diff(log(stock$DJI)),
  NVDA = diff(log(stock$NVDA)),
  AAPL = diff(log(stock$AAPL)),
  BTC = diff(log(crypto$BTC)),
  ETH = diff(log(crypto$ETH)),
  XRP = diff(log(crypto$XRP)),
  BNB = diff(log(crypto$BNB)),
  TRX = diff(log(crypto$TRX))
)




###############################################
#### STATIONARITY CHECK########################


library(tseries)
library(rugarch)
library(FinTS)
library(e1071)

SPX = stock$SPX
IXIC = stock$IXIC
DJI = stock$DJI
NVDA = stock$NVDA
AAPL = stock$AAPL
BTC = crypto$BTC
ETH = crypto$ETH
XRP = crypto$XRP
BNB = crypto$BNB
TRX = crypto$TRX

# Define a vector of tickers to plot individually
tickers <- list(SPX, IXIC, DJI, NVDA, AAPL, BTC, ETH, XRP, BNB, TRX)
ticker_names <- c("SPX", "IXIC", "DJI", "NVDA", "AAPL", "BTC", "ETH", "XRP", "BNB", "TRX")

# Set up the plotting area (adjust the number of rows and columns)
par(mfrow = c(5, 2), mar = c(4, 4, 2, 1))  # 5 rows, 2 columns of plots

# Loop through tickers and plot each one individually
for (i in 1:length(tickers)) {
  plot(tickers[[i]], type = "l", col = "blue", 
       xlab = "Time", ylab = "Value", 
       main = paste("Average Price of", ticker_names[i]))
}


rspx = diff(log(SPX))
rixic = diff(log(IXIC))
rdji = diff(log(DJI))
rnvda = diff(log(NVDA))
raapl = diff(log(AAPL))
rbtc = diff(log(BTC))
reth = diff(log(ETH))
rxrp = diff(log(XRP))
rbnb = diff(log(BNB))
rtrx = diff(log(TRX))

######## checking STATIONARITY ADF TEST AGAIN
adf.test(rspx)
adf.test(rixic)
adf.test(rdji)
adf.test(rnvda)
adf.test(raapl)
adf.test(rbtc)
adf.test(reth)
adf.test(rxrp)
adf.test(rbnb)
adf.test(rtrx)

kpss.test(rspx)
kpss.test(rixic)
kpss.test(rdji)
kpss.test(rnvda)
kpss.test(raapl)
kpss.test(rbtc)
kpss.test(reth)
kpss.test(rxrp)
kpss.test(rbnb)
kpss.test(rtrx)
### RESULT - STATIONARY


##################################
##### CHECK FOR VOLATILITY CLUSTERING
##################################



# Function to perform ARCH-LM Test
arch_lm_test <- function(series, name, lags = 10) {
  test_result <- ArchTest(series, lags = lags)
  cat("ARCH-LM Test for", name, ":\n")
  print(test_result)
  cat("\n")
}

# Apply the ARCH-LM Test to all series
arch_results <- lapply(names(log_returns), function(name) {
  cat("\n###############################\n")
  cat("Performing ARCH-LM Test for:", name, "\n")
  cat("###############################\n")
  arch_lm_test(log_returns[[name]], name)
})


######################################
######### CHECK FOR GARCH MODEL SPECIFICATION - GARCH AND ARCH ORDER
#####################################


# Function to analyze a single series
analyze_series <- function(series, series_name) {
  cat("\n###############################\n")
  cat("Analyzing Series:", series_name, "\n")
  cat("###############################\n")
  
  # Stationarity Tests
  adf <- adf.test(series)
  kpss <- kpss.test(series)
  arch_test <- ArchTest(series, lags = 10)
  cat("ADF Test p-value:", adf$p.value, "\n")
  cat("KPSS Test p-value:", kpss$p.value, "\n")
  cat("ARCH-LM Test p-value:", arch_test$p.value, "\n")
  
  # Define ARMA and GARCH Orders
  arma_orders <- list(c(0, 0), c(1, 0), c(0, 1), c(1, 1))
  garch_orders <- list(c(1, 1), c(1, 2), c(2, 1), c(2, 2))
  distributions <- c("norm", "std", "sstd", "ged", "sged")
  
  # Initialize DataFrame for Baseline Models
  baseline_results <- data.frame(
    ARMA_Order = character(),
    GARCH_Order = character(),
    Distribution = character(),
    AIC = numeric(),
    BIC = numeric(),
    LogLikelihood = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through ARMA and GARCH combinations
  for (arma_order in arma_orders) {
    for (garch_order in garch_orders) {
      for (dist in distributions) {
        spec <- ugarchspec(
          mean.model = list(armaOrder = arma_order),
          variance.model = list(model = "sGARCH", garchOrder = garch_order),
          distribution.model = dist
        )
        fit <- tryCatch(
          ugarchfit(spec = spec, data = series, solver = "hybrid", solver.control = list(maxit = 10000)),
          error = function(e) NULL
        )
        if (!is.null(fit) && !is.null(fit@fit$LLH)) {
          log_likelihood <- fit@fit$LLH
          num_params <- length(fit@fit$coef)
          num_obs <- length(series)
          aic <- -2 * log_likelihood + 2 * num_params
          bic <- -2 * log_likelihood + num_params * log(num_obs)
          baseline_results <- rbind(baseline_results, data.frame(
            ARMA_Order = paste(arma_order, collapse = ","),
            GARCH_Order = paste(garch_order, collapse = ","),
            Distribution = dist,
            AIC = aic,
            BIC = bic,
            LogLikelihood = log_likelihood
          ))
        }
      }
    }
  }
  
  # Check if baseline_results is empty
  if (nrow(baseline_results) == 0) {
    cat("No baseline models were successfully fitted.\n")
    return(NULL)
  }
  
  # Find the best baseline model by AIC
  best_baseline_model <- baseline_results[which.min(baseline_results$AIC), ]
  cat("\nBest Baseline Model:\n")
  print(best_baseline_model)
  
  # Advanced GARCH Family Models
  advanced_specs <- list(
    "sGARCH" = ugarchspec(
      mean.model = list(armaOrder = as.numeric(unlist(strsplit(best_baseline_model$ARMA_Order, ",")))),
      variance.model = list(model = "sGARCH", garchOrder = as.numeric(unlist(strsplit(best_baseline_model$GARCH_Order, ",")))),
      distribution.model = best_baseline_model$Distribution
    ),
    "E-GARCH" = ugarchspec(
      mean.model = list(armaOrder = as.numeric(unlist(strsplit(best_baseline_model$ARMA_Order, ",")))),
      variance.model = list(model = "eGARCH", garchOrder = as.numeric(unlist(strsplit(best_baseline_model$GARCH_Order, ",")))),
      distribution.model = best_baseline_model$Distribution
    ),
    "T-GARCH" = ugarchspec(
      mean.model = list(armaOrder = as.numeric(unlist(strsplit(best_baseline_model$ARMA_Order, ",")))),
      variance.model = list(model = "fGARCH", garchOrder = as.numeric(unlist(strsplit(best_baseline_model$GARCH_Order, ","))), submodel = "TGARCH"),
      distribution.model = best_baseline_model$Distribution
    ),
    "I-GARCH" = ugarchspec(
      mean.model = list(armaOrder = as.numeric(unlist(strsplit(best_baseline_model$ARMA_Order, ",")))),
      variance.model = list(model = "iGARCH", garchOrder = as.numeric(unlist(strsplit(best_baseline_model$GARCH_Order, ",")))),
      distribution.model = best_baseline_model$Distribution
    ),
    "GARCH-M" = ugarchspec(
      mean.model = list(armaOrder = as.numeric(unlist(strsplit(best_baseline_model$ARMA_Order, ","))), archm = TRUE, archpow = 2),
      variance.model = list(model = "sGARCH", garchOrder = as.numeric(unlist(strsplit(best_baseline_model$GARCH_Order, ",")))),
      distribution.model = best_baseline_model$Distribution
    )
  )
  
  # Fit Advanced Models
  advanced_results <- data.frame(
    Model = character(),
    AIC = numeric(),
    BIC = numeric(),
    LogLikelihood = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (model_name in names(advanced_specs)) {
    spec <- advanced_specs[[model_name]]
    fit <- tryCatch(
      ugarchfit(spec = spec, data = series, solver = "hybrid", solver.control = list(maxit = 2000)),
      error = function(e) NULL
    )
    if (!is.null(fit) && !is.null(fit@fit$LLH)) {
      log_likelihood <- fit@fit$LLH
      num_params <- length(fit@fit$coef)
      num_obs <- length(series)
      aic <- -2 * log_likelihood + 2 * num_params
      bic <- -2 * log_likelihood + num_params * log(num_obs)
      advanced_results <- rbind(advanced_results, data.frame(
        Model = model_name,
        AIC = aic,
        BIC = bic,
        LogLikelihood = log_likelihood
      ))
    }
  }
  
  # Check if advanced_results is empty
  if (nrow(advanced_results) == 0) {
    cat("No advanced models were successfully fitted.\n")
    return(list(Baseline = best_baseline_model))
  }
  
  # Print Advanced Results
  cat("\nAdvanced Model Results:\n")
  print(advanced_results)
  
  # Find the Best Advanced Model
  best_advanced_model <- advanced_results[which.min(advanced_results$AIC), ]
  cat("\nBest Advanced Model:\n")
  print(best_advanced_model)
  
  return(list(Baseline = best_baseline_model, Advanced = best_advanced_model))
}



# Analyze all series
results <- lapply(names(log_returns), function(name) analyze_series(log_returns[[name]], name))

# Print results
names(results) <- names(log_returns)
results


###########################################################
########## FORECASTING ####################################
###########################################################


# Define the Best Advanced GARCH Models for each series
best_specs <- list(
  SPX = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(model = "fGARCH", garchOrder = c(1, 2), submodel = "TGARCH"),
    distribution.model = "sstd"
  ),
  IXIC = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(model = "fGARCH", garchOrder = c(1, 2), submodel = "TGARCH"),
    distribution.model = "sged"
  ),
  DJI = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "TGARCH"),
    distribution.model = "std"
  ),
  NVDA = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "TGARCH"),
    distribution.model = "std"
  ),
  AAPL = ugarchspec(
    mean.model = list(armaOrder = c(0, 1)),
    variance.model = list(model = "fGARCH", garchOrder = c(1, 2), submodel = "TGARCH"),
    distribution.model = "std"
  ),
  BTC = ugarchspec(
    mean.model = list(armaOrder = c(1, 1)),
    variance.model = list(model = "eGARCH", garchOrder = c(1, 2)),
    distribution.model = "std"
  ),
  ETH = ugarchspec(
    mean.model = list(armaOrder = c(1, 1)),
    variance.model = list(model = "iGARCH", garchOrder = c(1, 1)),
    distribution.model = "std"
  ),
  XRP = ugarchspec(
    mean.model = list(armaOrder = c(1, 1)),
    variance.model = list(model = "fGARCH", garchOrder = c(1, 2), submodel = "TGARCH"),
    distribution.model = "std"
  ),
  BNB = ugarchspec(
    mean.model = list(armaOrder = c(1, 1)),
    variance.model = list(model = "eGARCH", garchOrder = c(1, 2)),
    distribution.model = "std"
  ),
  TRX = ugarchspec(
    mean.model = list(armaOrder = c(1, 1)),
    variance.model = list(model = "eGARCH", garchOrder = c(1, 2)),
    distribution.model = "std"
  )
)

# Forecast Function
forecast_volatility <- function(series, spec, forecast_periods = 15) {
  # Fit the GARCH model
  fit <- tryCatch(
    ugarchfit(spec = spec, data = series),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    cat("Model failed to fit.\n")
    return(rep(NA, forecast_periods))
  }
  
  # Forecast volatility
  forecast <- ugarchforecast(fit, n.ahead = forecast_periods)
  sigma_forecast <- forecast@forecast$sigmaFor
  return(sigma_forecast)
}

# Forecast for All Series
forecast_results <- list()
forecast_periods <- 15

for (ticker in names(best_specs)) {
  cat("\nProcessing Ticker:", ticker, "\n")
  
  # Select the series for log returns
  series <- log_returns[[ticker]]
  
  # Forecast volatility for 15 periods
  sigma_forecast <- forecast_volatility(series, best_specs[[ticker]], forecast_periods)
  
  # Store the forecast results
  forecast_results[[ticker]] <- sigma_forecast
  
  # Print the forecasted volatility
  cat("Forecasted Volatility for", ticker, ":\n")
  print(sigma_forecast)
}



##################################################################
################## EWMA
##################################################################



# Function to calculate EWMA volatility and forecast
ewma_forecast <- function(series, lambda = 0.94, forecast_periods = 15) {
  # Calculate initial variance using the sample variance
  n <- length(series)
  ewma_var <- numeric(n)  # EWMA variance vector
  ewma_var[1] <- var(series, na.rm = TRUE)  # Initialize with sample variance
  
  # Recursive calculation of EWMA variance
  for (t in 2:n) {
    ewma_var[t] <- lambda * ewma_var[t - 1] + (1 - lambda) * series[t - 1]^2
  }
  
  # Forecast volatility for the next periods using the last variance
  last_variance <- tail(ewma_var, 1)
  forecasted_variances <- last_variance * lambda^(0:(forecast_periods - 1))
  forecasted_volatility <- sqrt(forecasted_variances)
  
  # Return the full EWMA volatility and forecasts
  return(list(
    ewma_volatility = sqrt(ewma_var), 
    forecasted_volatility = forecasted_volatility
  ))
}

# Apply EWMA to all log return series
ewma_results <- list()
forecast_periods <- 15  # Forecasting 15 periods ahead
lambda <- 0.94          # Smoothing parameter

for (ticker in names(log_returns)) {
  cat("\nProcessing Ticker:", ticker, "\n")
  
  # Apply EWMA to each series
  series <- log_returns[[ticker]]
  ewma_result <- ewma_forecast(series, lambda, forecast_periods)
  
  # Store results
  ewma_results[[ticker]] <- ewma_result
  
  # Print EWMA forecast
  cat("EWMA Forecasted Volatility for", ticker, ":\n")
  print(ewma_result$forecasted_volatility)
}



##################################################################
############### MSE and MAE
###################################################################


#out-of-sample data
stockos = read_excel("stockos.xlsx")
cryptoos = read_excel("cryptoos.xlsx")

# Convert Date column to Date format
stockos$Date <- as.Date(stockos$Date, format = "%Y/%m/%d")
cryptoos$Date <- as.Date(cryptoos$Date, format = "%Y/%m/%d")
str(cryptoos)
str(stockos)

# Compute log returns for all series
log_returnsos <- list(
  SPX = diff(log(stockos$SPX)),
  IXIC = diff(log(stockos$IXIC)),
  DJI = diff(log(stockos$DJI)),
  NVDA = diff(log(stockos$NVDA)),
  AAPL = diff(log(stockos$AAPL)),
  BTC = diff(log(cryptoos$BTC)),
  ETH = diff(log(cryptoos$ETH)),
  XRP = diff(log(cryptoos$XRP)),
  BNB = diff(log(cryptoos$BNB)),
  TRX = diff(log(cryptoos$TRX))
)


# Extract the first 15 log returns as actual volatility (since you're using log returns as volatility)
actual_volatility <- lapply(log_returnsos, function(x) x[1:15])

# Function to calculate MSE and MAE
calculate_metrics <- function(actual, forecasted) {
  mse <- mean((actual - forecasted)^2)  # Mean Squared Error
  mae <- mean(abs(actual - forecasted))  # Mean Absolute Error
  return(c(MSE = mse, MAE = mae))
}

# Calculate MSE and MAE for GARCH forecast
garch_metrics <- list()
for (ticker in names(forecast_results)) {
  actual <- actual_volatility[[ticker]]
  forecasted <- forecast_results[[ticker]]
  garch_metrics[[ticker]] <- calculate_metrics(actual, forecasted)
}

# Calculate MSE and MAE for EWMA forecast
ewma_metrics <- list()
for (ticker in names(ewma_results)) {
  actual <- actual_volatility[[ticker]]
  forecasted <- ewma_results[[ticker]]$forecasted_volatility
  ewma_metrics[[ticker]] <- calculate_metrics(actual, forecasted)
}

# Print results for both models
cat("\nGARCH Model MSE and MAE:\n")
print(garch_metrics)

cat("\nEWMA Model MSE and MAE:\n")
print(ewma_metrics)




###################################################
## creating table




# Create an empty data frame to store comparison results
comparison_df <- data.frame(
  Ticker = character(),
  GARCH_MSE = numeric(),
  GARCH_MAE = numeric(),
  EWMA_MSE = numeric(),
  EWMA_MAE = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each ticker and combine results
for (ticker in names(garch_metrics)) {
  garch_mse <- garch_metrics[[ticker]]["MSE"]
  garch_mae <- garch_metrics[[ticker]]["MAE"]
  
  # Get EWMA results
  ewma_mse <- ewma_metrics[[ticker]]["MSE"]
  ewma_mae <- ewma_metrics[[ticker]]["MAE"]
  
  # Append the results for the current ticker to the comparison dataframe
  comparison_df <- rbind(comparison_df, data.frame(
    Ticker = ticker,
    GARCH_MSE = garch_mse,
    GARCH_MAE = garch_mae,
    EWMA_MSE = ewma_mse,
    EWMA_MAE = ewma_mae
  ))
}

# Write the comparison dataframe to a CSV file
#write.csv(comparison_df, "model_comparison_results.csv", row.names = FALSE)

# Print out the comparison dataframe (optional)
print(comparison_df)




##################################################################################

##################################################################


# Calculate Relative Error for GARCH and EWMA
relative_error_garch <- abs((actual_volatility$SPX - forecast_results$SPX) / actual_volatility$SPX)
relative_error_ewma <- abs((actual_volatility$SPX - ewma_results$SPX$forecasted_volatility) / actual_volatility$SPX)

# Calculate mean relative error for each model
mean_relative_error_garch <- mean(relative_error_garch, na.rm = TRUE)
mean_relative_error_ewma <- mean(relative_error_ewma, na.rm = TRUE)

cat("Mean Relative Error for GARCH:", mean_relative_error_garch, "\n")
cat("Mean Relative Error for EWMA:", mean_relative_error_ewma, "\n")



# Define the tickers and the corresponding data (actual volatility, GARCH forecasts, and EWMA forecasts)
tickers <- names(log_returns)  # Assuming log_returns contains the tickers

# Loop through each ticker to plot Actual vs GARCH vs EWMA volatility
for (ticker in tickers) {
  # Retrieve the actual, GARCH, and EWMA volatility
  actual_vol <- actual_volatility[[ticker]]
  garch_forecast <- forecast_results[[ticker]]
  ewma_forecast <- ewma_results[[ticker]]$forecasted_volatility
  
  # Set color scheme (you can customize the colors as you like)
  colors <- c("green", "red", "blue")  # Actual, GARCH, EWMA
  
  # Plot actual volatility, GARCH forecast, and EWMA forecast for the ticker
  plot(actual_vol, type = "l", col = colors[1], xlab = "Time", ylab = "Volatility", 
       main = paste("Comparison of Actual, GARCH, and EWMA Volatility for", ticker), 
       ylim = range(c(actual_vol, garch_forecast, ewma_forecast)))  # Set y-axis range to include all data
  lines(garch_forecast, col = colors[2])  # GARCH forecast
  lines(ewma_forecast, col = colors[3])  # EWMA forecast
  
  # Add legend
  legend("bottomright", legend = c("Actual", "GARCH Forecast", "EWMA Forecast"), 
         col = colors, lty = 1)
}




#############################################################################




# Initialize empty lists to store errors for each model
forecast_error_garch_all <- list()
forecast_error_ewma_all <- list()

# Initialize empty vectors to store mean error and standard deviation
mean_error_garch_all <- numeric(length(tickers))
mean_error_ewma_all <- numeric(length(tickers))
sd_error_garch_all <- numeric(length(tickers))
sd_error_ewma_all <- numeric(length(tickers))

# Loop through each ticker to calculate forecast errors and conservatism metrics
for (i in 1:length(tickers)) {
  ticker <- tickers[i]
  
  # Retrieve actual volatility, GARCH forecast, and EWMA forecast for the current ticker
  actual_vol <- actual_volatility[[ticker]]
  garch_forecast <- forecast_results[[ticker]]
  ewma_forecast <- ewma_results[[ticker]]$forecasted_volatility
  
  # Calculate Forecast Error
  forecast_error_garch <- garch_forecast - actual_vol
  forecast_error_ewma <- ewma_forecast - actual_vol
  
  # Store the forecast errors in the lists
  forecast_error_garch_all[[ticker]] <- forecast_error_garch
  forecast_error_ewma_all[[ticker]] <- forecast_error_ewma
  
  # Calculate Mean Forecast Error and Standard Deviation of Errors for each model
  mean_error_garch_all[i] <- mean(forecast_error_garch, na.rm = TRUE)
  mean_error_ewma_all[i] <- mean(forecast_error_ewma, na.rm = TRUE)
  sd_error_garch_all[i] <- sd(forecast_error_garch, na.rm = TRUE)
  sd_error_ewma_all[i] <- sd(forecast_error_ewma, na.rm = TRUE)
  
  # Print results for each ticker
  cat("\nMean Forecast Error for GARCH for", ticker, ":", mean_error_garch_all[i], "\n")
  cat("Standard Deviation of Forecast Error for GARCH for", ticker, ":", sd_error_garch_all[i], "\n")
  cat("Mean Forecast Error for EWMA for", ticker, ":", mean_error_ewma_all[i], "\n")
  cat("Standard Deviation of Forecast Error for EWMA for", ticker, ":", sd_error_ewma_all[i], "\n")
}

# Optionally, create a summary data frame for all tickers
conservatism_df <- data.frame(
  Ticker = tickers,
  GARCH_Mean_Error = mean_error_garch_all,
  EWMA_Mean_Error = mean_error_ewma_all,
  GARCH_SD_Error = sd_error_garch_all,
  EWMA_SD_Error = sd_error_ewma_all
)

# Print the conservatism comparison data frame
print(conservatism_df)





##############################################################



# Assume you have actual_volatility and forecast_results as vectors for SPX
actual_volatility_spx <- actual_volatility$SPX  # Replace with actual volatility data for SPX
garch_forecast_spx <- forecast_results$SPX  # Replace with GARCH forecasted volatility for SPX
ewma_forecast_spx <- ewma_results$SPX$forecasted_volatility  # Replace with EWMA forecasted volatility for SPX

# Calculate Mean Forecast Error (MFE) for GARCH
mfe_garch_spx <- mean(garch_forecast_spx - actual_volatility_spx, na.rm = TRUE)

# Calculate Mean Forecast Error (MFE) for EWMA
mfe_ewma_spx <- mean(ewma_forecast_spx - actual_volatility_spx, na.rm = TRUE)

# Calculate Standard Deviation of Forecast Error (SD) for GARCH
sd_error_garch_spx <- sd(garch_forecast_spx - actual_volatility_spx, na.rm = TRUE)

# Calculate Standard Deviation of Forecast Error (SD) for EWMA
sd_error_ewma_spx <- sd(ewma_forecast_spx - actual_volatility_spx, na.rm = TRUE)

# Print Results
cat("GARCH Mean Forecast Error for SPX:", mfe_garch_spx, "\n")
cat("EWMA Mean Forecast Error for SPX:", mfe_ewma_spx, "\n")
cat("GARCH Standard Deviation of Forecast Error for SPX:", sd_error_garch_spx, "\n")
cat("EWMA Standard Deviation of Forecast Error for SPX:", sd_error_ewma_spx, "\n")

