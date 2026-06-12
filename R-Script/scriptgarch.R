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
stock$Date <- as.Date(stock$Date, format = "%d/%m/%Y")
crypto$Date <- as.Date(crypto$Date, format = "%d/%m/%Y")

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

# Function to calculate descriptive statistics
calculate_stats <- function(series) {
  data.frame(
    Mean = mean(series, na.rm = TRUE),
    Std_Dev = sd(series, na.rm = TRUE),
    Skewness = skewness(series, na.rm = TRUE),
    Kurtosis = kurtosis(series, na.rm = TRUE)
  )
}

# Calculate descriptive statistics for each log return series
stats <- lapply(log_returns, calculate_stats)
# Install and load necessary library
#install.packages("knitr")
library(knitr)
# Combine results into a single data frame
stats_table <- do.call(rbind, stats)
row.names(stats_table) <- names(log_returns)
# Generate a table with kable
kable(stats_table, caption = "Descriptive Statistics of Log Returns (Stocks and Cryptocurrencies)")


#dev.off()  # Clear previous plots
par(mfrow = c(1, 1))
plot(log_returns$SPX, type = "l", main = "Log Returns of SPX", ylab = "Log Returns", xlab = "Time")
plot(log_returns$IXIC, type = "l", main = "Log Returns of IXIC", ylab = "Log Returns", xlab = "Time")
plot(log_returns$DJI, type = "l", main = "Log Returns of DJI", ylab = "Log Returns", xlab = "Time")
plot(log_returns$NVDA, type = "l", main = "Log Returns of NVDA", ylab = "Log Returns", xlab = "Time")
plot(log_returns$AAPL, type = "l", main = "Log Returns of AAPL", ylab = "Log Returns", xlab = "Time")

# Visualize log returns

plot(log_returns$BTC, type = "l", main = "Log Returns of BTC", ylab = "Log Returns", xlab = "Time")
plot(log_returns$ETH, type = "l", main = "Log Returns of ETH", ylab = "Log Returns", xlab = "Time")
plot(log_returns$XRP, type = "l", main = "Log Returns of XRP", ylab = "Log Returns", xlab = "Time")
plot(log_returns$BNB, type = "l", main = "Log Returns of BNB", ylab = "Log Returns", xlab = "Time")
plot(log_returns$TRX, type = "l", main = "Log Returns of TRX", ylab = "Log Returns", xlab = "Time")

plot(abs(log_returns$BTC), type = "l", main = "Absolute Returns of SPX", ylab = "Absolute Returns")
plot(log_returns$BTC^2, type = "l", main = "Squared Returns of SPX", ylab = "Squared Returns")



###############################################
#### STATIONARITY CHECK########################


library(tseries)
#install.packages("rugarch")
library(rugarch)
#install.packages("FinTS")
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

## ADF Test
adf.test(SPX)
adf.test(IXIC)
adf.test(DJI)
adf.test(NVDA)
adf.test(AAPL)
adf.test(BTC)
adf.test(ETH)
adf.test(XRP)
adf.test(BNB)
adf.test(TRX)
### RESULT - NON STATIONARY

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

pp.test(rspx)
pp.test(rixic)
pp.test(rdji)
pp.test(rnvda)
pp.test(raapl)
pp.test(rbtc)
pp.test(reth)
pp.test(rxrp)
pp.test(rbnb)
pp.test(rtrx)


##################################
##### CHECK FOR VOLATILITY CLUSTERING
##################################


# Load Required Libraries
library(ggplot2)  # For visualization
library(gridExtra) # For arranging multiple plots

# Function to plot log returns and squared log returns
plot_volatility <- function(series, name) {
  par(mfrow = c(1, 2))  # Split plot window into two
  plot(series, type = "l", main = paste("Log Returns -", name),
       ylab = "Log Returns", xlab = "Time", col = "blue")
  plot(series^2, type = "l", main = paste("Squared Log Returns -", name),
       ylab = "Squared Returns", xlab = "Time", col = "red")
  par(mfrow = c(1, 1))  # Reset plot window
}

# Apply the function to all series
for (name in names(log_returns)) {
  cat("Plotting for series:", name, "\n")
  plot_volatility(log_returns[[name]], name)
}

# Function to plot ACF of squared returns
plot_acf_squared <- function(series, name) {
  acf(series^2, main = paste("ACF of Squared Returns -", name))
}

# Apply the ACF function to all series
for (name in names(log_returns)) {
  cat("ACF Plot for series:", name, "\n")
  plot_acf_squared(log_returns[[name]], name)
}


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
          ugarchfit(spec = spec, data = series, solver = "hybrid", solver.control = list(maxit = 2000)),
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
############## ADVANCED RESULTS continuing ##########################
###########################################################

# Summarize Results for All Series

# Initialize a summary data frame to store results for easy export and reporting
results_summary <- data.frame(
  Series = character(),
  Best_Baseline_Model = character(),
  Best_Advanced_Model = character(),
  Baseline_AIC = numeric(),
  Advanced_AIC = numeric(),
  stringsAsFactors = FALSE
)

# Populate the results summary from the `results` list
for (series_name in names(results)) {
  series_result <- results[[series_name]]
  
  if (!is.null(series_result$Baseline) && !is.null(series_result$Advanced)) {
    results_summary <- rbind(results_summary, data.frame(
      Series = series_name,
      Best_Baseline_Model = paste(
        "ARMA(", series_result$Baseline$ARMA_Order, 
        "), GARCH(", series_result$Baseline$GARCH_Order, 
        "), Dist: ", series_result$Baseline$Distribution, sep = ""
      ),
      Best_Advanced_Model = series_result$Advanced$Model,
      Baseline_AIC = series_result$Baseline$AIC,
      Advanced_AIC = series_result$Advanced$AIC
    ))
  }
}

# Display the summarized results
cat("\n###############################\n")
cat("Summarized Results for All Series\n")
cat("###############################\n")
print(results_summary)

# Export the summary as a CSV for reporting purposes (optional)
#write.csv(results_summary, "results_summary.csv", row.names = FALSE)

# Visualize the AIC Comparison for Baseline vs. Advanced Models
# Step 7: Visualize the AIC Comparison for Baseline vs. Advanced Models
library(ggplot2)
# Prepare data for visualization
viz_data <- results_summary %>%
  tidyr::pivot_longer(cols = c("Baseline_AIC", "Advanced_AIC"), 
                      names_to = "Model_Type", 
                      values_to = "AIC")

# Plot AIC comparison
ggplot(viz_data, aes(x = Series, y = AIC, fill = Model_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "AIC Comparison for Baseline and Advanced Models",
    x = "Series",
    y = "AIC",
    fill = "Model Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



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

# Display the Volatility Forecasts
cat("\n### Final Forecast Results ###\n")
for (ticker in names(forecast_results)) {
  cat("\nTicker:", ticker, "\n")
  print(forecast_results[[ticker]])
}

#Plot function
par(mfrow = c(1, 2))
plot(forecast_results$SPX, type = "l", main = "Forecasted Volatility for SPX", ylab = "Volatility", xlab = "Time")
plot(forecast_results$IXIC, type = "l", main = "Forecasted Volatility for IXIC", ylab = "Volatility", xlab = "Time")
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
plot(forecast_results$BTC, type = "l", main = "Forecasted Volatility for BTC", ylab = "Volatility", xlab = "Time")
plot(forecast_results$XRP, type = "l", main = "Forecasted Volatility for XRP", ylab = "Volatility", xlab = "Time")
par(mfrow = c(1, 1))


#########################################################
################ EWMA
#########################################################

# Function to calculate EWMA volatility forecast
ewma_forecast <- function(series, lambda = 0.94, forecast_periods = 15) {
  # Calculate initial variance for the series
  n <- length(series)
  ewma_var <- numeric(n)  # EWMA variance vector
  ewma_var[1] <- var(series, na.rm = TRUE)  # Initialize with the sample variance
  
  # Recursive calculation of EWMA variance
  for (t in 2:n) {
    ewma_var[t] <- lambda * ewma_var[t - 1] + (1 - lambda) * series[t - 1]^2
  }
  
  # Forecast volatility for the next periods using the last variance
  last_variance <- tail(ewma_var, 1)
  forecasted_variances <- last_variance * lambda^(0:(forecast_periods - 1))
  forecasted_volatility <- sqrt(forecasted_variances)
  
  return(forecasted_volatility)
}

# Apply EWMA Forecast to All Log Return Series
ewma_forecast_results <- list()
forecast_periods <- 15  # Forecasting 15 periods ahead
lambda <- 0.94          # Smoothing parameter

for (ticker in names(log_returns)) {
  cat("\nProcessing Ticker:", ticker, "\n")
  
  # Apply EWMA to each series
  series <- log_returns[[ticker]]
  ewma_volatility <- ewma_forecast(series, lambda, forecast_periods)
  
  # Store results
  ewma_forecast_results[[ticker]] <- ewma_volatility
  
  # Print EWMA forecast
  cat("EWMA Forecasted Volatility for", ticker, ":\n")
  print(ewma_volatility)
}

# Display Results
cat("\n### Final EWMA Forecast Results for 15 Periods ###\n")
for (ticker in names(ewma_forecast_results)) {
  cat("\nTicker:", ticker, "\n")
  print(ewma_forecast_results[[ticker]])
}



# Optionally, plot the EWMA forecast for any series
plot(ewma_forecast_results$SPX, type = "l", main = "EWMA Forecasted Volatility for SPX",
     ylab = "Volatility", xlab = "Forecast Periods", col = "blue")
plot(ewma_forecast_results$BTC, type = "l", main = "EWMA Forecasted Volatility for BTC",
     ylab = "Volatility", xlab = "Forecast Periods", col = "blue")



# Function to combine EWMA forecast results into a data frame
combine_ewma_forecast <- function(ewma_results, forecast_periods) {
  # Combine all series into a single data frame
  combined_ewma <- do.call(
    cbind,
    lapply(ewma_results, function(x) {
      # Ensure x is a numeric vector of the correct length
      if (all(length(x) < forecast_periods)) {
        x <- c(x, rep(NA, forecast_periods - length(x)))  # Pad with NAs if needed
      }
      return(x[1:forecast_periods])  # Ensure exactly 'forecast_periods' rows
    })
  )
  
  # Convert to data frame
  combined_ewma <- as.data.frame(combined_ewma)
  
  # Assign row names
  rownames(combined_ewma) <- paste0("T+", seq_len(forecast_periods))
  
  # Rename columns with ticker names
  colnames(combined_ewma) <- names(ewma_results)
  
  return(combined_ewma)
}

# Combine the EWMA results
ewma_forecast_table <- combine_ewma_forecast(ewma_forecast_results, forecast_periods)

# Save the combined forecasts to a CSV file
#write.csv(ewma_forecast_table, "ewma_forecast_volatility.csv", row.names = TRUE)

# Confirmation message
cat("\n### EWMA Forecasted Volatility Results Saved as 'ewma_forecast_volatility.csv' ###\n")




#########################################################
######### Comparing GARCH and EWMA
#########################################################


# Plot of Comparison of GARCH and EWMA Volatility Forecasts for BT

a = ewma_forecast_results$SPX
b = forecast_results$SPX
c = "Comparison of GARCH and EWMA Volatility Forecasts for SPX"

# Extract forecasts
ewma_vol <- a
garch_vol <- b

# Align forecast periods to match lengths
forecast_periods <- 1:max(length(ewma_vol), length(garch_vol))

# Adjust forecasts to match lengths
ewma_vol <- c(ewma_vol, rep(NA, length(forecast_periods) - length(ewma_vol)))
garch_vol <- c(garch_vol, rep(NA, length(forecast_periods) - length(garch_vol)))

# Plot EWMA Volatility
plot(forecast_periods, ewma_vol, type = "l", col = "blue", lwd = 2,
     main = c,
     xlab = "Forecast Periods", ylab = "Volatility", ylim = range(c(ewma_vol, garch_vol), na.rm = TRUE))

# Add GARCH Volatility
lines(forecast_periods, garch_vol, col = "red", lwd = 2)

# Add Legend
legend("right", legend = c("EWMA", "GARCH"), col = c("blue", "red"), lwd = 2)




