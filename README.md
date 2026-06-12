# Thesis-on-Volatility-Forecasting-Models
Repository of my RTU Master's Thesis evaluating GARCH-family models against EWMA specifications across traditional equities (S&amp;P 500, NASDAQ, DJI, NVDA, AAPL) and cryptocurrency markets (BTC, ETH, XRP, BNB, TRX). Features dataset processing, stationarity testing, and 15-day out-of-sample forward projections in R.


# Assessment of Volatility Forecasting Models in Financial Markets

[![R Project](https://img.shields.io/badge/Language-R-%23276DC3.svg?logo=r&logoColor=white)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This repository contains the dataset processing workflow, econometric testing frameworks, and advanced volatility forecasting scripts utilized in my Master's Thesis submitted to **Riga Technical University (RTU)**. 

The primary objective of this research is to evaluate and compare the predictive accuracy of various parametric Generalized Autoregressive Conditional Heteroskedasticity (GARCH-family) models against non-parametric Exponentially Weighted Moving Average (EWMA) specifications across traditional equity indices, technological assets, and prominent cryptocurrency markets.

---

##  Empirical Scope & Data

The analysis utilizes daily financial time-series data spanning traditional and decentralized capital markets:

* **Traditional Market Indices:** S&P 500 (`SPX`), NASDAQ-100 (`IXIC`), Dow Jones Industrial Average (`DJI`).
* **High-Beta Equities:** NVIDIA Corporation (`NVDA`), Apple Inc. (`AAPL`).
* **Cryptocurrencies:** Bitcoin (`BTC`), Ethereum (`ETH`), Ripple (`XRP`), Binance Coin (`BNB`), Tron (`TRX`).

The source spreadsheet inputs should be placed inside a localized environment folder as:
* `data/stock.xlsx` (containing traditional stock data)
* `data/crypto.xlsx` (containing cryptocurrency data)

---

##  Econometric Framework & Methodology

The pipeline follows a rigid empirical script execution layout:

1. **Descriptive Statistical Analysis:** Computation of distribution moments (Mean, Standard Deviation, Skewness, Kurtosis) on generated continuous compounded log-returns.
2. **Stationarity Verification:** Dual unit-root assessments implementing Augmented Dickey-Fuller (ADF) tests, Phillips-Perron (PP) tests, and the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test to ensure modeling suitability.
3. **Heteroskedasticity Diagnostics:** Verification of conditional variance and volatility clustering via trailing Autocorrelation Functions (ACF) on squared returns along with Engle’s ARCH-LM test.
4. **Optimal Spec Model Grid Search:** Automated programmatic parameters sweep evaluating optimal **ARMA(p,q)** conditional mean distributions combined with alternative conditional variance architectures across 5 distinct error margins (`norm`, `std`, `sstd`, `ged`, `sged`).
5. **Advanced Volatility Forecasting:** 15-day out-of-sample forward projections comparing the customized GARCH architectures against RiskMetrics EWMA modeling ($\lambda = 0.94$).

---

##  Repository Structure

```text
├── Data/
│   ├── stock.xlsx                  # Traditional stock index inputs 
│   └── crypto.xlsx                 # Cryptocurrency asset inputs
├── r-scripts/
│   └── finalfileforGARCH.R         # Core execution file 
    └── correctionforMSe.R          # Execution file for MSe  
├── Thesis.docx                     # Complete written Master's Thesis document
└── README.md                       # Repository overview documentation
```


### Prerequisites
To run the analysis pipeline locally, you will need a system running **R** or **RStudio**. 

You will also need to have the following libraries installed on your machine:
* **Data Import & Manipulation:** `readxl`, `dplyr`, `tidyr`
* **Statistical & Econometric Analysis:** `e1071`, `tseries`, `rugarch`, `FinTS`
* **Reporting & Visualization:** `knitr`, `ggplot2`

Open your R Console or RStudio and execute the command below to automatically download and install all required dependencies:

```R
install.packages(c("readxl", "dplyr", "e1071", "tseries", 
                   "rugarch", "FinTS", "knitr", "ggplot2", "tidyr"))
```
