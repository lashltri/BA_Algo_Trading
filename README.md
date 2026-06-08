# BA_Algo_Trading

This repository contains the R code for my bachelor thesis on neural-network-based trading strategies. The project evaluates whether neural-network models can improve out-of-sample trading performance compared with Buy and Hold and classical rule-based benchmark strategies.

The analysis is based on a rolling out-of-sample backtesting framework and compares several strategy types across equity indices, commodities, Bitcoin, and an equally weighted multi-asset portfolio.

## Project Overview

The main research question is whether neural-network-based trading strategies can improve trading performance compared with:

1. Buy and Hold,
2. each other, and
3. classical benchmark strategies from the preceding project thesis.

The repository focuses on three main neural-network-based approaches:

- **Simple NN**: a feedforward neural network that directly predicts next-day returns.
- **ARMA-NN**: a hybrid model combining ARMA-GARCH time-series structure with a neural-network residual forecast.
- **LPD-ELM-NN**: a sensitivity-based model using Linear Parameter Data (LPD) and an Extreme Learning Machine-style final-layer refit for risk-management-based market exposure control.

## Repository Structure

The repository is structured around the main R scripts, strategy implementations, and supporting functions.

- `R/00_main_mixed_market.R`: main mixed-market evaluation
- `R/00_main_crisis_market.R`: crisis-period evaluation
- `R/feature_engineering.R`: feature construction
- `R/Rolling_Framework.R`: rolling-window backtesting framework
- `R/eval_utils.R`: performance evaluation functions
- `R/Hypothesis_Testing.R`: statistical comparison procedures
- `R/Strategies/`: implemented trading strategies
- `R/Strategies/nn_functions/`: neural-network helper functions

## Main Scripts

### `00_main_mixed_market.R`

Runs the main mixed-market evaluation. This includes downloading financial market data, constructing features, running the rolling-window backtests, evaluating strategy performance, creating portfolio-level results, and preparing standardized returns for hypothesis testing.

The main market universe includes equity indices, Bitcoin, energy commodities, and metals.

### `00_main_crisis_market.R`

Runs the crisis-period evaluation for the global financial crisis period. Bitcoin is excluded from this setting because BTC-USD data is not available for the full 2007–2010 crisis period.

## Strategy Files

### `BuyHold.R`

Implements the Buy and Hold benchmark strategy.

### `simpleFNN.R`

Implements the simple feedforward neural-network trading strategy.

### `ARMA_NN.R`

Implements the hybrid ARMA-GARCH neural-network strategy. The model combines a classical time-series component with a neural-network residual forecast.

### `LPD_NN.R`

Implements the LPD-based neural-network strategies. These include variants with and without ELM-style final-layer refitting and with or without the neural-network trading signal.

## Supporting Files

### `feature_engineering.R`

Creates the input features used by the models, including lagged returns, rolling volatility, rolling means, ARMA-based components, and GARCH-based volatility features.

### `Rolling_Framework.R`

Contains the rolling-window backtesting framework. The framework trains models on historical windows and evaluates their out-of-sample trading performance.

### `eval_utils.R`

Contains helper functions for evaluating trading performance, including return preparation, portfolio construction, Sharpe ratios, adjusted Sharpe ratios, drawdowns, exposure, and trading activity.

### `Hypothesis_Testing.R`

Contains functions for standardized return comparisons and paired statistical tests between strategies.

## Required R Packages

The main scripts use the following R packages:

- `quantmod`
- `PerformanceAnalytics`
- `lubridate`
- `fGarch`
- `neuralnet`

Install them in R with:

    install.packages(c(
      "quantmod",
      "PerformanceAnalytics",
      "lubridate",
      "fGarch",
      "neuralnet"
    ))

## How to Run

Clone the repository and open the project directory in R or RStudio.

Then run the main mixed-market script:

    source("R/00_main_mixed_market.R")

or, for the crisis evaluation:

    source("R/00_main_crisis_market.R")

The scripts download market data using Yahoo Finance through `quantmod`, construct the feature sets, run the backtests, and compute the evaluation results.

## Methodology

The project uses a rolling out-of-sample framework. Models are trained on historical data and then evaluated on future periods. This structure is used to reduce look-ahead bias and to make the evaluation closer to a realistic forecasting and trading setting.

The main performance metrics include:

- annualized Sharpe ratio
- exposure-adjusted Sharpe ratio
- maximum drawdown
- mean market exposure
- trading activity


## Notes

This repository contains research code written for a bachelor thesis. The code is intended for academic analysis and reproducibility of the thesis results. It is not financial advice and should not be used as a live trading system without further validation, robustness checks, and transaction cost modelling.

## Author

Trinity Lashley

Bachelor Thesis – Algorithmic Trading with Neural Networks
