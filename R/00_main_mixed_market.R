
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(fGarch)
library(neuralnet)

source("R/backtesting.R")
source("R/Hypothesis_Testing.R")
source("R/Strategies/BuyHold.R")
source("R/feature_engineering.R")
source("R/Strategies/FNN.R")


## ---- main_config
#Config-------------------------------------------------------------------------

symbols   <- c("^SSMI","^GSPC","^IXIC","^GDAXI","BTC-USD","CL=F","NG=F","GC=F","SI=F","HG=F")
date_from <- "2014-01-01"
date_to   <- "2024-12-31"
date_from_to <- paste0(date_from, "::", date_to)

STRATS <- list()

PARAMS <- list(
  NN = NULL
)

max_train_date <- "2017-12-31"
max_validation_date <- "2018-12-31"

## ---- main_data
#Data--------------------------------------------------------------------------- 
getSymbols(symbols, src="yahoo", from= date_from, to=date_to, auto.assign=TRUE)

px_list <- list(
  "SSMI"  = `SSMI`,
  "GSPC"  = `GSPC`,
  "IXIC"  = `IXIC`,
  "GDAXI" = `GDAXI`,
  "BTC-USD"= `BTC-USD`,
  "CL=F"   = `CL=F`,
  "NG=F"   = `NG=F`,
  "GC=F"   = `GC=F`,
  "SI=F"   = `SI=F`,
  "HG=F"   = `HG=F`
)
rm(SSMI, GSPC, IXIC, GDAXI, `BTC-USD`, `CL=F`, `NG=F`, `GC=F`, `SI=F`, `HG=F`)


px_list <- lapply(px_list, function(x) na.approx(x, maxgap=5))



#lapply(px_list, function(x)any(is.na(c)))         
px_list$`GC=F`$`GC=F.Adjusted`[px_list$`GC=F`$`GC=F.Adjusted`<= 0]<-0.01

#saveRDS(px_list, file = "R/data/px_list_mixed.rds")

# --- feature engineering
# Feature Engineering-----------------------------------------------------------
# NOT SURE IF DIFFERENT FOR EACH STRATEGY
FEATS <- list(SSMI = feature_eng(data = px_list$SSMI, lags = 5, train_split = max_train_date),
              GSPC = feature_eng(data = px_list$GSPC, lags = 5, train_split = max_train_date),
              IXIC = feature_eng(data = px_list$IXIC, lags = 5, train_split = max_train_date),
              GDAXI = feature_eng(data = px_list$GDAXI, lags = 5, train_split = max_train_date),
              `BTC-USD` = feature_eng(data = px_list$`BTC-USD`, lags = 5, train_split = max_train_date),
              `CL=F` = feature_eng(data = px_list$`CL=F`, lags = 5, train_split = max_train_date),
              `NG=F` = feature_eng(data = px_list$`NG=F`, lags = 5, train_split = max_train_date),
              `GC=F` = feature_eng(data = px_list$`GC=F`, lags = 5, train_split = max_train_date),
              `SI=F`= feature_eng(data = px_list$`SI=F`, lags = 5, train_split = max_train_date),
              `HG=F` = feature_eng(data = px_list$`HG=F`, lags = 5, train_split = max_train_date)
              )



## ---- main_signals
#indicators and signals---------------------------------------------------------
STRATS$BH <- lapply(px_list, strat_bh)
STRATS$NN <- lapply(FEATS, function(x){simple_FNN(x, max_train_date, max_validation_date)})

simple_FNN(feats = FEATS$SSMI, train_split = max_train_date, valid_split = max_validation_date)

## ---- main_backtests
# Backtests ---------------------------------------------------------------

BACKT <- list(BH = lapply(STRATS$BH, function(x){backtest_log_ret(x)}))


#For comparability we want all TS to have intersecting dates
all_series <- unlist(BACKT, recursive = FALSE)
common_idx <- Reduce(intersect, lapply(all_series, function(x) as.Date(index(x))))
BACKT <- lapply(BACKT, function(group) lapply(group, function(x) x[common_idx]))
rm(all_series)


## ---- main_eval
# Create Portfolio -------------------------------------------------------      

PORTF <- lapply(BACKT, create_portfolio)



# Evaluate Backtest Index---------------------------------------------------

EVAL_BT <- list(
  BH = lapply(BACKT$BH, eval_backtest)
)

EVAL_PORTF <- list(
  BH = lapply(PORTF$BH, eval_backtest)
)

# Extract Sharpe ratios and Drawdowns into matrices ------------------------ 

mat_sharpe <- sapply(EVAL_BT, function(x) sapply(x, function(y) y$Sharpe))
mat_drawdown <- sapply(EVAL_BT, function(x) sapply(x, function(y) y$MaxDrawdown))

# Add portfolio rows
mat_sharpe <- rbind(mat_sharpe,
                    sapply(EVAL_PORTF, function(x) sapply(x, function(y) y$Sharpe)))
rownames(mat_sharpe)[nrow(mat_sharpe)] <- "portfolio"

mat_drawdown <- rbind(mat_drawdown,
                      sapply(EVAL_PORTF, function(x) sapply(x, function(y) y$MaxDrawdown)))
rownames(mat_drawdown)[nrow(mat_drawdown)] <- "portfolio"




# Hypothesis Testing  Backtest Index-----------------------------------------
STD_RET <- standardized_returns(
  returns = BACKT,
  portf   = PORTF
)

grid <- expand.grid(A = names(STD_RET), B = names(STD_RET), stringsAsFactors = FALSE)
grid <- subset(grid, A != B)

ALL_TVALS_DIR <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  A <- grid$A[i]; B <- grid$B[i]
  out <- paired_t_value(STD_RET[[A]], STD_RET[[B]])
  transform(out, strat_A = A, strat_B = B)
}))

