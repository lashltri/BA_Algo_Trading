
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(fGarch)
library(neuralnet)
#detach("package:dplyr", unload = TRUE)

source("R/eval_utils.R")
source("R/Hypothesis_Testing.R")
source("R/Strategies/BuyHold.R")
source("R/feature_engineering.R")
source("R/Rolling_Framework.R")
source("R/Strategies/simpleFNN.R")
source("R/Strategies/LPD_NN.R")
source("R/Strategies/ARMA_NN.R")
source("R/Strategies/nn_functions/neuralnet_functions.R")
source("R/Strategies/nn_functions/quantile_computation_for_risk_management_with LPD_QPD.R")

options(scipen = 999)
set.seed(123)
## ---- main_config
#Config-------------------------------------------------------------------------

symbols   <- c("^SSMI","^GSPC","^IXIC","^GDAXI","CL=F","NG=F","GC=F","SI=F","HG=F")
date_from <- "2000-01-01"
date_to   <- "2009-12-31"
date_from_to <- paste0(date_from, "::", date_to)

STRATS <- list()

FEATS <- list()

max_train_date <- "2006-12-31"

## ---- main_data
#Data--------------------------------------------------------------------------- 
getSymbols(symbols, src="yahoo", from= date_from, to=date_to, auto.assign=TRUE)

px_list <- list(
  "SSMI"  = `SSMI`,
  "GSPC"  = `GSPC`,
  "IXIC"  = `IXIC`,
  "GDAXI" = `GDAXI`,
  "CL=F"   = `CL=F`,
  "NG=F"   = `NG=F`,
  "GC=F"   = `GC=F`,
  "SI=F"   = `SI=F`,
  "HG=F"   = `HG=F`
)
rm(SSMI, GSPC, IXIC, GDAXI, `CL=F`, `NG=F`, `GC=F`, `SI=F`, `HG=F`)


px_list <- lapply(px_list, function(x) na.approx(x, maxgap=5))



#lapply(px_list, function(x)any(is.na(c)))         
px_list$`GC=F`$`GC=F.Adjusted`[px_list$`GC=F`$`GC=F.Adjusted`<= 0]<-0.01
px_list_crisis <- px_list
#save(px_list_crisis, file = "R/data/px_list_crisis.Rdata")

# --- feature engineering
# Feature Engineering-----------------------------------------------------------
# NOT SURE IF DIFFERENT FOR EACH STRATEGY
FEATS$FULL <- list(SSMI = feature_eng(data = px_list$SSMI, train_split = max_train_date),
              GSPC = feature_eng(data = px_list$GSPC, train_split = max_train_date),
              IXIC = feature_eng(data = px_list$IXIC, train_split = max_train_date),
              GDAXI = feature_eng(data = px_list$GDAXI, train_split = max_train_date),
              `CL=F` = feature_eng(data = px_list$`CL=F`, train_split = max_train_date),
              `NG=F` = feature_eng(data = px_list$`NG=F`, train_split = max_train_date),
              `GC=F` = feature_eng(data = px_list$`GC=F`, train_split = max_train_date),
              `SI=F`= feature_eng(data = px_list$`SI=F`, train_split = max_train_date),
              `HG=F` = feature_eng(data = px_list$`HG=F`, train_split = max_train_date)
              )


FEATS$LPD <- lapply(FEATS$FULL, function(x) x[, c("rt_lag0", "rt_lag1",
                                                      "rt_lag2", "rt_lag3",
                                                      "rt_lag4", "rt_lag5",
                                                      "roll_sd_5", "roll_sd_10",
                                                      "arma_y_t", "garch_sd_t"
                                                  )])

FEATS$BASE_NN <- lapply(FEATS$FULL, function(x) x[, c("rt_lag0", "rt_lag1", 
                                                      "rt_lag2", "rt_lag3", 
                                                      "rt_lag4", "rt_lag5", 
                                                      "roll_sd_5", "roll_sd_10",
                                                      "roll_mean_10")])

FEATS$ARMA_NN <- lapply(FEATS$FULL, function(x) x[, c("rt_lag0", "arma_y_t",   # try around with features, maybe less is more
                                                      "garch_sd_t",            #or simpler model
                                                      "u_t", "u_lag1",
                                                      "u_lag2",
                                                      "rt_lag1",
                                                      "roll_mean_10"
                                                      )])


## ---- main_Rolling_signal_and_backtests
#indicators and signals---------------------------------------------------------
STRATS$BH <- lapply(px_list, strat_bh)

STRATS$BASE_NN <- Map(
  function(x, nm) rolling_framework(x, FUN = simple_FNN_wildi, index_name = nm, train_years = 7),
  FEATS$BASE_NN,
  names(FEATS$BASE_NN)
)


STRATS$ARMA_NN <- Map(
  function(x, nm) rolling_framework(x, FUN = ARMA_FNN, index_name = nm, train_years = 7),
  FEATS$ARMA_NN,
  names(FEATS$ARMA_NN)
)

STRATS$LPD_ELM_NN <- Map(
  function(x, nm) rolling_framework(x, FUN = LPD_FNN_wildi, index_name = nm, train_years = 7, 
                                    ELM = TRUE, NN_trading = TRUE, 
                                    number_neurons = 100, learning_rate = 0.03),
  FEATS$LPD,
  names(FEATS$LPD)
)

# STRATS$LPD_ELM_NN <- Map(
#   function(x, nm) rolling_framework(x, FUN = LPD_FNN_wildi, index_name = nm, train_years = 7, 
#                                     ELM = TRUE, NN_trading = TRUE, 
#                                     number_neurons = 100, learning_rate = 0.3),
#   FEATS$LPD,
#   names(FEATS$LPD)
# )

# STRATS$LPD_ELM <- Map(
#   function(x, nm) rolling_framework(x, FUN = LPD_FNN_wildi, index_name = nm,  train_years = 7, 
#                                     ELM = TRUE, NN_trading = FALSE, number_neurons = 100),
#   FEATS$LPD,
#   names(FEATS$LPD)
# )

# STRATS$LPD_NN <- Map(
#   function(x, nm) rolling_framework(x, FUN = LPD_FNN_wildi, index_name = nm, train_years = 7,
#                                     ELM = FALSE, NN_trading = TRUE, learning_rate = 0.3,
#                                     number_neurons = 100),
#   FEATS$LPD,
#   names(FEATS$LPD)
# )
# 
# STRATS$LPD <- Map(
#   function(x, nm) rolling_framework(x, FUN = LPD_FNN_wildi, index_name = nm,  
#                                     train_years = 7, ELM = FALSE, NN_trading = FALSE, 
#                                     learning_rate = 0.3, number_neurons = 100),
#   FEATS$LPD,
#   names(FEATS$LPD)
# )

# train_data <- FEATS$BASE_NN$SSMI[index(FEATS$BASE_NN$SSMI) < "2018-12-31",]
# test_data <- FEATS$BASE_NN$SSMI[index(FEATS$BASE_NN$SSMI) > "2018-12-31",]
#c<-LPD_FNN_wildi(train_data, test_data)
# q<-ARMA_FNN(train_data, test_data)
# w<-simple_FNN(train_data, test_data)
# x<-rolling_framework(FEATS$ARMA_NN$SSMI, FUN = ARMA_FNN)
# y<-rolling_framework(FEATS$BASE_NN$SSMI, FUN = simple_FNN)
#z <-rolling_framework(FEATS$BASE_NN$SSMI, FUN = LPD_FNN_wildi)

# mean_LPD <- STRATS$ARMA_NN$SSMI$LPD$LPD_mean_oos  #plot not correct names and legend
# par(mfrow = c(1, 1))
# ts.plot(mean_LPD, col = rainbow(ncol(FEATS$ARMA_NN$SSMI[, -1])))
# 
# legend(
#   "topright",
#   c(colnames(FEATS$ARMA_NN$SSMI[, -1])),
#   lwd = 1,
#   col = rainbow(ncol(FEATS$ARMA_NN$SSMI[, -1]))
# )

## ---- main_prep returns
# Prep and align returns -------------------------------------------------------

BACKT <- list(BH = lapply(STRATS$BH, function(x){prepare_returns(x)}),
              NN = lapply(STRATS$BASE_NN, function(x){prepare_returns(x)}),
              ARMA_NN = lapply(STRATS$ARMA_NN, function(x){prepare_returns(x)}),
              LPD_ELM_NN = lapply(STRATS$LPD_ELM_NN, function(x){prepare_returns(x)})#,
              # LPD_ELM = lapply(STRATS$LPD_ELM, function(x){prepare_returns(x)}),
              # LPD_NN = lapply(STRATS$LPD_NN, function(x){prepare_returns(x)}),
              # LPD = lapply(STRATS$LPD, function(x){prepare_returns(x)})
              )


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
  BH = lapply(BACKT$BH, eval_backtest),
  NN = lapply(BACKT$NN, eval_backtest),
  ARMA_NN = lapply(BACKT$ARMA_NN, eval_backtest),
  LPD_ELM_NN = lapply(BACKT$LPD_ELM_NN, eval_backtest)#,
  # LPD_ELM = lapply(BACKT$LPD_ELM, eval_backtest),
  # LPD_NN = lapply(BACKT$LPD_NN, eval_backtest),
  # LPD = lapply(BACKT$LPD, eval_backtest)
)

EVAL_PORTF <- list(
  BH = lapply(PORTF$BH, eval_backtest),
  NN = lapply(PORTF$NN, eval_backtest),
  ARMA_NN = lapply(PORTF$ARMA_NN, eval_backtest),
  LPD_ELM_NN = lapply(PORTF$LPD_ELM_NN, eval_backtest)#,
  # LPD_ELM = lapply(PORTF$LPD_ELM, eval_backtest),
  # LPD_NN = lapply(PORTF$LPD_NN, eval_backtest),
  # LPD = lapply(PORTF$LPD, eval_backtest)
)

# Extract Sharpe ratios and Drawdowns into matrices ------------------------ 

mat_sharpe <- sapply(EVAL_BT, function(x) sapply(x, function(y) y$Sharpe))
mat_sharpe_adj <- sapply(EVAL_BT, function(x) sapply(x, function(y) y$Sharpe_adj))
mat_drawdown <- sapply(EVAL_BT, function(x) sapply(x, function(y) y$MaxDrawdown))
mat_exposure <- sapply(EVAL_BT, function(x) sapply(x, function(y) y$MeanExposure))
mat_activity <- sapply(EVAL_BT, function(x) sapply(x, function(y) y$TradingActivity))

# Add portfolio rows
mat_sharpe <- rbind(mat_sharpe,
                    sapply(EVAL_PORTF, function(x) sapply(x, function(y) y$Sharpe)))
rownames(mat_sharpe)[nrow(mat_sharpe)] <- "portfolio"

mat_sharpe_adj <- rbind(mat_sharpe_adj,
                    sapply(EVAL_PORTF, function(x) sapply(x, function(y) y$Sharpe_adj)))
rownames(mat_sharpe_adj)[nrow(mat_sharpe_adj)] <- "portfolio"

mat_drawdown <- rbind(mat_drawdown,
                      sapply(EVAL_PORTF, function(x) sapply(x, function(y) y$MaxDrawdown)))
rownames(mat_drawdown)[nrow(mat_drawdown)] <- "portfolio"

mat_exposure <- rbind(mat_exposure,
                      sapply(EVAL_PORTF, function(x) sapply(x, function(y) y$MeanExposure)))
rownames(mat_exposure)[nrow(mat_exposure)] <- "portfolio"

mat_activity <- rbind(mat_activity,
                      portfolio = round(colMeans(mat_activity, na.rm = TRUE)))

mat_sharpe
mat_sharpe_adj

# mean_exposure <- sapply(PORTF, function(p) mean(attr(p, "signal"), na.rm = TRUE))
# adjusted_sharpe <- as.numeric(mat_sharpe["portfolio", ]) / sqrt(mean_exposure[colnames(mat_sharpe)])
# adjusted_sharpe
# mean_exposure

# mats without the extra lpd models
mat_sharpe_focus <- mat_sharpe[, !colnames(mat_sharpe) %in% c("LPD_ELM", "LPD_NN", "LPD")]
mat_sharpe_adj_focus <- mat_sharpe_adj[, !colnames(mat_sharpe_adj) %in% c("LPD_ELM", "LPD_NN", "LPD")]


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

#---------- change name for crisis period ------------------------------------

mat_sharpe_crisis <- mat_sharpe
mat_sharpe_adj_crisis <- mat_sharpe_adj
ALL_TVALS_DIR_crisis <- ALL_TVALS_DIR
mat_drawdown_crisis <- mat_drawdown
mat_activity_crisis <- mat_activity
mat_exposure_crisis <- mat_exposure

mat_sharpe_crisis
mat_sharpe_adj_crisis



mat_sharpe_focus_crisis <- mat_sharpe_focus
mat_sharpe_adj_focus_crisis <- mat_sharpe_adj_focus

PORTF_crisis <- PORTF

STRATS_crisis <- STRATS
BACKT_crisis <- BACKT

save(
  mat_sharpe_focus_crisis,
  mat_sharpe_adj_focus_crisis,
  mat_drawdown_crisis,
  mat_sharpe_crisis,
  mat_sharpe_adj_crisis,
  mat_activity_crisis,
  mat_exposure_crisis,
  
  ALL_TVALS_DIR_crisis,
  PORTF_crisis,
  STRATS_crisis,
  BACKT_crisis,
  file = "R/data/main_crisis.RData"
)

