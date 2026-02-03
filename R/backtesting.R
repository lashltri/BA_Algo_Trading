
## ---- backtest_logRet-fn
backtest_log_ret <- function(x){
  price <- x$price
  signal   <- x$signal
  
  ret <- suppressWarnings(diff(log(price)))
  
  trade <- lag(signal, 1)
  
  strat <- na.omit(merge(ret, trade, join = "inner"))
  strat_ret <- strat[,1] * strat[,2]
  
  attr(strat_ret, "signal") <- strat[, 2]
  return(strat_ret)
}

## ---- Evel_backtest-fn
eval_backtest <- function(x) {
  r <- exp(x) - 1
  sharpe <- SharpeRatio.annualized(r, scale = 252, Rf = 0)
  mdd <- maxDrawdown(r)
  
  return(list(
    Sharpe = as.numeric(sharpe),
    MaxDrawdown = as.numeric(mdd)
  ))
}


## ---- create_portfolio-fn
create_portfolio <- function(x){
  # x: list of log-return xts, each with attr("signal")
  
  Rlog <- do.call(merge, c(x, list(all = FALSE)))
  r <- exp(Rlog) - 1 
  
  #Equal Weight
  portfolio_R <- xts(rowMeans(r, na.rm = TRUE), order.by = index(r))
  portfolio_Rlog <- log(portfolio_R + 1)
  
  #extract signals POSt merge and reattach as an attribute 
  sig_list <- lapply(x, function(s) attr(s, "signal"))
  sig_mat  <- do.call(merge, c(sig_list, list(all = FALSE)))
  port_sig <- xts(rowMeans(sig_mat, na.rm = TRUE), order.by = index(sig_mat))
  
  attr(portfolio_Rlog, "signal") <- port_sig
  return(portfolio_Rlog)
}


