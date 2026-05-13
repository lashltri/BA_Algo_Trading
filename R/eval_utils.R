
# ---- prep_returns
prepare_returns <- function(x){
  ret <- x$return
  attr(ret, "signal") <- x$signal
  return(ret)
}

## ---- Eval_backtest-fn
eval_backtest <- function(x) {
  r <- exp(x) - 1
  sharpe <- SharpeRatio.annualized(r, scale = 252, Rf = 0)
  
  signal <- attr(x, "signal")
  
  mean_exposure <- mean(signal, na.rm = TRUE)
  sharpe_adj <- sharpe / sqrt(mean_exposure)
  
  mdd <- maxDrawdown(r)
  
  trading_activity <- round(sum(abs(diff(signal)), na.rm = TRUE))
  
  return(list(
    Sharpe = as.numeric(sharpe),
    MaxDrawdown = as.numeric(mdd),
    Sharpe_adj = as.numeric(sharpe_adj),
    MeanExposure = as.numeric(mean_exposure),
    TradingActivity = as.numeric(trading_activity)
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


