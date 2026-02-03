
## ---- tuning-fn
tune_rolling_window_LB <- function(price, 
                                   param_grid, 
                                   strategyfun, 
                                   backtestfun, 
                                   needs_lookback = TRUE,
                                   tuning_window = 5) {
  price <- Ad(price)
  years <- unique(lubridate::year(index(price)))
  n_years <- length(years)
  
  # max lookback needed
  max_lb <- if (!needs_lookback) 0L else max(apply(param_grid, 1, max)) - 1L
  
  
  strat <- NULL
  
  for (i in seq_len(n_years - tuning_window)) {
    train_years <- years[i:(i + (tuning_window-1))]   # 5-year train
    test_year   <- years[i + tuning_window]       # next-year test
    
    train_idx <- which(year(index(price)) %in% train_years)
    test_idx  <- which(year(index(price)) %in% test_year)
    
    train_set <- price[train_idx]
    
    # add warm-up rows before the first test row -----------
    if (length(test_idx) == 0) next
    start_pos <- max(min(test_idx) - max_lb, 1)
    test_warm_idx <- seq(start_pos, max(test_idx))
    test_set_warm <- price[test_warm_idx]
    
    cat("Train:", min(year(index(train_set))), "to", max(year(index(train_set))),
        "| Test:", test_year, "\n")
    
    # Train scoring ----------------------------------------
    scores <- apply(param_grid, 1, function(row) {
      p  <- as.numeric(row)
      so <- strategyfun(p, train_set)
      bt <- backtestfun(so)
      return(eval_backtest(bt)$Sharpe)
    })
    
    best_ix     <- which.max(scores)
    best_param  <- as.numeric(param_grid[best_ix, , drop = FALSE])
    cat("Best Parameters:", paste(best_param, collapse = ","), "\n\n")
    
    # Test using warm-up, then drop warm-up rows for aggregation -----------
    so_test_full <- strategyfun(best_param, test_set_warm)
    
    # keep only true test-year rows (drop the warm-up)---------------------
    keep <- which(lubridate::year(index(so_test_full$price)) == test_year)
    
    
    p_keep  <- so_test_full$price[keep, , drop = FALSE]
    s_keep  <- so_test_full$signal[keep, , drop = FALSE]
    pa_keep <- as.matrix(so_test_full$param[keep, , drop = FALSE])
    st_keep <- so_test_full$strategy[keep, , drop = FALSE]
    
    if (is.null(strat)) {
      strat <- list(
        price    = p_keep,
        signal   = s_keep,
        param    = pa_keep,
        strategy = st_keep
      )
    } else {
      strat$price    <- rbind(strat$price,    p_keep)
      strat$signal   <- rbind(strat$signal,   s_keep)
      strat$param    <- rbind(strat$param,    pa_keep)
      strat$strategy <- rbind(strat$strategy, st_keep)
    }
  }
  
  return(strat)
}

## ---- end

test <- function(x) {
  print("Hello World")
}
test()