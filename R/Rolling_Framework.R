library(lubridate)

# feats = FEATS$SSMI

rolling_framework <- function(feats, FUN = simple_FNN, index_name = NULL) {
  
  year_idx <- year(as.Date(index(feats)))
  years <- unique(year_idx)
  
  pb <- txtProgressBar(min = 1, max = length(years)-5, style = 3)
  for (i in 6:length(years)) {
    
    cat("\n", index_name, ": ","Training:", years[i-5], "-", years[i-1],
        "| Testing:", years[i], "\n")
    
    train_data <- feats[year_idx %in% years[(i-5):(i-1)], ]
    test_data  <- feats[year_idx == years[i], ]
    
    results <- FUN(train_data, test_data)
    
    if (i == 6) {
      signal_oos <- results$signal_out_avg
      perf_oos   <- results$perf_out_avg
      
      signal_in  <- results$signal_in_avg
      perf_in    <- results$perf_in_avg
    } else {
      signal_oos <- rbind(signal_oos, results$signal_out_avg)
      perf_oos   <- rbind(perf_oos, results$perf_out_avg)
      
      signal_in  <- rbind(signal_in, results$signal_in_avg)
      perf_in    <- rbind(perf_in, results$perf_in_avg)
    }
    
    setTxtProgressBar(pb, i-5)
  }
  close(pb)
  
  sharpe_in  <- SharpeRatio.annualized(perf_in, scale = 252, Rf = 0)
  sharpe_oos <- SharpeRatio.annualized(perf_oos, scale = 252, Rf = 0)
  
  print(paste0(
    index_name,
    " | IS Sharpe: ", round(as.numeric(sharpe_in), 3),
    " | OOS Sharpe: ", round(as.numeric(sharpe_oos), 3)
  ))
  
  # sanity check--------------------------------------
  # plot(cumsum(perf_oos), col = 2)
  # lines(cumsum(feats$rt_lag0[index(feats > "2019")]), col =1)
  
  return(list(
    perf_oos   = perf_oos,
    signal_oos = signal_oos
  ))
}
