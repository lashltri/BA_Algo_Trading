#feats = FEATS$ARMA_NN$SSMI

rolling_framework <- function(feats, FUN, index_name = NULL) {
  
  year_idx <- year(as.Date(index(feats)))
  years <- unique(year_idx)
  
  pb <- txtProgressBar(min = 0, max = length(years)-5, style = 3)
  setTxtProgressBar(pb, 0)
  
  
  for (i in 6:length(years)) {
    cat("\n","\n","\n", index_name, ": ","Training:", years[i-5], "-", years[i-1],
        "| Testing:", years[i], "\n")
    
    train_data <- feats[year_idx %in% years[(i-5):(i-1)], ]
    test_data  <- feats[year_idx == years[i], ]
    
    LPD_oos <- NULL
    LPD_is_list <- list()
    
    results <- FUN(train_data, test_data)
    setTxtProgressBar(pb, i-5)
    
    if (i == 6) {
      signal_oos <- results$signal
      ret_oos    <- results$return
      
      ret_oos_runs <- results$return_out_mat
      
      if (!is.null(results$LPD)) {
        LPD_oos <- results$LPD$LPD_mean_oos
        LPD_is_list[[paste0("train_", years[i-5], "_", years[i-1])]] <- results$LPD$LPD_mean_is
      }
      
    } else {
      signal_oos <- rbind(signal_oos, results$signal)
      ret_oos    <- rbind(ret_oos, results$return)
      
      ret_oos_runs <- rbind(ret_oos_runs, results$return_out_mat)
      
      if (!is.null(results$LPD)) {
        LPD_oos <- rbind(LPD_oos, results$LPD$LPD_mean_oos)
        LPD_is_list[[paste0("train_", years[i-5], "_", years[i-1])]] <- results$LPD$LPD_mean_is
      }
    }
    
  }
  close(pb)
  
  # Sharpe metrics  -----------------------------------------
  sharpe_oos <- as.numeric(SharpeRatio.annualized(ret_oos, scale = 252, Rf = 0))
  sharpe_nn <- apply(ret_oos_runs, 2, function(r) {mean(r) / sd(r) * sqrt(252)})
  se_sharpe_oos <- sd(sharpe_nn) / sqrt(length(sharpe_nn))
  sd_sharpe_nn <- sd(sharpe_nn)
  
  cat(paste0(
    "\n",
    index_name,
    " | OOS Sharpe: ", round(as.numeric(sharpe_oos), 3),
    "\n"
  ))
  
  # sanity check--------------------------------------
  # par(mfrow = c(2,1))
  # plot(cumsum(ret_oos), col = 2)
  # plot(cumsum(ret_oos), col = 2)
  # lines(cumsum(feats$rt_lag0[index(ret_oos)]), col =1)
  # 
  # plot(cumsum(ret_oos_runs))
  # 
  # plot(signal_oos)
  # plot(feats$sigma_t[index(signal_oos)])
  
  
  return(list(
    return = ret_oos,
    signal = signal_oos,
    info = list(sharpe_oos = sharpe_oos,
                sd_sharpe_oos = se_sharpe_oos,
                sharpe_runs = sharpe_nn, 
                sd_sharpe_runs = sd_sharpe_nn,
                return_out_runs = ret_oos_runs),
    LPD = list(
      LPD_mean_oos = LPD_oos,
      LPD_mean_is = LPD_is_list
    )
  ))
}


