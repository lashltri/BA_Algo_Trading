
simple_FNN <- function(train_data, test_data,
                       number_neurons = c(12, 6),
                       num_sim = 20) {
  
  # preprocess ---------------------------------------------------------------
  feat_min <- apply(train_data, 2, quantile, probs = 0.01, na.rm = TRUE)
  feat_max <- apply(train_data, 2, quantile, probs = 0.99, na.rm = TRUE)
  
  scaled_train <- pmin(pmax(train_data, feat_min), feat_max)
  scaled_test  <- pmin(pmax(test_data,  feat_min), feat_max)
  
  scaled_train[, -1] <- scale(train_data[, -1],
                              center = feat_min[-1],
                              scale  = feat_max[-1] - feat_min[-1])
  
  scaled_test[, -1] <- scale(test_data[, -1],
                             center = feat_min[-1],
                             scale  = feat_max[-1] - feat_min[-1])
  
  # initialize containers ----------------------------------------------------
  mse_mat <- matrix(NA_real_, nrow = num_sim, ncol = 2)
  colnames(mse_mat) <- c("In sample MSE", "Out sample MSE")
  
  sharpe_in  <- rep(NA_real_, num_sim)
  sharpe_out <- rep(NA_real_, num_sim)
  
  perf_in_mat <- xts(matrix(NA_real_, nrow = nrow(train_data), ncol = num_sim),
                          order.by = index(train_data))
  perf_out_mat <- xts(matrix(NA_real_, nrow = nrow(test_data), ncol = num_sim),
                           order.by = index(test_data))
  
  signal_in_mat <- xts(matrix(NA_real_, nrow = nrow(train_data), ncol = num_sim),
                            order.by = index(train_data))
  signal_out_mat <- xts(matrix(NA_real_, nrow = nrow(test_data), ncol = num_sim),
                             order.by = index(test_data))
  
  #pb <- txtProgressBar(min = 1, max = num_sim, style = 3)
  
  # simulation loop ----------------------------------------------------------
  for (i in 1:num_sim) { #i=1
    
    nn_obj <- try(
      neuralnet(rt_lag0 ~ .,
                data = scaled_train,
                hidden = number_neurons,
                linear.output = TRUE,
                stepmax = 1e+05),
      silent = TRUE
    )
    
    if (class(nn_obj) == "try-error" || is.null(nn_obj$net.result[[1]])){
      #setTxtProgressBar(pb, i)
      next
    }
    
    predicted_train <- nn_obj$net.result[[1]]
    predicted_test  <- predict(nn_obj, as.matrix(scaled_test[, -1]))
    
    mse_mat[i, 1] <- mean((scaled_train[, 1] - predicted_train)^2)
    mse_mat[i, 2] <- mean((scaled_test[, 1] - predicted_test)^2)
    
    perf_in_mat[, i]   <- (predicted_train > 0) * train_data[,1]
    perf_out_mat[, i]  <- (predicted_test  > 0) * test_data[,1]
    
    signal_in_mat[, i]  <- (predicted_train > 0)
    signal_out_mat[, i] <- (predicted_test  > 0)
    
    sharpe_in[i]  <- SharpeRatio.annualized(perf_in_mat[, i], scale = 252, Rf = 0)
    sharpe_out[i] <- SharpeRatio.annualized(perf_out_mat[, i], scale = 252, Rf = 0)
    
    print(c(i, mse_mat[i, ]))
    #setTxtProgressBar(pb, i)
  }
  
  #close(pb)
  
  # combine successful runs --------------------------------------------------
  
  perf_out_avg <- xts(apply(perf_out_mat, 1, mean), order.by = index(test_data))
  signal_out_avg <- xts(apply(signal_out_mat, 1, mean), order.by = index(test_data))
  
  perf_in_avg <- xts(apply(perf_in_mat, 1, mean), order.by = index(train_data))
  signal_in_avg <- xts(apply(signal_in_mat, 1, mean), order.by = index(train_data))
  
  # summary ------------------------------------------------------------------
  cat("Mean IS MSE:", mean(mse_mat[, 1], na.rm = TRUE),
      "| Mean OOS MSE:", mean(mse_mat[, 2], na.rm = TRUE), "\n")
  cat("Sd IS MSE:", sd(mse_mat[, 1], na.rm = TRUE),
      "| Sd OOS MSE:", sd(mse_mat[, 2], na.rm = TRUE), "\n")
  cat("Sharpe IS:", mean(sharpe_in, na.rm = TRUE),
      "| Sharpe OOS:", mean(sharpe_out, na.rm = TRUE), "\n")
  
  cat("IS MSE vs. OOS Sharpe Ratio Correlation: ",
      cor(sharpe_out, mse_mat[, "In sample MSE"], use = "complete.obs"), "\n")
  cat("IS Sharpe Ratio vs. OOS Sharpe Ratio Correlation: ",
      cor(sharpe_out, sharpe_in, use = "complete.obs"), "\n")
  
  # Sanity Check ----------------------------------------------------------------
  if (!is.null(perf_in_mat) && !is.null(perf_out_mat)) {
    par(mfrow = c(1, 2))
    plot(cbind(cumsum(perf_in_mat), cumsum(train_data[,1])),
         col = c(1:ncol(perf_in_mat), 1),
         lwd = c(rep(1, ncol(perf_in_mat)), 3),
         main = "In-Sample Performance",
         ylab = "Cumulative Return")
    plot(cbind(cumsum(perf_out_mat), cumsum(test_data[,1])),
         col = c(1:ncol(perf_out_mat), 1),
         lwd = c(rep(1, ncol(perf_out_mat)), 3),
         main = "Out-of-Sample Performance",
         ylab = "Cumulative Return")
  }
  

  
  return(list(perf_out_avg = perf_out_avg,
              signal_out_avg = signal_out_avg,
              perf_in_avg = perf_in_avg,
              signal_in_avg = signal_in_avg))
}
