simple_FNN <- function(train_data, test_data,
                       number_neurons = c(12, 6),
                       num_sim = 100) {
  
  # preprocess ---------------------------------------------------------------
  feat_min <- apply(train_data, 2, min, na.rm = TRUE)
  feat_max <- apply(train_data, 2, max, na.rm = TRUE)
  
  scaled_train <- train_data
  scaled_test  <- test_data
  
  scaled_train[, -1] <- scale(scaled_train[, -1],
                              center = feat_min[-1],
                              scale  = feat_max[-1] - feat_min[-1])
  
  scaled_test[, -1] <- scale(scaled_test[, -1],
                             center = feat_min[-1],
                             scale  = feat_max[-1] - feat_min[-1])
  
  # initialize containers ----------------------------------------------------
  mse_runs <- matrix(NA, nrow = num_sim, ncol = 2)
  colnames(mse_runs) <- c("In sample MSE", "Out sample MSE")
  
  ret_is_runs <- xts(matrix(NA, nrow = nrow(train_data), ncol = num_sim),
                     order.by = index(train_data))
  ret_oos_runs <- xts(matrix(NA, nrow = nrow(test_data), ncol = num_sim),
                      order.by = index(test_data))
  
  signal_is_runs <- xts(matrix(NA, nrow = nrow(train_data), ncol = num_sim),
                        order.by = index(train_data))
  signal_oos_runs <- xts(matrix(NA, nrow = nrow(test_data), ncol = num_sim),
                         order.by = index(test_data))
  
  pred_oos_runs <- xts(matrix(NA, nrow = nrow(test_data), ncol = num_sim),
                       order.by = index(test_data))
  
  
  # simulation loop ----------------------------------------------------------
  cat("i |", paste(colnames(mse_runs), collapse = " | "), "\n")
  for (i in 1:num_sim) {
    
    nn_obj <- try(
      neuralnet(paste(colnames(scaled_train)[1], "~ ."),
                data = scaled_train,
                hidden = number_neurons,
                linear.output = TRUE),
      silent = TRUE
    )
    
    if (class(nn_obj) == "try-error" || is.null(nn_obj$net.result[[1]])){next}
    
    pred_is <- nn_obj$net.result[[1]]
    pred_oos_runs[, i] <- predict(nn_obj, as.matrix(scaled_test[, -1]))
    
    mse_runs[i, 1] <- mean((scaled_train[, 1] - pred_is)^2)
    mse_runs[i, 2] <- mean((scaled_test[, 1] - pred_oos_runs[, i])^2)
    
    ret_is_runs[, i]   <- (pred_is > 0) * train_data[,1]
    ret_oos_runs[, i]  <- (pred_oos_runs[, i]  > 0) * test_data[,1]
    
    signal_is_runs[, i]  <- (pred_is > 0)
    signal_oos_runs[, i] <- (pred_oos_runs[, i]  > 0)
    
    
    cat(i, "|", paste(round(mse_runs[i, ], 8), collapse = " | "), "\n")
  }
  
  
  # combine successful runs --------------------------------------------------
  
  ret_oos_avg <- xts(apply(ret_oos_runs, 1, mean), order.by = index(test_data))
  signal_oos_avg <- xts(apply(signal_oos_runs, 1, mean), order.by = index(test_data))
  
  # summary ------------------------------------------------------------------
  cat("Mean IS MSE:", mean(mse_runs[, 1], na.rm = TRUE),
      "| Mean OOS MSE:", mean(mse_runs[, 2], na.rm = TRUE), "\n")
  cat("Sd IS MSE:", sd(mse_runs[, 1], na.rm = TRUE),
      "| Sd OOS MSE:", sd(mse_runs[, 2], na.rm = TRUE), "\n")
  
  
  # Sanity Check ----------------------------------------------------------------
  # if (!is.null(ret_is_runs) && !is.null(ret_oos_runs)) {
  #   par(mfrow = c(1, 2))
  #   plot(cbind(cumsum(ret_is_runs), cumsum(train_data[,1])),
  #        col = c(1:ncol(ret_is_runs), 1),
  #        lwd = c(rep(1, ncol(ret_is_runs)), 3),
  #        main = "In-Sample Performance",
  #        ylab = "Cumulative Return")
  #   plot(cbind(cumsum(ret_oos_runs), cumsum(test_data[,1])),
  #        col = c(1:ncol(ret_oos_runs), 1),
  #        lwd = c(rep(1, ncol(ret_oos_runs)), 3),
  #        main = "Out-of-Sample Performance",
  #        ylab = "Cumulative Return")
  # }
  
  
  
  return(list(return = ret_oos_avg,
              signal = signal_oos_avg,
              
              return_out_mat = ret_oos_runs,
              predicted_oos_mat = pred_oos_runs))
}
