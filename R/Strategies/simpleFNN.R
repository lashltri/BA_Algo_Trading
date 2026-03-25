
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
  mse_mat <- matrix(NA, nrow = num_sim, ncol = 2)
  colnames(mse_mat) <- c("In sample MSE", "Out sample MSE")
  
  perf_in_mat <- xts(matrix(NA, nrow = nrow(train_data), ncol = num_sim),
                          order.by = index(train_data))
  perf_out_mat <- xts(matrix(NA, nrow = nrow(test_data), ncol = num_sim),
                           order.by = index(test_data))
  
  signal_in_mat <- xts(matrix(NA, nrow = nrow(train_data), ncol = num_sim),
                            order.by = index(train_data))
  signal_out_mat <- xts(matrix(NA, nrow = nrow(test_data), ncol = num_sim),
                             order.by = index(test_data))
  
  predicted_oos_mat <- xts(matrix(NA, nrow = nrow(test_data), ncol = num_sim),
                        order.by = index(test_data))

  
  # simulation loop ----------------------------------------------------------
  cat("i |", paste(colnames(mse_mat), collapse = " | "), "\n")
  for (i in 1:num_sim) { #i=1
    
    nn_obj <- try(
      neuralnet(paste(colnames(scaled_train)[1], "~ ."),  # Takes first column as terget variable
                data = scaled_train,
                hidden = number_neurons,
                linear.output = TRUE),
      silent = TRUE
    )
    
    if (class(nn_obj) == "try-error" || is.null(nn_obj$net.result[[1]])){next}
    
    predicted_in <- nn_obj$net.result[[1]]
    predicted_oos_mat[, i] <- predict(nn_obj, as.matrix(scaled_test[, -1]))
    
    mse_mat[i, 1] <- mean((scaled_train[, 1] - predicted_in)^2)
    mse_mat[i, 2] <- mean((scaled_test[, 1] - predicted_oos_mat[, i])^2)
    
    perf_in_mat[, i]   <- (predicted_in > 0) * train_data[,1]
    perf_out_mat[, i]  <- (predicted_oos_mat[, i]  > 0) * test_data[,1]
    
    signal_in_mat[, i]  <- (predicted_in > 0)
    signal_out_mat[, i] <- (predicted_oos_mat[, i]  > 0)
    
    
    cat(i, "|", paste(round(mse_mat[i, ], 8), collapse = " | "), "\n")
  }
  
  
  # combine successful runs --------------------------------------------------
  
  perf_out_avg <- xts(apply(perf_out_mat, 1, mean), order.by = index(test_data))
  signal_out_avg <- xts(apply(signal_out_mat, 1, mean), order.by = index(test_data))
  
  # summary ------------------------------------------------------------------
  cat("Mean IS MSE:", mean(mse_mat[, 1], na.rm = TRUE),
      "| Mean OOS MSE:", mean(mse_mat[, 2], na.rm = TRUE), "\n")
  cat("Sd IS MSE:", sd(mse_mat[, 1], na.rm = TRUE),
      "| Sd OOS MSE:", sd(mse_mat[, 2], na.rm = TRUE), "\n")

  
  # Sanity Check ----------------------------------------------------------------
  # if (!is.null(perf_in_mat) && !is.null(perf_out_mat)) {
  #   par(mfrow = c(1, 2))
  #   plot(cbind(cumsum(perf_in_mat), cumsum(train_data[,1])),
  #        col = c(1:ncol(perf_in_mat), 1),
  #        lwd = c(rep(1, ncol(perf_in_mat)), 3),
  #        main = "In-Sample Performance",
  #        ylab = "Cumulative Return")
  #   plot(cbind(cumsum(perf_out_mat), cumsum(test_data[,1])),
  #        col = c(1:ncol(perf_out_mat), 1),
  #        lwd = c(rep(1, ncol(perf_out_mat)), 3),
  #        main = "Out-of-Sample Performance",
  #        ylab = "Cumulative Return")
  # }


  
  return(list(return = perf_out_avg,
              signal = signal_out_avg,
              
              return_out_mat = perf_out_mat,
              predicted_oos_mat = predicted_oos_mat))
}
