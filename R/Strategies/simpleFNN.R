# simple_FNN <- function(train_data, test_data,
#                        number_neurons = c(12, 6),
#                        num_sim = 10) {
#   
#   # preprocess ---------------------------------------------------------------
#   feat_min <- apply(train_data, 2, min, na.rm = TRUE)
#   feat_max <- apply(train_data, 2, max, na.rm = TRUE)
#   
#   scaled_train <- train_data
#   scaled_test  <- test_data
#   
#   scaled_train[, -1] <- scale(scaled_train[, -1],
#                               center = feat_min[-1],
#                               scale  = feat_max[-1] - feat_min[-1])
#   
#   scaled_test[, -1] <- scale(scaled_test[, -1],
#                              center = feat_min[-1],
#                              scale  = feat_max[-1] - feat_min[-1])
#   
#   # initialize containers ----------------------------------------------------
#   mse_runs <- matrix(NA, nrow = num_sim, ncol = 2)
#   colnames(mse_runs) <- c("In sample MSE", "Out sample MSE")
#   
#   ret_is_runs <- xts(matrix(NA, nrow = nrow(train_data), ncol = num_sim),
#                      order.by = index(train_data))
#   ret_oos_runs <- xts(matrix(NA, nrow = nrow(test_data), ncol = num_sim),
#                       order.by = index(test_data))
#   
#   signal_is_runs <- xts(matrix(NA, nrow = nrow(train_data), ncol = num_sim),
#                         order.by = index(train_data))
#   signal_oos_runs <- xts(matrix(NA, nrow = nrow(test_data), ncol = num_sim),
#                          order.by = index(test_data))
#   
#   pred_oos_runs <- xts(matrix(NA, nrow = nrow(test_data), ncol = num_sim),
#                        order.by = index(test_data))
#   
#   
#   # simulation loop ----------------------------------------------------------
#   cat("i |", paste(colnames(mse_runs), collapse = " | "), "\n")
#   for (i in 1:num_sim) {
#     
#     nn_obj <- try(
#       neuralnet(paste(colnames(scaled_train)[1], "~ ."),
#                 data = scaled_train,
#                 hidden = number_neurons,
#                 linear.output = TRUE),
#       silent = TRUE
#     )
#     
#     if (class(nn_obj) == "try-error" || is.null(nn_obj$net.result[[1]])){next}
#     
#     pred_is <- nn_obj$net.result[[1]]
#     pred_oos_runs[, i] <- predict(nn_obj, as.matrix(scaled_test[, -1]))
#     
#     mse_runs[i, 1] <- mean((scaled_train[, 1] - pred_is)^2)
#     mse_runs[i, 2] <- mean((scaled_test[, 1] - pred_oos_runs[, i])^2)
#     
#     ret_is_runs[, i]   <- (pred_is > 0) * train_data[,1]
#     ret_oos_runs[, i]  <- (pred_oos_runs[, i]  > 0) * test_data[,1]
#     
#     signal_is_runs[, i]  <- (pred_is > 0)
#     signal_oos_runs[, i] <- (pred_oos_runs[, i]  > 0)
#     
#     
#     cat(i, "|", paste(round(mse_runs[i, ], 8), collapse = " | "), "\n")
#   }
#   
#   
#   # combine successful runs --------------------------------------------------
#   
#   ret_oos_avg <- xts(apply(ret_oos_runs, 1, mean), order.by = index(test_data))
#   signal_oos_avg <- xts(apply(signal_oos_runs, 1, mean), order.by = index(test_data))
#   
#   # summary ------------------------------------------------------------------
#   cat("Mean IS MSE:", mean(mse_runs[, 1], na.rm = TRUE),
#       "| Mean OOS MSE:", mean(mse_runs[, 2], na.rm = TRUE), "\n")
#   cat("Sd IS MSE:", sd(mse_runs[, 1], na.rm = TRUE),
#       "| Sd OOS MSE:", sd(mse_runs[, 2], na.rm = TRUE), "\n")
#   
#   
#   # Sanity Check ----------------------------------------------------------------
#   # if (!is.null(ret_is_runs) && !is.null(ret_oos_runs)) {
#   #   par(mfrow = c(1, 2))
#   #   plot(cbind(cumsum(ret_is_runs), cumsum(train_data[,1])),
#   #        col = c(1:ncol(ret_is_runs), 1),
#   #        lwd = c(rep(1, ncol(ret_is_runs)), 3),
#   #        main = "In-Sample Performance",
#   #        ylab = "Cumulative Return")
#   #   plot(cbind(cumsum(ret_oos_runs), cumsum(test_data[,1])),
#   #        col = c(1:ncol(ret_oos_runs), 1),
#   #        lwd = c(rep(1, ncol(ret_oos_runs)), 3),
#   #        main = "Out-of-Sample Performance",
#   #        ylab = "Cumulative Return")
#   # }
#   
#   
#   
#   return(list(return = ret_oos_avg,
#               signal = signal_oos_avg,
#               
#               return_out_mat = ret_oos_runs,
#               predicted_oos_mat = pred_oos_runs))
# }







# predict_nn <- function(x_new, updated_params, x_train_ref, y_train_ref,
#                                 neuron_vec, linear_output = TRUE,
#                                 atan_not_sigmoid = FALSE) {
#   
#   layer_size <- getLayerSize(x_train_ref, y_train_ref, neuron_vec)
#   
#   fwd <- forwardPropagation(
#     x_train = as.matrix(x_new),
#     params = updated_params,
#     list_layer_size = layer_size,
#     linear_output = linear_output,
#     atan_not_sigmoid = atan_not_sigmoid
#   )
#   
#   as.vector(fwd$A_list[[length(fwd$A_list)]])
# }



simple_FNN_wildi <- function(train_data, test_data,
                                number_neurons = c(100),
                                num_sim = 10,
                                epochs = 200,
                                learning_rate = 0.3) {
  
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
  
  # containers ---------------------------------------------------------------
  mse_runs <- matrix(NA, nrow = num_sim, ncol = 2)
  colnames(mse_runs) <- c("In sample MSE", "Out sample MSE")
  
  ret_oos_runs <- xts(matrix(NA, nrow = nrow(test_data), ncol = num_sim),
                      order.by = index(test_data))
  
  signal_oos_runs <- xts(matrix(NA, nrow = nrow(test_data), ncol = num_sim),
                         order.by = index(test_data))
  
  pred_oos_runs <- xts(matrix(NA, nrow = nrow(test_data), ncol = num_sim),
                       order.by = index(test_data))
  
  cost_hist <- matrix(NA, nrow = epochs+1, ncol = num_sim)
  
  LPD_array_is  <- array(NA, dim = c(num_sim, nrow(scaled_train[, -1]), ncol(scaled_train[, -1])))
  LPD_array_oos <- array(NA, dim = c(num_sim, nrow(scaled_test[, -1]),  ncol(scaled_test[, -1])))
  
  # simulation loop ----------------------------------------------------------
  
  x_train <- as.matrix(scaled_train[, -1, drop = FALSE])
  y_train <- as.matrix(scaled_train[,  1, drop = FALSE])
  x_test  <- as.matrix(scaled_test[, -1, drop = FALSE])
  
  hyper_list <- list(
    epochs = epochs,
    learning_rate = learning_rate,
    linear_output = TRUE,
    atan_not_sigmoid = FALSE,
    neuron_vec = number_neurons,
    parm_init = NULL
  )
  
  for (i in 1:num_sim) {
    nn_obj <- try(
      trainModel(x_train = x_train, y = y_train, hyper_list = hyper_list),
      silent = TRUE
    )
    layer_size <- getLayerSize(x_train, y_train, number_neurons)
    
    cache_is <- forwardPropagation(
      x_train = x_train,
      params = nn_obj$updated_params,
      list_layer_size = layer_size,
      linear_output = TRUE,
      atan_not_sigmoid = FALSE
    )
    
    cache_oos <- forwardPropagation(
      x_train = x_test,
      params = nn_obj$updated_params,
      list_layer_size = layer_size,
      linear_output = TRUE,
      atan_not_sigmoid = FALSE
    )
    
    pred_is  <- as.vector(cache_is$A_list[[length(cache_is$A_list)]])
    pred_oos <- as.vector(cache_oos$A_list[[length(cache_oos$A_list)]])
    pred_oos_runs[, i] <- pred_oos
    
    LPD_obj_is <- LPD(cache = cache_is, params = nn_obj$updated_params,
                        list_layer_size = layer_size, linear_output = TRUE, 
                        atan_not_sigmoid = FALSE)
                      
    LPD_obj_oos <- LPD(cache = cache_oos, params = nn_obj$updated_params,
                        list_layer_size = layer_size, linear_output = TRUE, 
                        atan_not_sigmoid = FALSE)
    
    LPD_array_is[i, , ]  <- LPD_obj_is$LPD_t
    LPD_array_oos[i, , ] <- LPD_obj_oos$LPD_t
    
    mse_runs[i, 1] <- mean((as.numeric(scaled_train[, 1]) - pred_is)^2, na.rm = TRUE)
    mse_runs[i, 2] <- mean((as.numeric(scaled_test[, 1])  - pred_oos)^2, na.rm = TRUE)
    
    signal_oos_runs[, i] <- as.numeric(pred_oos > 0)
    ret_oos_runs[, i]    <- as.numeric(pred_oos > 0) * as.numeric(test_data[,1])
    
    cost_hist[1:length(nn_obj$cost_hist), i] <- nn_obj$cost_hist
    
    cat("i |", paste(colnames(mse_runs), collapse = " | "), "\n")
    cat(i, "|", paste(round(mse_runs[i, ], 8), collapse = " | "), "\n", "\n", "\n")
  }
  #ts.plot(nn_obj$cost_hist, main = "Convergence")
  
  # averages -----------------------------------------------------------------
  ret_oos_avg <- xts(apply(ret_oos_runs, 1, mean, na.rm = TRUE),
                     order.by = index(test_data))
  
  signal_oos_avg <- xts(apply(signal_oos_runs, 1, mean, na.rm = TRUE),
                        order.by = index(test_data))
  
  pred_oos_avg <- xts(apply(pred_oos_runs, 1, mean, na.rm = TRUE),
                      order.by = index(test_data))
  
  mean_LPD_oos<-NULL
  for (k in 1:dim(LPD_array_oos)[3])
    mean_LPD_oos<-cbind(mean_LPD_oos,apply(LPD_array_oos[,,k],2,mean))
  
  mean_LPD_is<-NULL
  for (k in 1:dim(LPD_array_is)[3])
    mean_LPD_is<-cbind(mean_LPD_is,apply(LPD_array_is[,,k],2,mean))
  
  # summary ------------------------------------------------------------------
  cat("Mean IS MSE:", mean(mse_runs[, 1], na.rm = TRUE),
      "| Mean OOS MSE:", mean(mse_runs[, 2], na.rm = TRUE), "\n")
  cat("Sd IS MSE:", sd(mse_runs[, 1], na.rm = TRUE),
      "| Sd OOS MSE:", sd(mse_runs[, 2], na.rm = TRUE), "\n")
  
  return(list(
    return = ret_oos_avg,
    signal = signal_oos_avg,
    return_out_mat = ret_oos_runs,
    
    predicted_oos_mat = pred_oos_runs,
    predicted_oos_avg = pred_oos_avg,
    
    # extra info
    cost_hist = cost_hist,
    LPD = list(LPD_mean_oos = mean_LPD_oos,
               LPD_mean_is = mean_LPD_is)
  ))
}
