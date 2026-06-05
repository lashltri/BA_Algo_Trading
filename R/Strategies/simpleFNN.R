simple_FNN_wildi <- function(train_data, test_data,
                             number_neurons = c(100),
                             num_sim = 100,
                             epochs = 100,
                             learning_rate = 0.3,
                             ELM = FALSE) {
  
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
  
  pred_is_runs <- xts(matrix(NA, nrow = nrow(train_data), ncol = num_sim), #changed
                      order.by = index(train_data)) #changed
  
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
    
    if (ELM == TRUE) {
      params_el <- nn_obj$updated_params
      
      cache_hidden <- forwardPropagation(
        x_train = x_train,
        params = params_el,
        list_layer_size = layer_size,
        linear_output = TRUE,
        atan_not_sigmoid = FALSE
      )
      
      H <- t(cache_hidden$A_list[[length(layer_size$n_h)]])
      y <- as.numeric(y_train)
      
      fit_el <- lm(y ~ H)
      coef_el <- coef(fit_el)
      coef_el[is.na(coef_el)] <- 0
      
      params_el$W_list[[length(layer_size$n_h) + 1]] <- matrix(coef_el[-1], nrow = 1)
      params_el$b_list[[length(layer_size$n_h) + 1]] <- matrix(coef_el[1], nrow = 1)
      
      nn_obj$updated_params <- params_el
      
      cache_el <- forwardPropagation(
        x_train = x_train,
        params = nn_obj$updated_params,
        list_layer_size = layer_size,
        linear_output = TRUE,
        atan_not_sigmoid = FALSE
      )
      
      cost_el <- computeCost(y_train, cache_el)
      nn_obj$cost_hist[length(nn_obj$cost_hist)] <- cost_el
    }
    
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
    pred_is_runs[, i] <- pred_is #changed
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
  
  pred_is_avg <- xts(apply(pred_is_runs, 1, mean, na.rm = TRUE), #changed
                     order.by = index(train_data)) #changed
  
  pred_oos_avg <- xts(apply(pred_oos_runs, 1, mean, na.rm = TRUE),
                      order.by = index(test_data))
  
  # mean_LPD_oos<-NULL
  # for (k in 1:dim(LPD_array_oos)[3])
  #   mean_LPD_oos<-cbind(mean_LPD_oos,apply(LPD_array_oos[,,k],2,mean))
  # 
  # mean_LPD_is<-NULL
  # for (k in 1:dim(LPD_array_is)[3])
  #   mean_LPD_is<-cbind(mean_LPD_is,apply(LPD_array_is[,,k],2,mean))
  
  
  mean_LPD_oos <- NULL
  for (k in 1:dim(LPD_array_oos)[3]) {
    mean_LPD_oos <- cbind(mean_LPD_oos, apply(LPD_array_oos[,,k], 2, mean))
  }
  
  mean_LPD_oos <- xts(mean_LPD_oos, order.by = index(test_data))
  colnames(mean_LPD_oos) <- colnames(scaled_test[, -1])
  
  mean_LPD_is <- NULL
  for (k in 1:dim(LPD_array_is)[3]) {
    mean_LPD_is <- cbind(mean_LPD_is, apply(LPD_array_is[,,k], 2, mean))
  }
  
  mean_LPD_is <- xts(mean_LPD_is, order.by = index(train_data))
  colnames(mean_LPD_is) <- colnames(scaled_train[, -1])
  
  ## Std of LPD ----
  LPD_std <- NULL
  for (k in 1:dim(LPD_array_oos)[3]) {  
    std <- sqrt(apply(t(LPD_array_oos[,,k]), 1, var))
    LPD_std <- cbind(LPD_std, std)
  }
  
  LPD_std <- xts(LPD_std, order.by = index(test_data))
  colnames(LPD_std) <- colnames(scaled_test[, -1])
  
  # summary ------------------------------------------------------------------
  cat("Mean IS MSE:", mean(mse_runs[, 1], na.rm = TRUE),
      "| Mean OOS MSE:", mean(mse_runs[, 2], na.rm = TRUE), "\n")
  cat("Sd IS MSE:", sd(mse_runs[, 1], na.rm = TRUE),
      "| Sd OOS MSE:", sd(mse_runs[, 2], na.rm = TRUE), "\n")
  
  return(list(
    return = ret_oos_avg,
    signal = signal_oos_avg,
    return_out_mat = ret_oos_runs,
    
    predicted_is_mat = pred_is_runs, #changed
    predicted_is_avg = pred_is_avg, #changed
    predicted_oos_mat = pred_oos_runs,
    predicted_oos_avg = pred_oos_avg,
    
    # extra info
    cost_hist = cost_hist,
    LPD = list(
      LPD_mean_oos = mean_LPD_oos,
      LPD_mean_is  = mean_LPD_is,
      LPD_sd       = LPD_std,
      LPD_array = LPD_array_oos
    )
  ))
}
