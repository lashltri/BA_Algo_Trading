

ARMA_FNN <- function(train_data, test_data,
                     number_neurons = c(12, 6),
                     num_sim = 100,
                     epochs = 100,
                     learning_rate = 0.3,
                     ELM = FALSE) {
  
  arma_y_is  <- train_data$arma_y_t
  arma_y_oos <- test_data$arma_y_t
  
  # remove arma_y_t
  drop_cols <- c("arma_y_t", "rt_lag0")
  nn_train <- train_data[, setdiff(colnames(train_data), drop_cols)]
  nn_test  <- test_data[,  setdiff(colnames(test_data),  drop_cols)]
  
  # move eps_t to first column
  nn_train <- nn_train[, c("u_t", setdiff(colnames(nn_train), "u_t"))]
  nn_test  <- nn_test[,  c("u_t", setdiff(colnames(nn_test),  "u_t"))]
  
  nn_results <- simple_FNN_wildi(train_data = nn_train, test_data = nn_test, 
                           number_neurons = number_neurons, 
                           num_sim = num_sim, epochs = epochs, ELM = ELM)
  
  eps_hat_oos_runs <- nn_results$predicted_oos_mat * as.numeric(test_data$garch_sd_t)
  
  
  # Predicted returns
  pred_hat_oos_runs <- as.numeric(arma_y_oos) + eps_hat_oos_runs
  ret_oos_runs <- (pred_hat_oos_runs > 0) * as.numeric(test_data[,1])
  
  ret_oos_avg <- xts(apply(ret_oos_runs, 1, mean), order.by = index(ret_oos_runs))
  signal_oos <- xts(apply((pred_hat_oos_runs > 0), 1, mean), order.by = index((pred_hat_oos_runs > 0)))
  
  return(list(return = ret_oos_avg,
              signal = signal_oos,
              
              return_out_mat = ret_oos_runs,
              predicted_oos_mat = pred_hat_oos_runs,
              
              y_hat = arma_y_oos,
              eps_hat_avg = reclass(rowMeans(eps_hat_oos_runs), arma_y_oos),
              LPD = nn_results$LPD))
}
# par(mfrow = c(2,2))
# plot(xts(apply(pred_hat_oos_runs, 1, mean), order.by = index(pred_hat_oos_runs)))
# plot(test_data$rt_lag0)
# plot(arma_y_oos)
# plot(eps_hat_oos_avg)
