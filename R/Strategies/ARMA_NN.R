train_data <- FEATS$ARMA_NN$SSMI[index(FEATS$ARMA_NN$SSMI) < "2018-12-31", ]
test_data <- FEATS$ARMA_NN$SSMI[index(FEATS$ARMA_NN$SSMI) > "2018-12-31", ]

ARMA_FNN <- function(train_data, test_data) {
  arma_y_is  <- train_data$arma_y_t
  arma_y_oos <- test_data$arma_y_t
  
  # eps_is <- train_data[, c("eps_t")]
  # eps_oos <- test_data[, c("eps_t")]
  
  nn_train <- train_data[, c("eps_t", "arma_y_t", "garch_sd_t")]
  nn_test  <- test_data[, c("eps_t", "arma_y_t", "garch_sd_t")]
  
  nn_results <- simple_FNN(train_data = nn_train, test_data = nn_test, 
                           number_neurons = c(12, 6), num_sim = 10)
  
  eps_hat_oos_runs <- nn_results$predicted_oos_mat
  eps_hat_oos_avg <- xts(rowMeans(nn_results$predicted_oos_mat),
                         order.by = index(nn_results$predicted_oos_mat))
  
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
              eps_hat_avg = reclass(rowMeans(eps_hat_oos_runs), arma_y_oos)))
}

par(mfrow = c(2,2))
plot(xts(apply(pred_hat_oos_runs, 1, mean), order.by = index(pred_hat_oos_runs)))
plot(test_data$rt_lag0)
plot(arma_y_oos)
plot(eps_hat_oos_avg)
