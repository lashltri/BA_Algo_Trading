simple_FNN <- function(feats, 
                       train_split,
                       valid_split, 
                       formula = "rt_lag0 ~.",
                       number_neurons = c(12,6),
                       num_sim = 100,
                       validate = TRUE){
  
  # initial training for validation_____________________________________________
  
  init_train_data <- feats[index(feats) <= train_split,]
  init_valid_data <- feats[index(feats) > train_split & index(feats) <= valid_split]
  
  # Scale Features----------------------
  feat_min <- apply(init_train_data, MARGIN = 2, FUN = min)
  feat_max <- apply(init_train_data, MARGIN = 2, FUN = max)
  scaled_feats_train <- scale(x = init_train_data,
                        scale = feat_max - feat_min,
                        center = feat_min)
  scaled_feats_valid <- scale(x = init_valid_data,
                        scale = feat_max - feat_min,
                        center = feat_min)
  
                        # Alternative scaling method to try               BUT ALSO FIX BACKTRANSFORMATION!!!!!!!!!!!!!!!!!!
                        # feat_mean <- colMeans(init_train_data)
                        # feat_sd <- apply(init_train_data, MARGIN = 2, FUN = sd)
                        # 
                        # scaled_feats <- scale(x = init_train_data,
                        #                       scale = feat_sd, center = feat_mean)
  

  

  #plot(nn)
  
  # Initial training in and out of sample
  MSE_mat<-matrix(ncol=2,nrow=num_sim)
  colnames(MSE_mat)<-c("In sample MSE","Out sample MSE")
  sharpe_nn<-sharpe_nn_in<-1:num_sim
  
  pb <- txtProgressBar(min = 1, max = num_sim, style = 3)
  
  for (i in 1:num_sim)#i<-1
  {
    nn.obj <- try(estimate_nn(train_set = scaled_feats_train,
                              test_set  = scaled_feats_valid,
                              stepmax = 1e+04),
                    silent = TRUE)
    if (class(nn.obj) == "try-error") next

    # MSE in Sample and OOS
    MSE_mat[i,] <- nn.obj$MSE_nn
    print(c(i,MSE_mat[i,]))
    
    # Trading performance in Sample and OOs
    perf_nn <- (nn.obj$predicted_nn > 0) * init_valid_data[,1]
    perf_nn_in <- (nn.obj$predicted_nn_in_sample > 0) * init_train_data[,1]
    
    if (i==1){  
      perf_nn_mat<-perf_nn
      perf_nn_mat_in<-perf_nn_in
    } else{
      perf_nn_mat<-cbind(perf_nn_mat,perf_nn)
      perf_nn_mat_in<-cbind(perf_nn_mat_in,perf_nn_in)
    }
    
    sharpe_nn[i]<- SharpeRatio.annualized(perf_nn, scale = 252, Rf = 0)
    sharpe_nn_in[i]<- SharpeRatio.annualized(perf_nn_in, scale = 252, Rf = 0)
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  cat("Mean IS MSE:", apply(MSE_mat,2,mean)[1], 
      "| Mean OOS MSE:", apply(MSE_mat,2,mean)[2], "\n")
  cat("Sd IS MSE:", apply(MSE_mat,2,sd)[1], 
      "| Sd OOS MSE:", apply(MSE_mat,2,sd)[2], "\n")
  
  plot((cumsum(perf_nn_mat)))
  cat("IS MSE vs. OOS Sharpe Ratio Correlation: ",
      cor(sharpe_nn, MSE_mat[,"In sample MSE"]), "\n")
  cat("IS Sharpe Ratio vs. OOS Sharpe Ratio Correlation: ",
      cor(sharpe_nn,sharpe_nn_in), "\n")
}




estimate_nn<-function(train_set, test_set, 
                      min_target = feat_min[1], max_target = feat_max[1],
                      num_neurons = number_neurons, f = formula){
  
  nn <- neuralnet(f, data = train_set,
                  hidden = num_neurons,linear.output = TRUE)
  
  
  # In sample performance
  predicted_scaled_in_sample<-nn$net.result[[1]]
  # Scale back from interval [0,1] to original log-returns
  predicted_nn_in_sample<-predicted_scaled_in_sample*(max_target-min_target)+min_target
  # In-sample MSE
  MSE.in.nn<-mean(((train_set[,1]-predicted_scaled_in_sample)*(max_target-min_target))^2)
  
  # Out-of-sample performance
  # Compute out-of-sample forecasts
  predicted_scaled <- predict(nn,as.matrix(test_set[,2:ncol(test_set)]))

  # Descaling for comparison
  predicted_nn <- predicted_scaled*(max_target-min_target)+min_target
  test.r <- test_set[,1]*(max_target-min_target)+min_target
  # Calculating MSE
  MSE.out.nn <- mean((test.r - predicted_nn)^2)
  
  # Compare in-sample and out-of-sample
  MSE_nn<-c(MSE.in.nn,MSE.out.nn)
  
  
  
  
  return(list(MSE_nn=MSE_nn,
              predicted_nn=predicted_nn,
              predicted_nn_in_sample=predicted_nn_in_sample))
  
}
