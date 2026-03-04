
feature_eng <- function(data, train_split, lags = 5, seasonal_lags = NULL){
  
  returns <- na.omit(diff(log(Ad(data))))
  
  # Laged Returns-----------------------------------
  lag_list <- lapply(0:lags,function(k) lag(returns, k = k))
  lag_mat <- do.call(cbind, lag_list)
  colnames(lag_mat) <- paste("rt_lag", 0:lags, sep = "")
  
  # additional seasonal lags------------------------
  if (!is.null(seasonal_lags)) {
    seas_list <- lapply(seasonal_lags, function(k) lag(returns, k = k))
    seas_mat  <- do.call(cbind, seas_list)
    colnames(seas_mat) <- paste("rt_lag", seasonal_lags, sep = "")
    lag_mat <- cbind(lag_mat, seas_mat)
  }
  
  #sd_t---------------------------------------------
  GARCH<-garchFit(~garch(1,1), data = returns[index(returns) <= train_split],
                  delta=2, include.delta=F,include.mean=T,  cond.dist = "sstd")
  
  sigma_t <- c(GARCH@sigma.t, 
                   sigma_t_oos(r_in = returns[index(returns) <= train_split],
                                GARCH = GARCH, 
                                r_out = returns[index(returns) > train_split])
                              )
  sigma_t <- reclass(sigma_t, returns)
  
  #final features------------------------------------
  data_mat <- cbind(lag_mat, sigma_t)
  data_mat <- na.omit(data_mat)
  
  return(data_mat)
}

#standartisation later bc of data leakage. future information would be used


sigma_t_oos<-function(r_out, GARCH, r_in)
{
  # Vola based on in-sample parameters
  sigma_t<-rep(NA,length(r_out))
  a<-GARCH@fit$coef["beta1"]
  b<-GARCH@fit$coef["alpha1"]
  d<-GARCH@fit$coef["omega"]
  mu <- GARCH@fit$coef["mu"]
  
  # First data point: based on last in-sample data
  sigma_t[1]<- sqrt(d + a*GARCH@sigma.t[length(GARCH@sigma.t)]^2 + b*(r_in[length(r_in)]-mu)^2)
  # On out-of-sample span
  for (i in 2:length(r_out))
  {
    sigma_t[i]<-sqrt(d+a*sigma_t[i-1]^2+b*(r_out[i-1]-mu)^2)
  }

  return(sigma_t)
}


#ssmi
# data = px_list$IXIC
# returns <- na.omit(diff(log(Ad(data))))
# train_split = max_train_date
# 
# par(mfrow = c(2,2))
# acf(returns[index(returns) <= train_split])
# acf(returns[index(returns) <= train_split]^2)
# 
# GARCH<-garchFit(~garch(1,1), data = returns[index(returns) <= train_split],
#                 delta=2, include.delta=F,include.mean=T,  cond.dist = "sstd")
# acf(GARCH@residuals/GARCH@sigma.t)             
             
