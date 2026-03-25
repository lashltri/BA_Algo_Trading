
# ---- Base features (lags etc.) ----------------------------------------
feature_eng <- function(data, train_split, lags = 5, seasonal_lags =  NULL){
  
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
  
  # ---- rolling features ----------------------------------------------------
  ret_lag1 <- lag(returns, 1)

  roll_mean_5  <- rollapplyr(ret_lag1, width = 5,  FUN = mean, fill = NA)
  roll_sd_5    <- rollapplyr(ret_lag1, width = 5,  FUN = sd,   fill = NA)

  roll_mean_10 <- rollapplyr(ret_lag1, width = 10, FUN = mean, fill = NA)
  roll_sd_10   <- rollapplyr(ret_lag1, width = 10, FUN = sd,   fill = NA)

  roll_mean_20 <- rollapplyr(ret_lag1, width = 20, FUN = mean, fill = NA)
  roll_sd_20   <- rollapplyr(ret_lag1, width = 20, FUN = sd,   fill = NA)
  
  cum_ret_5  <- rollapplyr(ret_lag1, width = 5,  FUN = sum, fill = NA)
  cum_ret_10 <- rollapplyr(ret_lag1, width = 10, FUN = sum, fill = NA)
  cum_ret_20 <- rollapplyr(ret_lag1, width = 20, FUN = sum, fill = NA)
  

  roll_feats <- cbind(
    roll_sd_5, roll_sd_10, roll_sd_20,
    roll_mean_5, roll_mean_10, roll_mean_20,
    cum_ret_5, cum_ret_10, cum_ret_20
  )
  
  colnames(roll_feats) <- c(
    "roll_sd_5", "roll_sd_10", "roll_sd_20",
    "roll_mean_5", "roll_mean_10", "roll_mean_20",
    "cum_ret_5", "cum_ret_10", "cum_ret_20"
  )
  
  # ARMA-GARCH on training window only -----------------------------------
  r_in  <- returns[index(returns) <= train_split]
  r_out <- returns[index(returns) >  train_split]
  
  AG <- garchFit(~ arma(1,0) + garch(1,1),
                 data = r_in,
                 delta = 2,
                 include.delta = FALSE,
                 include.mean = TRUE,
                 cond.dist = "sstd",
                 trace = FALSE)
  
  # in-sample objects
  mu_in    <- fitted(AG)
  sigma_in <- AG@sigma.t
  eps_in   <- residuals(AG)
  
  # recursive OOS forecasts
  ag_oos <- arma_garch_oos(r_out = r_out, AG = AG, r_in = r_in)
  
  arma_mu_t    <- reclass(c(mu_in, ag_oos$mu), returns)
  garch_sd_t <- reclass(c(sigma_in, ag_oos$sigma), returns)
  eps_t   <- reclass(c(eps_in,   ag_oos$eps),   returns)   #EPS t not lagged can only be used as a target variable!!!!!!!!!!!!!!
  eps_lag1  <- reclass(lag(eps_t, k = 1),   returns)
  eps_lag2  <- reclass(lag(eps_t, k = 2),   returns)
  
  ag_feats <- cbind(arma_mu_t, garch_sd_t, eps_t, eps_lag1, eps_lag2)
  colnames(ag_feats) <- c("arma_mu_t", "garch_sd_t", "eps_t", "eps_lag1", "eps_lag2")
  
  #final features------------------------------------------
  data_mat <- cbind(lag_mat, roll_feats, ag_feats)
  data_mat <- na.omit(data_mat)
  
  return(data_mat)
}






#--------------------------------- Helper---------------------------------------
arma_garch_oos<-function(r_out, AG, r_in)
{
  mu_t    <- rep(NA, length(r_out))
  sigma_t <- rep(NA, length(r_out))
  eps_t   <- rep(NA, length(r_out))
  
  # Vola based on in-sample parameters
  sigma_t<-rep(NA,length(r_out))
  beta<-AG@fit$coef["beta1"]
  alpha<-AG@fit$coef["alpha1"]
  omega<-AG@fit$coef["omega"]
  mu <- AG@fit$coef["mu"]
  ar <- if ("ar1" %in% names(AG@fit$coef)) AG@fit$coef["ar1"] else 0
  ma <- if ("ma1" %in% names(AG@fit$coef)) AG@fit$coef["ma1"] else 0
  
  r_prev     <- as.numeric(last(r_in))
  eps_prev   <- as.numeric(last(residuals(AG)))
  sigma_prev <- as.numeric(tail(AG@sigma.t, 1))
  
   # On out-of-sample span
  for (i in 1:length(r_out))
  {
    mu_t[i] <- mu + ar * r_prev + ma * eps_prev
    sigma_t[i] <- sqrt(omega + alpha * eps_prev^2 + beta * sigma_prev^2)
    eps_t[i] <- as.numeric(r_out[i]) - mu_t[i]
    
    # update recursion
    r_prev     <- as.numeric(r_out[i])
    eps_prev   <- eps_t[i]
    sigma_prev <- sigma_t[i]
  }

  return(list(mu = mu_t, sigma = sigma_t, eps = eps_t))
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
             
