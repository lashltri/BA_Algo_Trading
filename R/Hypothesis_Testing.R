# 
# ## ---- std_returns-fn
# standardized_returns <- function(returns = BACKT, portf = PORTF) {
#   #--------- std returns for each asset ----------
#   
#   #time varying standarddeviation
#   sigma_t <- lapply(names(returns$BH), function(sym) {
#     fit <- garchFit(~garch(1,1),
#                     data = returns$BH[[sym]],
#                     delta = 2,
#                     include.delta = FALSE,
#                     include.mean = TRUE,
#                     cond.dist = "sstd",
#                     trace = FALSE)
#     
#     # Use reclass to attach returns dates
#     reclass(fit@sigma.t, returns$BH[[sym]])
#   })
#   
#   names(sigma_t) <- names(returns$BH)
#   
#   
#   standard_ret <- lapply(returns, function(strategy) {
#                       Map(function(r, sd) {
#                         m <- na.omit(merge(r, sd, join = "inner"))
#                         z <- m[, 1] / m[, 2]
#                       }, strategy, sigma_t[names(strategy)])
#                     })
#   
#   # --------- add std portfolio inside each strategy ----------
#   for (strat in  names(portf)) {
#     p_log <- na.omit(portf[,strat])
#     
#     fit_p <- garchFit(~ garch(1,1),
#                       data = p_log,
#                       delta = 2, include.delta = FALSE,
#                       include.mean = TRUE,
#                       trace = FALSE)
#     sigma_p <- reclass(fit_p@sigma.t, p_log)
#     
#     z_p <- p_log / sigma_p
#     colnames(z_p) <- "Portfolio"
#     
#     # Add portfolio under ticker name "PORTF"
#     standard_ret[[strat]][["Portfolio"]] <- z_p
#   }
#   
#   return(standard_ret)
# }

## ---- std_returns-fn
standardized_returns <- function(returns = BACKT, portf = PORTF) {
  #--------- 1) GARCH(1,1)-Sigma für BUY & HOLD pro Ticker ----------
  sigma_bh <- lapply(names(returns$BH), function(sym) {
    fit <- garchFit(~ garch(1,1),
                    data          = returns$BH[[sym]],
                    delta         = 2,
                    include.delta = FALSE,
                    include.mean  = TRUE,
                    trace         = FALSE)
    reclass(fit@sigma.t, returns$BH[[sym]])
  })
  names(sigma_bh) <- names(returns$BH)
  
  
  #--------- 2) standardisierte Returns pro Strategie ----------
  standard_ret <- list()
  
  for (strat_name in names(returns)) {
    strategy <- returns[[strat_name]]
    
    if (strat_name == "AG") {
      # ----- ARMA-GARCH: GARCH(1,1) auf STRATEGIE-RETURNS -----
      std_list <- lapply(strategy, function(r) {
        # r = arma-garch strategy returns (signal * log_ret_out)
        fit_ag <- garchFit(~ garch(1,1),
                           data          = r,
                           delta         = 2,
                           include.delta = FALSE,
                           include.mean  = TRUE,
                           trace         = FALSE)
        
        sigma_ag <- reclass(fit_ag@sigma.t, r)
        m <- na.omit(merge(r, sigma_ag, join = "inner"))
        z <- m[, 1] / m[, 2]
        colnames(z) <- "z"
        z
      })
      
    } else {
      # ----- alle anderen Strategien: BH-Sigma pro Ticker -----
      std_list <- Map(function(r, sd) {
        m <- na.omit(merge(r, sd, join = "inner"))
        z <- m[, 1] / m[, 2]
        colnames(z) <- "z"
        z
      }, strategy, sigma_bh[names(strategy)])
    }
    
    standard_ret[[strat_name]] <- std_list
  }
  
  
  #--------- 3) standardisierte Portfolio-Returns pro Strategie ----------
  for (strat in names(portf)) {
    p_log <- na.omit(portf[[strat]])
    
    fit_p <- garchFit(~ garch(1,1),
                      data          = p_log,
                      delta         = 2,
                      include.delta = FALSE,
                      include.mean  = TRUE,
                      trace         = FALSE)
    
    sigma_p <- reclass(fit_p@sigma.t, p_log)
    m_p <- na.omit(merge(p_log, sigma_p, join = "inner"))
    z_p <- m_p[, 1] / m_p[, 2]
    colnames(z_p) <- "Portfolio"
    
    standard_ret[[strat]][["Portfolio"]] <- z_p
  }
  
  return(standard_ret)
}





## ---- t_value-fn
paired_t_value <- function(std_ret_A, std_ret_B) {
  
  # align by ticker
  common_tickers <- intersect(names(std_ret_A), names(std_ret_B))
  
  # t-value per ticker
  t_values <- lapply(common_tickers, function(tk) {
    A <- std_ret_A[[tk]]
    B <- std_ret_B[[tk]]
    
    # merge standardized returns on common dates
    m <- na.omit(merge(A, B, join = "inner"))
    d_t <- m[, 1] - m[, 2]
    d_bar <- mean(d_t)
    s_d   <- sd(d_t)
    n     <- NROW(d_t)
    
    t_val <- sqrt(n) * d_bar / s_d
    p_val = 2* (1-pt(abs(t_val), df = n-1))
    return(data.frame(t_val, p_val, ticker = tk))
  })
  
  return(do.call(rbind, t_values))
}

