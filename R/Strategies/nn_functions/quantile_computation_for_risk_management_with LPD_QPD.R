
generate_quantile_LPD_adjusted_performance_func<-function(LPD_t,k,y,quantile_select,length_roll_quantile,x)
{
# Extract column k  
  xyz<-LPD_t[,k]
# Make an xts object  
# The dates correspond to x_test since LPD is based on x_test (its the same as y_test...)
  names(xyz)<-index(x)
  xyz<-as.xts(xyz)
# The dates correspond to x_test since LPD is based on x_test
  index(xyz)<-index(x)
# Apply trading function  
  weight_obj<-weight_trade_func(xyz,quantile_select,length_roll_quantile)
# Long if LPD is below upper quantile: leads to short signals during up-turns (bad)
  weight_trade_up<-weight_obj$weight_trade_up
# Long if LPD is below upper quantile and above lower quantile: short during down-turns (good) and up-turns (bad)
  weight_trade<-weight_obj$weight_trade_two_side
# Long if LPD is above lower quantile: leads to short signals during down-turns (good)
  weight_trade_low<-weight_obj$weight_trade_low
# Upper and lower quantiles  
  roll_quant_up=weight_obj$roll_quant_up
  roll_quant_low=weight_obj$roll_quant_low
# Performance when being in long position outside critical time points identified by LPD
# Note that rolling quantile is based on lagged data (LPD is based on x_test and is therefore lagged with respect to y_test).
  perf_weight_low<-cumsum(na.exclude(y*weight_trade_low))
  perf_weight_up<-cumsum(na.exclude(y*weight_trade_up))
# Synchronize all time series on common time span and remove NAs
# We lag LPD (xyz) by -1 because LPD is originally lagged by one day with respect to y: 
#  pulling it back one day allows for consistent plots (it is just for plots...)  
  mplot_all<-na.exclude(as.matrix(cbind(cumsum(y),perf_weight_low,perf_weight_up,weight_trade_up,weight_trade_low,roll_quant_up,roll_quant_low,lag(xyz,k=-1))))
  colnames(mplot_all)<-c("buy-and-hold","perf_weight_low","perf_weight_up","weight_trade_up","weight_trade_low","roll_quant_up","roll_quant_low","LPD")
# Same as above but as xts object
# We lag LPD (xyz) by -1 because LPD is originally lagged by one day with respect to y: 
#  pulling it back one day allows for consistent plots (it is just for plots...)  
  mplot_all_xts<-na.exclude(cbind(cumsum(y),perf_weight_low,perf_weight_up,weight_trade_up,weight_trade_low,roll_quant_up,roll_quant_low,lag(xyz,k=-1)))
  colnames(mplot_all_xts)<-c("buy-and-hold","perf_weight_low","perf_weight_up","weight_trade_up","weight_trade_low","roll_quant_up","roll_quant_low","LPD")
  
  return(list(mplot_all=mplot_all,mplot_all_xts=mplot_all_xts,weight_trade_low=weight_trade_low,weight_trade_up=weight_trade_up,roll_quant_up=roll_quant_up,roll_quant_low=roll_quant_low))
}



# Determine trading signals based on rolling quantiles
#   No cheating: real-time
#   xyz<-std_mean
# quantile_select<-0.5
weight_trade_func<-function(xyz,quantile_select,length_roll_quantile)
{
# Rolling quantile  
# New own code which can be replicated and does it right (checks if width is too large at start and truncates)
  roll_quant_up<-apply.rolling_marcsmodified(xyz,width=length_roll_quantile,p=quantile_select)
  roll_quant_low<-apply.rolling_marcsmodified(xyz,width=length_roll_quantile,p=1-quantile_select)
# Weight is one if xyz smaller than upper quantile or xyz larger than lower quantile
# we use lagged quantile because a current negative outlier will pull-down the quantile, too, which is 'bad' idea
#   This has almost no effect because it affects the quantile (and not the current observation which is compared to quantile)
# We do not lag the weight_trade series because they are based on LPD which is in turn based on x_test 
#   which is already lagged when compared to y_test (the latter is traded by weighting with the weight_trade series)
  weight_trade_up<-(xyz<=lag(roll_quant_up))
  weight_trade_low<-(xyz>=lag(roll_quant_low))
  weight_trade_two_side<-(xyz<=lag(roll_quant_up))&(xyz>=lag(roll_quant_low))
  
  if (F)
  {
# Weight is one if xyz smaller than UN-lagged quantile: nearly the same as above: skipped
    weight_trade_up<-(xyz<=(roll_quant_up))
    weight_trade_low<-(xyz>=(roll_quant_low))
    weight_trade_two_side<-(xyz<=(roll_quant_up))&(xyz>=(roll_quant_low))
  }
  
  return(list(weight_trade_up=weight_trade_up,weight_trade_low=weight_trade_low,weight_trade_two_side=weight_trade_two_side,roll_quant_up=roll_quant_up,roll_quant_low=roll_quant_low))
  
}


#width<-length_roll_quantile
# p<-1-quantile_select
apply.rolling_marcsmodified<-function(xyz,width,p)
{
  R = checkData(xyz)
  R = na.omit(R)
  rows = NROW(R)
  head(R)
  result = xts(, order.by = time(R))
  dates = time(R)
  calcs = matrix()
  if (width == 0) {
    gap = gap
  } else
  {
    gap = width
  }
  for (row in 1:nrow(R))#row<-100
  {
    if (width == 0)
    {
      r = R[1:row, ]
    } else
    {
      # Truncate at start if width is too large
      r = R[max(1,(row - width + 1)):row, ]
    }
    calc = apply(r, MARGIN = 2, FUN = "quantile",probs=p)
    calcs = rbind(calcs, calc)
  }
  
  calcs = xts(calcs[-1], order.by = dates[1:nrow(R)])
  result = merge(result, calcs)
  result = reclass(result, R)
  return(result)
}


