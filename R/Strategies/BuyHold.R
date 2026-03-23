#--------------------------------Buy Hold---------------------------------------
strat_bh <- function(price) {
  price <- Ad(price)

  signal <- rep(1, length(price))
  signal <- reclass(signal, price)
  
  ret <- suppressWarnings(diff(log(price)))
  
  list(
    return    = ret,
    signal   = signal)
}
