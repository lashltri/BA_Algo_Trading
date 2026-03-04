#--------------------------------Buy Hold---------------------------------------
strat_bh <- function(price) {
  price <- Ad(price)

  signal <- rep(1, length(price))
  signal <- reclass(signal, price)

  
  list(
    price    = price,
    signal   = signal,
    pred_price = NA
  )
}
