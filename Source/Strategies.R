SR_lines_strategy <- function() {

  SP <- frollapply(df[,.(low)],
                   testing_params$look_back[j],
                   function(x) support(x, n_sort = testing_params$n_sort[j]))
  RS <- frollapply(df[,.(high)], testing_params$look_back[j],
                   function(x) resistance(x, n_sort = testing_params$n_sort[j]))
  
  
  df[close > unlist(RS) , signal := "long"]
  df[close < unlist(SP) , signal := "short"]

}

SR_lines_strategy_breakout <- function() {
  
  SP <- frollapply(df[,.(low)],
                   testing_params$look_back[j],
                   function(x) support(x, n_sort = testing_params$n_sort[j],
                                      n_exclude = testing_params$n_exclude[j]))
  RS <- frollapply(df[,.(high)], testing_params$look_back[j],
                   function(x) resistance(x, n_sort = testing_params$n_sort[j],
                                          n_exclude =   testing_params$n_exclude[j]))
  
  
  df[close > unlist(RS) + (unlist(RS)* testing_params$per[j])  , signal := "long"]
  df[close < unlist(SP) - (unlist(SP)* testing_params$per[j]), signal := "short"]
  
}




RSI_strategy <- function() {
  
  df[, rsi := RSI(close, n = testing_params$rsi[j])]
  df[rsi > testing_params$rsi_top[j] , signal := "short"]
  df[rsi < testing_params$rsi_bot[j] , signal := "long"]
}


EMA_strategy <- function() {
  
  df[, ema := EMA(close, n = testing_params$ema[j])]
  df[close > ema , signal := "long"]
  df[close < ema , signal := "short"]
}





Calculate_spline <- function(x){

  smoothingSpline_fast = smooth.spline(x ~ as.numeric(1:length(x)) , spar = 0.5)
  spline <- tail(predict(smoothingSpline_fast)$y, 1)
  deriv <- tail(predict(smoothingSpline_fast, deriv = 1)$y, 1)
  return(spline)
}


Splines_deriv_strategy <- function() {
  
  spline <- frollapply(df[,.(close)],
                   n=500,
                   function(x) Calculate_spline(x))
  df[, deriv := spline]
  
  
  df[deriv > 0 , signal := "long"]
  df[deriv < 0 , signal := "short"]
  
}



SR_lines_strategy_breakout_luc <- function() {
  # close <- df[, close]
  # close_sp <- close[-( (length(close)-1):  length(close))]
  SP <- frollapply(df[, close],
                   testing_params$look_back[j],
                   function(x) support(x, n_sort = testing_params$n_sort[j],
                                       n_exclude =   testing_params$n_exclude[j]))
  

  df[close < unlist(SP) - (unlist(SP)* testing_params$per[j])  , signal := "long"]
  
  
}
