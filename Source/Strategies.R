SR_lines_strategy <- function() {

  SP <- frollapply(df[,.(low)],
                   testing_params$look_back[j],
                   function(x) support(x, n_sort = testing_params$n_sort[j]))
  RS <- frollapply(df[,.(high)], testing_params$look_back[j],
                   function(x) resistance(x, n_sort = testing_params$n_sort[j]))
  
  
  df[close > unlist(RS) , signal := "long"]
  df[close < unlist(SP) , signal := "short"]

}


RSI_strategy <- function() {
  
  df[, rsi := RSI(close, n = testing_params$rsi[j])]
  df[rsi > testing_params$rsi_top[j] , signal := "short"]
  df[rsi < testing_params$rsi_bot[j] , signal := "long"]
  
  
}
