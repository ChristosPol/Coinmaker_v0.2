# # Testing strategies ------------------------------------------------------------
paraller_exec <- FALSE

# Badget 
initial_budget <- 200

# select period of data 
candles_recent <- as.data.table(klines[[1]])

# training data here is not the traditional training set, but more of a 
# placeholder initial dataset to calculate the indicators
train_n <- ceiling(nrow(candles_recent) / 10)

train_data <- candles_recent[1:train_n, ]

test_data <- candles_recent[(train_n + 1):nrow(candles_recent), ]

# Support and resistance strategy
myresult <- Support_Resistance(takeprofit = 0.015, stoploss_trail = 1000,stoploss_ult = 0.02
                               ,plot.it= T,n_sort = 3, roll= 50)

myresult <- simple_SMA(sma=130, takeprofit = 10000,stoploss_ult = 10000, plot.it = T) 

myresult <- splines_fast_slow_cross(spar_fast = 0.4,spar_slow = 0.8, takeprofit=0.02,stoploss_ult=0.02,plot.it=T)
  
myresult <- Splines_Tangent(takeprofit =0.02, stoploss_trail =0.02,stoploss_ult =0.02,
                            spar=0.7,plot.it=T, rsi_period=14)
  
calculate_profits(myresult)

