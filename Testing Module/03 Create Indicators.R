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

calculate_profits(myresult)

