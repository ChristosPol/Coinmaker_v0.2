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
myresult <- Support_Resistance(takeprofit = 10000, stoploss_trail = 1000,stoploss_ult = 1000
                               ,plot.it= T,n_sort = 10, roll= 100)

myresult <- Splines_Tangent(takeprofit =0.04, stoploss_trail =0.2,stoploss_ult =0.02,
                            spar=0.7,plot.it=T, rsi_period=14)

calculate_profits(myresult)
head(df$close, 1)
tail(df$close, 1)
View(myresult)
myresult$x <- 1:nrow(myresult)
tail(myresult$Price)
plot(myresult$close, type = "l")
lines(myresult$SMA, col = "red")
points(myresult$x[myresult$action == "buy"], myresult$close[myresult$action == "buy"], pch = 19, col ="green")
points(myresult$x[myresult$action == "sell"], myresult$close[myresult$action == "sell"], pch = 19, col ="red")
