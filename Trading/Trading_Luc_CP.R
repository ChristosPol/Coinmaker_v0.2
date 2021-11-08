Sys.sleep(50)

# Source functions
path_source <- "/Users/christos.polysopoulos/Repositories/Coinmaker_v0.2/Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))

pair <- c("ADAEUR", "XETHZEUR", "BTCEUR", "XRPEUR", "LINKEUR",
           "ALGOEUR", "AAVEEUR")

params <- read_csv("/Users/christos.polysopoulos/Repositories/Coinmaker_v0.2/Trading/params.csv",
                   col_types = cols())
setDT(params)
initial_budget <- 20

print("-------------------------------------------")
print("-------------------------------------------")
print("-------------------------------------------")
print("Initiating loop")
print("-------------------------------------------")
print("-------------------------------------------")
print("-------------------------------------------")

i <- 2
for(i in 1:length(pair)){
  
  print("-------------------------------------------")
  print("-------------------------------------------")
  print("-------------------------------------------")
  print("-------------------------------------------")
  print("-------------------------------------------")
  print("-------------------------------------------")
  
  print( paste0("Reading parameters for ", pair[i], "..."))
  
  look_back <- params[pair == pair[i], look_back]
  n_sort <- params[pair == pair[i], n_sort]
  rsi_below <- params[pair == pair[i], rsi_below]
  rsi_n <- params[pair == pair[i], n_rsi]
  per <- params[pair == pair[i], per]
  tp <- params[pair == pair[i], tp]/100
  sl <- -params[pair == pair[i], sl]/100
  n_exclude <- params[pair == pair[i], n_exclude]
  
  print( paste0("Reading action table for ", pair[i], "..."))
  trades <- read.table(paste0("/Users/christos.polysopoulos/Repositories/Coinmaker_v0.2",
                              "/Trading",
                              "/action_tables",
                              paste0("/action_table_",pair[i], ".csv") ), sep =","
                       , header = T)
  setDT(trades)

  print( paste0("Getting OHLC for ", pair[i], "..."))
  repeat{
    tryCatch({
      df <- simple_OHLC(interval = 60, pair = pair[i])
      df <- df[-720, ]
      # print(tail(df,1))
      # Hourly
      curr_hour <- as.numeric(substr(as.character(Sys.time()), 12,13))
      previous_hour <- as.numeric(substr(tail(df$Date_POSIXct, 1), 12,13))
      
      # curr_hour <- as.numeric(substr(as.character(Sys.time()), 15,16))
      if(curr_hour == 0){
        curr_hour <- 24
      }
      # previous_hour <- as.numeric(substr(tail(df$Date_POSIXct, 1), 15,16))
      # print(curr_hour)
      # print(previous_hour)
      mybreak <- curr_hour-previous_hour
      if(mybreak == 1) break;
    }, error = function(e) { return(NA) })
  }
  print( paste0("OHLC retrieved for ", pair[i], "..."))
  print(tail(df, 1))
  
  print( paste0("Trading process started for ", pair[i], "..."))
  tryCatch({
    SP <- support(tail(df$close, look_back), n_sort = n_sort,
                  n_exclude =   n_exclude)
    # plot(df$close, type= "l")
    # abline(h = SP)
    df[, rsi := RSI(close, rsi_n)]
    
    last_close <- df[, tail(close, 1)]
    last_action <- trades[, tail(action, 1)]
    trades[, Support := SP]
    trades[, Rsi := tail(df$rsi, 1)]
    trades[, current_price := last_close]
    trades[, Support_enter_level := Support - Support * per]
    
    # Percentage position
    if (last_action %in% c("enter_long", "keep_long")){
      trades[, pos_perc := (last_close - price_enter) / price_enter]
    }  else {
      trades[, pos_perc := 0]
    }
    
    # Exit condition
    trades[,exit_condition :=  trades[, pos_perc] > tp | trades[, pos_perc] < sl]
    
    
    # Enter long
    if(last_close <  trades[, Support_enter_level] &  trades[, Rsi] < rsi_below  & last_action == "no_action") {
      
      # Give API Order to buy at market
      buy_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                                 key = API_Key, secret = API_Sign, pair = pair[i], type = "buy",
                                 ordertype = "market", volume = initial_budget / last_close)
      
      trades[, current_price := last_close]
      trades[, price_enter := last_close]
      trades[, at := Sys.time()]
      trades[, action := "enter_long"]
      trades[, volume_enter := readr::parse_number(as.character(buy_it$result$descr))]
      trades[, id := buy_it$result$txid]
      
      # trades[, id := round(runif(1, 0, 1000))]
      
      write.table(trades, paste0("/Users/christos.polysopoulos/Repositories/Coinmaker_v0.2/Trading", "/trades.csv"), append = T, sep =",", row.names = F, col.names = F)
      
  
      
    } else if ( (trades[, exit_condition == FALSE]) &
                trades[, action] %in% c("enter_long", "keep_long")) {
      
      trades[, current_price := last_close]
      trades[, at := Sys.time()]
      trades[, action := "keep_long"]
      
      
    } else if (trades[, exit_condition == TRUE] &
               trades[, action] %in% c("enter_long", "keep_long")){
      
      # crypto_hold <- get_balance(url = "https://api.kraken.com/0/private/Balance",
      #                            key = API_Key, secret = API_Sign)
      # crypto_hold_eth <- as.numeric(crypto_hold$result$XETH)
      #
      sell_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                                  key = API_Key, secret = API_Sign, pair = pair[i], type = "sell",
                                  ordertype = "market", volume = trades[, volume_enter])
      
      trades[, current_price := last_close]
      trades[, price_exit := last_close]
      trades[, at := Sys.time()]
      trades[, action := "exit_long"]
      trades[, id := sell_it$result$txid]
      
      write.table(trades, paste0("/Users/christos.polysopoulos/Repositories/Coinmaker_v0.2/Trading", "/trades.csv"), append = T, sep =",", row.names = F, col.names = F)
      
    } else {
      
      trades[, current_price := last_close]
      trades[, at := Sys.time()]
      trades[, action := "no_action"]
      
    }
    write.csv(trades, paste0("/Users/christos.polysopoulos/Repositories/Coinmaker_v0.2",
                             "/Trading", 
                             "/action_tables",
                             paste0("/action_table_",pair[i], ".csv")), row.names=FALSE)
  }, error = function(e) { return(NA) })
  print( paste0("Trading process finished for ", pair[i], "..."))
  print(trades)
  Sys.sleep(3)
}




