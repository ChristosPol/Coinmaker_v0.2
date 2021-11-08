rm(list=ls())
# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)


look_back <- 80
n_sort <- 16


initial_budget <- 10
pair <- "XETHZEUR"
tp <- 0.015
sl <- -0.01
trades <- data.table(current_price = as.numeric(rep(NA, length(pair))), price_enter = as.numeric(rep(NA, length(pair))),
                     price_exit = as.numeric(rep(NA, length(pair))),volume_enter = NA ,exit_condition = FALSE,  
                     RS = NA, SP = NA, at = rep(Sys.time(), length(pair)),
                     action = rep("no_action", length(pair)), pos_perc = as.numeric(rep(NA, length(pair))),
                     pair  = pair, id = as.character(rep(NA, length(pair))))



repeat {
  
  tryCatch({
  
  df <- simple_OHLC(interval = 60, pair = pair)
  SP_point <- mean(head(sort(df[, tail(low, look_back)]), n_sort))
  RS_point <- mean(head(sort(df[, tail(high, look_back)], decreasing = TRUE), n_sort))
  last_close <- df[, tail(close, 1)]
  last_action <- trades[, tail(action, 1)]
  trades[, SP := SP_point]
  trades[, RS := RS_point]
  trades[, current_price := last_close]
  # last_close <- 2500
  
  # Percentage position
  if (last_action %in% c("enter_long", "keep_long")){
    trades[, pos_perc := (last_close - price_enter) / price_enter]
  } else if (last_action %in% c("enter_short", "keep_short")){
    trades[, pos_perc := -(current_price - price_enter) / price_enter  ]
  } else {
    trades[, pos_perc := 0]
  }
  
  # Exit condition
  trades[,exit_condition :=  trades[, pos_perc] > tp | trades[, pos_perc] < sl]
  
  
  # Enter long
  if(last_close >  RS_point & last_action == "no_action") {
    
    # Give API Order to buy at market
    buy_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                               key = API_Key, secret = API_Sign, pair = pair, type = "buy",
                               ordertype = "market", volume = initial_budget / last_close)
    trades[, current_price := last_close]
    trades[, price_enter := last_close]
    trades[, at := Sys.time()]
    trades[, action := "enter_long"]
    trades[, volume_enter := readr::parse_number(as.character(buy_it$result$descr))]
    trades[, id := buy_it$result$txid]
    
    # trades[, id := round(runif(1, 0, 1000))]
  
    write.table(trades, paste0(path_source, "/trades.csv"), append = T, sep =",", row.names = F, col.names = F)
    
    
    
    # crypto_hold <- get_balance(url = "https://api.kraken.com/0/private/Balance",
    #                            key = API_Key, secret = API_Sign)
    # crypto_hold_eth <- as.numeric(crypto_hold$result$XETH)
    # # # Give API Order to buy at market
    # sell_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
    #                             key = API_Key, secret = API_Sign, pair = pair, type = "sell",
    #                             ordertype = "market", volume = crypto_hold_eth)
    # sell_it$result$txid
    # 
    
    #Enter short
  } else if (last_close < SP_point & last_action == "no_action") {
    
    sell_it <- add_market_order_short(url = "https://api.kraken.com/0/private/AddOrder",
                           key = API_Key, secret = API_Sign, pair = pair, type = "sell",
                           ordertype = "market", volume = initial_budget / last_close)
    
    trades[, current_price := last_close]
    trades[, price_enter := last_close]
    trades[, at := Sys.time()]
    trades[, action := "enter_short"]
    trades[, id := sell_it$result$txid]
    trades[, volume_enter := readr::parse_number(as.character(sell_it$result$descr))]
    
    # trades[, id := round(runif(1, 0, 1000))]
    write.table(trades, paste0(path_source, "/trades.csv"), append = T, sep =",", row.names = F, col.names = F)
    
    
  } else if ( (trades[, exit_condition == FALSE]) &
              trades[, action] %in% c("enter_long", "keep_long")) {
    
  trades[, current_price := last_close]
  trades[, at := Sys.time()]
  trades[, action := "keep_long"]
  
  
  # trades[, pos_perc := (current_price - price_enter) / price_enter  ]
  } else if (trades[, exit_condition == FALSE] &
             trades[, action] %in% c("enter_short", "keep_short")) {
  
  trades[, current_price := last_close]
  trades[, at := Sys.time()]
  trades[, action := "keep_short"]
  
  
  # exit long
  } else if (trades[, exit_condition == TRUE] &
             trades[, action] %in% c("enter_long", "keep_long")){
    
    # crypto_hold <- get_balance(url = "https://api.kraken.com/0/private/Balance",
    #                            key = API_Key, secret = API_Sign)
    # crypto_hold_eth <- as.numeric(crypto_hold$result$XETH)
    # 
    sell_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                                key = API_Key, secret = API_Sign, pair = pair, type = "sell",
                                ordertype = "market", volume = trades[, volume_enter])
    # sell_it$result$txid
    
    trades[, current_price := last_close]
    trades[, at := Sys.time()]
    trades[, action := "exit_long"]
    trades[, id := sell_it$result$txid]
    
    write.table(trades, paste0(path_source, "/trades.csv"), append = T, sep =",", row.names = F, col.names = F)
    
    
    # exit short
  } else if (trades[, exit_condition == TRUE] &
             trades[, action] %in% c("enter_short", "keep_short")){
    
    buy_it <- add_market_order_short(url = "https://api.kraken.com/0/private/AddOrder",
                                      key = API_Key, secret = API_Sign, pair = pair, type = "buy",
                                      ordertype = "market", volume = trades[, volume_enter])
    
    trades[, current_price := last_close]
    trades[, at := Sys.time()]
    trades[, action := "exit_short"]
    trades[, id := buy_it$result$txid]
    
    write.table(trades, paste0(path_source, "/trades.csv"), append = T, sep =",", row.names = F, col.names = F)
    
    # Do nothing
  } else {
    
    trades[, current_price := last_close]
    trades[, at := Sys.time()]
    trades[, action := "no_action"]
    
  }
  
    # short_volume <- readr::parse_number(sell_it$result$descr$order)
    
    
    # buy_it <- add_market_order_short(url = "https://api.kraken.com/0/private/AddOrder",
                                      # key = API_Key, secret = API_Sign, pair = pair, type = "buy",
                                      # ordertype = "market", volume = short_volume)
    
  #   
  # } else if (last action %in% long, short & exit condition == false) {
  #   
  #   no action
  # } else {
  #   
  #   no action
  # }
  # 
  
  # print(paste0("support is : ", SP))
  # print(paste0("price is : ", df[,tail(close, 1)]))
  # print(paste0("resistance is : ", RS))
  }, error = function(e) { return(NA) })
  Sys.sleep(5)
  print(trades)
}


