rm(list = ls())
.rs.restartR()
options(scipen=999)

# Source functions
path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
path_values <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/current_values/"

# Choose pair and interval
pair <- c("ETH/EUR", "ADA/EUR", "BTC/EUR", "DOT/EUR", "XRP/EUR", "LINK/EUR", "LTC/EUR",
          "OXT/EUR", "LSK/EUR")
# , 
# "SC/EUR", "TRX/EUR", "GRT/EUR", "DOGE/EUR", "STORJ/EUR",
# "FIL/EUR", "KSM/EUR" ,"MANA/EUR", "KAVA/EUR", "ALGO/EUR", "FLOW/EUR", "UNI/EUR",
# "ICX/EUR", "CRV/EUR", "BCH/EUR",  "ATOM/EUR", "MLN/EUR", "XTZ/EUR")
vol <- c(0.01,
         25,
         0.0003,
         0.5,
         39.40,
         0.80,
         0.12,
         31.93,
         5)
# ,
# 1500,300,
# 20, 300,
# 20, 0.17,
# 0.04, 50, 5,
# 15, 1,
# 1, 10, 10,
# 0.03, 1, 0.26,
# 3)

interval = 240

# params <- read.csv("params.csv")

# API info
api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading", "API_Keys.txt", sep = "/"), sep = ";", header = T)
API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)


# Initiate websocket stream --- In screen
# source("Websocket.R")

trades <- data.table(current_price = as.numeric(rep(NA, length(pair))), price_bought = as.numeric(rep(NA, length(pair))),
                     price_sold = as.numeric(rep(NA, length(pair))), at = rep(Sys.time(), length(pair)),
                     action = rep("no_action", length(pair)), pos_perc = as.numeric(rep(NA, length(pair))),
                     exit = rep(NA, length(pair)), signal = as.numeric(rep(NA, length(pair))), pair  = pair,
                     id = as.character(rep(NA, length(pair))))

repeat{
  
  tryCatch({
    
    for (i in 1:length(pair)) {
      
      dat <- read.csv(paste0(path_values, gsub("/", "", pair[i]) ,"_val.csv"))
      signal <- dat$x
      latest_val <- dat$val
      
      if((signal >= 3| signal <= -10) & (trades[pair == pair[i], action] == "no_action"  | trades[pair == pair[i], action] == "sold")) {
        
        # Give API Order to buy at market
        buy_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                                   key = API_Key, secret = API_Sign, pair = pair[i], type = "buy",
                                   ordertype = "market", volume = vol[i])
        
        trades[pair == pair[i], "price_bought"] <- latest_val
        trades[pair == pair[i], "current_price"] <- latest_val
        trades[pair == pair[i], "at"] <- Sys.time()
        trades[pair == pair[i], "action"] <- "long"
        trades[pair == pair[i], "pos_perc"] <- 0
        trades[pair == pair[i], "id"] <- buy_it$result$txid
        trades[pair == pair[i], "signal"] <- signal
        write.table(buy_it$result$txid, "ids.csv", append = T, sep =",", row.names = F, col.names = F)
        
      } else if (( trades[pair == pair[i], "pos_perc"] >= 2 | trades[pair == pair[i], "pos_perc"] < -3) &  trades[pair == pair[i], action] %in% c("long", "keep") ){
        
        # # Give API Order to buy at market
        sell_it <- add_market_order(url = "https://api.kraken.com/0/private/AddOrder",
                                    key = API_Key, secret = API_Sign, pair = pair[i], type = "sell",
                                    ordertype = "market", volume = vol[i])
        
        trades[pair == pair[i], action := "sold"]
        trades[pair == pair[i], price_sold := latest_val]
        trades[pair == pair[i], current_price := latest_val]
        trades[pair == pair[i], at := Sys.time()]
        trades[pair == pair[i], pos_perc := (price_bought - current_price)/price_bought *100 ] 
        trades[pair == pair[i], "signal"] <- signal
        trades[pair == pair[i], "id"] <- sell_it$result$txid
        write.table(sell_it$result$txid, "ids.csv", append = T, sep =",", row.names = F, col.names = F)
        
      } else if (trades[pair == pair[i], action] %in% c("long", "keep") &  trades[pair == pair[i], "pos_perc"] < 2  ) {
        
        trades[pair == pair[i], action := "keep"]
        trades[pair == pair[i], current_price := latest_val]
        trades[pair == pair[i], at := Sys.time()]
        trades[pair == pair[i], price_bought := trades[pair == pair[i], price_bought]]
        trades[pair == pair[i], pos_perc := (current_price - price_bought)/price_bought *100 ]
        trades[pair == pair[i], "signal"] <- signal
        
      } else {
        
        trades[pair == pair[i], action := "no_action"]
        trades[pair == pair[i], current_price := latest_val]
        trades[pair == pair[i], at := Sys.time()]
        trades[pair == pair[i], pos_perc := NA]
        trades[pair == pair[i], "signal"] <- signal
      }
    }
    
  }, error = function(e) { return(NA) })
  print(trades)
  Sys.sleep(0.2)
}
# Close websocket connection
ws2$close()
