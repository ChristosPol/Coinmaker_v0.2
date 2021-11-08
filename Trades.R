url = "https://api.kraken.com/0/private/TradesHistory"
key = API_Key
secret = API_Sign
offset <- 0
i <- 1
trades_raw <- list()
while (offset <= 200) {
  
  trades_raw[[i]] <- get_trade_history(url, key, secret, offset)
  
  offset <- offset + 50
  i <- i +1  
  Sys.sleep(5)
}

df_list <- list()
myls <- list()

for(k in 1:length(trades_raw)){
  
  for (i in 1:length(trades_raw[[k]]$result[[1]])){
    
    myls[[i]] <- as.data.frame(trades_raw[[k]]$result[[1]][[i]])
    
  }
  df_list[[k]] <- myls
  
}


df_list <- lapply(df_list, function(x)rbindlist(x, fill = T))
View(df_list)

b <- rbindlist(df_list, fill = T)
b$Date_POSIXct <- as.character(anytime(as.numeric(as.character(b$time))))

paste0(getwd(), "/Trading/trades.csv")
trades_bot <- read_csv(paste0(getwd(), "/Trading/trades.csv"), col_names = FALSE)

columnames <- data.table(current_price = as.numeric(NA), price_enter = as.numeric(NA),
                     price_exit = as.numeric(NA), volume_enter = NA, exit_condition = FALSE,
                     Support = NA, Support_enter_level = NA, at = Sys.time(),
                     action = "no_action", pos_perc = as.numeric(NA),
                     pair  = pair[i], id = as.character(NA),
                     Rsi = as.numeric(NA))
colnames(trades_bot) <- colnames(columnames)
View(trades_bot)

ids <- c("OTNWFP-BAYUK-TWPR5S",
         "OHKJI6-6RBIO-LY32MY",
         "OFEGNS-GJARX-GY33GM",
         "OA5Q6O-SF4RD-LRVLWQ",
         "OPENEB-ZYTJB-QKVHW4",
         "OPIEUW-F76UV-EU6Q7P",
         "OGBRE7-OISAR-7LLTSC",
         "O275UZ-XH5VM-LRCE5L")
b <- b[ordertxid %in% ids]

key <- c("ordertxid", "pair", "type")
key <- c("pair", "type")
c <- b[, .(fee = sum(as.numeric(fee)), cost=sum(as.numeric(cost)), vol=sum(as.numeric(vol))), by=key]
View(c)
c <- c[!pair %in% c("XETHZEUR", "ADAEUR", "ADAETH"), ]
c$vol <- round(c$vol, 5)
c[, true_cost := cost - fee]
View(c)

d <- c[, true_cost[type =="sell"] - true_cost[type =="buy"], by = .(pair, vol) ]

sum(d$V1)
