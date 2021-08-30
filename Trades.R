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


b <- rbindlist(df_list, fill = T)
b$Date_POSIXct <- as.character(anytime(as.numeric(as.character(b$time))))
b <- subset(b, Date_POSIXct > "2021-06-24 18:52:52")



key <- c("ordertxid", "pair", "type")
key <- c("pair", "type")
c <- b[, .(fee = sum(as.numeric(fee)), cost=sum(as.numeric(cost)), vol=sum(as.numeric(vol))), by=key]

c <- c[!pair %in% c("XETHZEUR", "ADAEUR", "ADAETH"), ]
c$vol <- round(c$vol, 5)
c[, true_cost := cost - fee]
View(c)

d <- c[, true_cost[type =="sell"] - true_cost[type =="buy"], by = .(pair, vol) ]
sum(d$V1)
