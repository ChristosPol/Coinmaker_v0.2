# Connect through the websocket of kraken to get live stream of prices
screen -S websocket R

options(scipen = 999)
library(websocket)

library(RJSONIO)
library(utils)

# Choose pair and interval
pair <- c("ETH/EUR")#, "ADA/EUR", "BTC/EUR", "DOT/EUR", "XRP/EUR", "LINK/EUR", "LTC/EUR",
          #"OXT/EUR", "LSK/EUR")
# , 
# "SC/EUR", "TRX/EUR", "GRT/EUR", "DOGE/EUR", "STORJ/EUR",
# "FIL/EUR", "KSM/EUR" ,"MANA/EUR", "KAVA/EUR", "ALGO/EUR", "FLOW/EUR", "UNI/EUR",
# "ICX/EUR", "CRV/EUR", "BCH/EUR",  "ATOM/EUR", "MLN/EUR", "XTZ/EUR")
interval = 240

# Path to write values
path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/current_values/"

# Handler function
poll_until_connected <- function(ws, timeout = 5) {
  connected <- FALSE
  end <- Sys.time() + timeout
  while (!connected && Sys.time() < end) {
    # Need to run the event loop for websocket to complete connection.
    later::run_now(0.1)
    
    ready_state <- ws$readyState()
    if (ready_state == 0L) {
      # 0 means we're still trying to connect.
      # For debugging, indicate how many times we've done this.
      cat(".")         
    } else if (ready_state == 1L) {
      connected <- TRUE
    } else {
      break
    }
  }
  
  if (!connected) {
    stop("Unable to establish websocket connection.")
  }
}

# Create json to send
x <- list( event = 'subscribe', pair  = as.array(pair),
           subscription = list(name="trade") )
json <- toJSON(x, pretty = T )

# Connect to websocket
ws2 <- websocket::WebSocket$new("wss://ws.kraken.com/", autoConnect = FALSE) 
price <- c()
volume <- c()
type <- c()
# Action on message
ws2$onMessage(function(event) {
  
  input_message <<- jsonlite::fromJSON(event$data)
  # print(input_message)
  if(length(input_message$event) == 1) {
    print("heartbeat")

  } else if (length(input_message$event) == 0){
    # print(input_message[[2]][, 1])
    # print(input_message[[2]][, 2])
 
    price <<- tail(append(price, as.numeric(input_message[[2]][, 1])), 1000)
    volume <<- tail(append(volume, as.numeric(input_message[[2]][, 2])), 1000)
    type <<- tail(append(type, input_message[[2]][, 4]), 1000)
    
    df1 <<- data.table(price, volume, type)
    print(df1)
    col <- ifelse(type == "b", "green", "red")
    # print(col)
    # # print(df)
    smoothingSpline_fast = smooth.spline(df1[, price] ~ as.numeric(rownames(df1)) , spar = 0.6)
    smoothingSpline_slow = smooth.spline(df1[, price] ~ as.numeric(rownames(df1)) , spar = 0.9)
    
    df1[, spline_fast := predict(smoothingSpline_fast)$y]
    df1[, spline_slow := predict(smoothingSpline_slow)$y]

    par(mfrow=c(2,1))
    plot(df1[, price], type = "l")
    lines(df1[, spline_fast], col="red" )
    lines(df1[, spline_slow], col="blue" )
    plot(df1[, volume], col = col, type = "h", cex=2)

  }
  #     if (input_message[[4]][1] == "ETH/EUR"){
  #       x <- ((as.numeric(input_message[[2]][6]) - as.numeric(input_message[[2]][3])) / as.numeric(input_message[[2]][3]))*100
  #       val <- as.numeric(input_message[[2]][6])
  #       write.csv(data.frame(x = x, val = val), paste0(path ,"ETHEUR_","val.csv"))
  #       print(paste0("ETH/EUR values exported.. current price ", val))
  # } else if (input_message[[4]][1] == "ADA/EUR"){
  #       x <- ((as.numeric(input_message[[2]][6]) - as.numeric(input_message[[2]][3])) / as.numeric(input_message[[2]][3]))*100
  #       val <- as.numeric(input_message[[2]][6])
  #       write.csv(data.frame(x = x, val = val), paste0(path ,"ADAEUR_","val.csv"))
  #       print(paste0("ADA/EUR values exported.. current price ", val))
  # } else if (input_message[[4]][1] == "XBT/EUR"){
  #       x <- ((as.numeric(input_message[[2]][6]) - as.numeric(input_message[[2]][3])) / as.numeric(input_message[[2]][3]))*100
  #       val <- as.numeric(input_message[[2]][6])
  #       write.csv(data.frame(x = x, val = val), paste0(path ,"BTCEUR_","val.csv"))
  #       print(paste0("BTC/EUR values exported.. current price ", val))
  # } else if (input_message[[4]][1] == "DOT/EUR"){
  #       x <- ((as.numeric(input_message[[2]][6]) - as.numeric(input_message[[2]][3])) / as.numeric(input_message[[2]][3]))*100
  #       val <- as.numeric(input_message[[2]][6])
  #       write.csv(data.frame(x = x, val = val), paste0(path ,"DOTEUR_","val.csv"))
  #       print(paste0("DOT/EUR values exported.. current price ", val))
  # }else if (input_message[[4]][1] == "XRP/EUR"){
  #   x <- ((as.numeric(input_message[[2]][6]) - as.numeric(input_message[[2]][3])) / as.numeric(input_message[[2]][3]))*100
  #   val <- as.numeric(input_message[[2]][6])
  #   write.csv(data.frame(x = x, val = val), paste0(path ,"XRPEUR_","val.csv"))
  #   print(paste0("XRP/EUR values exported.. current price ", val))
  # }else if (input_message[[4]][1] == "LINK/EUR"){
  #   x <- ((as.numeric(input_message[[2]][6]) - as.numeric(input_message[[2]][3])) / as.numeric(input_message[[2]][3]))*100
  #   val <- as.numeric(input_message[[2]][6])
  #   write.csv(data.frame(x = x, val = val), paste0(path ,"LINKEUR_","val.csv"))
  #   print(paste0("LINK/EUR values exported.. current price ", val))
  # }else if (input_message[[4]][1] == "LTC/EUR"){
  #   x <- ((as.numeric(input_message[[2]][6]) - as.numeric(input_message[[2]][3])) / as.numeric(input_message[[2]][3]))*100
  #   val <- as.numeric(input_message[[2]][6])
  #   write.csv(data.frame(x = x, val = val), paste0(path ,"LTCEUR_","val.csv"))
  #   print(paste0("LTC/EUR values exported.. current price ", val))
  # }else if (input_message[[4]][1] == "OXT/EUR"){
  #   x <- ((as.numeric(input_message[[2]][6]) - as.numeric(input_message[[2]][3])) / as.numeric(input_message[[2]][3]))*100
  #   val <- as.numeric(input_message[[2]][6])
  #   write.csv(data.frame(x = x, val = val), paste0(path ,"OXTEUR_","val.csv"))
  #   print(paste0("OXT/EUR values exported.. current price ", val))
  # }else if (input_message[[4]][1] == "LSK/EUR"){
  #   x <- ((as.numeric(input_message[[2]][6]) - as.numeric(input_message[[2]][3])) / as.numeric(input_message[[2]][3]))*100
  #   val <- as.numeric(input_message[[2]][6])
  #   write.csv(data.frame(x = x, val = val), paste0(path ,"LSKEUR_","val.csv"))
  #   print(paste0("LSK/EUR values exported.. current price ", val))
  # }
  #   
# }
})

# send the call
ws2$connect()
poll_until_connected(ws2)
ws2$send(json)
# ws2$close()


