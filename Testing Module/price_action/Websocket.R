# Connect through the websocket of kraken to get live stream of prices
screen -S websocket R

library(websocket)
library(RJSONIO)
library(utils)

# Choose pair and interval
pair <- c("ETH/EUR", "ADA/EUR", "BTC/EUR", "DOT/EUR", "XRP/EUR", "LINK/EUR", "LTC/EUR",
          "OXT/EUR", "LSK/EUR", "SC/EUR", "TRX/EUR", "GRT/EUR", "DOGE/EUR", "STORJ/EUR",
          "FIL/EUR", "KSM/EUR" ,"MANA/EUR", "KAVA/EUR", "ALGO/EUR", "FLOW/EUR", "UNI/EUR",
          "ICX/EUR", "CRV/EUR", "BCH/EUR",  "ATOM/EUR", "MLN/EUR", "XTZ/EUR")
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
           subscription = list(name="ohlc", interval = interval) )
json <- toJSON(x, pretty = T )

# Connect to websocket
ws2 <- websocket::WebSocket$new("wss://ws.kraken.com/", autoConnect = FALSE) 

# Action on message
ws2$onMessage(function(event) {
  
  input_message <- jsonlite::fromJSON(event$data)
  # print(input_message)
  if (input_message[[4]][1] == "ETH/EUR") {
    write.csv(input_message[[2]][6], paste0(path ,"ETHEUR_","val.csv"))
    print( paste0(input_message[[2]][6], " _ETHEUR"))
  
  } else if (input_message[[4]][1] == "ADA/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"ADAEUR_","val.csv"))
    print( paste0(input_message[[2]][6], " _ADAEUR"))
  
  } else if (input_message[[4]][1] == "XBT/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"BTCEUR_","val.csv"))
    print( paste0(input_message[[2]][6], " _BTCEUR"))
  
  } else if (input_message[[4]][1] == "DOT/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"DOTEUR_","val.csv"))
    print( paste0(input_message[[2]][6], " _DOTEUR"))
  
  } else if (input_message[[4]][1] == "XRP/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"XRPEUR_","val.csv"))
    print( paste0(input_message[[2]][6], " _XRPEUR"))
  
  } else if (input_message[[4]][1] == "LINK/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"LINKEUR_","val.csv"))
    print( paste0(input_message[[2]][6], " _LINKEUR"))
  
  } else if (input_message[[4]][1] == "LTC/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"LTCEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _LTCEUR"))
  
  } else if (input_message[[4]][1] == "SC/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"SCEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _SCEUR"))
  
  } else if (input_message[[4]][1] == "XDG/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"DOGEEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _DOGEEUR"))
  
  } else if (input_message[[4]][1] == "LSK/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"LSKEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _LSKEUR"))
  
  } else if (input_message[[4]][1] == "GRT/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"GRTEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _GRTEUR"))
  
  } else if (input_message[[4]][1] == "TRX/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"TRXEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _TRXEUR"))
  
  } else if (input_message[[4]][1] == "OXT/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"OXTEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _OXTEUR"))
    
  } else if (input_message[[4]][1] == "STORJ/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"STORJEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " STORJEUR"))
    
  } else if (input_message[[4]][1] == "FIL/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"FILEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _FILEUR"))
    
  } else if (input_message[[4]][1] == "KSM/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"KSMEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _KSMEUR"))
    
  } else if (input_message[[4]][1] == "MANA/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"MANAEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _MANAEUR"))
    
  } else if (input_message[[4]][1] == "KAVA/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"KAVAEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _KAVAEUR"))
    
  } else if (input_message[[4]][1] == "ALGO/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"ALGOEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _ALGOEUR"))
    
  } else if (input_message[[4]][1] == "FLOW/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"FLOWEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _FLOWEUR"))
    
  } else if (input_message[[4]][1] == "UNI/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"UNIEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _UNIEUR"))
    
  } else if (input_message[[4]][1] == "ICX/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"ICXEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _ICXEUR"))
    
  } else if (input_message[[4]][1] == "CRV/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"CRVEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _CRVEUR"))
    
  } else if (input_message[[4]][1] == "BCH/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"BCHEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _BCHEUR"))
    
  } else if (input_message[[4]][1] == "ATOM/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"ATOMEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _ATOMEUR"))
    
  } else if (input_message[[4]][1] == "MLN/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"MLNEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _MLNEUR"))
    
  } else if (input_message[[4]][1] == "XTZ/EUR"){
    write.csv(input_message[[2]][6], paste0(path ,"XTZEUR_","val.csv"))
    print(paste0(input_message[[2]][6], " _XTZEUR"))
  }
  
})

# send the call
ws2$connect()
poll_until_connected(ws2)
ws2$send(json)
# ws2$close()


