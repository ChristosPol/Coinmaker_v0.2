rm(list=ls())

# Source functions
path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/ohlc/"

# Choose pair and interval
pair <- c("ETH/EUR", "ADA/EUR", "BTC/EUR", "DOT/EUR", "XRP/EUR", "LINK/EUR", "LTC/EUR",
          "OXT/EUR", "LSK/EUR", "SC/EUR", "TRX/EUR", "GRT/EUR", "DOGE/EUR", "STORJ/EUR",
          "FIL/EUR", "KSM/EUR" ,"MANA/EUR", "KAVA/EUR", "ALGO/EUR", "FLOW/EUR", "UNI/EUR",
          "ICX/EUR", "CRV/EUR", "BCH/EUR",  "ATOM/EUR", "MLN/EUR", "XTZ/EUR")
interval = 240

for (i in 1:length(pair)){
  OHLC <- simple_OHLC(interval, pair[i])
  OHLC_last_bar <- OHLC[nrow(OHLC)-1, ]
  write.csv(OHLC_last_bar, paste0(path ,"OHLC_last_bar_", gsub("/", "-",pair[i]), ".csv"))
  Sys.sleep(10)
}

