# Packages --------------------------------------------------------------------
# install_github("daroczig/binancer")
# https://github.com/daroczig/binancer/
suppressMessages(library(xts))
suppressMessages(library(Rbitcoin))
suppressMessages(library(httr))
suppressMessages(library(anytime))
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
suppressMessages(library(ggplot2))
suppressMessages(library(TTR))
suppressMessages(library(openssl))
suppressMessages(library(digest))
suppressMessages(library(zoo))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))
suppressMessages(library(bit64))
suppressMessages(library(nanotime))
suppressMessages(library(gganimate))
suppressMessages(library(gapminder))
suppressMessages(library(gifski))
suppressMessages(library(gridExtra))
suppressMessages(library(R.utils))
suppressMessages(library(plotly))
suppressMessages(library(Metrics))
suppressMessages(library(plm))
suppressMessages(library(randomForest))
suppressMessages(library(rredis))
# Options
setDTthreads(1)
options(stringsAsFactors = FALSE)

# API info
api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading",
                             "API_Keys.txt",
                             sep = "/"),
                       sep = ";", header = T)
API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)

# Functions --------------------------------------------------------------------

# Plot candlestick data --------------------------------------------------------
plot_candlesticks <- function(dta, Ns, asset){

  dta <- tail(dta, Ns)
  mn <- min(dta$low)
  mx <- max(dta$high)

  xs <- c(1:nrow(dta))
  color_list <- ifelse(dta$close >= dta$open, "green", "red")

  plot(dta$high, main = asset, xaxt = "n", xlab = "", ylab = "price", ylim = c(mn, mx), type = "n")
  par(new = T)
  plot(dta$low, main = "", axes = F, xlab = "", ylab = "", ylim = c(mn, mx), type = "n")
  segments(x0 = xs, y0 = dta$open, x1 = xs, y1 = dta$close, col = color_list, lwd = 4)
  segments(x0 = xs, y0= dta$low, x1 = xs, y1 = dta$high, col = color_list, lwd = 1)
  axis(1, at = 1:length(dta$interval), las = 2)
}

# # Plot chart with SR lines and return values
SR_lines <- function(data, roll, n_sort, pair, Ns, plot.it = FALSE){

  last_close <- data$high[nrow(data)]

  last_prices <- tail(data$high[-nrow(data)], roll)
  last_volumes <- tail(data$volume[-nrow(data)], roll)
  mydf <- data.frame(last_prices, last_volumes)
  mydf <- arrange(mydf, mydf$last_prices)

  sup_df <- head(mydf, n_sort)
  sup_w_mean <- sum(sup_df$last_prices *sup_df$last_volumes)/sum(sup_df$last_volumes)

  rs_df <- tail(mydf, n_sort)
  rs_w_mean <- sum(rs_df$last_prices *rs_df$last_volumes)/sum(rs_df$last_volumes)
  if(plot.it == TRUE){


    plot_candlesticks(dta = data, Ns = Ns, asset = pair)
    abline(h = rs_w_mean, col = "black", lty = "dashed")
    abline(h = sup_w_mean, col = "black", lty = "dashed")
  }
  return(list(SL = sup_w_mean, RL = rs_w_mean))

}

# Plots boolinger bands
bollinger_bands <- function(periods, times_sd, data){
  df <- data
  plot(df$close[-c(1:(periods-1))], type ="l", lwd =2)
  lines(SMA(df$close, n=periods)[-c(1:(periods-1))] + times_sd*rollapply(df$close, periods, sd), col ="red")
  lines(SMA(df$close, n=periods)[-c(1:(periods-1))] - times_sd*rollapply(df$close, periods, sd), col ="green")
}

# how many trades were succesful (to be functioned)
win_ratio <- function(dataset){
  df <- dataset
  mah <- subset(df, df$action %in% c("buy","sell") )
  profitable_trades <- list()
  ids_s <- unique(mah$id)
  for(i in 1:length(unique(mah$id))){

    profitable_trades[[i]] <- mah$close[mah$id == ids_s[i] & mah$action =="sell"]-mah$close[mah$id == ids_s[i] & mah$action =="buy"]
  }
  res <- table(unlist(profitable_trades) > 0)[names(table(unlist(profitable_trades) > 0)) ==T]/sum(table(unlist(profitable_trades) > 0))
  if(length(res)==0){
    res <- 0
  }

  return(res)
}

# Convert historical dates to candlesticks
trades_to_OHLC <- function(pair, interval, from_date, to_date, date_subset) {
  # Read it
  file <- paste0(paste(pair_data_results, pair, sep = "/"), ".csv.gz")
  frame <- fread(file)

  colnames(frame) <- c("price", "volume", "epoch_time", "buy_sell", "market_limit",
                        "last_id", "Date_POSIXct", "Time", "Date", "Hour")

  print("File loaded..")
  # Subset the time period
  if(date_subset) {
    frame <- subset(frame, frame$Date >= from_date & frame$Date < to_date)
  }

  candles <- list()
  for (i in 1:length(intervals)){
    # Select interval
    copied <- copy(frame)
    copied[, interval := strftime(floor_date(as.POSIXct(Date_POSIXct), intervals[i]),
                                  format = '%Y-%m-%d %H:%M:%S')]

    candles[[i]] <- copied[, .(high = max(price), low = min(price), open = first(price),
                               close = last(price), volume = sum(volume)),
                           by = .(interval)]
    candles[[i]]$full_date_time <- as.POSIXct(paste(candles[[i]]$Date,
                                                    candles[[i]]$interval),
                                              format="%Y-%m-%d %H:%M:%S")

    print(paste0("Reduced to ", intervals[i], " intervals.." ))
  }
  return(candles)
}








calculate_profits_LS <- function(dataset, params){

  calcu <- dataset[action %in% c("enter_long", "enter_short", "exit_long", "exit_short"), ]
  calcu <- subset(calcu,  !calcu$id %in% names(which(table(calcu$id) ==1)))
  if (nrow(calcu) > 0) {

    profit_long <- c()
    profit_short <- c()
    profit_sum <- c()
    ids_long <- unique(calcu$id[calcu$action %in% c("enter_long", "exit_long")])
    ids_short <- unique(calcu$id[calcu$action %in% c("enter_short", "exit_short")])
    for(i in 1:length(ids_long)){

      profit_long[i] <- calcu$equity[calcu$action =="exit_long" & calcu$id == ids_long[i]] - calcu$Price[calcu$action =="enter_long" & calcu$id == ids_long[i]]
    }

    for(i in 1:length(ids_short)){

      profit_short[i] <- calcu$equity[calcu$action =="exit_short" & calcu$id == ids_short[i]] - calcu$Price[calcu$action =="enter_short" & calcu$id == ids_short[i]]
    }

    profit_sum_long <- sum(profit_long)
    profit_sum_short <- sum(profit_short)
    profit_sum <- profit_sum_long +  profit_sum_short
    dd <- data.frame(profit_sum_long = profit_sum_long, profit_sum_short = profit_sum_short, profit = profit_sum, n_trades = length(unique(calcu$id)),
                     enter_date = unique(calcu$Date)[1], exit_date = tail(unique(calcu$Date), 1))
  } else {

    dd <- data.frame(profit = 0,
                     n_trades = 0,
                     enter_date = as.Date("2020-04-07"),
                     exit_date =as.Date("2020-04-07"))
  }

  if(paraller_exec ==TRUE){
    dd$params <- params
  }

  write.table(dd, "/media/chris/DATA/Documents/Bot_Trading/Historical_data/myDF.csv",
              sep = ",", row.names = FALSE, col.names = !file.exists("/media/chris/DATA/Documents/Bot_Trading/Historical_data/myDF.csv"), append = T)
  return(dd)
}








###
# Calculate profits and number of trades ---------------------------------------

calculate_profits <- function(dataset, params){

  calcu <- dataset[action %in% c("buy", "sell"), ]
  calcu <- subset(calcu,  !calcu$id %in% names(which(table(calcu$id) ==1)))
  if (nrow(calcu) > 0) {

  profit <- c()

  ids <- unique(calcu$id)
  for(i in 1:length(ids)){

    profit[i] <-   calcu$Price[calcu$action =="sell" & calcu$id == ids[i]] - calcu$Price[calcu$action =="buy" & calcu$id == ids[i]]

  }

  profit1 <- tail(calcu$Price, 1)
  dd <- data.frame(profit = profit1-initial_budget,
                   n_trades = length(unique(calcu$id)),
                   biggest_lost =min(profit[profit < 0]),
                   biggest_win = max(profit[profit > 0 ]),
                   avg_loss = mean(profit[profit < 0]),
                   avg_win = mean(profit[profit > 0]),
                   winratio = win_ratio(myresult),
                   enter_date = unique(calcu$full_date_time)[1],
                   exit_date = tail(unique(calcu$full_date_time), 1))
  } else {

    dd <- data.frame(profit = 0,
                     n_trades = 0,
                     biggest_lost = 0,
                     biggest_win = 0,
                     avg_loss = 0,
                     avg_win = 0,
                     winratio = 0,
                     enter_date = as.Date("2020-04-07"),
                     exit_date =as.Date("2020-04-07"))
  }

  if(paraller_exec ==TRUE){
    dd$params <- params
  }

  write.table(dd, "/media/chris/DATA/Documents/Bot_Trading/Historical_data/myDF.csv",
              sep = ",", row.names = FALSE, col.names = !file.exists("/media/chris/DATA/Documents/Bot_Trading/Historical_data/myDF.csv"), append = T)
  return(dd)
}


# Download OLHC data and calcualte indicators. Write last line and append each time
#-------------------------------------------------------------------------------

OHLC_action <- function(pair, interval){
  repeat{
    # 1. Get the OHLC - Repeat this call every x interval
    what <- tryCatch(
      {
        url <- paste0('https://api.kraken.com/0/public/OHLC?pair=',pair,'&interval=', interval)
        dat <- jsonlite::fromJSON(url)
      },
      error = function(e){})

    if(is.null(dat$result[1])) next # error, skip
    if(nrow(as.data.frame(dat$result[1])) == 0) break # last batch empty

    df <- as.data.table(dat$result$XETHZEUR)
    colnames(df) <- c("time", "open", "high", "low", "close",
                      "vwap", "volume", "count")
    df[, Date_POSIXct := anytime(as.numeric(as.character(time)))]

    # as numeric
    df$open <- as.numeric(df$open)
    df$high <- as.numeric(df$high)
    df$low <- as.numeric(df$low)
    df$close <- as.numeric(df$close)

    # 2. Add Indicators
    df[, c(paste0("EMA", "_", fast_EMA), paste0("EMA", "_", slow_EMA), "crossover", "action", "Units", "Price", "id") := list(NA, NA,0, NA, NA, NA, NA) ]
    df[, c(paste0("EMA", "_", fast_EMA), paste0("EMA", "_", slow_EMA)) := list( EMA(close, n = fast_EMA), EMA(close, n = slow_EMA) ) ]
    df$crossover[get(paste0("EMA", "_", fast_EMA), df) > get(paste0("EMA", "_", slow_EMA), df)] <- 1

    print(tail(df, 1))
    Sys.sleep(interval*60)
  }
}

roundup <- function(x, towhere){
  res <- ceiling(x/towhere)*towhere
  return(res)
}

