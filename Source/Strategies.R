trend_obv <- function(obv_ema, sma_ema, takeprofit,stoploss_ult, plot.it) {

  # Train and test datasets
  train_data[, c("x",
                 "OBV",
                 "OBV_SMA",
                 "SMA",
                 "exit_condition",
                 "tp",
                 "ult_sl",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA) ]

  test_data[, c("x",
                  "OBV",
                  "OBV_SMA",
                  "SMA",
                  "exit_condition",
                  "tp",
                  "ult_sl",
                  "action",
                  "Units",
                  "Price",
                  "id") := list(NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])
    fut$x <- 1:nrow(fut)

    # OBV and SMA calculation -------------------------------------------------------
    fut[, OBV := OBV(fut[, close], fut[, volume])]
    fut[, OBV_SMA := SMA(fut[, OBV], n = obv_ema)]
    fut[, SMA := SMA(fut[, close], n = sma_ema)]


    if(plot.it == TRUE){

      plot_df <- tail(fut, 200)

      # sr <- SR_lines(fut, roll = 100, Ns = nrow(fut), n_sort = 15, pair = pair,plot.it = F)
      # fut <- tail(fut, 250)
      df_points_buy <- data.frame(x = na.omit(plot_df$x[plot_df$action =="buy"]),
                                  y = na.omit(plot_df$close[plot_df$action =="buy"]))
      df_points_sell <- data.frame(x = na.omit(plot_df$x[plot_df$action =="sell"]),
                                   y = na.omit(plot_df$close[plot_df$action == "sell"]))

      par(mfrow = c(2, 1))
      plot(plot_df$close, type ="l", main = paste0("profits = ", tail(na.omit(plot_df$Price), 1)))
      lines(plot_df$SMA, col ="blue")

      points(rownames(plot_df)[plot_df$x %in% df_points_buy$x], df_points_buy$y, col ="green", pch = 19)
      points(rownames(plot_df)[plot_df$x %in% df_points_sell$x], df_points_sell$y, col ="red", pch = 19)

      plot(plot_df$OBV, type ="l")
      lines(plot_df$OBV_SMA, col ="red")

    }

    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl

    fut$exit_condition[nrow(fut)] <-  fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]

    # Deciding upon action -----------------------------------------------------
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$OBV[nrow(fut)] > fut$OBV_SMA[nrow(fut)] &  fut$close[nrow(fut)] > fut$SMA[nrow(fut)]) ) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)] - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      # fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE  )) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]  - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      # fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    # print(i)
    if(plot.it == TRUE){
      Sys.sleep(0.1)
      # flush.console()
    }
    print(i)
  }
  return(train_data)
}


splines_fast_slow_cross_eff <- function(spar_fast, spar_slow) {

  # Train and test datasets
  train_data[, c("x",
                 "spline_fast",
                 "spline_slow",
                 "action") := list(NA, NA, NA, NA) ]

  test_data[, c("x",
                "spline_fast",
                "spline_slow",
                "action") := list(NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])
    fut$x <- 1:nrow(fut)

    # Spline calculation -------------------------------------------------------
    # Calculate spline - derivative
    smoothingSpline_fast = smooth.spline(fut[, close] ~ as.numeric(rownames(fut)) , spar = spar_fast)
    fut[, spline_fast := predict(smoothingSpline_fast)$y]

    # Spline calculation -------------------------------------------------------
    # Calculate spline - derivative
    smoothingSpline_slow = smooth.spline(fut[, close] ~ as.numeric(rownames(fut)) , spar = spar_slow)
    fut[, spline_slow := predict(smoothingSpline_slow)$y]

    # Deciding upon action -----------------------------------------------------
    # Buy condition

    fut$action[nrow(fut)] <- ifelse(fut$spline_fast[nrow(fut)] > fut$spline_slow[nrow(fut)], 1, 0)
    train_data <- fut
    print(i)
  }
  return(train_data)
}

splines_fast_slow_cross <- function(spar_fast,spar_slow, takeprofit,stoploss_ult,plot.it) {

  # Train and test datasets
  train_data[, c("x",
                 "spline_fast",
                 # "deriv_fast",
                 # "sign_derivs_fast",
                 # "change_sign_fast",
                 "spline_slow",
                 # "deriv_slow",
                 # "sign_derivs_slow",
                 # "change_sign_slow",
                 "exit_condition",
                 "tp",
                 "ult_sl",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA) ]

  test_data[, c("x",
                "spline_fast",
                # "deriv_fast",
                # "sign_derivs_fast",
                # "change_sign_fast",
                "spline_slow",
                # "deriv_slow",
                # "sign_derivs_slow",
                # "change_sign_slow",
                "exit_condition",
                "tp",
                "ult_sl",
                "action",
                "Units",
                "Price",
                "id") := list(NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])
    fut$x <- 1:nrow(fut)

    # Spline calculation -------------------------------------------------------
    # Calculate spline - derivative
    smoothingSpline_fast = smooth.spline(fut[, close] ~ as.numeric(rownames(fut)) , spar = spar_fast)
    fut[, spline_fast := predict(smoothingSpline_fast)$y]
    # fut[, deriv_fast := predict(smoothingSpline_fast, deriv = 1)$y]

    # Sign of deriv - [-2 for desc, 2 for asc]
    # fut[, sign_derivs_fast := c(sign(deriv_fast))]
    # fut[, change_sign_fast := c(0, diff(sign(deriv_fast)))]

    # Spline calculation -------------------------------------------------------
    # Calculate spline - derivative
    smoothingSpline_slow = smooth.spline(fut[, close] ~ as.numeric(rownames(fut)) , spar = spar_slow)
    fut[, spline_slow := predict(smoothingSpline_slow)$y]
    # fut[, deriv_slow := predict(smoothingSpline_slow, deriv = 1)$y]

    # Sign of deriv - [-2 for desc, 2 for asc]
    # fut[, sign_derivs_slow := c(sign(deriv_slow))]
    # fut[, change_sign_slow := c(0, diff(sign(deriv_slow)))]
    if(plot.it == TRUE){

      plot_df <- tail(fut, 200)

      # sr <- SR_lines(fut, roll = 100, Ns = nrow(fut), n_sort = 15, pair = pair,plot.it = F)
      # fut <- tail(fut, 250)
      df_points_buy <- data.frame(x = na.omit(plot_df$x[plot_df$action =="buy"]),
                                  y = na.omit(plot_df$close[plot_df$action =="buy"]))
      df_points_sell <- data.frame(x = na.omit(plot_df$x[plot_df$action =="sell"]),
                                   y = na.omit(plot_df$close[plot_df$action == "sell"]))

      par(mfrow = c(1, 1))
      plot(plot_df$close, type ="l", main = paste0("profits = ", tail(na.omit(plot_df$Price), 1)))
      lines(plot_df$spline_slow, col ="blue")
      lines(plot_df$spline_fast, col ="red")

      points(rownames(plot_df)[plot_df$x %in% df_points_buy$x], df_points_buy$y, col ="green", pch = 19)
      points(rownames(plot_df)[plot_df$x %in% df_points_sell$x], df_points_sell$y, col ="red", pch = 19)
    }

    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl

    fut$exit_condition[nrow(fut)] <-  fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]

    # Deciding upon action -----------------------------------------------------
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$spline_fast[nrow(fut)] > fut$spline_slow[nrow(fut)])) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)] - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      # fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$spline_fast[nrow(fut)] < fut$spline_slow[nrow(fut)] | fut$exit_condition[nrow(fut)] == TRUE  )) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]  - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      # fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                (fut$spline_fast[nrow(fut)] > fut$spline_slow[nrow(fut)]  | fut$exit_condition[nrow(fut)] == FALSE )) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    # print(i)
    if(plot.it == TRUE){
      Sys.sleep(0.1)
      # flush.console()
    }
    print(i)
  }
  return(train_data)
}



# Splines trend inversion
Splines_Tangent <- function(takeprofit, stoploss_trail,stoploss_ult, spar,plot.it, rsi_period) {

  # Train and test datasets
  train_data[, c("RSI",
                 "x",
                 "spline",
                 "deriv",
                 "sign_derivs",
                 "change_sign",
                 "exit_condition",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c("RSI",
                "x",
                "spline",
                "deriv",
                "sign_derivs",
                "change_sign",
                "exit_condition",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA,NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])
    fut$x <- 1:nrow(fut)
    fut$RSI <- RSI(fut$close, n = rsi_period)
    # Calculate spline - derivative
    smoothingSpline = smooth.spline(fut[, close] ~ as.numeric(rownames(fut)) , spar = spar)
    fut[, spline := predict(smoothingSpline)$y]
    fut[, deriv := predict(smoothingSpline, deriv = 1)$y]

    # Sign of deriv - [-2 for desc, 2 for asc]
    fut[, sign_derivs := c(sign(deriv))]
    fut[, change_sign := c(0, diff(sign(deriv)))]

    if(plot.it == TRUE){

      sr <- SR_lines(fut, roll = 100, Ns = nrow(fut), n_sort = 15, pair = pair,plot.it = F)

      fut <- tail(fut, 250)
      df_points_buy <- data.frame(x = na.omit(fut$x[fut$action =="buy"]),
                                  y = na.omit(fut$close[fut$action =="buy"]))
      df_points_sell <- data.frame(x = na.omit(fut$x[fut$action =="sell"]),
                                   y = na.omit(fut$close[fut$action == "sell"]))

      par(mfrow = c(2, 1))
      plot(fut$close, type ="l", main = paste0("profits = ", tail(na.omit(fut$Price), 1)))
      lines(fut$spline, col ="red")
      points(df_points_buy$x, df_points_buy$y, col ="green", pch = 19)
      points(df_points_sell$x, df_points_sell$y, col ="red", pch = 19)
      abline(h = sr$SL)
      abline(h =sr$RL)
      plot(fut$deriv, type ="l", main = paste0("sign: ",
                                               " sign deriv: ", fut$sign_derivs[nrow(fut)], " deriv ", fut$deriv[nrow(fut)]))
      abline(h = 0, col = "red", lty = 5, lwd = 2)
      # plot(fut$RSI, type ="l")
      # abline(h = 80,  col = "red", lty = 5, lwd = 2)

    }

    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }


    # Trailing stop loss
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      } else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      }

    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

    } else {
      trail_sl <-0
    }

    if(length(trail_sl) == 0 ){

      trail_sl <- 0
    }

    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]


    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$deriv[nrow(fut)] > 0 )) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE | fut$deriv[nrow(fut)] < 0  )) {
      # | fut$deriv[nrow(fut)] < 0
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    # print(i)
    if(plot.it == TRUE){
      Sys.sleep(0.1)
      # flush.console()
    }
  }
  return(train_data)
}


# Strategy using volumes spikes and RSI oversold conditions
Pure_RSI_Volume_Trailing <- function(RSI_Period, RSI_below, EMA_volume,
                                     takeprofit, stoploss_trail,stoploss_ult,
                                     times_vol, plot.it) {

  # Train and test datasets
  train_data[, c("x",
                 "SMA",
                 "RSI",
                 "EMA_volume",
                 "exit_value",
                 "exit_condition",
                 "crossover_volume",
                 "crossover_RSI",
                 "Slope",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c("x",
                "SMA",
                "RSI",
                "EMA_volume",
                "exit_value",
                "exit_condition",
                "crossover_volume",
                "crossover_RSI",
                "Slope",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])

    # RSI and Volume
    fut$RSI <- RSI(fut$close, n = RSI_Period)
    fut$EMA_volume <- EMA(fut$volume, n = EMA_volume)
    fut$x <- 1:nrow(fut)

    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover_volume[nrow(fut)] <- ifelse(fut$volume[nrow(fut)] > fut$EMA_volume[nrow(fut)] * times_vol ,
                                              "volume_higher", "volume_lower")
    # RSI Crossing of upper or lower bounds
    fut$crossover_RSI[nrow(fut)] <- ifelse(fut$RSI[nrow(fut)] < RSI_below ,
                                           "RSI_lower", "RSI_higher")
    if(plot.it == TRUE){

      plot_df <- tail(fut, 200)
      df_points_buy <- data.frame(x = na.omit(plot_df$x[plot_df$action =="buy"]),
                                  y = na.omit(plot_df$close[plot_df$action =="buy"]))
      df_points_sell <- data.frame(x = na.omit(plot_df$x[plot_df$action =="sell"]),
                                   y = na.omit(plot_df$close[plot_df$action == "sell"]))

      par(mfrow = c(3, 1))
      plot(plot_df$close, type ="l", main = paste0("profits = ", tail(na.omit(plot_df$Price), 1)))
      # lines(plot_df$spline, col ="red")
      points(rownames(plot_df)[plot_df$x %in% df_points_buy$x], df_points_buy$y, col ="green", pch = 19)
      points(rownames(plot_df)[plot_df$x %in% df_points_sell$x], df_points_sell$y, col ="red", pch = 19)

      plot(plot_df$RSI, type ="l")
      abline(h = RSI_below,  col = "red", lty = 5, lwd = 2)

      plot(plot_df$volume, type ="l")
      # lines(plot_df$EMA_volume,  col = "red", lty = 5, lwd = 2)
      abline(h = plot_df$EMA_volume[nrow(plot_df)],  col = "red", lty = 5, lwd = 2)
    }


    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }


    # Trailing stop loss
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      } else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      }

    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

    } else {
      trail_sl <-0
    }

    if(length(trail_sl) == 0 ){

      trail_sl <- 0
    }

    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]


    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover_volume[nrow(fut)] == "volume_higher" &  fut$crossover_RSI[nrow(fut)] == "RSI_lower") {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE )) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    # print(i)
    if(plot.it == TRUE){
      Sys.sleep(0.1)
      # flush.console()
    }
  }
  return(train_data)
}

splines_fast_slow <- function(spar_fast, spar_slow, plot.it) {

  # Train and test datasets
  train_data[, c("x",
                 "spline_fast",
                 "deriv_fast",
                 "sign_derivs_fast",
                 "change_sign_fast",
                 "spline_slow",
                 "deriv_slow",
                 "sign_derivs_slow",
                 "change_sign_slow",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA, NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c("x",
                "spline_fast",
                "deriv_fast",
                "sign_derivs_fast",
                "change_sign_fast",
                "spline_slow",
                "deriv_slow",
                "sign_derivs_slow",
                "change_sign_slow",
                "action",
                "Units",
                "Price",
                "id") := list(NA, NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])
    fut$x <- 1:nrow(fut)

    # Spline calculation -------------------------------------------------------
    # Calculate spline - derivative
    smoothingSpline_fast = smooth.spline(fut[, close] ~ as.numeric(rownames(fut)) , spar = spar_fast)
    fut[, spline_fast := predict(smoothingSpline_fast)$y]
    fut[, deriv_fast := predict(smoothingSpline_fast, deriv = 1)$y]

    # Sign of deriv - [-2 for desc, 2 for asc]
    fut[, sign_derivs_fast := c(sign(deriv_fast))]
    fut[, change_sign_fast := c(0, diff(sign(deriv_fast)))]

    # Spline calculation -------------------------------------------------------
    # Calculate spline - derivative
    smoothingSpline_slow = smooth.spline(fut[, close] ~ as.numeric(rownames(fut)) , spar = spar_slow)
    fut[, spline_slow := predict(smoothingSpline_slow)$y]
    fut[, deriv_slow := predict(smoothingSpline_slow, deriv = 1)$y]

    # Sign of deriv - [-2 for desc, 2 for asc]
    fut[, sign_derivs_slow := c(sign(deriv_slow))]
    fut[, change_sign_slow := c(0, diff(sign(deriv_slow)))]

    if(plot.it == TRUE){

      plot_df <- tail(fut, 200)

      # sr <- SR_lines(fut, roll = 100, Ns = nrow(fut), n_sort = 15, pair = pair,plot.it = F)
      # fut <- tail(fut, 250)
      df_points_buy <- data.frame(x = na.omit(plot_df$x[plot_df$action =="buy"]),
                                  y = na.omit(plot_df$close[plot_df$action =="buy"]))
      df_points_sell <- data.frame(x = na.omit(plot_df$x[plot_df$action =="sell"]),
                                   y = na.omit(plot_df$close[plot_df$action == "sell"]))

      par(mfrow = c(3, 1))
      plot(plot_df$close, type ="l", main = paste0("profits = ", tail(na.omit(plot_df$Price), 1)))
      lines(plot_df$spline_slow, col ="red")
      lines(plot_df$spline_fast, col ="blue")

      points(rownames(plot_df)[plot_df$x %in% df_points_buy$x], df_points_buy$y, col ="green", pch = 19)
      points(rownames(plot_df)[plot_df$x %in% df_points_sell$x], df_points_sell$y, col ="red", pch = 19)
      # abline(h = sr$SL)
      # abline(h =sr$RL)
      plot(plot_df$deriv_slow, type ="l", main = paste0("sign: ",
                                                        " sign deriv: ", plot_df$sign_derivs_slow[nrow(plot_df)], " deriv ", plot_df$deriv_slow[nrow(plot_df)]))
      abline(h = 0, col = "red", lty = 5, lwd = 2)

      plot(plot_df$deriv_fast, type ="l", main = paste0("sign: ",
                                                        " sign deriv: ", plot_df$sign_derivs_fast[nrow(plot_df)], " deriv ", plot_df$deriv_fast[nrow(plot_df)]))
      abline(h = 0, col = "red", lty = 5, lwd = 2)
    }



    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$deriv_slow[nrow(fut)] > 0  & fut$deriv_fast[nrow(fut)] > 0)) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$deriv_fast[nrow(fut)] < 0 | fut$deriv_slow[nrow(fut)] < 0 )) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                (fut$deriv_slow[nrow(fut)] > 0  & fut$deriv_fast[nrow(fut)] > 0)   ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    # print(i)
    if(plot.it == TRUE){
      Sys.sleep(0.1)
      # flush.console()
    }
    # print(i)
  }
  return(train_data)
}


# Dynamic sr lines
Dynamic_SR_Lines <- function(roll,
                             n_sort,
                             takeprofit,
                             stoploss_trail,
                             stoploss_ult,
                             RSI_Period,
                             RSI_below) {

  # Train and test datasets
  train_data[, c("RSI",
                 "crossover_RSI",
                 "SL",
                 "RL",
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c("RSI",
                "crossover_RSI",
                "SL",
                "RL",
                "exit_value",
                "exit_condition",
                "crossover",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA, NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])

    lines_sr <- SR_lines(data = fut, roll = roll, n_sort = n_sort, pair = "BTCEUR", Ns = nrow(fut))
    fut$RL[nrow(fut)] <- lines_sr$RL
    fut$SL[nrow(fut)] <- lines_sr$SL

    if(fut$close[nrow(fut)] < lines_sr$SL){

      fut$crossover[nrow(fut)] <- "Below"
    }else if (fut$close[nrow(fut)] > lines_sr$RL){
      fut$crossover[nrow(fut)] <- "Above"
    }else{
      fut$crossover[nrow(fut)] <- "Between"
    }

    # RSI calculation
    fut$RSI <- RSI(fut$close, n = RSI_Period)

    # RSI Crossing of upper or lower bounds
    fut$crossover_RSI[nrow(fut)] <- ifelse(fut$RSI[nrow(fut)] < RSI_below ,
                                           "RSI_lower", "RSI_higher")



    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }


    # Trailing stop loss
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      } else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      }

    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

    } else {
      trail_sl <-0
    }

    if(length(trail_sl) == 0 ){

      trail_sl <- 0
    }

    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]


    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$crossover[nrow(fut)] == "Below" &  fut$crossover_RSI[nrow(fut)] == "RSI_lower")) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      # fut$exit_condition[nrow(fut)] == TRUE)) {
      fut$exit_condition[nrow(fut)] == TRUE | fut$crossover[nrow(fut)] == "Above")) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    print(i)
  }
  return(train_data)
}
# Only support and resistance
Support_Resistance <- function(takeprofit, stoploss_trail,stoploss_ult,plot.it,n_sort, roll) {

  # Train and test datasets
  train_data[, c("x",
                 "exit_condition",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA,NA, NA,NA, NA, NA, NA, NA, NA) ]

  test_data[, c("x",
                "exit_condition",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA,NA, NA,NA, NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])
    fut$x <- 1:nrow(fut)

    # Calculate spline - derivative

    sr <- SR_lines(fut, roll = roll, Ns = nrow(fut), n_sort = n_sort, pair = pair,plot.it = F)
    if(plot.it == TRUE){

      plot_df <- tail(fut, 300)
      df_points_buy <- data.frame(x = na.omit(plot_df$x[plot_df$action =="buy"]),
                                  y = na.omit(plot_df$close[plot_df$action =="buy"]))
      df_points_sell <- data.frame(x = na.omit(plot_df$x[plot_df$action =="sell"]),
                                   y = na.omit(plot_df$close[plot_df$action == "sell"]))

      par(mfrow = c(2, 1))
      plot(plot_df$close, type ="l", main = paste0("profits = ", tail(na.omit(plot_df$Price), 1)))

      points(rownames(plot_df)[plot_df$x %in% df_points_buy$x], df_points_buy$y, col ="green", pch = 19)
      points(rownames(plot_df)[plot_df$x %in% df_points_sell$x], df_points_sell$y, col ="red", pch = 19)

      abline(h = sr$SL)
      abline(h =sr$RL)

      plot(plot_df$volume, type ="l")
      # lines(plot_df$EMA_volume,  col = "red", lty = 5, lwd = 2)
      abline(h = plot_df$EMA_volume[nrow(plot_df)],  col = "red", lty = 5, lwd = 2)

      # sr <- SR_lines(fut, roll = 100, Ns = nrow(fut), n_sort = 15, pair = pair,plot.it = F)

      # plot_df <- tail(fut, 300)
      # df_points_buy <- data.frame(x = na.omit(plot_df$x[plot_df$action =="buy"]),
      #                             y = na.omit(plot_df$close[plot_df$action =="buy"]))
      # df_points_sell <- data.frame(x = na.omit(plot_df$x[plot_df$action =="sell"]),
      #                              y = na.omit(plot_df$close[plot_df$action == "sell"]))
      #
      # par(mfrow = c(2, 1))
      # plot(plot_df$close, type ="l", main = paste0("profits = ", tail(na.omit(plot_df$Price), 1)))
      #
      # points(df_points_buy$x, df_points_buy$y, col ="green", pch = 19)
      # points(df_points_sell$x, df_points_sell$y, col ="red", pch = 19)
      # abline(h = sr$SL)
      # abline(h =sr$RL)
      # plot(plot_df$volume, type ="l")
      # # lines(plot_df$EMA_volume,  col = "red", lty = 5, lwd = 2)
      # abline(h = plot_df$EMA_volume[nrow(plot_df)],  col = "red", lty = 5, lwd = 2)

    }

    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }


    # Trailing stop loss
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      } else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      }

    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){
      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

    } else {
      trail_sl <-0
    }

    if(length(trail_sl) == 0 ){

      trail_sl <- 0
    }

    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]


    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$close[nrow(fut)] <= sr$SL  )) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]- (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE | fut$close[nrow(fut)] > sr$RL )) {
      # | fut$deriv[nrow(fut)] < 0
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]- (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    # print(i)
    if(plot.it == TRUE){
      Sys.sleep(0.1)
      # flush.console()
    }
  }
  return(train_data)
}



# Indicator trading ------------------------------------------------------------
# MACD crossover going long
MACD_long <- function(EMA_Trend, nfast, nslow, nsig, stoploss, takeprofit) {

  # Train and test datasets
  train_data[, c("macd",
                 "signal",
                 "hist",
                 paste0("EMA_Trend", "_", EMA_Trend),
                 "Uptrend",
                 "hist_positive",
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0, NA, 0 ,0, NA, NA, 0, NA, NA, NA, NA, NA, NA) ]

  test_data[,  c("macd",
                 "signal",
                 "hist",
                 paste0("EMA_Trend", "_", EMA_Trend),
                 "Uptrend",
                 "hist_positive",
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0, NA, 0, 0, NA, NA, 0, NA, NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])


    # Technical indicators -----------------------------------------------------
    macd_indicator <- as.data.frame(MACD(fut$close, nFast = nfast, nSlow = nslow, nSig = nsig))
    macd_indicator$hist <-macd_indicator$macd - macd_indicator$signal

    fut$macd <- macd_indicator$macd
    fut$signal <- macd_indicator$signal
    fut$hist <- macd_indicator$hist

    fut[, c(paste0("EMA_Trend", "_", EMA_Trend)) := list(EMA(close, n = EMA_Trend)) ]

    # Long  signal
    fut$crossover[fut$macd > fut$signal] <- "macd_higher"
    fut$crossover[fut$macd <=  fut$signal] <- "macd_lower"

    fut$hist_positive[fut$macd & fut$signal < 0] <- "hist_negative"
    fut$hist_positive[fut$macd & fut$signal >= 0] <- "hist_positive"

    # Uptrend signal
    fut$Uptrend[fut$close > get(paste0("EMA_Trend", "_", EMA_Trend), fut)] <- TRUE
    fut$Uptrend[fut$close <= get(paste0("EMA_Trend", "_", EMA_Trend), fut)] <- FALSE

    # Exit condition for stop loss
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)) / tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)

    if (length(exit_value) == 0) {
      exit_value <- 0
    }

    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <- fut$exit_value > takeprofit | fut$exit_value <= -stoploss

    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover[nrow(fut)] == "macd_higher" &  fut$Uptrend[nrow(fut)] == TRUE & fut$hist_positive[nrow(fut)] == "hist_negative"  ) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               (fut$exit_condition[nrow(fut)] == TRUE | fut$crossover[nrow(fut)] =="macd_lower") ) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                (fut$exit_condition[nrow(fut)] == FALSE | fut$crossover[nrow(fut)] =="macd_higher" )  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    # print(i)
  }
  return(train_data)
}



# Splines trend inversion
Splines_Tangent_SL <- function(takeprofit, stoploss_trail, stoploss_ult, spar, plot.it, rsi_period) {

  # Train and test datasets
  train_data[, c("rsi",
                 "x",
                 "spline",
                 "deriv",
                 "sign_derivs",
                 "change_sign",
                 "exit_condition_long",
                 "exit_condition_short",
                 "action",
                 "Units",
                 "Price",
                 "equity",
                 "tp_long",
                 "ult_sl_long",
                 "trail_sl_long",
                 "tp_short",
                 "ult_sl_short",
                 "trail_sl_short",
                 "id") := list(NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA) ]

  test_data[, c("rsi",
                "x",
                "spline",
                "deriv",
                "sign_derivs",
                "change_sign",
                "exit_condition_long",
                "exit_condition_short",
                "action",
                "Units",
                "Price",
                "equity",
                "tp_long",
                "ult_sl_long",
                "trail_sl_long",
                "tp_short",
                "ult_sl_short",
                "trail_sl_short",
                "id") := list(NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])
    fut$x <- 1:nrow(fut)
    fut$rsi <- RSI(fut$close, n = rsi_period)
    # Calculate spline - derivative
    smoothingSpline = smooth.spline(fut[, close] ~ as.numeric(rownames(fut)) , spar = spar)
    fut[, spline := predict(smoothingSpline)$y]
    fut[, deriv := predict(smoothingSpline, deriv = 1)$y]

    # Sign of deriv - [-2 for desc, 2 for asc]
    fut[, sign_derivs := c(sign(deriv))]
    fut[, change_sign := c(0, diff(sign(deriv)))]

    if(plot.it == TRUE){

      fut <- tail(fut, 200)
      df_points_enter_long <- data.frame(x = na.omit(fut$x[fut$action =="enter_long"]),
                                         y = na.omit(fut$close[fut$action =="enter_long"]))
      df_points_exit_long <- data.frame(x = na.omit(fut$x[fut$action =="exit_long"]),
                                        y = na.omit(fut$close[fut$action =="exit_long"]))

      df_points_enter_short <- data.frame(x = na.omit(fut$x[fut$action =="enter_short"]),
                                          y = na.omit(fut$close[fut$action == "enter_short"]))
      df_points_exit_short <- data.frame(x = na.omit(fut$x[fut$action =="exit_short"]),
                                         y = na.omit(fut$close[fut$action == "exit_short"]))

      par(mfrow = c(3, 1))
      plot(fut$close, type ="l", main = paste0("profits = ", tail(na.omit(fut$equity), 1)))
      lines(fut$spline, col ="red")
      points(df_points_enter_long$x, df_points_enter_long$y, col ="green", pch = 19)
      points(df_points_exit_long$x, df_points_exit_long$y, col ="red", pch = 19)

      points(df_points_enter_short$x, df_points_enter_short$y, col ="blue", pch = 19)
      points(df_points_exit_short$x, df_points_exit_short$y, col ="black", pch = 19)

      plot(fut$deriv, type ="l", main = paste0("sign: ",
                                               " sign deriv: ", fut$sign_derivs[nrow(fut)], " deriv ", fut$deriv[nrow(fut)]))
      abline(h = 0, col = "red", lty = 5, lwd = 2)
      plot(fut$rsi, type ="l")
      abline(h =30, col ="red")
      abline(h =70, col ="red")
    }

    # Exit condition for long trades -------------------------------------------
    tp_long <- tail(fut$close[fut$action == "enter_long"][!is.na(fut$close[fut$action == "enter_long"])], 1) + takeprofit * tail(fut$close[fut$action == "enter_long"][!is.na(fut$close[fut$action == "enter_long"])], 1)

    if (length(tp_long) == 0) {
      tp_long <- 0
    }

    # Ultimate stop loss
    ult_sl_long <- tail(fut$close[fut$action == "enter_long"][!is.na(fut$close[fut$action == "enter_long"])], 1) - stoploss_ult * tail(fut$close[fut$action == "enter_long"][!is.na(fut$close[fut$action == "enter_long"])], 1)

    if (length(ult_sl_long) == 0) {
      ult_sl_long <- 0
    }



    if (fut$action[nrow(fut)-1] %in% c("enter_long", "keep_long") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl_long <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl_long < tail(fut$trail_sl_long[!is.na(fut$trail_sl_long)], 1)){
        trail_sl_long <- tail(fut$trail_sl_long[!is.na(fut$trail_sl_long)], 1)

      }else {
        trail_sl_long <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]

      }


    } else if (fut$action[nrow(fut)-1] %in% c("enter_long", "keep_long") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){

      trail_sl_long <- tail(fut$trail_sl_long[!is.na(fut$trail_sl_long)], 1)
    } else {

      trail_sl_long <-0
    }


    fut$tp_long[nrow(fut)] <- tp_long
    fut$ult_sl_long[nrow(fut)] <- ult_sl_long
    fut$trail_sl_long[nrow(fut)] <- trail_sl_long

    # Exit condition for short trades ------------------------------------------
    tp_short <- tail(fut$close[fut$action == "enter_short"][!is.na(fut$close[fut$action == "enter_short"])], 1) - takeprofit * tail(fut$close[fut$action == "enter_short"][!is.na(fut$close[fut$action == "enter_short"])], 1)

    if (length(tp_short) == 0) {
      tp_short <- 0
    }

    # Ultimate stop loss
    ult_sl_short <- tail(fut$close[fut$action == "enter_short"][!is.na(fut$close[fut$action == "enter_short"])], 1) + stoploss_ult * tail(fut$close[fut$action == "enter_short"][!is.na(fut$close[fut$action == "enter_short"])], 1)

    if (length(ult_sl_short) == 0) {
      ult_sl_short <- 0
    }



    if (fut$action[nrow(fut)-1] %in% c("enter_short", "keep_short") & ( fut$close[nrow(fut)] < fut$close[nrow(fut)-1] )  ){

      trail_sl_short <- fut$close[nrow(fut)] + stoploss_trail * fut$close[nrow(fut)]

      if( trail_sl_short > tail(fut$trail_sl_short[!is.na(fut$trail_sl_short)], 1)){
        trail_sl_short <- tail(fut$trail_sl_short[!is.na(fut$trail_sl_short)], 1)

      } else {
        trail_sl_short <- fut$close[nrow(fut)] + stoploss_trail * fut$close[nrow(fut)]

      }


    } else if (fut$action[nrow(fut)-1] %in% c("enter_short", "keep_short") & ( fut$close[nrow(fut)] >= fut$close[nrow(fut)-1] ) ){

      trail_sl_short <- tail(fut$trail_sl_short[!is.na(fut$trail_sl_short)], 1)
    } else {

      trail_sl_short <-1000000
    }


    fut$tp_short[nrow(fut)] <- tp_short
    fut$ult_sl_short[nrow(fut)] <- ult_sl_short
    fut$trail_sl_short[nrow(fut)] <- trail_sl_short


    fut$exit_condition_long[nrow(fut)] <- fut$trail_sl_long[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl_long[nrow(fut)] > fut$close[nrow(fut)] | fut$tp_long[nrow(fut)] < fut$close[nrow(fut)]
    fut$exit_condition_short[nrow(fut)] <- fut$trail_sl_short[nrow(fut)] < fut$close[nrow(fut)] | fut$ult_sl_short[nrow(fut)] < fut$close[nrow(fut)] | fut$tp_short[nrow(fut)] > fut$close[nrow(fut)]


    # Deciding upon action -----------------------------------------------------

    # Enter long position ------------------------------------------------------
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("exit_long", "no action", "exit_short")) &
         fut$deriv[nrow(fut)] > 0 ) {

      fut$action[nrow(fut)] <- "enter_long"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      fut$equity[nrow(fut)] <- initial_budget
      # Exit long position -----------------------------------------------------
    } else if (fut$action[nrow(fut) - 1] %in% c("keep_long", "enter_long") & (
      fut$exit_condition_long[nrow(fut)] == TRUE | fut$deriv[nrow(fut)] < 0 )) {

      fut$action[nrow(fut)] <- "exit_long"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      # fut$Price[nrow(fut)] <- (tail(fut$Price[fut$action == "exit_long"], 1) - tail(fut$Price[fut$action == "enter_long"], 1)) + tail(fut$Price[fut$action == "enter_long"], 1)

      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- (tail(fut$Price[fut$action == "exit_long"], 1) - tail(fut$Price[fut$action == "enter_long"], 1)) + tail(fut$Price[fut$action == "enter_long"], 1)
      fut$equity[nrow(fut)] <- initial_budget
      # initial_budget <- tail(fut$Price[fut$action == "enter_long"],1) + (fut$Price[nrow(fut)] - tail(fut$Price[fut$action == "enter_long"], 1))

      # Keep long position -----------------------------------------------------
    } else if ( fut$action[nrow(fut) - 1] %in% c("enter_long", "keep_long")   &
                fut$exit_condition_long[nrow(fut)] == FALSE  ) {

      fut$action[nrow(fut)] <- "keep_long"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

      # Enter short position ---------------------------------------------------
    } else if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("exit_long", "no action", "exit_short")) &
               fut$deriv[nrow(fut)] < 0  ) {

      fut$action[nrow(fut)] <- "enter_short"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      fut$equity[nrow(fut)] <- initial_budget

      # Exit short position ----------------------------------------------------
    } else if(fut$action[nrow(fut) - 1] %in% c("keep_short", "enter_short") & (
      fut$exit_condition_short[nrow(fut)] == TRUE | fut$deriv[nrow(fut)] > 0 )) {

      fut$action[nrow(fut)] <- "exit_short"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      # fut$Price[nrow(fut)] <- (tail(fut$Price[fut$action == "enter_short"], 1) - tail(fut$Price[fut$action == "exit_short"], 1)) + tail(fut$Price[fut$action == "enter_short"], 1)
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- (tail(fut$Price[fut$action == "enter_short"], 1) - tail(fut$Price[fut$action == "exit_short"], 1)) + tail(fut$Price[fut$action == "enter_short"], 1)
      fut$equity[nrow(fut)] <- initial_budget
      # initial_budget <- tail(fut$Price[fut$action == "enter_short"], 1) + (tail(fut$Price[fut$action == "enter_short"],1) - (fut$Price[nrow(fut)]))
      # initial_budget <- fut$Price[nrow(fut)]

      # Keep short position ----------------------------------------------------
    } else if(fut$action[nrow(fut) - 1] %in% c("enter_short", "keep_short")   &
              fut$exit_condition_short[nrow(fut)] == FALSE  ) {


      fut$action[nrow(fut)] <- "keep_short"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"
    }

    train_data <- fut
    # print(i)
    if(plot.it == TRUE){
      Sys.sleep(0.8)
      # flush.console()
    }
  }
  return(train_data)
}


# Strategy  --------------------------------------------------------------------

Volume_candle_patterns <- function(takeprofit, stoploss, SMA_Volume) {

  # Train and test datasets
  train_data[, c(paste0("Volume_SMA", "_", SMA_Volume),
                 "exit_value",
                 "pattern",
                 "candle_type",
                 "exit_condition",
                 "crossover",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0, NA,0 ,0,NA, NA, NA, NA, NA, NA) ]

  test_data[,  c(paste0("Volume_SMA", "_", SMA_Volume),
                 "exit_value",
                 "pattern",
                 "candle_type",
                 "exit_condition",
                 "crossover",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0, NA,0,0, NA, NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])

    # Candle type
    fut$candle_type[fut$open > fut$close] <- "bearish"
    fut$candle_type[fut$open < fut$close] <- "bullish"
    fut$candle_type[fut$open == fut$close] <- "neutral"
    # Technical indicators -----------------------------------------------------

    fut[, c(paste0("Volume_SMA", "_", SMA_Volume)) := list(SMA(volume, n = SMA_Volume)) ]

    # Volume Crossing of volume over the mean(volume, n_periods) + times * sd(volume, n_periods)
    fut$crossover[fut$volume > get(paste0("Volume_SMA", "_", SMA_Volume), fut)  ] <- "volume_higher"
    fut$crossover[fut$volume <= get(paste0("Volume_SMA", "_", SMA_Volume), fut)  ] <- "volume_lower"

    fut$pattern[nrow(fut)] <- fut$candle_type[nrow(fut)] == "bullish" & fut$candle_type[nrow(fut) -1] == "bearish"


    # Exit condition for stop loss
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) &fut$action %in% c("buy", "keep")], 1)) / tail(fut$close[!is.na(fut$action) &fut$action %in% c("buy", "keep")], 1)

    if (length(exit_value) == 0) {
      exit_value <- 0
    }

    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <-  fut$exit_value <= -stoploss



    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover[nrow(fut)] == "volume_higher" & fut$pattern[nrow(fut)] == TRUE ) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               fut$exit_condition[nrow(fut)] == TRUE ) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    # print(i)
  }
  return(train_data)
}

################################################################################

# ------------------------------------------------------------------------------
Volume_RSI_TP_SL <- function(RSI_Period, RSI_below, RSI_above, SMA_volume, takeprofit, stoploss, candle_action_long) {

  # Train and test datasets
  train_data[, c(paste0("RSI", "_", RSI_Period),
                 paste0("SMA_volume", "_", SMA_volume),
                 "candle_type",
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "crossover_RSI",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0, 0,NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c(paste0("RSI", "_", RSI_Period),
                paste0("SMA_volume", "_", SMA_volume),
                "exit_value",
                "candle_type",
                "exit_condition",
                "crossover",
                "crossover_RSI",
                "action",
                "Units",
                "Price",
                "id") := list(0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])

    # Mean and sd of volume in train set
    fut[, c(paste0("RSI", "_", RSI_Period),
            paste0("SMA_volume", "_", SMA_volume)) := list(RSI(close, n = RSI_Period),
                                                           EMA(close, n = SMA_volume)) ]

    fut$candle_type[fut$open > fut$close] <- "bearish"
    fut$candle_type[fut$open < fut$close] <- "bullish"
    fut$candle_type[fut$open == fut$close] <- "neutral"
    # Technical indicators -----------------------------------------------------


    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover[fut$volume > get(paste0("SMA_volume", "_", SMA_volume), fut) ] <- "volume_higher"
    fut$crossover[fut$volume <= get(paste0("SMA_volume", "_", SMA_volume), fut)  ] <- "volume_lower"

    # RSI Crossing of upper or lower bounds
    fut$crossover_RSI[get(paste0("RSI", "_", RSI_Period), fut) < RSI_below ] <- "rsi_below"
    fut$crossover_RSI[get(paste0("RSI", "_", RSI_Period), fut) > RSI_above ] <- "rsi_above"
    fut$crossover_RSI[is.na(fut$crossover_RSI)] <- "rsi_between"


    # Exit condition for stop loss
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)) / tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)

    if (length(exit_value) == 0) {
      exit_value <- 0
    }

    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <- fut$exit_value > takeprofit | fut$exit_value <= -stoploss


    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover[nrow(fut)] == "volume_higher" & fut$candle_type[nrow(fut)] == candle_action_long & fut$crossover_RSI[nrow(fut)] == "rsi_below" ) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE  | fut$crossover_RSI[nrow(fut)] =="rsi_above")) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    # print(i)
  }
  return(train_data)
}
################################################################################
# Strategy 1

invert_EMAs_volume_Spikes <- function(fast_EMA, slow_EMA, times_sd, takeprofit, stoploss, n_mean_sd) {


  # Train and test datasets
  train_data[, c("mean_volume",
                 "sd",
                 paste0("EMA", "_", fast_EMA),
                 paste0("EMA", "_", slow_EMA),
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "crossover_Volume",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0, 0, NA, NA,NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c("mean_volume",
                "sd",
                paste0("EMA", "_", fast_EMA),
                paste0("EMA", "_", slow_EMA),
                "exit_value",
                "exit_condition",
                "crossover",
                "crossover_Volume",
                "action",
                "Units",
                "Price",
                "id") := list(0, 0, NA, NA,NA, NA, NA, NA ,NA, NA, NA, NA) ]

  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {

    # Bind last row
    fut <- rbind(train_data, test_data[i, ])

    # Create indicators
    fut[, c(paste0("EMA", "_", fast_EMA),
            paste0("EMA", "_", slow_EMA)) := list(EMA(close, n = fast_EMA),
                                                  EMA(close, n = slow_EMA)) ]


    # Mean and sd of volume in train set
    fut$mean_volume[nrow(fut)] <- mean(tail(fut$volume, n_mean_sd))
    fut$sd[nrow(fut)] <- sd(tail(fut$volume), n_mean_sd)




    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) < get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_lower"
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) >= get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_higher"

    # Volume Crossing of volume over the mean(volume, n_periods) + times * sd(volume, n_periods)
    fut$crossover_Volume[fut$volume > (fut$mean_volume +  times_sd * fut$sd)  ] <- "volume_higher"
    fut$crossover_Volume[fut$volume <= (fut$mean_volume +  times_sd * fut$sd)  ] <- "volume_lower"

    # Exit condition for stop loss
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)) / tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)

    if (length(exit_value) == 0) {
      exit_value <- 0
    }

    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <- fut$exit_value > takeprofit | fut$exit_value <= -stoploss


    # Buy Condition
    if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
        (fut$crossover[nrow(fut)] =="faster_EMA_lower"  &
         fut$crossover_Volume[nrow(fut)] == "volume_higher") ){

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Keep Condition
    } else if ( (fut$action[nrow(fut) - 1] %in% c("buy", "keep")) &
                (fut$exit_condition[nrow(fut)] == FALSE) ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

      # Sell Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &  fut$exit_condition[nrow(fut)] == TRUE) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # No Action Condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("sell", "no action") | (is.na(fut$action[nrow(fut) - 1]) & fut$crossover[nrow(fut)] == "faster_EMA_lower" & fut$crossover_Volume[nrow(fut)] == "volume_lower")) {

      fut$action[nrow(fut)] <- "no action"
    }

    train_data <- fut
  }
  return(train_data)
}
################################################################################
# Strategy 00 ------------------------------------------------------------------

Volume_trading <- function(EMA_volume, rsi_period, takeprofit, stoploss_trail,stoploss_ult, times_vol, candle_action_long, plot.it) {

  # Train and test datasets
  train_data[, c("EMA_volume",
                 "x",
                 "RSI",
                 "exit_value",
                 "exit_condition",
                 "crossover_volume",
                 "candle_type",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA,NA,NA, NA,NA, NA,NA,NA,NA, NA, NA, NA, NA, NA) ]

  test_data[, c("EMA_volume",
                "x",
                "RSI",
                "exit_value",
                "exit_condition",
                "crossover_volume",
                "candle_type",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA,NA,NA, NA,NA, NA,NA,NA,NA, NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])
    fut$x <- 1:nrow(fut)

    # Mean and sd of volume in train set
    fut$EMA_volume <- EMA(fut$volume, n = EMA_volume)
    fut$RSI <- RSI(fut$close, n = rsi_period)
    # Technical indicators -----------------------------------------------------

    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover_volume[nrow(fut)] <- ifelse(fut$volume[nrow(fut)] > fut$EMA_volume[nrow(fut)] * times_vol ,
                                              "volume_higher", "volume_lower")

    fut$candle_type[fut$open > fut$close] <- "bearish"
    fut$candle_type[fut$open < fut$close] <- "bullish"
    fut$candle_type[fut$open == fut$close] <- "neutral"
    # Technical indicators -----------------------------------------------------

    if(plot.it == TRUE){

      plot_df <- tail(fut, 200)

      # sr <- SR_lines(fut, roll = 100, Ns = nrow(fut), n_sort = 15, pair = pair,plot.it = F)
      # fut <- tail(fut, 250)
      df_points_buy <- data.frame(x = na.omit(plot_df$x[plot_df$action =="buy"]),
                                  y = na.omit(plot_df$close[plot_df$action =="buy"]))
      df_points_sell <- data.frame(x = na.omit(plot_df$x[plot_df$action =="sell"]),
                                   y = na.omit(plot_df$close[plot_df$action == "sell"]))

      par(mfrow = c(3, 1))
      plot_candlesticks(dta = plot_df, Ns = nrow(plot_df), asset = "whatever")
      # plot(plot_df$close, type ="l", main = paste0("profits = ", tail(na.omit(plot_df$Price), 1)))

      points(rownames(plot_df)[plot_df$x %in% df_points_buy$x],
             df_points_buy$y, col ="blue", pch = 19, cex = 1.2)
      points(rownames(plot_df)[plot_df$x %in% df_points_sell$x],
             df_points_sell$y, col ="black", pch = 19, cex = 1.2)

      plot(plot_df$volume, type = "l", main = paste0("profits = ", tail(na.omit(plot_df$Price), 1)))
      lines(plot_df$EMA_volume, col = "red")

      plot(plot_df$RSI, type = "l")
      abline(h =25,  col ="red")
    }


    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }



    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]

      }


    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){

      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {

      trail_sl <-0
    }


    if(length(trail_sl) == 0 ){

      trail_sl <- 0
    }
    # browser()



    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]



    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover_volume[nrow(fut)] == "volume_higher" & fut$candle_type[nrow(fut)] == candle_action_long) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]  - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               fut$exit_condition[nrow(fut)] == TRUE ) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]  - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    if(plot.it == TRUE){
      Sys.sleep(0.1)
      # flush.console()
    }
    # print(i)
  }
  return(train_data)
}



# Strategy 0 -------------------------------------------------------------------
Long_term_trend_Slope_RSI <- function(RSI_Period,takeprofit, stoploss_trail,stoploss_ult,upper_RSI_Bound,lower_RSI_Bound, trend, trend_periods) {

  # Train and test datasets
  train_data[, c(paste0("RSI", "_", RSI_Period),
                 paste0("EMA_Trend", "_", trend),
                 "crossover",
                 "exit_value",
                 "exit_condition",
                 "RSI_Flag",
                 "Trend_Slope",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA,NA,NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c(paste0("RSI", "_", RSI_Period),
                paste0("EMA_Trend", "_", trend),
                "crossover",
                "exit_value",
                "exit_condition",
                "RSI_Flag",
                "Trend_Slope",
                "tp",
                "ult_sl",
                "trail_sl",
                "action",
                "Units",
                "Price",
                "id") := list(NA,NA,NA,NA, NA, NA,NA,NA, NA, NA, NA, NA, NA, NA)]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    # browser()
    fut <- rbind(train_data, test_data[i, ])
    fut[, c(paste0("RSI", "_", RSI_Period),
            paste0("EMA_Trend", "_", trend)) := list(RSI(close, n = RSI_Period),
                                                     EMA(close, n = trend)) ]
    # Technical indicators -----------------------------------------------------


    # RSI Crossing
    fut$RSI_Flag[get(paste0("RSI", "_", RSI_Period), fut) < lower_RSI_Bound] <- "RSI_Below"
    fut$RSI_Flag[get(paste0("RSI", "_", RSI_Period), fut) > upper_RSI_Bound] <- "RSI_Above"
    fut$RSI_Flag[is.na(fut$RSI_Flag)] <- "In_Between"

    # Find if trend is positive or negative
    fut$Trend_Slope[nrow(fut)] <- get(paste0("EMA_Trend", "_", trend), fut[nrow(fut)]) - get(paste0("EMA_Trend", "_", trend), fut[nrow(fut) - trend_periods])

    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }


    # Trailing stop loss

    # id_trail <- tail(fut$id[!is.na(fut$id)], 1)
    #
    # if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
    #
    #   trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #
    #   if(trail_sl > max(fut$trail_sl[which(fut$id == id_trail)+1], na.rm =T)){
    #     trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #
    #   }else{
    #
    #     trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)+1])
    #   }
    #
    #
    #
    # } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1]) ){
    #
    #   trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)])
    #
    # } else {
    #
    #   trail_sl <-0
    # }
    #

    # if(length(trail_sl) == 0 ){
    #
    #   trail_sl <- 0
    # }

    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]

      }


    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){

      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {

      trail_sl <-0
    }


    if(length(trail_sl) == 0 ){

      trail_sl <- 0
    }
    # browser()



    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]

    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$Trend_Slope[nrow(fut)] > 0  &  fut$RSI_Flag[nrow(fut)] == "RSI_Below")) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$RSI_Flag[nrow(fut)] == "RSI_Above" | fut$exit_condition[nrow(fut)] == TRUE) ){

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$RSI_Flag[nrow(fut)] %in% c("RSI_Below", "In_Between" )
    )

    {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    # print(i)
  }
  return(train_data)
}


#

# Strategy 1
invert_EMAs_volume <- function(fast_EMA, slow_EMA, RSI_period, RSI_below, Volume_EMA, takeprofit, stoploss_trail,stoploss_ult, times_vol) {



  # Train and test datasets
  train_data[, c("EMA_fast",
                 "EMA_slow",
                 "RSI",
                 "EMA_volume",
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "crossover_Volume",
                 "crossover_RSI",
                 "candle_type",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA,NA, NA,NA,NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c("EMA_fast",
                "EMA_slow",
                "RSI",
                "EMA_volume",
                "exit_value",
                "exit_condition",
                "crossover",
                "crossover_Volume",
                "crossover_RSI",
                "candle_type",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA,NA,NA, NA, NA,NA,NA, NA, NA, NA ,NA, NA, NA, NA, NA, NA, NA) ]

  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {

    # Bind last row
    fut <- rbind(train_data, test_data[i, ])


    # Create indicators
    fut[, c("EMA_fast",
            "EMA_slow",
            "RSI") := list(RSI(close, n = fast_EMA),
                           EMA(close, n = slow_EMA),
                           RSI(close, n = RSI_period)) ]

    # Candle type
    fut$candle_type[fut$open > fut$close] <- "bullish"
    fut$candle_type[fut$open < fut$close] <- "bearish"
    fut$candle_type[fut$open == fut$close] <- "neutral"

    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover_Volume[nrow(fut)] <- ifelse(fut$volume[nrow(fut)] > fut$EMA_volume[nrow(fut)] * times_vol ,
                                              "volume_higher", "volume_lower")

    # RSI Crossing of upper or lower bounds
    fut$crossover[nrow(fut)] <- ifelse(fut$EMA_fast[nrow(fut)] < fut$EMA_slow[nrow(fut)] ,
                                       "EMA_lower", "EMA_higher")

    # RSI Crossing of upper or lower bounds
    fut$crossover_RSI[nrow(fut)] <- ifelse(fut$RSI[nrow(fut)] < RSI_below ,
                                           "RSI_lower", "RSI_higher")


    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }



    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]

      }


    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){

      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {

      trail_sl <-0
    }


    if(length(trail_sl) == 0 ){

      trail_sl <- 0
    }



    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]


    # Buy Condition
    if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
        fut$crossover[nrow(fut)] == "EMA_lower"  &
        fut$crossover_RSI[nrow(fut)] == "RSI_lower" ){

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Keep Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("buy", "keep") &
               (fut$exit_condition[nrow(fut)] == FALSE   )) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

      # Sell Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &  fut$exit_condition[nrow(fut)] == TRUE) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # No Action Condition
    } else {

      fut$action[nrow(fut)] <- "no action"
    }

    train_data <- fut
  }
  return(train_data)
}

# Strategy 2 -------------------------------------------------------------------
# Crossing fast ema to slower with stoploss condition

cross_EMA_stoploss <- function(EMA_fast, EMA_slow, EMA_volume, takeprofit, stoploss_ult, plot.it) {

  # Train and test datasets$
  train_data[, c("fast_EMA",
                 "slow_EMA",
                 "volume_EMA",
                 "exit_condition",
                 "crossover_EMA",
                 "crossover_Volume",
                 "candle_type",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "x",
                 "id") := list(NA,NA,NA, NA, NA,NA,NA,NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c("fast_EMA",
                "slow_EMA",
                "volume_EMA",
                "exit_condition",
                "crossover_EMA",
                "crossover_Volume",
                "candle_type",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "x",
                "id") := list(NA,NA,NA, NA, NA,NA,NA,NA, NA, NA, NA, NA, NA, NA) ]

  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {

    # Bind last row
    fut <- rbind(train_data, test_data[i, ])

    # Create indicators
    fut[, fast_EMA := EMA(close, n = EMA_fast)]
    fut[, slow_EMA := EMA(close, n = EMA_slow)]
    fut[, volume_EMA := EMA(volume, n = EMA_volume)]

    # Candle type
    fut$candle_type[fut$open > fut$close] <- "bearish"
    fut$candle_type[fut$open < fut$close] <- "bullish"
    fut$candle_type[fut$open == fut$close] <- "neutral"

    # Price action
    fut$crossover_EMA[fut$fast_EMA > fut$slow_EMA] <- "faster_EMA_higher"
    fut$crossover_EMA[fut$fast_EMA <= fut$slow_EMA] <- "faster_EMA_lower"

    # Volume action
    fut$crossover_Volume[fut$volume > fut$volume_EMA] <- "volume_higher"
    fut$crossover_Volume[fut$volume <= fut$volume_EMA] <- "volume_lower"

    fut$x <- 1:nrow(fut)

    if(plot.it == TRUE){

      plot_df <- tail(fut, 200)
      df_points_buy <- data.frame(x = na.omit(plot_df$x[plot_df$action =="buy"]),
                                  y = na.omit(plot_df$close[plot_df$action =="buy"]))
      df_points_sell <- data.frame(x = na.omit(plot_df$x[plot_df$action =="sell"]),
                                   y = na.omit(plot_df$close[plot_df$action == "sell"]))

      par(mfrow = c(1, 1))
      plot(plot_df$close, type ="l", main = paste0("profits = ", tail(na.omit(plot_df$Price), 1)))
      lines(get(paste0("EMA", "_", fast_EMA), plot_df) , col ="blue")
      lines(get(paste0("EMA", "_", slow_EMA), plot_df), col ="grey")


      points(rownames(plot_df)[plot_df$x %in% df_points_buy$x], df_points_buy$y, col ="green", pch = 19)
      points(rownames(plot_df)[plot_df$x %in% df_points_sell$x], df_points_sell$y, col ="red", pch = 19)
    }


    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }

    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl


    fut$exit_condition[nrow(fut)] <- fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]

    # BUY Condition
    if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action"))
        & fut$crossover_EMA[nrow(fut)] == "faster_EMA_higher" & fut$candle_type[nrow(fut)] =="bullish" &
        fut$crossover_Volume[nrow(fut)] == "volume_higher") {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Keep Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("buy", "keep") &
               fut$crossover_EMA[nrow(fut)] == "faster_EMA_higher" &
               fut$exit_condition[nrow(fut)] == FALSE ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

      # Sell Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               (fut$crossover_EMA[nrow(fut)] %in% c("faster_EMA_lower", "faster_EMA_higher") &
                fut$exit_condition[nrow(fut)] == TRUE | fut$crossover_EMA[nrow(fut)] %in% c("faster_EMA_lower"))) {


      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # No Action Condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("sell", "no action") | (is.na(fut$action[nrow(fut) - 1])
                                                                         & fut$crossover_EMA[nrow(fut)] == "faster_EMA_lower")) {

      fut$action[nrow(fut)] <- "no action"
    }

    train_data <- fut
    # print(i)
    if(plot.it == TRUE){
      Sys.sleep(0.1)
      # flush.console()
    }
  }
  return(train_data)
}


# Stategy 3 --------------------------------------------------------------------
RSI_Crossover_EMA_Slope <- function(RSI_Period, upper_RSI_Bound, lower_RSI_Bound, trend_EMA) {

  # Train and test datasets
  train_data[, c("Slope_EMA",
                 paste0("RSI", "_", RSI_Period),
                 paste0("trend_EMA", "_", trend_EMA),
                 "crossover",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c("Slope_EMA",
                paste0("RSI", "_", RSI_Period),
                paste0("trend_EMA", "_", trend_EMA),
                "crossover",
                "action",
                "Units",
                "Price",
                "id") := list(NA, NA, NA ,NA, NA, NA, NA, NA) ]

  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {

    # Bind last row
    fut <- rbind(train_data, test_data[i, ])

    # Create Indicators
    fut[, c(paste0("RSI", "_", RSI_Period)) := list(RSI(close, n = RSI_Period)) ]
    fut[, c(paste0("trend_EMA", "_", trend_EMA)) := list(EMA(close, n = trend_EMA)) ]

    # Slope
    fut$Slope_EMA[nrow(fut)] <- get(paste0("trend_EMA_", trend_EMA), fut[nrow(fut)]) - get(paste0("trend_EMA_", trend_EMA), fut[nrow(fut) -1])


    fut$crossover[get(paste0("RSI", "_", RSI_Period), fut) > upper_RSI_Bound] <- "RSI_Higher"
    fut$crossover[get(paste0("RSI", "_", RSI_Period), fut) <= lower_RSI_Bound] <- "RSI_Lower"
    fut$crossover[is.na(fut$crossover)] <- "In_Between"

    # BUY Condition
    if (  (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
          fut$crossover[nrow(fut)] =="RSI_Lower" & fut$Slope_EMA[nrow(fut)] > 0) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Keep Condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep") &  fut$crossover[nrow(fut)] %in% c("In_Between","RSI_Lower")  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

      # Sell Condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("keep", "buy") &  fut$crossover[nrow(fut)] %in% c("RSI_Higher")  ) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # No action condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("sell", "no action") | (is.na(fut$action[nrow(fut) - 1]) & fut$crossover[nrow(fut)] == "In_Between")) {

      fut$action[nrow(fut)] <- "no action"
    }

    train_data <- fut
  }
  return(train_data)
}

# Strategy 4 -------------------------------------------------------------------
# Combine crossover with volume, RSI and Overall trend
RSI_Swings <- function(fast_EMA, slow_EMA, Volume_EMA, RSI_Period, upper_RSI_Bound,lower_RSI_Bound, stoploss, trend, trend_periods) {

  # Train and test datasets
  train_data[, c(paste0("EMA", "_", fast_EMA),
                 paste0("EMA", "_", slow_EMA),
                 paste0("Volume_EMA", "_", Volume_EMA),
                 paste0("RSI", "_", RSI_Period),
                 paste0("EMA_Trend", "_", trend),
                 "crossover",
                 "crossover_Volume",
                 "RSI_Flag",
                 "exit_value",
                 "exit_condition",
                 "Trend_Slope",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA ,NA,NA, NA, NA, NA) ]

  test_data[, c(paste0("EMA", "_", fast_EMA),
                paste0("EMA", "_", slow_EMA),
                paste0("Volume_EMA", "_", Volume_EMA),
                paste0("RSI", "_", RSI_Period),
                paste0("EMA_Trend", "_", trend),
                "crossover",
                "crossover_Volume",
                "RSI_Flag",
                "exit_value",
                "exit_condition",
                "Trend_Slope",
                "action",
                "Units",
                "Price",
                "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA ,NA, NA, NA, NA, NA)]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])
    fut[, c(paste0("EMA", "_", fast_EMA),
            paste0("EMA", "_", slow_EMA),
            paste0("Volume_EMA", "_", Volume_EMA),
            paste0("RSI", "_", RSI_Period),
            paste0("EMA_Trend", "_", trend)) := list(EMA(close, n = fast_EMA),
                                                     EMA(close, n = slow_EMA),
                                                     EMA(volume, n = Volume_EMA),
                                                     RSI(close, n = RSI_Period),
                                                     EMA(close, n = trend)) ]

    # Technical indicators -----------------------------------------------------

    # EMA Crossing
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) < get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_lower"
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) >= get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_higher"

    # Volume Crossing
    fut$crossover_Volume[get(paste0("Volume_EMA", "_", Volume_EMA), fut) < fut$volume] <- "volume_higher"
    fut$crossover_Volume[get(paste0("Volume_EMA", "_", Volume_EMA), fut) >= fut$volume] <- "volume_lower"

    # RSI Crossing
    fut$RSI_Flag[get(paste0("RSI", "_", RSI_Period), fut) < lower_RSI_Bound] <- "RSI_Below"
    fut$RSI_Flag[get(paste0("RSI", "_", RSI_Period), fut) > upper_RSI_Bound] <- "RSI_Above"
    fut$RSI_Flag[is.na(fut$RSI_Flag)] <- "In_Between"

    # Find if trend is positive or negative
    fut$Trend_Slope[nrow(fut)] <- get(paste0("EMA_Trend", "_", trend), fut[nrow(fut)]) - get(paste0("EMA_Trend", "_", trend), fut[nrow(fut) - trend_periods])

    # Exit condition for stop loss ---------------------------------------------
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) & fut$action == "buy"], 1)) / tail(fut$close[!is.na(fut$action) & fut$action == "buy"], 1)

    if (length(exit_value) == 0) {
      exit_value <- 0
    }

    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <- fut$exit_value <= -stoploss

    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$Trend_Slope[nrow(fut)] > 0 ) &
         (fut$crossover[nrow(fut)] == "faster_EMA_lower")  & (
           (fut$crossover_Volume[nrow(fut)] == "volume_higher" | fut$RSI_Flag[nrow(fut)] == "RSI_Below")) ){

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               fut$crossover[nrow(fut)] == "faster_EMA_higher" &
               fut$RSI_Flag[nrow(fut)] == "RSI_Above") {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   & (
      (fut$crossover[nrow(fut)] == "faster_EMA_higher" &  fut$crossover_Volume[nrow(fut)] == "volume_higher" &  fut$RSI_Flag[nrow(fut)] == "RSI_Below") |
      (fut$crossover[nrow(fut)] == "faster_EMA_higher" &  fut$crossover_Volume[nrow(fut)] == "volume_higher" &  fut$RSI_Flag[nrow(fut)] == "In_Between") |
      (fut$crossover[nrow(fut)] == "faster_EMA_higher" &  fut$crossover_Volume[nrow(fut)] == "volume_lower" &  fut$RSI_Flag[nrow(fut)] == "RSI_Below") |
      (fut$crossover[nrow(fut)] == "faster_EMA_higher" &  fut$crossover_Volume[nrow(fut)] == "volume_lower" &  fut$RSI_Flag[nrow(fut)] == "In_Between") |
      (fut$crossover[nrow(fut)] == "faster_EMA_lower" &  fut$crossover_Volume[nrow(fut)] == "volume_higher" &  fut$RSI_Flag[nrow(fut)] == "RSI_Above") |
      (fut$crossover[nrow(fut)] == "faster_EMA_lower" &  fut$crossover_Volume[nrow(fut)] == "volume_lower" &  fut$RSI_Flag[nrow(fut)] == "In_Between") |
      (fut$crossover[nrow(fut)] == "faster_EMA_lower" &  fut$crossover_Volume[nrow(fut)] == "volume_lower" &  fut$RSI_Flag[nrow(fut)] == "RSI_Above") |
      ((fut$crossover[nrow(fut)] == "faster_EMA_lower")  & (
        (fut$crossover_Volume[nrow(fut)] == "volume_higher" | fut$RSI_Flag[nrow(fut)] == "RSI_Below")))))

    {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
  }
  return(train_data)
}
# ------------------------------------------------------------------------------
# Strategy 4
cross_4_EMA_stoploss <- function(EMA1, EMA2, EMA3, stoploss, takeprofit) {

  # Train and test datasets$
  train_data[, c(paste0("EMA", "_", EMA1),
                 paste0("EMA", "_", EMA2),
                 paste0("EMA", "_", EMA3),
                 "exit_value",
                 "exit_condition",
                 "crossover",
                 "crossover_Volume",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA, NA, NA,  NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c(paste0("EMA", "_", EMA1),
                paste0("EMA", "_", EMA2),
                paste0("EMA", "_", EMA3),
                "exit_value",
                "exit_condition",
                "crossover",
                "crossover_Volume",
                "action",
                "Units",
                "Price",
                "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {

    # Bind last row
    fut <- rbind(train_data, test_data[i, ])

    # Create indicators
    fut[, c(paste0("EMA", "_", EMA1),
            paste0("EMA", "_", EMA2),
            paste0("EMA", "_", EMA3)) := list(EMA(close, n = EMA1),
                                              EMA(close, n = EMA2),
                                              EMA(close, n = EMA3)) ]


    fut$crossover[get(paste0("EMA", "_", EMA1), fut) > get(paste0("EMA", "_", EMA2), fut) & get(paste0("EMA", "_", EMA2), fut) > get(paste0("EMA", "_", EMA3), fut)  ] <- "faster_EMA_higher"
    fut$crossover[is.na(fut$crossover)] <- "faster_EMA_lower"
    # Exit condition for stop loss
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)) / tail(fut$close[!is.na(fut$action) &fut$action =="buy"], 1)

    if (length(exit_value) == 0) {
      exit_value <- 0
    }

    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <- fut$exit_value > takeprofit | fut$exit_value <= -stoploss

    # BUY Condition
    if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) & fut$crossover[nrow(fut)] == "faster_EMA_higher") {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Keep Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("buy", "keep") &(
      fut$crossover[nrow(fut)] == "faster_EMA_higher" &
      fut$exit_condition[nrow(fut)] == FALSE )) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

      # Sell Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               (fut$exit_condition[nrow(fut)] == TRUE | fut$crossover[nrow(fut)] %in% c("faster_EMA_lower"))) {


      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # No Action Condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("sell", "no action") | (is.na(fut$action[nrow(fut) - 1])
                                                                         & fut$crossover[nrow(fut)] == "faster_EMA_lower")) {

      fut$action[nrow(fut)] <- "no action"
    }

    train_data <- fut
    # print(i)
  }
  return(train_data)
}

# Ninja trader Volume reversal with RSI ----------------------------------------
Volume_Reversal_RSI_NJ <- function(stoploss, rsi_bound, rsi_period, periods_volume, times_EMA_Vol, EMA_periods) {

  # Train and test datasets
  train_data[, c("exit_value",
                 "exit_condition",
                 "EMA_volume",
                 "EMA",
                 "RSI",
                 "crossover_volume",
                 "candle_pattern",
                 "Long_singal",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(0,NA, NA,NA,NA,NA, NA, 0, NA, NA, NA, NA) ]

  test_data[, c("exit_value",
                "exit_condition",
                "EMA_volume",
                "EMA",
                "RSI",
                "crossover_volume",
                "candle_pattern",
                "Long_singal",
                "action",
                "Units",
                "Price",
                "id") := list(0, NA,NA,NA,NA,NA, NA, 0, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])


    # Technical indicators -----------------------------------------------------


    fut$EMA_volume <- EMA(fut$volume, periods_volume)
    fut$crossover_volume[nrow(fut)-1] <-  ifelse(fut$volume[nrow(fut)-1] >  fut$EMA_volume[nrow(fut)-1]*times_EMA_Vol, "Volume_Spike","No_Spike")
    # fut$crossover_volume[nrow(fut)-1] <- ifelse(fut$volume[nrow(fut)-1] >  max(fut$volume[(nrow(fut)-periods_volume): (nrow(fut)-2)]), "Volume_Spike","No_Spike")
    fut$RSI <- RSI(fut$close, n = rsi_period)

    fut$candle_pattern[nrow(fut)] <-  (fut$low[nrow(fut)-2] <= fut$low[nrow(fut)-1]) & (fut$low[nrow(fut)-2] <= fut$low[nrow(fut)]) &
      (fut$close[nrow(fut)-2] >= fut$close[nrow(fut)-1]) & (fut$close[nrow(fut)-2] >= fut$open[nrow(fut)-2]) &
      (fut$close[nrow(fut)-2] - fut$open[nrow(fut)-2]) < (fut$high[nrow(fut)-2] - fut$low[nrow(fut)-2])


    fut$Long_singal[nrow(fut)][fut$candle_pattern[nrow(fut)] == TRUE & fut$crossover[nrow(fut)-1] =="Volume_Spike"] <- TRUE

    fut$EMA <- EMA(fut$close, n = EMA_periods)

    # Exit condition for stop loss
    exit_value <- (fut$close[nrow(fut)] - tail(fut$close[!is.na(fut$action) & fut$action %in% c("buy", "keep")], 1)) / tail(fut$close[!is.na(fut$action) &fut$action %in% c("buy", "keep")], 1)

    if (length(exit_value) == 0) {
      exit_value <- 0
    }

    fut$exit_value[nrow(fut)] <- exit_value
    fut$exit_condition <-  fut$exit_value <= -stoploss



    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$Long_singal[nrow(fut)] == TRUE & fut$RSI[nrow(fut)] < rsi_bound) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               fut$exit_condition[nrow(fut)] == TRUE ) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    # print(i)
  }
  return(train_data)
}


# Test  ------------------------------------------------------------------------
Pure_RSI_Volume_Trailing_test <- function(RSI_Period,RSI_above,  RSI_below, EMA_volume, takeprofit, stoploss_trail,stoploss_ult, times_vol) {

  # Train and test datasets
  train_data[, c("RSI",
                 "EMA_volume",
                 "exit_value",
                 "exit_condition",
                 "crossover_volume",
                 "crossover_RSI",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c("RSI",
                "EMA_volume",
                "exit_value",
                "exit_condition",
                "crossover_volume",
                "crossover_RSI",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])

    # RSI and Volume
    fut$RSI <- RSI(fut$close, n = RSI_Period)
    fut$EMA_volume <- EMA(fut$volume, n = EMA_volume)

    # Technical indicators -----------------------------------------------------


    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover_volume[nrow(fut)] <- ifelse(fut$volume[nrow(fut)] > fut$EMA_volume[nrow(fut)] * times_vol ,
                                              "volume_higher", "volume_lower")


    # RSI Crossing of upper or lower bounds
    if(fut$RSI[nrow(fut)] < RSI_below){

      fut$crossover_RSI[nrow(fut)] <- "RSI_lower"

    } else if (fut$RSI[nrow(fut)] > RSI_above){

      fut$crossover_RSI[nrow(fut)] <- "RSI_above"
    } else {

      fut$crossover_RSI[nrow(fut)] <- "RSI_between"
    }



    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }


    # Trailing stop loss

    # id_trail <- tail(fut$id[!is.na(fut$id)], 1)
    #
    # if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
    #
    #   trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #
    #   if(trail_sl > max(fut$trail_sl[which(fut$id == id_trail)+1], na.rm =T)){
    #     trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #
    #   }else{
    #
    #     trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)+1])
    #   }
    #
    #
    #
    # } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1]) ){
    #
    #   trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)])
    #
    # } else {
    #
    #   trail_sl <-0
    # }
    #

    # if(length(trail_sl) == 0 ){
    #
    #   trail_sl <- 0
    # }

    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]

      }


    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){

      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {

      trail_sl <-0
    }


    if(length(trail_sl) == 0 ){

      trail_sl <- 0
    }
    # browser()



    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]


    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover_volume[nrow(fut)] == "volume_higher" &  fut$crossover_RSI[nrow(fut)] == "RSI_above" ) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE  )) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    print(i)
  }
  return(train_data)
}



# ------------------------------------------------------------------------------
# Intented for Forex

EMA_below <- function(EMA_period, percent_below, takeprofit, stoploss_trail,stoploss_ult) {

  # Train and test datasets
  train_data[, c("EMA",
                 "exit_condition",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c("EMA",
                "exit_condition",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])

    # RSI and Volume
    fut$EMA <- EMA(fut$close, n = EMA_period)




    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }


    # Trailing stop loss

    # id_trail <- tail(fut$id[!is.na(fut$id)], 1)
    #
    # if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
    #
    #   trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #
    #   if(trail_sl > max(fut$trail_sl[which(fut$id == id_trail)+1], na.rm =T)){
    #     trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #
    #   }else{
    #
    #     trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)+1])
    #   }
    #
    #
    #
    # } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1]) ){
    #
    #   trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)])
    #
    # } else {
    #
    #   trail_sl <-0
    # }
    #

    # if(length(trail_sl) == 0 ){
    #
    #   trail_sl <- 0
    # }

    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]

      }


    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){

      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {

      trail_sl <-0
    }


    if(length(trail_sl) == 0 ){

      trail_sl <- 0
    }
    # browser()



    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]


    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$close[nrow(fut)]  < fut$EMA[nrow(fut)]*(1-percent_below)) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE  )) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    print(i)
  }
  return(train_data)
}

# ------------------------------------------------------------------------------
# Enchanced previous version of RSI and Volume trailing stop loss with resistance and support levels

Pure_RSI_Volume_Trailing_Dynamic_SRL <- function(RSI_Period, RSI_below, EMA_volume, takeprofit, stoploss_trail,stoploss_ult, times_vol, df_tail, mean_n) {

  # Train and test datasets
  train_data[, c("RSI",
                 "EMA_volume",
                 "exit_value",
                 "exit_condition",
                 "support",
                 "resistance",
                 "crossover_SL_RS",
                 "crossover_volume",
                 "crossover_RSI",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c("RSI",
                "EMA_volume",
                "exit_value",
                "exit_condition",
                "support",
                "resistance",
                "crossover_SL_RS",
                "crossover_volume",
                "crossover_RSI",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)) {

    fut <- rbind(train_data, test_data[i, ])

    # RSI and Volume
    fut$RSI <- RSI(fut$close, n = RSI_Period)
    fut$EMA_volume <- EMA(fut$volume, n = EMA_volume)

    # Technical indicators -----------------------------------------------------


    # Volume Crossing of volume over the SMA(volume, n_periods)
    fut$crossover_volume[nrow(fut)] <- ifelse(fut$volume[nrow(fut)] > fut$EMA_volume[nrow(fut)] * times_vol ,
                                              "volume_higher", "volume_lower")


    # RSI Crossing of upper or lower bounds
    fut$crossover_RSI[nrow(fut)] <- ifelse(fut$RSI[nrow(fut)] < RSI_below ,
                                           "RSI_lower", "RSI_higher")

    # calculate resistance and support level
    fut$support[nrow(fut)] <- mean(tail(sort(tail(fut$close, df_tail)), mean_n))
    fut$resistance[nrow(fut)] <- mean(head(sort(tail(candles$close, df_tail)), mean_n))

    if( fut$close[nrow(fut)] < fut$support[nrow(fut)]) {

      fut$crossover_SL_RS[nrow(fut)] <- "Below_support"

    } else if (fut$close[nrow(fut)] > fut$resistance[nrow(fut)] ) {

      fut$crossover_SL_RS[nrow(fut)] <- "Above_resistance"
    }else{

      fut$crossover_SL_RS[nrow(fut)] <- "Above_resistance"
    }

    # abline( h =   fut$resistance[nrow(fut)])
    # abline( h = fut$support[nrow(fut)] )


    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }



    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]

      }


    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){

      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {

      trail_sl <-0
    }


    if(length(trail_sl) == 0 ){

      trail_sl <- 0
    }
    # browser()



    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]


    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$crossover_volume[nrow(fut)] == "volume_higher" &  fut$crossover_RSI[nrow(fut)] == "RSI_lower"  & fut$crossover_SL_RS[nrow(fut)] == "Below_support") {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE  )) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    print(i)
  }
  return(train_data)
}


# Strategy using volumes spikes and RSI oversold conditions
RSI_splines <- function (RSI_Period,RSI_lower,RSI_upper,spar,stoploss_ult,plot.it){

  # Train and test datasets
  train_data[, c("x",
                 "RSI",
                 "spline",
                 "deriv",
                 "exit_condition",
                 "signal_RSI",
                 "action",
                 "Units",
                 "Price",
                 "ult_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA) ]

  test_data[, c("x",
                "RSI",
                "spline",
                "deriv",
                "exit_condition",
                "signal_RSI",
                "action",
                "Units",
                "Price",
                "ult_sl",
                "id") := list(NA, NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA)  ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])

    # RSI and Volume
    fut$RSI <- RSI(fut$close, n = RSI_Period)
    fut$x <- 1:nrow(fut)

    # RSI Crossing of upper or lower bounds

    if(fut$RSI[nrow(fut)] < RSI_upper &  fut$RSI[nrow(fut)] > RSI_lower){

      fut$signal_RSI[nrow(fut)] <-"RSI_between"
    } else if (fut$RSI[nrow(fut)] < RSI_lower){
      fut$signal_RSI[nrow(fut)] <-"RSI_lower"
    } else {
      fut$signal_RSI[nrow(fut)] <-"RSI_above"
    }
    # Calculate spline - derivative
    smoothingSpline = smooth.spline(fut[, close] ~ as.numeric(rownames(fut)) , spar = spar)
    fut[, spline := predict(smoothingSpline)$y]
    fut[, deriv := predict(smoothingSpline, deriv = 1)$y]


    if(plot.it == TRUE){

      plot_df <- tail(fut, 200)
      df_points_buy <- data.frame(x = na.omit(plot_df$x[plot_df$action =="buy"]),
                                  y = na.omit(plot_df$close[plot_df$action =="buy"]))
      df_points_sell <- data.frame(x = na.omit(plot_df$x[plot_df$action =="sell"]),
                                   y = na.omit(plot_df$close[plot_df$action == "sell"]))

      par(mfrow = c(3, 1))
      plot(plot_df$close, type ="l", main = paste0("profits = ", tail(na.omit(plot_df$Price), 1)))
      lines(plot_df$spline, col ="red")
      points(rownames(plot_df)[plot_df$x %in% df_points_buy$x], df_points_buy$y, col ="green", pch = 19)
      points(rownames(plot_df)[plot_df$x %in% df_points_sell$x], df_points_sell$y, col ="red", pch = 19)


      plot(plot_df$RSI, type ="l")
      abline(h = RSI_upper,  col = "red", lty = 5, lwd = 2)
      abline(h = RSI_lower,  col = "red", lty = 5, lwd = 2)

      plot(plot_df$deriv, type ="l")
      abline(h = 0, lty = 5, lwd = 2)
      }



    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }


    fut$ult_sl[nrow(fut)] <- ult_sl


    fut$exit_condition[nrow(fut)] <-  fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)]


    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$signal_RSI[nrow(fut)] == "RSI_lower" & fut$deriv[nrow(fut)] > 0
    ) {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)] - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE | fut$RSI[nrow(fut)] > RSI_upper )) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)] - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                (fut$exit_condition[nrow(fut)] == FALSE)  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    # print(i)
    if(plot.it == TRUE){
      Sys.sleep(0.1)
      # flush.console()
    }
  }
  return(train_data)
}


# Strategy 2 -------------------------------------------------------------------
# Crossing fast ema to slower with stoploss condition

cross_EMA_stoploss_trail <- function(fast_EMA, slow_EMA,takeprofit, stoploss_trail,stoploss_ult) {

  # Train and test datasets$
  train_data[, c(paste0("EMA", "_", fast_EMA),
                 paste0("EMA", "_", slow_EMA),
                 "exit_value","exit_condition",
                 "crossover",
                 "crossover_Volume",
                 "action",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "Units",
                 "Price",
                 "id") := list(NA, NA,NA,NA,NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c(paste0("EMA", "_", fast_EMA),
                paste0("EMA", "_", slow_EMA),
                "exit_value",
                "exit_condition",
                "crossover",
                "crossover_Volume",
                "action",
                "tp",
                "ult_sl",
                "trail_sl",
                "Units",
                "Price",
                "id") := list(NA, NA, NA,NA,NA,NA, NA, NA, NA ,NA, NA, NA, NA) ]

  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {

    # Bind last row
    fut <- rbind(train_data, test_data[i, ])

    # Create indicators
    fut[, c(paste0("EMA", "_", fast_EMA),
            paste0("EMA", "_", slow_EMA)) := list(EMA(close, n = fast_EMA),
                                                  EMA(close, n = slow_EMA)) ]


    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) > get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_higher"
    fut$crossover[get(paste0("EMA", "_", fast_EMA), fut) <= get(paste0("EMA", "_", slow_EMA), fut)] <- "faster_EMA_lower"






    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)


    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }


    # Trailing stop loss

    # id_trail <- tail(fut$id[!is.na(fut$id)], 1)
    #
    # if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){
    #
    #   trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #
    #   if(trail_sl > max(fut$trail_sl[which(fut$id == id_trail)+1], na.rm =T)){
    #     trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
    #
    #   }else{
    #
    #     trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)+1])
    #   }
    #
    #
    #
    # } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1]) ){
    #
    #   trail_sl <- max(fut$trail_sl[which(fut$id == id_trail)])
    #
    # } else {
    #
    #   trail_sl <-0
    # }
    #

    # if(length(trail_sl) == 0 ){
    #
    #   trail_sl <- 0
    # }

    # Trailing stop loss
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]

      }


    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){

      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {

      trail_sl <-0
    }


    if(length(trail_sl) == 0 ){

      trail_sl <- 0
    }



    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]


    # BUY Condition
    if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action"))
        & fut$crossover[nrow(fut)] == "faster_EMA_higher") {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Keep Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("buy", "keep") &
               fut$exit_condition[nrow(fut)] == FALSE ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

      # Sell Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               fut$exit_condition[nrow(fut)] == TRUE) {


      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # No Action Condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("sell", "no action") | (is.na(fut$action[nrow(fut) - 1])
                                                                         & fut$crossover[nrow(fut)] == "faster_EMA_lower")) {

      fut$action[nrow(fut)] <- "no action"
    }

    train_data <- fut
  }
  return(train_data)
  if(plot.it == TRUE){
    Sys.sleep(0.1)
    # flush.console()
  }
}


cross_EMA_stoploss_trail_simple <- function(slow_SMA,takeprofit, stoploss_trail,stoploss_ult) {

  # Train and test datasets$
  train_data[, c("SMA",
                 "exit_value","exit_condition",
                 "crossover",
                 "crossover_Volume",
                 "action",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "Units",
                 "Price",
                 "id") := list(NA,NA,NA,NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c("SMA",
                "exit_value",
                "exit_condition",
                "crossover",
                "crossover_Volume",
                "action",
                "tp",
                "ult_sl",
                "trail_sl",
                "Units",
                "Price",
                "id") := list(NA, NA,NA,NA,NA, NA, NA, NA ,NA, NA, NA, NA) ]

  # Loop to evaluate last line of the test dataset
  for (i in 1:nrow(test_data)) {

    # Bind last row
    fut <- rbind(train_data, test_data[i, ])

    # Create indicators
    fut$SMA <- SMA(fut$close, n = slow_SMA)


    fut$crossover[nrow(fut)] <- ifelse(fut$close[nrow(fut)] > fut$SMA[nrow(fut)]  ,
                                       "price_higher", "price_lower")

    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)


    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }




    # Trailing stop loss
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] > fut$close[nrow(fut)-1] )  ){

      trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      }else {
        trail_sl <- fut$close[nrow(fut)] - stoploss_trail * fut$close[nrow(fut)]

      }


    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$close[nrow(fut)] <= fut$close[nrow(fut)-1] ) ){

      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {

      trail_sl <-0
    }


    if(length(trail_sl) == 0 ){

      trail_sl <- 0
    }



    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]


    # BUY Condition
    if ((is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action"))&
        fut$crossover[nrow(fut)] == "price_higher") {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)]*fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Keep Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("buy", "keep") &
               (fut$exit_condition[nrow(fut)] == FALSE & fut$crossover[nrow(fut)] == "price_higher") ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

      # Sell Condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               (fut$exit_condition[nrow(fut)] == TRUE | fut$crossover[nrow(fut)] == "price_lower")) {


      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # No Action Condition
    } else  {

      fut$action[nrow(fut)] <- "no action"
    }

    train_data <- fut
    print(i)

    }

  return(train_data)
}


portfolio_rsi_reversal <- function(ATR_period, RSI_period, roll_max_period, roll_min_period ,takeprofit, stoploss_trail,stoploss_ult) {

  # Train and test datasets
  train_data[, c("ATR",
                 "RSI",
                 "Roll_Max",
                 "Roll_Min",
                 "M_minus",
                 "Signal",
                 "exit_condition",
                 "action",
                 "Units",
                 "Price",
                 "tp",
                 "ult_sl",
                 "trail_sl",
                 "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  test_data[, c("ATR",
                "RSI",
                "Roll_Max",
                "Roll_Min",
                "M_minus",
                "Signal",
                "exit_condition",
                "action",
                "Units",
                "Price",
                "tp",
                "ult_sl",
                "trail_sl",
                "id") := list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ]

  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){

    fut <- rbind(train_data, test_data[i, ])

    # Technical indicators -----------------------------------------------------
    fut$ATR <- ATR(fut[,c("High","Low","Close")], n = ATR_period)[,2]
    fut$RSI <- RSI(fut$Close, n = RSI_period)
    fut$Roll_Max <- runMax(fut$Close, n = roll_max_period, cumulative = FALSE)
    fut$Roll_Min <- runMin(fut$Close, n = roll_min_period, cumulative = FALSE)

    fut$M_minus <- (fut$Close - fut$Roll_Min)/fut$ATR
    fut$Signal <- ifelse(fut$M_minus <= 2 & fut$RSI > 30, "long", "nothing")

    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$Close[fut$action == "buy"][!is.na(fut$Close[fut$action == "buy"])], 1) + takeprofit * tail(fut$Close[fut$action == "buy"][!is.na(fut$Close[fut$action == "buy"])], 1)

    if (length(tp) == 0) {
      tp <- 0
    }

    # Ultimate stop loss
    ult_sl <- tail(fut$Close[fut$action == "buy"][!is.na(fut$Close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$Close[fut$action == "buy"][!is.na(fut$Close[fut$action == "buy"])], 1)

    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }


    # Trailing stop loss
    # browser()
    if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$Close[nrow(fut)] > fut$Close[nrow(fut)-1] )  ){

      trail_sl <- fut$Close[nrow(fut)] - stoploss_trail * fut$Close[nrow(fut)]
      if( trail_sl < tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)){
        trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)

      }else {
        trail_sl <- fut$Close[nrow(fut)] - stoploss_trail * fut$Close[nrow(fut)]

      }


    } else if (fut$action[nrow(fut)-1] %in% c("buy", "keep") & ( fut$Close[nrow(fut)] <= fut$Close[nrow(fut)-1] ) ){

      trail_sl <- tail(fut$trail_sl[!is.na(fut$trail_sl)], 1)
    } else {

      trail_sl <-0
    }


    if(length(trail_sl) == 0 ){

      trail_sl <- 0
    }
    # browser()



    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    fut$trail_sl[nrow(fut)] <- trail_sl


    fut$exit_condition[nrow(fut)] <- fut$trail_sl[nrow(fut)] > fut$Close[nrow(fut)] | fut$ult_sl[nrow(fut)] > fut$Close[nrow(fut)] | fut$tp[nrow(fut)] < fut$Close[nrow(fut)]


    # Deciding upon action -----------------------------------------------------

    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         fut$Signal[nrow(fut)] == "long") {

      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$Close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$Close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))

      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") &
               (fut$exit_condition[nrow(fut)] == TRUE  | fut$RSI[nrow(fut)] > 70 | fut$RSI[nrow(fut)] < 15)   ) {

      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$Close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]

      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE  ) {

      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]

    } else {

      fut$action[nrow(fut)] <- "no action"

    }

    train_data <- fut
    print(i)
  }
  return(train_data)
}

splines_fast_slow_cross <- function(spar_fast, spar_slow,spar_slower, takeprofit,stoploss_ult,plot.it) {
  
  # Train and test datasets
  train_data[, c("x",
                 "spline_fast",
                 "spline_slow",
                 "spline_slower",
                 "exit_condition",
                 "tp",
                 "ult_sl",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA) ]
  
  test_data[, c("x",
                "spline_fast",
                "spline_slow",
                "spline_slower",
                "exit_condition",
                "tp",
                "ult_sl",
                "action",
                "Units",
                "Price",
                "id") := list(NA, NA, NA, NA, NA, NA,
                              NA, NA, NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    fut$x <- 1:nrow(fut)
    
    # Spline calculation -------------------------------------------------------
    
    fut[, spline_fast := SMA(close, spar_fast) ]
    fut[, spline_slow := SMA(close, spar_slow) ]
    fut[, spline_slower := SMA(close,spar_slower) ]
    
    if(plot.it == TRUE){
      
      plot_df <- tail(fut, 200)
      
      # sr <- SR_lines(fut, roll = 100, Ns = nrow(fut), n_sort = 15, pair = pair,plot.it = F)
      # fut <- tail(fut, 250)
      df_points_buy <- data.frame(x = na.omit(plot_df$x[plot_df$action =="buy"]),
                                  y = na.omit(plot_df$close[plot_df$action =="buy"]))
      df_points_sell <- data.frame(x = na.omit(plot_df$x[plot_df$action =="sell"]),
                                   y = na.omit(plot_df$close[plot_df$action == "sell"]))
      
      par(mfrow = c(1, 1))
      plot(plot_df$close, type ="l", main = paste0("profits = ", tail(na.omit(plot_df$Price), 1)))
      lines(plot_df$spline_slow, col ="blue")
      lines(plot_df$spline_fast, col ="red")
      lines(plot_df$spline_slower, col ="green")
      
      points(rownames(plot_df)[plot_df$x %in% df_points_buy$x], df_points_buy$y, col ="green", pch = 19)
      points(rownames(plot_df)[plot_df$x %in% df_points_sell$x], df_points_sell$y, col ="red", pch = 19)
    }
    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    
    fut$exit_condition[nrow(fut)] <-  fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    
    # Deciding upon action -----------------------------------------------------
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$spline_fast[nrow(fut)] > fut$spline_slow[nrow(fut)] &
          fut$spline_fast[nrow(fut)] > fut$spline_slower[nrow(fut)])) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)] - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      # fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == TRUE  )) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]  - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      # fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == FALSE ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    # print(i)
    if(plot.it == TRUE){
      Sys.sleep(0.1)
      # flush.console()
    }
    print(i)
  }
  return(train_data)
}


simple_SMA <- function(sma, takeprofit,stoploss_ult, plot.it) {
  
  # Train and test datasets
  train_data[, c("x",
                 "SMA",
                 "exit_condition",
                 "tp",
                 "ult_sl",
                 "action",
                 "Units",
                 "Price",
                 "id") := list(NA, NA, NA, NA, NA, NA,
                               NA, NA, NA) ]
  
  test_data[, c("x",
                "SMA",
                "exit_condition",
                "tp",
                "ult_sl",
                "action",
                "Units",
                "Price",
                "id") := list(NA, NA, NA, NA, NA, NA,
                              NA, NA, NA) ]
  
  # Going intro the loop for test data -----------------------------------------
  for (i in 1:nrow(test_data)){
    
    fut <- rbind(train_data, test_data[i, ])
    fut$x <- 1:nrow(fut)
    
    # OBV and SMA calculation -------------------------------------------------------
    fut[, SMA := EMA(fut[, close], n = sma)]
    
    if(plot.it == TRUE){
      
      plot_df <- tail(fut, 200)
      
      # sr <- SR_lines(fut, roll = 100, Ns = nrow(fut), n_sort = 15, pair = pair,plot.it = F)
      # fut <- tail(fut, 250)
      df_points_buy <- data.frame(x = na.omit(plot_df$x[plot_df$action =="buy"]),
                                  y = na.omit(plot_df$close[plot_df$action =="buy"]))
      df_points_sell <- data.frame(x = na.omit(plot_df$x[plot_df$action =="sell"]),
                                   y = na.omit(plot_df$close[plot_df$action == "sell"]))
      
      par(mfrow = c(1, 1))
      plot(plot_df$close, type ="l", main = paste0("profits = ", tail(na.omit(plot_df$Price), 1)))
      lines(plot_df$SMA, col ="blue")
      
      points(rownames(plot_df)[plot_df$x %in% df_points_buy$x], df_points_buy$y, col ="green", pch = 19)
      points(rownames(plot_df)[plot_df$x %in% df_points_sell$x], df_points_sell$y, col ="red", pch = 19)
      
    }
    
    # Exit condition for takeprofit  - Fixed
    tp <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) + takeprofit * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(tp) == 0) {
      tp <- 0
    }
    
    # Ultimate stop loss
    ult_sl <- tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1) - stoploss_ult * tail(fut$close[fut$action == "buy"][!is.na(fut$close[fut$action == "buy"])], 1)
    
    if (length(ult_sl) == 0) {
      ult_sl <- 0
    }
    fut$tp[nrow(fut)] <- tp
    fut$ult_sl[nrow(fut)] <- ult_sl
    
    # fut$exit_condition[nrow(fut)] <-  fut$ult_sl[nrow(fut)] > fut$close[nrow(fut)] | fut$tp[nrow(fut)] < fut$close[nrow(fut)]
    fut$exit_condition[nrow(fut)] <-  fut$close[nrow(fut)] > fut$SMA[nrow(fut)]
    # Deciding upon action -----------------------------------------------------
    # Buy condition
    if ( (is.na(fut$action[nrow(fut) - 1]) |  fut$action[nrow(fut) - 1] %in% c("sell", "no action")) &
         (fut$exit_condition[nrow(fut)] == TRUE) ) {
      
      fut$action[nrow(fut)] <- "buy"
      fut$Units[nrow(fut)] <- initial_budget / fut$close[nrow(fut)]
      fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)] - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      # fut$Price[nrow(fut)] <- fut$Units[nrow(fut)] * fut$close[nrow(fut)]
      fut$id[nrow(fut)] <- round(runif(1, 10000, 5000000))
      
      # Sell condition
    } else if (fut$action[nrow(fut) - 1] %in% c("keep", "buy") & (
      fut$exit_condition[nrow(fut)] == FALSE  )) {
      
      fut$action[nrow(fut)] <- "sell"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1]
      fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]  - (0.0026 * fut$Units[nrow(fut)] * fut$close[nrow(fut)])
      # fut$Price[nrow(fut)] <- fut$close[nrow(fut)]* fut$Units[nrow(fut)]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      initial_budget <- fut$Price[nrow(fut)]
      
      # Keep condition
    } else if ( fut$action[nrow(fut) - 1] %in% c("buy", "keep")   &
                fut$exit_condition[nrow(fut)] == TRUE ) {
      
      fut$action[nrow(fut)] <- "keep"
      fut$Units[nrow(fut)] <- fut$Units[nrow(fut) -1 ]
      fut$id[nrow(fut)] <- fut$id[nrow(fut)-1]
      
    } else {
      
      fut$action[nrow(fut)] <- "no action"
      
    }
    
    train_data <- fut
    # print(i)
    if(plot.it == TRUE){
      Sys.sleep(0.1)
      # flush.console()
    }
    print(i)
  }
  return(train_data)
}
