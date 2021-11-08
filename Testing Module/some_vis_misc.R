df <- copy(klines[[1]][1101:2000,])
df$idx <- 1:nrow(df)
testing_params$n_sort[j] <- 1
testing_params$look_back[j] <- 50
testing_params$n_exclude[j] <- 1
testing_params$per[j] <- 0
SR_lines_strategy_breakout_luc()
df$signal[is.na(df$signal)] <- "noaction"
plot(df$close, type ="l")
points(df$idx[df$signal == "long"], df$close[df$signal == "long"], col = "green", pch =19)
abline(h = df$support)
plot.it <- TRUE

i <- 201
for (i in 201:nrow(df)){
  df1 <- df[1:i, ]
  if(plot.it == TRUE){
    
    plot_df <- tail(df1, 200)
    plot(plot_df$close, type ="l")
    # abline( h = tail(plot_df$support, 1))
    
    if(tail(plot_df$close, 1) < tail(plot_df$support, 1)){
     points(200, tail(plot_df$close, 1), col = "green", pch = 19) 
    }
    # sr <- SR_lines(fut, roll = 100, Ns = nrow(fut), n_sort = 15, pair = pair,plot.it = F)
    # fut <- tail(fut, 250)
    
  }
Sys.sleep(0.1)
}

View(df)
# notes
# Firsst I need a good visualization of the strategy
# Then i need a robust test of the strategy with a single parameter set
# When this is done, neeed performace metrics for the single parameter set
# Then think about parameter optimization
# df <- copy(klines[[1]][1101:2000,])
df$x <- 1:nrow(df)
plot.it <- TRUE

i <- 201
for (i in 201:nrow(df)){
  df1 <- df[1:i, ]
  if(plot.it == TRUE){
    
    plot_df <- tail(df1, 200)
    plot(plot_df$close, type ="l")
    # abline( h = tail(plot_df$support, 1))
    
    df_points_buy <- data.frame(x = na.omit(plot_df$x[plot_df$status =="entered_trade"]),
                                y = na.omit(plot_df$close[plot_df$status =="entered_trade"]))
    df_points_sell <- data.frame(x = na.omit(plot_df$x[plot_df$status =="exited_trade"]),
                                 y = na.omit(plot_df$close[plot_df$status == "exited_trade"]))
    
    
    points(rownames(plot_df)[plot_df$x %in% df_points_buy$x], df_points_buy$y, col ="green", pch = 19)
    points(rownames(plot_df)[plot_df$x %in% df_points_sell$x], df_points_sell$y, col ="red", pch = 19)
    lines(plot_df$ema1, col = "red")
    lines(plot_df$ema2, col = "darkgreen")
    lines(plot_df$ema3, col = "blue")
    # sr <- SR_lines(fut, roll = 100, Ns = nrow(fut), n_sort = 15, pair = pair,plot.it = F)
    # fut <- tail(fut, 250)
    
  }
  Sys.sleep(0.1)
}


# notes
# Firsst I need a good visualization of the strategy
# Then i need a robust test of the strategy with a single parameter set
# When this is done, neeed performace metrics for the single parameter set
# Then think about parameter optimization
# 
# 
par(mfrow =c(1,1))
plot(df$close, type = "l")
df$state <- NA
df$state[df$position == "long"] <- 1
df$state[is.na(df$state)] <- 0
polygon(x = c(0,0, 200, 400),                           # X-Coordinates of polygon
        y = c(0,2,0, 2),                             # Y-Coordinates of polygon
        col = "blue")
segments(200, 2, 400, 2.5)                                      # Add line segments to plot
plot(df$state, type="l")


ggplot(data = df, mapping = aes(x = x, y = close)) +
  geom_line()+
  geom_area(mapping = aes(x = ifelse(state == 1  , x, 0)), fill = "red")


ggplot(df, aes(x,close)) + 
  geom_rect(data=df, inherit.aes=FALSE,
            aes(xmin=min(x),xmax=max(x),ymin=min(close),ymax=max(close)), alpha=0.2)+
  geom_line(size=4, shape=19) +
  scale_color_manual(values=c("red", "gray55"))+
  scale_fill_manual(values=c("green", "blue")) +
  guides(fill="none")       



p <- ggplot(df, aes(x = x, y = close)) +
  geom_line()

View(df)

p +
  annotate("rect", xmin = c(67, 420), xmax = c(79, 600), ymin = c(0.8, 0.8), ymax = c(2.5, 2.5),
           alpha = .1,fill = "blue")
