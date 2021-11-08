# Select parameters ------------------------------------------------------------

# 1. General EMAS
ema1 <- data.frame(ema1 = c(10 ,50, 10), flag = 1)
ema2 <- data.frame(ema2 = seq(50 ,100, 10), flag = 1)
ema3 <- data.frame(ema3 = seq(200, 250, 10), flag = 1)
tp <- data.frame(tp = c(0.1)*100, flag = 1)
sl <- data.frame(sl = c(0.05)*100, flag = 1)
testing_params <- as.data.table(left_join(ema1, ema2) %>% left_join(ema3) %>% left_join(tp)
                                %>% left_join(sl))
# testing_params <- as.data.table(left_join(ema1, ema2) %>% left_join(ema3))
# testing_params <- testing_params[ema1 < ema2]
# testing_params <- testing_params[ema2 < ema3]
# 2. General support and resistance levels
n_sort <- data.frame(n_sort = c(2, 5), flag = 1)
look_back <- data.frame(look_back = c(10, 20, 50, 100), flag = 1)
tp <- data.frame(tp = c(0.01, 0.02, 0.04)*100, flag = 1)
sl <- data.frame(sl = c(0.01, 0.02)*100, flag = 1)
per <- data.frame(per = c(0, 0.01, 0.02, 0.03), flag = 1)
n_exclude <- data.frame(n_exclude = c(5, 10), flag = 1)
n_rsi <- data.frame(n_rsi = c(14, 28, 5), flag = 1)
rsi_below <- data.frame(rsi_below = c(25, 30, 10), flag = 1)

testing_params <- as.data.table(left_join(n_sort, look_back) %>% left_join(sl)
                                %>% left_join(tp)%>% left_join(per)%>%left_join(n_exclude)%>%left_join(n_rsi)%>%left_join(rsi_below))
testing_params<- testing_params[look_back > n_sort,]
testing_params<- testing_params[look_back > n_exclude,]

n_sort <- data.frame(n_sort = c(5), flag = 1)
look_back <- data.frame(look_back = c(20, 50, 100), flag = 1)
tp <- data.frame(tp = c(0.03,  0.04, 0.06, 0.1)*100, flag = 1)
tp <- data.frame(tp = c(0.05)*100, flag = 1)

sl <- data.frame(sl = c(0.02, 0.03, 0.04)*100, flag = 1)
per <- data.frame(per = c(0, 0.03, 0.05, 0.1), flag = 1)
n_exclude <- data.frame(n_exclude = c(10, 20, 50), flag = 1)
n_rsi <- data.frame(n_rsi = c(14, 28, 5), flag = 1)
rsi_below <- data.frame(rsi_below = c(25, 30, 10), flag = 1)

testing_params <- as.data.table(left_join(n_sort, look_back) %>% left_join(sl)
                                %>% left_join(tp)%>% left_join(per)%>%left_join(n_exclude)%>%left_join(n_rsi)%>%left_join(rsi_below))
testing_params<- testing_params[look_back > n_sort,]
testing_params<- testing_params[look_back > n_exclude,]

# Regression
# window <- data.frame(window = c(50, 100, 200, 300, 400, 600), flag = 1)
# tp <- data.frame(tp = c(0.01, 0.02, 0.03, 0.05)*100, flag = 1)
# sl <- data.frame(sl = c(0.01, 0.02)*100, flag = 1)
# atr_wind <- data.frame(atr_wind = c(14, 500), flag = 1)
# testing_params <- as.data.table(left_join(window, tp) %>% left_join(sl))%>% left_join(atr_wind)
# testing_params <- testing_params[tp >= sl, ]
# testing_params <- testing_params[window >= atr_wind, ]

# Ema plus sd
# window <- data.frame(window = seq(10, 600, 10), flag = 1)
# tp <- data.frame(tp = c(0.01, 0.02, 0.03, 0.05)*100, flag = 1)
# sl <- data.frame(sl = c(0.01, 0.02)*100, flag = 1)
# ema <- data.frame(ema = seq(10, 600, 10), flag = 1)
# testing_params <- as.data.table(left_join(window, tp) %>% left_join(sl))%>% left_join(ema)
# testing_params <- testing_params[tp >= sl, ]
# testing_params <- testing_params[window >= ema, ]



# can also add parameter for atr
# Initialise
strategy_results <- list()
set_summary <- list()
profs <- c()
wr <- c()
n_trades <- c()
j <- 268
for(j in 1:nrow(testing_params)){
  
  # Select df  
  df <- copy(klines[[1]])
  
  # Define sl and tp  
  tp <- testing_params$tp[j]
  sl <- -testing_params$sl[j]
  df[, rsi := RSI(close, n = testing_params$n_rsi[j])]
  SR_lines_strategy_breakout_luc_rsi()
  # EMA_strategy123()
  
  trades_evaluation_withTPSL()
  
  if("status" %in% colnames(df)){
  df[, returns_strat := c(diff(df[, close]), 0)]
  df[status =="exited_trade", returns_strat := 0]
  
  # idx <- df[position == "long"   ,max(index), by = groups]
  # df[idx$V1, returns_strat := 0]
  # View(df)
  df[is.na(position), returns_strat := 0]
  df[, returns := c(diff(df[, close]), 0)]
  
  df[status %in% c("entered_trade", "exited_trade") , fees := 0.0026*close]
  df[status %in% c("entered_trade", "exited_trade") ,returns_strat := returns_strat - fees]
  profs[j] <- sum(df$returns_strat)
  } else {
    profs[j] <- 0
  }
  
  if (profs[j] != 0 ){
  trade_ids <- data.table(enters = which(df$status == "entered_trade"), exits = which(df$status == "exited_trade"))
  trade_ids[, id := sample(500:5000, nrow(trade_ids), replace = FALSE)]
  }
  percent <- c()
  for(o in 1:nrow(trade_ids)){
    df[trade_ids$enters[o]:trade_ids$exits[o], id := trade_ids$id[o]]
    percent[o] <- df[trade_ids$exits[o], close] - df[trade_ids$enters[o], close]
  }
  
  wr[j] <- length(percent[percent>0])/length(percent)
  n_trades[j] <- length(percent)
  # print(paste0(profs[j], " for rep ", j, " params: ",  paste(testing_params$n_sort[j], 
  #                                                            testing_params$look_back[j],
  #                                                            testing_params$sl[j],
  #                                                            testing_params$tp[j],
  #                                                            testing_params$per[j],
  #                                                            testing_params$n_exclude[j],
  #                                                            testing_params$n_rsi[j],
  #                                                            testing_params$rsi_below[j],
  #                                                            sep = "-")))
  print(j)
  
}

# Parameter result
# set_summary <- do.call(rbind, strategy_results)
# set_summary[, params_string := paste(n_sort,look_back, sl, tp,per,n_exclude, sep = "_")]
# View(set_summary)
# Select best for visualizations
View(data.table(profs, wr, n_trades, testing_params))
j <- which.max(profs)
profs[j]
wr[j]
n_trades[j]
df <- copy(klines[[1]])
plot(density(profs))
mean(profs)
# Define sl and tp  
# tp <- testing_params$tp[j]
# sl <- -testing_params$sl[j]

# Select strategy
# SR_lines_strategy_breakout_luc()
df <- copy(klines[[1]])

# Define sl and tp  
tp <- testing_params$tp[j]
sl <- -testing_params$sl[j]
# Select strategy

df[, rsi := RSI(close, n = testing_params$n_rsi[j])]
SR_lines_strategy_breakout_luc_rsi()
# regression()
# EMA_strategy123()
# ema_sd()

# EMA_strategy123_sar()
# df[is.na(signal), signal := "noaction"]
# df[, index :=  1:nrow(df)]
# df[, groups:= as.character(rep(1:length(rle(df$signal)$lengths),
#                                rle(df$signal)$lengths))]
# df[is.na(signal),  groups := NA]
# entries <- df[, head(index, 1), by=groups][!is.na(groups)]
# exits <- df[, tail(index, 1), by=groups][!is.na(groups)]
# df[entries$V1, status := "entered"]
# df[exits$V1, status := "exited"]
trades_evaluation_withTPSL()

df[, returns_strat := c(diff(df[, close]), 0)]
df[status =="exited_trade", returns_strat := 0]

# idx <- df[position == "long"   ,max(index), by = groups]
# df[idx$V1, returns_strat := 0]
# View(df)
df[is.na(position), returns_strat := 0]
df[, returns := c(diff(df[, close]), 0)]

df[status %in% c("entered_trade", "exited_trade") , fees := 0.0026*close]
df[status %in% c("entered_trade", "exited_trade") ,returns_strat := returns_strat - fees]

# Visualization of best parameter setting

# Cumulative return vs strategy
# df[, diffs := c(0,diff(df[, close]))]
# df[status =="exited_trade", diffs := 0]
# df[, diffs_strategy := c(0,diff(df[, close]))]
# df[status =="exited_trade", diffs_strategy := 0]
# df[is.na(position), diffs_strategy:= 0]
# View(df)
p1 <- ggplot(df, aes(x = full_date_time)) + 
  geom_line(aes(y = cumsum(returns)), color = "darkred") + 
  geom_line(aes(y = cumsum(returns_strat)), color="steelblue") 

(df[, tail(close, 1)] - df[, head(close, 1)])/df[, head(close, 1)]*100

(tail(cumsum(df[, returns_strat]), 1)+df[, head(close, 1)] - df[, head(close, 1)])/df[, head(close, 1)]*100

# Trades area
df[, x := 1:nrow(df)]


df1 <- df
entries <- df1[status == "entered_trade", .(x, close)][, status := "entered_trade"]
exits <- df1[status == "exited_trade", .(x, close)][, status := "exited_trade"]
all <- rbind(entries, exits)



p <- ggplot(df1, aes(x = x, y = close)) +
  geom_line()+ theme(legend.title = element_blank())
xsmin <- df1[status == "entered_trade", x]
xsmax <- df1[status == "exited_trade", x]

p2 <- p +  theme(legend.position="none")+
          annotate("rect", xmin = xsmin, xmax = xsmax, ymin = min(df1[, close]),
                   ymax = rep(max(df1[, close]), length(xsmax)),
                   alpha = .2,fill = "blue")+
          geom_point(data = all, 
                     mapping = aes(x = x, y = close, color = factor(status)))+
          scale_color_manual(values=c("green", "red")) 
p2
# p2 <-  ggplot(data=df1, aes(x = x, y = rsi))+ geom_line()
library(ggpubr)
ggarrange(p1, p2, nrow=2)

p2 <- p1 + 
  geom_line(data= df, aes(x = x, y = volatup), color = "blue") +
  geom_line(data= df, aes(x = x, y = volatdown), color = "yellow")
p2
View(df)

