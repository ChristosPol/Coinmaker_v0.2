# To implement:
# Run all params for train data and then all for test
# Select the ones that are consistent and pick best
# define parameters
# n_sort <- data.frame(n_sort = seq(2, 50, 1), flag = 1)
# look_back <- data.frame(look_back = seq(20, 500, 20), flag = 1)
rsi <- data.frame(rsi = seq(2, 20, 1), flag = 1)
rsi_top <- data.frame(rsi_top = seq(60, 95, 5), flag = 1)
rsi_bot <- data.frame(rsi_bot = seq(5, 45, 5), flag = 1)
tp <- data.frame(tp = c(0.015, 0.02, 0.03, 0.04)*100, flag = 1)
sl <- data.frame(sl = c(0.01, 0.02, 0.03)*100, flag = 1)

# Create set pof parameters
# testing_params <- as.data.table(left_join(n_sort, look_back) %>% left_join(sl)%>% left_join(tp))
# testing_params<- testing_params[look_back > n_sort,]
# testing_params<- testing_params[tp > sl,]

testing_params <- as.data.table(left_join(rsi, rsi_top) %>% left_join(rsi_bot)%>% left_join(sl)%>% left_join(tp))
testing_params <- as.data.table(testing_params)

# Initialise
strategy_results <- list()
profs <- c()


for(j in 1:nrow(testing_params)){

  df <- copy(klines_train[[1]])
  # df <- copy(klines_test[[1]])
  
  tp <- testing_params$tp[j]
  sl <- -testing_params$sl[j]
  
  
  # SR_lines_strategy()
  RSI_strategy()

  df[, groups:= as.character(rep(1:length(rle(df$signal)$lengths),
                                 rle(df$signal)$lengths))]
  
  df[is.na(signal), groups := NA]
  df[, index :=  1:nrow(df)]
  
  entries <- df[, head(index, 1), by=groups][!is.na(groups)]
  
  df[entries$V1, entry := "enter"]
  df[, position := character()]
  idx_signals <- which(df$entry == "enter")
  price_signals <- as.vector(na.omit(df$close[(df$entry == "enter")]))
  
  if(length(idx_signals) >0 ){
    if(length(price_signals) == 1){
      index <-1
    } else
       index <- (length(idx_signals)-1)
    
  for (i in 1:index){
    
    if(is.na(df$position[idx_signals[i]])  & df$signal[idx_signals[i]] == "long"){
      returns_1 <- round((df$close[idx_signals[i]:nrow(df)] - price_signals[i]) / price_signals[i], 3)*100
      returns_1 <-returns_1[-1]
      exit_1 <- head(which(returns_1 > tp | returns_1 <sl),1)
      if(length(exit_1) == 0){
        exit_1 <- nrow(df)-idx_signals[i]
      }
      
      df[idx_signals[i]: (exit_1+idx_signals[i]), position := "long"]
      
      # print("Position entered")
    } else if(is.na(df$position[idx_signals[i]])  & df$signal[idx_signals[i]] == "short") {
      returns_1 <- -round((df$close[idx_signals[i]:nrow(df)] - price_signals[i]) / price_signals[i], 3)*100
      returns_1 <-returns_1[-1]
      exit_1 <- head(which(returns_1 > tp | returns_1 <sl),1)
      
      if(length(exit_1) == 0){
        exit_1 <- nrow(df)-idx_signals[i]
      }
      
      df[idx_signals[i]: (exit_1+idx_signals[i]), position := "short"]
    }
    
  }
  }
  df$returns <- c(diff(df$close), 0)
  profs[j] <- df[position =="long", sum(returns)] + df[position =="short", sum(-returns)]
  
  dd <- rle(df[, position])
  lens <- dd$lengths
  reps <- 1:length(lens)
  df$trade <- as.character(rep(reps, lens))
  df[is.na(position), trade := NA]
  
  summaries_long <- df[position == "long", .(outcome = sum(returns)), by = trade][!is.na(trade)][, type:= "long"]
  summaries_short <- df[position == "short", .(outcome = sum(-returns)), by = trade][!is.na(trade)][, type:= "short"]
  summaries <- rbind(summaries_long, summaries_short)
  summaries[outcome > 0, .N]/nrow(summaries)
  
  types <- unique(df[, .(position, trade)])
  types_profs <- merge(summaries, types, by = "trade", all.x = T)
  types_profs[, sum(outcome), by= position]
  
  
  # summaries
  strategy_results[[j]] <- data.table(HODL = tail(df[, close],1) - head(df[, close],1),
                                 PROFS = profs[j],
                                 PROFS_PER = round(profs[j] /head(df[,close], 1)*100, 2),
                                 WINRATIO = summaries[outcome > 0, .N]/nrow(summaries),
                                 N_TRADES = sum(unique(df[, .(position, trade)])[, table(position)]),
                                 N_LONG = unique(df[, .(position, trade)])[, table(position)][1],
                                 N_SHORT = unique(df[, .(position, trade)])[, table(position)][2],
                                 SHORT_PROFS = types_profs[, sum(outcome), by= position][position =="short", V1],
                                 LONG_PROFS = types_profs[, sum(outcome), by= position][position =="long", V1],
                                 SHORT_WINRATIO = types_profs[position == "short"  & outcome > 0, .N]/nrow(types_profs[position == "short"]),
                                 LONG_WINRATIO = types_profs[position == "long"  & outcome > 0, .N]/nrow(types_profs[position == "long"]))
  strategy_results[[j]] <- cbind(strategy_results[[j]], testing_params[j, ])
  
  print(paste0("for rep ", j, " profs: ", profs[j]))
}
results_1hour_ETH <- rbindlist(strategy_results)
View(results_1hour_ETH)
# saveRDS(results_1hour_ETH, "results_1hour_ETH.RDS")
View(results_1hour_ETH[sl == 1 & tp == 1])

View(cbind(testing_params, profs))

j <- which.max(profs)

dd <- rle(df[, position])
lens <- dd$lengths
reps <- 1:length(lens)
df$trade <- as.character(rep(reps, lens))
df[is.na(position), trade := NA]

summaries_long <- df[position == "long", .(outcome = sum(returns)), by = trade][!is.na(trade)][, type:= "long"]
summaries_short <- df[position == "short", .(outcome = sum(-returns)), by = trade][!is.na(trade)][, type:= "short"]
summaries <- rbind(summaries_long, summaries_short)
summaries[outcome > 0, .N]/nrow(summaries)

types <- unique(df[, .(position, trade)])
types_profs <- merge(summaries, types, by = "trade", all.x = T)
types_profs[, sum(outcome), by= position]


# summaries
strategy_results <- data.table(HODL = tail(df[, close],1) - head(df[, close],1),
                               PROFS = profs[j],
                               PROFS_PER = round(profs[j] /head(df[,close], 1)*100, 2),
                               WINRATIO = summaries[outcome > 0, .N]/nrow(summaries),
                               N_TRADES = sum(unique(df[, .(position, trade)])[, table(position)]),
                               N_LONG = unique(df[, .(position, trade)])[, table(position)][1],
                               N_SHORT = unique(df[, .(position, trade)])[, table(position)][2],
                               SHORT_PROFS = types_profs[, sum(outcome), by= position][position =="short", V1],
                               LONG_PROFS = types_profs[, sum(outcome), by= position][position =="long", V1],
                               SHORT_WINRATIO = types_profs[position == "short"  &outcome > 0, .N]/nrow(types_profs[position == "short"]),
                               LONG_WINRATIO = types_profs[position == "long"  &outcome > 0, .N]/nrow(types_profs[position == "long"]))
strategy_results
plot(df[, close], type= "l")

df[!is.na(trade), indexing := 1:.N , by = trade]

df[indexing == 1 & position == "short", action := "enter_short"]
df[indexing == 1 & position == "long", action := "enter_long"]

sales <- df[, .I[which.max(indexing)] , by = .(trade, position)]
df[sales$V1[sales$position == "short"], action :="exit_short"]
df[sales$V1[sales$position == "long"], action :="exit_long"]

df[, x:= 1:nrow(df)]


df_plot<- copy(df)
plot(df_plot[, x], df_plot[, close], type= "l")
points(df_plot[action =="enter_long", x], df_plot[action =="enter_long", close], col = "green", pch = 19)
points(df_plot[action =="exit_long", x], df_plot[action =="exit_long", close], col = "red", pch = 19)

points(df_plot[action =="enter_short", x], df_plot[action =="enter_short", close], col = "blue", pch = 19)
points(df_plot[action =="exit_short", x], df_plot[action =="exit_short", close], col = "black", pch = 19)
View(df)
