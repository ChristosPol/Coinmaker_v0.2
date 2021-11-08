# To implement:
# Run all params for train data and then all for test
# Select the ones that are consistent and pick best

# Ideas to implement
# Wait for x bars to re enter to avoid going over or below the top, make sure you
# have a breakout

# Fix final position column, there is an error, doesnt immmediately position re enter

# Avoid getting parameter results within the loop, just export the final column position
# and then apply summary of results, should be faster

# Before setting up parameters, calculate risk to reward ratio



# define parameters
n_sort <- data.frame(n_sort = seq(1), flag = 1)
look_back <- data.frame(look_back = c(50), flag = 1)
# rsi <- data.frame(rsi = seq(5, 20, 2), flag = 1)
# rsi_top <- data.frame(rsi_top = seq(70, 95, 5), flag = 1)
# rsi_bot <- data.frame(rsi_bot = seq(5, 30, 5), flag = 1)
# ema <- data.frame(ema = seq(10, 200, 20), flag = 1)
tp <- data.frame(tp = c(0.005, 0.01, 0.02)*100, flag = 1)
# tp <- data.frame(tp = c( 0.006)*100, flag = 1)
sl <- data.frame(sl = c(0.01)*100, flag = 1)
# sl <- data.frame(sl = c(0.006)*100, flag = 1)
per <- data.frame(per = c(0), flag = 1)
n_exclude <- data.frame(n_exclude = c(1), flag = 1)

# Create set pof parameters
testing_params <- as.data.table(left_join(n_sort, look_back) %>% left_join(sl)
                                %>% left_join(tp)%>% left_join(per))%>%left_join(n_exclude)
testing_params<- testing_params[look_back > n_sort,]
# testing_params <- as.data.table(left_join(ema, tp) %>% left_join(sl))

# testing_params <- testing_params[tp > sl,]

# testing_params <- as.data.table(left_join(rsi, rsi_top) %>% left_join(rsi_bot)%>% left_join(sl)%>% left_join(tp))
testing_params <- as.data.table(testing_params)
j <- 1
k <- 1

# Initialise
strategy_results <- list()
set_summary <- list()
profs <- c()
sets <- unique(mydata[, set])

for (k in 1:length(sets)){
  
  
  
  for(j in 1:nrow(testing_params)){
    
    df <- copy(mydata[set == sets[k]])
    
    
    
    # df <- copy(klines[[1]])
    
    tp <- testing_params$tp[j]
    sl <- -testing_params$sl[j]
    
    # EMA_strategy()
    # SR_lines_strategy()
    # RSI_strategy()
    # SR_lines_strategy_breakout()
    SR_lines_strategy_breakout_luc()
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
      
      # for (i in 1:index){
      i <-1
      while (i <= length(idx_signals)){
        
        # print(idx_signals)
        # print(idx_signals[i])
        if(is.na(df$position[idx_signals[i]])  & df$signal[idx_signals[i]] == "long"){
          returns_1 <- round((df$close[idx_signals[i]:nrow(df)] - price_signals[i]) / price_signals[i], 3)*100
          
          returns_1 <-returns_1[-1]
          exit_1 <- head(which(returns_1 > tp | returns_1 <sl),1)
          if(length(exit_1) == 0){
            exit_1 <- nrow(df)-idx_signals[i]
          }
          
          df[idx_signals[i]: (exit_1+idx_signals[i]), position := "long"]
          df[idx_signals[i], status := "entered_trade"]
          df[(exit_1+idx_signals[i]), status := "exited_trade"]
          
          if(is.na(df[exit_1+idx_signals[i] + 1, position]) & !is.na(df[exit_1+idx_signals[i] + 1, signal]) ){
            
            if(df[exit_1+idx_signals[i] + 1, signal] %in% c("long", "short")){
              idx_signals <- sort(append(idx_signals, values= exit_1+idx_signals[i] + 1))
              price_signals <- df[idx_signals, close]  
            }
            
          }
          
          
          # print("Position entered")
        } else if(is.na(df$position[idx_signals[i]])  & df$signal[idx_signals[i]] == "short") {
          returns_1 <- -round((df$close[idx_signals[i]:nrow(df)] - price_signals[i]) / price_signals[i], 3)*100
          returns_1 <-returns_1[-1]
          exit_1 <- head(which(returns_1 > tp | returns_1 <sl),1)
          
          if(length(exit_1) == 0){
            exit_1 <- nrow(df)-idx_signals[i]
          }
          
          df[idx_signals[i]: (exit_1+idx_signals[i]), position := "short"]
          df[idx_signals[i], status := "entered_trade"]
          df[(exit_1+idx_signals[i]), status := "exited_trade"]
          if(is.na(df[exit_1+idx_signals[i] + 1, position]) & !is.na(df[exit_1+idx_signals[i] + 1, signal]) ){
            
            if(df[exit_1+idx_signals[i] + 1, signal] %in% c("long", "short")){
              idx_signals <- sort(append(idx_signals, values= exit_1+idx_signals[i] + 1))
              price_signals <- df[idx_signals, close]  
            }
            
          }
        }
       i <- i+1 
      }
    }
    
    
    
    
    df$returns <- c(diff(df$close), 0)
    if(is.null(df$status) == FALSE){
    
      ids <- round(runif(n = length(which(!is.na(df$status)))/2, min = 10000, max = 10000000))
      ids_imp <- data.table(idx = which(!is.na(df$status)), id = rep(ids, each =2))
      df$trade_id <- NA
      df$trade_id[ids_imp$idx] <- ids_imp$id
      
    idss<- as.vector(na.omit(unique(df$trade_id)))
    
    for(g in 1:length(idss)){
      
      df$trade_id[which(df$trade_id %in% idss[g])[1]:which(df$trade_id %in% idss[g])[2]] <- idss[g]
      
    }
    
    df[status %in% c("entered_trade", "exited_trade"), fees := close*0.0026]
    
    fees <- df[, sum(fees, na.rm = T), by = trade_id][!is.na(trade_id)]
    
    profs[j] <- df[position =="long", sum(returns)] + df[position =="short", sum(-returns)] - sum(fees$V1)
    
    # dd <- rle(df[, position])
    # lens <- dd$lengths
    # reps <- 1:length(lens)
    # df$trade <- as.character(rep(reps, lens))
    # df[is.na(position), trade := NA]
    
    summaries_long <- df[position == "long", .(outcome = sum(returns)), by = trade_id][!is.na(trade_id)][, type:= "long"]
    summaries_long <- merge(summaries_long, fees, by = "trade_id", all.x = T)
    summaries_long[, outcome:= outcome -V1]
    summaries_short <- df[position == "short", .(outcome = sum(-returns)), by = trade_id][!is.na(trade_id)][, type:= "short"]
    summaries_short <- merge(summaries_short, fees, by = "trade_id", all.x = T)
    summaries_short[, outcome:= outcome -V1]
    summaries <- rbind(summaries_long, summaries_short)
    summaries[outcome > 0, .N]/nrow(summaries)
    
    types <- unique(df[, .(position, trade_id)])
    types_profs <- merge(summaries, types, by = "trade_id", all.x = T)
    types_profs[, sum(outcome), by= position]
    
    
    # summaries
    strategy_results[[j]] <- data.table(HODL = tail(df[, close],1) - head(df[, close],1),
                                        PROFS = profs[j],
                                        PROFS_PER = round(profs[j] /head(df[,close], 1)*100, 2),
                                        WINRATIO = summaries[outcome > 0, .N]/nrow(summaries),
                                        N_TRADES = sum(unique(df[, .(position, trade_id)])[, table(position)]),
                                        N_LONG = unique(df[, .(position, trade_id)])[, table(position)][1],
                                        N_SHORT = unique(df[, .(position, trade_id)])[, table(position)][2],
                                        SHORT_PROFS = types_profs[, sum(outcome), by= position][position =="short", V1],
                                        LONG_PROFS = types_profs[, sum(outcome), by= position][position =="long", V1],
                                        SHORT_WINRATIO = types_profs[position == "short"  & outcome > 0, .N]/nrow(types_profs[position == "short"]),
                                        LONG_WINRATIO = types_profs[position == "long"  & outcome > 0, .N]/nrow(types_profs[position == "long"]))
    strategy_results[[j]] <- cbind(strategy_results[[j]], testing_params[j, ])
    
    } else {
      profs[j]<- 0
      strategy_results[[j]] <- data.table(HODL = tail(df[, close],1) - head(df[, close],1),
                                          PROFS = profs[j],
                                          PROFS_PER = round(profs[j] /head(df[,close], 1)*100, 2),
                                          WINRATIO = 0,
                                          N_TRADES = 0,
                                          N_LONG = 0,
                                          N_SHORT = 0,
                                          SHORT_PROFS = 0,
                                          LONG_PROFS = 0,
                                          SHORT_WINRATIO = 0,
                                          LONG_WINRATIO = 0)
      strategy_results[[j]] <- cbind(strategy_results[[j]], testing_params[j, ])
    }
    print(paste0("for rep ", j, " profs: ", profs[j], " for set: ", k))
  }
  set_summary[[k]] <- strategy_results
}

set_summary <- lapply(set_summary, rbindlist)
names(set_summary) <- sets
for(i in 1:length(set_summary)){
  set_summary[[i]][, set:= names(set_summary)[i]]
}
set_summary_binded <- rbindlist(set_summary)


# set_summary_binded[, params_string := paste(ema,tp, sl,  sep = "_")]
set_summary_binded[, params_string := paste(n_sort,look_back, sl, tp,per,n_exclude, sep = "_")]

# data_path <- "/media/chris/DATA/Documents/Bot_Trading/Historical_data"
# saveRDS(set_summary_binded, "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.2/Testing Module/klines1min_eth_2021_01_2021_06_SRLINES.RDS")


results <- set_summary_binded[, .(mean_profs_per = mean(PROFS_PER),
                                  sd_profs_per = sd(PROFS_PER),
                                  mean_profs = mean(PROFS),
                                  sd_profs = sd(PROFS),
                                  mean_wr = mean(WINRATIO),
                                  sd_wr = sd(WINRATIO),
                                  mean_n_trades = mean(N_TRADES),
                                  sum_hodl = sum(HODL),
                                  sum_prof = sum(PROFS),
                                  beat_hold = table(PROFS>HODL)[2]/ sum(table(PROFS>HODL))), by = params_string]
View(results)

ggplot(results, aes(x = params_string, y = mean_profs))+
  geom_pointrange(aes(ymin = mean_profs - sd_profs, ymax =  mean_profs + sd_profs))

better_results <- results[results$mean_profs_per - results$sd_profs_per > -3.5,]
ggplot(better_results, aes(x = params_string, y = mean_profs_per))+
  geom_pointrange(aes(ymin = mean_profs_per - sd_profs_per, ymax =  mean_profs_per + sd_profs_per))


better_results <- copy(results)
setorder(better_results, -mean_profs_per)
better_results1 <- copy(better_results[1:20, ])
ggplot(better_results1, aes(x = params_string, y = mean_profs_per))+
  geom_pointrange(aes(ymin = mean_profs_per - sd_profs_per, ymax =  mean_profs_per + sd_profs_per))


better_results <- copy(results)
setorder(better_results, sd_profs_per )
better_results1 <- copy(better_results[1:20, ])
ggplot(better_results1, aes(x = params_string, y = mean_profs_per))+
  geom_pointrange(aes(ymin = mean_profs_per - sd_profs_per, ymax =  mean_profs_per + sd_profs_per))



results <- klines1min_eth_2021_01_2021_06_SRLINES
results <- results[, .(mean_profs_per = mean(PROFS_PER),
                       sd_profs_per = sd(PROFS_PER),
                       mean_profs = mean(PROFS),
                       sd_profs = sd(PROFS),
                       mean_wr = mean(WINRATIO),
                       sd_wr = sd(WINRATIO),
                       beat_hold = table(PROFS>HODL)[2]/ sum(table(PROFS>HODL))), by = params_string]



View(results)
results$mean_profs
results$sd_profs
# saveRDS(results_1hour_ETH, "results_1hour_ETH.RDS")

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
sum(types_profs$outcome)

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

par(mfrow = c(1,1))
df_plot<- copy(df)
plot(df_plot[, x], df_plot[, close], type= "l")
lines(EMA(df$close, n = testing_params$ema[j]), col = "red")

points(df_plot[status == "entered_trade" & position =="long", x], df_plot[status == "entered_trade" & position =="long", close], col = "green", pch = 19)
points(df_plot[status == "exited_trade" & position =="long", x], df_plot[status == "exited_trade" & position =="long", close], col = "red", pch = 19)

points(df_plot[status == "entered_trade" & position =="short", x], df_plot[status == "entered_trade" & position =="short", close], col = "blue", pch = 19)
points(df_plot[status == "exited_trade" & position =="short", x], df_plot[status == "exited_trade" & position =="short", close], col = "black", pch = 19)
par(new=TRUE)

plot(df_plot$volume, type = "h",xaxt = "n", yaxt = "n", ylim = c(0, 100000))
axis(4)

# plot(df_plot$volume, type = "h", ylim = c(0, 2000))



plot(1:2)

mtext("right y axis", side=4, line=-1.5)

par(mar=c(5,4,4,5)+.1)
plot(1:2)
axis(4)
mtext("right y axis", side=4, line=3)     