# Preamble ---------------------------------------------------------------------
# screen -S pullingETH R
.rs.restartR()
rm(list = ls())
### retire this git, go to binance
## Take best funtionalities from here move to binance
# Source functions
path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Choose any pair to pull
pair <- c("ETH/EUR")
ticks <- c(240)

str_right <- function(string, n) {
  substr(string, nchar(string) - (n - 1), nchar(string))
}
result <- list()

for(i in 1:length(pair)){

# Loading Data for operations --------------------------------------------------

OHLC <- simple_OHLC(ticks, pair[i])
df <- copy(OHLC)

# Determine volatility
# next_c <- c(0, df$close[-nrow(df)])
# df[, next_close := next_c]
df[, o_l:= round(((low - open)/open)*100, 3)]
df[, o_c:= round(((close - open)/open)*100, 3)]
df[, o_h:= round(((high - open)/open)*100, 3)]

df[, jump_enter := round( ((low-next_close)/next_close)*100, 4)]
df <- df[-1, ]

df$exit_close <- round(((df$high - df$low)/ df$close) * 100,4)

df_trades <- df[jump_enter <=-2, ]
df_trades$jump_enter_cat <- NA
df_trades$jump_enter_cat[df_trades$jump_enter > -3] <- "2-3"
df_trades$jump_enter_cat[df_trades$jump_enter > -4 & df_trades$jump_enter <= -3] <- "3-4"
df_trades$jump_enter_cat[df_trades$jump_enter > -5 & df_trades$jump_enter <= -4] <- "4-5"
df_trades$jump_enter_cat[df_trades$jump_enter > -6 & df_trades$jump_enter <= -5] <- "5-6"
df_trades$jump_enter_cat[df_trades$jump_enter > -7 & df_trades$jump_enter <= -6] <- "6-7"
df_trades$jump_enter_cat[df_trades$jump_enter <= -7] <- "7plus"
df_trades <- copy(df)
View(df_trades)

box_perc <- boxplot(df_trades$jump_enter)
bounds <- c(box_perc$stats[1], box_perc$stats[1] + 1)
df_trades$jump_enter_cat <- NA
df_trades$jump_enter_cat[df_trades$jump_enter <= bounds[2] & df_trades$jump_enter >= bounds[1]] <- "selected_enter"
df_trades$jump_enter_cat[is.na(df_trades$jump_enter_cat)] <- "selected_non_enter"

res <- df_trades[, list(median(exit_close), .N), by = jump_enter_cat]
# res[, prof := (V1*N) - 0.026*N]

# res <- res[jump_enter_cat != "7plus", ]
# setorder(res, -prof)

# res[, signal := str_right(jump_enter_cat, 1)]
# res[, tp := V1]
res[, pair := pair[i]]
res[, signal := box_perc$stats[1]]
# res[, box := box_perc$stats[1]]
res[, sl := (signal - box_perc$stats[1])*-1]
setnames(res, "V1", "tp")

result[[i]] <- res[jump_enter_cat =="selected_enter", ] 
Sys.sleep(10)
print(pair[i])
}

params <- do.call(rbind, result)
write.csv(params, "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/params.csv")

# decrease <- df[jump_enter < 0, ]
# mean_decrease <- mean(decrease[, jump_enter])
# median_decrease <- median(decrease[, jump_enter])
# a <- hist(decrease[jump_enter>-11, jump_enter], breaks = 30)
# boxplot(decrease[, jump_enter])
