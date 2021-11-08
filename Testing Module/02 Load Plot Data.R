# Loading Data for operations --------------------------------------------------

# Create candlesticks for different intervals
ticks <- c(5, 60, 2, 4, 6, 12, 24)
units <- c(rep("minutes", 2), rep("hours", 5))

# Or choose a single one
ticks <- c(60)
units <- c(rep("minutes", 1))
intervals <- paste(ticks, units, sep = " ")

# Load trades and conver to OHLC, applies filtering
klines <- trades_to_OHLC(pair = pair,
                         interval = intervals,
                         from_date = "2021-10-01",
                         to_date = "2021-10-30",
                         date_subset = F)
names(klines) <- gsub(" ", "_", intervals)


# Get a first visual
df <- klines[[1]]
# fig <- df %>% plot_ly(x = ~full_date_time , type="candlestick",
#                       open = ~open, close = ~close,
#                       high = ~high, low = ~low) 
# fig <- fig %>% layout(title = pair,
#                       xaxis = list(rangeslider = list(visible = F)))
# fig

p <- ggplot(df, aes(x = full_date_time, y = close)) +
  geom_line()+
  ggtitle(pair)+
  theme_linedraw()
p
# n_data <- 10
# mydata <- copy(klines[[1]])
# split_by <- nrow(mydata)/n_data
# 
# idx <- round(seq(1, nrow(mydata), by = split_by))
# 
# for(i in 2:length(idx)){
#   mydata[idx[i-1]:idx[i], set := as.character(idx[i]) ]
# }
# mydata[is.na(set), set := as.character(round(idx[length(idx)] + split_by))]
# mydata[, table(set)]