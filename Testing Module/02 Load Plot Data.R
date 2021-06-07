# Loading Data for operations --------------------------------------------------

# Create candlesticks for different intervals
ticks <- c(5, 60, 2, 4, 6, 12, 24)
units <- c(rep("minutes", 2), rep("hours", 5))

# Or choose a single one
ticks <- c(5)
units <- c(rep("minutes", 1))
intervals <- paste(ticks, units, sep = " ")

# Load trades and conver to OHLC, applies filtering
klines <- trades_to_OHLC(pair = pair,
               interval = intervals,
               from_date = "2021-04-01",
               to_date = "2021-06-07",
               date_subset = T)
names(klines) <- gsub(" ", "_", intervals)

# Get a first visual
df <- klines[[1]]
fig <- df %>% plot_ly(x = ~full_date_time , type="candlestick",
                      open = ~open, close = ~close,
                      high = ~high, low = ~low) 
fig <- fig %>% layout(title = pair,
                      xaxis = list(rangeslider = list(visible = F)))
fig
