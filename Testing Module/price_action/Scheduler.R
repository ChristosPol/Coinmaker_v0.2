# Description ------------------------------------------------------------------
library(cronR)

# Path of live trading Rscript
# path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Trading Module/Live_trading.R"
path <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/"

# Command
cmd <- cron_rscript(paste0(path, "GetOHLC.R"))

# add frequency and intervals
cron_add(cmd, frequency = '*/6 * * * *', id = 'GETOHLC', description = 'GETOHLC', at = '10:28')

# Check all jobs
cron_ls()

# Stop Job
# cron_clear(ask = FALSE)
cron_rm(id = "GETOHLC")


