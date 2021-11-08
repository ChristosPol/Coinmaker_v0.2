# Description ------------------------------------------------------------------
# Scheduler for trading at the end of each candle and not intra-candle
library(cronR)

# Path of live trading Rscript
path <- "/Users/christos.polysopoulos/Repositories/Coinmaker_v0.2/Trading/Trading_Luc_CP.R"

# Command
cmd <- cron_rscript(path)

# add frequency and intervals
cron_add(cmd, frequency = 'hourly', id = 'Live trading', description = 'Live trading')

# Check all jobs
cron_ls()

# Stop Job
# cron_clear(ask = FALSE)
cron_rm(id = "Live trading")
