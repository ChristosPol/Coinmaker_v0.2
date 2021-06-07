# Preamble ---------------------------------------------------------------------
# screen -S pullingETH R
.rs.restartR()
rm(list = ls())

# Source functions
path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Choose which unix time to use for pulling data
# Choose from ["start_of_time", "manually", "latest_available"]
unix_time <- "manually"

# Choose any pair to pull
pair <- "XETHZEUR"
# pair <- "DOTEUR"
# pair <- "ETHEUR"
# pair <- "ALGOEUR"
# pair <- "KAVAEUR"
# pair <- "GNOEUR"
pair <- "ADAEUR"
# pair <- "STORJEUR"
# Path to save results
data_path <- "/media/chris/DATA/Documents/Bot_Trading/Historical_data"

# Create pair directory
dir.create(paste(data_path, pair, sep ="/"), showWarnings = FALSE)

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

# Select initial id based on unix_time arg
if (unix_time == "start_of_time") {
  initial_id <- 0
} else if (unix_time == "manually") {
  # select number of days starting from todays date
  options("width" = 60)
  v <- nanotime(Sys.time() - as.difftime(1000, unit = "days"))
  initial_id <- as.integer64(v)
} else {
  file <- paste0(pair_data_results, "/", pair, ".csv.gz")
  nL <- countLines(file)
  dt <- fread(file, skip = nL-1)
  initial_id <- dt$V6  
}
 
# Pull historical trades since initial id from epoch time
hist_trades_pair(sleep = 3, hist_id = initial_id, pair = pair)

