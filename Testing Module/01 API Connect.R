# Preamble ---------------------------------------------------------------------
# screen -S pullingETH R
.rs.restartR()
rm(list = ls())

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Choose which unix time to use for pulling data
# Choose from ["start_of_time", "manually", "latest_available"]
unix_time <- "manually"

# Choose any pair to pull
# pair <- "XETHZEUR"
pair <- "ADAEUR"
# pair <- "ETHEUR"
# pair <- "ALGOEUR"
# pair <- "KAVAEUR"
# pair <- "GNOEUR"
# pair <- "ADAEUR"
# pair <- "STORJEUR"
# Path to save results
data_path <- "Data"

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
  v <- nanotime(Sys.time() - as.difftime(30, unit = "days"))
  initial_id <- as.integer64(v)
} else {
  file <- paste0(pair_data_results, "/", pair, ".csv.gz")
  nL <- countLines(file)
  dt <- fread(file, skip = nL-1)
  initial_id <- dt$V6  
}
 
# Pull historical trades since initial id from epoch time
hist_trades_pair(sleep = 3, hist_id = initial_id, pair = pair)

