rm(list=ls())
options(scipen = 999)
# Source functions
path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
# API info
api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading", "API_Keys.txt", sep = "/"), sep = ";", header = T)
API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)

# start <- "0"
# url = "https://api.kraken.com/0/private/TradesHistory"
# secret = API_Sign
# key = API_Key
# 
# 
# Get_hist_trades <- function (url, key, secret, start) {
#   
#   mylist <- list()
#   count <- myfun(url = "https://api.kraken.com/0/private/TradesHistory",
#                   secret = API_Sign, key = API_Key)
#   count1 <- count$result$count
#   count2 <- roundup(x = count1, 100)
#   trade_seq <- seq(0, count1, 50)
#   trade_seq[length(trade_seq)] <- count2 -(count2 - count1)
#   
#   Sys.sleep(10)
#   print("Got trade count...")
#   
#   for(i in 1:length(trade_seq)){
#     
#     # Nonce and post info
#     nonce <- as.character(as.numeric(Sys.time()) * 1000000)
#     post_data <- paste0("nonce=", nonce, "&start=", start, "&ofs=",1180)
#     
#     # Strip kraken url
#     method_path <- gsub("^.*?kraken.com", "", url)
#     
#     # Secret APi key 
#     sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
#                  object = c(charToRaw(method_path), digest(object = paste0(nonce, post_data),
#                                                            algo = "sha256",
#                                                            serialize = FALSE, 
#                                                            raw = TRUE)),
#                  algo = "sha512", raw = TRUE)
#     # Header
#     httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
#     
#     curl <- RCurl::getCurlHandle(useragent = paste("Rbitcoin", packageVersion("Rbitcoin")))
#     query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
#                                                         url = url,
#                                                         binary = TRUE,
#                                                         postfields = post_data, 
#                                                         httpheader = httpheader))
#     query_result <- jsonlite::fromJSON(query_result_json)
#     
#     mylist[[i]] <- names(query_result$result$trades)
#     Sys.sleep(5)
#     print(i)
#   }
#   
#   return(mylist)
# }



# trades <- trades$result
# trades <- trades$trades
# trades[["TT5IIY-D6HWI-QREOMK"]]
# names(trades)
trades_hist <- read.csv("/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/trades_hist.csv")
trades <- read.csv("/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.1/Testing Module/price_action/ids.csv", header = F)
mytrades <- trades_hist[trades_hist$ordertxid %in% trades$V1, ]
mytrades <- trades_hist 
mytrades$cleancost <- mytrades$cost-mytrades$fee
mytrades <- as.data.table(mytrades)
setorder(mytrades, pair, time)

mytrades$id <-  rep(sample(50:10000, nrow(mytrades)/2, replace = F), each = 2)
ids <- unique(mytrades$id)
profs_perc <- c()
profs_act <- c()
for(i in 1:length(ids)){
  profs_perc[i] <- (mytrades$cleancost[mytrades$id == ids[i] & mytrades$type =="sell"] - mytrades$cleancost[mytrades$id == ids[i] & mytrades$type =="buy"])/mytrades$cleancost[mytrades$id == ids[i] & mytrades$type =="buy"]
  profs_act[i] <- (mytrades$cleancost[mytrades$id == ids[i] & mytrades$type =="sell"] - mytrades$cleancost[mytrades$id == ids[i] & mytrades$type =="buy"])
print(i)
}

stats <- data.table(perc = profs_perc,act = profs_act ,pair=unique(mytrades[, c("id", "pair")])$pair,  id = ids)

View(stats)
stats[ ,sum(perc>0)/.N, by = pair]
stats[ ,sum(act), by = pair]
sum(stats$perc > 0)/nrow(stats)


key <- c("type", "pair")
res1 <- mytrades[, sum(cleancost), by = key]

key <- c("type")
res2 <- mytrades[, sum(cleancost), by = key]



View(trades_hist)
ids <- names(trades)
res <- data.frame(id = rep(NA, length(ids)), pair = rep(NA, length(ids)), type = rep(NA, length(ids)),
                  ordertype = rep(NA, length(ids)), vol = rep(NA, length(ids)), cost = rep(NA, length(ids)),
                  fee = rep(NA, length(ids)), price = rep(NA, length(ids)))
for(i in 1:length(ids)){
  id <- ids[i]  
  res$id[i] <- id
  res$pair[i] <- trades[[id]]$descr$pair
  res$type[i] <- trades[[id]]$descr$type
  res$ordertype[i] <- trades[[id]]$descr$ordertype
  res$vol[i] <- as.numeric(trades[[id]]$vol)
  res$cost[i] <- as.numeric(trades[[id]]$cost)
  res$fee[i] <- as.numeric(trades[[id]]$fee)
  res$price[i] <- as.numeric(trades[[id]]$price)
  
}

res$cleancost <- res$cost-res$fee
View(res)

res <- as.data.table(res)
key <- c("type", "pair")
res1 <- res[, sum(cleancost), by = key]
View(res1)
