rm(list = ls())

path_source <- "/media/chris/DATA/Documents/Bot_Trading/Coinmaker_v0.2/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)
path_alerts <- "/media/chris/DATA/Documents/Bot_Trading/Alerts"


api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading", "API_Keys.txt", sep = "/"), sep = ";", header = T)
API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)

avail_pairs <- myfun("https://api.kraken.com/0/public/AssetPairs", secret = API_Sign, key = API_Key)
all_pairs <- names(avail_pairs[[2]])

# Get only EUR related crypto pairs
EUR_pairs <- grep("EUR|ETH", all_pairs, value = T)
EUR_pairs <- grep("EUR", all_pairs, value = T)
# Remove Forex pairs
to_remove <- grep(paste(c("USD",
                          ".d",
                          "AUD",
                          "CAD",
                          "JPY",
                          "CHF",
                          "GBP",
                          "PAX",
                          "DAI",
                          "BAT"), collapse ="|"), EUR_pairs, value = T)
EUR_pairs <- EUR_pairs[!EUR_pairs %in% to_remove]

# Dynamic support and resistance
# Get OHLC data and determine trends

look_back1 <- 100
look_back2 <- 200
n_sort <- 1
n_exclude <- 10
interval <- 60
# value_price <- list()

pdf(paste0(path_alerts, "/plots.pdf"), onefile = TRUE)

for (i in 1:length(EUR_pairs)){
  msg <- tryCatch({
  df <- simple_OHLC(interval = interval, pair = EUR_pairs[i])
  df[, candle_type := ifelse(close > open, "green", "red")]
  SP1 <- support(tail(df[, close], look_back1),
                n_sort = n_sort,
                n_exclude = n_exclude)
  RS1 <- resistance(tail(df[, close], look_back1),
                   n_sort = n_sort,
                   n_exclude = n_exclude)
  
  SP2 <- support(tail(df[, close], look_back2),
                 n_sort = n_sort,
                 n_exclude = n_exclude)
  RS2 <- resistance(tail(df[, close], look_back2),
                    n_sort = n_sort,
                    n_exclude = n_exclude)
  
  df$x <- 1:nrow(df)
  df[, volume_quote := close*volume]

  p1 <- ggplot(data = df, aes(x =x , y = close))+
    geom_line(alpha = 0.8)+
    geom_hline(yintercept= SP1, linetype="dashed", color = "red")+
    geom_hline(yintercept= RS1, linetype="dashed", color = "green")+
    geom_hline(yintercept= SP2, linetype="dashed", color = "red")+
    geom_hline(yintercept= RS2, linetype="dashed", color = "green")+
    
    geom_vline(xintercept = nrow(df) -look_back1 , linetype="dashed", 
               color = "blue")+
  geom_vline(xintercept = nrow(df) -look_back2 , linetype="dashed", 
             color = "blue")
  
  p2 <- qplot(x=x,xend=x,y=0,yend=volume_quote,data=df,geom="segment", color = candle_type)+
              scale_color_manual(values=c("green", "red"))+
              theme(legend.position = "none", axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  
  
  print(grid.arrange(p1, p2, 
               ncol = 1, nrow = 2,
               top = textGrob(paste0("pair: ", EUR_pairs[i], " Export: ", Sys.time()) ,gp=gpar(fontsize=20,font=3))))
  
  
  }, error = function(e){
  })
  print(i/length(EUR_pairs))
  # print(paste0("Sharpe Ratio for: ",  EUR_pairs[i]," ", round(unique(df$sharpe), 4))) 
  Sys.sleep(5)
  
}
dev.off()



library(emayili)
library(magrittr)

email <- envelope()

email <- email %>%
  from("schlieren.bewohner@gmail.com") %>%
  to("schlieren.bewohner@gmail.com")

email <- email %>% subject("Hourly report")
# email <- email %>% text("Hello!")
# path_alerts <- "/media/chris/DATA/Documents/Bot_Trading/Alerts"

email <- email %>% attachment(list.files(path_alerts, full.names = T))


smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "schlieren.bewohner@gmail.com",
               password = "Polisopoulos89!")
smtp(email, verbose = TRUE)

print(email, details = TRUE)

