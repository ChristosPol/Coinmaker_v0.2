train <- df[1:720, ]
test <- df[721:nrow(df), ]
splits <- data.table(idx = c(1:100, 101:200, 201:300, 301:400, 401:500, 501:600, 601:700),
                     seq = rep(1:7, each = 100),
                     tops = rep(seq(100, 700,by = 100), each = 100))

SP <- c()
RS <- c()
j <-1
for(j in 1:length(unique(splits[, seq]))){
  subdf <- train[splits[seq == j, idx], ]
  SP[j] <- median(head(sort(subdf[, close]), 5))
  RS[j] <- median(tail(sort(subdf[, close]), 5))
}

df1 <- rbind(train, test)

last_close <- tail(df1[, close], 1)
SP <- SP[SP < last_close]
RS <- RS[RS > last_close]

# df1$x <- 1:nrow(df1)
# df1[, volume_quote := close*volume]
# 
# p1 <- ggplot(data = df1, aes(x = x , y = close))+
#   geom_line(alpha = 0.8)+
#   geom_hline(yintercept= RS, linetype="dashed", color = "red")+
#   geom_hline(yintercept= SP, linetype="dashed", color = "green")+
#   geom_hline(yintercept= tail(df$close, 1), linetype="dotted", color = "black")+
#   scale_y_continuous(breaks=c( tail(df$close, 1) ,SP, RS))+
#   geom_vline(xintercept = splits[, tops] , linetype="dashed", 
#              color = "blue")
# p1
# 
# 
# i <-721
# df$x <- 1:nrow(df)
# 
# for(i in 721:nrow(df)){
# p1 <- ggplot(data = df[1:i, ], aes(x = x , y = close))+
#   geom_line(alpha = 0.8)
# # +
# #   geom_hline(yintercept= RS, linetype="dashed", color = "red")+
# #   geom_hline(yintercept= SP, linetype="dashed", color = "green")+
# #   geom_hline(yintercept= tail(df$close, 1), linetype="dotted", color = "black")+
# #   scale_y_continuous(breaks=c( tail(df$close, 1) ,SP, RS))+
# #   geom_vline(xintercept = splits[, tops] , linetype="dashed", 
# #              color = "blue")
# ggsave(file = paste0("charts/",i,".jpg"), plot = p1, width = 8, height = 4.5, units = "in", dpi=300)
# print(paste0("processing: ",i))
# }
# 
# p1 <- ggplot(data = df, aes(x = x , y = close, frame =x, cumulative = T))+
#   geom_line(alpha = 0.8)
# gganimate(p1, interval = 0.1)
# 


# system("convert -delay 10 combined/*.jpg warming2.gif")
# 
# 
# for (y in 721:1126) {
#   system(paste0("convert charts/",y,".jpg maps/map",y,".jpg -geometry +305+72 -composite -pointsize 100 -font Georgia -annotate +2000+1120 ",y," combined/img",y,".jpg"))
#   print(paste0("processing: ",y))
# }
# 
# gganimate(p1, interval = 0.1)
# remotes::install_github("thomasp85/gganimate@v0.1.1")
# 
# 
# 

df$x <- 1:nrow(df)
df$signal <- NULL
i <- 721
for (i in 721:nrow(df)){
  df[i, signal:= ifelse(any(close<SP), "yes", "no") ]
}
df[1:720, signal := "no"]
pointss <- df[signal == "yes"]


ggplot(data = df, aes(x = x , y = close))+
  geom_line(alpha = 0.8) +
  geom_hline(yintercept= RS, linetype="dashed", color = "red")+
  geom_hline(yintercept= SP, linetype="dashed", color = "green")+
  geom_hline(yintercept= tail(df$close, 1), linetype="dotted", color = "black")+
  scale_y_continuous(breaks=c( tail(df$close, 1) ,SP, RS))+
  geom_vline(xintercept = splits[, tops] , linetype="dashed",
             color = "blue")+
  geom_point(data = pointss, aes(x = x , y = close), color = "red")

p1 <- ggplot(data = df, aes(x = x , y = close))+
  geom_line(alpha = 0.8) +
  geom_hline(yintercept= RS, linetype="dashed", color = "red")+
  geom_hline(yintercept= SP, linetype="dashed", color = "green")+
  geom_hline(yintercept= tail(df$close, 1), linetype="dotted", color = "black")+
  scale_y_continuous(breaks=c( tail(df$close, 1) ,SP, RS))+
  geom_vline(xintercept = splits[, tops] , linetype="dashed",
             color = "blue")+
  geom_point(data = pointss, aes(x = x , y = close), color = "red")+
 transition_reveal(x)#+shadow_mark()

animate(p1, nframes = 500)
animate(p1, fps = 5)
fps = 20

