df <- copy(klines[[1]])
df <- df[1:1000,]

par(mfrow=c(1,1))
plot(df$close, type="l")
abline(h=mean(head(sort(df[, low]), 10)))
abline(h=mean(head(sort(df[, high], decreasing = T), 10)))

mean(head(sort(df[, low]), 10))

SR <- function(lows, highs, n_sort){
  SP <- mean(head(sort(lows), n_sort))
  RS <- mean(head(sort(highs, decreasing = T), n_sort))
  return(list("SP" = SP, "RS" = RS))
}

SR <- function(x, y){
  SP <- mean(head(sort(x), 50))
  RS <- mean(head(sort(y, decreasing = T), 50))
}

support <- function(x, n_sort){
  SP <- mean(head(sort(x), n_sort))
  return(SP)
}

resistance <- function(x, n_sort){
  RS <- mean(head(sort(x, decreasing = T), n_sort))
  return(RS)
}


SP <- frollapply(df[,.(low)], 100, function(x) support(x, n_sort = 5))
RS <- frollapply(df[,.(high)], 100, function(x) resistance(x, n_sort = 5))
df1 <- cbind(df, SP = unlist(SP), RS = unlist(RS))
df1[, rr:= 1:nrow(df1)]

df1[, long:= ifelse(close<SP, "enter", NA)]

plot(df1[, rr], df1[, close], type = "l")
abline(h = df1[1000, SP])
abline(h = df1[1000, RS])
points(df1[long == "enter", rr], df1[long == "enter", close], pch = 19, col = "green")



for(i in 100:nrow(df1)){
  
  plot(df1[1:i, rr], df1[1:i, close], type = "l")
  abline(h = df1[i, SP])
  abline(h = df1[i, RS])
Sys.sleep(0.1)  
  
}
