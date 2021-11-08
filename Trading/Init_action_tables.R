pair <- c("ADAEUR", "XETHZEUR", "BTCEUR", "XRPEUR", "LINKEUR",
          "MATICEUR", "ALGOEUR", "AAVEEUR")
i <- 8
for (i in 1:length(pair)){
  trades <- data.table(current_price = as.numeric(NA), price_enter = as.numeric(NA),
                       price_exit = as.numeric(NA), volume_enter = NA, exit_condition = FALSE,
                       Support = NA, Support_enter_level = NA, at = Sys.time(),
                       action = "no_action", pos_perc = as.numeric(NA),
                       pair  = pair[i], id = as.character(NA),
                       Rsi = as.numeric(NA))
  
  write.csv(trades, paste0("/Users/christos.polysopoulos/Repositories/Coinmaker_v0.2",
                            "/Trading", 
                            "/action_tables",
                             paste0("/action_table_",pair[i], ".csv")), row.names=FALSE)
  Sys.sleep(2)
}

