
get.CutOffvsDay <- function(power, start_x, end_x){
  start.date <- as.POSIXct(start_x, format = "%Y-%m-%d %H:%M:%S")
  end.date <- as.POSIXct(end_x, format = "%Y-%m-%d %H:%M:%S")
  
  day.sec <- 86400 # number of seconds in a day to iterate by day
  
  current.day.start <- start.date
  current.day.end   <- start.date + as.difftime(1, units = "days")
  
  
  cutoff_table <- data.frame(
    Date = as.Date(character()),
    Cutoff = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  
  while(current.day.start <= end.date){
    signal <- cut.data(power, current.day.start, current.day.end)
    day <- as.Date(format(current.day.start))
    temp.cutOff <- findCutoff(signal)
    
    cutoff_table <- rbind(cutoff_table, data.frame(Date = day, Cutoff = temp.cutOff))
    
    
    
    current.day.start <- current.day.start + day.sec
    current.day.end <- current.day.end + day.sec
    print(paste("Day:", day, "Cutoff:", temp.cutOff))
  }
  
  return(cutoff_table)
}


findCutoff <- function(signal, step = 0.1, step.decimals = 1) {
  signal_times <- as.POSIXct(signal[[1]], format = "%Y-%m-%d %H:%M:%S")
  
  cutOff <- floor(max(signal[[2]]) / step) * step
  lastCutoff <- cutOff
  running <- TRUE  
  while(running){
    sum.power <- signal
    bat <- create_battery(0)
    
    for(i in seq_along(signal[[1]])){
      charge.rate <- (cutOff - signal[[2]][i])
      battery.out <- bat(charge.rate)
      sum.power[[2]][i] <- signal[[2]][i] +  battery.out
    }
    #fail condition where battery runs out (last cutoff was optimal)

    if(any(sum.power[[2]] > cutOff, na.rm = TRUE)){
      return(lastCutoff)
    }
    else{
      lastCutoff <- cutOff
      cutOff <- cutOff - step
      if (cutOff <= 0) stop("Cutoff dropped below 0")
    }
  }
}


table <- get.CutOffvsDay(allPowerData, "2019-01-01 00:00:00", "2023-12-31 00:00:00")

data <- read.csv("model_data.csv")
data

table

merged <- merge(data, table, by = "Date", all.x = TRUE)
merged

names(merged)[names(merged) == "Cutoff.y"] <- "Peak_Cutoff"
names(merged)[names(merged) == "Cutoff.x"] <- "Demand_Cutoff"

merged


write.csv(merged,"model_data.csv", row.names = FALSE)
