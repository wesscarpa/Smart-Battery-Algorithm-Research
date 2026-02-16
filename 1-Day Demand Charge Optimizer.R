rm(list = ls())

setwd("C:/Users/wscar/Downloads/Files for distribution/Files for distribution")
source("C:/Users/wscar/Downloads/Files for distribution/Files for distribution/R Scripts/Battery.R")

# High Peak Period (1:00 PM - 5:00 PM) - $9.70 per kW
# Low Peak Period (10:00 AM - 1:00 PM) - $3.30 per kW

energyFiles <- list(
  "2021, Oxy Energy Use.txt",
  "2022, Oxy Energy Use.txt",
  "2023, Oxy Energy Use.txt"
)


# Holds Power vs Time for each year in the file list
powerTables <- list()


# populate the list of Power Usage vs Time for 2021 -2023

for(file in energyFiles){
  energy.values <- read.csv(file)
  
  #convert all time values to one format
  energy.values$Time <- as.POSIXct(energy.values$Time, format = "%m/%d/%y %H:%M")
  
  #convert to kW
  energy.values$College.Net.Use.kWh <- energy.values$College.Net.Use.kWh / 0.25/1000
  
  names(energy.values) <- c("Time", "Power")
  
  powerTables[[file]] <- energy.values
  
}

names(powerTables) <- c("2021", "2022", "2023")




cut.data <- function(signal, start.date, end.date){
  
  start.date <- as.POSIXct(start.date, format = "%Y-%m-%d %H:%M:%S")
  end.date <- as.POSIXct(end.date, format = "%Y-%m-%d %H:%M:%S")
  cut.signal <- subset(signal, as.POSIXct(signal[[1]] ) > start.date & as.POSIXct(signal[[1]]) < end.date)
  return(cut.signal)
}


septPower <- list()


# populates the list of power vs time for September only for all years in powerTables
for(year in names(powerTables)){
  table <- powerTables[[year]]
  
  sept.start <- as.POSIXct(paste0(year, "-09-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S")
  sept.end <- as.POSIXct(paste0(year, "-10-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S")
  
  septPower[[year]] <- cut.data(table, sept.start, sept.end)
  
}


septPower[["2021"]]



#Strategy: Run the battery on max power between 1 and 5

battery <- create_battery(0)
battery(1)


strategy1 <- function(signal){
  sum.power <- signal
  bat <- create_battery(0)
  signal_times <- as.POSIXct(signal[[1]], format = "%Y-%m-%d %H:%M:%S")
  
  
  for(i in seq_along(signal_times)){
    hr <- as.numeric(format(signal_times[i], "%H")) 
    if(hr >= 13 && hr < 17){
      charge.rate <- - (signal[[2]][i])
    }else if(hr >= 10 && hr < 13){
      charge.rate <- 0
    }else{
      charge.rate <- 1
    }
    
    battery.out <- bat(charge.rate)
    sum.power[[2]][i] <- signal[[2]][i] +  (battery.out)
  }
  
  return(sum.power)
}


sumPwr21 <- strategy1(septPower[["2021"]])

sumPwr21


plot(septPower[["2021"]][[1]],septPower[["2021"]][[2]], ,type = "l",xlab = "Time", ylab = "Power (kW)", main = "Power vs Time" )
plot(sumPwr21[[1]], sumPwr21[[2]],type = "l",xlab = "Time", ylab = "Power (kW)", main = "Sum Power vs Time", col = "green")



get.CutOffvsDay <- function(power, start_x, end_x){
  yr.start <- as.POSIXct(start_x, format = "%Y-%m-%d %H:%M:%S")
  yr.end <- as.POSIXct(end_x, format = "%Y-%m-%d %H:%M:%S")
  
  day.sec <- 86400 # number of seconds in a day to iterate by day
  
  current.day.start <- yr.start
  current.day.end   <- yr.start + as.difftime(1, units = "days")
  
  
  cutoff_table <- data.frame(
    Date = as.Date(character()),
    Cutoff = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  
  while(current.day.end < yr.end){
    signal <- cut.data(power, current.day.start, current.day.end)
    day <- as.Date(format(current.day.start))
    temp.cutOff <- findCutoff(signal)
    
    cutoff_table <- rbind(cutoff_table, data.frame(Date = day, Cutoff = temp.cutOff))
    
    
    
    current.day.start <- current.day.start + day.sec
    current.day.end <- current.day.end + day.sec
    print(temp.cutOff)
  }
  
  return(cutoff_table)
}


# finds cutoff for 1 day only cutting off between 1-5 PM
findCutoff <- function(signal, step = 0.1, step.decimals = 1) {
  signal_times <- as.POSIXct(signal[[1]], format = "%Y-%m-%d %H:%M:%S")
  
  cutOff <- floor(max(signal[[2]]) / step) * step
  lastCutoff <- cutOff
  running <- TRUE  
  while(running){
    sum.power <- signal
    bat <- create_battery(0)
    
    for(i in seq_along(signal[[1]])){
      hr <- as.numeric(format(signal_times[i], "%H")) 
      if(hr >= 13 && hr < 17){
        charge.rate <- (cutOff - signal[[2]][i])
      }else if(hr >= 10 && hr < 13){
        charge.rate <- 0
      }else{
        charge.rate <- 0.1
      }
      battery.out <- bat(charge.rate)
      sum.power[[2]][i] <- signal[[2]][i] +  battery.out
    }
    #fail condition where battery runs out (last cutoff was optimal)
    #only check if any of the values between 1-5 are above the cutoff for 1-5
    high_idx <- which(format(signal_times, "%H") >= "13" & format(signal_times, "%H") < "17")
  
    if(any(sum.power[[2]][high_idx] > cutOff, na.rm = TRUE)){
      return(lastCutoff)
    }
    else{
      lastCutoff <- cutOff
      cutOff <- cutOff - step
      if (cutOff <= 0) stop("Cutoff dropped below 0")
    }
  }
}

day.start <- as.POSIXct("2021-09-13 00:00:00", format = "%Y-%m-%d %H:%M:%S")
day.end <- as.POSIXct("2021-09-14 00:00:00", format = "%Y-%m-%d %H:%M:%S")

sept13 <- cut.data(powerTables[["2021"]], day.start, day.end)
plot(sept13[[1]], sept13[[2]], ,type = "l",xlab = "Time", ylab = "Power (kW)", main = "Power vs Time" )


sept13cut <- findCutoff(sept13)
sept13cut


