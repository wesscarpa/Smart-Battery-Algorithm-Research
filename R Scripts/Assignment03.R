rm(list = ls())

setwd("C:/Users/wscar/Downloads/Files for distribution/Files for distribution")
source("C:/Users/wscar/Downloads/Files for distribution/Files for distribution/R Scripts/Battery.R")

battery <- create_battery( 0 ) # This initializes everything and creates a function called battery



filename <- "2021, Oxy Energy Use.txt"

read.csv(filename)
energy.values <- read.csv(filename)
energy.values


#convert all time values to one format
energy.values$Time <- as.POSIXct(energy.values$Time, format = "%m/%d/%y %H:%M")
power.values <- energy.values 
#convert to kW
power.values$College.Net.Use.kWh = power.values$College.Net.Use.kWh/0.25/1000

power.values



#input: signal[[Time], [Power]], start time of cut, end time of cut,   --> output: subset of data between start and end
# Time in format %Y-%m-%d %H:%M:%S
cut.data <- function(signal, start.date, end.date){
  start.date <- as.POSIXct(start.date, format = "%Y-%m-%d %H:%M:%S")
  end.date <- as.POSIXct(end.date, format = "%Y-%m-%d %H:%M:%S")
  cut.signal <- subset(signal, as.POSIXct(signal[[1]] ) > start.date & as.POSIXct(signal[[1]]) < end.date)
  return(cut.signal)
}
min.date <- as.POSIXct("2023-08-25 00:00:00", format = "%Y-%m-%d %H:%M:%S")

max.date <- as.POSIXct("2023-09-26 00:00:00", format = "%Y-%m-%d %H:%M:%S")

month.power <- cut.data(power.values,min.date, max.date)
plot(month.power[[1]], month.power[[2]], type = "l", xlab = "Time", ylab = "Power MW", col = "Red")

max(month.power$College.Net.Use.kWh)

names(month.power)



#-----------------------Find Cutoff for ant Signal------------------------------#
#-------------------------------------------------------------------------------#
#signal[[Time], [Power]]
findCutoff <- function(signal, step = 0.1, step.decimals = 1) {
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
    if(max(sum.power[[2]]) > cutOff){
      return(lastCutoff)
    }
    else{
      lastCutoff <- cutOff
      cutOff <- cutOff - step
      if (cutOff <= 0) stop("Cutoff dropped below 0 — battery insufficient.")
    }
  }
}

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#

findCutoff(month.power)

plot(as.POSIXct(sum.power[["Time"]], format = "%Y-%m-%d %H:%M:%S" ), sum.power[["College.Net.Use.kWh"]], type = "l", xlab = "Time", ylab = "Power kW", col = "Green", ylim = c(0,2.9))
abline(h= cutOff, col = "Purple")



#----------------------------------- FFT ---------------------------------------#
#-------------------------------------------------------------------------------#

#input is y values of signal, it will plot the frequency domain and return the normalized and positive fft
plot_fft <- function(signal){
  dt <- 0.25                # sampling interval in hours
  N <- length(signal)       # number of samples
  # Compute FFT
  Y <- fft(signal)
  # Normalize amplitudes
  amp <- Mod(Y) / N
  
  # One-sided amplitude spectrum
  amp_one_sided <- 2 * amp[1:(N/2)]
  
  # Frequency axis in cycles per hour
  freq <- (0:(N-1)) / (N * dt)
  
  # Plot one-sided spectrum
  plot(freq[1:(N/2)], amp_one_sided, type = "l",
       xlab = "Frequency (cycles/hour)", ylab = "Amplitude (MW)")
  
  return(list(freq = freq[1:(N/2)], amplitude = amp_one_sided))
}
plot_fft(month.power$College.Net.Use.kWh)
plot_fft(power.values$College.Net.Use.kWh)

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#



#Find all Optimal Cutoffs For Each Day
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
get.CutOffvsDay <- function(power, start_x, end_x){
  yr.start <- as.POSIXct(start_x, format = "%Y-%m-%d %H:%M:%S")
  yr.end <- as.POSIXct(end_x, format = "%Y-%m-%d %H:%M:%S")
  
  day.sec <- 86400 # number of seconds in a day to iterate by day

  current.day.start <- current.day.start + as.difftime(1, units = "days")
  current.day.end   <- current.day.end + as.difftime(1, units = "days")

  
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



table <-get.CutOffvsDay()
cutOff.table <- table

plot(table$Date, table$Cutoff, type = "l", ylab = "Optimal Cutoff [MW]", xlab = "Day")



#------------------------------ Temp Comparison-----------------------------------#

temps.file <- "Temperature Quarter-Hour Averages 2021.txt"
temps2023 <- read.csv(temps.file)

temps2023$Time <- as.POSIXct(temps2023$Time,  format = "%Y-%m-%d %H:%M:%S")

temps2023 <- subset(temps2023, Average.Temperature.C != 1000000)



yr.start <- as.POSIXct(temps2023$Time[1], format = "%Y-%m-%d %H:%M:%S")
yr.end <- as.POSIXct(temps2023$Time[nrow(temps2023) - 1], format = "%Y-%m-%d %H:%M:%S")

yr.start
yr.end

day.sec <- 86400 # number of seconds in a day to iterate by day

current.day.start <-yr.start
current.day.end <- current.day.start + day.sec


temps.avg <- data.frame(
  Date = as.Date(character()),
  Avg.Temp = numeric(),
  stringsAsFactors = FALSE
)

while(current.day.end < yr.end){
  temps.day <- cut.data(temps2023, current.day.start, current.day.end)
  day <- as.Date(format(current.day.start))
  N <- nrow(temps.day)
  day.avg.temp <- sum(temps.day$Average.Temperature.C)/N
  temps.avg <- rbind(temps.avg, data.frame(Date = day, Avg.Temp = day.avg.temp))
  
  
  
  current.day.start <- current.day.start + day.sec
  current.day.end <- current.day.end + day.sec
  print(day.avg.temp)
}




temps.avg



#-------------Find Best Correlation------------------# 

par(new=TRUE) 
plot(temps.avg$Date, temps.avg$Avg.Temp, type = "o", col= "Purple", xlab = "", ylab = "", axes = FALSE)


axis(side=4, col="purple", col.axis="purple")
  
day.vs.AVGTemp.vs.OptCutoff.2022 <- merge(temps.avg.2022, cutOff.table, by="Date", all=TRUE)

cor(temps.avg.2023$Avg.Temp, cutOff.table.2023$Cutoff )


plot(day.vs.AVGTemp.vs.OptCutoff.2022$Avg.Temp, day.vs.AVGTemp.vs.OptCutoff.2022$Cutoff, col = "blue", ylim = c(0,3), xlab = "AVG Temp [°C]", ylab = "Optimal Cutoff (Daily) [MW]")

# Fit model
bfl <- lm(Cutoff ~ Avg.Temp, data = merged)

# Get fitted line coefficients

coef(bfl)

alpha <- coef(bfl)[2]
beta <- coef(bfl)[1]

# Residual standard deviation (RMSE)
resid_sd <- sigma(bfl)  

# Draw bfl line
abline(bfl, col="red")

m <- 1/13
x1 <- 17
y1 <- 1.75
b <- y1 - m * x1



abline(h=1.75, col = "pink")
abline(v = 17, col = "grey")
abline(a = b, b = m, col = "pink")
points(14.4,1.75, col = "red")



summary(bfl)$r.squared




bfl_prediction_model <-function(daily.avg.temp){
  est.cutoff <- alpha*daily.avg.temp + beta
  return(est.cutoff)
}

# y = 1/13x - 17/13 + 1.95 for x > 14.4
piecewise_model <- function(daily.avg.temp){
  ifelse(daily.avg.temp < 17,
         1.75,
         daily.avg.temp * (1/13) - 17/13 + 1.75)
}



#-----------------Get Table of Daily Optimal Cutoffs---------------------------#
power.values.2021

cutOff.table.2021
temps.avg.2021

cutOff.table.2023 
temps.avg.2023 

temps.avg.2022  
cutOff.table.2022

max(temps.avg.2022$Avg.Temp)


bfl.cutoff.pred.2022 <- data.frame(
  Date   = temps.avg.2022$Date,
  cutOff = bfl_prediction_model(temps.avg.2022$Avg.Temp)
)

better.model.cutoff.pred.2022 <- data.frame(
  Date   = temps.avg.2022$Date,
  cutOff = piecewise_model(temps.avg.2022$Avg.Temp)
)

better.model.cutoff.pred.2022$cutOff[better.model.cutoff.pred.2022$Date == as.Date("2022-11-1")]



# fix missing cutoffs (no weather data) by setting cutoff to day before cutoff
for (i in 2:nrow(better.model.cutoff.pred.2022)) {
  if (is.na(better.model.cutoff.pred.2022$cutOff[i])) {
    better.model.cutoff.pred.2022$cutOff[i] <- better.model.cutoff.pred.2022$cutOff[i - 1]
  }
}




#-------------------Test Models on 2022------------------------#

test_model <- function(modeled.cutoff.table, signal){
  sum.power <- signal
  bat <- create_battery(0)
  signal_dates <- as.Date(signal[[1]])
  # Shift timestamps by 6 hours backward so "day" starts at 6 AM
  signal_times <- as.POSIXct(signal[[1]], format = "%Y-%m-%d %H:%M:%S")
  shifted_dates <- as.Date(signal_times - 6*3600)  # subtract 6 hours
  
  
  for(i in seq_along(signal_times)){
    cutOff <- modeled.cutoff.table$cutOff[modeled.cutoff.table$Date == shifted_dates[i]]
    
    
    
    # Handle missing or multiple matches safely
    if (length(cutOff) == 0 || all(is.na(cutOff))) {
      message(paste("Missing cutOff for", signal_dates[i]))
      cutOff <- 0
    } else {
      cutOff <- cutOff[1]  # take first value if multiple
    }
    
    
    
    charge.rate <- (cutOff - signal[[2]][i])
    battery.out <- bat(charge.rate)
    sum.power[[2]][i] <- signal[[2]][i] +  battery.out
  }
  
  return(sum.power)
}





plot(as.POSIXct(power.values[[1]], format = "%Y-%m-%d %H:%M:%S" ), power.values[[2]], type = "l", xlab = "Time", ylab = "Power kW", col = "Red") #, xlim = c(start_time, end_time)'''#)


improved.power.usage = test_model(better.model.cutoff.pred.2022, power.values)
lines(as.POSIXct(improved.power.usage[[1]], format = "%Y-%m-%d %H:%M:%S" ), improved.power.usage[[2]], type = "l", xlab = "Time", ylab = "Power kW", col = "green") #, xlim = c(start_time, end_time))

start_time <- as.POSIXct("2022-10-31 06:00:00")
end_time   <- as.POSIXct("2022-11-2 06:00:00")

specific_day <- as.Date("2022-11-1")
cutoff_valuez <- better.model.cutoff.pred.2022$cutOff[
  better.model.cutoff.pred.2022$Date == specific_day
]
cutoff_value <- cutoff_valuez[1]
cutoff_value


plot(as.POSIXct(power.values[[1]], format = "%Y-%m-%d %H:%M:%S" ), power.values[[2]], type = "l", xlab = "Time", ylab = "Power kW", col = "Red", xlim = c(start_time, end_time))
abline(h = cutoff_value, col = "blue")



max.usage.2022 <- max(power.values$College.Net.Use.kWh)
max.usage.2022
max.usage.2022.reduced <- max(improved.power.usage$College.Net.Use.kWh)
max.usage.2022.reduced

j <- which(improved.power.usage$College.Net.Use.kWh == max.usage.2022.reduced)

improved.power.usage$Time[j]

FDC1 <- max.usage.2022 * 8.95
FDC2 <- max.usage.2022.reduced *8.95

savedmoney <- (FDC1 - FDC2) * 12 * 1000 
savedmoney


#------------------ 2022-2023 -------------------------------#

temps.avg.2022_2023 <- rbind(temps.avg.2022, temps.avg.2023)
cutOff.table.2022_2023 <- rbind(cutOff.table.2022, cutOff.table.2023)


better.model.cutoff.pred.2022_2023 <- data.frame(
  Date   = temps.avg.2022_2023$Date,
  cutOff = piecewise_model(temps.avg.2022_2023$Avg.Temp)
)
better.model.cutoff.pred.2021 <- data.frame(
  Date   = temps.avg.2021$Date,
  cutOff = piecewise_model(temps.avg.2021$Avg.Temp)
)



for (i in 2:nrow(better.model.cutoff.pred.2021)) {
  if (is.na(better.model.cutoff.pred.2021$cutOff[i])) {
    better.model.cutoff.pred.2021$cutOff[i] <- better.model.cutoff.pred.2021$cutOff[i - 1]
  }
}





power.values.2021
power.values.2022 
power.values.2023 


power.values.2022_2023 <- rbind(power.values.2022, power.value.2023)


improved.power.usage.2022_2023 = test_model(better.model.cutoff.pred.2022_2023, power.values.2022_2023)
improved.power.usage.2021 <- test_model(better.model.cutoff.pred.2021, power.values.2021)

plot(as.POSIXct(power.values.2021[[1]], format = "%Y-%m-%d %H:%M:%S" ), power.values.2021[[2]], type = "l", xlab = "Time", ylab = "Power kW", col = "red")#  xlim = c(start_time, end_time) )
lines(as.POSIXct(improved.power.usage.2021[[1]], format = "%Y-%m-%d %H:%M:%S" ), improved.power.usage.2021[[2]], type = "l", xlab = "Time", ylab = "Power kW", col = "green") # xlim = c(start_time, end_time))


day.vs.OptCutoff.2022 <- get.CutOffvsDay(power.values.2022_2023, "2022-01-01 00:00:00", "2023-01-01 00:00:00")
day.vs.OptCutoff.2023 <- get.CutOffvsDay(power.values.2022_2023, "2023-01-01 00:00:00", "2024-01-01 00:00:00")

day.vs.AVGTemp.vs.OptCutoff.2022 <- merge(temps.avg.2022, day.vs.OptCutoff.2022, by="Date", all=TRUE)
day.vs.AVGTemp.vs.OptCutoff.2023 <- merge(temps.avg.2023, day.vs.OptCutoff.2023, by="Date", all=TRUE)




plot(day.vs.AVGTemp.vs.OptCutoff.2022$Avg.Temp, day.vs.AVGTemp.vs.OptCutoff.2022$Cutoff, col = "blue", ylim = c(0,3), xlim = c(0,40), xlab = "AVG Temp [°C]", ylab = "Optimal Cutoff (Daily) [MW]")
par(new=TRUE) 
plot(day.vs.AVGTemp.vs.OptCutoff.2023$Avg.Temp, day.vs.AVGTemp.vs.OptCutoff.2023$Cutoff, col = "green", ylim = c(0,3),xlim = c(0,40), xlab = "AVG Temp [°C]", ylab = "Optimal Cutoff (Daily) [MW]")


(max(power.values.2021$College.Net.Use.kWh) - max(improved.power.usage.2021$College.Net.Use.kWh)) *12000






