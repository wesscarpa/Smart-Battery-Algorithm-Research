setwd( "C:/Users/wscar/Downloads/Files for distribution/Files for distribution") # Change for your computer and analysis directory

filename <- "2023, Oxy Energy Use.txt"
read.csv(filename)
data.values <- read.csv(filename)

plot(as.POSIXct(data.values[["Time"]], format = "%Y-%m-%d %H:%M:%S" ), data.values[["College.Net.Use.kWh"]], type = "l", xlab = "Time", ylab = "Power", col = "Red")

max_index <- which.max(data.values$College.Net.Use.kWh)


max_index

peak_day <- data.values$Time[max_index]
peak_day <- substr(peak_day, 1, nchar(peak_day) - 9)

peak_day_data <- data.values[grepl(peak_day, data.values$Time),]
peak_day_data


plot(as.POSIXct(peak_day_data[["Time"]], format = "%Y-%m-%d %H:%M:%S" ), peak_day_data[["College.Net.Use.kWh"]], type = "l", xlab = "Time", ylab = "Power", col = "Red")

#do an integral form y0 to y_max, change y0 until integral is as close to 2000kWh as possible
y.start <- 600
data.abv.y = peak_day_data[(peak_day_data$College.Net.Use.kWh > y.start),]
remaining.data = peak_day_data[(peak_day_data$College.Net.Use.kWh < y.start),]


plot(as.POSIXct(peak_day_data[["Time"]], format = "%Y-%m-%d %H:%M:%S" ), peak_day_data[["College.Net.Use.kWh"]], type = "l", xlab = "Time", ylab = "Power", col = "Red")
lines(as.POSIXct(remaining.data[["Time"]], format = "%Y-%m-%d %H:%M:%S" ), remaining.data[["College.Net.Use.kWh"]], type = "l", xlab = "Time", ylab = "Power", col = "Blue" )


data.abv.y

calc.set <- data.abv.y

calc.set$College.Net.Use.kWh <- calc.set$College.Net.Use.kWh - y.start
calc.set$College.Net.Use.kWh <- (calc.set$College.Net.Use.kWh)

energy.abv.y0 <- sum(calc.set$College.Net.Use.kWh, na.rm = TRUE)
Power.graph <- peak_day_data
Power.graph$College.Net.Use.kWh <- 4*peak_day_data$College.Net.Use.kWh

energy.abv.y0 <- 0


remaining.data <- peak_day_data
target_energy <- 2000
energy_abv_y0 <- 0
y_start <- 700
while(energy_abv_y0 < target_energy){
  y_start <- y_start - 1  # decrement by 1 kW
  
  # Select values above y_start
  data_abv_y <- Power.graph[Power.graph$College.Net.Use.kWh > y_start, ]
  remaining.data$Time = Power.graph$Time
  remaining.data$College.Net.Use.kWh <- pmin(Power.graph$College.Net.Use.kWh,y_start )
  
  # Subtract y_start and divide by 4
  calc_set <- data_abv_y
  calc_set$College.Net.Use.kWh <- (calc_set$College.Net.Use.kWh - y_start)/4
  
  
  # Sum energy above y_start
  energy_abv_y0 <- sum(calc_set$College.Net.Use.kWh, na.rm = TRUE)
}
y_cutOff <- y_start
energy.abv.y0

cat("y0 value for 2000 kWh above it:", y_cutOff, "\n")


plot(as.POSIXct(peak_day_data[["Time"]], format = "%Y-%m-%d %H:%M:%S" ), peak_day_data[["College.Net.Use.kWh"]], type = "l", xlab = "Time", ylab = "Power", col = "Red", ylim = c(0,800))
lines(as.POSIXct(remaining.data[["Time"]], format = "%Y-%m-%d %H:%M:%S" ), remaining.data[["College.Net.Use.kWh"]], type = "l", xlab = "Time", ylab = "Power", col = "Purple")


ogFDC <-8.95 * max(peak_day_data$College.Net.Use.kWh)*4

newFDC <- 8.95 * y_cutOff*4
savedMoney = ogFDC - newFDC
savedMoney




#graph battery Power output over time
calc_set$College.Net.Use.kWh <- calc_set$College.Net.Use.kWh * 
plot(as.POSIXct(calc_set[["Time"]], format = "%Y-%m-%d %H:%M:%S" ), calc_set[["College.Net.Use.kWh"]], type = "l", xlab = "Time", ylab = "Power", col = "Green")

