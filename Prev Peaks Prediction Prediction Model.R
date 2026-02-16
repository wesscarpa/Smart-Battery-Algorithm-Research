rm(list = ls())


setwd("C:/Users/wscar/OneDrive/OCCIDENTAL/Fall 2025/Research/Files for distribution")
source("C:/Users/wscar/OneDrive/OCCIDENTAL/Fall 2025/Research/Files for distribution/R Scripts/Battery.R")

#----------Get data for training Model---------------#
data.demand <- data.frame(
  avg.temp = numeric(), 
  max.temp = numeric(),
  month = numeric(), 
  d.o.w = numeric(),
  prev.peak = numeric(),
  prev.energy = numeric(), 
  demand.cutOff = numeric(),
  stringsAsFactors = FALSE
)



energyFiles <- c(
  "2019, Oxy Energy Use.txt",
  "2020, Oxy Energy Use.txt",
  "2021, Oxy Energy Use.txt",
  "2022, Oxy Energy Use.txt",
  "2023, Oxy Energy Use.txt"
)

years <- c(2019, 2020, 2021, 2022, 2023)
tempFiles <- paste0("Temperature Quarter-Hour Averages ", years, ".txt")


optCutOff20_23 <- read.csv("optCutOff20_23")

# Initialize an empty data frame
allPowerData <- data.frame(Time = as.POSIXct(character()), Power = numeric())

for(file in energyFiles){
  # Read the CSV
  energy.values <- read.csv(file)
  
  # Convert all time values to POSIXct
  energy.values$Time <- as.POSIXct(energy.values$Time, format = "%m/%d/%y %H:%M")
  
  # Convert to kW
  energy.values$College.Net.Use.kWh <- energy.values$College.Net.Use.kWh / 0.25 / 1000
  
  # Rename columns
  names(energy.values) <- c("Time", "Power")
  
  # Append to the main data frame
  allPowerData <- rbind(allPowerData, energy.values)
}

# Optionally, sort by time
allPowerData <- allPowerData[order(allPowerData$Time), ]

allPowerData


# Initialize an empty data frame for temperature
allTempData <- data.frame(Time = as.POSIXct(character()), Temp = numeric())

for(file in tempFiles){
  # Read the CSV
  temp.values <- read.csv(file)
  
  # Convert all time values to POSIXct
  temp.values$Time <- as.POSIXct(temp.values$Time, format = "%Y-%m-%d %H:%M:%S")
  
  # Rename columns (assuming the temperature column is the second column)
  names(temp.values) <- c("Time", "Temp F", "Temp C")
  
  # Append to the main data frame
  allTempData <- rbind(allTempData, temp.values)
}

# Optionally, sort by time
allTempData <- allTempData[order(allTempData$Time), ]


allTempData


# Suppose you only want 'Time' and 'Power' from allPowerData
powerSubset <- allPowerData[, c("Time", "Power")]

# And 'Time' and 'Temp' from allTempData
tempSubset <- allTempData[, c("Time", "Temp C")]

# Now merge only these columns by Time
data <- merge(powerSubset, tempSubset, by = "Time", all = TRUE)


getmodeldata <- function(data){
  start.date <- data$Time[1]
  end.date <- data$Time[length(data$Time)]
  
  day.sec <- 86400 # number of seconds in a day to iterate by day
  
  current.day.start <- start.date
  current.day.end   <- start.date + as.difftime(1, units = "days")
  
  
  table <- data.frame(
    avg.temp = numeric(), 
    max.temp = numeric(),
    month = numeric(), 
    d.o.w = numeric(),
   prev.peak = numeric(),
    prev.energy = numeric(), 
    demand.cutOff = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  
  while(current.day.start <= end.date){
    #cut to current day
    signal <- cut.data(data, current.day.start, current.day.end)
    #get day
    day <- as.Date(format(current.day.start))
    #get month
    month <- as.numeric(format(day, "%m"))
    #get d.o.w Mon = 1, Sun = 7
    d.o.w <- wday(day, week_start = 1)
    #get prev.peak
    #prev.peak <- get.Prev.Peak(day)
  
      
    temp.cutOff <- getCutOff(day)
    
    table <- rbind(table, data.frame(Date = day, Cutoff = temp.cutOff))
    
    
    
    
    
    current.day.start <- current.day.start + day.sec
    current.day.end <- current.day.end + day.sec
    print(paste("Day:", day, "Cutoff:", temp.cutOff))
  }
  
  return(cutoff_table)
}

getCutOff <- function(day){
  # Find the matching row
  cutoff_val <- optCutOff20_23$Cutoff[optCutOff20_23$Date == day]
  
  # If not found, return NA
  if (length(cutoff_val) == 0) {
    warning(paste("No cutoff found for day:", day))
    return(NA)
  }
  
  return(cutoff_val)


}






data <- read.csv("model_data.csv")
data  

table

