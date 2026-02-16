setwd("C:/Users/wscar/OneDrive/OCCIDENTAL/Fall 2025/Research/Files for distribution")
source("C:/Users/wscar/OneDrive/OCCIDENTAL/Fall 2025/Research/Files for distribution/R Scripts/Battery.R")

model_data <- read.csv("model_data.csv")




#Model for Facility Charge 


plot(model_data$EnergySum_prev, model_data$Peak_Cutoff, col = "blue", ylim = c(0,3), xlim = c(0,70), xlab = "Prev Day Energy Use [MWh]", ylab = "Optimal Peak Cutoff (Daily) [MW]")



# Fit a simple linear model
fit <- lm(Peak_Cutoff ~ EnergySum_prev, data = model_data)
# Get the R^2 value: 0.7154163
summary(fit)$r.squared

coefs <- coef(fit)
a <- coefs[1]   # intercept
b <- coefs[2]   # slope
shift <- 0.4
abline(a + shift, b, col = "red", lwd = 2)

facilityChargeModel <- function(EnergySum_prev) {
  0.17692715 + 0.4 + 0.03753222 * EnergySum_prev
}


#Model for Demand Charge 



plot(model_data$EnergySum_prev, model_data$Demand_Cutoff, col = "blue", ylim = c(0,3), xlim = c(0,70), xlab = "Prev Day Energy Use [MWh]", ylab = "Optimal Peak Cutoff (Daily) [MW]")



# Fit a simple linear model
fit <- lm(Demand_Cutoff ~ EnergySum_prev, data = model_data)
# Get the R^2 value: 0.6207982
summary(fit)$r.squared

coefs2 <- coef(fit)
a2 <- coefs2[1]   # intercept
b2 <- coefs2[2]   # slope
shift2 <- 0.35
abline(a2 + shift2, b2, col = "red", lwd = 2)
coefs2

demandChargeModel <- function(EnergySum_prev) {
  -0.65591328 + 0.35 + 0.04090796 * EnergySum_prev
}



#---------------------Helper Functions-----------------------------#
#------------------------------------------------------------------#

test_model_demand <- function(modeled.cutoff.table, signal){
  sum.power <- signal
  bat <- create_battery(0)
  signal_dates <- as.Date(signal[[1]])
  signal_times <- as.POSIXct(signal[[1]], format = "%Y-%m-%d %H:%M:%S")
  
  for(i in seq_along(signal_times)){
    cutOff <- modeled.cutoff.table[[2]][modeled.cutoff.table[[1]] == signal_dates[i]]
    
    
    
    # Handle missing or multiple matches safely
    if (length(cutOff) == 0 || all(is.na(cutOff))) {
      message(paste("Missing cutOff for", signal_dates[i]))
      cutOff <- 0
    } else {
      cutOff <- cutOff[1]  # take first value if multiple
    }
    
    hr <- as.numeric(format(signal_times[i], "%H")) 
    if(hr >= 13 && hr < 17){
      charge.rate <- (cutOff - signal[[2]][i])
    }else if((hr >= 10 && hr < 13) || (hr >= 17 && hr < 20)){
      charge.rate <- 0
    }else{
      charge.rate <- 0.5
    }
    battery.out <- bat(charge.rate)
    sum.power[[2]][i] <- signal[[2]][i] +  battery.out
    
    
    
  }
  
  return(sum.power)
}



library(lubridate)

high_low_Periods <- function(signal){
  
  # Ensure correct types
  signal$Time  <- as.POSIXct(signal$Time)
  signal$Power <- as.numeric(signal$Power)
  
  # Extract components
  signal$Year  <- year(signal$Time)
  signal$Month <- month(signal$Time)
  signal$Hour  <- hour(signal$Time)
  signal$WeekdayNum <- wday(signal$Time, week_start = 1) # 1 = Monday
  
  # Weekdays only (Mon–Fri)
  weekday_df <- subset(signal, WeekdayNum <= 5)
  
  # HIGH PEAK = 1 PM – 5 PM
  high_peak <- subset(weekday_df, Hour >= 13 & Hour < 17)
  
  # LOW PEAK = 10–1 PM AND 5–8 PM
  low_peak <- subset(
    weekday_df,
    (Hour >= 10 & Hour < 13) |  # 10AM–1PM
      (Hour >= 17 & Hour < 20)    # 5PM–8PM
  )
  
  return(list(
    high_peak = high_peak,
    low_peak = low_peak
  ))
}






test_model <- function(modeled.cutoff.table, signal){
  sum.power <- signal
  bat <- create_battery(0)
  signal_dates <- as.Date(signal[[1]])
  # Shift timestamps by 6 hours backward so "day" starts at 6 AM
  signal_times <- as.POSIXct(signal[[1]], format = "%Y-%m-%d %H:%M:%S")
  shifted_dates <- as.Date(signal_times - 6*3600)  # subtract 6 hours
  
  
  for(i in seq_along(signal_times)){
    cutOff <- modeled.cutoff.table$facilityCutOff[modeled.cutoff.table$Date == shifted_dates[i]]
    
    
    
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




library(dplyr)
library(lubridate)

getEnergySum <- function(data) {
  data %>%
    mutate(Date = as.Date(Time)) %>%             # extract date from timestamp
    group_by(Date) %>%                           # group by day
    summarise(
      Peak = max(Power, na.rm = TRUE),           # daily max
      EnergySum = sum(Power * 0.25, na.rm = TRUE) # daily energy
    ) %>%
    ungroup()
}



runFacilityModel <- function(signal) {
  
  
  return(list(
    cutoff_table = modeled.cutoff.table,
    adjusted_signal = output_signal
  ))
}

calculateFacilityCharge <- function(signal) {
  # Expect columns: Time and Power
  signal$Time <- as.POSIXct(signal$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  signal$Power <- as.numeric(signal$Power)
  
  # Find the yearly peak power (convert MW → kW if necessary)
  peak_power_kw <- max(signal$Power, na.rm = TRUE) * 1000
  
  # Facility charge rate ($ per kW per month)
  rate <- 8.95
  
  # Facility charge for the year
  annual_facility_charge <- peak_power_kw * rate * 12
  
  # Return as data frame
  result <- data.frame(
    Peak_Power_kW = peak_power_kw,
    Monthly_Rate = rate,
    Months = 12,
    Annual_Facility_Charge = annual_facility_charge
  )
  
  return(result)
}



calculateCost <- function(signal) {
  # Expect signal columns: Time and Power
  signal$Time <- as.POSIXct(signal$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  signal$Power <- as.numeric(signal$Power)
  
  # Extract components
  signal$Year <- as.numeric(format(signal$Time, "%Y"))
  signal$Month <- as.numeric(format(signal$Time, "%m"))
  signal$Hour <- as.numeric(format(signal$Time, "%H"))
  signal$Weekday <- weekdays(signal$Time)
  
  # Find all (year, month) combinations
  all_months <- unique(signal[, c("Year", "Month")])
  
  monthly_costs <- list()
  
  for (i in seq_len(nrow(all_months))) {
    year <- all_months$Year[i]
    month <- all_months$Month[i]
    
    sub_df <- subset(signal, Year == year & Month == month)
    if (nrow(sub_df) == 0) next  # skip missing months
    
    # Filter for Monday–Friday
    weekday_df <- subset(sub_df, Weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    
    # Identify peak periods
    high_peak <- subset(weekday_df, Hour >= 13 & Hour < 17)  # 1–5 PM
    low_peak <- subset(weekday_df, (Hour >= 10 & Hour < 13) | (Hour >= 17 & Hour < 20))  # 10–1, 5–8
    
    # Determine season and rates
    if (month %in% c(1, 2, 3, 4, 5, 10, 11, 12)) {
      season <- "Winter"
      high_rate <- 4.3
      low_rate <- 0
    } else if (month %in% c(6, 7, 8, 9)) {
      season <- "Summer"
      high_rate <- 9.7
      low_rate <- 3.3
    } else {
      season <- "Unknown"
      high_rate <- 0
      low_rate <- 0
    }
    
    # Peak values (kW)
    high_peak_val <- ifelse(nrow(high_peak) > 0, max(high_peak$Power, na.rm = TRUE), 0)
    low_peak_val  <- ifelse(nrow(low_peak)  > 0, max(low_peak$Power,  na.rm = TRUE), 0)
    
    # Compute charges for this month
    monthly_demand_low <- (low_peak_val  * 1000 * low_rate)
    monthly_demand_high <- (high_peak_val * 1000 * high_rate)
    monthly_cost <- (high_peak_val * 1000 * high_rate) + 
      (low_peak_val  * 1000 * low_rate)
    
    monthly_costs[[i]] <- data.frame(
      Year = year,
      Month = month,
      Season = season,
      High_Peak_kW = high_peak_val,
      Low_Peak_kW = low_peak_val,
      High_Rate = high_rate,
      Low_Rate = low_rate,
      Low_Cost = monthly_demand_low, 
      High_Cost = monthly_demand_high, 
      Monthly_Demand_Cost = monthly_cost
    )
  }
  
  result_df <- do.call(rbind, monthly_costs)
  result_df$Facility_Charge <- calculateFacilityCharge(signal)$Annual_Facility_Charge
  result_df$Total_Demand_Cost <- sum(result_df$Monthly_Cost)
  result_df$Total_Cost <- result_df$Total_Demand_Cost + result_df$Facility_Charge
  
  
  return(result_df)
}




plot_monthly_high_low <- function(oldPeriods, newPeriods, save_path = "plots/") {
  # Create folder if it does not exist
  if (!dir.exists(save_path)) dir.create(save_path)
  
  # Extract all (Year, Month) pairs that exist in the high-peak data
  months <- unique(format(oldPeriods$high_peak$Time, "%Y-%m"))
  
  for (m in months) {
    # Filter each subset for that month
    old_sub_H <- subset(oldPeriods$high_peak, format(Time, "%Y-%m") == m)
    new_sub_H <- subset(newPeriods$high_peak, format(Time, "%Y-%m") == m)
    
    old_sub_L <- subset(oldPeriods$low_peak, format(Time, "%Y-%m") == m)
    new_sub_L <- subset(newPeriods$low_peak, format(Time, "%Y-%m") == m)
    
    
    
    # Save PDF — file name like "high_peak_2024-03.pdf"
    pdf(file.path(save_path, paste0("high_peak_", m, ".pdf")), width=8, height=5)
    
    plot(
      old_sub_H$Time, old_sub_H$Power,
      type = "p",
      col = "red",
      pch = 20,
      ylab = "Power [MW]",
      xlab = "Time",
      ylim = c(0, max(c(old_sub_H$Power, new_sub_H$Power), na.rm = TRUE))
    )
    
    points(
      new_sub_H$Time, new_sub_H$Power,
      col = "green",
      pch = 20
    )
    
    title(paste("High Peak: ", m))
    
    dev.off()
    
    
    
    
    
    # Save PDF — file name like "high_peak_2024-03.pdf"
    pdf(file.path(save_path, paste0("low_peak_", m, ".pdf")), width=8, height=5)
    
    plot(
      old_sub_L$Time, old_sub_L$Power,
      type = "p",
      col = "red",
      pch = 20,
      ylab = "Power [MW]",
      xlab = "Time",
      ylim = c(0, max(c(old_sub_L$Power, new_sub_L$Power), na.rm = TRUE))
    )
    
    points(
      new_sub_L$Time, new_sub_L$Power,
      col = "green",
      pch = 20
    )
    
    title(paste("High Peak: ", m))
    
    dev.off()
  }
  
}



plot_max_peak_month <- function(signal, output_signal, save_path = "plots/") {
  
  # Ensure save directory exists
  if (!dir.exists(save_path)) dir.create(save_path, recursive = TRUE)
  
  # Convert Time to Date (if not already)
  signal$Time <- as.POSIXct(signal$Time)
  output_signal$Time <- as.POSIXct(output_signal$Time)
  
  # Extract month as "YYYY-MM"
  signal$Month <- format(signal$Time, "%Y-%m")
  
  # Compute monthly max power
  monthly_peaks <- aggregate(Power ~ Month, data = signal, max)
  
  # Find the month with maximum peak
  max_month <- monthly_peaks$Month[which.max(monthly_peaks$Power)]
  cat("Max peak month:", max_month, "\n")
  
  # Subset data for that month
  sig_sub <- subset(signal, format(Time, "%Y-%m") == max_month)
  out_sub <- subset(output_signal, format(Time, "%Y-%m") == max_month)
  
  # Create PDF
  pdf_file <- file.path(save_path, paste0("Max_Peak_Month_", max_month, ".pdf"))
  pdf(pdf_file, width = 10, height = 5)
  
  # Plot original signal
  plot(
    sig_sub$Time, sig_sub$Power,
    type = "p",
    col = "red",
    pch = 20,
    ylab = "Power [MW]",
    xlab = "Time",
    ylim = c(0, max(c(sig_sub$Power, out_sub$Power), na.rm = TRUE)),
    main = paste("Power Usage During Highest Peak Month:", max_month)
  )
  
  # Overlay output signal
  points(
    out_sub$Time, out_sub$Power,
    col = "green",
    pch = 20
  )
  
  dev.off()
  
  cat("Saved PDF:", pdf_file, "\n")
}




#------------------------------------------------------------------#
#------------------------------------------------------------------#








#--------------------------Run Here -------------------------------#
#------------------------------------------------------------------#


# Change to Desired Test Year 
file <- "2024, Oxy Energy Use.txt"



# Step 0: Read File and convert Time & Power

signal <- read.csv(file)
signal <- signal %>%
  mutate(
    Time = as.POSIXct(Time, format = "%m/%d/%y %H:%M"),  # convert to POSIXct
    Power = (College.Net.Use.kWh / 0.25) / 1000  # kWh per 15min → MW
  ) %>%
  select(Time, Power)  # remove the Solar.Production.kWh column

signal


# Step 1: Get daily EnergySum and Peak
daily_data <- getEnergySum(signal)
daily_data

# Step 2: Shift EnergySum forward one day (so cutoff for a day uses previous day’s energy)
daily_data <- daily_data %>%
  arrange(Date) %>%
  mutate(EnergySum_prev = lag(EnergySum, 1))
daily_data <- daily_data %>%
  arrange(Date) %>%
  mutate(Peak_prev = lag(Peak, 1))

daily_data

# Step 3: Predict cutoffs using facilityChargeModel & Demand charge Model
daily_data <- daily_data %>%
  mutate(facilityCutOff = facilityChargeModel(EnergySum_prev))
daily_data <- daily_data %>%
  mutate(demandCutOff = demandChargeModel(EnergySum_prev))
daily_data

# Step 4: Prepare cutoff table for test_model
modeled.facilityCutOff.table <- daily_data %>%
  select(Date, facilityCutOff)
modeled.demandCutOff.table <- daily_data %>%
  select(Date, demandCutOff)

modeled.facilityCutOff.table
modeled.demandCutOff.table


# Step 5: Run test_model with predicted cutoffs
output_signal <- test_model(modeled.facilityCutOff.table, signal)


# Step 6: Plot Original Signal vs Reduced Signal 
plot(signal$Time,signal$Power, ,type = "l",xlab = "Time", ylab = "Power (kW)", main = "Power vs Time", col = "red", ylim = c(0,3) )
lines(output_signal$Time, output_signal$Power, ,type = "l",xlab = "Time", ylab = "Power (kW)", main = "Power vs Time", col = "green", ylim = c(0,3) )


#Step 7: Plot Low and High Peak for Each Month 

oldHighLowPeriods <- high_low_Periods(signal)
newHighLowPeriods <- high_low_Periods(output_signal)


plot_monthly_high_low(oldHighLowPeriods, newHighLowPeriods)


save_path = "plots/"
if (!dir.exists(save_path)) dir.create(save_path)

pdf(file.path(save_path, paste0("Facility Charge.pdf")), width=8, height=5)

plot(
  signal$Time, signal$Power,
  type = "p",
  col = "red",
  pch = 20,
  ylab = "Power [MW]",
  xlab = "Time",
  ylim = c(0, max(c(signal$Power, output_signal$Power), na.rm = TRUE))
)

points(
  output_signal$Time, output_signal$Power,
  col = "green",
  pch = 20
)

dev.off()


plot_max_peak_month(signal, output_signal)





#Step 8: Calculate Demand + Facility Charges before and after reducing and find the saved amount
actualCost <- calculateCost(signal)

reducedCost <- calculateCost(output_signal)

actualCost
reducedCost

saved <- actualCost
saved$Low_Cost <- actualCost$Low_Cost - reducedCost$Low_Cost
saved$High_Cost <- actualCost$High_Cost - reducedCost$High_Cost

saved$Monthly_Demand_Cost <- actualCost$Monthly_Demand_Cost - reducedCost$Monthly_Demand_Cost
saved$Total_Demand_Cost <- actualCost$Total_Demand_Cost - reducedCost$Total_Demand_Cost
saved$Facility_Charge <- actualCost$Facility_Charge - reducedCost$Facility_Charge
saved$Total_Cost <- actualCost$Total_Cost - reducedCost$Total_Cost
saved





dollarsSaved <- saved$Total_Cost[1]


sum(saved$Low_Cost)
sum(saved$High_Cost)
saved$Facility_Charge[1]
dollarsSaved
dollarsSaved + sum(saved$High_Cost) + sum(saved$Low_Cost)


