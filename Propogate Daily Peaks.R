allPowerData
allPowerData <- allPowerData[, !is.na(names(allPowerData))]



library(dplyr)
library(lubridate)
library(purrr)


propDailyPeaks <- function(data) {
  data %>%
    mutate(Date = as.Date(Time)) %>%             # extract date from timestamp
    group_by(Date) %>%                           # group by day
    summarise(
      Peak = max(Power, na.rm = TRUE),           # daily max
      EnergySum = sum(Power * 0.25, na.rm = TRUE) # daily energy
    ) %>%
    ungroup()
}

    
    
    
    
    
peakTable <- propDailyPeaks(allPowerData)
    
peakTable
names(data)

write.csv(peakTable, "PeakPower&EnergySum19_23", row.names = FALSE)
    
    
a <- read.csv("Avg.Temps.19_23")
b <- read.csv("optCutOff20_23")
c <- read.csv("PeakPower&EnergySum19_23")

merged <- list(a, b, c) %>%
  reduce(full_join, by = "Date")


merged <- merged %>%
  mutate(Month = as.numeric(format(as.Date(Date), "%m")))








merged$Date <- as.Date(merged$Date)


library(dplyr)




merged <- merged %>%
  left_join(optCutOff19, by = "Date", suffix = c("", ".new")) %>%  # add new Cutoff column
  mutate(Cutoff = ifelse(is.na(Cutoff), Cutoff.new, Cutoff)) %>%   # fill NAs from new data
  select(-Cutoff.new)                                               # drop the helper column

merged$DayOfWeekNum <- as.numeric(format(as.Date(merged$Date), "%u"))

merged$DayOfWeek <- NULL



merged$Peak_prev <- dplyr::lag(merged$Peak, 1)
merged$EnergySum_prev <- dplyr::lag(merged$EnergySum, 1)


merged


write.csv(merged, "model_data.csv", row.names = FALSE)



plot(merged$Peak_prev, merged$Cutoff, col = "blue", ylim = c(0,3), xlim = c(0,4), xlab = "Peak", ylab = "Optimal Cutoff (Daily) [MW]")
plot(merged$EnergySum_prev, merged$Cutoff, col = "blue", ylim = c(0,3), xlim = c(0, 80), xlab = "Energy Sum", ylab = "Optimal Cutoff (Daily) [MW]")

    
# Fit a simple linear regression model
model <- lm(Cutoff ~ EnergySum_prev, data = merged)

# Get R² from the model summary
summary(model)$r.squared


