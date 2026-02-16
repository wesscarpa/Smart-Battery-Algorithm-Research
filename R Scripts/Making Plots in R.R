# You can do almost anything you want as far as plotting data in R.  Just ask Google "How do I XXX in R".
# Below my intention is just to show you some of the basic plot commands

# Working directory

setwd( "C:/Users/wscar/Downloads/Files for distribution/Files for distribution/R Scripts" ) # Change for your computer and analysis directory

# Load measurement.r.  See Using measurement.r .R to understand.

source( "C:/Users/wscar/Downloads/Files for distribution/Files for distribution/R Scripts/measurement.r" )

# Let's start with some "data"

times <- seq( from = 0, to = 2, by = 0.1 )
times
voltage <- sqrt( times )
voltage

# The basic plot command is just

plot( times, voltage )

# add titles
plot( times, voltage, xlab = "Time (S)", ylab = "Voltage (V)", main = "Voltage vs Time" )

# change axes limits if you want
plot( times, voltage, xlim = c( -1, 3 ), ylim = c( -1, 3 ), xlab = "Time (S)", ylab = "Voltage (V)", main = "Voltage vs Time" )

# draw small points instead of the default 0s
plot( times, voltage, pch = 20, cex = 0.5, xlim = c( -1, 3 ), ylim = c( -1, 3 ), xlab = "Time (S)", ylab = "Voltage (V)", main = "Voltage vs Time" )

# don't draw the points, then draw a line
plot( times, voltage, type = "n", pch = 20, cex = 0.5, xlim = c( -1, 3 ), ylim = c( -1, 3 ), xlab = "Time (S)", ylab = "Voltage (V)", main = "Voltage vs Time" )
lines( times, voltage )

# color is always nice
plot( times, voltage, type = "n", pch = 20, cex = 0.5, xlim = c( -1, 3 ), ylim = c( -1, 3 ), xlab = "Time (S)", ylab = "Voltage (V)", main = "Voltage vs Time" )
lines( times, voltage, col = "green" )

# one can always add other lines to the plot

times2 <- seq( from = 0, to = 2, by = 0.05 )
voltage2 <- times2

plot( times, voltage, type = "n", pch = 20, cex = 0.5, xlim = c( -1, 3 ), ylim = c( -1, 3 ), xlab = "Time (S)", ylab = "Voltage (V)", main = "Voltage vs Time" )
lines( times, voltage, col = "green" )
lines( times2, voltage2, col = "red" )

# You can show the points with points()

plot( times, voltage, type = "n", pch = 20, cex = 0.5, xlim = c( -1, 3 ), ylim = c( -1, 3 ), xlab = "Time (S)", ylab = "Voltage (V)", main = "Voltage vs Time" )
lines( times, voltage, col = "green" )
points( times, voltage, pch = 20, cex = 0.5, col = "green" )
lines( times2, voltage2, col = "red" )
points( times2, voltage2, pch = 20, cex = 0.5, col = "red" )

# sometimes it is helpful to add text to the plot

plot( times, voltage, type = "n", pch = 20, cex = 0.5, xlim = c( -1, 3 ), ylim = c( -1, 3 ), xlab = "Time (S)", ylab = "Voltage (V)", main = "Voltage vs Time" )
lines( times, voltage, col = "green" )
points( times, voltage, pch = 20, cex = 0.5, col = "green" )
text( times[length(times)], voltage[length(voltage)], "Voltage", pos = 4 )
lines( times2, voltage2, col = "red" )
points( times2, voltage2, pch = 20, cex = 0.5, col = "red" )
text( times2[length(times2)], voltage2[length(voltage2)], "Voltage2", pos = 4 )

# you can easily add lines to your plot

plot( times, voltage, type = "n", pch = 20, cex = 0.5, xlim = c( -1, 3 ), ylim = c( -1, 3 ), xlab = "Time (S)", ylab = "Voltage (V)", main = "Voltage vs Time" )
lines( times, voltage, col = "green" )
points( times, voltage, pch = 20, cex = 0.5, col = "green" )
text( times[length(times)], voltage[length(voltage)], "Voltage", pos = 4 )
lines( times2, voltage2, col = "red" )
points( times2, voltage2, pch = 20, cex = 0.5, col = "red" )
text( times2[length(times2)], voltage2[length(voltage2)], "Voltage2", pos = 4 )
abline( h = 1, col = "purple")
abline( v = 1, col = "purple")

# legends are always nice

plot( times, voltage, type = "n", pch = 20, cex = 0.5, xlim = c( -1, 3 ), ylim = c( -1, 3 ), xlab = "Time (S)", ylab = "Voltage (V)", main = "Voltage vs Time" )
lines( times, voltage, col = "green" )
points( times, voltage, pch = 20, cex = 0.5, col = "green" )
lines( times2, voltage2, col = "red" )
points( times2, voltage2, pch = 20, cex = 0.5, col = "red" )
legend( "topright", legend = c( "Voltage", "Voltage2" ), pch = 20, col = c( "green", "red" ) )

# measurement.r also has a rudimentary way of adding errors to the plot

times <- measurement( seq( from = 0, to = 2, by = 0.1 ), 0 ) # now times is a measurement array with 0 error
times
voltage <- measurement( sqrt( times$val ), 0.1 ) # now voltage is a measurement array with 0.1 error
voltage

plot( times, voltage, pch = 20, cex = 0.5, xlim = c( -1, 3 ), ylim = c( -1, 3 ), xlab = "Time (S)", ylab = "Voltage (V)", main = "Voltage vs Time" )


