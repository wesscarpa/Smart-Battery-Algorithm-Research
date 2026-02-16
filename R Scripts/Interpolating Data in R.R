# You can do almost anything you want as far as plotting data in R.  Just ask Google "How do I XXX in R".

# Below my intention is just to show you how to interpolate data.  It often happens that you are given
# discrete data points instead of a function for some process, say resistance as a function of temperature.
# It might be given to you in table format.  What if the temperature is between two of the data points.
# This happens frequently.  In order to estimate the resistance for a temperature which is not exactly
# the same as one in the table you need to interpolate.  The simplest way for this to occur is to choose the
# two nearest points, one with temperature below and one with temperature above, draw a straight line between
# them and use that to "guess" the resistance value.  A more sophisticated method is called using a spline.

# Below I will go over the simplest method which uses the R function approx().

# Working directory

setwd( "/Users/ifft/Documents/P315F22/R Scripts" ) # Change for your computer and analysis directory

# Load measurement.r.  See Using measurement.r .R to understand.

source( "/users/ifft/documents/measurement.r" )

# Let's start with some "data"

temperature <- seq( from = 0, to = 100, by = 10 )
temperature
resistance <- temperature^0.5
resistance

# Plot the data

plot( temperature, resistance, xlab = "Temperature (C)", ylab = "Resistance (Ohms)", main = "Resistance vs Temperature" )
lines( temperature, resistance )

# You will notice there is no data at T = 5

cbind( temperature, resistance )

# To get that you can use approx()

this.temperature <- 5
approx( temperature, resistance, this.temperature )

# You will notice that approx gives you back your point and the interpolated point.  approx() actually
# returns a structure.  You can access the individual points using $x or $y.

output <- approx( temperature, resistance, this.temperature )
output$x
output$y

# I often just force approx to give me what I want

this.resistance <- approx( temperature, resistance, this.temperature )$y
this.resistance

# Now we can plot it

points( this.temperature, this.resistance, col = "red" )

# As usual this.temperature can just as easily be an array

this.temperature <- seq( from = 15, to = 100, by = 10 )
this.temperature

# I often just force approx to give me what I want

this.resistance <- approx( temperature, resistance, this.temperature )$y
this.resistance
points( this.temperature, this.resistance, col = "red" )

# Warning.  approx() gets very unhappy if the points are not contained within the range of x.  It won't extrapolate
# beyond the ends of the table.

