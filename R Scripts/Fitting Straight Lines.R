# R can fit straight lines but it can also do fits for more complicated functions.

# Working directory

setwd( "C:/Users/wscar/Downloads/Files for distribution/Files for distribution/R Scripts" ) # Change for your computer and analysis directory

# Load measurement.r.  See Using measurement.r .R to understand.

source( "C:/Users/wscar/Downloads/Files for distribution/Files for distribution/R Scripts/measurement.r" )

# Let's start with some "data"

times <- seq( from = 0, to = 2, by = 0.1 )
times
voltage <- times * 5 + 2
voltage

# Let's check this is a straight line

plot( times, voltage )

# Let's add some theoretical noise to the voltage measurement

voltage <- voltage + rnorm( length( voltage ), sd = 0.5 )
plot( times, voltage )

# The simplest fitting routine in R is lsfit()

lsfit( times, voltage )

# As you see the output of lsfit is a big list.  I always like to shove all that stuff into another variable.

ls.fit <- lsfit( times, voltage )

# Now you can conveniently add the fit line with

abline( ls.fit )

# And you can have R output a convenient table with all of the fit parameters using ls.print()

ls.print( ls.fit )

# Maybe that's all you need.  If you want to store the information that comes out of ls.print

ls.out <- ls.print( ls.fit )
fit.numbers <- unclass( unlist( ls.out$coef.table ) )
fit.numbers
b <- measurement( fit.numbers[1], fit.numbers[3] ) # intercept
b
m <- measurement( fit.numbers[2], fit.numbers[4] ) # slope
m

# b and m should be within errors what we set above 2 and 5
