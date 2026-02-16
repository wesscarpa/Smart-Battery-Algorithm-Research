# One of the most powerful tools in R is the ability to make logical cuts on data.

# Working directory

setwd( "C:/Users/wscar/Downloads/Files for distribution/Files for distribution" ) # Change for your computer and analysis directory

# First a very quick lesson in logic in R

a <- 1
a > 0
a < 0

b <- 2
a > 0 & b > 0 # the AND operator
a > 1.5 & b > 1.5 # the AND operator again
a > 1.5 | b > 1.5 # the OR operator
!(a > 1.5) # the NOT operator

# Let's start with some "data"

times <- seq( from = 0, to = 2, by = 0.1 )
times
voltage <- times * 5 + 2
voltage <- voltage + rnorm( length( voltage ), sd = 0.5 )
plot( times, voltage )

# And now let's add in some wacko data

voltage[2] <- voltage[2] - 10
voltage[20] <- voltage[20] + 10
plot( times, voltage )

# and fit a straight line to the data including the wacko points, see "Fitting Straight Lines.R"

ls.fit <- lsfit( times, voltage )
abline( ls.fit )

# That's not a good fit to the bulk of the data because of the wacko points

voltage < 15

# You will notice that this is a TRUE/FALSE array equal in length to voltage.  Only one point, the upper
# wacko point, is false

voltage > -5 & voltage < 15

# This is FALSE for both points.  I like to call this array the standard cut, stdcut for short, array

stdcut <- voltage > -5 & voltage < 15

times[stdcut]
voltage[stdcut]

# If you look closely at these two arrays they do not contain the two wacko points
# Or you can plot it to check

plot( times[stdcut], voltage[stdcut] )

# Of course it is better to show the wacko points and then discuss why you cut them

plot( times, voltage ) # plot all of the data
ls.fit <- lsfit( times[stdcut], voltage[stdcut] ) # fit to only the "good" data
abline( ls.fit )

