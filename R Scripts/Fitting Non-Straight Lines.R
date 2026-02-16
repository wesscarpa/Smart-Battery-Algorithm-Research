# R can fit non straight lines.

# Working directory

setwd( "C:/Users/wscar/Downloads/Files for distribution/Files for distribution/R Scripts" ) # Change for your computer and analysis directory

# Load measurement.r.  See Using measurement.r .R to understand.

source( "C:/Users/wscar/Downloads/Files for distribution/Files for distribution/R Scripts/measurement.r" )

# There are two ways to fit non-straight lines depending on the situation you are trying to deal with.  If
# 
# y(x) = A * f1(x) + B * f2(x)
#
# where f1 and f2 are functions of x and A and B are the only free parameters then this is still considered
# "linear" fitting.  For contrast an example of non-linear fitting would be,
# 
# y(x) = A * f1(C * x) + B * f2(D * x)
#
# where A, B, C and D are free parameters.  Because C and D are not coefficients in front of f1 and f2 but
# are inside this is called a non-linear fit.
#
# R can handle both but with different routines.  Let's deal with linear fitting first.

# Let's start with some "data"

times <- seq( from = 0, to = 5, by = 0.1 )
times
voltage <- 2 + 3 * times^2 + 10 * sin( times ) # a crazy function but "linear" according the the above definition
voltage

# Let's check this out

plot( times, voltage )

# Let's add some theoretical noise to the voltage measurement

voltage <- voltage + rnorm( length( voltage ), sd = 0.5 )
plot( times, voltage )

# Interestingly this can still be handled by lsfit.  Instead of times we give it f1 and f2.  In a sense we did
# that the last time.  It is just that f1(x) was x.  This is how you handle this new situation.

lsfit( cbind( times^2, sin( times ) ), voltage )

# It's a sign of the times!  Get it sin( times ).  Nevermind.

# As you see the output of lsfit is a big list.  I always like to shove all that stuff into another variable.

ls.fit <- lsfit( cbind( times^2, sin( times ) ), voltage )

# And you can have R output a convenient table with all of the fit parameters using ls.print()

ls.print( ls.fit )

# You will notice that the table includes an intercept, a coefficient X1 and a coefficient X2.  X1 is the thing
# which multiplies f1 and X2 is the thing which multiplies f2.  We called them A and B above.  This should yield
# X1 = 3 and X2 = 10 within errors.

# If you want to store the information that comes out of ls.print

ls.out <- ls.print( ls.fit )
fit.numbers <- unclass( unlist( ls.out$coef.table ) )
fit.numbers

# To add a trendline you have to do it manually

x.fit <- seq( from = min( times ), to = max( times ), by = 0.01 )
y.fit <- fit.numbers[1] + fit.numbers[2] * x.fit^2 + fit.numbers[3] * sin( x.fit )
lines( x.fit, y.fit, col = "dark green" )

# Finally R can also fit non-linear functions with the nls() function

# Let's start with some fake data.  I will use a Gaussian as an example.  For details as to how I get this
# see "Making a Histogram-Frequency Plot-Bar Chart.R"

x.values <- rnorm( 1000, mean = 1, sd = 0.5 ) # Gaussian distribution with a mean of 1 and a standard deviation of 0.5

delta.x <- 0.1
hist( x.values, breaks = seq( from = min( x.values ) - delta.x, to = max( x.values ) + delta.x, by = delta.x ) ) # save the output
h <- hist( x.values, breaks = seq( from = min( x.values ) - delta.x, to = max( x.values ) + delta.x, by = delta.x ), plot = FALSE ) # save the output

# h$breaks is less useful since it will have one more data point than counts.  Let's have a look

points( h$mids, h$counts )

# or

x <- h$mids
y <- h$counts

plot( x, y )

# Now for the fit.  Notice that you enter the function as an equation with a ~ instead of an =.
# The function will have some free, fit, parameters.  In this case they are A, tau, omega, phi and C.
# And you have to give the nls function some values to get it started.

fit <- nls( y ~ A * exp( -( x - center )^2 / ( 2 * sigma^2 ) ), start = list( A = 80, center = 1, sigma = 0.5 ) )

# To see what the values for the fit variables, A, tau, omega, phi and C  and errors are use summary()

summary( fit )

# To extract the coefficients use

A <- coef(summary(fit))[1, "Estimate"]
center <- coef(summary(fit))[2, "Estimate"]
sigma <- coef(summary(fit))[3, "Estimate"]

A.err <- coef(summary(fit))[1, "Std. Error"]
center.err <- coef(summary(fit))[2, "Std. Error"]
sigma.err <- coef(summary(fit))[3, "Std. Error"]

# You should find A ~ 80, center = 1, sigma = 0.5 within errors since that is what was put
# into the data above.

measurement( A, A.err )
measurement( center, center.err )
measurement( sigma, sigma.err )

# Then the fit can be displayed

x.fit <- seq( from = min( x.values ) - delta.x, to = max( x.values ) + delta.x, by = delta.x / 10 )
y.fit <- A * exp( -( x.fit - center )^2 / ( 2 * sigma^2 ) )

hist( x.values, breaks = seq( from = min( x.values ) - delta.x, to = max( x.values ) + delta.x, by = delta.x ) ) # save the output
lines( x.fit, y.fit, col = "blue" )




