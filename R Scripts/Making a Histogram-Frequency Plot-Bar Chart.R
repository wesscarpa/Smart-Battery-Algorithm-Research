# Some times you will need to make a histogram of your data.  First let's talk about the word
# histogram.  It may be unfamiliar to you.  It is also called a frequency plot or a bar chart.  Whatever
# you call it its purpose is to show you how frequently a particular value is found in a data set.
# For real numbers, doubles in computer speak, that is going to be close to zero so as a practical matter
# we seek the number of data points in bins of some width delta.x.  Honestly it is a bit of an art
# choosing the correct delta.x.  For a particular experiment you may want to consult your instructor
# to find the right binning.

# Working directory

setwd( "/Users/ifft/desktop/R Scripts" ) # Change for your computer and analysis directory

# Let's start with some "data"

x <- rnorm( 1000, mean = 1, sd = 0.5 ) # Gaussian distribution with a mean of 1 and a standard deviation of 0.5

plot( x ) # this is a cute trick in R.  It takes seq( x ) as the "x" value for the data.  Just a quick way to take a look

# The command in R to make a histogram is hist()

hist( x )

# R will choose a binning for you.  If you want to quickly change the binning you can use the nclass parameter

hist( x, nclass = 10 )

hist( x, nclass = 50 )

hist( x, nclass = 1000 )

# Generally you will want to control the binning with the breaks parameter

delta.x <- 0.1
hist( x, breaks = seq( from = min( x ) - delta.x, to = max( x ) + delta.x, by = delta.x ) )

# Make sure that breaks spans the range of x or R will not like it.

# Finally you will want to be able to access the values that come out of the binning process

hist( x, breaks = seq( from = min( x ) - delta.x, to = max( x ) + delta.x, by = delta.x ), plot = FALSE )

# You can add labels to a histogram in the same way you do for a plot.  See "Making Plots in R.R" for more details.
# In R a histogram is just a plot.

# The output is a structure with lots of different pieces.  $mids and $counts are probably the most useful

h <- hist( x, breaks = seq( from = min( x ) - delta.x, to = max( x ) + delta.x, by = delta.x ), plot = FALSE ) # save the output
h$mids
h$counts

# h$breaks is less useful since it will have one more data point than counts.  Let's have a look

points( h$mids, h$counts )

# or

plot( h$mids, h$counts )
