# This script will give you some hints about how to read data into R and R variables.  There are many ways
# to do this.  I will show you the most rudimentary way.  Your data, for this example, needs to be 
# in .csv (comma-separated-values) text format.  If you have data in Excel you can export it to a .csv file.
# This is just a text file where, as the name implies, all the data values are separated by commas.  It's worth
# taking a look with a text editor.  I've provided a file F0010CH1.CSV for you to look at.  You will notice that the
# top 18 lines are just header information.  We will want to skip those lines.  Scan is the easiest way to read in
# data from a text file

# Working directory

setwd( "C:/Users/wscar/Downloads/Files for distribution/Files for distribution/R Scripts" ) # Change for your computer and analysis directory

# Here's the basic move.  This assumes you have the file F0010CH1.CSV in your working directory

filename <- "F0010CH1.CSV"
scan( filename, skip = 1, sep = "," )

# What you see scan() doing here is opening the file, skipping 18 lines and then reading in doubles as separate values.

data.values <- scan( filename, skip = 1, sep = "," )

# Now the data is stored in an R variable.  If you looked at the original text file you will have seen two columns of
# data.  scan() reads left to right and top to bottom and just spits them out.

data.values[1:10]

# We now want to separate them back into columns.  matrix() is the ticket.

matrix( data.values, ncol = 2, byrow = TRUE )

mat <- matrix( data.values, ncol = 2, byrow = TRUE )

# Now the data that was in the text file is stored in a matrix in R called mat.  To access the values of a matrix
# in R use, in this example, mat[row number, column.number]

mat[1,1]
mat[1,2]

mat[10,1]

# Make sense?  Look back at the text file to insure you are understanding.
# mat[,1] refers to all data in column 1 and mat[,2] refers to all data in column 2.  So,

times <- mat[,1] # there is a function called time so I try to avoid using it as a variable name
voltage <- mat[,2]

plot( times, voltage, type = "n", pch = 20, cex = 0.01, xlab = "Time (S)", ylab = "Voltage (V)", main = "Voltage vs Time" )
lines( times, voltage, col = "green" )

# And you are off to the races

# I broke out the steps above for educational purposes but you can do all the reading in in one line like this

mat <- matrix( scan( filename, skip = 18, sep = "," ), ncol = 2, byrow = TRUE )

