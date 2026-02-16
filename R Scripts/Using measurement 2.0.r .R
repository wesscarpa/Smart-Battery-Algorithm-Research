# This code will allow you to do error propagation easily.  

# Working directory

setwd( '/Users/ifft/Documents/P315F25/R Scripts' ) # Change for your computer and analysis directory

# R allows you to define new kinds of variables and to also define how they interaction with each other.  
# For instance R has a predefined variable for 
# complex numbers.  It knows how to multiply them properly.  Here's a simple example,

complex( 1, 1, 1 )
a <- complex( 1, 1, 1 )
b <- complex( 1, 1, -1 )
a * b

# I took advantage of this ability to define variables and how they interact to define a measurement variable.
# To get this to work you need to run measurement.r in the command window.

source( '/Users/ifft/Documents/Error Analysis/measurement 2.0.r' )

# You will need to replace the full pathname to point to whereever you have stored the file on your computer. 
# FYI "./measurement.r" would refer to where you started R or wherever getwd() is.  Either way the command above will
# "load" the measurement functions into your current working session.

# To make a measurement variable you run,

measurement( 1, 0.1 )

# This creates a variable whose value is 1 and whose associated error is 0.1.  Notice that like complex numbers
# it prints out in a nice way.  measurement.r knows how to multiply measurements.

a <- measurement( 1, 0.1 )
b <- measurement( 2, 0.1 )
a * b

# It can also add, subtract, divide and several other mathematical operations.  Have a look at measurement.r to see
# what it can and cannot do.  Any mathematical expression with those mathematical operations is fine.  It also
# understands operational order of precedence.

# If you use this enough you are going to have to understand just a little more about what a measurement variable is.
# Clearly it has two parts, a value and an error.  But these are some how stuffed into one thing, a in the example
# above.  In R one thing that contains many things is called a list.  To figure out what is in the list type,

unlist( a )

# You will see that a has two parts val and err for the value and the error of a.  To access either one use the $ sign.

a$val
a$err

# This is homebrew code with no guarantees.  I will allow you to use it, free of charge, under one condition,
# you give me feedback.

