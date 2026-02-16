# This code assumes you are dealing with a .tif or .tiff file.  If not then just replace tiff with your image
# type and it should work the same.

# Working directory

setwd( "/Users/ifft/desktop/R Scripts" ) # Change for your computer and analysis directory

# You will need to load a library of functions called tiff.

library( tiff )

# Prior to doing this you may need to download and install the "tiff" package.  Use Packages Install for this.

img <- readTIFF("./Color Bars.tiff")
dim( img )

# The variable img is a gigantic matrix with first two dimensions telling you which pixel and the third dimension
# giving the three color components, red, green and blue with an intensity from 0 to 1.

# Extract the x and y size

x.size <- dim( img )[2] # Enter this manual.  Look at the specs for the photo
y.size <- dim( img )[1] # Enter this manual.  Look at the specs for the photo

# Strangely the first dimension tells you y and the second dimension tells you x.

# Now "plot" it

plot( 0, 0, xlim = c( 0, x.size ), ylim = c( 0, y.size ), type = "n" )
rasterImage( img, 0, 0, x.size, y.size ) # You may get a warning message.  Be patient.  There is a lot of data to handle.

# To get at one point 

xy <- locator(1) # R will wait for you to click on the graph somewhere returning the xy coordinates
this.x <- round( xy$x )
this.y <- round( y.size - xy$y ) # Note that the orgin for a picture is in the upper left!
img[ this.y, this.x, 1] # Red
img[ this.y, this.x, 2] # Green
img[ this.y, this.x, 3] # Blue

# So img[1,x.size,2] is the intensity in green of the upper right pixel.  Right?

# If you want to read off a line of pixel values in x just drop the x specification from the matrix

plot( 0, 0, xlim = c( 0, x.size ), ylim = c( 0, y.size ), type = "n" )
rasterImage( img, 0, 0, x.size, y.size ) # You may get a warning message.  Be patient.  There is a lot of data to handle.

xy <- locator(1) # R will wait for you to click on the graph somewhere returning the xy coordinates
this.y <- round( y.size - xy$y ) # We just want the y value this time
red.pixel.line <- img[ this.y, , 1] # Red
green.pixel.line <- img[ this.y, , 2] # Green
blue.pixel.line <- img[ this.y, , 3] # Blue

seq.pixel.line <- seq( red.pixel.line )

plot( seq.pixel.line, red.pixel.line, ylim = c( 0, 1 ), type = "n" ) # empty graph with the right dimensions
lines( seq.pixel.line, red.pixel.line, col = "red" )
lines( seq.pixel.line, green.pixel.line, col = "green" )
lines( seq.pixel.line, blue.pixel.line, col = "blue" )


