# This function creates a function which you can call whatever you want.  See below.
# You feed this function the power (in MW) you want to charge or discharge the battery for the current 15 minute interval
# power > 0 => + => charge the battery
# power = 0 => 0 => leave the battery alone
# power < 0 => - => discharge the battery
# You must issue this command for EVERY 15 minutes
# The command affects the NEXT 15 minutes.
# In the final as in real life you will not know what the average power is for the next 15 minutes.

create_battery <- function( power_requested ) {
  battery_file <- "battery.txt" # this file is a record of all of the battery states
  write( c( "Status, Power Requested (MW)", "Power Delivered (MW)", "Energy (MWh)" ), file = battery_file, ncolumns = 3, sep = ",\t", append = FALSE )
  power_max <- 1 # in MW
  energy_max <- 2 # in MWh
  energy <- energy_max # battery starts off with max-energy
  function( power_requested ) {
    power <- power_requested
    OK <- 1
    if ( power_requested > power_max ) {
      power <- power_max
      OK <- 0
      # print( "Warning: power_requested > power_max!")
    }
    if ( power_requested < -power_max ) {
      power <- -power_max
      OK <- 0
      # print( "Warning: power_requested < -power_max!")
    }
    
    if ( ( energy + power * 0.25 ) >= energy_max ) {
      power_delivered <- ( energy_max - energy ) / 0.25
      energy <<- energy_max # filled up to max
      OK <- 0
      # print( "Warning: battery full!")
    } else if ( ( energy + power * 0.25 ) <= 0 ) {
      power_delivered <- ( 0.0 - energy ) / 0.25
      energy <<- 0 # battery is drained
      OK <- 0
      # print( "Warning: battery empty!")
    } else {
      power_delivered <- power
      energy <<- energy + power * 0.25 # '<<-' assigns to the variable 'energy' in the enclosing environment
    }
    
    #print( paste0( "Power Requested / Delivered = ", power_requested, "/", power_delivered, " MW" ) )
    
    write( c( OK, power_requested, power_delivered, energy ), file = battery_file, ncolumns = 4, sep = ",\t\t\t", append = TRUE )
    
    return( power_delivered )
  }
}

# To Use
# Do not comment out the next line!  To run select from the # to the end of the line and run
# source( '/Users/ifft/Documents/P395F25 - Directed Research/Battery.R' )
# You can uncomment the next 2 lines
# battery <- create_battery( 0 ) # This initializes everything and creates a function called battery
# battery( 1 ) # This is how you run the battery function.  This example would charge the battery at 1 MW.

