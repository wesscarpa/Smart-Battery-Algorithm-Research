#source( "/Users/ifft/Documents/Error Analysis/measurement 2.0.r")
'+.measurement' <- function( m1, m2 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" & class( m2 ) == "measurement" )
	{
		out$val <- m1$val + m2$val
		out$err <- sqrt( m1$err^2 + m2$err^2 )
		return( out )
	}
	if ( class( m1 ) == "measurement" & class( m2 ) == "numeric" )
	{
		out$val <- m1$val + m2
		out$err <- m1$err
		return( out )
	}
	if ( class( m1 ) == "numeric" & class( m2 ) == "measurement" )
	{
		out$val <- m1 + m2$val
		out$err <- m2$err
		return( out )
	}
}
'-.measurement' <- function( m1, m2 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" & class( m2 ) == "measurement" )
	{
		out$val <- m1$val - m2$val
		out$err <- sqrt( m1$err^2 + m2$err^2 )
		return( out )
	}
	if ( class( m1 ) == "measurement" & class( m2 ) == "numeric" )
	{
		out$val <- m1$val - m2
		out$err <- m1$err
		return( out )
	}
	if ( class( m1 ) == "numeric" & class( m2 ) == "measurement" )
	{
		out$val <- m1 - m2$val
		out$err <- m2$err
		return( out )
	}
}
'*.measurement' <- function( m1, m2 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" & class( m2 ) == "measurement" )
	{
		out$val <- m1$val * m2$val
		out$err <- ( m1$val * m2$val ) * sqrt( ( m1$err / m1$val )^2 + ( m2$err / m2$val )^2 )
		return( out )
	}
	if ( class( m1 ) == "measurement" & class( m2 ) == "numeric" )
	{
		out$val <- m1$val * m2
		out$err <- abs( m1$err * m2 )
		return( out )
	}
	if ( class( m1 ) == "numeric" & class( m2 ) == "measurement" )
	{
		out$val <- m1 * m2$val
		out$err <- abs( m1 * m2$err )
		return( out )
	}	
}
'/.measurement' <- function( m1, m2 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" & class( m2 ) == "measurement" )
	{
		out$val <- m1$val / m2$val
		out$err <- ( m1$val / m2$val ) * sqrt( ( m1$err / m1$val )^2 + ( m2$err / m2$val )^2 )
		return( out )
	}
	if ( class( m1 ) == "measurement" & class( m2 ) == "numeric" )
	{
		out$val <- m1$val / m2
		out$err <- abs( m1$err / m2 )
		return( out )
	}
	if ( class( m1 ) == "numeric" & class( m2 ) == "measurement" )
	{
		out$val <- m1 / m2$val
		out$err <- abs( m1 / m2$val ) * ( m2$err / m2$val )
		return( out )
	}	
}
'^.measurement' <- function( m1, m2 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" & class( m2 ) == "numeric" )
	{
		out$val <- m1$val^m2
		out$err <- abs( ( m1$val^m2 ) * m2 * ( m1$err / m1$val ) )
		return( out )
	}
}
'sqrt.measurement' <- function( m1 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" )
	{
		out$val <- m1$val^(1/2)
		out$err <- abs( ( m1$val^(1/2 ) ) * (1/2) * ( m1$err / m1$val ) )
		return( out )
	}
}
'exp.measurement' <- function( m1 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" )
	{
		out$val <- exp( m1$val )
		out$err <- abs( exp( m1$val ) * m1$err )
		return( out )
	}
}
'log.measurement' <- function( m1 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" )
	{
		out$val <- log( m1$val )
		out$err <- abs( ( 1 / m1$val ) * m1$err )
		return( out )
	}
}
'sin.measurement' <- function( m1 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" )
	{
		out$val <- sin( m1$val )
		out$err <- abs( cos( m1$val ) * m1$err )
		return( out )
	}
}
'cos.measurement' <- function( m1 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" )
	{
		out$val <- cos( m1$val )
		out$err <- abs( sin( m1$val ) * m1$err )
		return( out )
	}
}
'tan.measurement' <- function( m1 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" )
	{
		out$val <- tan( m1$val )
		out$err <- abs( ( 1 / cos( m1$val ) )^2 * m1$err )
		return( out )
	}
}
'sinh.measurement' <- function( m1 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" )
	{
		out$val <- sin( m1$val )
		out$err <- abs( cosh( m1$val ) * m1$err )
		return( out )
	}
}
'cosh.measurement' <- function( m1 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" )
	{
		out$val <- cos( m1$val )
		out$err <- abs( sinh( m1$val ) * m1$err )
		return( out )
	}
}
'tanh.measurement' <- function( m1 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" )
	{
		out$val <- tan( m1$val )
		out$err <- abs( ( 1 / cosh( m1$val ) )^2 * m1$err )
		return( out )
	}
}
'round.measurement' <- function( m1 )
{
	out <- list( val = 0, err = 0 )
	class( out ) <- 'measurement'
	if ( class( m1 ) == "measurement" )
	{
		half.issue <- log10( m1$err ) - floor( log10( m1$err ) ) > 0.146128 & log10( m1$err ) - floor( log10( m1$err ) ) < 0.20412 # in the half range
		if( !half.issue ) {
			out$err <- signif( m1$err, 1 )
			msd.err <- floor( log10( out$err ) ) # most significant decimal
			msd.val <- floor( log10( signif( m1$val, 1 ) ) ) # most significant decimal
			if( msd.val >= msd.err ) {
				out$val <- signif( m1$val, msd.val - msd.err + 1 )
			} else {
				out$val <- 0
			}
		} else {
			out$err <- signif( m1$err, 2 )
			msd.err <- floor( log10( out$err ) ) # most significant decimal
			msd.val <- floor( log10( signif( m1$val, 1 ) ) ) # most significant decimal
			if( msd.val >= msd.err ) {
				out$val <- signif( m1$val, msd.val - msd.err + 2 )
			} else {
				out$val <- 0
			}
		}
		return( out )
	}
}
'print.measurement' <- function( m1 )
{
	print( paste( m1$val, " +/- ", m1$err, sep = "" ), quote = F )
}
'plot.measurement' <- function( m1, m2, tick.fraction = 0.005, xlab = "x", ylab = "y", xlim = c( min( m1$val - m1$err ), max( m1$val + m1$err ) ), ylim = c( min( m2$val - m2$err ), max( m2$val + m2$err ) ), type = "p", ... )
{
	if ( class( m1 ) == "measurement" && class( m2 ) == "measurement" ) # Currently this is the only case that works.
	{
		plot( c( m1$val - m1$err, m1$val + m1$err ), c( m2$val - m2$err, m2$val + m2$err ), xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, type = "n", ... )
		if ( type != "n" )
		{
			points( m1$val, m2$val, col = par()$col, ... )
			segments( m1$val, m2$val - m2$err, m1$val, m2$val + m2$err, col = par()$col, ... )
			segments( m1$val - m1$err, m2$val, m1$val + m1$err, m2$val, col = par()$col, ... )
			x.distance <- par()$xaxp[2] - par()$xaxp[1]
			y.distance <- par()$yaxp[2] - par()$yaxp[1]
			segments( m1$val - x.distance * tick.fraction, m2$val + m2$err, m1$val + x.distance * tick.fraction, m2$val + m2$err, col = par()$col, ... )
			segments( m1$val - x.distance * tick.fraction, m2$val - m2$err, m1$val + x.distance * tick.fraction, m2$val - m2$err, col = par()$col, ... )
			segments( m1$val + m1$err, m2$val - y.distance * tick.fraction, m1$val + m1$err, m2$val + y.distance * tick.fraction, col = par()$col, ... )
			segments( m1$val - m1$err, m2$val - y.distance * tick.fraction, m1$val - m1$err, m2$val + y.distance * tick.fraction, col = par()$col, ... )
		}
	} else if ( class( m1 ) != "measurement" && class( m2 ) == "measurement" )
	{
		plot( c( m1$val - m1$err, m1$val + m1$err ), c( m2$val - m2$err, m2$val + m2$err ), xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, type = "n", ... )
		if ( type != "n" )
		{
			points( m1, m2$val, col = par()$col, ... )
			segments( m1, m2$val - m2$err, m1, m2$val + m2$err, col = par()$col, ... )
			x.distance <- par()$xaxp[2] - par()$xaxp[1]
			y.distance <- par()$yaxp[2] - par()$yaxp[1]
			segments( m1 - x.distance * tick.fraction, m2$val + m2$err, m1 + x.distance * tick.fraction, m2$val + m2$err, col = par()$col, ... )
			segments( m1 - x.distance * tick.fraction, m2$val - m2$err, m1 + x.distance * tick.fraction, m2$val - m2$err, col = par()$col, ... )
		}
	}
}
'points.measurement' <- function( m1, m2, tick.fraction = 0.005, ... )
{
	points( m1$val, m2$val, ... )
	segments( m1$val, m2$val - m2$err, m1$val, m2$val + m2$err, col = par()$col )
	segments( m1$val - m1$err, m2$val, m1$val + m1$err, m2$val, col = par()$col )
	x.distance <- par()$xaxp[2] - par()$xaxp[1]
	y.distance <- par()$yaxp[2] - par()$yaxp[1]

	segments( m1$val - x.distance * tick.fraction, m2$val + m2$err, m1$val + x.distance * tick.fraction, m2$val + m2$err, col = par()$col )
	segments( m1$val - x.distance * tick.fraction, m2$val - m2$err, m1$val + x.distance * tick.fraction, m2$val - m2$err, col = par()$col )
	segments( m1$val + m1$err, m2$val - y.distance * tick.fraction, m1$val + m1$err, m2$val + y.distance * tick.fraction, col = par()$col )
	segments( m1$val - m1$err, m2$val - y.distance * tick.fraction, m1$val - m1$err, m2$val + y.distance * tick.fraction, col = par()$col )
}
'points.measurement' <- function( m1, m2, tick.fraction = 0.005, ... )
{
	points( m1$val, m2$val, ... )
	segments( m1$val, m2$val - m2$err, m1$val, m2$val + m2$err, ... )
	segments( m1$val - m1$err, m2$val, m1$val + m1$err, m2$val, ... )
	x.distance <- par()$xaxp[2] - par()$xaxp[1]
	y.distance <- par()$yaxp[2] - par()$yaxp[1]

	segments( m1$val - x.distance * tick.fraction, m2$val + m2$err, m1$val + x.distance * tick.fraction, m2$val + m2$err, ... )
	segments( m1$val - x.distance * tick.fraction, m2$val - m2$err, m1$val + x.distance * tick.fraction, m2$val - m2$err, ... )
	segments( m1$val + m1$err, m2$val - y.distance * tick.fraction, m1$val + m1$err, m2$val + y.distance * tick.fraction, ... )
	segments( m1$val - m1$err, m2$val - y.distance * tick.fraction, m1$val - m1$err, m2$val + y.distance * tick.fraction, ... )
}
'lines.measurement' <- function( m1, m2, ... )
{
	lines( m1$val, m2$val, col = par()$col, ... )
}
measurement <- function( value, error )
{
	out <- list( val = value, err = error )
	class( out ) <- 'measurement'
	return( out )
}

