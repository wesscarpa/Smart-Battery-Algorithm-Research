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
'print.measurement' <- function( m1 )
{
	print( paste( m1$val, " +/- ", m1$err, sep = "" ), quote = F )
}
'plot.measurement' <- function( m1, m2, tick.fraction = 0.2, xlab = "x", ylab = "y", type = "p", ... )
{
	plot( c( m1$val - m1$err, m1$val + m1$err ), c( m2$val - m2$err, m2$val + m2$err ), xlab = xlab, ylab = ylab, type = "n", ... )
	if ( type != "n" )
	{
		plot( c( m1$val - m1$err, m1$val + m1$err ), c( m2$val - m2$err, m2$val + m2$err ), xlab = xlab, ylab = ylab, type = "n", ... )
		points( m1$val, m2$val, ... )
		segments( m1$val, m2$val - m2$err, m1$val, m2$val + m2$err )
		segments( m1$val - m1$err, m2$val, m1$val + m1$err, m2$val )
		x.distance <- par()$xaxp[2] - par()$xaxp[1]
		y.distance <- par()$yaxp[2] - par()$yaxp[1]
		segments( m1$val - m2$err * ( x.distance / y.distance ) * tick.fraction, m2$val + m2$err, m1$val + m2$err * ( x.distance / y.distance ) * tick.fraction, m2$val + m2$err )
		segments( m1$val - m2$err * ( x.distance / y.distance ) * tick.fraction, m2$val - m2$err, m1$val + m2$err * ( x.distance / y.distance ) * tick.fraction, m2$val - m2$err )
		segments( m1$val + m1$err, m2$val - m1$err * ( y.distance / x.distance ) * tick.fraction, m1$val + m1$err, m2$val + m1$err * ( y.distance / x.distance ) * tick.fraction )
		segments( m1$val - m1$err, m2$val - m1$err * ( y.distance / x.distance ) * tick.fraction, m1$val - m1$err, m2$val + m1$err * ( y.distance / x.distance ) * tick.fraction )
	}
}
'points.measurement' <- function( m1, m2, tick.fraction = 0.2, ... )
{
	points( m1$val, m2$val, ... )
	segments( m1$val, m2$val - m2$err, m1$val, m2$val + m2$err, ... )
	segments( m1$val - m1$err, m2$val, m1$val + m1$err, m2$val, ... )
	x.distance <- par()$xaxp[2] - par()$xaxp[1]
	y.distance <- par()$yaxp[2] - par()$yaxp[1]
	segments( m1$val - m2$err * ( x.distance / y.distance ) * tick.fraction, m2$val + m2$err, m1$val + m2$err * ( x.distance / y.distance ) * tick.fraction, m2$val + m2$err, ... )
	segments( m1$val - m2$err * ( x.distance / y.distance ) * tick.fraction, m2$val - m2$err, m1$val + m2$err * ( x.distance / y.distance ) * tick.fraction, m2$val - m2$err, ... )
	segments( m1$val + m1$err, m2$val - m1$err * ( y.distance / x.distance ) * tick.fraction, m1$val + m1$err, m2$val + m1$err * ( y.distance / x.distance ) * tick.fraction, ... )
	segments( m1$val - m1$err, m2$val - m1$err * ( y.distance / x.distance ) * tick.fraction, m1$val - m1$err, m2$val + m1$err * ( y.distance / x.distance ) * tick.fraction, ... )
}
'lines.measurement' <- function( m1, m2, ... )
{
	lines( m1$val, m2$val, ... )
}
measurement <- function( value, error )
{
	out <- list( val = value, err = error )
	class( out ) <- 'measurement'
	return( out )
}

