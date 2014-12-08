#' Convert a sweep to Cartesian coordinates.
#'
#' From a sweep of radar data as returned by \code{getSweep()}, use
#' bicubic spline interpolation on the polar coordinate side to
#' generate a Cartesian raster of radar reflection intensities.
#'
#' @param sweep: raw sweep object, as returned by \code{getSweep()}
#' 
#' @param xlim: 2-element real vector; range of east/west coordinates in metres.
#' \code{xlim[1]} is the minimum and \code{xlim[2]} is the maximum.
#' These are offsets relative to the radar's x position. Negative is west, positive
#' is east.
#' 
#' @param ylim: 2-element real vector; range of north/south coordinates in metres.
#' \code{ylim[1]} is the minimum and \code{ylim[2]} is the maximum.
#' These are offsets relative to the radar's y position. Negative is south, positive
#' is north.
#'
#' @param res: real scalar; desired resolution for output grid, in metres.  This
#' is used for both x and y directions.  Default: 3.6 m
#' 
#' @param azires: real scalar; desired angular resolution from input
#' data, in degrees. If the sweep has pulses spaced more closely than desired, they are
#' subselected or averaged according to the \code{mode} parameter.  If the sweep
#' has fewer pulses than required, they are replicated.  Default: 0.25 degrees
#'
#' @param azimode: character scalar "nearest" or "mean".  Determines how data are reduced
#' to the desired \code{azires}.  The default, "nearest", means the closest pulse to
#' the dsired output azimuth is used.  For "mean", input pulses are grouped with their
#' closest output azimuth, and averaged within these groups.  "median" applies the
#' same grouping as "mean", but chooses the median rather than average sample value
#' at each range slot.  FIXME: "mean" and "median" are not yet implemented.
#'
#' @param bkgd: numeric value to return in portion of matrix outside of radar data.
#' Default: 0.
#' 
#' @return numeric matrix; dimensions are \code{ceil(diff(range(xlim)) / res} columns
#' by \code{ceil(diff(range(ylim)) / res} rows.  Values are interpolated data values
#' within the range of radar data, and \code{bkgd} outside there.
#' 
#' @note Requires that library akima already be loaded.
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}
##

toCart = function(s, xlim, ylim, res=3.6, azires = 0.25, azimode="nearest", bkgd=0) {

    ## desired azimuths
    theta = seq(from = 0, to = 1, by = azires / 360)

    ## number of samples per pulse
    ns = attr(s, "ns")

    ## input range cell size, in metres
    ires = 2.99792458E8 / (2 * attr(s, "rate"))

    ## values of input range
    range = ires * (seq(from = 0.5, to = ns - 0.5, by = 1))
    
    ## subselect / replicate the input data according to azimuth mode
    ## this generates the matrix d with appropriate dimensions This
    ## clause must create variables np (number of pulses) and d (data
    ## matrix, with dimensions c(ns, np)), and azi (azimuths of pulses
    # in d.
    
    if (azimode == "nearest") {
        
        ## the best pulse for each azimuth
        use = floor(approx(s$azi, 1:nrow(s), theta, method="constant", f=0.5)$y)

        ## drop out-of-range portions
        use = use[! is.na(use)]

        np = length(use)
        
        d = matrix(readBin(unlist(s$samples[use]), integer(), size=2, signed=FALSE, n=np * ns), ns, np)

        azi = s$azi[use]
        
    } else {
        stop("FIXME: values for azimode other than 'nearest' not yet implemented")
    }

    ## we now have a matrix d with raw values for ns samples in each of np pulses,
    ## where pulse azimuths are in azi and range cell centres are in range

    ## set up the output grid locations, relative to radar location

    xgrid = seq(from = xlim[1], to = xlim[2], by = res)
    ygrid = seq(from = ylim[1], to = ylim[2], by = res)

    ## output matrix size
    nx = length(xgrid)
    ny = length(ygrid)

    ## replicate the output coordinates for each valid point, varying x faster
    xout = rep(xgrid, times = ny)
    yout = rep(ygrid, each = nx)

    ## range of all desired output points
    rangeout = sqrt(xout^2 + yout^2)
    
    ## azimuth of all desired output points on scale of 0..1
    aziout = ((pi / 2 + 136.8 * (pi / 180) - atan2(yout, xout)) %% (2 * pi)) / (2 * pi)

    keep = which(rangeout <= max(range) & aziout >= azi[1] & aziout <= tail(azi,1))

    ## interpolate
    z = bicubic(range, azi, d, rangeout[keep], aziout[keep])$z

    rv = matrix(bkgd, nx, ny)
    rv[keep] = z

    return(rv)
}
