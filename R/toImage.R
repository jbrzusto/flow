#' Convert a sweep to a Cartesian image (rectangle of pixel values).
#'
#' From a sweep of radar data as returned by \code{getSweep()}, use
#' bicubic spline interpolation on the polar coordinate side to
#' generate a Cartesian raster of radar reflection intensities.
#' 
#' @param sweep: raw sweep object, as returned by \code{getSweep()}
#'
#' @param pix: nativeRaster 4-channel matrix; should be NULL on first call
#' with given parameters, then can use return value on subsequent calls
#' 
#' @param xlim: 2-element real vector; range of east/west coordinates
#' in metres.  \code{xlim[1]} is the minimum and \code{xlim[2]} is the
#' maximum.  These are offsets relative to the radar's x
#' position. Negative is west, positive is east.
#' 
#' @param ylim: 2-element real vector; range of north/south
#' coordinates in metres.  \code{ylim[1]} is the minimum and
#' \code{ylim[2]} is the maximum.  These are offsets relative to the
#' radar's y position. Negative is south, positive is north.
#'
#' @param res: real scalar; desired resolution for output grid, in
#' metres.  This is used for both x and y directions.  Default: 3.6 m
#' 
#' @param azires: real scalar; desired angular resolution from input
#' data, in degrees. If the sweep has pulses spaced more closely than
#' desired, they are subselected or averaged according to the
#' \code{mode} parameter.  If the sweep has fewer pulses than
#' required, they are replicated.  Default: 0.25 degrees
#'
#' @param bkgd: pixel value to return in portion of matrix outside of radar data.
#' Default: 0.
#'
#' @param black: sample value corresponding to black, in output image
#' 
#' @param white: sample value corresponding to white, in output image
#'
#' @param palette: integer vector of RGBA colours for palette
#' 
#' @param aziOffset: azimuth offset of heading pulse, clockwise from North.
#'
#' @param cache: environment.  If not \code{NULL}, this is a place to
#' store indexes between repeated calls to \code{toCart} with the same
#' parameter values but different sweep data.  On the first call for a
#' sequence of sweeps, pass a variable which has been assigned a new
#' (empty) environment, e.g. \code{cc <- new.env(); x=toCart(...,
#' cache=cc)}, and then pass the same variable for cache on subsequent
#' calls.  
#' 
#' @return integer matrix; dimensions are \code{ceil(diff(range(ylim))
#' / res} rows by \code{ceil(diff(range(xlim)) / res} columns.  Values
#' are pixels (RGBA).  Each column is a strip of image going
#' from North to South, and the matrix consists of data columns going
#' from West to East.
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}
##

toImage = function(s, pix = NULL, xlim, ylim, res=3.6, azires = 0.25, bkgd=0, black = 6100, white = 16383, palette=radarImagePalette, aziOffset = 136.8, cache=NULL) {
    ## number of pulses
    np = nrow(s)
    
    ## meta data
    meta = attr(s, "radar.meta")

    ## decimation rate (knowing the full rate is 125MHz)
    decim = 125e6 / meta$rate
    
    ## desired azimuths
    theta = seq(from = s$azi[1], to = s$azi[nrow(s)], by = azires / 360)

    keep = approx(s$azi, 1:nrow(s), theta, method="constant", rule=2)$y
    
    samples = unlist(s$samples[keep]) ## get samples only for pulses of interest

    ## number of samples per pulse
    ns = meta$ns
    
    ## pixels per metre:
    ppm = 1.0 / res

    ## metres per sample
    VELOCITY_OF_LIGHT = 2.99792458E8

    mps = VELOCITY_OF_LIGHT / meta$rate / 2.0

    iwidth = round(diff(xlim) * ppm)
    iheight = round(diff(ylim) * ppm)

    if (is.null(pix) || storage.mode(pix) != "integer" || ! is.matrix(pix) || ! inherits(pix, "nativeRaster") || iwidth != ncol(pix) || iheight != nrow(pix)) {
        pix = matrix(0L, iheight, iwidth)
        class(pix)="nativeRaster"
        attr(pix, "channels") = 4
    }

    if (is.null(cache) || ! exists("scanConv", cache)) {
        scanConv = .Call("make_scan_converter", as.integer(c(length(theta), ns, iwidth, iheight, 0, 0, iwidth, ylim[2] * ppm, TRUE)), c(ppm * mps, 0, (aziOffset-90)/360+theta[1], (aziOffset-90)/360+tail(theta,1)))
    } else {
        scanConv = cache$scanConv
    }
    .Call("apply_scan_converter", scanConv, samples, pix, palette, as.integer(c(iwidth, black * decim, 0.5 + (white - black) * decim / 255)))

    return(pix)
}
