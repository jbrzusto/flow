#' Initialize a flow estimator.
#'
#' Generates an empty flow object with specified parameters.  Sweeps are
#' processed by calling \code{addSweep()}, and flow estimates are obtained
#' by calling \code{estFlow()}
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
#' @param res: numeric scalar or pair, size of estimation cells in metres; if a pair,
#' then the first element is the x (east/west) dimension of the cell, and the second
#' element is the y (north/south) dimension.  One flow estimate is produced for each
#' cell for each timestep once the time window is filled.
#'
#' @param timeWin: numeric scalar, length of time window in
#' seconds. This, together with the sweep period, determines the fixed
#' number of consecutive sweeps used in the sliding estimation window.
#'
#' @param winFun: Fourier windowing function.  Choices are "hamming", "hanning", and
#' "blackman".
#' 
#' @param azires: real scalar; desired angular resolution from input
#' data, in degrees. If the sweep has pulses spaced more closely than desired, they are
#' subselected or averaged according to the \code{azimode} parameter.  If the sweep
#' has fewer pulses than required, they are replicated.  Default: 0.25 degrees
#'
#' @param azimode: character scalar "nearest" or "mean".  Determines how data are reduced
#' to the desired \code{azires}.  The default, "nearest", means the closest pulse to
#' the dsired output azimuth is used.  For "mean", input pulses are grouped with their
#' closest output azimuth, and averaged within these groups.  "median" applies the
#' same grouping as "mean", but chooses the median rather than average sample value
#' at each range slot.  FIXME: "mean" and "median" are not yet implemented.
#'
#' @return an object of class "Flow".  The first call to \code{addSweep()} on this
#' object will establish further parameters such as matrix dimensions.
#' 
#' @note Requires that library RSQLite already be loaded.
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

initFlow = function(xlim, ylim, res, timeWin, winFun=hamming, azires=0.25, azimode) {
    if (length(xlim) != 2 || diff(xlim) <= 0 || length(ylim) != 2 || diff(ylim) <= 0)
        stop("xlim and ylim must each be 2-element vectors with the first element smaller than the second")
    if (length(res) < 1 || length(res) > 2 || ! all(is.finite(res)) || any(res <= 0))
        stop("res must be one or two positive numbers")
    if (length(timeWin) != 1 || ! is.finite(timeWin) || timeWin < 0)
        stop("timeWin must be a positive number")

    ## create a new object (environment, really) with
    ## the parameters as passed and a few other slots
    rv = new.env()
    rv$xlim = xlim
    rv$ylim = ylim
    rv$res = res
    rv$azires = azires
    rv$timeWin = timeWin
    rv$winFun = winFun
    rv$numSweeps = 0
    rv$dt = NULL ## timestep between sweeps
    rv$sweepMat = NULL ## 3d sweep/time matrix (ring buffer along 3rd axis)
    rv$sweepIndex = 1 ## index along 3rd axis of sweepMat for location of next sweep
    rv$winMat = NULL ## windowing matrix for elementwise-multiplication of 3d sweep matrix
    rv$dim = NULL ## will be dimensions of 3d sweep/time matrix
    rv$chunkDim = NULL ## will be dimensions of chunks on which estimation occurs
    
    return(structure(rv, class="Flow"))
}
