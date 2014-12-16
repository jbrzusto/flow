#' Initialize a flow estimator.
#'
#' Generates an empty flow object with specified parameters.  Sweeps are
#' processed by calling \code{addSweep()}, and flow estimates are obtained
#' by calling \code{estFlow()}
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
#' @return an object of class "Flow".  The first call to \code{addSweep()} on this
#' object will establish further parameters such as matrix dimensions.
#' 
#' @note Requires that library RSQLite already be loaded.
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

initFlow = function(res, timeWin, winFun="hamming") {
    if (length(res) < 1 || length(res) > 2 || ! all(is.finite(res)) || any(res <= 0))
        stop("res must be one or two positive numbers")
    if (length(timeWin) != 1 || ! is.finite(timeWin) || timeWin < 0)
        stop("timeWin must be a positive number")
    if (! winFun %in% c("hamming", "hanning", "blackman"))
        stop('winFin must be "hamming", "hanning", or "blackman"')

    ## create a new object (environment, really) with
    ## the parameters as passed and a few other slots
    rv = new.env()
    rv$res = res
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
