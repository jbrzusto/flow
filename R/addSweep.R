#' Add a sweep of data to a flow estimator.
#'
#' Processes a new sweep of Cartesian radar reflectivity data, possibly estimating
#' currents.
#' 
#' @param F: an object of class Flow, as returned by \code{initFlow()}.
#'
#' @param m: a matrix of sweep data, with metadata attributes, as
#' returned by \code{toCart()}.  If this is the first sweep added to
#' \code{flow}, it defines the cell-sizes used for Fourier transforms
#' and estimation.  Otherwise, it must have the same dimensions as the
#' sweeps already added.  Sweeps must be added in chronological order,
#' and at constant time spacing (i.e. no missing sweeps are allowed).
#' 
#' @return boolean; TRUE if estimates of current are available, FALSE otherwise.
#' Once the function has accumulated enough data to return TRUE, it will continue to return
#' 
#' @note Requires that library RSQLite already be loaded.
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

addSweep = function(F, m) {
    if (!inherits(F, "Flow"))
        stop("first parameter must be an object of class 'Flow'")

    ## meta data
    meta = attr(m, "radar.meta")

    if (is.null(meta))
        stop("m is not a sweep object - it does not have a 'radar.meta' attribute")

    if (! is.null(F$radar.meta)) {
        if (!identical(F$radar.meta, meta))
            stop("sweep m was obtained in a different radar or digitizing mode than previous sweeps for this flow")
        if (F$sweepIndex == 1) {
            ## we've only added one sweep.  Now that we have a second, we know the timestep
            F$dt = meta$ts[1] - F$radar.meta$ts[1]
            F$dim = c(dim(m), ceiling(f$timeWin / F$dt))
            F$chunkDim = c(ceiling(dim(m) / F$res), F$dim[3])
            F$sweepMat = array(0, F$dim)
            F$sweepMat[,,1] = unclass(F$sweepMat)
            F$sweepMat[,,2] = unclass(m)
            F$sweepIndex = 3
            F$winMat = getWindow(F$winFun, F$dim, f$chunkDim)
        } else {
            F$sweepMat[,,F$sweepIndex] = unclass(m)
            F$sweepIndex = if (F$sweepIndex == F$dim[3]) 1 else 1 + F$sweepIndex
        }
    } else {
        ## first sweep to be added - record meta data and set up matrices
        F$radar.meta = meta
        F$sweepMat = m ## don't set up full array now - we don't know the time spacing yet
    }
    F$numSweeps = F$numSweeps + 1
    return(isTRUE(F$numSweeps >= F$dim[3]))
}

        

