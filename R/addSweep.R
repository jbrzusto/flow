#' Add a sweep of data to a flow estimator.
#'
#' Processes a new sweep of Cartesian radar reflectivity data, possibly estimating
#' currents.
#' 
#' @param F: an object of class Flow, as returned by \code{initFlow()}.
#'
#' @param s: a sweep of data, with metadata attributes, as returned by
#' \code{getSweep()}.  Sweeps must be added in chronological order,
#' and at constant time spacing (i.e. no missing sweeps are allowed).
#' 
#' @return boolean; TRUE if estimates of current are available, FALSE
#' otherwise.  Once the function has accumulated enough data to return
#' TRUE, it will continue to return
#' 
#' @note Requires that library RSQLite already be loaded.
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

addSweep = function(F, s) {
    if (!inherits(F, "Flow"))
        stop("first parameter must be an object of class 'Flow'")
    if (!inherits(s, "Sweep"))
        stop("first parameter must be an object of class 'Sweep'")

    m = toCart(s, F$xlim, F$ylim, attr(s, "radar.meta")$res, F$azires)
    ## meta data
    meta = attr(m, "radar.meta")

    if (! is.null(F$radar.meta)) {
        if (!identical(F$radar.meta[c("rate", "format", "ns", "scale")], meta[c("rate", "format", "ns", "scale")]))
            stop("sweep m was obtained in a different radar or digitizing mode than previous sweeps for this flow")
        if (F$sweepIndex == 1) {
            ## we've only added one sweep.  Now that we have a second, we know the timestep
            F$dt = meta$ts[1] - F$radar.meta$ts[1]
            F$dim = c(dim(m), ceiling(F$timeWin / F$dt))
            F$chunkDim = c(ceiling(dim(m) / F$res), F$dim[3])
            F$sweepMat = array(0, F$dim)
            F$sweepMat[,,1] = unclass(F$firstSweep)
            F$sweepMat[,,2] = unclass(m)
            F$sweepIndex = 3
            F$winMat = getWindow(F$winFun, F$dim, F$chunkDim)
        } else {
            F$sweepMat[,,F$sweepIndex] = unclass(m)
            F$sweepIndex = if (F$sweepIndex == F$dim[3]) 1 else 1 + F$sweepIndex
        }
    } else {
        ## first sweep to be added - record meta data and set up matrices
        F$radar.meta = meta
        F$firstSweep = m ## don't set up full array now - we don't know the time spacing yet
    }
    F$numSweeps = F$numSweeps + 1
    return(isTRUE(F$numSweeps >= F$dim[3]))
}

        

