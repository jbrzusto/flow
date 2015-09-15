#' Get the time range covered by a folder.
#'
#' Given the results of \code{scanFolder()}, return a vector of
#' timestamps giving ranges covered by the folder. A range indicates a
#' change in file and a time gap of at least 1 second, at least as
#' \code{scanFolder()} is currently written.
#' 
#' @param finfo: object returned by \code{scanFolder()}
#' 
#' @return a numeric vector of class \code{c("POSIXt", "POSIXct")} of
#' even length.  The odd-numbered entries give the start of a time range, the even-
#' numbered entries give the end of a time range.  Time ranges are separated by
#' at least 10 minutes without data.
#'  
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

timeRange = function(finfo) {
    maxf = length(environment(finfo)$filenames)
    tr = environment(environment(finfo)$tsToFileno)$x
    fno = environment(environment(finfo)$tsToFileno)$y

    ## environment(finfo)$tsToFileno is an approxfun mapping time
    ## to file number, which is an index in environment(finfo)$filenames
    
    ## Gaps in the coverage are indicated by ranges mapping to
    ## 1 + maxf as the file number.  Otherwise, ranges map to a
    ## valid file number.

    return (tr[fno <= maxf])
}
