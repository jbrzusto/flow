#' Get runs of TRUE in a logical vector.
#'
#' Find (maximal) runs of consecutive TRUE values in a logical vector
#' and return the start index and length of each one.
#' 
#' @param x: a logical vector
#'
#' @return a list with these items:
#' \enumerate{
#' \item start: integer vector; start index of the ith run of
#' consecutive {TRUE} values in \code{x}.
#' \item len: integer vector; length of the ith run of consecutive
#' {TRUE} values in \code{x}.
#' }
#' Both items have length zero if \code{any(x) == FALSE}.

#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

getRuns = function(x) {
    start = which(diff(c(FALSE, x)) ==  1L)
    end   = which(diff(c(x, FALSE)) == -1L)
    return (list(start=start, len = end - start + 1L))
}
