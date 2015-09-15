#' Round a timestamp up to a the nearest seconds, minutes, hours, or days.
#'
#' This is complementary to trunc.POSIXt, but operates as \code{ceiling()}
#' rather than as \code{floor()}
#'
#' @param ts: vector of POSIXt timestamps
#'  
#' @param units: character scalar; one of "secs", "mins", "hours", or "days".
#'  
#' @return a vector of timestamps, "rounded" up to a second, minute, hour,
#' or day boundary.
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

roundUpTS = function(ts, units) {
    tf = function(t) structure(- as.numeric(t), class=c("POSIXt", "POSIXct"))
    return (tf(trunc(tf(ts), units)))
}
