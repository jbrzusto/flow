#' Find the start of a run of good sweeps of radar data.
#'
#' Look for a sequence of good consecutive sweeps in a specified time interval.
#'
#' @param ts: double vector of length 2; numeric timestamps (unclassed POSIXct) as \code{c(start, end)}
#' 
#' @param map: function mapping timestamps to SQLite files, as returned by \code{scanFolder}
#'
#' @param len: integer; minimum number of consecutive good sweeps
#'
#' @param minPulses: integer; minimum number of pulses for a good sweep
#'
#' @param aziRange: double vector of length 2; minimum covered range of azimuths for a good sweep,
#' as \code{c(low, high)}.  A sweep, \code{s}, is only "good" if its range of azimuths includes this range,
#' i.e. \code{min(s$azi) <= low && max(s$azi) >= high}
#' 
#' @param persist: logical; if \code{FALSE}, each call to \code{getSweep}
#' opens and closes an sqlite connection to the appropriate database
#' file.  Otherwise, the default, the function maintains a
#' connection to the database for the current sweep, storing it in
#' variable \code{con}, and its name in \code{dbfile}, in
#' \code{environment(map)}.  Subsequent calls to \code{getSweep} will use the
#' existing connection if appropriate, otherwise, will close it and replace
#' it with a new one.
#' 
#' @return scalar double timestamp of first pulse in first sweep of
#' run of consecutive good sweeps of requested length.  or \code{NULL}
#' if no such run was found.
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

findGoodSweeps = function(ts, map, len, minPulses, aziRange, persist = TRUE) {
    ts = as.numeric(ts)
    f = map(ts[1])
    if (is.na(f))
        return(NULL)
    env = environment(map)
    con = NULL

    ## check for an existing connection and re-use or close as appropriate
    if (!is.null(env$dbfile)) {
        if (persist && f == env$dbfile) {
            con = env$con
        } else {
            ## close connection, which is to different file
            dbDisconnect(env$con)
            env$con = NULL
            env$dbfile = NULL
        }
    }

    ## open a new connection if we didn't re-use a persistent one
    if (is.null(con)) {
        con = dbConnect("SQLite", f, flags=1L) ## open read-only
        if (persist) {
            ## retain for later use
            env$con = con
            env$dbfile = f
        }
    }

    ## find the appropriate start and end sweeps in this file
    swpStart = dbGetQuery(con, sprintf("select sweep_key from pulses where ts >= %.3f order by ts limit 1", ts[1]))[1, 1]
    swpEnd = dbGetQuery(con, sprintf("select sweep_key from pulses where ts <= %.3f order by ts desc limit 1", ts[2]))[1, 1]

    sweeps = dbGetQuery(con, sprintf("select min(ts) as ts, min(azi) as aziLow, max(azi) as aziHigh, count(*) as pulses from pulses where sweep_key >= %d and sweep_key <= %d group by sweep_key order by sweep_key", swpStart, swpEnd))

    runs = getRuns(with(sweeps, pulses >= minPulses & aziLow <= aziRange[1] & aziHigh >= aziRange[2]))

    ## close the connection, if not persisting
    if (!persist)
        dbDisconnect(con)

    if (length(runs$len) == 0)
        return (NULL)

    if (max(runs$len) < len)
        return (NULL)
    
    return (sweeps$ts[runs$start[which(runs$len >= len)[1]]])
}
