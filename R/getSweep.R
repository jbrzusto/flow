#' Get a sweep of radar data.
#'
#' Given a timestamp and a function mapping timestamps to sqlite filenames,
#' returns a data.frame with full raw radar data for the sweep containing the
#' timestamp.
#'
#' @param ts: scalar, numeric timestamp (unclassed POSIXct)
#' 
#' @param map: function mapping timestamps to SQLite files, as returned by \code{scanFolder}
#'
#' @param data: logical; if TRUE, the default, returns full sweep data; otherwise, returns
#' only metadata.
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
#' @return NULL, if \code{map} does not include \code{ts}, otherwise a data.frame with
#' these columns:
#' \enumerate{
#' \item ts double, timestamp for start of pulse
#' \item trigs integer, trigger count, for detecting dropped pulses
#' \item trig_clock integer, for accurate timing since start of sweep
#' \item azi float, azimuth of pulse, relative to start of heading pulse, (0..1)
#' \item elev float, elevation angle (radians)
#' \item rot float, rotation of waveguide (polarization - radians)
#' \item  samples raw vector, digitized samples for each pulse
#' }
#' The data.frame will also have these attributes:
#' \enumerate{
#' \item rate, digitizing rate in Hz
#' \item format, number of bits per sample (lower 8 bits);
#' higher order bits are flags: bit 8 = 1 means
#' samples bits are packed across bytes.  Otherwise,
#' (when bit 8 is 0), each sample occupies a whole number of bytes.
#' \item ns, number of samples per pulse
#' \item scale, maximum possible value of sample.
#' \item heading, angle of heading pulse (azimuth=0) in degrees clockwise from North
#' }
#' 
#' @note Requires that library RSQLite already be loaded.
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

getSweep = function(ts, map, data = TRUE, persist = TRUE) {
    ts = as.numeric(ts)
    f = map(ts)
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
        con = dbConnect(RSQLite::SQLite(), f, flags=1L) ## open read-only
        if (persist) {
            ## retain for later use
            env$con = con
            env$dbfile = f
        }
    }

    ## find the appropriate sweep in this file
    swp = dbGetQuery(con, sprintf("select sweep_key from pulses where ts >= %.3f order by ts limit 1", ts))[1, 1]

    if (data) {
        ## get all pulses from this sweep
        rv = dbGetQuery(con, sprintf("select ts,trigs,trig_clock,azi,elev,rot,samples from pulses where sweep_key = %d order by ts", swp))
        ## get metadata for this sweep
        mode = dbGetQuery(con, sprintf("select mode_key from pulses where sweep_key=%d order by ts limit 1", swp))[1, 1]
        params = unclass(dbGetQuery(con, sprintf("select t2.rate, t2.format, t2.ns, t2.scale from modes as t1 join digitize_modes as t2 on t1.digitize_mode_key = t2.digitize_mode_key where t1.mode_key=%d", mode)))
        
        ## get latest (but preceding) position / orientation value from geo table.
        ## FIXME: Interpolate.  (doesn't matter for fixed site).
        
        params = c(params, unclass(dbGetQuery(con, sprintf("select lat, lon, alt, heading from geo where ts <= %f order by ts desc limit 1", ts))))
        params$res = 2.99792458E8 / (2 * params$rate)    ## input range cell size, in metres
        
        attr(rv, "radar.meta") = params
        class(rv) = c(class(rv), "Sweep")
    } else {
        ## get summary of data from this sweep
        rv = dbGetQuery(con, sprintf("select min(ts) as tsStart,max(ts) as tsEnd, min(azi) as aziStart, max(azi) as aziEnd, max(trigs) - min(trigs) + 1 as trigs, count(*) as pulses from pulses where sweep_key = %d", swp))
        ## get metadata for this sweep
        class(rv) = c(class(rv), "SweepMeta")
    }        


    ## close the connection, if not persisting
    if (!persist)
        dbDisconnect(con)

    return(rv)
}
