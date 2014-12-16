#' Export a sweep of data as a binary file in a quick and dirty format.
#'
#' Given a sweep of data returned by \code{getSweep()} write it to a binary
#' file in the specified folder.  
#'
#' @param s: sweep of radar data as returned by \code{getSweep()}
#'
#' @param f: path to folder which will receive the exported file; defaults to "."
#'
#' @param siteCode: site code; defaults to FORCEVC; will form the prefix for
#' the exported filename
#'
#' @param ares: the angular resolution, in degrees.  Only the closest
#' pulse to each value in \code{seq(from = 0, to = 360, by = ares)}
#' will be exported.
#' 
#' @param sweepNum: if specified, record this as the sweep number.
#' Otherwise, use the global variable \code{FlowSweepNum} if that is
#' set, otherwise, use 1.
#' 
#' @return TRUE on sucess, FALSE otherwise
#'
#' @note  The output file is organized as one 'line' per pulse.
#' Each line has this format:
#'
#' sweep_no:   16 bit unsigned integer; records in each sweep have the same value
#' pulse_no:   16 bit unsigned integer; starting at 0 for each sweep
#'       ts:   64-bit double; number of seconds since 1 Jan 1970
#'      azi:   32-bit float; range [0..1]; fraction of sweep from heading
#'  samples:   16-bit unsigned samples; 1664 samples digitized at 41.66666 MHz
#'
#' As a side effect, this function increments the value of the global
#' variable \code{FlowSweepNum} or sets it to 1 if it does not exist.

exportSweep = function(s, f=".", siteCode="FORCEVC", ares=0.25, sweepNum) {
    if (missing(sweepNum)) {
        if (! exists("FlowSweepNum", 1)) {
            FlowSweepNum = 1
        }
    } else {
        FlowSweepNum = sweepNum
    }
    
    outf = file(file.path(f, sprintf("%s_sweep_%s.dat", siteCode,
        format(s$ts[1], "%Y-m-%dT%H:%M:%OS3", tz="GMT"))), "wb")

    use = approx(dat$azi, 1:nrow(dat), seq(from=0, to=0.1, by=ares/360),
        method="constant", rule=1)
    use = use[!is.na(use)]
    for (j in use) {
        writeBin(c(FlowSweepNum, j), outf, size=2)  ## sweep number, pulse_no
        writeBin(dat$ts[j], outf, size=8) ## ts
        writeBin(dat$azi[j], outf, size=4) ## azi
        writeBin(dat$samples[[j]], out) ## raw pulses
    }
    FlowSweepNum <<- FlowSweepNum + 1L
    close(outf)
}
