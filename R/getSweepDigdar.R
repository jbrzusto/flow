#' Get a sweep of radar data from a digdar file.  The file can optionally
#' be gz-compressed.
#'
#' @param f: path to digdar file, e.g. csh-2018-05-22T22-00-01.878Z.dat
#'
#' @return NULL on error, otherwise a list with
#' these columns:
#' \enumerate{
#' \item ts double, timestamp for start of pulse
#' \item azi float, azimuth of pulse, relative to start of heading pulse, (0..1)
#' \item trigs integer, trigger count, for detecting dropped pulses
#' \item samples raw vector, digitized samples for each pulse
#' }
#' The list will also have an attribute "radar.meta" which is a list
#' with these items:
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

getSweepDigdar = function(f) {
    zcon = gzfile(f, "rb") ## might not be compressed; gzfile() doesn't care
    hdr = readLines(zcon, 2)
    if (! grepl("^DigDar", hdr[1]))
        stop("file is not a DigDar sweep file: ", f)
    meta = fromJSON(hdr[2])

    samplesPerPulse = meta$ns
    samplingRate = meta$clock * 1e6
    decimation = meta$decim

    ## metres per sample
    mps = VELOCITY_OF_LIGHT / (samplingRate / decimation) / 2.0

    clocks = readBin(zcon, integer(), n = meta$np, size=4) / (meta$clock * 1e6)
    sweep = list (
        ts      = meta$ts0 - clocks[1] + clocks,
        azi     = readBin(zcon, numeric(), n = meta$np, size=4),
        trigs   = readBin(zcon, integer(), n = meta$np, size=4),
        samples = readBin(zcon, raw(), n = meta$np * meta$ns * 2)
    )
    close(zcon)
    meta$rate = meta$clock * 1e6 / meta$decim
    attr(sweep, "radar.meta") = meta
    return(sweep)
}
