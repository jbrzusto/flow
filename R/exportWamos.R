#' Export a set of sweeps in WAMOS format.
#'
#' Writes a file in the WAMOS .pol format which contains sweeps from the set.
#'
#' @param sweeps: a list of sweeps of data, with metadata attributes,
#' as returned by \code{getSweep()}.  Sweeps must be in chronological
#' order, and at constant time spacing (i.e. no missing sweeps are
#' allowed).
#'
#' @param path: path to destination folder
#'
#' @param depths: vector of water depths at sweep times (tide height + constant)
#' Defaults to 0.
#'
#' @param nACP: number of bearing pulses to simulate.  A bearing pulse
#' is indicated by setting bit 5 of the high byte of the first sample
#' for a pulse.
#'
#' @param aziLim: 2-element vector giving start and end azimuths
#' (each as a fraction in [0, 1] of the sweep starting at the heading
#' pulse.  If NULL, the whole sweep is present, otherwise, the
#' azimuths from aziLim[1] to aziLim[2] are present.  If
#' aziLim[1] > aziLim[2], the pulses wrap around the heading
#' and the missing segment is somewhere in the middle of the sweep.
#' Because the WAMOS .pol format requires a full sweep, in this situation,
#' we add padding zero pulses with an appropriate number of 0-1 transitions
#' on the azimuth pulse flag.  We also record the true azimuth (relative
#' to the heading pulse) of the first digitized pulse, in degrees, in the
#' heading field HDGDL.  The compass heading angle of the radar is recorded
#' in GYROC, and BO2RA is set to 0.
#'
#' @param rangeLim: 2-element vector giving start and end ranges of samples,
#' in metres.  If NULL, the entire pulse is used.  Otherwise, only samples
#' overlapping the specified range are included.
#'
#' @param decim: integer; if 1, use all samples in a pulse, subject to \code{range}, above.
#' If > 1, then take the last of each sequence of decim consecutive samples, discarding
#' the rest.  The sampling rate is adjusted accordingly.
#'
#' @param meta; list with these named items which are used to populate .pol file fields:
#' \itemize{
#' \item tower: character scalar; label for radar site
#' \item ident: character scalar; 3-letter code for site
#' \item user: character scalar; models of radar and digitizer
#' \item lat: numeric scalar; latitude, in degrees North
#' \item long: numeric; longitude, in degrees East
#' \item heading: numeric; angle of heading pulse, in degrees clockwise from true North
#' \item aziStart: numeric; angle of first pulse, in degrees clockwise from true North
#' @return full path to the file written
#'
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

library(magrittr)

wamosHeader =
"CC *********** PARAMETER WRITTEN BY exportWamos.R   **************\r
OWNER John Brzustowski  CC OWNER OF PROGRAM\r
VINFO capture - Version 0.1\r
VERSN Jun 13 2018 00:00:00 CC COMPILATION DATE AND TIME\r
TOWER @TOWER\r
IDENT @IDENT   CC SHORT IDENTIFIER\r
USER  @USER\r
LAT   @LAT   CC [Degree] POSITION NORTH\r
LONG  @LONG   CC [Degree] POSITION EAST\r
POSTV     1          CC          LAT LONG VALID FLAG\r
DATE  @DATE\r
TIME  @TIME\r
ZONE      0\r
INTER @INTER CC [min]  TIME INTERVAL\r
NIPOL @NIPOL CC NUMBER OF POLAR IMAGES\r
NUMRE @NUMRE CC NUMBER OF RECORDED POLAR IMAGES\r
RPT   @RPT CC [sec]  ANTENNA REPETITION TIME\r
SDRNG @SDRNG CC [m]    SAMPLE DELAY RANGE\r
SFREQ @SFREQ CC [MHz]  SAMPLING FREQUENCY\r
FIFO  @FIFO CC        Number of samples in range\r
BO2RA @BO2RA CC [deg]  ANGLE BETWEEN BOW AND RADAR HEADING\r
HDGDL   0.0 CC [deg]  ANGLE BETWEEN ANTENNA HEADING AND PICTURE START\r
GYROC @GYROC CC [deg]  SHIP'S COMPASS HEADING\r
GYROV     1 CC        GYROC VALID FLAG\r
VGAIN     0 CC        WaMoS VIDEOGAIN\r
CMPOFF    0 CC Offset added to Ship's compass\r
WDEPF     1 CC Waterdepth from 0=global Waterdepth, 1=NMEA-Data, 2=cartesian Boxes\r
P_DEP @P_DEP CC [m] Waterdepth-List in meter\r
PDEPV     0 CC        WATER DEPTH VALID FLAG\r
SHIPR     0 CC [deg]  SHIP'S GROUND TRACK FROM GPS\r
SHIRV     0 CC        SHIP GROUND TRACK VALID FLAG\r
SHIPS   0.0 CC [kn]   SHIP SPEED OVER GROUND\r
SHISV     0 CC        SHIP SPEED OVER GROUND VALID FLAG\r
SPTWL     0 CC [m/s]  LONG SHIP SPEED IN WATER\r
SPWLV     0 CC        LONG SHIP SPEED IN WATER VALID FLAG\r
SPTWT     0 CC [m/s]  TRANS SHIP SPEED IN WATER\r
SPWTV     0 CC        TRANS SHIP SPEED IN WATER VALID FLAG\r
WINDS     0 CC [m/s]  WIND SPEED\r
WINSV     0 CC        WIND SPEED VALID FLAG\r
WINDR     0 CC [deg]  WIND DIRECTION\r
WINRV     0 CC        WIND DIRECTION VALID FLAG\r
WINDT     0 CC        0=APPARENT WIND, 1=TRUE WIND\r
WINDH     0 CC [m]    WIND SENSOR HEIGHT\r
WATSP     0 CC [m/s]  SPEED THROUGH WATER\r
WATSV     0 CC        SPEED THROUGH WATER VALID FLAG\r
DABIT    12 CC        DATA BITS PER PIXEL\r
CC     **************** START FRAMEDATA SECTION **************\r
CC     Date        Time          GYROC  SHIPS  RPT    LAT           LONG         SHIPR WINDS WINDR P_DEP   SPTWL  SPTWT  WATSP \r
CC                               [deg]  [kn]   [s]                               [deg] [m/s] [deg] [m]     [m/s]  [m/s]  [m/s] \r
"

wamosTrailer =
"RPM   @RPM CC [sec]  ANTENNA REPETITION TIME\r
CC     **************** STOP FRAMEDATA SECTION *************\r
EOH   CC ************ END OF HEADER **************\r
"

fmtLat = function(lat) {
    letter = if (lat > 0) "N" else "S"
    lat = abs(lat)
    sprintf("%03d\xb0%.3f %s", trunc(lat), 60 * (lat - trunc(lat)), letter)
}
fmtLong = function(lon) {
    letter = if (lon > 0) "E" else "W"
    lon = abs(lon)
    sprintf("%03d\xb0%.3f %s", trunc(lon), 60 * (lon - trunc(lon)), letter)
}

wamosFormat = list(
    TOWER = "%s",
    IDENT = "%s",
    USER = "%s",
    LAT = fmtLat,
    LONG = fmtLong,
    DATE  = "%m-%d-%Y",
    TIME  = "%H:%M:%S",
    INTER = "%5g",
    NIPOL = "%5d",
    NUMRE = "%5d",
    RPT   = "%5.2f",
    SDRNG = "%5g",
    SFREQ = "%5.1f",
    FIFO  = "%5d",
    BO2RA = "%5.1f",
    GYROC = "%5.1f",
    P_DEP = "%5.1f",
    ## following fields are for the frame table
    FDATE = "%m-%d-%Y",
    FTIME = "%H:%M:%OS3",
    FNUM  = "F%04d",
    FRPT  = "%5.3f",
    RPM   = "%5.2f",
    FDEP  = "%5.1f"
    )


wamosFrameDataLine =
"@FNUM @FDATE @FTIME @GYROC 000.0 @FRPT @LAT  @LONG  000  000.0  000  @FDEP  -09.0  -09.0  -09.0       \r\n"

wamosFilenameFormat = "%Y%m%d%H%M%S%%s.pol"  ## '%%s' is for short site code; strftime is used first, then sprintf

FORCEHdg = 136.8 ## heading marker of FORCE VC radar, in degrees
                 ## clockwise from True North.

FORCEStart = 180 ## true azimuth of first captured pulse; we capture
                 ## azimuth from fraction 0.12 to 0.43 around the
                 ## compass, starting at heading.  i.e. at 0.12 * 360
                 ## + 136.8 = 180 to 0.43 * 360 + 136.8 = 291.6
                 ## degrees from true North.

TS = function(x) structure(x, class=class(Sys.time()))
FMT = function(x, y) if (inherits(x, "POSIXt")) format(x, y) else if (inherits(y, "function")) y(x) else paste(sprintf(y, x), collapse=" ")
FILLIN = function(s, x, y) sub(paste0("@", x), FMT(y, wamosFormat[[x]]), s, useBytes=TRUE, fixed=TRUE)

VELOCITY_OF_LIGHT = 2.99792458E8

exportWamos = function(sweeps, path, depths = 0, nACP=450, aziLim = NULL, rangeLim = NULL, decim = 1, meta) {
    ## format timestamp of last pulse in first sweep into filename and open it

    nsw = length(sweeps)

    ## timestamps of last pulse in first and last sweeps
    ts    = tail(sweeps[[  1]]$ts, 1)
    tsEnd = tail(sweeps[[nsw]]$ts, 1)

    fname = TS(ts) %>% FMT(wamosFilenameFormat) %>% sprintf (meta$ident) %>% file.path(path, .)

    f = file(fname, "wb")

    ## sweep attributes
    sa = attr(sweeps[[1]], "radar.meta")

    ## add in site metadata
    sa[names(meta)] = meta

    ## allow override by site file
    sa[names(meta$overrides)] = meta$overrides

    ## samples per pulse in input
    nsIn = sa$ns

    ## repetition period (sweep duration)

    RPT = (tsEnd - ts) / (nsw - 1)

    ## sample selector for each pulse; first apply decimation:
    SSEL = rep(c(rep(FALSE, decim - 1), TRUE), length=nsIn)

    ## apply range limits

    if (is.null(rangeLim)) {
        range0 = 0
    } else {
        sampleRange = VELOCITY_OF_LIGHT / sa$rate / 2
        S0 = 1 + floor  (rangeLim[1] / sampleRange)
        range0 = (S0 - 1) * sampleRange
        SN = ceiling(rangeLim[2] / sampleRange)
        ## sample selector: remove any samples out of the range
        if (S0 > 1)
            SSEL[1:S0] = FALSE
        if (SN < nsIn)
            SSEL[SN:nsIn] = FALSE
    }
    ## number of samples retained per pulse:
    nsOut = sum(SSEL)

    ## if we don't have full sweeps, pad with zero pulses and
    ## appropriate numbers of heading pulses

    if (is.null(aziLim)) {
        ## number of extra bytes to write
        padLength = 0
    } else {
        ## a pair of padding pulses with a single ACP pulse (1 then 0)
        ## 2 pulses times 2 bytes per sample times num samples
        padACP = raw(2 * 2 * nsOut)
        ## set bit 5 of high byte of first sample in first pulse
        padACP[2] = as.raw(32)

        if (diff(aziLim) > 0) {
            ## usual situation: no zero crossing; must pad at
            ## start and end of included area
            padEnds = TRUE
        } else {
            ## only pad in middle; real sweep data at start
            ## and end
            padEnds = FALSE

            ## pulse bytes to insert between first and second segments
            ## of sweep
            midPad = rep(padACP, - diff(floor(nACP * aziLim)))
            padLength = length(midPad)
        }
    }


    ## make the header by replacing tagged strings with their
    ## formatted items

    cat ((
        wamosHeader
        %>% FILLIN("DATE",  TS(ts)                        )
        %>% FILLIN("TIME",  TS(ts)                        )
        %>% FILLIN("INTER", 30                            )  ## 30 minute sampling interval?
        %>% FILLIN("NIPOL", nsw - 1                       )
        %>% FILLIN("NUMRE", nsw                           )
        %>% FILLIN("RPT",   RPT                           )
        %>% FILLIN("SDRNG", round(range0)                 )
        %>% FILLIN("SFREQ", sa$rate / 1E6 / decim         )
        %>% FILLIN("FIFO",  nsOut                         )  ## number of samples
        %>% FILLIN("BO2RA", meta$heading                  )  ## compass direction of heading, degrees clockwise from N
        %>% FILLIN("GYROC", meta$aziStart - meta$heading  )  ## nominal azimuth of first pulse, relative to heading
        %>% FILLIN("P_DEP", mean(depths)                  )  ## mean depth across sweeps
        %>% FILLIN("LAT",   meta$lat                      )
        %>% FILLIN("LONG",  meta$long                     )
        %>% FILLIN("TOWER", meta$tower                    )
        %>% FILLIN("IDENT", meta$ident                    )
        %>% FILLIN("USER",  meta$user                     )

        ), file=f)

    ## for each sweep, generate an entry in the frame table

    ## generate estimated "start" of first sweep, since we have only a sector

    tsLastEnd = tail(sweeps[[1]]$ts, 1) - (sweeps[[2]]$ts[1] - sweeps[[1]]$ts[1])

    for (i in seq(along=sweeps)) {
        np = length(sweeps[[i]]$ts)
        tsEnd = sweeps[[i]]$ts[np]

        cat ((
            wamosFrameDataLine
            %>% FILLIN("FNUM",  i                        )
            %>% FILLIN("FDATE", TS(tsEnd)                )
            %>% FILLIN("FTIME", TS(tsEnd)                )
            %>% FILLIN("GYROC", sweeps[[i]]$azi[1] * 360 )
            %>% FILLIN("FRPT",  tsEnd - tsLastEnd        )
            %>% FILLIN("FDEP",  depths[i]                )
            %>% FILLIN("LAT",   meta$lat                 )
            %>% FILLIN("LONG",  meta$long                )
            ), file=f)
        tsLastEnd = tsEnd
    }

    ## trailer, with RPM estimate
    cat ((
        wamosTrailer
        %>% FILLIN("RPM",  RPT )  ## FIXME: any reason to not use this?
        ), file=f)

    ## each sweep's raw samples, preceded by a number padded left with
    ## space to 10 chars, giving size of following binary data, in
    ## bytes, not including the 10-byte size field.

    for (i in seq(along=sweeps)) {

        ## for each sweep, write its raw sample data.

        ## Convert to 12-bit scale using site's dynRange, and depending on the
        ## digitizing mode ("sum" or "first")

        np = length(sweeps[[i]]$ts)
        nsamps = nsIn * np

        ## read samples as unsigned integers, and apply the sample selector
        ## (SSEL gets recycled for each pulse)
        samps = as.numeric(readBin(unlist(sweeps[[i]]$samples), integer(), size=2, n=nsamps, signed=FALSE)[SSEL])

        ## number of samples combined (by the digitizer) into each sample
        ddec = if (sa$mode=="sum") round(125e6 / sa$rate) else 1

        ## convert to full-scale 12 bit data.

        samps = as.integer(round((samps - (ddec * 4096)) / (ddec * (diff(sa$dynRange))) * 4095))

        ## get bearing pulse count at each pulse; i.e. number of bearing pulses
        ## which should have been seen before or at this pulse
        bpc = floor(nACP * sweeps[[i]]$azi)

        ## which pulses get a bearing pulse
        tick = 1 + which(diff(bpc) > 0)

        ## which sample slots get the bit flag for a bearing pulse
        tick = 1 + (tick - 1) * nsOut

        ## set the bearing pulse bit
        samps[tick] = samps[tick] + 8192L


        ## write raw data, possibly with padding
        if (is.null(aziLim)) {
            ## write 10-byte byte count
            cat (sprintf("%10d", length(samps) * 2), file=f)
            writeBin(samps, f, size=2, useBytes=TRUE)
        } else {
            if (padEnds) {
                ## add any pulse bytes to include before start
                ## of real data
                if (bpc[1] > 0)
                    prePad = rep(padACP, times = bpc[1])
                else
                    prePad = NULL
                if (tail(bpc, 1) < nACP)
                    postPad = rep(padACP, times = nACP - tail(bpc, 1))
                else
                    postPad = NULL
                ## write 10-byte byte count
                padLength = length(prePad) + length(postPad)
                cat (sprintf("%10d", length(samps) * 2 + padLength), file=f)

                writeBin(prePad, f, useBytes = TRUE)
                writeBin(samps, f, size=2, useBytes=TRUE)
                writeBin(postPad, f, useBytes = TRUE)
            } else {
                ## number of data pulses that come before padding
                ninit = sum(sweeps[[i]]$azi < aziLim[2])
                init = 1:(nsOut * ninit)

                ## amount of padding (add any missing bpc)
                midPad = rep(padACP, times = bpc[ninit + 1] - bpc[ninit])
                padLength = length(midPad)

                ## write 10-byte byte count
                cat (sprintf("%10d", length(samps) * 2 + padLength), file=f)

                ## write initial segment
                writeBin(samps[  init ], f, size=2, useBytes=TRUE)
                ## write padding
                writeBin(midPad, f, useBytes = TRUE)
                ## write remaining segment
                writeBin(samps[ -init ], f, size=2, useBytes=TRUE)
            }
        }
    }
    close(f)
    return(paste0(fname, ".bz2"))
}
