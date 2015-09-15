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
#' @param aziRange: 2-element vector giving start and end azimuths
#' (each as a fraction in [0, 1] of the sweep starting at the heading
#' pulse.  If NULL, the whole sweep is present, otherwise, the
#' azimuths from aziRange[1] to aziRange[2] are present.  If
#' aziRange[1] > aziRange[2], the pulses wrap around the heading
#' and the missing segment is somewhere in the middle of the sweep.
#' 
#' @return boolean; TRUE if export was successful, FALSE otherwise.
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

library(magrittr)

wamosHeader = 
"CC *********** PARAMETER WRITTEN BY exportWamos.R   **************\r
OWNER John Brzustowski  CC OWNER OF PROGRAM\r
VINFO capture - Version 0.1\r
VERSN Jul 03 2015 00:00:00 CC COMPILATION DATE AND TIME\r
TOWER FORCE Visitor Centre\r
IDENT FVC   CC SHORT IDENTIFIER\r
USER  Bridgemaster E + redpitaya digdar\r
LAT   045\xb022.281 N   CC [Degree] POSITION NORTH\r
LONG  064\xb024.167 W   CC [Degree] POSITION EAST\r
POSTV     0          CC          LAT LONG VALID FLAG\r
DATE  @DATE\r
TIME  @TIME\r
ZONE      0\r
INTER @INTER CC [min]  TIME INTERVAL\r
NIPOL @NIPOL CC NUMBER OF POLAR IMAGES\r
NUMRE @NUMRE CC NUMBER OF RECORDED POLAR IMAGES\r
RPT   @RPT   CC [sec]  ANTENNA REPETITION TIME\r
SDRNG @SDRNG CC [m]    SAMPLE DELAY RANGE\r
SFREQ @SFREQ CC [MHz]  SAMPLING FREQUENCY\r
FIFO  @FIFO  CC        Number of samples in range\r
BO2RA @BO2RA CC [deg]  ANGLE BETWEEN BOW AND RADAR HEADING\r
HDGDL   0    CC [deg]  ANGLE BETWEEN ANTENNA HEADING AND PICTURE START\r
GYROC @GYROC CC [deg]  SHIP'S COMPASS HEADING\r
GYROV     1  CC        GYROC VALID FLAG\r
VGAIN     0  CC        WaMoS VIDEOGAIN\r
CMPOFF    0  CC Offset added to Ship's compass\r
WDEPF     1  CC Waterdepth from 0=global Waterdepth, 1=NMEA-Data, 2=cartesian Boxes\r
P_DEP @P_DEP CC [m] Waterdepth-List in meter\r
PDEPV     0  CC        WATER DEPTH VALID FLAG\r
SHIPR     0  CC [deg]  SHIP'S GROUND TRACK FROM GPS\r
SHIRV     0  CC        SHIP GROUND TRACK VALID FLAG\r
SHIPS   0.0  CC [kn]   SHIP SPEED OVER GROUND\r
SHISV     0  CC        SHIP SPEED OVER GROUND VALID FLAG\r
SPTWL     0  CC [m/s]  LONG SHIP SPEED IN WATER\r
SPWLV     0  CC        LONG SHIP SPEED IN WATER VALID FLAG\r
SPTWT     0  CC [m/s]  TRANS SHIP SPEED IN WATER\r
SPWTV     0  CC        TRANS SHIP SPEED IN WATER VALID FLAG\r
WINDS     0  CC [m/s]  WIND SPEED\r
WINSV     0  CC        WIND SPEED VALID FLAG\r
WINDR     0  CC [deg]  WIND DIRECTION\r
WINRV     0  CC        WIND DIRECTION VALID FLAG\r
WINDT     0  CC        0=APPARENT WIND, 1=TRUE WIND\r
WINDH     0  CC [m]    WIND SENSOR HEIGHT\r
WATSP     0  CC [m/s]  SPEED THROUGH WATER\r
WATSV     0  CC        SPEED THROUGH WATER VALID FLAG\r
DABIT    12  CC        DATA BITS PER PIXEL\r
CC     **************** START FRAMEDATA SECTION **************\r
CC     Date        Time          GYROC  SHIPS  RPT    LAT           LONG         SHIPR WINDS WINDR P_DEP   SPTWL  SPTWT  WATSP \r
CC                               [deg]  [kn]   [s]                               [deg] [m/s] [deg] [m]     [m/s]  [m/s]  [m/s] \r
"

wamosTrailer =
"RPM   @RPM      CC [sec]  ANTENNA REPETITION TIME\r
CC     **************** STOP FRAMEDATA SECTION *************\r
EOH   CC ************ END OF HEADER **************\r
"

wamosFormat = list(
    DATE  = "%m-%d-%Y",
    TIME  = "%H:%M:%S",
    INTER = "%5g",
    NIPOL = "%5d",
    NUMRE = "%5d",
    RPT   = "%5.2f",
    SDRNG = "%5g",
    SFREQ = "%5.1f",
    FIFO  = "%5d",
    BO2RA = "%5g",
    GYROC = "%5.1f",
    P_DEP = "%5.1f",
    ## following fields are for the frame table
    FDATE = "%m-%d-%Y",
    FTIME = "%H:%M:%OS3",
    FNUM  = "F%04d",
    FRPT  = "%5.3f",
    RPM   = "%5.2f"
    )


wamosFrameDataLine = 
"@FNUM @FDATE @FTIME @GYROC 000.0 @FRPT 045\xb022.281 N  064\xb024.167 W  000  000.0  000  0000.0  -09.0  -09.0  -09.0       \r\n"

wamosFilenameFormat = "%Y%m%d%H%Mfvc.pol"  ## fvc is force visitor centre

FORCEHdg = 136.8 ## heading marker of FORCE VC radar, in degrees
                 ## clockwise from True North.

FORCEStart = 180 ## true azimuth of first captured pulse; we capture
                 ## azimuth from fraction 0.12 to 0.43 around the
                 ## compass, starting at heading.  i.e. at 0.12 * 360
                 ## + 136.8 = 180 to 0.43 * 360 + 136.8 = 291.6
                 ## degrees from true North.

TS = function(x) structure(x, class=class(Sys.time()))
FMT = function(x, y) if (inherits(x, "POSIXt")) format(x, y) else sprintf(y, x)
FILLIN = function(s, x, y) sub(paste0("@", x), FMT(y, wamosFormat[[x]]), s, useBytes=TRUE, fixed=TRUE)

exportWamos = function(sweeps, path, depths = 0, nACP=450, aziRange=NULL) {
    ## format timestamp of last pulse in first sweep into filename and open it

    nsw = length(sweeps)

    ## timestamps of last pulse in first and last sweeps
    ts    = tail(sweeps[[  1]]$ts, 1)
    tsEnd = tail(sweeps[[nsw]]$ts, 1)
    
    f = ( TS(ts)
         %>% FMT(wamosFilenameFormat)
         %>% file.path(path, .)
         %>% file(., "wb")
         )

    ## sweep attributes
    sa = attr(sweeps[[1]], "radar.meta")

    ## repetition period (sweep duration)
    
    RPT = (tsEnd - ts) / (nsw - 1)

    ## if we don't have full sweeps, pad with zero pulses and
    ## appropriate numbers of heading pulses

    if (is.null(aziRange)) {
        ## number of extra bytes to write
        padLength = 0
    } else {
        ## a pair of padding pulses with a single ACP pulse (1 then 0)
        ## 2 pulses times 2 bytes per sample times num samples
        padACP = raw(2 * 2 * sa$ns)
        ## set bit 5 of high byte of first sample in first pulse
        padACP[2] = as.raw(32)
        
        if (diff(aziRange) > 0) {
            ## usual situation: no zero crossing; must pad at
            ## start and end of included area
            padEnds = TRUE
            ## pulse bytes to include before start
            ## of real data
            prePad = rep(padACP, times = floor(nACP * aziRange[1]))

            ## pulse bytes to include after start
            ## of real data
            postPad = rep(padACP, times = nACP - floor(nACP * aziRange[2]))
            padLength = length(prePad) + length(postPad)
        } else {
            ## only pad in middle; real sweep data at start
            ## and end
            padEnds = FALSE

            ## pulse bytes to insert between first and second segments
            ## of sweep
            midPad = rep(padACP, - diff(floor(nACP * aziRange)))
            padLength = length(midPad)
        }
    }
    
    ## make the header by replacing tagged strings with their
    ## formatted items

    cat ((
        wamosHeader
        %>% FILLIN("DATE",  TS(ts)                )
        %>% FILLIN("TIME",  TS(ts)                )
        %>% FILLIN("INTER", 15                    )  ## 15 minute sampling interval?
        %>% FILLIN("NIPOL", nsw - 1               )
        %>% FILLIN("NUMRE", nsw                   )
        %>% FILLIN("RPT",   RPT                   )
        %>% FILLIN("SDRNG", 0                     )
        %>% FILLIN("SFREQ", sa$rate / 1E6         )
        %>% FILLIN("FIFO",  sa$ns                 )  ## number of samples
        %>% FILLIN("BO2RA", FORCEStart - FORCEHdg )
        %>% FILLIN("GYROC", FORCEHdg              )
        %>% FILLIN("P_DEP", depths                )  ## FIXME fails when length(depths) > 1

        ), file=f)
    
    ## for each sweep, generate an entry in the frame table

    ## generate estimated "start" of first sweep, since we have only a sector

    tsLastEnd = tail(sweeps[[1]]$ts, 1) - (sweeps[[2]]$ts[1] - sweeps[[1]]$ts[1])

    for (i in seq(along=sweeps)) {
        nr = nrow(sweeps[[i]])
        tsEnd = sweeps[[i]]$ts[nr]
        
        cat ((
            wamosFrameDataLine
            %>% FILLIN("FNUM",  i               )
            %>% FILLIN("FDATE", TS(tsEnd)       )
            %>% FILLIN("FTIME", TS(tsEnd)       )
            %>% FILLIN("GYROC", FORCEHdg        )
            %>% FILLIN("FRPT",  tsEnd - tsLastEnd )
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

        ## Convert to 12-bit scale.

        ## Max sample value in 14 bits is 16383.  Lowest sample value
        ## in 14 bits with existing voltage mapping is ~ 4096.  We've
        ## been adding rather than decimating at low decimation rates.

        nr = nrow(sweeps[[i]])
        nsamps = sa$ns * nr
        samps = as.numeric(readBin(unlist(sweeps[[i]]$samples), integer(), size=2, n=nsamps))

        ## number of samples combined into each sample
        dec = round(125e6 / sa$rate)

        ## convert to full-scale 12 bit data.

        samps = as.integer(round((samps - (dec * 4096)) / (dec * (16383 - 4096)) * 4095))
        
        ## get bearing pulse count at each pulse
        bpc = round(nACP * sweeps[[i]]$azi)

        ## which pulses get a bearing pulse
        tick = c(1, (1 + which(diff(bpc) > 0)) * sa$ns)
        
        samps[tick] = samps[tick] + 8192L

        ## write 10-byte byte count
        cat (sprintf("%10d", length(samps) * 2 + padLength), file=f)

        ## write raw data, possibly with padding
        if (is.null(aziRange)) {
            writeBin(samps, f, size=2, useBytes=TRUE)
        } else {
            if (padEnds) {
                writeBin(prePad, f, useBytes = TRUE)
                writeBin(samps, f, size=2, useBytes=TRUE)
                writeBin(postPad, f, useBytes = TRUE)
            } else {
                ## number of data pulses that come before padding
                np = sum(sweeps[[i]]$azi < aziRange[2])
                init = 1:(sa$ns * np)
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
}
