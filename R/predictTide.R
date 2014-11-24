#' Get the predicted tides for a period of time.
#'
#' Returns the minimum data.frame of consecutive hourly or high and
#' low tide times and heights such that the specified period is fully
#' covered.  The predictions come from the Natural Resources Canada
#' server at http://tides.gc.ca
#'
#' @param start: starting time, as a POSIXct timestamp; default:
#' midnight of the current day.
#' 
#' @param end: ending time, as a POSIXct timestamp; default: midnight
#' of the following day.
#' 
#' @param station: integer, the integer code for the statio, as
#' obtained from http://tides.gc.ca; default: 247, which is Dilligent
#' River, NS
#'
#' @param hourly: boolean, if TRUE, return tide height at start of each hour;
#'                if FALSE, return times and heights at highs and lows;
#'                default: FALSE
#' 
#' @param timezone: character, the 3-letter timezone abbreviation.
#' default "UTC"
#' 
#' @return a data.frame with these columns:
#' \enumerate{
#' \item "ts": the date/time of the prediction, as a POSIXct
#' timestamp
#' 
#' \item "height": the tide height, in metres
#'}
#' and, if \code{hourly == FALSE},:
#' \enumerate{
#' \item "high": boolean, TRUE if the row is high tide; FALSE for low tide
#' }
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

predictTide = function(
    start = as.POSIXct(trunc(Sys.time(), "day")),
    end = as.POSIXct(trunc(Sys.time(), "day") + 24 * 3600),
    station = 247,
    hourly = FALSE,
    timezone = "UTC")
{

    ## extend time brackets

    if (hourly) {
        start2 = trunc(start, "day")     ## truncate down to closest day
        end2 = round(end + 1800, "hour") ## round up to closest hour
    } else {
        start2 = start - 24 * 3600       ## back one day
        end2 = end + 24 * 3600           ## forward one day
    }

    ## reply strings from the server
    reply = ""

    ## number of hourly readings, for scan
    nhr = 0

    ## time iterator
    ti = start2
    
    ## query the server
    while (ti < end2) {
        x = xmlTreeParse(
            sprintf(
                "http://tides.gc.ca/eng/station?type=0&date=%s&sid=%d&tz=%s&pres=%d",
                format(ti, "%Y%%2F%m%%2F%d"),  ## '%%2F' is the URL-encoded '/'
                station,
                timezone,
                if (hourly) 1 else 2
                ),
            useInternalNodes = TRUE)

        if (hourly) {
            reply = c(reply, do.call(paste, c(lapply(getNodeSet(x, "//td[not(@class)]")[1:(7*24)], xmlValue), sep=",")))
            nhr = nhr + 7 * 24
        } else {
            reply = c(reply, xmlValue(getNodeSet(x, "//div[@class='stationTextData']")[[1]]))
        }
        ti = ti + 7 * 24 * 3600
    }
    ## concatenate all lines together
    reply = paste(reply, collapse="")

    if (hourly) {
        ## parse hourly heights
        h = scan(textConnection(reply), sep=",", quiet=TRUE, n=nhr)

        ## timestamps are top of the hour from first requested day
        ts = start2 + (0:(length(h)-1)) * 3600

        ## select desired range
        keep = which (ts >= start & ts <= end)

        return (data.frame(ts=ts[keep], height=h[keep]))
    } else {
        ## drop blank lines
        reply = gsub('[ \r\n]+', "\n", reply, perl=TRUE)

        ## read as a table
        y = read.table(textConnection(reply), sep=";")

        ## parse timestamps
        ts = as.POSIXct(strptime(paste(y[[1]], y[[2]]), format="%Y/%m/%d %H:%M:%S"))

        ## parse height
        h = as.numeric(gsub("(m)", "", y[[3]], fixed=TRUE))

        ## select desired range
        keep = (which(ts > start)[1] - 1) : (tail(which(ts < end), 1) + 1)

        ## is first tide high or low?
        high = diff(h[keep[1:2]]) < 0
        
        ## create return dataframe
        return(data.frame(
            ts=ts[keep],
            height=h[keep],
            high = rep(if(high) c(TRUE, FALSE) else c(FALSE, TRUE), length = length(keep))
            ))
    }
}
