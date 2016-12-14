#'
#' get the FORCE VC water depth at a sequence of times
#'
#' Accesses the Ocean Networks Canada data server to get archived tide
#' gauge data for force VC.  If no data are available there, they are
#' instead obtained from averaging Environment Canada predictions for
#' Diligent River and Cape Sharpe stations..
#'
#' @param t: numeric vector of times
#'
#' @return numeric vector of depths, calculcated
#' using a spline smoother from whatever data are
#' available in the requested time span
#'
library(RCurl)
library(jsonlite)

getDepth = function(t) {
    tFrom = TS(min(t) - 1)
    tTo = TS(max(t) + 1)
    token = 'ae479e84-1e6e-4a9f-82ad-231c1e2a111e'
    station = "BFIP"
    devCategory = "DEPTH_TEMP"
    ISOfmt = "%Y-%m-%dT%H:%M:%OS3Z"
    url = sprintf("http://dmas.uvic.ca/api/scalardata?token=%s&method=getByStation&station=%s&deviceCategory=%s&outputFormat=Array&dateFrom=%s&dateTo=%s",
                  token,
                  station,
                  devCategory,
                  format(tFrom, ISOfmt),
                  format(tTo, ISOfmt)
                  )
    resp = getURL(url)
    d = NULL
    tryCatch({
        d <- fromJSON(resp)
    }, error = function(e) {
        d <- list(serviceMetadata=list(totalActualSamples=0))
    })
    if (length(d) == 0 || length(d$serviceMetadata) == 0 || length(d$serviceMetadata$totalActualSamples) == 0 || d$serviceMetadata$totalActualSamples == 0) {
        ## no data on ONC for this period, so use average of Env. Can tide predictions between
        ## Diligent River (station 247) and Cape Sharpe (station 250)
        tlo = tFrom - 7200
        thi = tTo + 7200

        dr = predictTide(start=tlo, end=thi, station=247, hourly=TRUE)
        cs = predictTide(start=tlo, end=thi, station=250, hourly=TRUE)
        ts = as.numeric(dr$ts)
        vals = (dr$height + cs$height) / 2
    } else {
        d = fromJSON(resp)
        idepth = grep("depth", d$sensorData$sensor, ignore.case=TRUE)
        ts = as.numeric(ymd_hms(d$sensorData$data$sampleTimes[[idepth]]))
        vals = d$sensorData$data$values[[idepth]]
    }
    depth = spline(x = ts-ts[1], y = vals, xout = as.numeric(t) - ts[1])$y
    return(depth)
}
