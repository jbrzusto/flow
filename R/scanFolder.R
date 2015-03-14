#' Scan folders for sqlite radar databases.
#'
#' Returns a function, \code{f}, which maps timestamps to database
#' pathnames.  For timestamps not covered by databases in the scanned
#' folders, \code{f} the function returns NA.
#'
#' @param folder: vector of folder names
#' 
#' @param pattern: regex for matching sqlite filenames in folder list
#' 
#' @return a function mapping timestamps to filenames, and which returns
#' NA for timestamps not covered by the radar databases.
#'
#' @note Requires that library RSQLite already be loaded.
#' 
#' @author John Brzustowski \email{jbrzusto@@REMOVE_THIS_PART_fastmail.fm}

scanFolder = function(folder, pattern="^FORCEVC_raw.*[.]sqlite$") {

    ## get paths to all matching radar databases in given folders
    files = dir(folder, pattern=pattern, recursive=TRUE, full.names=TRUE)
    n = length(files)
    
    ## Allocate a dataframe matching time endpoints to filenames.
    map = data.frame(from = numeric(n), to = numeric(n), file=I(character(n)))

    i = 0
    for (f in files) {
        con = dbConnect(RSQLite::SQLite(), f)
        if (dbExistsTable(con, "pulses")) {
            ## get time range for pulses in this file
            ## Note: for some reason, doing this as the single query
            ## 'select min(ts), max(ts) from pulses'
            ## is extremely slow, so we do it as two
            from = as.numeric(dbGetQuery(con, "select min(ts) from pulses")[1,1])
            ## don't register empty tables
            if (!is.na(from)) {
                i = i + 1
                map$from[i] = from
                map$to[i]   = as.numeric(dbGetQuery(con, "select max(ts) from pulses")[1,1])
                map$file[i] = f
            }
        }
        dbDisconnect(con)
    }

    if (i == 0)
        return(NULL)

    ## drop unused entries
    map = map[1:i,]
    
    ## sort by starting time
    map = map[order(map$from),]

    ## build up the sequence of points on the approximation map

    ts = c()
    fileno = c()

    for (j in 1:i) {
        ## if this isn't the first file, see whether it is consecutive
        ## with the previous one, and if not, add an interval mapping to NA
        if (j > 1 && map$from[j] - map$to[j - 1] > 1) {
            ## not consecutive
                ts = c(ts, map$to[j - 1] + 0.001, map$from[j] - 0.001)
                fileno = c(fileno, i+1, i+1)  ## map to non-existing file index
            }
        ts = c(ts, map$from[j], map$to[j])
        fileno = c(fileno, j, j)
    }

    ## get a function mapping timestamp to file numbers
    tsToFileNo = approxfun(ts, fileno, method="constant", rule=1)

    ## a function to map timestamps to filenames
    tsToFilename = function(ts) {
        ## map timestamps to file number, then lookup corresponding
        ## filenames, which will be NA for out-of-bounds timestamps
        return (filenames[tsToFileno(ts)])
    }

    ## make an environment with the filenames and mapping function
    env = new.env()
    env$filenames = map$file
    env$tsToFileno = tsToFileNo

    ## set that as the new function's environment
    ## (we do it this way to avoid carrying around all 
    ## the other variables from the current environment
    environment(tsToFilename) = env

    return(tsToFilename)
}
