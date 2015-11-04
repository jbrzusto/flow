#' read a sequence of radar sweeps from a WAMOS .pol file
#'
#' @param f: character scalar full path to .pol file
#'
#' @return: list of sweep matrices with these columns
#' ts,azi,samples
#' and these attributes:
#'
#'

importWamos = function(f) {
    x = readBin(f, raw(), n=file.info(f)$size)
    con = rawConnection(x, "r")
    hdr = list()
    repeat {
        a = readLines(con, 1)
        if (substr(a, 1, 2) == "CC")
            next
        kw = gsub(" *", "", substr(a, 1, 5), perl=TRUE)
        if (kw == "EOH")
            break
        a = sub("CC.*", "", a, perl=TRUE)
        vals = substring(a, 7)
        switch(kw,
               DATE = {
                   dt = mdy(vals)
               },
               TIME = {
                   hdr[["DATETIME"]] = dt + hms(vals)
               },
               LAT = {
                   vv = sub(" .*", "", vals)
                   hdr[["LAT"]] = as.numeric(substr(vv,1, 3)) + as.numeric(substring(vv, 5)) / 60
                   if (substr(vals, 2 + nchar(vv), 2 + nchar(vv)) == "S")
                       hdr[["LAT"]] = - hdr[["LAT"]]
               },
               LONG = {
                   vv = sub(" .*", "", vals)
                   hdr[["LONG"]] = as.numeric(substr(vv,1, 3)) + as.numeric(substring(vv, 5)) / 60
                   if (substr(vals, 2 + nchar(vv), 2 + nchar(vv)) == "W")
                       hdr[["LONG"]] = - hdr[["LONG"]]
               },
               {
                   hdr[[kw]] = read.table(textConnection(vals))
                   if (length(hdr[[kw]]) == 1)
                       hdr[[kw]] = c(unlist(hdr[[kw]]))
               }
               )
    }
    sweeps = list("vector", hdr$NUMRE)
    ns = hdr$FIFO
    for (i in 1:hdr$NUMRE) {
        nb = as.numeric(readChar(con, 10))
        np = nb / (2 * ns)
        sweeps[[i]] = readBin(con, integer(), size=2, signed=FALSE, n=np * ns)
        dim(sweeps[[i]]) = c(ns, np)
        azp = sweeps[[i]][1,] >= 8192L
        sweeps[[i]][1, azp] = sweeps[[i]][1, azp] - 8192L
        attr(sweeps[[i]], "azi") = cumsum(azp)
    }
    attr(sweeps, "hdr") = hdr
    return(sweeps)
}

