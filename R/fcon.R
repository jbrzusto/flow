#  File src/library/graphics/R/filled.contour.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#  Don't use layout - use par and change clipping to preserve
#  coordinate system of plot so it can be used with locator().

fcon <-
function (x = seq(0, 1, length.out = nrow(z)),
          y = seq(0, 1, length.out = ncol(z)),
          z,
          xlim = range(x, finite=TRUE),
          ylim = range(y, finite=TRUE),
          zlim = range(z, finite=TRUE),
          levels = pretty(zlim, nlevels), nlevels = 64,
          color.palette = rainbow,
          col = color.palette(length(levels) - 1),
          plot.title, plot.axes, key.title, key.axes,
          asp = NA, xaxs = "i", yaxs = "i", las = 1, axes = TRUE,
          frame.plot = axes, ...)
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
        stop("increasing 'x' and 'y' values expected")

    mar.orig <- (par.orig <- par(c("mar","las","mfrow", "plt", "xpd")))$mar
    on.exit(par(par.orig))

    w <- (3 + mar.orig[2L]) * par("csi")
    plt = par.orig$plt
    din = par("din")
    plt[2] = plt[2] - w / din[1]
    par(plt=plt)
    
    par(las = las)

    ## Plot contour-image::
    ## mar <- mar.orig
    ## mar[4L] <- 1
    ## par(mar = mar)
    ## plot.new()
    plot(xlim, ylim, "", pch="", type="n", xaxs = xaxs, yaxs = yaxs, asp = asp, xlim=xlim, ylim=ylim, xlab="", ylab="")

    .filled.contour(x, y, z, levels, col)
    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2)
        }
    }
    else plot.axes
    if (frame.plot) box()
    if (missing(plot.title))
        title(...)
    else
	plot.title
    
    ## Plot the 'plot key' (scale):
    par(xpd=NA)

    ylev = ylim[1] + (levels - levels[1]) * diff(ylim) / diff(range(levels))
    xs = diff(xlim) / par("fin")[1]
    rs = xlim[2] + xs * 0.2
    rect(rs, ylev[-length(ylev)], rs + (w/2-0.2) * xs , ylev[-1L], col = col)
    if (missing(key.axes)) {
        if (axes)
            axis(4, pos=rs + (w/2) * xs )
    }
    else key.axes
    box()
    if (!missing(key.title))
	key.title

    invisible()
}
