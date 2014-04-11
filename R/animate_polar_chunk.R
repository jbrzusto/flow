##
##
## animate a chunk of the currently saved raw image data in polar form
##

animate_polar_chunk = function(ij, tk = FALSE) {
  ## ij is a pair (i, j) giving the column and row of the chunk)

  p = getChunk(P, ij, PolarChunkDim)

  ## get range for this chunk
  r = ((1:nrow(p)) + (ij[1] - 1) * PolarChunkDim[1]) * PolarUnit[1]

  ## y limits for plotting (scaled power units)
  ylim = c(0.5, 1.5)

  ## x limits for plotting (range in metres)
  xlim = range(r)

  ## location for timestamp
  tsloc = c(mean(xlim), ylim[1]+diff(ylim)*0.1)

  ## scale for tk plots, in pixels per scaled power unit; plotted perpendicular
  ## to the pulse
  tkscale = 50
  
  id = 0
  for (i in 1:5) {
    ## create the initial plot
    plot( 0,
         0,
         xlab = "Range (m)",
         ylab = "Power (scaled units)",
         ylim = ylim,
         xlim = xlim
         )

    for(i in 1:TimeSteps) {
      ## wrap i appropriately; P is a ring buffer along the last axis
      ii = 1 + (OldestSweepIndex + (i - 1) - 1) %% TimeSteps

      ## erase previous curve, if any
      rect(xlim[1], ylim[1], xlim[2], ylim[2], col="white", border="white")

      ## draw new curve
      points(r, p[, ii], type="l", col="black")

      ## title
      text (t(tsloc), labels = format(LastAddedSweepTS - (TimeSteps - i) * TimeStep, "%Y-%m-%d %H:%M:%S GMT"),)

      ## time sweep indication
      tsx = xlim[1] + diff(xlim) * (i / TimeSteps)
      
      points (rep(tsx, 2), tsloc[2] - diff(ylim) * c(0.05, 0.10), col="green", type="l")

      if (tk) {
        th = pi/2 - rad(GUI$north.angle + 360 * (ij[2] - 0.5) / PolarDim[2])
        x = GUI$plot.origin[1] + cos(th) / GUI$mpp * r - sin(th) * tkscale * p[, ii]
        y = GUI$plot.origin[2] + sin(th) / GUI$mpp * r + cos(th) * tkscale * p[, ii]
        tcl(GUI$plot, "delete", id)
        id = tclint(GUI$plot, "create", "line", c(rbind(x, y)), fill="red")
        tcl("update")
      }
  
      ## pause
      Sys.sleep(0.1)
    }
  }
}
