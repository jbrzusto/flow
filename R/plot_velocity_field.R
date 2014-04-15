plot_velocity_field = function(res, minrsquared=0.9, arrowscale=15) {
  ## points at each chunk centre
##  png(sprintf("velplot%05d.png", NumPlots), width=800, height=800)
  with(res,
       plot(row, col, xlim=c(0, 1 + NumChunks[1]), ylim=c(0, 1 + NumChunks[2]), pch=".")
       )

  ## velocity vectors for those chunks with a reasonable model fit
  with (subset(res, rsquared >= minrsquared),
        {
          arrows(row, col, row + ux/arrowscale, col + uy/arrowscale, length=0.1, col=rainbow(16)[log(1/(1-rsquared))])
        })
##  dev.off()
  if (exists("NumPlots"))
    NumPlots <<- NumPlots + 1
  else
    NumPlots <<- 1
}
