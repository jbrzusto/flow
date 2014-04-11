plot_radial_velocity_field = function(res, ts = RSS$scan.info$ts) {
  
  range = res$row * RSS$scan.info$sample.dist * PolarChunkDim[1]
  theta = pi / 2 - (res$col - 0.5) * (2 * pi / dim(BinPS)[2])  ## Note clockwise sweep order, starting from up.
  
  ## points at each chunk centre
  with(res,
       {
         maxRange = RSS$scan.info$samples.per.pulse * RSS$scan.info$sample.dist
         
         plot(range*cos(theta), range*sin(theta), xlim=c(-maxRange - 50, maxRange + 50), ylim = c(-maxRange - 50, maxRange + 50), pch=".",
              xlab="Easting (metres)", ylab="Northing (metres)", main=format(ts, "Radial velocity component at %Y-%m-%d %H:%M:%OS3"))
       }
       )

  ## velocity vectors for those chunks with a reasonable model fit
  with (res,
        {
          arrows(range*cos(theta), range*sin(theta), cos(theta) * (range + u*20), sin(theta) * (range + u*20), length=0.05)
        })
}
