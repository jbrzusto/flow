##
## polar2cart.R: project data from polar coordinates to cartesian ones,
## interpolating appropriately
##
## p: matrix of data in polar form.  Each column represents an azimuth,
##   each row a range slot.  Azimuth is assumed to span the circle.
##
## rangeScale: scalar; linear units in output per range bin in input
##  e.g. if input data have range resolution of 5 metres, and
##       output data are a 10m cartesian grid, then rangeScale = 2
##
## outDim: 2-vector; dimensions of output matrix (rows for y, columns for x)
##
## origin: 2-vector; location of polar origin in units of output dimensions
## e.g. if outDim = c(1001, 1001) and the polar origin is in the centre,
## then origin = c(501, 501)
##
## Maps output cartesian coordinates back to polar and uses bicubic
## interpolation (from the akima package) of polar z values there.
## Does *not* interpolate across the cut at theta=0, since this
## represents a time jump of ~2.4 seconds.  Portions of the output
## grid not covered by the input data are set to 0.

polar2cart = function(p, rangeScale, outDim, origin = (outDim + 1) / 2, doPlot=FALSE) {
  ## generate theta and r coordinate axes for input matrix, given

  theta_in = (2 * pi / ncol(p)) * 0:(ncol(p)-1)  ## theta = 0 is North (up)
  r_in = 1:nrow(p)

  ## x and y coordinates of output grid:
  nx = rep(rangeScale * ((1:outDim[1]) - origin[1]), times=outDim[2])
  ny = rep(rangeScale * ((1:outDim[2]) - origin[2]), each =outDim[1])

  ## pullback to polar coordinates
  
  nth = (pi/2 - atan2(ny, nx)) %% (2 * pi)
  nr = sqrt(nx^2+ny^2)

  ## keep track of which locations are covered by input data
  keep = which(nr <= nrow(p) / rangeScale)

  ## for those, use bicubic interpolation in the polar coordinates
  ## to get a z value
  z = bicubic(r_in, theta_in, p, nr[keep], nth[keep])$z
  
  rv = matrix(0, outDim[1], outDim[2])
  rv[keep] = z
  if (doPlot)
    image(rv, useRaster=TRUE)
  return(rv)
}
