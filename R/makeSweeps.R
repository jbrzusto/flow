## makeSweeps:
##
## generate simulated radar sweeps of data corresponding
## to a plane wave.  The radar parameters are:
##
## ns: number of sweeps
## np: number of pulses in sweep
## spp: samples per pulse of sweep
## dr: range cell size (range covered by one sample)
## dt: time taken for entire sweep

## k, omega, phi: parameters for the planeWave function

makeSweeps = function(ns, np, spp, dr, dt, k, omega, phi) {
  
  rv = array(dim=c(spp, np, ns))

  ## generate x, y coordinates of samples

  r = (1:spp) * dr
  th = pi / 2 - (0:(np - 1)) * (2 * pi / np)
  x = outer(r, th, function(r, th) r * cos(th))
  y = outer(r, th, function(r, th) r * sin(th))

  xy = cbind(c(x), c(y))
  ## generate times of samples for one sweep (we ignore the fast time
  ## axis; all samples in the pulse are treated as being from the same
  ## time).  These are just theta scaled to a sweep interval and replicated

  t = rep(th * (dt / (2 * pi)), each=spp)

  for (i in 1:ns) {
    rv[,,i] = planeWave(xy, t + (i-1) * dt, k, omega, phi)
  }
  return(rv)
}
