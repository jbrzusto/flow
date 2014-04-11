## planeWave:
## return the wave height of a unit plane wave with parameters
## - k (wave number in radians per metre along x and y axes)
## - omega (phase velocity in radians per second)
## - phi phase shift (i.e. phase at x = y = t = 0)
## at points given by:
## - xy: n by 2 matrix of space coordinates
## - t: time corresponding to space coordinates

planeWave = function(xy, t, k, omega, phi) {
  return (cos(xy %*% k + omega * t + phi))
}

