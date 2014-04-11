##
## utility functions
##

## Model raw power values as a A1 + A2 * range + A3 * log(range)

PowerCoeff <- NULL

plot_mean_power_vs_range = function(...) {
  pwr = apply(RSS$scan.mat[], 1, mean)
#  pwr = apply(RSS$scan.mat[], 1, quantile, 0.5)
  d = data.frame(r=RSS$scan.info$sample.dist * (1:RSS$scan.info$samples.per.pulse),
    pwr=pwr)
  d = d[-(1:100),]
  res <<- lm(pwr~r+log(r), d)
  print(res)
  PowerCoeff <<- rbind(PowerCoeff, res$coefficients[1:3,1])
  plot(d$r, d$pwr, type="l")
  points(d$r, res$coefficients[1, 1] + res$coefficients[2, 1]*d$r + res$coefficients[3, 1]*log(d$r), type="l", lty=2, col="red")
}

lsq2d = function(mat, ind) {
  if (ind[1] == 1 || ind[1] == nrow(mat) || ind[2] == 1 || ind[2] == ncol(mat))
    return(NULL)

  ## fit this 6 parameter model to the data at ind and neighbouring 8 cells
  ## z = a20*x^2 + a02*y^2 + a11*x*y + a10*x + a01*y + a00

  ## return the estimated location of the max, and the max value.

  v = function(i, j) mat[t(ind)+c(i, j)]
  
  ## The least squares fit for the 9 function values including and surrounding the
  ## cell at ind is given in exact form (courtesy of maxima) by:

  return(list (
    a20 = (v(1,1)+v(1,0)+v(1,-1)-2*v(0,1)-2*v(0,0)-2*v(0,-1)+v(-1,1)+v(-1,0)+v(-1,-1))/6,
    a02 = (v(1,1)-2*v(1,0)+v(1,-1)+v(0,1)-2*v(0,0)+v(0,-1)+v(-1,1)-2*v(-1,0)+v(-1,-1))/6,
    a11 = (v(1,1)-v(1,-1)-v(-1,1)+v(-1,-1))/4,
    a10 = (v(1,1)+v(1,0)+v(1,-1)-v(-1,1)-v(-1,0)-v(-1,-1))/6,
    a01 = (v(1,1)-v(1,-1)+v(0,1)-v(0,-1)+v(-1,1)-v(-1,-1))/6,
    a00 = (-v(1,1)+2*v(1,0)-v(1,-1)+2*v(0,1)+5*v(0,0)+2*v(0,-1)-v(-1,1)+2*v(-1,0)-v(-1,-1))/9
    ))
}

estimate2dpeak = function(mat, ind) {
  ## estimate a 2d peak at location ind of 2d FFT matrix mat
  ## (mat is complex)
  mag = sqrt(Mod(mat))
  ph = Arg(mat)

  ## find the least-squares 2nd order surface fitting mat in the 9 cells
  ## centred at ind, if possible
  
  lss = lsq2d(mag, ind)
  if (is.null(lss) || lss$a02 > 0 || lss$a20 > 0)
    return(c(ind, mat[t(ind)]))

  ## use this least squares solution to calculate the location and value of the peak
  ##
  ## then if a02 <= 0 and a20 <= 0, there's a max at
  ## 
  ##   x = (a01*a11-2*a10*a02) / d, y = (a10*a11 - 2*a01*a20) / d
  ## 
  ## where
  ##
  ##   d = 4 * a02 * a20 - a11^2
  ##
    
  est = with(lss, {
    d = 4*a02*a20 - a11^2
    x = (a01*a11 - 2*a10*a02) / d
    y = (a10*a11 - 2*a01*a20) / d
    z = a20*x^2 + a02*y^2 + a11*x*y + a10*x + a01 * y + a00
    c(x+ind[1], y+ind[2], z)
  })

  ## unwrap the phase at the 9 cells centred on the one containing
  ## the peak, and estimate the phase there
  ## A bit kludgy, since we only unwrap each phase relative to the
  ## one in the centre cell.

  uph = ph[ind[1] + (-1:1), ind[2] + (-1:1)]
  for (i in c(1:4, 6:9)) {
    d = uph[i] - uph[5]
    if (d >= pi)
      uph[i] = uph[i] - 2*pi
    else if (d < -pi)
      uph[i] = uph[i] + 2*pi
  }

  lss = lsq2d(uph, c(2,2))

  ## phase at estimated peak location

  x = est[1] - ind[1]
  y = est[2] - ind[2]

  estph = with(lss, a20*x^2 + a02*y^2 + a11*x*y + a10*x + a01 * y + a00)

  est[3] = est[3]^2 * exp(1i * estph)

  return(est)
}

  ##
  ## test to verify this function works:
  ## m = matrix(nrow=10,ncol=10)
  ## xc = 3.1415926
  ## yc = 2.7182818
  ## f = function(x,y) -(x-xc)^2-(y-yc)^2+1.6789*(x-xc)*(y-yc)+5.98765
  ## mm = f(row(m), col(m))
  ## mx = coords(which.max(mm), dim(mm))
  ## estimate2dpeak(mm, c(mx))
  ## mm = jitter(mm, factor=10)
  ## mx = coords(which.max(mm), dim(mm))
  ## estimate2dpeak(mm, c(mx))




polar_fft_bin_to_real = function(ind) {
  ## given the origin-1 index (i, j) into a polar fourier chunk,
  ## convert it to real units (radians / metre, radians per second)
  k = (ind[1] - 1) * (2 * pi / (PolarChunkDim[1] * PolarUnit[1])) ## convert spatial bin index to radians per metre
  w = (ind[2] - 1) * (2 * pi / (PolarChunkDim[3] * TimeStep))   ## convert time bin index to radians per second
  return (c(k, w))
}

  
