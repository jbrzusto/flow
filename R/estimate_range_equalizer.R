##
## estimate_range_equalizer.R
##
## estimate scaling coefficients to make power look uniform across range.
##
## For reasons not entirely clear to me, the following model gives
## excellent fit to mean reflected power (per cell) vs range
## for a uniform-looking wave field:
##
##    d = A + B * r + C * log(r)
##
## where d is the raw 12-bit value from the digitizer, and r is the
## range (in metres).  We fit this model to each sweep, then
## return the scaled power matrix, where the scaling factor is the
## reciprocal of fitted values of the above model.  (i.e. each row in the input
## matrix is divided by the fitted value corresponding to its range).
##

estimate_range_equalizer = function(pwr) {
  ## pwr: array of postive rank whose first axis is
  ## range.

  ## returns estimate of scaling values which can
  ## be multiplied by samples at each range to equalize
  ## power across range.
  
  meanPowerAtRange = apply(pwr, 1, mean)
  r = 1:dim(pwr)[1]
  return (1 / lm(meanPowerAtRange ~ r + log(r))$fitted)
}
