##
## correct_range_dependence.R
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
## The model coefficients are estimated only once, when the first
## TimeSteps sweeps have been acquired.

correct_range_dependence = function(pwr, estimate=FALSE) {
  ## given an array of positive rank whose first axis represents range
  ## to radar, correct its range dependence if the model has been
  ## estimated, otherwise, return the unscaled power.  if 'estimate'
  ## is TRUE, then estimate the model from the current data before
  ## applying it.

  if (estimate && is.null(RangeCorrectionScale)) {
    meanPowerAtRange = apply(pwr, 1, mean)
    r = 1:nrow(pwr)
    RangeCorrectionScale <<- 1 / lm(meanPowerAtRange ~ r + log(r))$fitted
    saveRDS(RangeCorrectionScale, rangeCorrectionFile)
  }

  if (is.null(RangeCorrectionScale))
    return(pwr)
 
  return (pwr * RangeCorrectionScale)
}
