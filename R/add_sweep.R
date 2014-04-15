##
## Add the latest sweep and reprocess current estimates.
##
##
## This function accumulates the current sweep into the global variables,
## re-runs the estimator, and generates output.
## It can be added as a hook function like so:
## rss.add.hook("FULL_SCAN", add_sweep)

## Uses global variables defined in init.R


add_sweep = function(...) {

  ## Don't add this sweep if it has already been added
  ## (radR sometimes process a sweep twice, once for user preview)
  
  ts = RSS$scan.info$timestamp
  if (LastAddedSweepTS == ts)
    return()

  LastAddedSweepTS <<- ts

  ## Which slice of the sweep buffer are we inserting into?
  ## We cycle through the sweep buffer, wrapping back to 1
  ## every TimeSteps sweeps.  This is much more efficient than
  ## shifting data from all previous sweeps.

  TSIndex <<- 1 + (NumSweeps %% TimeSteps)

  ## fix the range dependence of data.  We want to remove the
  ## 1/r^4 scaling of echo strength resulting from the radar equation,
  ## as well as the effect of seeing echos from cells whose size
  ## scales with r, and any other unknown but systematic range-dependency.
  ## Otherwise, the Fourier spectrum will be polluted by these effects,
  ## in a non-uniform manner across the sweep (e.g. cells where wave
  ## motion is primarily across the radar beam, vs. those where it is
  ## large along the radar beam).

  rawFrame = correct_range_dependence(RSS$scan.mat[])

  ## rescale the range corrected data so they fit conveniently in 12 bits,
  ## to match the original dynamic range of the data.  This is for convenience
  ## in 'scan conversion', the mapping from polar to rectangular coordinates.
  ## Its effect on the Fourier spectrum is: arbitrary change to the 0-frequency
  ## coefficient, and an arbitrary uniform scaling of the remaining coefficients.
  ## Hence, it has no effect on the current estimation algorithm.
  
  scaledRange = range(rawFrame)
  scale = diff(scaledRange)

  ## store the rescaled and range-corrected matrix back in
  ## radR's usual location (so it can be displayed etc.)
  
  RSS$scan.mat[] = (rawFrame - scaledRange[1]) * (4095 / scale)

  ## FIXME: if we figure out anything useful to do with the original
  ## raw polar data, this is the place to add it to the polar ring buffer
  ## like so:
  ## P[,,TSIndex] <<- rawFrame
  
  ## Note: I was going to use raw polar data, both as a check on the
  ## output of the Cartesian model, and because it could skip the potentially
  ## lossy and polluting scan conversion step, but this poses a
  ## problem: each peak in the range vs. time 2D fourier spectrum
  ## corresponds to a wave moving in an unknown direction relative to
  ## the radar beam at that point.  The effect is to decrease the
  ## apparent k value for the peak by the unknown factor cos(\theta),
  ## where \theta is the angle between the planar wave normal and the
  ## radar beam axis.  Because the apparent \omega value is unchanged,
  ## this adds one parameter to be estimated for each peak in the
  ## spectrum (the waves corresponding to different peaks are not
  ## necessarily moving in the same direction).  The way around this
  ## problem is probably to use how the phase of the peaks changes
  ## in the range vs. time 2D fourier spectrum from one pulse azimuth
  ## to the next, but I wasn't able to come up with a way to use this.


  ## use the most recent TimeStep.  FIXME: this should probably be
  ## the mean for the sweeps in the buffer.  In practice, this should
  ## make little difference as the variability in sweep duration for
  ## most marine radars is low, even under high wind load.
  
  TimeStep <<- RSS$scan.info$duration / 1000
  
  ## convert sweep to Cartesian coordinates
  ## (The call returns the scan converter object, which we save in CartConverter)
  ## FIXME: use a proper projection library here.
  
  CartConverter <<- .Call("radR_convert_scan",
                          RSS$scan.mat,  ## input array
                          Cart,   ## output array
                          CartClass,
                          CartPalette,
                          dim(Cart)/2, ## radar location in cart matrix
                          rss.planar.rps() / CartUnit,  ## 'pixels' per sample
                          0, ## angle offset, in degrees clockwise from north
                          c(0L, 0L), ## bit shift values for sample, class
                          1L, ## class is visible,
                          CartConverter,
                          c(0, 0, dim(Cart)),  ## geometry of output
                          0, ## number of samples to skip in radial direction (for trigger delay adjustment)
                          FALSE,
                          PACKAGE="radR")

  ## ## insert data into the Cartesian sweep buffer
  G[,,TSIndex] <<- Cart[] + 0.0

  ## increase sweep counter
  NumSweeps <<- NumSweeps + 1 
  
  ## The index of the oldest sweep in the sweep arrays,
  ## for use by the fft code.  This is the index right
  ## after TSIndex, except we need to wrap back to 1
  ## when TSIndex == TimeSteps.
  
  OldestSweepIndex <<- 1 + (TSIndex %% TimeSteps)

  ## if we have a full deck, estimate currents
  if (NumSweeps >= TimeSteps) {
    estimate_currents()
##    estimate_radial_currents()
  }
  
}





