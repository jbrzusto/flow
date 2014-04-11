##
## estimate_radial_currents.R - function to estimate radial components
## of currents from raw radar data, according to Airy wave theory.
## Quick and dirty large-depth version that only uses the largest peak in
## the spectrum, since we're just estimating a single parameter.

estimate_radial_currents = function() {

  ## compute the DFT of P using PolarPlan, then scale, take
  ## modulus and square to get power spectrum.  The 'start' parameter
  ## serves to rotate the input matrix so that the oldest sweep is at
  ## the front of the array.

  Ph <<- MR2CFFT(P, PolarPlan, start = OldestSweepIndex) / sqrt(prod(PolarChunkDim))

  ## sum FFT across pulses in a bin.  Due to silly R restriction on 'numeric' (non-complex)
  ## argument to rowsum (and hence its caller groupsum), we split this into real and imaginary components
  ## then recombine after summing.
  
  BinPh <<- groupsum(Re(Ph), group=PolarPulseBinNum, dim=2, reorder=FALSE) + groupsum(Im(Ph), group=PolarPulseBinNum, dim=2, reorder=FALSE) * (1i)

  ## Power spectrum

  PS <<- Mod(Ph)^2

  ## take sums of spectra across pulse bins
  ## First, assign a bin number to each pulse, according to number of pulses
  ## and angular resolution.

  ## calculate sums across pulses (i.e. dim = 2) in each bin
  BinPS <<- groupsum(PS, group=PolarPulseBinNum, dim=2, reorder=FALSE)
  
  ## number of chunks in the pulse-binned Polar Fft
  n = prod(PolarNumBinnedChunks)

  ## define a dataframe to accumulate model results
  PolarRes <<- data.frame(
    row  = integer(n),
    col  = integer(n),
    u    = numeric(n),
    k    = numeric(n),
    omega = numeric(n),
    cMax1 = numeric(n),
    cMax2 = numeric(n),
    cMaxRef1 = numeric(n),
    cMaxRef2 = numeric(n)
    )

  PolarGoodPowers <<- list()
  ## for each chunk, perform the estimation
  
  for (i in 1:n) {
  ## get rectangular coordinates of i-th chunk
    chunkI = coords(i, PolarNumBinnedChunks)

    ## extract fft results for this chunk
    ph = getChunk(BinPS, chunkI, PolarFftChunkDim)

    ## no energy in this chunk (presumably it is outside
    ## the circle of radar data).
    
    if (max(ph) == 0)
      next
  
    ## get the spectral maximum away from \omega = 0 (i.e. drop the 1st element
    ## along the \omega axis of ph)
    
    iMax = which.max(ph[ , -1])

    ## get coordinates of that max, relative to the full matrix (i.e. add 1 to \omega index)
    cMax = coords(iMax, dim(ph) - c(0, 1)) + c(0, 1)

    ## try to refine the estimate by averaging a bunch of quartic curves passing
    ## through the coarse estimate

    cMaxRef = estimate2dpeak(ph, cMax)

    ## map omega axis so we get both positive and negative values
    if (cMaxRef[2] > TimeSteps / 2)
      cMaxRef[2] = cMaxRef[2] - TimeSteps

    ## convert cMax from indices to real units

    k = (cMaxRef[1] - 1) * (2 * pi / (PolarChunkDim[1] * PolarUnit[1])) ## convert k to radians per metre
    omega = (cMaxRef[2] - 1) * (2 * pi / (PolarChunkDim[3] * TimeStep))     ## convert w to radians per second

    ## These are our best estimate of (k, \omega) for the strongest peak.

    ## gravitational acceleration constant g (could be made to depend on Lat/Lon)
    g = 9.8 ## m/s^2

    ## so u is best estimated as (\omega - sqrt(g * k)) / k  (deep water approximation)
    
    uEst = (omega - sqrt(g * abs(k))) / k
    
    PolarRes[i,] <<- c(chunkI, uEst, k, omega, cMax, cMaxRef)
  }

  ## Plot a velocity field
  plot_radial_velocity_field(PolarRes)

}
