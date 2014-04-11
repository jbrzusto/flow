##
## estimate_currents.R - function to estimate currents according to Airy wave theory.
##

estimate_currents = function() {
  
  ## compute the DFT of G using CartPlan, then scale, take
  ## modulus and square to get power spectrum.  The third
  ## argument to MR2CFFT indicates where in that array
  ## the first timeslice starts.
  
  Gh <<- Mod(MR2CFFT(G, CartPlan, start = OldestSweepIndex) / prod(ChunkDim)) ^ 2

  ## number of chunks in the Cartesian problem
  n = prod(NumChunks)

  ## define a dataframe to accumulate model results
  res <<- data.frame(
    row  = integer(n),
    col  = integer(n),
    ux   = numeric(n),
    uy   = numeric(n),
    rsquared  = numeric(n)
    )

  goodPowers <<- list()
  ## for each chunk, perform the estimation
  
  for (i in 1:n) {
  ## get rectangular coordinates of i-th chunk
    chunkI = coords(i, NumChunks)

    ## extract fft results for this chunk
    gh = getChunk(Gh, chunkI, FftChunkDim)

    ## no energy in this chunk (presumably it is outside
    ## the circle of radar data).
    
    if (max(gh) == 0)
      next

    ## zero out the non-wave-energy portion of the spectrum.
    ## (array indices start at 1, so these are the 'constant'
    ## fourier bins):
    
    gh[ , ,1] = 0 ## the mean spatial pattern, unchanging in time
    gh[1,1, ] = 0 ## the mean time pattern, unchanging in space
    
    ## get the spectral maximum

    specMax = max(gh)

    ## determine which spectral components have sufficient power
    ## by comparing to threshold times spectral maximum

    thresh = 0.3

    ## in case there are not enough points to run the model, we
    ## (possibly) repeat, relaxing the threshold each time

    enoughPoints = TRUE
    repeat {
      
      ## get a matrix with columns kx, ky, w of high power locations
      goodPower = coords(which(gh >= thresh * specMax), FftChunkDim)
      
      ## if we have at least 3 good points, quit, because we can
      ## estimate 2 parameters with an extra degree of freedom.
      ## Chances are, we'll have many more than this.
      
      ## FIXME: when we start estimating depth, maybe increase the
      ## lower limit to 4 so there's still an extra DOF
      
      if (nrow(goodPower) >= 3)
        break

      ## not enough 'good power points', so relax threshold and retry
      ## but only if threshold is not too small
      
      thresh = thresh * 0.8
      if (thresh < 0.05) {
        enoughPoints = FALSE
        break
      }
    }

    ## if we were not able to get enough points by relaxing the threshold,
    ## skip this chunk
    if (! enoughPoints)
      next

    ## look up corresponding spectral power values, for use in weighting the model
    goodPower = cbind(goodPower, gh[goodPower])

    ## convert bin numbers to origin-0 coordinates
    goodPower[,1:3] = goodPower[,1:3] - 1

    ## convert to data frame and give suitable names to columns
    goodPower = as.data.frame(goodPower)
    names(goodPower) = c("kx", "ky", "w", "power")

    ## convert kx, ky to true wave numbers (i.e. from index
    ## to radians per metre) and calculate k;
    ## convert w to radians per second

    ## gravitational acceleration constant g (could be made to depend on Lat/Lon)
    g = 9.8 ## m/s^2

    goodPower = within(goodPower,
      {
        kx = kx * (2 * pi / (ChunkDim[1] * CartUnit)) ## convert kx to radians per metre
        ky = ky * (2 * pi / (ChunkDim[2] * CartUnit)) ## convert ky to radians per metre
        k  = sqrt(kx^2 + ky^2)
        w  = w * (2 * pi / (ChunkDim[3] * TimeStep))  ## convert w to radians per second
        Omega = sqrt(g*k)                             ## theoretical dispersion

        diff = w - Omega                              ## observed - theoretical dispersion
      }
      )

    ## save in a global variable, for later analysis
    goodPowers[[i]] <<- goodPower
    
    ## fit a linear model to the difference between observed and stationary
    ## omegas; i.e. estimate coefficients for kx and ky (and no constant term)
    ## which will make the formula true in the least squares sense.
    ## We don't necessarily need to weight the model (i.e. find the weighted
    ## least squares solution, where weights simply multiply each term's
    ## residual), but it seems to give smoother results.
    ## It seems this is a more robust approach than trying to estimate each
    ## individual peak more precisely, since nearby bins end up contributing
    ## somewhat to the model, in rough proportion to the energy in the bin.
    
    fit = lm(diff ~ kx + ky + 0, goodPower, weights=power)

    ## save the result for chunk i.  fit$coefficients are the estimates
    ## for u_x and u_y
    
    res[i,] <<- c(chunkI, fit$coefficients, summary(fit)$r.squared)
  }

  ## Plot a velocity field
  plot_velocity_field(res)

}
