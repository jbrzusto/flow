## getWindow: given a windowing function for 1 dimension, return the array
## of multipliers for a multidimensional chunked window.
##
## winFun: a windowing function e.g. 'hamming' that accepts N
## and returns coefficients w(n), n = 0..N-1
##
## dim: dimensions of the full array
## chunkDim: dimensions of the chunks on which FFT will done
##
## returns the product of the 1-D window functions along each axis of
## chunkDim, replicated so that the result has dimensions dim, and
## can be multiplied element-wise by an array before running the FFT

getWindow = function(winFun, dim, chunkDim) {
  ## get the windowing function along the first axis, and replicate
  ## within and across chunks
  rv = repAlong(winFun(chunkDim[1]), dim, 1)
  
  for(i in 2:length(dim))
    ## for each subsequent dimension, multiply by the replicated
    ## windowing function along that dimension
    rv = rv * repAlong(winFun(chunkDim[i]), dim, i)
  return(rv)
}
