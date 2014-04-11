##
## zero_pad.R
##
zero_pad = function(m, n, causal=FALSE) {
  ## embed a array centred in a larger zero array
  ## m: input array
  ## n: padding factor.  The returned array has dimensions
  ##    (2*n+1) * dim(m)
  ## If causal == TRUE, the original array is copied to the centre dim(m)-cell
  ## of the new array.
  ## If causal == FALSE, the original array is copied
  ## to the origin of the new array, except that elements at
  ## indices with 'negative' values (i.e. larger than half the
  ## corresponding axis size) are wrapped to the negative arm
  ## of the corresponding axis.

  ## create zero return matrix
  dm = dim(m)
  dM = (2*n + 1) * dm
  M = array(0, dim = dM)
  
  ## generate coordinates for each element of array m
  ## shifted to centre of new array M
  N = prod(dim(m))
  i = coords(1:N, dm)
  if (causal) {
    i = t(t(i) + n * d,)
  } else {
    dm2 = ceiling(dm / 2)
    for (j in 1:length(dm)) {
      neg = i[,j] > dm2[j]
      i[,j][neg] = i[,j][neg] - dm[j] + dM[j]
    }
  }

  ## copy original array into location in new array
  M[i] = m

  return(M)
}
