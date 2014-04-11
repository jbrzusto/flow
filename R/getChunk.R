## getChunk:
## return a chunk from an n-dimensional array, where the chunks
## are non-overlapping pieces of size 'dim'.
## x: n-dimensional array
## i: chunk index: vector of length <= length(dim(x)), with
##    i <= i[j] <= dim(x)[j]
##    if i has fewer than n elements, it is extended with ones
##    to length n
## dim: dimensions of chunk.  dim[j] <= dim(x)[j] for j=1..n
## if 
getChunk = function(x, i, dim) {

  ## indexes of 'lower' and 'upper' corners of chunk in larger array
  if (length(i) < length(dim))
    i = c(i, rep(1, length(dim)-length(i)))
  
  l = 1 + (i-1) * dim
  u =       i   * dim

  ## ugly switch to make this work for up to n dimensions
  ## FIXME: n is only 3 right now
  
  return (switch (length(dim(x)),
                  
                  x[l:u],      #1D
                  
                  x[l[1]:u[1], #2D
                    l[2]:u[2]],
                  
                  x[l[1]:u[1], #3D
                    l[2]:u[2],
                    l[3]:u[3]]

                  ))
}
