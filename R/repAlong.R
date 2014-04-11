##
## repAlong.R: replicate a vector into an array of given dimensions
##
##
## v: vector
## dim: vector of dimensions of array
## axis: scalar in 1..length(dim) selecting which axis vector should
## be aligned with.
##
## result: array with dimensions 'dim', such that
## 
##    result[i_1 ,i_2 , axis ,i_{n-1} , i_n] == v
##
## for all choices of i_1, i_2, ..., i_n where n = length(dim) is the rank
## i.e. the result is a bunch of copies of v pasted together to
## the correct shape, and aligned along the axis-th dimension
## If necessary, elements from v are recycled to length dim[axis].
## If length(v) > dim[axis], only the first dim[axis] elements of v
## are used.

repAlong = function(v, dim, axis, warn=TRUE) {
  n = length(dim)
  
  if (axis < 1 || axis > n)
    stop("axis out of bounds")
  
  v = rep(v, length=dim[axis])

  ## how many times does each element of v appear consecutively in the
  ## result?  None, if axis = 1.  dim[1] if axis = 2, dim[1]*dim[2] if
  ## axis == 3, etc.
  each = prod(head(dim, axis-1))

  ## how many times does the vector get repeated (after duplicating
  ## each element 'each' times)?
  ## None, if axis = n.  dim[n] if axis = n - 1, dim[n]*dim[n-1] if
  ## axis == n-2, etc.

  times = prod(tail(dim, n - axis))

  ## replicate and return, with appropriate dimensions
  rv = rep(v, each=each, times=times)
  dim(rv) = dim
  return(rv)
}

