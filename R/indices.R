## indices:  convert rectangular to linear array coordinates
## i.e. convert matrix of origin 1 coordinates of an array with dimensions dim
## to a vector of origin 1 indices into array's storage

indices = function(coords, dim) {
  return (c(1 + ((coords - 1) %*% cumprod(c(1, head(dim, -1))))))
}

