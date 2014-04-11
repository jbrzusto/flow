## coords:
## convert linear to rectangular array coordinates
## i.e. convert a linear coordinates (origin 1) in an array with dimensions dim
## to matrix with dimensions c(length(i), length(dim))

coords = function(i, dim) {
  rv = NULL
  i = i - 1
  for (j in 1:length(dim)) {
    rv = cbind(rv, i %% dim[j])
    i = i %/% dim[j]
  }
  return(1 + rv)
}

