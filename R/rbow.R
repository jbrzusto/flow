##
## rbow: plot a filled rainbow-coloured contour plot
##
rbow = function(x) {
  filled.contour(x, color=rainbow, nlevels=64)
}
