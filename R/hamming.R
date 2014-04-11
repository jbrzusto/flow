## hamming window
##     w(n) = \alpha - \beta\; \cos\left( \frac{2 \pi n}{N - 1} \right),
##
## with
##
##     \alpha = 0.53836\, \beta = 1 - \alpha

hamming = function(N) {
  alpha = 0.53836
  beta = 1 - alpha
  n = 0 : (N - 1)
  return (alpha - beta * cos(n * (2 * pi / (N - 1))))
}
