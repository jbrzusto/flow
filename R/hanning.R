## hanning window
##
## w(n) = 0.5\; \left(1 - \cos \left ( \frac{2 \pi n}{N-1} \right) \right)
##
## (from wikipedia entry "Windowing Function")

hanning = function(N) {
  n = 0 : (N - 1)
  return (0.5 * (1 - cos(n * (2 * pi / (N - 1)))))
}

  
