## Exact Blackman window
##
##     w(n)=a_0 - a_1 \cos \left ( \frac{2 \pi n}{N-1} \right) + a_2 \cos \left ( \frac{4 \pi n}{N-1} \right)
##
##     a_0=\frac{1-\alpha}{2};\quad a_1=\frac{1}{2};\quad a_2=\frac{\alpha}{2}\,
##
## with a0 = 7938/18608 ≈ 0.42659, a1 = 9240/18608 ≈ 0.49656, and a2 = 1430/18608 ≈ 0.076849.
## (from wikipedia entry "Windowing Function")

blackman = function(N) {
  a0 = 0.42659
  a1 = 0.49656
  a2 = 0.076849
  n = 0 : (N - 1)
  x = n * (2 * pi / (N - 1))
  return (a0 - a1 * cos(x) + a2 * cos(2 * x))
}
