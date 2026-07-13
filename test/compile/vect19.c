void vec(unsigned n, const float a, const float * restrict x,
               float * restrict y) {
  for (unsigned i = 0; i < n; i++) {
    y[i] = n; //can be vectorized.
  }
  for (unsigned i = 0; i < n; i++) {
    y[i] = 10; //can be vectorized.
  }
}
