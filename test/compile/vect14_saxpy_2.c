void saxpy_vec(unsigned n, unsigned m, const float a, const float * restrict x,
               float * restrict y) {
  for (unsigned w = 0; w < m; w+=32) {
    for (unsigned i = 0; i < n; i++) {
      y[i] = a * x[i] + y[i]; //can be vectorized.
    }
    x += 32;
    y += 32;
  }
}
