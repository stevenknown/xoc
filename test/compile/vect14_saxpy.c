void saxpy_vec(unsigned m, const float a, const float * restrict x,
               float * restrict y) {
  for (unsigned w = 0; w < m; w+=32) {
    for (unsigned i = 0; i < 32; i++) {
      y[i] = a * x[i] + y[i]; //can be vectorized.
    }
    x += 32;
    y += 32;
  }
}
