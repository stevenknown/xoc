void saxpy_vec(unsigned n, const float a, const float * restrict x,
               float * restrict y) {
  for (unsigned i = 0; i < n; i++) {
    y[i] = a * x[i] + y[i]; //can be vectorized.
  }
}
