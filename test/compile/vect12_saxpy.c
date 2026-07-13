void saxpy_vec(unsigned n, const float a, const float * x,
               float * y) {
  for (unsigned i = 0; i < n; ++i) {
    y[i] = a * x[i] + y[i]; //can NOT be vectorized because x and y may alias.
  }
}
