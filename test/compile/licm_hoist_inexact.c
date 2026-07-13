void matmul(double *a, double *b, double *c, int n, int m, int o) {
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < m; ++j) {
      for (int k = 0; k < o; ++k)
        //RHS c[i * n + j] should NOT be hoisted.
        //RHS c[i * n + j]'s base expression should be hoisted.
        //LHS base expression should be hoisted.
        c[i * n + j] = c[i * n + j] + a[i * n + k] * b[j * m + k];
    }
}
