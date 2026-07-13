void matmul_auto_vec(double *a, double *b, double *c, int n, int m, int o) {
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < m; ++j) {
     // c[i * n + j] = 0;
      for (int k = 0; k < o; ++k)
        c[i * n + j] += a[i * n + k] * b[j * m + k];
    }
}
