void scalar(int i, int j, double * restrict a, double * restrict b,
            double * restrict c, int n, int m, int o) {
    for (int k = 0; k < o; ++k) {
        c[i * n + j] = c[i * n + j] + (a[i * n + k] * b[j * m + k]);
    }
}
