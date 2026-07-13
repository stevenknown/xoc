void scalar(int i, int j, double * restrict a, double * restrict b,
            double * restrict c,
            int n, int m, int o) {
    double ret;
    for (int k = 0; k < o; ++k) {
        ret = c[i * n + j] + (a[i * n + k] * b[j * m + k]);
        c[i * n + j] = ret; //c[i * n + j] should be promoted.
    }
}
