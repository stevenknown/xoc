void scalar(int i, int j, double *a, double *b, double *c,
            int n, int m, int o) {
    double ret;
    for (int k = 0; k < o; ++k) {
        ret = c[i * n + j] + (a[i * n + k] * b[j * m + k]);
        c[i * n + j] = ret; //c[i * n + j] is reduce operation.
    }
}
