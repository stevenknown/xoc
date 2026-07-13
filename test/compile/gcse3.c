void mat(double *a, double *b, double *c,
         int n, int m, int o) {
    int i1;
    int i2;
    int n1;
    int n2;
    int j1;
    int j2;
    int i,j;
    for (int k = 0; k < o; ++k) {
        //i1 * n1 + j1 and i2 * n2 + j2 are CSE.
        c[i1 * n1 + j1] = c[i1 * n1 + j1] +
            a[i * n + k] * b[j * m + k];
    }
}
