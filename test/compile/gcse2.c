void mat(double *a, double *b, double *c, int n, int m, int o) {
    int i1=1;
    int i2=1;
    int n1=70;
    int n2=70;
    int j1=2;
    int j2=2;
    int i,j;
    for (int k = 0; k < o; ++k) {
        //i1 * n1 + j1 and i2 * n2 + j2 are CSE.
        c[i1 * n1 + j1] = c[i2 * n2 + j2] + a[i * n + k] * b[j * m + k];
    }
}
