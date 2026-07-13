int g;
void scalar(int i, int j, double * restrict a, double * restrict b,
            double * restrict c, int n, int m, int o) {
    int w,u;
    for (int k = 0; k < o; ++k) {
        int t1 = i * n;
        int t2 = j * m;
        int t3 = t1 + k;
        int t4 = t2 + k;
        a[t3] = b[t4];
    }
}
