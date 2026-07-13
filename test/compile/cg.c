void daxpy(double dx[]);
void dgefa(double a[][201], int lda, int n, int ipvt[])
{
  double col_k[128];
  double t;
  int j,k,l;
  for (k = 0; k < 100; k++) {
    col_k[1] = a[1][1];
    l = 1;
    ipvt[1] = 0;
    if (col_k[0] != 0) {
      if (l != k) {
        col_k[l] = col_k[1];
        col_k[0] = t;
      }
      t = -1.0/col_k[1];
      for (j = 0; j < n; j++) {
        daxpy(col_k);
      }
    }
  }
}
