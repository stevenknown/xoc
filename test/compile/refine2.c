volatile float gf[100];
void foo(int n)
{
  volatile double *pd;
  volatile float *pf;
  for (int i = 0; i < 1; i++) {
    pf = gf;
    *(pf++) = n;
    *pf = n;
    *pf = n;
  }
}
