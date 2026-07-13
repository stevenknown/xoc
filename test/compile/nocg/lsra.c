volatile double gd[100];
void foo (int n)
{
  double d00, d10, d20, d30, d01, d11, d21, d31, d02, d12, d22, d32, d03, d13, d23, d33, d04, d14, d24, d34, d05, d15, d25, d35, d06, d16, d26, d36, d07, d17, d27, d37 ;
  volatile double *pd;
  volatile float *pf;
  int i;
  for (i = 0; i < 1; i++)
  {
    pd = gd;
    d34 += *pd, d05 += *pd, d15 += *pd, d25 += *pd, d35 += *pd,
    d06 += *pd, d16 += *pd, d26 += *pd, d36 += *pd, d07 += *pd,
    d17 += *pd, d27 += *pd, d37 += *pd;
  }
  *pd = d04, *pd = d14, *pd = d24, *pd = d34;
  *pd = d05, *pd = d15, *pd = d25, *pd = d35;
  *pd = d06, *pd = d16, *pd = d26, *pd = d36;
  *pd = d07, *pd = d17, *pd = d27, *pd = d37;
}
