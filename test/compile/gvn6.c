void dgesl(double a[][201], int n, double b[])
{
  double t;
  int k,kb;
  for (kb = 0; kb < n; kb++) {
    k = kb;
    b[k] = a[k][k]; //S1
    t = -b[k]; //S2, Note S1's b[k] is the killing-def of S2's b[k], even if it
               //does not have any exact MD.
  }
}
