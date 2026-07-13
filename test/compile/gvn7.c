void dgesl(double a[][201], int n, double b[])
{
  double t;
  int kb;
  int k,w,z;
  for (kb = 0; kb < n; kb++) {
    k = kb;
    w = kb+2;
    z = kb+4;
    b[k+z+w] = a[k][k]; //S1
    t = -b[k+z+w]; //S2
    //Note S1's b[k+z+w] is the killing-def of S2's b[k+z+w], even if it
    //does not have any exact MD.
  }
}
