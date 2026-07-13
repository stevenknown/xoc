double ddot();
double daxpy();
void dgesl(double b[], int job)
{
  int k,kb,nm1;
  if (job)
  {
      for (k = 0; k < nm1; k++) {
        daxpy();
      }
  }
  else
  {
      for (kb = 1; kb < nm1; kb++) {
        b[k] = ddot();
      }
  }
}
