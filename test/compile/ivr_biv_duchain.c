void daxpy();
void dgefa()
{
  int j,k,n;
  for (k = 0; k < 100; k++) {
      for (j = 0; j < n; j++) {
        daxpy();
      }
  }
}
