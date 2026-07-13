void foo (unsigned int n, unsigned char *z)
{
  float c[2048][4];
  unsigned int i;
  for (i = 0; i < n; i++) {
    z[i] = c[i][2];
  }
}
