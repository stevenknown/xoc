void foo()
{
  float c[2048][4];
  unsigned char *d;
  unsigned int i;
  do {
    float rr;
    rr = c[i][2];
    d[i] = rr;
  } while (0);
}
