float gf[100];
void foo()
{
  float f00, f10;
  float *pf;
  for (int i = 0; i < 1; i++) {
    pf = gf;
    f00 = *(pf++);
    f10 = *pf;
    pf = gf;
    *(pf++) = f00;
    *pf = f10;
  }
}
