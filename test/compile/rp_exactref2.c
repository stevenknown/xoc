double gd[100];
float gf[100];
void foo ()
{
  double d00 , d10;
  float f00 , f10 , f20;
  double *pd;
  float *pf;
  for (int i = 0; i < 1; i++)
  {
    pf = gf; f00 = *(pf++), f10 = *pf, f20 = *pf;
    pd = gd;
    d10 = *(pd++);
    d00 = *(pd++);
    pf = gf;
    *pf = f00;
  }
}
