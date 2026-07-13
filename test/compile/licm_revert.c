float gf[100];
void foo()
{
  float f30;
  float *pf;
  int i;
  for (i = 0; i < 1; i++)
  {
    pf = gf;
    f30 = *(pf++), f30 = *(pf++);
    pf = gf;
    *pf = 30 ;
  }
}
