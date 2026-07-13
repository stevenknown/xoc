void printf(char const*,...);
volatile double gd[100];
void foo()
{
  float f;
  float *pf;
  //pf, gd, i, f, *pf can be promoted.
  pf = gd;
  for (int i = 0; i < 1; i++)
  {
    pf = gd;
    f = *(pf++);
  }
}
