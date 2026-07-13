void printf(char const*,...);
unsigned int d2u(double d)
{
  return (unsigned) d;
}

int main()
{
  printf("\n>>%x\n\n", ((~0U) >> 1));
  //printf("\n>>%x\n\n", 0x7fffeFFFF);
  //printf("\n>>%x\n\n", 0x80000000);
//if (d2u((double)((~0U) >> 1)) != (~0U) >> 1)        /* 0x7fffffff */
//  return -1;
  return 0;
}
