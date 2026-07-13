int pos;
int mpr[16];
int foo(int ioff, int joff)
{
  int i,j;
  switch (ioff) {
  case 2:
    for (j=0; j < 4; j++)
      mpr[j] = joff;
    break;
  case 0:
    for(j=0;j<4;j++)
        mpr[j]=pos;
  }
  return 0;
}
