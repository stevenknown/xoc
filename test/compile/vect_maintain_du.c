int pos;
int mpr[16];
int foo()
{
  if (pos == 2)
    for (int j=0; j < 4; j++)
      mpr[j] = pos;
  else
    for(int j=0;j<4;j++)
        mpr[j]=pos;
  return 0;
}
