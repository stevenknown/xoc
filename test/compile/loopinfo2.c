int mpr[16][16];
void foo()
{
  int js[2][2];
  for (int j=0;j<2;j++)
  {
    for(int i=0;i<2;i++)
    {
      switch (i)
      {
      case 0:
        for (int ii=0; ii<4; ii++)
            mpr[ii+i][j+j]=js[i][j];
        break;
      }
    }
  }
}
