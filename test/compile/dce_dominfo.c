int g1;
int **mpr;
char *g2;
void foo()
{
  int j, ii, jj;
  char *img = g2;
  if (g1 == 0)
  {
  }
  for (j=0;j<2;j++) {
      if (g1==2)
      {
        for (ii=0; ii<4; ii++)
          for (jj=0; jj<4; jj++)
            mpr[ii][jj]=10;
      }
  }
}
