int cof[4][6][4][4];
void itrans_2()
{
  int i,j,i1;
  int M6[4];
  for (i=0;i<4;i++)
    for (i=0;i<2;i++)
    {
      i1=3-i;
      cof[i1][j][0][0]=M6[i]-M6[j];
    }
}
