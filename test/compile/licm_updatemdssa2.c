int cof[4][6][4][4];
void itrans_2()
{
  int i,j,j1;
  int M5[4];
  int M6[4];
  for (j=0;j<4;j++)
  {
    for (i=0;i<4;i++)
      M5[i]=cof[i][j][0][0];
    M6[1]=M5[0]-M5[2];
    M6[2]=M5[1]-M5[3];
    M6[3]=M5[1]+M5[3];
  }
  for (i=0;i<4;i++)
  {
    for (j=0;j<4;j++)
      M5[j]=cof[i][j][0][0];
    M6[2]=M5[3];
    cof[i][j][0][0]= M6[j1];
  }
}
