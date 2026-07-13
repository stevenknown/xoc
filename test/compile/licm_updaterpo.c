int cof[4][6][4][4];
void itrans_2()
{
  int i,j;
  int M5[4];
  int M6[4];
  for (j=0;j<4;j++) {
    for (i=0;i<4;i++)
      M5[i] = cof[i][j][0][0];
    for (i=0;i<2;i++)
      cof[i][j][0][0] = M5[i];
  }
}
