void itrans_sp()
{
  int i,j;
  int m5[4];
  int predicted_block[4][4];
  for (j=0; j < 4; j++)
  {
    for (i=0; i < 2; i++)
      m5[i]=predicted_block[i][j];
    predicted_block[1][j]=m5[3]*2+m5[2];
  }
}
