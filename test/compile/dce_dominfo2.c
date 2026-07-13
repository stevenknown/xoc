int cof[4][6][4][4];
void itrans_sp_chroma()
{
  int i,j,j2,n2,n1;
  int m5[4];
  int predicted_chroma_block[16][16];
  for (n2=0; n2 <= 4; n2 += 4)
  {
    for (j=0; j < 4; j++)
      predicted_chroma_block[n1][j]=m5[3];
    for (i=0; i < 4; i++)
        m5[i]=predicted_chroma_block[i][n2];
  }
  m5[3]=predicted_chroma_block[0][0];
  for (n2=0; n2 < 2; n2 ++)
  {
  }
  for (j=0;j< 4; j++)
    cof[n1][n2][i][j] = n2;
  cof[n1][4][0][0]=m5[3];
}
