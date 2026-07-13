int cof[4][6][4][4];
int sign(int a , int b);
void itrans_sp_chroma()
{
  int i,j,n2,n1,j1;
  int m5[4];
  int predicted_chroma_block[16][16],mp1[4];
  for (n2=0; n2 <= 4; n2 += 4)
  {
    for (j=0; j < 4; j++)
      predicted_chroma_block[n1+3][j]=m5[3];
    for (i=0; i < 4; i++)
      for (j=0; j < 2; j++)
        m5[j]=predicted_chroma_block[j1][n2]-predicted_chroma_block[j1][n2];
  }
  mp1[3]=predicted_chroma_block[0][0];
  for (n2=0; n2 < 2; n2 ++)
    mp1[n1]=sign(n2,0);
  for (j=0;j< 4; j++)
    cof[n1][n2][i][j] = sign(j,0);
  cof[1][5][0][0]=mp1[0];
}
