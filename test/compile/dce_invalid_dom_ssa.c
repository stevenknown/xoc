void itrans_sp_chroma()
{
  int i,j,n2,n1;
  int m5[4];
  int predicted_chroma_block[16/2][16/2],mp1[4];
  for (n2=0; n2 <= 4; n2 += 4)
  {
    for (j=0; j < 4; j++)
    {
      for (i=0; i < 2; i++)
        m5[i]=predicted_chroma_block[i][j];
      predicted_chroma_block[j][j]=m5[0];
    }
    for (i=0; i < 4; i++)
      for (j=0; j < 2; j++)
        m5[j]=0;
  }
}
