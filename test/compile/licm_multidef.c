void itrans_sp_chroma()
{
  int i,n2,j1;
  int m5[4];
  int predicted_chroma_block[16][16];
  for (n2=0; n2 <= 4; n2 += 4)
    for (i=0; i < 4; i++)
    {
      predicted_chroma_block[j1][n2]=m5[0];
      predicted_chroma_block[j1][n2]=m5[3]-m5[2];
    }
}
