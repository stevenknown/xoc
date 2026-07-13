int mpr[16][16]; //<! predicted block
void itrans_sp_chroma()
{
  int i,j;
  int predicted_chroma_block[16][16];
  for (j=0; j < 16; j++)
    for (i=0; i < 16; i++)
    {
      predicted_chroma_block[i][j]=mpr[i][j];
      mpr[i][j]=0;
    }
}
