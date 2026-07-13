struct img_par{
  int mpr[16][16];
};
void itrans_sp_chroma(struct img_par *img)
{
  int i,j,j2;
  int m5[4];
  int pre[16][16];
  for (j=0; j < 16/2; j++)
    for (i=0; i < 16/2; i++)
      img->mpr[i][j]=0;
  for (j=0; j < 2; j++)
  {
    j2=3-j;
    m5[j]=pre[i][j]-pre[i][j2];
  }
}
