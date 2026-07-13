typedef struct img_par
{
  int mpr[16][16];
} ImageParameters;
void itrans_sp_chroma(struct img_par *img)
{
  int i,j;
  for (j=0; j < 8; j++)
    for (i=0; i < 8; i++)
    {
      img->mpr[i][j]=0;
    }
}
