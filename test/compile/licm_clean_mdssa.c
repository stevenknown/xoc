typedef struct img_par
{
  int number;
  int mpr[16][16];
  int m7[16][16];
  int cof[4][6][4][4];
} ImageParameters;
extern ImageParameters *img;
void itrans_sp(struct img_par *img,
               int ioff,
               int joff,
               int i0,
               int j0)
{
  int i,j,i1,j1;
  int m5[4];
  int m6[4];
  for (j=0; j< 4; j++)
    for (i=0; i< 4; i++)
      img->mpr[i][j]=img->mpr[i+ioff][j+joff];
  for (j=0; j < 4; j++)
      i1=3-i;
  for (i=0; i < 4; i++)
      j1=3-j;
  for (j=0;j<4;j++)
  {
    for (i=0;i<4;i++)
      m5[i]=img->cof[i0][j0][i][j];
    for (i=0;i<2;i++)
      img->m7[i1][j]=m6[i]-m6[i1];
  }
  for (i=0;i<4;i++)
    m5[j]=img->m7[i][j];
}
