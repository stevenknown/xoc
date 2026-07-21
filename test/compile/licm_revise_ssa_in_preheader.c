typedef struct img_par
{
  int mpr;
  int m7[16][16];
  int cof[4][6][4][4];
} ImageParameters;
void itrans_sp(struct img_par *img,
               int i0,
               int j0)
{
  //Check if mdssainfo is valid after LICM moves all stmt to preheader.
  //Fixup mdssainfo for stmt's USE occ's mdssainfo and DefDef chain.
  int i,j,i1,j1;
  int m5[4];
  int m6[4];
  for (j=0;j<4;j++)
  {
    for (i=0;i<4;i++)
    {
      m5[i]=img->cof[i0][j0][i][j];
    }
    m6[0]=m5[0]+m5[2];
    m6[1]=m5[0]-m5[2];
    m6[2]=m5[1]-m5[3];
    m6[3]=m5[1]+m5[3];
    for (i=0;i<2;i++)
    {
      i1=3-i;
      img->m7[i][j]=m6[i1];
    }
  }
}
