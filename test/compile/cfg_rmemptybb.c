typedef struct img_par
{
  int cof[4][6][4][4];
};
extern const int coef[6][4][4];
void itrans_2(struct img_par *img)
{
  int i,j;
  int M5[4];
  int M6[4];
  for (j=0;j<4;j++)
  {
    M5[i]=img->cof[i][j][0][0];
    M6[0]=M5[2];
    M6[2]=M5[1]-M5[3];
  }
  for (i=0;i<4;i++)
  {
    M5[j]=img->cof[i][j][0][0];
    M6[3]=M5[1]+M5[3];
    for (j=0;j<2;j++)
      img->cof[i][j][0][0]= coef[i][0][0];
  }
}
