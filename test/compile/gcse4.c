int sign(int a);
typedef struct img_par
{
  int cof[4][6][4][4];
} ImageParameters;
static const int quant_coef[6][4][4];
void itrans_sp(struct img_par *img)
{
  int i,j;
  int m5[4];
  int predicted_block[4][4];
  for (j=0; j < 4; j++)
    predicted_block[3][j]=m5[3];
  for (i=0; i < 4; i++)
  {
    m5[j]=predicted_block[i][j];
    predicted_block[i][3]=m5[3];
  }
  for (i=0;i<4;i++)
    img->cof[i][j][i][j]=sign(quant_coef[i][i][j]);
}
