typedef struct img_par
{
  int mpr[16][16];
} ImageParameters;
void intrapred(struct img_par *img)
{
  int i,j;
  int PredPel[13];
  if (i==4) {
    img->mpr[i][j] = img->mpr[i][j] = 2*PredPel[9];
  }
}
