typedef struct img_par
{
  int m7[16][16]; //<! final 4x4 block. Extended to 16x16 for ABT
} ImageParameters;
typedef struct storable_picture
{
  char ** imgY;
} StorablePicture;
extern StorablePicture *dec_picture;
void copyblock_sp_hack(struct img_par *img)
{
  int i,j,j1,m5[4],m6[4];
  for (j=0;j<4;j++)
    m5[j]=img->m7[i][j];
  m6[2]=m5[3];
  for (j=0;j<2;j++)
    img->m7[i][j1]=m6[j];
  for (j=0; j < 4; j++)
      dec_picture->imgY[j][i]=img->m7[i][j];
}
