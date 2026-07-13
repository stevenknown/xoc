typedef struct img_par
{
  int m7[16][16];
  int pix_x;
} ImageParameters;
typedef struct storable_picture
{
  char ** imgY;
} StorablePicture;
StorablePicture *dec_picture;
void copyblock_sp(struct img_par *img)
{
  int i,j,m6[4];
  for (i=0;i<4;i++)
    for (j=0;j<2;j++)
      img->m7[i][j] =m6[j] ? 0 : m6[j];
  for (j=0; j < 4; j++)
    for (i=0; i < 4; i++)
      dec_picture->imgY[i][img->pix_x]=0;
}
