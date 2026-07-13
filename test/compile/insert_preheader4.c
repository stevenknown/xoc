typedef struct storable_picture
{
  unsigned char ** imgY;
} StorablePicture;
typedef struct img_par
{
  int pix_y;
  int pix_x;
  int m7[100][100];
} ImageParameters;
extern StorablePicture *dec_picture;
void copyblock_sp(struct img_par *img)
{
  int i,j;
  for (j=0; j < 4; j++)
    for (i=0; i < 4; i++)
      dec_picture->imgY[img->pix_y][img->pix_x]=img->m7[i][j];
}
