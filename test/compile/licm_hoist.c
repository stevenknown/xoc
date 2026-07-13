struct img_par;
typedef struct img_par
{
  int width;
  int block[16];
  int mpr[16][16];
} ppp;
typedef struct _pos
{
  int available;
  int addr;
} Pos;
int g1;
int g2;
int ** mpr;
int intra(struct img_par *img, int mode)
{
  int s0;
  int i,j;
  int ih,iv;
  int ib,ic,iaa;
  Pos ll[16];
  int up, left, leftup;
  if (!img->width)
  {
    left = ll[0].available;
    leftup = ll[0].available;
  }
  else
  {
    for (i=1; i<17;i++)
      left = ll[i].available ? img->block[ll[i].addr]: 0;
  }
  switch (mode)
  {
  case 0:
    for(j=0;j<1;j++)
        g2=g1;
    break;
  case 2:
    if (up && left)
      s0=5;
    for(i=0;i<1;i++)
      for(j=0;j<1;j++)
        img->mpr[i][j]=s0;
    break;
  case 3:
    if (leftup)
      g1=2;
    ih=0;
    iv=0;
    for (i=1;i<9;i++)
        ih = i;
    ib=ih;
    ic=iv;
    for (i=0;i<1;i++)
    {
      mpr[i][j]= 0 > (iaa < 255 ? (i*ib+j*ic) : 255) ?
              0 : ((i*ib +j*ic) < 255 ? (i*ib+j*ic) : 255);
    }
  }
  return 0;
}
