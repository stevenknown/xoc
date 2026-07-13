typedef struct st
{
  char ** imgY;
} S;
struct par
{
  int mpr[16][16];
};
typedef struct pos
{
  int pos_x;
} Pos;
void getNeighbour(int curr_mb_nr, int xN, int yN, int luma, Pos *pix);
S *dec;
void intrapred(struct par *img)
{
  int i,j;
  char **imgY = dec->imgY;
  Pos pix[4];
  getNeighbour(1, 1, 1, 1, &pix[i]);
  switch (i)
  {
  case 1:
    for(i=0;i<4;i++)
      img->mpr[i][j]=imgY[j][pix[j].pos_x];
    break;
  }
}
