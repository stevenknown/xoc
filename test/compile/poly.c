//Exerpt from ldecod/src/image.c
typedef char byte;
typedef struct _dec_pic {
  int mb_field[12];
  byte ** imgY;
  byte *** imgUV;
} dec_pic;

dec_pic * dec_picture;

typedef struct _IMG {
   short PicSizeInMbs;
} IMG;

IMG * img;
int MB_BLOCK_SIZE = 1024;
int BLOCK_SIZE = 64;

void get_mb_pos(int, int*, int*);

int predicted_block[32][128];
int m5[128];

void MbAffPostProc()
{
  byte ** imgY  = dec_picture->imgY;
  byte *** imgUV = dec_picture->imgUV;
  byte temp[16][32];

  int i, x, y, x0, y0, uv;
  for (i = 0; i < (int)img->PicSizeInMbs; i += 2)
  {
    if (dec_picture->mb_field[i])
    {
      get_mb_pos(i, &x0, &y0);
      for (y=0; y<(2*MB_BLOCK_SIZE);y++)
        for (x=0; x<MB_BLOCK_SIZE; x++)
          temp[x][y] = imgY[y0+y][x0+x];

      for (y=0; y<MB_BLOCK_SIZE;y++)
        for (x=0; x<MB_BLOCK_SIZE; x++)
        {
          imgY[y0+(2*y)][x0+x]   = temp[x][y];
          imgY[y0+(2*y+1)][x0+x] = temp[x][y+MB_BLOCK_SIZE];
        }

      x0 = x0/2;
      y0 = y0/2;

      for (uv=0; uv<2; uv++)
      {
        for (y=0; y<(2*MB_BLOCK_SIZE/2);y++)
          for (x=0; x<MB_BLOCK_SIZE/2; x++)
            temp[x][y] = imgUV[uv][y0+y][x0+x];

        for (y=0; y<MB_BLOCK_SIZE/2;y++)
          for (x=0; x<MB_BLOCK_SIZE/2; x++)
          {
            imgUV[uv][y0+(2*y)][x0+x]   = temp[x][y];
            imgUV[uv][y0+(2*y+1)][x0+x] = temp[x][y+MB_BLOCK_SIZE/2];
          }
      }
    }
  }
}


struct img_par {
   int mpr[64][1024];
};

void copyblock_sp(struct img_par *img,int block_x,int block_y)
{
  //Through polyhedral trans, move dependent computation closer to improve
  //locality, and move independent computation further away to explore
  //parallelism.
  //Horizontal transform
  int j;
  int i;
  for (j=0; j< BLOCK_SIZE; j++)
    for (i=0; i< BLOCK_SIZE; i++)
      predicted_block[i][j]=img->mpr[i+block_x][j+block_y];
  for (j=0; j < BLOCK_SIZE; j++)  {
    for (i=0; i < 2; i++) {
      m5[i]=predicted_block[i][j]+predicted_block[3-i][j];
      m5[3-i]=predicted_block[i][j]-predicted_block[3-i][j];
    }
    predicted_block[0][j]=(m5[0]+m5[1]);
    predicted_block[2][j]=(m5[0]-m5[1]);
    predicted_block[1][j]=m5[3]*2+m5[2];
    predicted_block[3][j]=m5[3]-m5[2]*2;
  }

  //  Vertival transform
  for (i=0; i < BLOCK_SIZE; i++) {
    for (j=0; j < 2; j++) {
      m5[j]=predicted_block[i][j]+predicted_block[i][3-j];
      m5[3-j]=predicted_block[i][j]-predicted_block[i][3-j];
    }
    predicted_block[i][0]=(m5[0]+m5[1]);
    predicted_block[i][2]=(m5[0]-m5[1]);
    predicted_block[i][1]=m5[3]*2+m5[2];
    predicted_block[i][3]=m5[3]-m5[2]*2;
  }
 }
