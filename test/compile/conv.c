int out[100][100][100];
int in[100][100][100];
int weight[100][100][100][100];
int stride;
void conv(int HO, int WO, int CO, int CI, int KH, int KW)
{
    for (int row=0;row<HO;row++)
      for (int col=0;col<WO;col++)
        for (int co=0;co<CO;co++)
          for (int ci=0;ci<CI;ci++)
            for (int kh=0;kh<KH;kh++)
              for (int kw=0;kw<KW;kw++)
                out[co][row][col] += weight[co][ci][kh][kw] *
                                     in[ci][stride*row + kh][stride*col + kw];
     
}
