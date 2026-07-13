int *intra_block;
int intrapred(int ioff)
{
  int i,j;
  int PredPel[13]; // array of predictor pels
  char **imgY;
  int block_available_up;
  int block_available_left;
  int block_available_up_right;
  if (ioff)
  {
    for (i=0; i<4;i++)
      block_available_left = intra_block[i];
    block_available_up = intra_block[i];
    block_available_up_right = i ? intra_block[i] : 0;
  }
  if (block_available_up)
    PredPel[4] = imgY[j][i];
  return 0;
}
