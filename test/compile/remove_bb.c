int s84 ()
{
  int i, j, k;
  static int x3d[3][5][7];
  int rc;
  char *ps, *pt;
  for (i = 0; i < 3; i++)
    for (j = 0; j < 5; j++)
      for (k = 0; k < 7; k++)
        x3d[i][j][k] = i * 35 + j * 7 + k;
  return rc;
}
