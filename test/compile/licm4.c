static void LCS(char *a, char *b, int len[10 + 1][12 + 1])
{
  int i;
  int j;
  for (i = 1; i <= 10; i++) {
    for (j = 1; j <= 12; j++) {
      if (a[i - 1] == b[j - 1])
        len[i][j] = len[i - 1][j - 1] + 1;
    }
  }
}
