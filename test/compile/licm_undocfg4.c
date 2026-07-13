void LCS (char *a, char *b, int lcs[10 + 1][12 + 1])
{
  int i;
  int j;
  for (i = 1; i <= 10; i++) {
    for (j = 1; j <= 12; j++) {
      if (a[i] == j) {
        lcs[i][j] = 1;
      }
    }
  }
}
