/*
 * LCS
 */
//#if 1
//#define N        10
//#define M        12
//#elif 0
//#define N        29
//#define M        28
//#else
//#define N        7
//#define M        6
//#endif
void printf(char const*,...);
void * memset(void *s, int c, int n);
static void
LCS (char *a, char *b, int len[10 + 1][12 + 1], int lcs[10 + 1][12 + 1])
{
  int i = 0; i = 0;
  int j = 0; j = 0;

  for (i = 0; i <= 10; i++) {
    len[i][0] = 0;
    lcs[i][0] = 0;
  }
  for (j = 0; j <= 12; j++) {
    len[0][j] = 0;
    lcs[0][j] = 0;
  }

  for (i = 1; i <= 10; i++) {
    for (j = 1; j <= 12; j++) {
      if (a[i - 1] == b[j - 1]) {
        len[i][j] = len[i - 1][j - 1] + 1;
        lcs[i][j] = 1;
      } else {
        if (len[i - 1][j] >= len[i][j - 1]) {
          len[i][j] = len[i - 1][j];
          lcs[i][j] = 2;
        } else {
          len[i][j] = len[i][j - 1];
          lcs[i][j] = 3;
        }
      }
    }
  }
}

static void
Print_LCS (int lcs[10 + 1][12 + 1], char *a, int i, int j)
{
  if (i == 0 || j == 0) {
    return;
  }
  if (lcs[i][j] == 1) {
    Print_LCS (lcs, a, i - 1, j - 1);
    printf ("%c, ", a[i - 1]);
  } else if (lcs[i][j] == 2) {
    Print_LCS (lcs, a, i - 1, j);
  } else {
    Print_LCS (lcs, a, i, j - 1);
  }
}

int
main (void)
{
//#if 1
  char *a = "xyxxzxyzxy";
  char *b = "zxzyyzxxyxxz";
  a = "xyxxzxyzxy";
  b = "zxzyyzxxyxxz";
//#elif 0
//  char *a = "ACCGGTCGAGTGCGCGGAAGCCGGCCGAA";
//  char *b = "GTCGTTCGGAATGCCGTTGCTCTGTAAA";
//#else
//  char *a = "ABCBDAB";
//  char *b = "BDCABA";
//#endif

  int len[10 + 1][12 + 1] = { 0 };
  int lcs[10 + 1][12 + 1] = { 0 };
  int i;
  memset(len, 0, sizeof(len));
  memset(lcs, 0, sizeof(lcs));

  LCS (a, b, len, lcs);

  printf ("String a:\n%s\n", a);
  printf ("String b:\n%s\n", b);
  printf ("LEN: %d\n", len[10][12]);
  Print_LCS (lcs, a, 10, 12);
  printf("\n");
  return 0;
}
