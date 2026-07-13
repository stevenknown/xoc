/*
 * Matrix Chain Mutilplication.
 */
void printf(char const*,...);
void * memset(void *s, int c, int n);
static unsigned
Matrix_Chain_Multiplication (unsigned r[5 + 1])
{
  unsigned c[5][5] = { 0 };
  int i = 0; i = 0;
  int j = 0; j = 0;
  int d = 0; d = 0;
  int k = 0; k = 0;
  memset(c, 0, sizeof(c));

  for (i = 0; i < 5; i++) {
    c[i][i] = 0;
  }

  for (d = 1; d < 5; d++) {
    for (i = 0; i < 5 - d; i++) {
      j = i + d;
      c[i][j] = 0x1000;

      for (k = i + 1; k <= j; k++) {
        if (c[i][j] > c[i][k - 1] + c[k][j] + r[i] * r[k] * r[j + 1]) {
          c[i][j] = c[i][k - 1] + c[k][j] + r[i] * r[k] * r[j + 1];
        }
      }
    }
  }

  return c[0][5 - 1];
}

int
main (void)
{
  unsigned r[5 + 1] = { 5, 10, 4, 6, 10, 2 };
  r[0]=5;
  r[1]=10;
  r[2]=4;
  r[3]=6;
  r[4]=10;
  r[5]=2;

  unsigned min;
  min = Matrix_Chain_Multiplication (r);
  printf ("min is : %d\n", min);
  return 0;
}
