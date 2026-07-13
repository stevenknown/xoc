/* KNAPSACK
 */

//#define   N   4
//#define   C   9

void printf(char const*,...);
void memcpy(void* tgt, void* src, int size);
void * memset(void *s, int c, int n);
static int
KNAPSACK (int size[4 + 1], int value[4 + 1])
{
  int i, j;
  int v[4 + 1][9 + 1] = { 0 };
  memset(v, 0, sizeof(v));

  for (i = 0; i <= 4; i++) {
    v[i][0] = 0;
  }
  for (j = 0; j <= 9; j++) {
    v[0][j] = 0;
  }

  for (i = 1; i <= 4; i++) {
    for (j = 1; j <= 9; j++) {
      v[i][j] = v[i - 1][j];
      if (size[i] <= j) {
        if (v[i][j] < v[i - 1][j - size[i]] + value[i]) {
          v[i][j] = v[i - 1][j - size[i]] + value[i];
        }
      }
    }
  }

  return v[4][9];
}

static void
KNAPSACK_Optimized (int v[4 + 1][9 + 1], int size[4 + 1], int value[4 + 1])
{
  int i, j;

  for (i = 0; i <= 4; i++) {
    v[i][0] = 0;
  }
  for (j = 0; j <= 9; j++) {
    v[0][j] = 0;
  }

  for (i = 1; i <= 4; i++) {
    for (j = 1; j <= 9; j++) {
      v[i][j] = v[i - 1][j];
      if (size[i] <= j) {
        if (v[i][j] < v[i - 1][j - size[i]] + value[i]) {
          v[i][j] = v[i - 1][j - size[i]] + value[i];
        }
      }
    }
  }
}

/* Time:  O(4 * 9)
 * Space: O(2 * 9), but can't record items put to package.
 */
static int
KNAPSACK_Optimized_Space (int size[4 + 1], int value[4 + 1])
{
  int v[2][9 + 1] = { 0 };
  memset(v, 0, sizeof(v));
  int i, j, k;

  for (i = 0; i <= 1; i++) {
    v[i][0] = 0;
  }
  for (j = 0; j <= 9; j++) {
    v[0][j] = 0;
  }

  /* Every time set v[1].  */
  k = 1;

  for (i = 1; i <= 4; i++) {
    for (j = 1; j <= 9; j++) {
      v[k][j] = v[k - 1][j];
      if (size[i] <= j) {
        if (v[k][j] < v[k - 1][j - size[i]] + value[i]) {
          v[k][j] = v[k - 1][j - size[i]] + value[i];
        }
      }
    }
    memcpy (v[k - 1], v[k], sizeof (v[0][0]) * (9 + 1));
  }

  return v[1][9];
}

static void
Print_KNAPSACK (int v[4 + 1][9 + 1], int value[4 + 1], int size[4 + 1],
                int i, int j)
{
  if (i == 0) {
    return;
  } else if (v[i][j] == (v[i - 1][j - size[i]] + value[i])) {
    Print_KNAPSACK (v, value, size, i - 1, j - size[i]);
    printf ("%d ", value[i]);
  } else {
    Print_KNAPSACK (v, value, size, i - 1, j);
  }
}

int
main (void)
{
  int size[4 + 1] = { 0, 2, 3, 4, 5 };
  size[0] = 0;
  size[1] = 2;
  size[2] = 3;
  size[3] = 4;
  size[4] = 5;
  int value[4 + 1] = { 0, 3, 4, 5, 7 };
  value[0] = 0;
  value[1] = 3;
  value[2] = 4;
  value[3] = 5;
  value[4] = 7;
  int v[4 + 1][9 + 1] = { 0 };
  memset(v, 0, sizeof(v));

  KNAPSACK_Optimized (v, size, value);
  printf ("max value: %d\n", v[4][9]);
  printf ("item value: \n");
  Print_KNAPSACK (v, value, size, 4, 9);
  printf ("\n");
  return 0;
}
