/*
 * FLOYD
 */
void printf(char const*,...);
void memcpy(void* tgt, void* src, int size);
void * memset(void *s, int c, int n);

static void
FLOYD (int graph[3][3], int path[3][3])
{
  int k, i, j;

  memcpy (path, graph, sizeof (path[0][0]) * 3 * 3);

  for (k = 0; k < 3; k++) {
    for (i = 0; i < 3; i++) {
      for (j = 0; j < 3; j++) {
        if (path[i][j] > path[i][k] + path[k][j]) {
          path[i][j] = path[i][k] + path[k][j];
        }
      }
    }
  }
}

int
main (void)
{
  int graph[3][3] = { {0, 2,   9},
                      {8, 0,   6},
                      {1, 0x1000, 0} };
  graph[0][0] = 0;
  graph[0][1] = 2;
  graph[0][2] = 9;
  graph[1][0] = 8;
  graph[1][1] = 0;
  graph[1][2] = 6;
  graph[2][0] = 1;
  graph[2][1] = 0x1000;
  graph[2][2] = 0;

  int path[3][3] = { 0 };
  memset(path, 0, sizeof(path));
  int i, j;

  FLOYD (graph, path);

  for (i = 0; i < 3; i++) {
    for (j = 0; j < 3; j++) {
      printf ("%d ", path[i][j]);
    }
    printf ("\n");
  }

  return 0;
}
