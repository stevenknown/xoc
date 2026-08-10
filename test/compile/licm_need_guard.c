void memcpy(void* tgt, void* src, int size);
int n,m,o;
static void
FLOYD (int graph[6][6], int path[6][6], int i, int j, int k)
{
  memcpy (path, graph, sizeof (path[0][0]) * 3 * 3);
  for (k = 0; k < n; k++) {
    for (i = 0; i < 5; i++) {
      for (j = 0; j < o; j++) {
          path[i][j] = path[i][k] + path[k][j];
      }
    }
  }
}
