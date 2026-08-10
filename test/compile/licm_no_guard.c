void memcpy(void* tgt, void* src, int size);
static void
FLOYD (int graph[6][6], int path[6][6], int i, int j, int k)
{
  memcpy (path, graph, sizeof (path[0][0]) * 3 * 3);
  for (k = 0; k < 3; k++) {
    for (i = 0; i < 4; i++) {
      for (j = 0; j < 5; j++) {
          path[i][j] = path[i][k] + path[k][j];
      }
    }
  }
}
