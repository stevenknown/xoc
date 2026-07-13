int s241()
{
  int j;
  long g[39];
  for (j = 0; j < 17; j++)
    g[j] = j;
  for (j = 18; j < 39;)
    {
      g[j - 1] = g[j] - 1;
      j = j + 2;
    }
  return j;
}
