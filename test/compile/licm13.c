int s61()
{
  int rc, lrc;
  int j;
  char *pc[6];
  static char blank_and_NUL[];
  pc[5] = blank_and_NUL;
  for (j = 0; j < 6; j++)
    while (*pc[j])
      if (*pc[j]++ < 0)
        rc = 1;
  return rc;
}
