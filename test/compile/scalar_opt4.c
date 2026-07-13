void printf(char const*, ...);
int s61()
{
  static char s61er[] = "s61,er%d\n";
  long int to, longint;
  int rc, lrc;
  int j;
  char *pc[6];
  lrc = 0;

  for (j = 0; j < 6; j++)
    while (*pc[j])
      if (*pc[j]++ < 0)
        lrc = 1;

  if (lrc != 0)
    {
      rc = rc + 2;
      if (to != 0)
        printf (s61er, 2);
    }

  return rc;
}
