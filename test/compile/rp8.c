void rp_loop()
{
  char *s, *s2;
  int lrc, j;
  char *ps, *pt;
  s = "a"; //
  s = "a"; //It should be rhs of init-def's DEF
  lrc = 0;
  for (j = 0; j < 5; j++)
    if (s[j] != s2[j])
      lrc = 1;
}
