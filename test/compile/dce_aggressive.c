int foo()
{
  //There is no dead-code.
  char *s, *s2;
  int lrc, j;
  for (j = 0; j < sizeof(int); j++)
    if (s[j] != s2[j])
      lrc = 1;
  return lrc;
}
